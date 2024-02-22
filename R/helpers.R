# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_participant_info <- function(data){
  name <- paste0(unlist(strsplit(stringr::str_replace(as.character(data[5,2]), " ", ""), "[,]"))[2], " ", unlist(strsplit(stringr::str_replace(as.character(data[5,2]), " ", ""), "[,]"))[1])
  age <- as.numeric(data[6, 2])
  gender <- ifelse(paste0(data[6, 5])=="F", "Female", "Male")
  height_cm <- round(as.numeric(paste0(data[7,2])) * 2.54, 2)
  weight_kg <- round(as.numeric(data[7,7]) / 2.205, 2)
  rbind(
    paste("Name: ", name), paste("Age:", age, "years"), paste("Gender:", gender),
    paste("Weight: ", weight_kg, "kg"), paste("Height:", height_cm, "cm")
  )
}

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_test_env <- function(data){
  # Test Environment
  location <- paste0(data[1, 1], " ", data[2, 1])
  starttime <- .get_start(data)
  date <- format(as.Date(starttime), "%B %d, %Y")
  InspTemp <- paste0(round(as.numeric(data[13,2]), 3)*(9/5) + 32, " deg F")
  BaroPres <- paste(round(as.numeric(data[13,5]), 2), data[13, 6])
  ExpFlowTemp <- data[14, 2]
  BaseO2 <- round(as.numeric(data[19, 2]), 2)
  BaseCO2 <- round(as.numeric(data[19, 5]), 2)
  MeasuredO2 <- round(as.numeric(data[19, 8]), 2)
  MeasuredCO2 <- round(as.numeric(data[19, 11]), 2)
  rbind(
    paste("Location:", location), paste("Date:", date), paste("Start Time:", starttime), 
    paste("Insp Temp:", InspTemp), paste("Baro Press:", BaroPres), paste("Exp Flow Temp:", ExpFlowTemp),
    paste0("Base O2 (", BaseO2, ")", " and measured O2 (", MeasuredO2, ")"),
    paste0("Base CO2 (", BaseCO2, ")", " and measured CO2 (", MeasuredCO2, ")")
  )
}

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_data <- function(data, vo2_change, rer_limit){
  
  starttime <- .get_start(data)
  ds <- grep("TIME", data[, 1, drop = TRUE])
  
  # Extract VO2 max information
  s_event <- .parvo_extract_vo2_max_meta(data)[[1]]
  #s_vo2 <- .parvo_extract_vo2_max_meta(data)[[2]]
  
  vo2 <- 
    data[ds:nrow(data), ] %>%
    name_repair(., rows = 1:3) %>%
    {.[stats::complete.cases(.), ]} %>%
    dplyr::tibble() %>%
    dplyr::mutate_all(as.numeric) %>%
    dplyr::mutate(datetime = (starttime + (as.numeric(time_min)*60)), .before = 1) %>%
    dplyr::mutate(vo2_ml_min = vo2_l_min * 1000, log_ve_l_min = log10(ve_l_min)) %>%
    .check_for_time_gaps() %>%
    dplyr::filter(datetime < s_event[s_event$Events == "Cool Down", 2, drop = TRUE])

  #vo2MaxValue <- s_vo2[s_vo2$Measure=="vo2_ml_kg_min", 2, drop = TRUE]
  data$oues <- data$event <- NA
  # Identify events
  if(any(grepl("Start", s_event$Events))){
    exerciseStart <- max(s_event[s_event$Events=="Start Exercise", 2, drop = TRUE] - starttime)
  } else{
    exerciseStart <- max(s_event[s_event$Events=="Warm Up", 2, drop = TRUE] - starttime)
  }
  
  # How to identify max here?
  time2max100 <-  vo2[vo2$time_min >= vo2$time_min[nrow(vo2)] - 2, ] %>%
    dplyr::mutate(changef = (vo2_kg_ml_kg_m - dplyr::lag(vo2_kg_ml_kg_m)) / dplyr::lag(vo2_kg_ml_kg_m) * 100,
                  changeb = (vo2_kg_ml_kg_m - dplyr::lead(vo2_kg_ml_kg_m)) / dplyr::lead(vo2_kg_ml_kg_m) * 100) %>%
    dplyr::mutate(changem = rowSums(dplyr::select(., c("changef", "changeb")), na.rm = TRUE) / 2) %>%
    dplyr::filter(abs(changem) <= vo2_change & rer >= rer_limit) %>%
    {.[which.max(round(.$vo2_kg_ml_kg_m, 3)), "time_min", drop = TRUE]}
 
  time2max90 <- time2max100 * 0.9
  time2max75 <- time2max100 * 0.75
  
  # Flag events
  vo2[which.max(vo2$time_min >= exerciseStart), "event"] <- "start"
  vo2[which.max(vo2$time_min >= time2max100), "event"] <- "vo2_max_100"
  vo2[which.max(vo2$time_min >= time2max90), "event"] <- "vo2_max_90"
  vo2[which.max(vo2$time_min >= time2max75), "event"] <- "vo2_max_75"
  # Get row indices
  start <- which(vo2$event=="start")
  vo2Max100 <- which(vo2$event == "vo2_max_100")
  vo2Max90 <- which(vo2$event == "vo2_max_90")
  vo2Max75 <- which(vo2$event == "vo2_max_75")
  # Calculate OUES
  
  outs <- tryCatch({
    fit100 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max100, ])
    vo2[vo2Max100, "oues"] <- unname(fit100$coefficients[2])
    fit90 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max90, ])
    vo2[vo2Max90, "oues"] <- unname(fit90$coefficients[2])
    fit75 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max75, ])
    vo2[vo2Max75, "oues"] <- unname(fit75$coefficients[2])
    # Return OUES values and data
    out <- list(oues = cbind(vo2[vo2Max75, "oues"], vo2[vo2Max90, "oues"], vo2[vo2Max100, "oues"]), data = vo2)
  }, 
  error = function(x){
    out <- list(oues = cbind(NA, NA, NA), data = vo2)
    }
  )
  return(outs)
}

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_meta <- function(data){
  starttime <- .get_start(data)
  rm_all_na <- function(x) !all(is.na(x))
  eventSummary <- tryCatch({
    events <- data[which(grepl("event", data[, 1], ignore.case = TRUE)):nrow(data), ]
    events <- events[apply(events[, -1], 1, rm_all_na), apply(events, 2, rm_all_na)]
    events[, 1] <- as.POSIXct(starttime, format = "%Y-%m-%d %H:%M:%S") + (as.numeric(events[, 1]) * 60)
    eventSummary <- tibble(Events = events[, 2], Time = events[, 1])
  }, 
    error = function(x){
      endtime <- starttime + (as.numeric(data[(grep("Max VO2", data[, 1]) - 1), 1, drop = TRUE]) * 60)
      eventSummary <- dplyr::tibble(Events = c("Start Exercise", "Cool Down"), Time = c(starttime, endtime))
      }
    )
  
  # VO2 Max Summary
  vo2Summary <- 
    data[, 1] %>%
    grepl("Max VO2", .) %>%
    which() %>%
    data[., apply(data[., ], 2, rm_all_na)] %>%
    {matrix(.[, 2:ncol(.)], ncol = 2, byrow = TRUE)} %>%
    data.frame() %>%
    `colnames<-`(c("Values", "Measure")) %>%
    dplyr::mutate(Measure = gsub("[/]", "_", tolower(Measure)),
                  Measure = ifelse(grepl("min", Measure), paste0("vo2_", Measure), Measure),
                  Values = round(as.numeric(Values), 3)) %>%
    {tibble(Measure = .[, "Measure"], Values = .[, "Values"])}
  
  return(list(eventSummary, vo2Summary))
}

#' @noRd
#' @keywords internal
# .clean_names <- function(data){
#   i.col <- which(data[, 1] == "TIME")
#   colNames <- t(data[i.col:(i.col + 2), ])
#   colNames <- apply(colNames, 2, tolower)
#   colNames[which(is.na(colNames))] <- ""
#   colNames[which(colNames %in% c("stpd", "btps", "bt/st"))] <- ""
#   colNames <- gsub("/", "_", colNames)
#   colNames[which(colNames == "vo2_kg")] <- "vo2_"
#   cols <- sapply(1:nrow(colNames), function(x) paste(colNames[x, 1], colNames[x, 2], colNames[x, 3]))
#   cols <- gsub("__", "_", gsub(" |  ", "_", cols))
#   i <- which(substr(cols, nchar(cols), nchar(cols)) == "_")
#   cols[i] <- substr(cols[i], 1, nchar(cols[i])-1)
#   return(cols)
# }

#' @noRd
#' @keywords internal
name_repair <- function(data, rows){
  
  repaired_names <- 
    apply(data[rows, ], 2, paste, collapse = "_") %>%
    unname() %>%
    tolower() %>%
    gsub("[/]", "_", .) %>%
    gsub("[^a-z0-9_]", "", .) %>%
    gsub("__", "_", .) %>%
    gsub("_$|_na|_bt_st|_stpd|_btps", "", .)
    
  
  colnames(data) <- repaired_names
  mc <- which(colnames(data) == "na")
  if(length(mc) == 0){
    data <- data[-seq(rows[1], rows[length(rows)], 1), ]
  } else{
    data <- data[-seq(rows[1], rows[length(rows)], 1), -mc]
  }
  return(data)
}


#' @noRd
#' @keywords internal
.get_start <- function(data){

  starttime <-
    paste0(
      data[3, 2], "/", data[3, 4], "/", data[3, 6], " ",
      data[3, 7], ":", data[3,9], ":", data[3,10]
    )

  as.POSIXct(starttime, ormat = "%Y/%m/%d %H:%M:%S", tz = "UTC")
}


#' @noRd
#' @keywords internal

aggregate_time <- function(data, time_breaks = "1 sec"){
  data <- 
    data %>% 
    dplyr::group_by(time = lubridate::floor_date(time, unit = time_breaks)) %>%
    dplyr::summarise_all(mean, na.rm = TRUE)
  return(data)
}

.check_for_time_gaps <- function(df){
  df$time_diff <- difftime(df$datetime, dplyr::lag(df$datetime), units = "min")
  m <- as.numeric(stats::median(df$time_diff, na.rm = TRUE))
  if(any(df$time_diff >= 5, na.rm = TRUE)){
    idx <- first(which(df$time_diff >= m*2))
    df <- df[1:(idx-1), ]
  }
  df$time_diff <- NULL
  return(df)
}

.retrieve_accelerometer <- function(path){
  data <- MoveKC::read_agd(path)
  data <- cbind(time = paste(data$Date, data$` Time`), data)
  data[, c("Date", " Time")] <- NULL
  data$time <- as.POSIXct(strptime(data$time, format = "%m/%d/%Y %H:%M:%S"), tz = "UTC")
  data$time <- lubridate::round_date(data$time, unit = "1 minute")
  data <- data[, c("time", " Axis1", "HR")]
  data <- dplyr::rename(data, "hr_bpm" = "HR", "counts" = " Axis1")
  data <- data %>%
    dplyr::group_by(time = cut(data$time, breaks = "1 min")) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup()
  data$time <- as.POSIXct(strftime(as.character(data$time), format = "%Y-%m-%d %H:%M:%S"),  tz = "UTC")
  return(data)
}


 


