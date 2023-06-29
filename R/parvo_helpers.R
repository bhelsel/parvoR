#' Copyright © 2023 University of Kansas. All rights reserved.
#'
#' Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_participant_info <- function(data){
  name <- paste0(unlist(strsplit(stringr::str_replace(as.character(data[6,2]), " ", ""), "[,]"))[2], " ", unlist(strsplit(stringr::str_replace(as.character(data[6,2]), " ", ""), "[,]"))[1])
  age <- as.numeric(data[7, 2])
  gender <- ifelse(paste0(data[7, 5])=="F", "Female", "Male")
  height_cm <- round(as.numeric(paste0(data[8,2])) * 2.54, 2)
  weight_kg <- round(as.numeric(data[8,7]) / 2.205, 2)
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
  InspTemp <- paste0(as.numeric(paste0(data[16,2]))*(9/5) + 32, " deg F")
  BaroPres <- paste(round(as.numeric(paste0(data[16,5])), 2), data[16, 6])
  ExpFlowTemp <- data[17, 2]
  BaseO2 <- round(as.numeric(data[18, 2]), 2)
  BaseCO2 <- round(as.numeric(data[18, 5]), 2)
  MeasuredO2 <- round(as.numeric(data[23, 8]), 2)
  MeasuredCO2 <- round(as.numeric(data[23, 11]), 2)
  rbind(
    paste("Location:", location), paste("Date:", date), paste("Start Time:", starttime), 
    paste("Insp Temp:", InspTemp), paste("Baro Press:", BaroPres), paste("Exp Flow Temp:", ExpFlowTemp),
    paste0("Base O2 (", BaseO2, ")", " and measured O2 (", MeasuredO2, ")"),
    paste0("Base CO2 (", BaseCO2, ")", " and measured CO2 (", MeasuredCO2, ")")
  )
}

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_data <- function(data){
  starttime <- .get_start(data)
  vo2 <- as.data.frame(data[31:nrow(data), 1:15])
  colnames(vo2) <- .clean_names(data)
  vo2 <- vo2[1:min(which(is.na(vo2$time_min))-1), ]
  vo2 <- cbind(datetime = (starttime + (as.numeric(vo2$time_min)*60)), vo2)
  vo2 <- vo2 %>% dplyr::mutate_at(.vars = c(2:16), .funs = as.numeric)
  vo2$vo2_ml_min <- vo2$vo2_l_min * 1000
  vo2$log_ve_l_min <- log10(vo2$ve_l_min)
  # Extract VO2 max information
  eventSummary <- .parvo_extract_vo2_max_meta(data)[[1]]
  vo2Summary <- .parvo_extract_vo2_max_meta(data)[[2]]
  vo2MaxValue <- as.numeric(vo2Summary[vo2Summary$Measure=="vo2_ml_kg_m", 2])
  vo2$oues <- vo2$event <- NA
  # Identify events
  exerciseStart <- as.numeric(eventSummary[eventSummary$Event=="Start Exercise", 2] - starttime)
  time2max100 <- vo2[which.max(round(vo2$vo2_ml_kg_m, 5) >= round(vo2MaxValue, 5)), "time_min"]
  time2max90 <- time2max100 * 0.9
  time2max75 <- time2max100 * 0.75
  # Flag events
  vo2[which.max(vo2$time_min >= exerciseStart), "event"] <- "start"
  vo2[which.max(round(vo2$vo2_ml_kg_m, 5) == round(vo2MaxValue, 5)), "event"] <- "vo2_max_100"
  vo2[which.max(vo2$time_min >= time2max90), "event"] <- "vo2_max_90"
  vo2[which.max(vo2$time_min >= time2max75), "event"] <- "vo2_max_75"
  # Get row indices
  start <- which(vo2$event=="start")
  vo2Max100 <- which(vo2$event == "vo2_max_100")
  vo2Max90 <- which(vo2$event == "vo2_max_90")
  vo2Max75 <- which(vo2$event == "vo2_max_75")
  # Calculate OUES
  fit100 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max100, ])
  vo2[vo2Max100, "oues"] <- unname(fit100$coefficients[2])
  fit90 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max90, ])
  vo2[vo2Max90, "oues"] <- unname(fit90$coefficients[2])
  fit75 <- stats::lm(vo2_ml_min ~ log_ve_l_min, data = vo2[start:vo2Max75, ])
  vo2[vo2Max75, "oues"] <- unname(fit75$coefficients[2])
  # Return OUES values and data
  list(oues = cbind(vo2[vo2Max75, "oues"], vo2[vo2Max90, "oues"], vo2[vo2Max100, "oues"]), data = vo2)
}

#' @noRd
#' @keywords internal
.parvo_extract_vo2_max_meta <- function(data){
  starttime <- .get_start(data)
  vo2 <- as.data.frame(data[31:nrow(data), 1:15])
  # Event Summary
  events <- c("Warm Up", "Start Exercise", "Cool Down")
  warm.up <- starttime + as.numeric(vo2$Col1[1]) * 60
  if("Start Exercise" %in% vo2$Col2){
    start <- starttime + as.numeric(vo2[which.max(vo2$Col2 == "Start Exercise"), 1]) * 60
  } else {
    start <- starttime + (2 * 60) # Assume 2 minutes from warm up  
  }
  cool.down <- starttime + as.numeric(vo2$Col1[min(which(is.na(vo2$Col1))-1)]) * 60
  eventSummary <- data.frame(Events = events, Time = c(warm.up, start, cool.down))
  # VO2 Max Summary
  maxValues <- vo2[which(vo2$Col1=="Max VO2"), 2:12][!is.na(vo2[which(vo2$Col1=="Max VO2"), 2:12])]
  measure <- c("vo2_l_min", "vo2_ml_kg_m", "mets")
  values <- c(as.numeric(maxValues[1]), as.numeric(maxValues[3]), as.numeric(maxValues[5]))
  vo2Summary <- data.frame(cbind(Measure = measure, Values = values))
  return(list(eventSummary, vo2Summary))
}

#' @noRd
#' @keywords internal
.clean_names <- function(data){
  i.col <- which(data[, 1] == "TIME")
  colNames <- t(data[i.col:(i.col + 2), ])
  colNames <- apply(colNames, 2, tolower)
  colNames[which(is.na(colNames))] <- ""
  colNames[which(colNames %in% c("stpd", "btps", "bt/st"))] <- ""
  colNames <- gsub("/", "_", colNames)
  colNames[which(colNames == "vo2_kg")] <- "vo2_"
  cols <- sapply(1:nrow(colNames), function(x) paste(colNames[x, 1], colNames[x, 2], colNames[x, 3]))
  cols <- gsub("__", "_", gsub(" |  ", "_", cols))
  i <- which(substr(cols, nchar(cols), nchar(cols)) == "_")
  cols[i] <- substr(cols[i], 1, nchar(cols[i])-1)
  return(cols)
}

#' @noRd
#' @keywords internal
.get_start <- function(data){
  
  starttime <- 
    paste0(
      data[3, 2], "/", data[3, 4], "/", data[3, 6], " ", 
      data[3, 7], ":", data[3,9], ":", data[3,10]
      )
  
  as.POSIXct(starttime, ormat = "%Y/%m/%d %H:%M:%S", tz = Sys.timezone())
}







