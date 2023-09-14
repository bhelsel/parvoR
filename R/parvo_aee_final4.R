# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Parvo AEE Final 4
#' @description Takes average of last 4 minutes of the walking protocol from the WalkDS study.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param corrected.time.path Pathname to a document of corrected time stamps, Default: NULL.
#' @param accel.path Pathname to the accelerometer AGD file, Default: NULL.
#' @param rest1met Resting VO2 for 1 metabolic equivalent (MET), Default = 3.5 ml/kg/min
#' @param return_raw_data Return raw data aggregated to 5-second epochs instead of a minute-level summary
#' @param epoch_size Epoch size for agcounts package to calculate counts, Default: 5
#' @param minutes_returned Number of minutes of data returned (range 1-7 minutes), Default: 4
#' @return Returns an average for the last 4 minutes of the WalkDS walking stages for VO2, METS, and RQ.
#' @details Takes average of last 4 minutes of the walking protocol from the WalkDS study.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[dplyr]{select}}
#' @rdname parvo_aee_final4
#' @export
#' @importFrom readxl read_xlsx
#' @importFrom lubridate force_tz round_date
#' @importFrom agcounts get_counts


parvo_aee_final4 <- function (parvo.path, corrected.time.path = NULL, accel.path = NULL, 
                              rest1met = 3.5, return_raw_data = FALSE, epoch_size = 5,
                              minutes_returned = 4) {
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
  id <- strsplit(x = gsub(pattern = ".xlsx", replacement = "", x = basename(parvo.path)), split = "S")[[1]][1]
  stage <- strsplit(x = gsub(pattern = ".xlsx", replacement = "", x = basename(parvo.path)), split = "S")[[1]][2]
  unit <- paste(epoch_size, "seconds")
  
  if(is.null(corrected.time.path)){
    starttime <- as.POSIXct(paste0(file[3, 2], "/", file[3, 4], "/", file[3, 6], " ", file[3, 7], ":", file[3,9], ":", file[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())  
  }
  
  if(!is.null(corrected.time.path)){
    corrected.time <- readxl::read_xlsx(corrected.time.path)
    corrected.time <- corrected.time[, c("id", "stage", "start")]
    starttime <- corrected.time[corrected.time$id==as.numeric(id) & corrected.time$stage==as.numeric(stage), "start"][[1]]
    starttime <- strsplit(x = starttime, split = "Start: ")[[1]][2]
    starttime <- as.POSIXct(starttime)
  }
  
  vo2 <- as.data.frame(file[32:nrow(file), 1:9])
  colnames(vo2) <- c("time.min", "vo2.l.min", "vo2.ml.kg.min", "mets", "rer", "ree.kcal.min", "tm.per.grade", "tm.speed", "ve.l.min")
  vo2 <- vo2[is.na(vo2$tm.speed)==FALSE & vo2$tm.speed!=0, ]
  vo2[names(dplyr::select(vo2, time.min:ve.l.min))] <- sapply(vo2[names(dplyr::select(vo2, time.min:ve.l.min))], as.numeric)
  vo2$mets <- vo2$vo2.ml.kg.min / rest1met
  vo2 <- vo2[vo2$time.min >= (7.5 - as.integer(minutes_returned)) & vo2$time.min <= 7.5, ]
  vo2 <- cbind(timestamp = format(starttime + as.numeric(vo2$time.min)*60, "%Y/%m/%d %H:%M:%S"),  vo2)
  vo2$timestamp <- as.POSIXct(vo2$timestamp, format = "%Y/%m/%d %H:%M:%S") %>% lubridate::force_tz("UTC")
  vo2.summary <- rbind(paste0("Start Time: ", starttime), paste0("VO2 L/min: ", round(mean(vo2$vo2.l.min), 3)),
                       paste0("VO2/kg: ", round(mean(vo2$vo2.ml.kg.min), 3)), paste0("METS: ", round(mean(vo2$mets), 3)), paste0("RQ: ", round(mean(vo2$rer), 3)))
  
  if(!is.null(accel.path)){
    gt3x.files <- list.files(accel.path, pattern = ".gt3x$", full.names = TRUE)
    agd.files <- list.files(accel.path, pattern = ".agd$", full.names = TRUE)
    hip <- get_counts(path = gt3x.files[grep(paste0("BH", id), gt3x.files)], epoch = epoch_size, parser = "read.gt3x")
    hip <- hip[c("time", "Axis1", "Vector.Magnitude")]
    
    hip <- 
      hip %>% 
      dplyr::filter(time >= lubridate::floor_date(vo2$timestamp[1], unit = unit)) %>%
      dplyr::filter(time <= lubridate::ceiling_date(vo2$timestamp[nrow(vo2)], unit = unit))
    
    hip <- 
      read_agd(agd.files[grep("BH", agd.files)]) %>%
      mutate(time = as.POSIXct(paste0(Date, " ", ` Time`), format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
             time = lubridate::round_date(time, unit = unit)) %>%
      group_by(time) %>%
      summarise(HR = round(mean(HR, na.rm = TRUE))) %>%
      mutate(HR = ifelse(HR < 30, NA, HR),
             HR = zoo::na.locf0(HR)) %>% # Last observation carried forward if HR is < 30
      dplyr::filter(time >= lubridate::floor_date(vo2$timestamp[1], unit = unit)) %>%
      dplyr::filter(time <= lubridate::ceiling_date(vo2$timestamp[nrow(vo2)], unit = unit)) %>%
      merge(x = hip, y = ., by = "time")
      
    hip.summary <- 
      hip %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), HR = mean(HR, na.rm = TRUE), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
    
    right.wrist <- get_counts(path = gt3x.files[grep(paste0("R", id), gt3x.files)], epoch = epoch_size, parser = "read.gt3x")
    right.wrist <- right.wrist[c("time", "Axis1", "Vector.Magnitude")]

    right.wrist <- 
      right.wrist %>% 
      dplyr::filter(time >= lubridate::floor_date(vo2$timestamp[1], unit = unit)) %>%
      dplyr::filter(time <= lubridate::ceiling_date(vo2$timestamp[nrow(vo2)], unit = unit))
    
    right.wrist.summary <- 
      right.wrist %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
  
    left.wrist <- get_counts(path = gt3x.files[grep(paste0("L", id), gt3x.files)], epoch = epoch_size, parser = "read.gt3x")
    left.wrist <- left.wrist[c("time", "Axis1", "Vector.Magnitude")]

    left.wrist <- 
      right.wrist %>% 
      dplyr::filter(time >= lubridate::floor_date(vo2$timestamp[1], unit = unit)) %>%
      dplyr::filter(time <= lubridate::ceiling_date(vo2$timestamp[nrow(vo2)], unit = unit))
    
    left.wrist.summary <- 
      left.wrist %>% 
      dplyr::group_by(time = lubridate::round_date(time, unit="1 minute")) %>% 
      dplyr::summarise(Axis1 = sum(Axis1), Vector.Magnitude = sum(Vector.Magnitude), n = dplyr::n()) %>%
      mutate(epoch_size = epoch_size)
    
    accel.summary <- rbind(paste0("HR: ", round(mean(hip.summary$HR), 1)), 
                           paste0("Hip Vertical Axis: ", round(mean(hip.summary$Axis1), 1)),
                           paste0("Hip Vector Magnitude: ", round(mean(hip.summary$Vector.Magnitude), 1)), 
                           paste0("Right Wrist Vertical Axis: ", round(mean(right.wrist.summary$Axis1), 1)),
                           paste0("Right Wrist Vector Magnitude: ", round(mean(right.wrist.summary$Vector.Magnitude), 1)), 
                           paste0("Left Wrist Vertical Axis: ", round(mean(left.wrist.summary$Axis1), 1)),
                           paste0("Left Wrist Vector Magnitude: ", round(mean(left.wrist.summary$Vector.Magnitude), 1)))
    
    hip %<>% rename("hip.va" = "Axis1", "hip.vm" = "Vector.Magnitude", "heart.rate" = "HR")
    right.wrist %<>% rename("rwrist.va" = "Axis1", "rwrist.vm" = "Vector.Magnitude")
    left.wrist %<>% rename("lwrist.va" = "Axis1", "lwrist.vm" = "Vector.Magnitude")
    accel_data <- merge(merge(hip, right.wrist, by = "time"), left.wrist, by = "time")
  }
  
  if(return_raw_data){
    
    if(epoch_size != 1){
      data <- 
        vo2 %>% 
        rename("time" = "timestamp") %>% 
        mutate(time = lubridate::round_date(time, unit = unit)) %>%
        group_by(time) %>%
        summarise_all(mean) %>%
        merge(., accel_data, by = "time", all.x = TRUE)
      
      if(any(is.na(data[1, ]))) data <- data[2:nrow(data), ]
      
    }
    
    if(epoch_size == 1) {
      data <- 
        vo2 %>%
        rename("time" = "timestamp") %>% 
        merge(., accel_data, by = "time") %>%
        mutate_at(.vars = vars(time:ve.l.min), .funs = function(x) zoo::na.locf0(x))
      }
  
    if(any(is.na(data$time.min))){
      indx <- which(is.na(data$time.min))
      varnames <- names(select(data, time.min:ve.l.min))
      for(n in varnames){
        for(i in indx){
          data[i, n] <- (data[i-1, n] + data[i+1, n]) / 2
        }
      }
    }
    
    return(data)
  }
  
  if(!return_raw_data){
    if(!is.null(accel.path)) {
      return(rbind(vo2.summary, accel.summary))
    }
    
    if(is.null(accel.path)) {
      return(vo2.summary)
    }
  }
}


