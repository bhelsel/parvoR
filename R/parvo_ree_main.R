# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Calculate Resting Metabolic Rate
#' @description Read in Parvo or Cosmed data and accelerometer data (if available) to calculate the estimated resting metabolic rate.
#' @param path Pathname to the Parvo XLSX file.
#' @param accel_path Pathname to the accelerometer AGD file, Default: NULL.
#' @param var_limit Number to ensure changes in ventilation, oxygen consumption, and respiratory quotient do not exceed variation limit, Default: 15.
#' @param steady_state The number of rows at the end of the data set to consider as the steady state measurement, Default: 15
#' @return Returns a tibble with the resting metabolic rate output, the length of time of steady state, and the number of the last observations used (maximum: 5).
#' @details Read in Parvo or Cosmed data and accelerometer data (if available) to calculate the estimated resting metabolic rate.
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[dplyr]{group_by}}
#'  \code{\link[dplyr]{lead-lag}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[dplyr]{rename}}
#'  \code{\link[dplyr]{select}}
#'  \code{\link[dplyr]{summarise_all}}
#'  \code{\link[lubridate]{round_date}}
#'  \code{\link[MoveKC]{read_agd}}
#' @rdname calculate_rmr
#' @export 
#' @importFrom lubridate round_date
#' @importFrom stats na.omit
#' @importFrom utils tail

calculate_rmr <- function(path, accel_path = NULL, var_limit = 15, steady_state = 15) {
  data <- extract_data(path, time_breaks = "1 min")
  data <- data[(nrow(data) - steady_state) : nrow(data), ]
  
  kcal_pos <- grep("kcal_d", colnames(data))
  
  data <- data[, c(1, grep(pattern = "^ve_l_min|^vo2_kg|^rq", colnames(data)), kcal_pos[which.min(kcal_pos)])]
  
  data <- 
    data[, 2:4] %>%
    apply(., 2, FUN = function(x){
      (((x - dplyr::lag(x)) / dplyr::lag(x)) * 100)
      }) %>%
    `colnames<-`(paste0("diff_", colnames(.))) %>%
    cbind(data, .) %>%
    dplyr::as_tibble() %>%
    stats::na.omit() %>%
    dplyr::filter_at(dplyr::vars(dplyr::contains("diff_")), ~abs(.x) < var_limit)
    
  
  if(is.null(accel_path)==FALSE){
    accel <- MoveKC::read_agd(accel_path)
    accel <- cbind(timestamp = paste(accel$Date, accel$` Time`), accel)
    accel[, c("Date", " Time")] <- NULL
    accel$timestamp <- as.POSIXct(strptime(accel$timestamp, tz = Sys.timezone(), format = "%m/%d/%Y %H:%M:%S"))
    accel$timestamp <- lubridate::round_date(accel$timestamp, unit = "1 minute")
    accel <- accel[, c("timestamp", " Axis1", "HR")]
    accel <- dplyr::rename(accel, "accel.hr.bpm" = "HR", "counts" = " Axis1")
    accel <- accel %>%
      dplyr::group_by(timestamp = cut(accel$timestamp, breaks = "1 min")) %>%
      dplyr::summarise_all(mean) %>%
      dplyr::ungroup()
    accel$timestamp <- as.POSIXct(strftime(as.character(accel$timestamp), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S"))
    data <- merge(data, accel, by="timestamp", all.x=TRUE)
    data <- data[data$counts<50, ]
  }
  
  
  data <- 
    data %>% 
    dplyr::group_by(group = cumsum(c(TRUE, diff(data$time) > 1))) %>% 
    dplyr::mutate(minutes = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(minutes == max(minutes))
  

  if(length(unique(data$group)) > 1){
    data <- data[data$time.group == max(data$time.group), ] # Return last steady state for periods of equivalent length
  }
  
  data <- utils::tail(data, 5) # Return last 5 if max is greater than 5 minutes.
  data$n.obs <- nrow(data) # Number of observations used.
  
  data <- data %>%
    dplyr::select(-c(time, dplyr::contains("diff_"))) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group)
  
  data$moderate_vo2_kg <- data[, grep("^vo2_kg", colnames(data)), drop = TRUE] * 3
  
  data$vigorous_vo2_kg <- data[, grep("^vo2_kg", colnames(data)), drop = TRUE] * 6
  
  return(data)
}









