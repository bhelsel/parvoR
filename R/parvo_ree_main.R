# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Calculate Resting Metabolic Rate
#' @description Read in Parvo or Cosmed data and accelerometer data (if available) to calculate the estimated resting metabolic rate.
#' @param path Pathname to the REE XLSX file.
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
    data <- merge(data, .retrieve_accelerometer(accel_path), by="time", all.x=TRUE)
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


#' @title Calculate Resting Metabolic Rate using Coefficient of Variation
#' @description Calculate Resting Metabolic Rate using Coefficient of Variation
#' @param path Pathname to the REE XLSX file.
#' @param accel_path Pathname to the accelerometer AGD file, Default: NULL., Default: NULL
#' @param window The length of the rolling window in minutes to calculate the coefficient of variation, Default: 5 minutes
#' @param excluded The number of minutes to exclude from the beginning of the analysis, Default: 5 minutes
#' @return OUTPUT_DESCRIPTION
#' @details Calculate Resting Metabolic Rate using Coefficient of Variation. Currently,
#'     it is set up to calculate coefficient of variation over 10 observations (e.g., 2.5 minutes
#'     at 15 second epochs, 5 minutes at 30 second epochs or 10 minutes at 1 minute epochs). This
#'     may be adjustable in the future.
#' @rdname calculate_rmr_cv
#' @export 
#' @importFrom zoo zoo rollapply
#' @importFrom stats sd
#' @importFrom dplyr filter group_by summarise_at vars all_of mutate select summarise_all

calculate_rmr_cv <- function(path, accel_path = NULL, window = 5, excluded = 5){
  data <- extract_data(path)
  time_diff <- as.numeric(difftime(data[2, "timestamp", drop = TRUE], data[1, "timestamp", drop = TRUE], units = "sec"))
  data <- data[(((excluded * 60) %/% time_diff)+1):nrow(data), ]
  zoo_df <- zoo::zoo(data[, c("vo2_ml_min", "vco2_ml_min")], order.by = data$timestamp)
  rolling_cv <- zoo::rollapply(zoo_df, width = (window * 60) %/% time_diff, FUN = function(x) (stats::sd(x) / mean(x)) * 100, by = 1)
  
  ilt10 <- which(rolling_cv[, 1] < 10 & rolling_cv[, 2] < 10) # Find indices less than 10% CV
  if(length(ilt10) != 0){
    min_index <- ilt10[which.min(rolling_cv[ilt10, 1] + rolling_cv[ilt10, 2])] # Find minimum values of the less than 10% CV indices
  } else{
    min_index <- which.min(rolling_cv[, 1] + rolling_cv[, 2]) # Find the minimum values even if not less than 10% CV
  }
  
  min_period <- data$timestamp[min_index:(min_index + (((window * 60) %/% time_diff) - 1))] # Extract the corresponding time period

  variables <- c("ve_l_min", "rq", "vo2_kg_ml_min_kg", names(data)[grep("eekc_kcal_day|ree_kcal_d", names(data))])
  
  data %<>%
    dplyr::filter(timestamp %in% min_period) %>%
    dplyr::group_by(timestamp = strptime(timestamp, format = "%Y-%m-%d %H:%M")) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::all_of(variables)), .funs = function(x) mean(x, na.rm = TRUE)) %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, tz = "UTC"))
  
  if(!is.null(accel_path)){
    data <- merge(data, .retrieve_accelerometer(accel_path), by.x="timestamp", by.y="time", all.x=TRUE)
    data <- data[data$counts<50, ]
  }
  
  data %<>% 
    dplyr::select(-timestamp) %>%
    dplyr::summarise_all(mean) %>% 
    dplyr::mutate(
      minutes = 5, n.obs = (window * 60) %/% time_diff, 
      moderate_vo2_kg = vo2_kg_ml_min_kg * 3, 
      vigorous_vo2_kg = vo2_kg_ml_min_kg * 6,
      cv_vo2_ml_min = as.numeric(rolling_cv[min_index, "vo2_ml_min"]),
      cv_vco2_ml_min = as.numeric(rolling_cv[min_index, "vco2_ml_min"]))
  
  return(data)
  
}





