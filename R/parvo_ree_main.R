#' @title Resting Energy Expenditure Main Function
#' @description Read in Parvo and Accelerometer data (if available) and calculate the estimated resting energy expenditure.
#' @param accel.path Pathname to the accelerometer AGD file, Default: NULL.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param var.limit Number to ensure changes in ventilation, oxygen consumption, and respiratory quotient do not exceed variation limit.
#' @return Returns a matrix with the resting energy expenditure output, the length of time of steady state, and the number of the last observations used (maximum: 5).
#' @details Read in Parvo and Accelerometer data (if available) and calculate the estimated resting energy expenditure.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[lubridate]{period}},\code{\link[lubridate]{round_date}}
#'  \code{\link[dplyr]{lead-lag}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{rename}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{context}},\code{\link[dplyr]{select}}
#'  \code{\link[MoveKC]{read_agd}}
#' @rdname parvo_ree_main
#' @export 
#' @importFrom lubridate minutes round_date
#' @importFrom stats na.omit

parvo_ree_main <- function(accel.path = NULL, parvo.path, var.limit = 15) {
  data <- parvo_extract_data(parvo.path, ree=TRUE, time.breaks = "1 min")
  data <- data[data$timestamp > min(data$timestamp) + lubridate::minutes(14), ]
  `%>%` <- dplyr::`%>%`
  
  data$diff.ve.l.min <- (((data$ve.l.min-dplyr::lag(data$ve.l.min, 1))/dplyr::lag(data$ve.l.min, 1))*100)
  data$diff.vo2.ml.kg.min <- (((data$vo2.ml.kg.min-dplyr::lag(data$vo2.ml.kg.min, 1))/dplyr::lag(data$vo2.ml.kg.min, 1))*100)
  data$diff.rq <- (((data$rq-dplyr::lag(data$rq, 1))/dplyr::lag(data$rq, 1))*100)
  
  data <- data %>% stats::na.omit() %>%
    dplyr::filter(abs(diff.ve.l.min)<var.limit & abs(diff.vo2.ml.kg.min)<var.limit & abs(diff.rq)<var.limit)
  
  
  if(is.null(accel.path)==FALSE){
    accel <- MoveKC::read_agd(accel.path)
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
  
  data$time.group <- cumsum(c(TRUE, diff(data$timestamp)>1))
  data <- data %>% dplyr::group_by(time.group) %>% dplyr::mutate(steady.state.minutes = dplyr::n())
  
  data <- data[data$steady.state.minutes==max(data$steady.state.minutes), ] # Return Max
  
  if(length(unique(data$time.group))>1){
    data <- data[data$time.group==max(data$time.group), ] # Return last steady state for periods of equivalent length
  }
  
  data <- data[(nrow(data)-4):nrow(data), ] # Return last 5 if max is greater than 5 minutes.
  data$n.obs <- nrow(data) # Number of observations used.
  
  data <- data %>%
    dplyr::select(-c(timestamp, diff.ve.l.min, diff.vo2.ml.kg.min, diff.rq)) %>%
    dplyr::group_by(time.group) %>%
    dplyr::summarise_all(mean) %>%
    dplyr::ungroup()
  data$mod.ml.kg.min <- data$vo2.ml.kg.min*3
  data$vig.ml.kg.min <- data$vo2.ml.kg.min*6
  data <- data[, names(dplyr::select(data, -c(time.group)))]
  return(t(data))
}