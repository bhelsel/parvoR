# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Data
#' @description Extracts data from Parvo or Cosmed .xlsx files.
#' @param path Path name to the Parvo or Cosmed .xlsx files
#' @param ... Additional arguments passed to extract_data.parvo or extract_data.cosmed. 
#'    See details for more information.
#' @return Returns a data set with the Parvo or Cosmed data.
#' @details Extracts data from the Parvo or Cosmed .xlsx file. Additional arguments
#'    can be passed to extract_data.parvo or extract_data.cosmed.
#'    Currently, these functions only take a time_breaks argument which will return the
#'    averages of the numeric variables at the specified time interval. Time values may include
#'    any value that can be passed to the unit argument in the \code{\link[lubridate]{floor_date}} function
#'    (e.g., 5 sec, 1 min, etc.). The default is set to return data at a '1 sec' interval to keep the data
#'    the same as the input .xlsx file.
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[lubridate]{as_date}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname extract_data
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom lubridate floor_date
#' @importFrom stats complete.cases
#' @importFrom openxlsx read.xlsx

extract_data <- function(path, ...){
  
  fc <- unname(unlist(as.vector(openxlsx::read.xlsx(path, colNames = FALSE, cols = 1, rows = 1))))
  if(fc == "ID1"){
    class(path) <- "cosmed"
  } else{
    class(path) <- "parvo"
  }
  
  UseMethod("extract_data", path)
}


#' @export 
extract_data.parvo <- function (path, ...) {
  input = list(...)
  if(!"time_breaks" %in% names(input)) {
    time_breaks <- "1 sec"
  } else{
    time_breaks <- input$time_breaks
  }
  data <- dplyr::as_tibble(openxlsx::read.xlsx(path, colNames = FALSE))
  ts <- paste0(data[3, 2], "/", data[3, 4], "/", data[3, 6], " ", data[3, 7], ":", data[3,9], ":", data[3,10])
  starttime <- as.POSIXct(ts, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
  ds <- grep("TIME", data[, 1, drop = TRUE])
  data <- data[ds:nrow(data), ]
  data <- name_repair(data, rows = 1:3)
  data <- data[stats::complete.cases(data), ]
  data <- dplyr::as_tibble(apply(data, 2, as.numeric))
  data$timestamp <- starttime + (data$time_min * 60)
  colnames(data)[1] <- "elapsed_time"
  data <- cbind(data[, "timestamp"], data[, -which(colnames(data) == "timestamp")])
  #if(aee==TRUE) vo2 <- vo2[is.na(vo2$tm.speed)==FALSE & vo2$tm.speed!=0, ]
  data <- aggregate_time(data, time_breaks)
  return(data)
}


#' @export 
extract_data.cosmed <- function(path, ...){
  input = list(...)
  if(!"time_breaks" %in% names(input)) {
    time_breaks <- "1 sec"
  } else{
    time_breaks <- input$time_breaks
  }
  data <- dplyr::as_tibble(openxlsx::read.xlsx(path, colNames = FALSE))
  starttime <- as.POSIXct(paste(data[1, 4], data[2, 4]), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  ds <- which(data[1, ] == "t")
  data <- data[, ds:ncol(data)]
  data <- name_repair(data, rows = c(1:2))
  data <- dplyr::as_tibble(suppressWarnings(apply(data, 2, as.numeric)))
  remove_na <- function(x) {!all(is.na(x))}
  data <- data[apply(data, 1, remove_na), apply(data, 2, remove_na)]
  data$t_s <- starttime + (data$t_s * 24 * 3600)
  colnames(data)[1] <- "time"
  data <- aggregate_time(data, time_breaks)
  return(data)
}
