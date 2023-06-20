#' @title Parvo Extract Data
#' @description Extracts the Parvo data from the XLSX file.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @param ree Logical value indicating whether you are extracting data from a resting energy expenditure measurement, Default: FALSE
#' @param aee Logical value indicating whether you are extracting data from a activity energy expenditure measurement, Default: FALSE
#' @param time.breaks Time value (e.g., 5 sec, 1 min) to indicate the interval for which the data should be aggregated, Default: '1 sec'
#' @return Returns a data set with the Parvo data.
#' @details Extracts the Parvo data from the XLSX file.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[lubridate]{as_date}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}}
#' @rdname parvo_extract_data
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom lubridate as_datetime

# Extract Observations from VO2 Max Test
parvo_extract_data <- function (parvo.path, ree=FALSE, aee=FALSE, time.breaks = "1 sec") {
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
  starttime <- as.POSIXct(paste0(file[3, 2], "/", file[3, 4], "/", file[3, 6], " ", file[3, 7], ":", file[3,9], ":", file[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  
  if (ree==TRUE){
    vo2 <- as.data.frame(file[32:nrow(file), 1:10])
    colnames(vo2) <- c("time.min", "vo2.ml.min", "vo2.ml.kg.min", "mets", "vco2.ml.min", "ve.l.min", "rq", "feo2%", "feco2%", "ree.kcal.d")
    vo2 <- vo2[is.na(vo2$ree.kcal.d)==FALSE, ]
  }
  
  if(aee==TRUE){
    vo2 <- as.data.frame(file[32:nrow(file), 1:9])
    colnames(vo2) <- c("time.min", "vo2.l.min", "vo2.ml.kg.min", "mets", "rer", "ree.kcal.min", "tm.per.grade", "tm.speed", "ve.l.min")
    vo2 <- vo2[is.na(vo2$tm.speed)==FALSE & vo2$tm.speed!=0, ]
  }
  
  vo2 <- cbind(datetime = (starttime + (as.numeric(vo2$time.min)*60)), vo2)
  #first_record = lubridate::as_datetime(paste0(vo2$date[1], " ", vo2$time[1]), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S")
  #last_record = lubridate::as_datetime(paste0(vo2$date[nrow(vo2)], " ", vo2$time[nrow(vo2)]), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S")
  vo2[names(dplyr::select(vo2, 3:ncol(vo2)))] <- round(sapply(vo2[names(dplyr::select(vo2, 3:ncol(vo2)))], as.numeric), 3)
  `%>%` <- dplyr::`%>%`
  
  n.obs <- vo2 %>% dplyr::group_by(time = format(vo2$datetime, format = "%H:%M")) %>% dplyr::summarise(n = dplyr::n())
  n.obs <- as.vector(n.obs[n.obs$n<6, "time"])
  vo2 <- vo2[!format(vo2$datetime, format="%H:%M") %in% n.obs, ] # Exclude first or last observations if they have fewer than 6 breaths
  
  vo2 <- vo2 %>%
    dplyr::group_by(timestamp = cut(vo2$datetime, breaks = time.breaks)) %>%
    dplyr::summarise_at(c(names(dplyr::select(vo2, 3:ncol(vo2)))), mean) %>%
    dplyr::ungroup()
  vo2$timestamp <- as.POSIXct(strftime(as.character(vo2$timestamp), tz = Sys.timezone(), format = "%Y-%m-%d %H:%M:%S"))
  return(vo2)
}
