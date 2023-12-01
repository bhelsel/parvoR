# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Extract Meta Data
#' @description Extracts the meta data from Parvo or Cosmed .xlsx files
#' @param path Path name to the .xlsx file.
#' @return Returns a vector with the stored meta data from the .xlsx file.
#' @details Extracts the meta data from Parvo or Cosmed .xlsx files
#' @rdname extract_meta
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_replace
#' @importFrom openxlsx read.xlsx

extract_meta <- function(path){
  
  fc <- unname(unlist(as.vector(openxlsx::read.xlsx(path, colNames = FALSE, cols = 1, rows = 1))))
  if(fc == "ID1"){
    class(path) <- "cosmed"
  } else{
    class(path) <- "parvo"
  }
  
  UseMethod("extract_meta", path)
}


#' @export 
extract_meta.parvo <- function (path) {
  id <- substr(basename(path), 1, 4)
  file <- readxl::read_xlsx(path, col_names = c(paste0("Col", 1:12)))
  meta <- file[1:26,]
  location <- paste0(meta[1, as.vector(!is.na(meta[1, ]))==TRUE], " ", meta[2, as.vector(!is.na(meta[1, ]))==TRUE])
  starttime <- as.POSIXct(paste0(meta[3, 2], "/", meta[3, 4], "/", meta[3, 6], " ", meta[3, 7], ":", meta[3,9], ":", meta[3,10]), format="%Y/%m/%d %H:%M:%S", tz=Sys.timezone())
  name <- paste0(unlist(strsplit(stringr::str_replace(as.character(meta[6,2]), " ", ""), "[,]"))[2], " ", unlist(strsplit(stringr::str_replace(as.character(meta[6,2]), " ", ""), "[,]"))[1])
  age <- as.numeric(meta[7, 2])
  gender <- paste0(meta[7, 5])
  height_in <- paste0(meta[8,2])
  weight_lbs <- round((as.numeric(meta[8,7])), 2)
  room_temp <- as.numeric(paste0(meta[16,2]))*(9/5) + 32
  baro_pres <- round(as.numeric(paste0(meta[16,5])), 2)
  humidity <- round((as.numeric(paste0(meta[17,4]))*100), 2)
  demo <- cbind(id = id, location, starttime = as.character(starttime), name, age, gender, height_in, weight_lbs, room_temp, baro_pres, humidity)
  return(demo)
}


#' @export 
extract_meta.cosmed <- function(path){
  id <- substr(basename(path), 1, 4)
  data <- readxl::read_xlsx(path, sheet = "Data", col_names = FALSE, .name_repair = "unique_quiet")
  colnames(data) <- paste0("v", 1:ncol(data))
  ds <- which(data[1, ] == "t")
  meta <- data[, 1:(ds-1)]
  remove_na <- function(x) {!all(is.na(x))}
  meta <- meta[apply(meta, 1, remove_na), apply(meta, 2, remove_na)]
  location <- NA
  starttime <- as.POSIXct(paste(meta[1, 4], meta[2, 4]), format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  name <- paste(meta[3, 2], meta[2, 2])
  age <- floor(as.numeric(meta[5, 2]))
  gender <- meta[4, 2, drop = TRUE]
  height_cm <- as.numeric(meta[6,2])
  weight_kg <- as.numeric(meta[7,2])
  room_temp <- as.numeric(meta[2,6])*(9/5) + 32
  baro_pres <- as.numeric(meta[1, 6])
  humidity <- as.numeric(meta[3, 6])
  demo <- data.frame(cbind(id = id, location, starttime = as.character(starttime), name, age, gender, height_cm, weight_kg, room_temp, baro_pres, humidity))
  return(demo)
}
