#' @title Parvo Extract Meta Data
#' @description Extracts the Parvo meta data from the top of the XLSX file.
#' @param parvo.path Pathname to the Parvo XLSX file.
#' @return Returns a vector with the stored meta data from the Parvo XLSX file.
#' @details Extracts the Parvo meta data from the top of the XLSX file.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname parvo_extract_meta
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom stringr str_replace

# Extract Meta Data
parvo_extract_meta <- function (parvo.path) {
  id <- substr(basename(parvo.path), 1, 4)
  file <- readxl::read_xlsx(parvo.path, col_names = c(paste0("Col", 1:12)))
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