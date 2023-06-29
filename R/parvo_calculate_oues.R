#' Copyright © 2023 University of Kansas. All rights reserved.
#'
#' Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title parvo_calculate_oues
#' @description A function to calculate Oxygen Uptake Efficiency Slope at 75%, 90%, and 100% VO2 max measured using a Parvo Medics TrueOne 2400 metabolic cart
#' @param path Path to a single file or directory with the VO2 Max excel files
#' @param return.data Returns the OUES results to the R console, Default: TRUE
#' @param write.summary.file Exports a summary CSV file of VO2 max characteristics for study participants, Default: FALSE
#' @param write.individual.file Exports individual CSV files for study participants, Default: FALSE
#' @param verbose Print the progress of the OUES calculation, Default: FALSE
#' @param ... arguments passed to \code{\link[data.table]{fwrite}}
#' @return Returns the OUES results or writes individual or summary files
#' @details A function to calculate Oxygen Uptake Efficiency Slope at 75%, 90%, and 100% VO2 max measured using a Parvo Medics TrueOne 2400 metabolic cart
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  parvo_calculate_oues(path)
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[data.table]{fwrite}}
#'  \code{\link[plyr]{rbind.fill}}
#'  \code{\link[stats]{lm}}
#' @rdname parvo_calculate_oues
#' @export 
#' @importFrom stringr str_split
#' @importFrom readxl read_xlsx
#' @importFrom data.table fwrite
#' @importFrom plyr rbind.fill
#' @importFrom stats lm

parvo_calculate_oues <- function(path, return.data = TRUE, write.summary.file = FALSE, write.individual.file = FALSE, verbose = FALSE, ...){
  
  if(dir.exists(path)){
    files <- list.files(path, full.names = TRUE, pattern = ".xls$|.xlsx$")
  } else {
    files <- path
  }
  
  main.data <- data.frame()
  
  for(i in 1:length(files)){
    print(files[i])
    d <- dirname(files[i])
    
    data <- readxl::read_xlsx(files[i], col_names = c(paste0("Col", 1:15)))
    location <- unlist(unname(as.vector(data[1, 1])))
    b <- stringr::str_split(basename(files[i]), "_")[[1]]
    
    # Participant Characteristics
    if(location == "University of Nevada") {
      id <- b[1]
      r15 <- c(unname(unlist(data[15, 1])), rep(NA, 14))
      r16 <- c(unname(unlist(as.vector(data[15, 2:15]))), NA)
      data <- rbind(data[1:14, ], r15, r16, data[16:nrow(data), ])
    }
    
    if(location == "KUMC, Center for Physical Activity") id <- paste0(substr(b[2], 1, 1), ".", substr(b[1], 1, 4))
    
    age <- as.numeric(data[7, 2])
    gender <- ifelse(paste0(data[7, 5])=="F", "Female", "Male")
    height_cm <- as.numeric(paste0(data[8,4]))
    weight_kg <- as.numeric(data[8,9])
    participant <- data.frame(id = id, age = age, gender = gender, height_cm = height_cm, weight_kg = weight_kg)
    
    # Parvo Data
    parvo_data <- .parvo_extract_vo2_max_data(data)
    colnames(parvo_data[[1]]) <- c("oues75", "oues90", "oues100")
    hr_bpm <- unique(parvo_data[[2]][which(parvo_data[[2]]$hr_bpm == max(parvo_data[[2]]$hr_bpm, na.rm = TRUE)), "hr_bpm"])
    vo2max <- cbind(parvo_data[[2]][which(parvo_data[[2]]$event=="vo2_max_100"), 2:13], parvo_data[[1]])
    vo2max$hr_bpm <- hr_bpm
    
    parvo_settings <- 
      data.frame(
        Col1 = rbind(
          paste("------------ Data Table Created By R from an Exported Parvo Medics TrueOne 2400 Metabolic Cart on", format(Sys.Date(), "%B %d, %Y"), "-----------"),
          "", "TEST ENVIRONMENT", .parvo_extract_vo2_max_test_env(data), "", "PARTICIPANT CHARACTERISTICS", .parvo_extract_vo2_max_participant_info(data), "",
          cbind(sapply(1:3, function(x) paste(.parvo_extract_vo2_max_meta(data)[[1]][x, 1], .parvo_extract_vo2_max_meta(data)[[1]][x, 2]))), "",
          cbind(sapply(1:3, function(x) paste(.parvo_extract_vo2_max_meta(data)[[2]][x, 1], round(as.numeric(.parvo_extract_vo2_max_meta(data)[[2]][x, 2]), 3)))),
          paste("Time to Max:", round(vo2max[1], 3), "minutes"), paste0("OUES 75: ", round(vo2max$oues75, 4), "; OUES 90: ", round(vo2max$oues90, 4), "; OUES 100: ", round(vo2max$oues100, 4)),
          paste0("--------------------------------------------------")
          )
        )
    
    new_path <- paste0(d, "/", gsub(".xls$|.xlsx$", "_OUES.csv", basename(files[i])))
    
    if(write.individual.file){
      data.table::fwrite(parvo_settings, new_path, append = FALSE, col.names = FALSE, ...)
      data.table::fwrite(parvo_data[[2]], new_path, append = TRUE, col.names = TRUE, ...)  
    }
    main.data <- plyr::rbind.fill(main.data, cbind(id, participant, vo2max))
  }
  
  if(write.summary.file) data.table::fwrite(main.data, paste0(d, "/ouesSummary.csv"), ...)
  if(return.data) return(main.data)
} 


