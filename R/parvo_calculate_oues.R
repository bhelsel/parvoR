# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title parvo_calculate_oues
#' @description A function to calculate Oxygen Uptake Efficiency Slope at 75%, 90%, and 100% VO2 max measured using a Parvo Medics TrueOne 2400 metabolic cart
#' @param path Path to a single file or directory with the VO2 Max excel files
#' @param vo2_change A value of which to exclude VO2 values exceeding the limit (e.g., 25 excludes VO2 change ± 25%)
#' @param rer_limit Add a limit to RER values requiring that VO2 max are ≥ the specified threshold
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
#' @importFrom stats lm median

parvo_calculate_oues <- function(path, vo2_change = 10, rer_limit = 1.0, 
                                 return.data = TRUE, write.summary.file = FALSE, 
                                 write.individual.file = FALSE, verbose = FALSE, ...){
  
  if(length(path) == 1){
    if(dir.exists(path)){
      files <- list.files(path, full.names = TRUE, pattern = ".xls$|.xlsx$")
    } else{
      files <- path
    }
  } else {
    files <- path
  }
  
  main.data <- data.frame()
  
  for(i in 1:length(files)){
    print(basename(files[i]))
    d <- dirname(files[i])
    
    data <- openxlsx::read.xlsx(files[i], colNames = FALSE)
    meta <- extract_meta(files[i])
    location <- meta[, "location"]
    bn <- stringr::str_split(basename(files[i]), "_")[[1]]
    
    # Participant Characteristics
    if(location == "University of Nevada") {
      id <- bn[1]
      r15 <- c(unname(unlist(data[15, 1])), rep(NA, 14))
      r16 <- c(unname(unlist(as.vector(data[15, 2:15]))), NA)
      data <- rbind(data[1:14, ], r15, r16, data[16:nrow(data), ])
    }
    
    if(grepl("KUMC", location)) id <- paste0(substr(bn[2], 1, 1), ".", substr(bn[1], 1, 4))
    
    age <- as.numeric(meta[, "age"])
    gender <- unname(ifelse(meta[, "gender"] == "F", "Female", "Male"))
    height_cm <- as.numeric(meta[, "height_in"]) * 2.54
    weight_kg <- as.numeric(meta[, "weight_lbs"]) / 2.205
    participant <- data.frame(id = id, age = age, gender = gender, height_cm = height_cm, weight_kg = weight_kg)
    
    # Parvo Data
    parvo_data <- .parvo_extract_vo2_max_data(data, vo2_change, rer_limit)
    colnames(parvo_data[[1]]) <- c("oues75", "oues90", "oues100")
    if("vo2_max_100" %in% parvo_data[[2]]$event){
      vo2max <- dplyr::tibble(cbind(parvo_data[[2]][which(parvo_data[[2]]$event=="vo2_max_100"), ], parvo_data[[1]]))
      vo2max$rer <- max(parvo_data[[2]]$rer, na.rm = TRUE)
    } else{
      temp <- matrix(NA, ncol = ncol(parvo_data[[2]]) + ncol(parvo_data[[1]]))
      colnames(temp) <- c(colnames(parvo_data[[2]]), colnames(parvo_data[[1]]))
      vo2max <- dplyr::as_tibble(temp)
    }
    
    if("hr_bpm" %in% names(parvo_data[[2]])) vo2max$hr_bpm <- max(parvo_data[[2]]$hr_bpm, na.rm = TRUE)
    
    env <- .parvo_extract_vo2_max_test_env(data)
    info <- .parvo_extract_vo2_max_participant_info(data)
    meta <- .parvo_extract_vo2_max_meta(data)
    
    parvo_settings <- 
      data.frame(
        Col1 = rbind(
          paste("------------ Data Table Created By R from an Exported Parvo Medics TrueOne 2400 Metabolic Cart on", format(Sys.Date(), "%B %d, %Y"), "-----------"),
          "", "TEST ENVIRONMENT", env, "", "PARTICIPANT CHARACTERISTICS", info, "",
          cbind(paste0(meta[[1]][, 1, drop = TRUE], ": ", meta[[1]][, 2, drop = TRUE])), "",
          rbind(paste0("VO2 (L/min): ", vo2max$vo2_l_min), 
                paste0("VO2 (ml/kg/min): ", vo2max$vo2_kg_ml_kg_m), 
                paste0("METS: ", vo2max$mets)), "",
          paste("Time to Max:", round(vo2max$time_min, 3), "minutes"), 
          paste0("OUES 75: ", round(vo2max$oues75, 4), "; OUES 90: ", round(vo2max$oues90, 4), "; OUES 100: ", round(vo2max$oues100, 4)),
          paste0("--------------------------------------------------")
          )
        )
    
    new_path <- paste0(d, "/", gsub(".xls$|.xlsx$", "_OUES.csv", basename(files[i])))
    
    if(write.individual.file){
      data.table::fwrite(parvo_settings, new_path, append = FALSE, col.names = FALSE, ...)
      data.table::fwrite(parvo_data[[2]], new_path, append = TRUE, col.names = TRUE, ...)  
    }
    main.data <- plyr::rbind.fill(main.data, dplyr::tibble(participant, vo2max))
  }
  
  if(write.summary.file) data.table::fwrite(main.data, paste0(d, "/ouesSummary.csv"), ...)
  if(return.data) return(main.data)
} 


