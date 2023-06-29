if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "diff.rq", "diff.ve.l.min", "diff.vo2.ml.kg.min", "vo2.l.min", "ve.l.min",
    "time.min", "time", "read_agd", "Date", " Time", "HR", "Axis1", "Vector.Magnitude",
    "time.group", "timestamp"))

#' @title parvoR: Useful functions to interact with data from the ParvoMedics TrueOne 2400.
#'
#' @description The parvoR package provides several important functions:
#' 
#' \code{\link{parvo_calculate_oues}}
#' 
#' \code{\link{parvo_extract_data}}
#' 
#' \code{\link{parvo_extract_meta}}
#' 
#' \code{\link{parvo_ree_main}}
#' 
#' \code{\link{parvo_aee_final4}}
#' 
#' @docType package
#' @name parvoR
#' @import dplyr
#' @import magrittr
#' @import MoveKC

NULL
