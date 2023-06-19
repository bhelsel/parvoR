if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "diff.rq", "diff.ve.l.min", "diff.vo2.ml.kg.min", "vo2.l.min", "ve.l.min",
    "time.min", "time", "read_agd", "Date", " Time", "HR", "Axis1", "Vector.Magnitude",
    "time.group", "timestamp"))

#' @title parvoR: Useful functions to interact with data from the ParvoMedics TrueOne 2400.
#'
#' @description The parvoR package provides several important functions:
#' 
#' \code{\link{parvo_get_oues}}
#' 
#' \code{\link{parvo.extract.data}}
#' 
#' \code{\link{parvo.extract.meta}}
#' 
#' \code{\link{parvo.ree.main}}
#' 
#' \code{\link{parvo.aee.final4}}
#' 
#' @docType package
#' @name parvoR
#' @import dplyr
#' @import magrittr

NULL
