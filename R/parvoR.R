# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

if(getRversion() >= "2.15.1")  utils::globalVariables(
  c(".", "time", "read_agd", "Date", " Time", "HR", "Axis1", "Vector.Magnitude",
    "group", "minutes", "time.min", "ve.l.min", "Measure", "Values", "changem", 
    "datetime", "rer", "time_min", "ve_l_min", "vo2_kg_ml_min_kg", "vo2_l_min",
    "vo2_kg_ml_kg_m"))

#' @title parvoR: Useful functions to interact with data from the ParvoMedics TrueOne 2400.
#'
#' @description The parvoR package provides several important functions:
#' 
#' \code{\link{parvo_calculate_oues}}
#' 
#' \code{\link{extract_data}}
#' 
#' \code{\link{extract_meta}}
#' 
#' \code{\link{calculate_rmr}}
#'
#' \code{\link{calculate_rmr_cv}}
#' 
#' \code{\link{parvo_aee_final4}}
#' 
#' @docType package
#' @name parvoR
#' @import dplyr
#' @import magrittr
#' @import MoveKC

NULL
