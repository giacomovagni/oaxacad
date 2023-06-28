#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#
library("roxygen2")
library("devtools")
#
roxygenise()
devtools::document()
#
library(magrittr)
library(broom)
library(dplyr)
library(ggplot2)
library(oaxaca)
library(gtools)
#

#
library("oaxacad")
#
# oaxacad::oaxaca_data()
#

#
# sim1 = oaxaca_sim()
# sim1$decomp_reg
#

# library("usethis")
# use_mit_license("Giacomo Vagni")
# usethis::use_package("dplyr")
#
