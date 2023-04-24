`%!in%` <- Negate(`%in%`)
libs <- c("shiny","patchwork","thematic", "plotly","tidyverse","mapdeck", "textshaping", "ragg", 
          "shinyWidgets","shinythemes","rdrop2", "cowplot", "magrittr", 
          "qs","data.table","bs4Dash","shinycssloaders", "shinyjs","fresh")
#source("R/functions.R")

#if("devtools" %!in% installed.packages()[,"Package"]) install.packages("devtools")
# update_github_pkgs()

# Инсталляция отсутствующих не гитхаб библиотек
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Включение библиотек
suppressMessages(lapply(libs, require, character.only = TRUE))




