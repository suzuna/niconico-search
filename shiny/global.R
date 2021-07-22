library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(rlist)
library(jsonlite)
library(DT)
library(Hmisc)
library(shinydashboard)
library(shinycssloaders)
library(shinyPagerUI)
library(shinyWidgets)
options(scipen = 100)


source("../niconico_api.R",encoding="UTF-8")
source("utils.R",encoding="UTF-8")

SLEEP_TIME <- 0.3
ALLOWED_MAX_TOTALCOUNT <- 100000
