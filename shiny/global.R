library(tidyverse)
library(lubridate)
library(rvest)
library(httr)
library(rlist)
library(jsonlite)
library(shinydashboard)
library(shinycssloaders)
library(shinyPagerUI)
library(shinyWidgets)
library(scroller)
options(scipen = 100)


source("niconico_api_shiny.R",encoding="UTF-8")
source("utils.R",encoding="UTF-8")

SLEEP_TIME <- 0.5
ALLOWED_MAX_TOTALCOUNT <- 100000
ONEPAGE_NUM <- 50

LAST_QUERY <- list(
  q=NULL,
  targets=NULL,
  startTime_from=NULL,startTime_to=NULL,
  viewCounter_from=NULL,viewCounter_to=NULL,
  commentCounter_from=NULL,commentCounter_to=NULL,
  mylistCounter_from=NULL,mylistCounter_to=NULL,
  likeCounter_from=NULL,likeCounter_to=NULL,
  lengthSeconds_from=NULL,lengthSeconds_to=NULL
)
LAST_FETCHED_DATA <- NULL
