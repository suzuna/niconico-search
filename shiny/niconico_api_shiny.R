library(tidyverse)
library(jsonlite)
library(httr)
options(scipen=100)


POSIXct_to_ISO8601chr <- function(datetime) {
  format(datetime,format="%Y-%m-%dT%H:%M:%S+09:00")
}

ISO8601chr_to_POSIXct <- function(str) {
  as.POSIXct(str,format="%Y-%m-%dT%H:%M:%S+09:00",tz="Asia/Tokyo")
}

#' @param filters a list of lists, 
#' each lists must be composed of "type", "field", "from", "to", "include_lower" and "include_upper".
create_jsonFilter <- function(all_type=c("equal","range","or","and","not"),filters) {
  list(
    type=all_type,
    filters=filters
  ) %>%
    jsonlite::toJSON(auto_unbox=TRUE,pretty=TRUE)
}

# filtersはjsonFilterでカバーできるので外した
query_at_single_offset <- function(q,targets,fields,jsonFilter,sort="-viewCounter",offset=0,limit=100,context="apiguide") {
  if (fields=="all") {
    fields <- "contentId,title,description,userId,channelId,viewCounter,mylistCounter,likeCounter,lengthSeconds,thumbnailUrl,startTime,lastResBody,commentCounter,lastCommentTime,categoryTags,tags,genre"
  }
  endpoint <- "https://api.search.nicovideo.jp/api/v2/snapshot/video/contents/search"
  resp <- httr::GET(
    url=endpoint,
    query=list(
      q=q,
      targets=targets,
      fields=fields,
      filters=NULL,
      jsonFilter=jsonFilter,
      `_sort`=sort,
      `_offset`=offset,
      `_limit`=limit,
      `_context`=context
    )
  )
  if (httr::status_code(resp)!=200) {
    stop(str_glue("Error: status_code is not 200 (status_code: {httr::status_code(resp)})
                  status: {httr::content(resp)$meta$status}
                  errorCode: {httr::content(resp)$meta$errorCode}
                  errorMessage: {httr::content(resp)$meta$errorMessage}"))
  }
  fetched <- httr::content(resp)
  fetched_data <- fetched$data %>% 
    purrr::quietly(~rlist::list.stack(.x))() %>% 
    .$result
  fetched_meta <- fetched$meta
  res <- list(data=fetched_data,meta=fetched_meta)
}

query <- function(q,targets,fields,jsonFilter,sort="-viewCounter",context="apiguide",sleep_time=1) {
  limit <- 100
  if (fields=="all") {
    fields <- "contentId,title,description,userId,channelId,viewCounter,mylistCounter,likeCounter,lengthSeconds,thumbnailUrl,startTime,lastResBody,commentCounter,lastCommentTime,categoryTags,tags,genre"
  }
  fetched_first <- query_at_single_offset(q=q,targets=targets,fields=fields,jsonFilter=jsonFilter,sort=sort,offset=0,limit=limit,context=context)
  totalCount <- fetched_first$meta$totalCount
  cat(stringr::str_glue("{Sys.time()} totalCount: {totalCount}"),"\n")
  max_offset_pages <- floor(totalCount/limit)
  if (totalCount==0) {
    cat(stringr::str_glue("{Sys.time()} fetched: 0"),"\n")
    res <- list(
      totalCount=totalCount,
      fetched_data=list(fetched_first)
    )
    return(res)
  }
  
  if (totalCount>=100001) {
    cat(stringr::str_glue("totalCount is more than 100000 (totalCount: {totalCount}), so only 1 - 100000 will be fetched."),"\n")
  }
   
  cat(stringr::str_glue("{Sys.time()} fetched: 1 - {min(limit,totalCount)}"),"\n")
  if (totalCount<limit & totalCount>=1) {
    res <- list(
      totalCount=totalCount,
      fetched_data=list(fetched_first)
    )
  } else if (totalCount>=limit) {
    offsets <- (1:max_offset_pages)*limit
    res <- offsets %>% 
      purrr::map(~{
        Sys.sleep(sleep_time)
        data <- query_at_single_offset(q=q,targets=targets,fields=fields,jsonFilter=jsonFilter,sort=sort,offset=.x,limit=limit,context=context)
        cat(stringr::str_glue("{Sys.time()} fetched: {.x+1} - {min(.x+limit,totalCount)}"),"\n")
        return(data)
      })
    res <- list(
      totalCount=totalCount,
      fetched_data=c(list(fetched_first),res)
    )
  }
  return(res)
}

#' @param data a return of query()
extract_result_from_query <- function(data) {
  data$fetched_data %>% 
    map_dfr("data")
}

fetch_last_modified <- function() {
  endpoint <- "https://api.search.nicovideo.jp/api/v2/snapshot/version"
  httr::GET(url=endpoint) %>% 
    httr::content() %>% 
    .$last_modified
}


# 使用例 ---------------------------------------------------------------------
# cond <- list(
#   list(
#     type="range",
#     field="startTime",
#     from=POSIXct_to_ISO8601chr(as.POSIXct("2020-01-01 00:00:00")),
#     to=POSIXct_to_ISO8601chr(as.POSIXct("2021-07-01 00:00:00")),
#     include_lower=TRUE,
#     include_upper=TRUE
#   ),
#   list(
#     type="range",
#     field="viewCounter",
#     from=0,
#     to=20000,
#     include_lower=TRUE,
#     include_upper=TRUE
#   )
# )
# 
# jf <- create_jsonFilter("and",cond)
# r <- query("きんいろモザイク","tagsExact","all",jf,sort="-viewCounter",context="apitest",sleep_time=0.3)
# extract_result_from_query(r)

# paramを補う
# cond_param <- list(
#   list(type="range",field="startTime",from=as.Date("2020-01-01",tz="Asia/Tokyo"),to=NULL),
#   list(type="range",field="viewCounter",from=3089,to=10000)
# )
# cond_param <- cond_param %>% 
#   map(~{
#     .x %>%
#       discard(is.null) %>% 
#       map_if(~lubridate::is.Date(.),~as.POSIXct(lubridate::force_tz(.,tzone="Asia/Tokyo"),tz="Asia/Tokyo")) %>% 
#       map_if(is.POSIXct,POSIXct_to_ISO8601chr) -> d
#     if (d$type=="range" & !is.null(d$from)) {
#       d$include_lower <- TRUE
#     }
#     if (d$type=="range" & !is.null(d$to)) {
#       d$include_upper <- TRUE
#     }
#     return(d)
#   })
# jf <- cond_param %>% 
#   create_jsonFilter("and",.)
# r <- query("きんいろモザイク 音mad","tagsExact","all",jf,sort="-viewCounter",context="apiguide",sleep_time=0.3)
# extract_result_from_query(r)
