convert_date_to_POSIXct <- function(date,tz="Asia/Tokyo") {
  date %>% 
    force_tz(tzone=tz) %>% 
    as.POSIXct(tz=tz)
}

#' @param filters a list of lists
complete_cond <- function(filters) {
  filters %>% 
    # fromとtoが両方NAのlistはフィルタをしていないということなので、NULLにする
    map(~{
      if (is.na(.x$from) & is.na(.x$to)) {
        return(NULL)
      } else {
        return(.x)
      }
    }) %>% 
    # 上でNULLにしたものをdiscardすることで、削除する
    discard(is.null) %>% 
    map(~{
      d <- .x %>% 
        # fromとtoのどちらかがNAの場合、それを削除する
        discard(is.na) %>% 
        map_if(~lubridate::is.Date(.),~as.POSIXct(lubridate::force_tz(.,tzone="Asia/Tokyo"),tz="Asia/Tokyo")) %>%
        map_if(is.POSIXct,POSIXct_to_ISO8601chr) %>%
        map_if(is.numeric, as.character)
      if (d$type=="range" & !is.null(d$from)) {
        d$include_lower <- TRUE
      }
      if (d$type=="range" & !is.null(d$to)) {
        d$include_upper <- TRUE
      }
      return(d)
    })
}

sort_df <- function(data,sort_by) {
  param <- switch(sort_by,
    "viewCounter_desc"=list(param_sort_by="viewCounter",is_desc=TRUE,exclude_zero_col=NULL),
    "viewCounter_asc"=list(param_sort_by="viewCounter",is_desc=FALSE,exclude_zero_col=NULL),
    "commentCounter_desc"=list(param_sort_by="commentCounter",is_desc=TRUE,exclude_zero_col=NULL),
    "commentCounter_asc"=list(param_sort_by="commentCounter",is_desc=FALSE,exclude_zero_col=NULL),
    "mylistCounter_desc"=list(param_sort_by="mylistCounter",is_desc=TRUE,exclude_zero_col=NULL),
    "mylistCounter_asc"=list(param_sort_by="mylistCounter",is_desc=FALSE,exclude_zero_col=NULL),
    "likeCounter_desc"=list(param_sort_by="likeCounter",is_desc=TRUE,exclude_zero_col=NULL),
    "likeCounter_asc"=list(param_sort_by="likeCounter",is_desc=FALSE,exclude_zero_col=NULL),
    
    "startTime_desc"=list(param_sort_by="startTime",is_desc=TRUE,exclude_zero_col=NULL),
    "startTime_asc"=list(param_sort_by="startTime",is_desc=FALSE,exclude_zero_col=NULL),
    "length_desc"=list(param_sort_by="lengthSeconds",is_desc=TRUE,exclude_zero_col=NULL),
    "length_asc"=list(param_sort_by="lengthSeconds",is_desc=FALSE,exclude_zero_col=NULL),
    
    "comment_prop_desc"=list(param_sort_by="comment_prop",is_desc=TRUE,exclude_zero_col="viewCounter"),
    "comment_prop_asc"=list(param_sort_by="comment_prop",is_desc=FALSE,exclude_zero_col="viewCounter"),
    "mylist_prop_desc"=list(param_sort_by="mylist_prop",is_desc=TRUE,exclude_zero_col="viewCounter"),
    "mylist_prop_asc"=list(param_sort_by="mylist_prop",is_desc=FALSE,exclude_zero_col="viewCounter"),
    "like_prop_desc"=list(param_sort_by="like_prop",is_desc=TRUE,exclude_zero_col="viewCounter"),
    "like_prop_asc"=list(param_sort_by="like_prop",is_desc=FALSE,exclude_zero_col="viewCounter"),
    
    "mylist_comment_prop_desc"=list(param_sort_by="mylist_comment_prop",is_desc=TRUE,exclude_zero_col="commentCounter"),
    "mylist_comment_prop_asc"=list(param_sort_by="mylist_comment_prop",is_desc=FALSE,exclude_zero_col="commentCounter")
  )
  if (!is.null(param$exclude_zero_col)) {
    data <- data %>% 
      filter(!!as.name(param$exclude_zero_col)>=1)
  }
  if (param$is_desc) {
    data <- data %>% 
      arrange(desc(across(param$param_sort_by)))
  } else {
    data <- data %>% 
      arrange(across(param$param_sort_by))
  }
  return(data)
}

# shiny::withProgressあり版
query2 <- function(q,targets,fields,jsonFilter,sort="-viewCounter",context="apiguide",sleep_time=1) {
  limit <- 100
  if (fields=="all") {
    fields <- "contentId,title,description,userId,channelId,viewCounter,mylistCounter,likeCounter,lengthSeconds,thumbnailUrl,startTime,lastResBody,commentCounter,lastCommentTime,categoryTags,tags,genre"
  }
  fetched_first <- query_at_single_offset(q=q,targets=targets,fields=fields,jsonFilter=jsonFilter,sort=sort,offset=0,limit=limit,context=context)
  totalCount <- fetched_first$meta$totalCount
  max_offset_pages <- ceiling(totalCount/limit)-1
  offsets <- (0:max_offset_pages)*limit
  Sys.sleep(sleep_time)
  
  withProgress({
    res <- offsets %>% 
      purrr::map(~{
        Sys.sleep(sleep_time)
        # incProgress(amount=1/length(0:max_offset_pages),message=str_glue("取得中です ({.x/limit+1}/{max_offset_pages+1})"))
        incProgress(amount=1/length(0:max_offset_pages),message=str_glue("取得中です ({.x}/{totalCount})"))
        data <- query_at_single_offset(q=q,targets=targets,fields=fields,jsonFilter=jsonFilter,sort=sort,offset=.x,limit=limit,context=context)
        cat(stringr::str_glue("{Sys.time()} fetched: {.x+1} - {min(.x+limit,totalCount)}"),"\n")
        return(data)
      })
  })
  res <- list(
    totalCount=totalCount,
    fetched_data=res
  )
  return(res)
}

# 50秒->0:50, 70秒->1:10, 3610秒->1:00:10
# 秒数は常に、分数は1時間以上の時にパディングする（8秒->0:08, 68秒->1:08, 3608秒->1:00:08）
#' @param seconds a numeric
#' @return a character
format_movie_length <- function(seconds) {
  hours <- floor(seconds%/%3600)
  minutes <- floor(seconds%/%60)-hours*60
  seconds <- seconds-(hours*3600+minutes*60)
  
  minutes <- if_else(hours>=1,str_pad(as.character(minutes),width=2,side="left",pad="0"),as.character(minutes))
  seconds <- str_pad(as.character(seconds),width=2,side="left",pad="0")
  
  res <- if_else(hours>=1,str_glue("{hours}:{minutes}:{seconds}"),str_glue("{minutes}:{seconds}"))
  return(res)
}
