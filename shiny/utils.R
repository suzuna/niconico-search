if_na_to_null <- function(x) {
  # if (is.null(x)|is.na(x))は、xがNULLのときにエラーが出る
  if (is.null(x)) {
    res <- NULL
  }
  else if (is.na(x)) {
    res <- NULL
  } else {
    res <- x
  }
  return(res)
}

safely_convert_date_to_POSIXct <- function(date,tz="Asia/Tokyo") {
  if (is.null(date)) {
    res <- NULL
  } else {
    res <- date %>% 
      force_tz(tzone=tz) %>% 
      as.POSIXct(tz=tz) 
  }
  return(res)
}

#' @param x a numeric or integer value, or NULL
convert_seconds_to_minutes <- function(x) {
  if (is.null(x)) {
    res <- NULL
  } else {
    res <- floor(x*60)
  }
  return(res)
}

#' @param filters a list of lists
complete_cond <- function(filters) {
  filters %>% 
    # fromとtoが両方NULLのlistは、フィルタをしていないということなので、NULLにする
    map(~{
      if (is.null(.x$from) & is.null(.x$to)) {
        return(NULL)
      } else {
        return(.x)
      }
    }) %>% 
    # 上でNULLにしたものをdiscardすることで、削除する
    discard(is.null) %>% 
    map(~{
      d <- .x %>% 
        # fromとtoのどちらかがNULLの場合、それを削除する
        discard(is.null) %>% 
        map_if(~lubridate::is.Date(.),~as.POSIXct(lubridate::force_tz(.,tzone="Asia/Tokyo"),tz="Asia/Tokyo")) %>%
        map_if(is.POSIXct,POSIXct_to_ISO8601chr)
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
    "lengthSeconds_desc"=list(param_sort_by="lengthSeconds",is_desc=TRUE,exclude_zero_col=NULL),
    "lengthSeconds_asc"=list(param_sort_by="lengthSeconds",is_desc=FALSE,exclude_zero_col=NULL),
    
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
