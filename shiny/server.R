shinyServer(
  function(input, output, session){
    Data=eventReactive(input$submit,{
      # インプットの整理 ----------------------------------------------------------------
      q <- input$q
      targets <- input$targets
      startTime_from <- safely_convert_date_to_POSIXct(if_na_to_null(input$startTime_from))
      startTime_to <- safely_convert_date_to_POSIXct(if_na_to_null(input$startTime_to))
      viewCounter_from <- if_na_to_null(input$viewCounter_from)
      viewCounter_to <- if_na_to_null(input$viewCounter_to)
      commentCounter_from <- if_na_to_null(input$commentCounter_from)
      commentCounter_to <- if_na_to_null(input$commentCounter_to)
      mylistCounter_from <- if_na_to_null(input$mylistCounter_from)
      mylistCounter_to <- if_na_to_null(input$mylistCounter_to)
      likeCounter_from <- if_na_to_null(input$likeCounter_from)
      likeCounter_to <- if_na_to_null(input$likeCounter_to)
      lengthSeconds_from <- convert_seconds_to_minutes(if_na_to_null(input$lengthMinutes_from))
      lengthSeconds_to <- convert_seconds_to_minutes(if_na_to_null(input$lengthMinutes_to))
      
      print(input$startTime)
      print(input$viewCounter_from)
      print(input$viewCounter_to)
      print(viewCounter_from)
      print(viewCounter_to)
      
      now_query <- list(
        q=q,
        targets=targets,
        startTime_from=startTime_from,startTime_to=startTime_to,
        viewCounter_from=viewCounter_from,viewCounter_to=viewCounter_to,
        commentCounter_from=commentCounter_from,commentCounter_to=commentCounter_to,
        mylistCounter_from=mylistCounter_from,mylistCounter_to=mylistCounter_to,
        likeCounter_from=likeCounter_from,likeCounter_to=likeCounter_to,
        lengthSeconds_from=lengthSeconds_from,lengthSeconds_to=lengthSeconds_to
      )
      if (identical(now_query,LAST_QUERY)) {
        fetched_data <- LAST_FETCHED_DATA
        return(list(q=q,last_modified=fetched_data$last_modified,totalCount=fetched_data$totalCount,df=fetched_data$df))
      }
      
      LAST_QUERY <<- now_query
      # 最終更新日時 ------------------------------------------------------------------
      last_modified <- fetch_last_modified() %>% ISO8601chr_to_POSIXct()
      
      
      # jsonFilterを作る ----------------------------------------------------------------------
      cond_param <- list(
        list(type="range",field="startTime",from=startTime_from,to=startTime_to),
        list(type="range",field="viewCounter",from=viewCounter_from,to=viewCounter_to),
        list(type="range",field="commentCounter",from=commentCounter_from,to=commentCounter_to),
        list(type="range",field="mylistCounter",from=mylistCounter_from,to=mylistCounter_to),
        list(type="range",field="likeCounter",from=likeCounter_from,to=likeCounter_to),
        list(type="range",field="lengthSeconds",from=lengthSeconds_from,to=lengthSeconds_to)
      )
      cond_param <- complete_cond(cond_param)
      jf <- create_jsonFilter("and",cond_param)
      
      
      # totalCountを取得（後続のエラー処理に使う） --------------------------------------------------------------------
      fields <- "all"
      sort <- "-viewCounter"
      totalCount <- query_at_single_offset(q,targets,fields,jf,sort,0,100,"apiguide")$meta$totalCount
      
      
      # totalCount=0,>=100001の除外処理 ----------------------------------------------
      if (totalCount==0 | totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        return(list(q=q,last_modified=last_modified,totalCount=totalCount,df=NULL))
      }
      
      
      # 検索する -------------------------------------------------------------------
      fetched <- query(q,targets,fields,jf,sort,context="apiguide",SLEEP_TIME)
      df <- extract_result_from_query(fetched)
      
      
      # 結果の整形 --------------------------------------------------------------------
      df <- df %>%
        mutate(
          comment_prop=commentCounter/viewCounter,
          mylist_prop=mylistCounter/viewCounter,
          like_prop=likeCounter/viewCounter,
          mylist_comment_prop=mylistCounter/commentCounter
        ) %>%
        mutate(url=str_glue("https://www.nicovideo.jp/watch/{contentId}")) %>%
        mutate(startTime=ISO8601chr_to_POSIXct(startTime)) %>%
        select(
          thumbnailUrl,url,title,startTime,
          viewCounter,commentCounter,comment_prop,mylistCounter,mylist_prop,
          likeCounter,like_prop,mylist_comment_prop
        )
      fetched_data <- list(q=q,last_modified=last_modified,totalCount=totalCount,df=df)
      LAST_FETCHED_DATA <<- fetched_data
      return(list(q=q,last_modified=fetched_data$last_modified,totalCount=fetched_data$totalCount,df=fetched_data$df))
    })
    
    output$last_modified <- renderText({
      based_date_chr <- as.character(as.Date(Data()$last_modified,tz="Asia/Tokyo"),format="%Y/%m/%d")
      based_dttm_chr <- str_c(based_date_chr,"05:00",sep=" ")
      last_modified_chr <- as.character(Data()$last_modified,format="%Y/%m/%d %H:%M:%S")
      
      str_glue("{based_dttm_chr}時点（生成日時：{last_modified_chr}）")
    })
    
    output$totalCount <- renderUI({
      if (Data()$totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        str_glue("{Data()$q}での検索結果: {Data()$totalCount}件{br()}
               ※上限：{ALLOWED_MAX_TOTALCOUNT}件です。件数を絞ってください。") %>% 
          HTML()
      } else {
        str_glue('"{Data()$q}"での検索結果: {Data()$totalCount}件')
      }
    })
      
    output$result <- renderUI({
      if (Data()$totalCount==0 | Data()$totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        return(NULL)
      }
      df <- sort_df(Data()$df,isolate(input$sort_by))
      df <- df %>%
        mutate(startTime=as.character(startTime,format="%Y/%m/%d %H:%M:%S"))
      
      if (nrow(df)==0) {
        return(NULL)
      }
      
      map(1:nrow(df),~{
        startTime_chr <- df$startTime[.x]
        url <- df$url[.x]
        url_thumbnail <- df$thumbnailUrl[.x]
        title <- df$title[.x]
        vc <- df$viewCounter[.x]
        cc <- df$commentCounter[.x]
        mc <- df$mylistCounter[.x]
        lc <- df$likeCounter[.x]
        cv_prop <- df$comment_prop[.x]
        mv_prop <- df$mylist_prop[.x]
        lv_prop <- df$like_prop[.x]
        mc_prop <- df$mylist_comment_prop[.x]
        
        vc_chr <- scales::comma(vc)
        cc_chr <- scales::comma(cc)
        mc_chr <- scales::comma(mc)
        lc_chr <- scales::comma(lc)
        cv_prop_chr <- scales::percent(cv_prop,accuracy=0.1)
        mv_prop_chr <- scales::percent(mv_prop,accuracy=0.1)
        lv_prop_chr <- scales::percent(lv_prop,accuracy=0.1)
        mc_prop_chr <- ifelse(is.infinite(mc_prop)|is.nan(mc_prop),"-",round(mc_prop,digits=1))
        
        fluidRow(
          column(
            12,
            shinydashboard::box(
              fluidRow(
                column(4,str_glue("{.x}位")),
                column(8,startTime_chr)
              ),
              fluidRow(
                column(4,tags$img(src=url_thumbnail)),
                column(8,tags$a(href=url,title))
              ),
              fluidRow(
                column(
                  12,
                  str_glue("再生 {vc_chr}　コメ {cc_chr} ({cv_prop_chr})　マイ {mc_chr} ({mv_prop_chr})　いいね {lc_chr} ({lv_prop_chr})　マイ/コメ {mc_prop_chr}")
                )
              ),
              width=12
            )
          )
        )
      })
    })
  })
