shinyServer(
  function(input,output,session){
    
    # データの取得部分 ----------------------------------------------------------------
    reactive_data <- eventReactive(input$submit,{
      # インプットの整理 ----------------------------------------------------------------
      q <- if_else(is.na(input$q),"",input$q)
      targets <- input$targets
      startTime_from <- convert_date_to_POSIXct(input$startTime[1])
      startTime_to <- convert_date_to_POSIXct(input$startTime[2])
      viewCounter_from <- input$viewCounter_from
      viewCounter_to <- input$viewCounter_to
      commentCounter_from <- input$commentCounter_from
      commentCounter_to <- input$commentCounter_to
      mylistCounter_from <- input$mylistCounter_from
      mylistCounter_to <- input$mylistCounter_to
      likeCounter_from <- input$likeCounter_from
      likeCounter_to <- input$likeCounter_to
      lengthSeconds_from <- floor(input$lengthMinutes_from*60)
      lengthSeconds_to <- floor(input$lengthMinutes_to*60)
      
      cat(str_glue("input_startTime[1]: {(input$startTime[1])} input_startTime[2]: {(input$startTime[2])}"),"\n")
      cat(str_glue("input_viewCounter_from: {(input$viewCounter_from)} input_viewCounter_to: {(input$viewCounter_to)}"),"\n")
      
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
      cat(str_glue("totalCount: {totalCount}"),"\n")
      
      
      # totalCount=0,>=100001の除外処理 ----------------------------------------------
      if (totalCount==0 | totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        return(list(q=q,last_modified=last_modified,totalCount=totalCount,df=NULL))
      }
      
      
      # 検索する -------------------------------------------------------------------
      fetched <- query2(q,targets,fields,jf,sort,context="apiguide",SLEEP_TIME)
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
    

    # ボタンでの投稿日時の更新 -----------------------------------------------------------------
    observeEvent(input$last_1m,{
      date_end <- as.Date(lubridate::with_tz(Sys.time(),tzone="Asia/Tokyo"),tz="Asia/Tokyo")
      date_start <- date_end-months(1)
      updateDateRangeInput(session,inputId="startTime",start=date_start,end=date_end)
    })
    observeEvent(input$last_1y,{
      date_end <- as.Date(lubridate::with_tz(Sys.time(),tzone="Asia/Tokyo"),tz="Asia/Tokyo")
      date_start <- date_end-lubridate::years(1)
      updateDateRangeInput(session,inputId="startTime",start=date_start,end=date_end)
    })


    # output部分 ----------------------------------------------------------------
    output$totalCount <- renderUI({
      bold_q <- tags$b(reactive_data()$q)
      bold_totalCount <- tags$b(reactive_data()$totalCount)
      
      if (reactive_data()$totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        str_glue("検索ワード {bold_q} での検索結果: {bold_totalCount} 件{br()}
               ※検索できる上限の件数は{ALLOWED_MAX_TOTALCOUNT}件です。件数を絞ってください。") %>% 
          HTML()
      } else {
        str_glue("検索ワード {bold_q} での検索結果: {bold_totalCount} 件") %>% 
          HTML()
      }
    })
    
    output$last_modified <- renderText({
      based_date_chr <- as.character(as.Date(reactive_data()$last_modified,tz="Asia/Tokyo"),format="%Y/%m/%d")
      based_dttm_chr <- str_c(based_date_chr,"05:00:00",sep=" ")
      last_modified_chr <- as.character(reactive_data()$last_modified,format="%Y/%m/%d %H:%M:%S")
      
      str_glue("{based_dttm_chr}時点（生成日時：{last_modified_chr}）")
    })
      
    output$result <- renderUI({
      if (reactive_data()$totalCount==0 | reactive_data()$totalCount>=ALLOWED_MAX_TOTALCOUNT+1) {
        return(NULL)
      }
      
      now_page <- input$pager_bottom$page_current
      row_start <- (now_page-1)*ONEPAGE_NUM+1
      row_end <- now_page*ONEPAGE_NUM

      df <- sort_df(reactive_data()$df,isolate(input$sort_by))
      df <- df %>%
        mutate(startTime=as.character(startTime,format="%Y/%m/%d %H:%M:%S")) %>% 
        mutate(rank=row_number())
      df_base <- df
      df <- df %>% 
        slice(row_start:row_end)
      
      if (nrow(df)==0) {
        return(NULL)
      }
      
      res <- map(1:nrow(df),~{
        rank <- df$rank[.x]
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
        mc_prop_chr <- ifelse(is.infinite(mc_prop)|is.nan(mc_prop),"-",
                              scales::number(round(mc_prop,digits=1),accuracy=0.1))
        
        fluidRow(
          column(
            12,
            shinydashboard::box(
              fluidRow(
                column(3,str_glue("{rank}位")),
                column(9,startTime_chr)
              ),
              fluidRow(
                column(3,tags$a(href=url,tags$img(src=url_thumbnail),target="_blank",rel="noopener noreferrer")),
                column(
                  9,
                  tags$a(href=url,title,target="_blank",rel="noopener noreferrer",id="movie_title"),
                  tags$div(
                    str_glue("再生 {vc_chr}　コメ {cc_chr} ({cv_prop_chr})　マイ {mc_chr} ({mv_prop_chr})"),
                    br(),
                    str_glue("いいね！ {lc_chr} ({lv_prop_chr})　マイ/コメ {mc_prop_chr}"),
                    id="movie_stat"
                  ),
                  id="movie_info"
                )
              ),
              width=12
            )
          )
        )
      })
      shinyPagerUI::updatePageruiInput(
        session,inputId="pager_bottom",
        page_current=now_page,
        pages_total=ceiling(nrow(df_base)/ONEPAGE_NUM)
      )
      return(res)
    })
  })
