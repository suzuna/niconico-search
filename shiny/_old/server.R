shinyServer(
  function(input, output, session){
    Data=eventReactive(input$submit,{
      
      fields_base <- "contentId,title,description,viewCounter,mylistCounter,lengthSeconds,thumbnailUrl,startTime,lastResBody,commentCounter,lastCommentTime,categoryTags,tags,genre"
      total_count <- make_url(input$search_word,"tagsExact",fields_base,NULL,NULL,"-viewCounter",0,1,"apiguide") %>% 
        fromJSON() %>% 
        .$meta %>% 
        .$totalCount
      # total_count <- 100
      
      res <- map_chr(seq(0,total_count-100,100),~{
        make_url(input$search_word,"tagsExact",fields_base,NULL,NULL,"-viewCounter",.x,100,"apiguide")
      }) %>% 
        map(~{
          Sys.sleep(1)
          cat(str_glue("{Sys.time()} {.x}"),"\n")
          fromJSON(.x)
        }) %>% 
        map_dfr("data")
      res <- res %>% 
        mutate(
          comment_prop=commentCounter/viewCounter,
          mylist_prop=mylistCounter/viewCounter,
          mylist_comment_prop=mylistCounter/commentCounter
        ) %>% 
        mutate(across(c(comment_prop,mylist_prop,mylist_comment_prop),~round(.x,digits=3))) %>% 
        mutate(
          thumbnail=str_glue("<img src='{thumbnailUrl}' height='50px'></img>"),
          url=str_glue("https://www.nicovideo.jp/watch/{contentId}"),
          title=str_glue("<a href='{url}' target='_blank' rel='noopener noreferrer'>{title}</a>")
        ) %>% 
        mutate(startTime=as.POSIXct(startTime,format="%Y-%m-%dT%H:%M:%S+09:00") %>% 
                 as.character(format="%Y/%m/%d %H:%M")) %>% 
        select(thumbnail,title,startTime,viewCounter,commentCounter,comment_prop,mylistCounter,mylist_prop,mylist_comment_prop) %>% 
        rename(
          サムネイル=thumbnail,
          タイトル=title,
          投稿日時=startTime,
          再生=viewCounter,
          コメ=commentCounter,
          コメ率=comment_prop,
          マイ=mylistCounter,
          マイ率=mylist_prop,
          `マイ/コメ`=mylist_comment_prop
        )
      return(list(total_count=total_count,res=res))
    })
    output$total_count <- renderText(
      Data()$total_count
    )
    output$result_table <- DT::renderDT(
      Data()$res %>% 
        DT::datatable(
          class="row-border",rownames=TRUE,escape=FALSE,
          options=list(
            pageLength=1000,
            scrollX=TRUE,
            ordering=TRUE,
            select=FALSE,
            autoWidth=TRUE,
            columnDefs=list(
              list(width="5rem",targets=1),
              list(width="10rem",targets=2),
              list(width="6rem",targets=3)
            )
          )
        ) %>% 
        DT::formatCurrency(c("再生","コメ","マイ"),currency="",digits=0,interval=3,mark=",") %>% 
        DT::formatPercentage(c("コメ率","マイ率"),digits=1,interval=3,mark=",") %>% 
        DT::formatRound("マイ/コメ",digits=1,interval=3,mark=",")
      )
  })
