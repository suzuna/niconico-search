shinyUI(
  fluidPage(
    # tags$head(includeHTML("google-analytics.html")),
    tags$head(includeScript("returnclick.js")),
    # tags$head(tags$style(HTML("body {font-size: 14px; max-width: 1080px; margin: 0 auto;} a {color: black;}"))),
    # tags$head(tags$style(HTML("div.box {background: #BEDDF1;}"))),
    tags$head(includeCSS("main.css")),
    titlePanel("ニコニコ検索（仮）"),
    tags$ul(
      tags$li("全件取得するので時間がかかる"),
      tags$li("マイリス率やコメ率、マイリス/コメソートができる")
    ),
    hr(),
    sidebarLayout(
      sidebarPanel(
        textInput(inputId="q",label=NULL,value="きんいろモザイク 音MAD", placeholder="検索ワード"),
        radioButtons(inputId="targets",label="検索モード",
                     choices=c("キーワード検索"="title,description,tags","タグ検索"="tagsExact"),inline=TRUE),
        dateRangeInput(inputId="startTime",label="投稿日",separator="～",start=NA_Date_,end=NA_Date_,format="yyyy/mm/dd"),
        fluidRow(
          column(6,numericInput(inputId="viewCounter_from",label="再生：下限",value=1000,step=1,width="100%")),
          column(6,numericInput(inputId="viewCounter_to",label="再生：上限",value=1500,step=1,width="100%"))
        ),
        fluidRow(
          column(6,numericInput(inputId="commentCounter_from",label="コメ：下限",value=NA_real_,step=1,width="100%")),
          column(6,numericInput(inputId="commentCounter_to",label="コメ：上限",value=NA_real_,step=1,width="100%"))
        ),
        fluidRow(
          column(6,numericInput(inputId="mylistCounter_from",label="マイリス：下限",value=NA_real_,step=1,width="100%")),
          column(6,numericInput(inputId="mylistCounter_to",label="マイリス：上限",value=NA_real_,step=1,width="100%"))
        ),
        fluidRow(
          column(6,numericInput(inputId="likeCounter_from",label="いいね：下限",value=NA_real_,step=1,width="100%")),
          column(6,numericInput(inputId="likeCounter_to",label="いいね：上限",value=NA_real_,step=1,width="100%"))
        ),
        fluidRow(
          column(6,numericInput(inputId="lengthMinutes_from",label="再生分数：下限",value=NA_real_,width="100%")),
          column(6,numericInput(inputId="lengthMinutes_to",label="再生分数：上限",value=NA_real_,width="100%"))
        ),
        actionButton(inputId="submit",label="検索"),
        actionButton(inputId="mcprop_desc",label="マイ/コメ降順")
      ),
      # mainPanel(
      #   textOutput(outputId="last_modified"),
      #   textOutput(outputId="totalCount"),
      #   DT::DTOutput(outputId="result"),
      #   width=8
      # )
      mainPanel(
        textOutput(outputId="last_modified"),
        textOutput(outputId="totalCount"),
        uiOutput(outputId="result"),
        # br()
      )
    )
  )
)