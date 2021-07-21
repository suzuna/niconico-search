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
        shinyWidgets::numericRangeInput(inputId="viewCounter",label="再生数",value=c(1000,1500),separator="～"),
        shinyWidgets::numericRangeInput(inputId="commentCounter",label="コメ数",value=c(NA_real_,NA_real_),separator="～"),
        shinyWidgets::numericRangeInput(inputId="mylistCounter",label="マイリス数",value=c(NA_real_,NA_real_),separator="～"),
        shinyWidgets::numericRangeInput(inputId="likeCounter",label="いいね数",value=c(NA_real_,NA_real_),separator="～"),
        shinyWidgets::numericRangeInput(inputId="lengthMinutes",label="再生時間（分）",value=c(NA_real_,NA_real_),separator="～"),
        "仮",
        fluidRow(
          column(5,numericInput("n1","st",NA_real_)),
          column(2,"～"),
          column(5,numericInput("n2","en",NA_real_)),
        ),
        actionButton(inputId="submit",label="検索"),
        actionButton(inputId="mcprop_desc",label="マイ/コメ降順")
        # style="border:1px solid #eee;",
        # style="padding: 0.5em 1em; margin: 2em 0; background: #BEDDF1;"
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