shinyUI(
  fluidPage(
    tags$head(tags$style(HTML("body {font-size: 14px; max-width: 1080px; margin: 0 auto;} a {color: black;}"))),
    titlePanel("ニコニコ検索（仮）"),
    hr(),
    mainPanel(
      h3("タグ検索"),
      textInput(inputId="search_word",label=""),
      actionButton("submit", "検索"),
      width=12
    ),
    hr(),
    mainPanel(
      textOutput(outputId="total_count"),
      hr(),  
      DT::DTOutput(outputId="result_table"),
      width=12
    )
  )
)
