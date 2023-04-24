library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)



word <-
  read_excel("../data/Client Communication Working Session(1-1) (2).xlsx") %>%
  pivot_longer(cols = starts_with("Provide"),
               names_to = "Term",
               values_to = "Word")

liket <-
  read_excel("../data/Client Communication Working Session(1-1).xlsx") %>%
  pivot_longer(
    .,
    cols = c("Statement 1", "Statement 2"),
    names_to = "Factor",
    values_to = "Value"
  ) %>%
  mutate(Value = as.numeric(Value))


ui <- dashboardPage(
  dashboardHeader(title = "Client Communication"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "WordCloud",
      tabName = "wordCloud",
      icon = icon("wordpress")
    ),
    menuItem("Liket", tabName = "liket", icon = icon("chat-bar"))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "wordCloud",
            wordcloud2Output("wordcloud")),
    tabItem(tabName = "liket", fluidRow(
      box(title = "Importance of Factors",
          plotOutput("barplot", height = 250)),
      box(
        title = "Controls",
        radioButtons(
          "graph",
          "Select Graph Type:",
          choices = c("bar plot", "box plot"),
          selected = "bar plot"
        )
      )
    ))
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  text <- word$Word
  # Create a corpus
  docs <- Corpus(VectorSource(text))

  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))

  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing = TRUE)
  df <- data.frame(word = names(words), freq = words)

  wordData <- reactive({
    return(df)
  })


  output$wordcloud <- renderWordcloud2({
    browser()
    wordcloud2(wordData())
  })

  output$barplot <- renderPlot({
    switch(input$graph,
           "bar plot" = {
             ggplot(liket, aes(x = reorder(Factor,-Value), y = Value)) +
               geom_bar(stat = "identity") +
               labs(x = "Factor", y = "Importance (1-5 scale)") +
               coord_flip() +
               theme_minimal()
           },
           "box plot" = {
             ggplot(liket, aes(x = reorder(Factor, Value), y = Value)) +
               geom_boxplot() +
               coord_flip() +
               labs(x = "Factor", y = "Importance (1-5 scale)") +
               theme_minimal()
           })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
