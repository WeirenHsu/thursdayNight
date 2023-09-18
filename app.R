###require shiny 1.6.0
###install.packages("https://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.6.0.tar.gz",repos = NULL, type="source")
library(DT)
library(shiny)
library(shinyWidgets)
library(magrittr)
library(purrr)
library(dplyr)
source("./helper.r")

rawdata <- read.csv("./data/data1.csv", header = TRUE, as.is=TRUE, encoding = "UTF-8", na.strings ="")

# data preprocessing
data <- rawdata %>% na_formatter()


###WARNING hard-coded!!!
data$日期 <- as.Date(data$日期)


# data info
n_cols <- ncol(data)
col_names <- colnames(data)
col_types <- sapply(data, class)


ui <- fluidPage(
  titlePanel("Title"),
  fixedRow(
    column(3,
        actionButton("update.filter", "Search"),
        input_constructer(n_cols,col_names,col_types,data)
    ),
    column(6,
      dataTableOutput("data"),
      uiOutput("text")
    )
  )
)

server <- function(input, output) {
  
  dataInput <- eventReactive(
    input$update.filter,{
      do.call(dplyr::filter, filter_maker(col_names, col_types,data,input))
    },
    ignoreNULL = FALSE
  )
  
  output$data <- renderDataTable(
    dataInput()
  )
  
  ### testing
  # output$text <- renderText({
  #   text <- input[["日期"]]
  #   paste(text, class(data[["日期"]]))
  # })
}

shinyApp(ui=ui,server=server)
