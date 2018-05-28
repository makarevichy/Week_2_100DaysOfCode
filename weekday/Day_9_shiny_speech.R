#You may see app here: https://makarevich.shinyapps.io/rech/
library(xlsx)
library(shiny)
df <- read.xlsx('data/unicode.xlsx', sheetIndex = 1, encoding = 'UTF-8')
for(i in seq_along(df)){
  df[,i] <- as.character(df[,i])
}
unicode <-  function(x = df, len = 5){
  senten <- vector('list', 0)
  for(i in 1:len){
    sen_1 <- sample(x[,1], 1)
    sen_2 <- sample(x[,2], 1)
    sen_3 <- sample(x[,3], 1)
    sen_4 <- sample(x[,4], 1)
    sen_long <- paste(sen_1, sen_2, sen_3, sen_4, sep = ' ')
    senten[i] <- sen_long
  }
  paste(unlist(senten), collapse = ' ')
}

# ui.R --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Cоставление речей для публичных выступлений"),
  fluidRow(
    br(),
    column(3, wellPanel(
      sliderInput('number', 'Количество минут выступления:', 
                  min = 1, max = 10, value = 2),
      br(),
      helpText('Данный текст выступления сгенерирован с использованием
               универсального кода речей, разработанного в 60х годах прошлого века.')
    )),
    column(7,
           h4("Текст выступления:"),
           textOutput("rech")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rech <- renderText({
    len_speech <- input$number * 9
    unicode(len = len_speech)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)