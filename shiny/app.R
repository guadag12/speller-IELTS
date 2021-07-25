## APP 2
library(tidyverse)
library(shiny)
data <- read.csv("https://raw.githubusercontent.com/guadag12/speller-IELTS/master/data.csv")
names(data)[2] <- "names_s"
names(data)[5] <- "date"
names(data)[7] <- "credit_card"

ui <- fluidPage(
    titlePanel("Practicing for IELTS listening!"),
    sidebarLayout(
        sidebarPanel(
            actionButton("generateBt", "Click here first!"),
            selectInput(inputId = "selector",
                        label = "Select the category which wants to spell:", 
                        choices = c("celphone", "credit_card", "date", "email", "money", "names_s", "numbers", "hour"), 
                        selected = 1 )
        ),
        mainPanel(
            column(6, 
                   wellPanel(
                       actionButton("play", "Play the Audio"),
                       checkboxInput('checkbox',
                                     'Show results!', 
                                     value = FALSE),
                       conditionalPanel("input.checkbox", verbatimTextOutput("value_ui"))
                   )
            )
        )
    )
)

server <- function(input, output, session) {
    
    random_number <- eventReactive(input$generateBt, {
        random_number <- sample(seq(1:50), 1)
        random_number
    })
    
    
    observeEvent(input$play, {
        insertUI(selector = "#play",
                 where = "afterEnd",
                 ui = tags$audio(src = paste0("https://github.com/guadag12/speller-IELTS/raw/master/audios/", input$selector,"/", input$selector, "_", random_number(), ".wav"), type = "audio/wav", autoplay = NA, controls = NA, style="display:none;")  
        )
    })
    output$value_ui <- renderPrint({
        data[ random_number(), input$selector]
    })
}

shinyApp(ui, server)

