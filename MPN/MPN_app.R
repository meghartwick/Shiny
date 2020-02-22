
library(shiny)
library(MPN)
library(dplyr)
library(DT)

M<-read.csv('MPN.csv')
fieldsAll = c("Site","Matrix","MPN")

ui <- fluidPage(theme = "bootstrap.min.css",
   # Application title
   titlePanel(h3("MPN Calculator")),
   sidebarLayout(
      sidebarPanel(width = 4,
        selectInput("Row", label = h4("Positive Row(s)"), 
                               choices = list("A-B-C" = 0.1, "B-C-D" = 1, "C-D-E" = 10, "D-E-F" = 100,
                               "E-F-G" = 1000, "F-G-H" = 10000, "G-H-I" = 100000, selected = 'A-B-C')),
        selectInput("row1", label = h5("Postive Tubes-Most Concentrate"), choices = list("0"= 0,"1" = 1, "2" = 2, "3" = 3), selected = 3),
        selectInput("row2", label = h5("Postive Tubes-Middle"),choices = list("0"= 0,"1" = 1, "2" = 2, "3" = 3), selected = 3),
        selectInput("row3", label = h5("Postive Tubes-Least Concentrate"), choices = list("0"= 0,"1" = 1, "2" = 2), selected = 2),
        selectInput("Site", h4("Site"), choice= c('NI', "OR", "Other"), selected = 'NI'),
        selectInput('Matrix', h4("Select Matrix"), choices = c('Oyster', 'Water', 'Sediment', 'Phytoplankton', 'Zooplankton', 'Other'),
                  selected = 'Oyster'),
        textInput("Plankton", h5('If plankton, enter amt added but omit decimals, otherwise leave "1"'), '1'),
        actionButton("submit", h3("Submit")),
        downloadButton("MPN", h4("Download in CSV"), class="butt")),
      
    
      mainPanel(mainPanel(
                          tabsetPanel(
                            tabPanel("Instructions", tags$img(src="MPN FIGURE 2.jpg", height = '500px', width  = '600pix') ),
                            tabPanel("Table",
                                     DT::dataTableOutput("MPN_Table"))
                                     
                            )
          )
)
)
)



server <- function(input, output) {
  
  M<-read.csv('MPN.csv', stringsAsFactors = F)
  
  Test <- reactiveValues()
  Test$df <- data.frame("Site"="Site", "Matrix" = "Matrix", "MPN" = "3-3-3 = < 1100")
  
    observeEvent(input$submit, {
      Test$df <- rbind(Test$df,cbind("Site" = input$Site, "Matrix" = input$Matrix, "MPN" = M %>%
                                 filter(ROW1 == input$row1 & ROW2 == input$row2 & ROW3 == input$row3)%>%
                                 select(MPN) %>%
                                 as.numeric(as.character())*as.numeric(input$Row)/as.numeric(input$Plankton)))
      output$MPN_Table <- DT::renderDataTable(Test$df)}
      
    )
    
      output$MPN<- downloadHandler(
      filename = function() {
        paste("MPN Calculations ", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data.frame(Test$df), file, row.names = F)
      }
    )
}
    

# Run the application 
shinyApp(ui = ui, server = server)

