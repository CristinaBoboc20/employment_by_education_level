

library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(shinythemes)
library(markdown)
library(data.table)
library(readr)
library(reshape2)
library(ggpmisc)



grid::current.viewport()
data_project <- read.csv("~/ProiectR/ProjectR/employment_data.csv")

data_hours<- read.csv("~/ProiectR/ProjectR/hours_data.csv")

data_self<-read.csv("~/ProiectR/ProjectR/self_employment.csv")

data_project$Location <- as.factor(data_project$Location)

data_project$Date<-as.integer(data_project$Date)

data_hours$Time<-as.integer(data_hours$Time)

data_self$Year<-as.integer(data_self$Year)


# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(
                  "Employment by education level",
                  tabPanel("Education",
                           sidebarPanel(
                             ("Country Status:"),
                             h2("Employment by education level"),
                            
                             selectInput(inputId = "locations", "Location(s)",
                                         choices = c ("NZL","GBR","CHE","TUR","NLD","GRC","KOR","EST","DEU","DNK","FIN","NOR","SVK","USA","RUS","EST","NLD","AUT","LUX","SVN","DNK","JPN","CZE","PRT","OAVG","BEL","ISL","LVA"),
                                         multiple = TRUE,
                                         selected = "NZL"),
                             radioButtons(inputId = "nivel",
                                          label = "Nivel de educatie:",
                                          choices = c(
                                            "Liceu" = "Liceu",
                                            "Gimnaziu" = "Gimnaziu",
                                            "Facultate" = "Facultate"
                                          )),
                             sliderInput(inputId="value1", "Procent:",min =0, max = 100, value = c(0,100)),
                             sliderInput(inputId="date", "An:",min =1997, max = 2021, value = c(1997,2021)),
                             
          
                             downloadButton(outputId = "download_data1", label = "Download"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             plotlyOutput(outputId = "plot"), br(),
                             br(), br(), br(),
                             DT::dataTableOutput(outputId = "table")
                             
                           ), # mainPanel
                        ),
                  
                  
                  
                  
                  tabPanel("Hours",
                           sidebarPanel(
                             h3("Years range"),
                             sliderInput(inputId="time", "An:",min =2018, max = 2021, value = c(2018,2021)),
                             
                             downloadButton(outputId = "download_data2", label = "Download"),
                             
                             
                           ),
                           
                           mainPanel (
                             plotlyOutput(outputId = "hours"), br(),
                             br(), br(), br(),
                             DT::dataTableOutput(outputId = "table2")
                           ),
                           
                           
                  ), # Navbar Romania, tabPanel
                  
                  
                  
                  
                  tabPanel("Self employment",
                           sidebarPanel(
                            h2("Selectati categoria"),                     
                            radioButtons(inputId = "gen",
                                         label = "Gen",
                                         choices = c(
                                           "Femei" = "WOMEN",
                                           "Barbati" = "MEN"
                                          
                                         )),
                             
                             
        
                             
                              downloadButton(outputId = "download_data3", label = "Download"),
                             
                             
                           ),
                           
                           mainPanel (
                             plotlyOutput(outputId = "self"), br(),
                             br(), br(), br(),
                             DT::dataTableOutput(outputId = "table3")
                           ),
                           
                           
                  ), # Navbar Romania, tabPanel
                  
                  
                  
                  
                  
                  
                 
                  
                ) # Contact tab
) # ui


server <- function(input, output) {
  
  filtered_data <- reactive({
    subset(data_project, Location == input$locations & Date>=input$date[1] & Date<=input$date[2] & SUBJECT==input$nivel & Value>=input$value1[1] & Value<=input$value1[2]
           
           #Location %in% input$location & Date %in% input$date
           #  (data_project[,c(5)] >= input$years[1] & data_project[,c(5)] <= input$years[2])
           #data_project [data_project$Date > input$years[1],] & data_project[data_project$Date < input$years[2],]
    )})
  
  filter_hours<-reactive({
    subset(data_hours, Time>=input$time[1] & Time<=input$time[2])
  })

  
  
  filter_gen<-reactive({
    subset(data_self, GEN==input$gen)
  })
  

  
  output$plot <- renderPlotly({
    ggplotly({
      
      p <- ggplot(filtered_data(), aes_string(x="Date", y="Value", color="Location")) +
        geom_point(alpha=1) + theme(legend.position = "none") +
        ylab("% change from baseline")
      p
    
    })
  })
  

  
  
  
  output$hours <-renderPlotly({
    ggplotly({
      
      h<-ggplot(filter_hours(), aes_string(x="Time", y="NoHours",color="COUNTRY")) +
        geom_bar(stat="identity", fill="steelblue")+
        geom_text(aes(label="Time"), vjust=-0.3, size=3.5)+
        theme_minimal()  
      h    
    })
  })
  

  
  
  output$self <-renderPlotly({
    ggplotly({
      
      
      u<-ggplot(filter_gen(), aes_string(x="GEN", y="Value", group="LOCATION")) + 
        geom_bar(aes(fill = "LOCATION"), stat = "identity", position = "dodge") 
        #annotate(geom = 'table',
         #        x=4,
          #       y=0,
           #      label=list(data_self))
      
      u 
    
    })
  })
  
  
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$table2 <-DT::renderDataTable({
    filter_hours()
    
  })
  
  output$table3 <-DT::renderDataTable({
    filter_gen()
    
  })
  
  
  output$download_data1 <- downloadHandler(
    filename = "download_data1.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  output$download_data2 <- downloadHandler(
    filename = "download_data2.csv",
    content = function(file) {
      data <- filter_hours()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  
  output$download_data3 <- downloadHandler(
    filename = "download_data3.csv",
    content = function(file) {
      data <- filter_gen()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)