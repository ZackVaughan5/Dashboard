
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(janitor)

Bio <- read_xlsx("~/onedrive/Athlete Info.xlsx")
Sprint <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx")
bio_sprint <- left_join(Bio,Sprint, by = c("Name" = "Athlete"))
Jump <- read_xlsx("~/onedrive/BPC Clients/Athlete Sprinting.xlsx",
                  sheet = "Jump")
bio_slj <- left_join(Bio,Jump, by = c("Name" = "Athlete"))
CMJ <- read_csv("~/onedrive/Athlete Data/cmj.csv") %>% clean_names()
bio_cmj<- left_join(Bio,CMJ, by = c("Name" = "name"))
Drop_Jump <- read_csv("~/onedrive/Athlete Data/drop_jump.csv") %>% clean_names()
bio_drop_jump <- left_join(Bio,Drop_Jump, by = c("Name" = "name"))
ui <- fluidPage(
    theme = shinytheme("slate"),
    useShinydashboard(),
    # Application title
    titlePanel("Athlete Profile"),


   fluidRow(
        column(4,
               numericInput("athlete_id", "Athlete ID", value = "", 
                            min = 1000, max = 9999, step = 1), 
               valueBoxOutput("maxv_box")
               
               ),
        column(4,         
                  plotOutput("maxv_plot"),
                  plotOutput("slj_plot")
        ),
        column(4,       
                  plotOutput("cmj_plot"),
                  plotOutput("drop_jump_plot"))
          )
        )
server <- function(input, output) {
  ###Max Velocity Plot
  output$maxv_plot <- renderPlot({
   maxv_data <-  bio_sprint %>%
      filter(Run_In == 25) %>%
      select(Athlete_ID, Name, Date, Gender,Top_mph, Avg_mph)
    
    ggplot(maxv_data, aes(Date, Top_mph)) +
      geom_point() +
      geom_point(data = maxv_data %>% filter(Athlete_ID == input$athlete_id),
                 aes(color = Name, size = 5)) +
      scale_color_manual(values = "red") +
      theme_bw() +
      theme(legend.position = "none")
  })
  ###valuebox with max MPH
  output$maxv_box <- renderValueBox({
    bio_sprint %>% filter(Run_In == 25, Athlete_ID == input$athlete_id) %>% max(Top_mph)
  })
  
  ### Standing Long Jump Plot
  output$slj_plot <- renderPlot({
    slj_data <-  bio_slj %>%
      filter(Jump_Type == "BJ") %>%
      select(Athlete_ID, Name, Date, Gender,Daily_Max, Daily_Avg)
    
    ggplot(slj_data, aes(Date, Daily_Max)) +
      geom_point() +
      geom_point(data = slj_data %>% filter(Athlete_ID == input$athlete_id),
                 aes(color = Name, size = 5)) +
      scale_color_manual(values = "red") +
      theme(legend.position = "none")
  })
  ### CMJ Plot
  output$cmj_plot <- renderPlot({
    cmj_data <-  bio_cmj %>%
      filter(is.na(tags), !is.na(jump_height)) %>%
      select(Athlete_ID, Name, date, Gender,jump_height )
    
   ggplot(cmj_data, aes(date, jump_height)) +
      geom_point() +
      geom_point(data = cmj_data %>% filter(Athlete_ID == input$athlete_id),
                 aes(color = Name, size = 5)) +
     scale_color_manual(values = "red") +
     theme(legend.position = "none")
  })
  ### Drop Jump Plot
  output$drop_jump_plot <- renderPlot({
    drop_jump_data <-  bio_drop_jump %>%
      filter(!is.na(jump_height)) %>%
      select(Athlete_ID, Name, date, Gender,jump_height)
    
    ggplot(drop_jump_data, aes(date, jump_height)) +
      geom_point() +
      geom_point(data = drop_jump_data %>% filter(Athlete_ID == input$athlete_id),
                 aes(color = Name, size = 5)) +
      scale_color_manual(values = "red")  +
      theme(legend.position = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
