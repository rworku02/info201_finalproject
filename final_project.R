library(shiny)
library(tidyverse)
library(rsconnect)
library(tidyr)
##hi
#hola
## konichiwa
totalStats <- read_delim("Count Us In/Total-Table 1.csv")
totalStats %>% 
  group_by(Year)

demographics <- read_delim("Count Us In/Demographics-Table 1.csv")
newDemographics <- demographics %>% 
  select_if(~ !any(is.na(.)))

yearRange <- newDemographics %>% 
  select(Year) %>% 
  summarise(high = max(Year))


cause <- read_delim("Count Us In/Cause-Table 1.csv")
newCause <- cause %>% 
  select_if(~ !any(is.na(.)))


ui <- fluidPage(tabsetPanel(
  tabPanel("Overview",titlePanel("Homelessness in Seattle"), sidebarLayout(
    sidebarPanel(
      p("Homlessness is a major problem in America. In December of just last year,
             there were about 582,000 people who were experiencing homelessness. In Seattle,
             there was a 13.8% increase in our homeless population from 2020 to 2022. Homeless people
             are struggling with the lack of food or shelter, and we must take action."),
      br(),
      p("With our project, we hope to study the data collected on Seattle's homeless population to
             get a better understanding of the problem. Understanding the cause and factors contributing
             to homelessness can help up elimiate this issue and to aid those are being affected.")),
    mainPanel(imageOutput("mapImage"))),
  ), 
  tabPanel("About", h3("Statistics sample"),
           p("This app uses data collected on homelessness in ", 
             em("Seattle "), "from the years ", strong("1998 - 2020")),
           br(),
           p("Below is a random sample of the total homeless population data in King County:"),
           sidebarLayout(
             sidebarPanel(selectInput("userSelect")),
             mainPanel(tableOutput("stats_table")),
           ),
           ),
  tabPanel("Plots", h3("Causes"),
           p("Many are homeless for various reasons. We don't 
             always know the reason we see a person in the street and how they 
             got there. In this section, select a cause and see how many 
             homeless people were affected"),
           sidebarLayout(
             sidebarPanel(
                          fluidRow(
                            column(6,
                                   radioButtons("color", "Choose color",
                                                choices = c("skyblue", "lawngreen", "orangered",
                                                                     "purple", "gold"))
                            ),
                            column(6,
                                   uiOutput("checkboxShelter")
                            )
                          )
             ),
             mainPanel(
               textOutput("years"),
               plotOutput("plot")
             )
           )),
  tabPanel("Table", h3("Demographics"),
           p("Homelessnes does not discrimminate. It does not 
             care about your gender, health, race or age. It can struck you at 
             any moment. Choose a demographic to see the type of people that may 
                            be affect."),
           sidebarLayout(
             sidebarPanel(
                          fluidRow(
                            column(6, uiOutput("checkboxDemo"))
                          )
             ),
             mainPanel(textOutput("h"), 
                       tableOutput("data_table"))
           )),
  tabPanel("Summary", sidebarLayout(
    sidebarPanel(h3("Key takeaways:"),
                 p("After analyzing our data, we came to the conclusion that most of the
                   recent homelessness was caused by problems with housing. Homelessness caused
                   by eviction and rent were the greatest in 2020. Seattle has had it's housing
                   prices steadly increasing which has caused many people to lose their homes."),
                 p("Another takeaway from the dataset was that 54% the homeless population
                   in 2020 had a mental health condition. There is a large portion of homeless
                   people dealing with a mental health disorder but aren't getting the professional
                   help they need. Homeless people have to pay for their physical needs such as food,
                   and psychiatric needs are lower on their priority list."),
                 br(),
                 h3("Data quality:"),
                 p("We thought the dataset was of high quality and was clear to read. The dataset
                   was well organized and easy to navigate. We believe it has unbiased results since
                   the data is diverse and takes many factors into account. Future ideas to
                   advance this project could be to take some personal stories of homeless people
                   in Seattle. We could get the first-hand experience to better understand them.")),
    mainPanel(imageOutput("barImage"))
  ))
)
)

server <- function(input, output) {
  output$random <- renderTable({
    totalStats %>%  
      sample_n(6)
  })
  ######
  output$selectYear <- renderUI({
    selectInput("userSelect", "Choose a demographic",
                 choices = unique(totalStats$Year))
  })
  select_sample <- reactive({
    s3 <- totalStats %>% 
      filter(!is.na()) %>% 
      filter(Year %in% input$userSelect)
    s3
  })
  
  output$stats_table <- renderTable({
    totalStats %>% 
      filter(Year == input$userSelect)
  })
  
  #######
  output$checkboxShelter <- renderUI({
    radioButtons("userCause", "Choose cause of homelessness",
                 choices = unique(newCause$Cause)
    )
  })
  
  sample <- reactive({
    newCause %>%
      filter(Cause %in% input$userCause)
  })
  
  output$plot <- renderPlot({
    p <- sample() %>%
      ggplot(aes(factor(Year), Count, fill = factor(Cause))) +
      geom_col() +
      labs(x = "Year", y = "Count", title = "Bar Plot for Causes of Homelessness",
           fill = "Cause") +
      scale_fill_manual(values = input$color)
    if(nrow(sample()) == 0) {
      p <- p + labs(title = "Please select a cause")
    }
    p
  })
  
  output$checkboxDemo <- renderUI({
    radioButtons("Demo", "Choose a demographic",
                 choices = unique(newDemographics$Demographic))
  })
  
  output$years <- renderText({
    years <- sample() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from", min(years), "to", max(years))
    }
  })
  
  table_sample <- reactive({
    s2 <- newDemographics %>% 
      filter(!is.na()) %>% 
      filter(Demographic %in% input$Demo)
    s2
  })
  
  output$data_table <- renderTable({
    newDemographics %>% 
      filter(Demographic == input$Demo)
  })
  
  output$h <- renderText({
    selected_data <- newDemographics %>% 
      filter(Demographic == input$Demo)
    
    n <- sum(!is.na(selected_data$Count))
    output_text <- paste("Selected demographic contains", n, "observations.")
  })
  
  output$mapImage <- renderImage({
    list(alt= "Map of homeless population by state", src= "homelessmap.jpg")
  })
  
  output$barImage <- renderImage({
    list(alt= "Bar plot of the homeless count in 2006-2020 ", src= "homeless_bar.jpg",
         width = "100%")
  })
}

shinyApp(ui = ui, server = server)
