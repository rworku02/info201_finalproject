library(shiny)
library(tidyverse)
library(rsconnect)

demographic <- read_delim("Count Us In/Demographics-Table 1.csv")
causes <- read_delim("Count Us In/Cause-Table 1.csv")
total <- read_delim("Count Us In/Total-Table 1.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Overview", titlePanel("Homelessness in Seattle"), sidebarLayout(
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
    
    tabPanel("Sample", h3("Statistics sample"),
             p("This app uses data collected on homelessness in ", 
               em("Seattle, WA"), "from", strong("1998-2020.")),
             br(),
             p("Below is a random sample of the homeless population in King 
               County along with its overall population."),
             tableOutput("random")
    ),
    
    tabPanel("Sample", h3("Statistics sample"),
             p("This app uses data collected on homelessness in ", 
               em("Seattle, WA"), "from", strong("1998-2020.")),
             br(),
             p("Below is a random sample of the homeless population in King 
               County along with its overall population."),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("n", "How many samples:",
                             min = 1,
                             max = 23,
                             value = 4),
               ),
               tableOutput("random")
             )
    ),
    
    tabPanel("Plot", h3("Possible causes"),
      p("Many are homeless for various reasons. We don't 
             always know the reason we see a person in the street and how they 
             got there. In this section, select a cause and see how many 
             homeless people were affected"), 
      sidebarLayout(
        sidebarPanel(
          p("Select a cause of homelessness and see how many are affected each 
            year."),
          fluidRow(
            column(6, 
                   radioButtons("color", "Choose color", 
                                choices = c("skyblue", "darkseagreen", "khaki", 
                                                   "orange", "tan", "sienna", 
                                                   "salmon", "pink", "thistle"))
            ),
            column(6,
                 uiOutput("checkboxShelter")
            )
          )
        ),
        mainPanel(
          plotOutput("plot"),
          textOutput("years")
        )
      )  
    ),
    
    tabPanel("Table", h3("Demographics"), 
      p("Homelessnes does not discrimminate. It does not 
             care about your gender, health, race or age. It can struck you at 
             any moment. Choose a demographic to see the type of people that may 
                            be affect."),
      sidebarLayout(
        sidebarPanel(
          p("Here, you may see the homeless data based on the demographic you 
            choose."),
          fluidRow(
            column(6,
                   uiOutput("checkboxDemographics")
            ),
          )
        ),
        mainPanel(
          textOutput("message"),
          tableOutput("data_table")
        )         
      )
    ),
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
                   in Seattle. We could get the first-hand experience to better understand them.")
      ),
      mainPanel(imageOutput("barImage"))
    )
    )
  )
)

server <- function(input, output) {
  ## For the first page
  output$mapImage <- renderImage({
    list(alt= "Map of homeless population by state", src= "homelessmap.jpg")
  })
  
  ## For the sample page
  output$random <- renderTable({
    total %>%  
      sample_n(6)
  })
  
  ## For the plot page
  cleaned_causes <- causes %>% 
    select_if(~!any(is.na(.)))
  
  output$checkboxShelter <- renderUI({
    radioButtons("Cause", "Causes of homelessness",
                 choices = unique(cleaned_causes$Cause),
                 selected = NULL
    )
  })
  
  plot_sample <- reactive({
    cleaned_causes %>%
      filter(Cause == input$Cause)
  })
  
  output$plot <- renderPlot({
    p <- plot_sample() %>%
      ggplot(aes(x = factor(Year), y = Count, fill = factor(Cause))) +
      geom_col() +
      labs(x = "Year", y = "Number of people homeless", fill = "Cause") +
      scale_fill_manual(values = input$color)
    
    if(nrow(plot_sample()) == 0) {
      p <- p + labs(title = "Please select a cause")
    } 
    p
  })
  
  output$years <- renderText({
    years <- plot_sample() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from", min(years), "to", max(years))
    }
  })
  
  ## For the table page
  demographics <- demographic %>% 
    select_if(~ !any(is.na(.)))
  
  output$checkboxDemographics <- renderUI({
    radioButtons("Demographic", "Choose a demographic",
                 choices = unique(demographics$Demographic),
                 selected = NULL)
    
  })
  
  selected_demographics <- reactive({
    demographics %>% 
      filter(Demographic == input$Demographic) %>% 
      filter(!is.na(Count))
  })
  
  output$data_table <- renderTable({
    selected_demographics()
  })
  
  output$message <- renderText({
    n <- nrow(selected_demographics())
    paste("Selected demographic contains", n, "observations.")
  })
  
  ## For the last page
  output$barImage <- renderImage({
    list(alt= "Bar plot of the homeless count in 2006-2020 ", src= "homeless_bar.jpg",
         width = "100%")
  })
}

shinyApp(ui = ui, server = server)
