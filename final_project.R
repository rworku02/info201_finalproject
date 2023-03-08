library(shiny)
library(tidyverse)
library(rsconnect)

demographic <- read_delim("Count Us In/Demographics-Table 1.csv")
causes <- read_delim("Count Us In/Cause-Table 1.csv")
total <- read_delim("Count Us In/Total-Table 1.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Overview", titlePanel("Homelessness in Seattle"),
             imageOutput("main_image"),
      p("Homelessness is a complex and pressing issue that affects many cities 
        around the world, including Seattle. Seattle has been grappling with a 
        rising homelessness crisis for years, with an estimated 11,000 people 
        experiencing homelessness in the city in 2020, according to the Seattle 
        Times. Here are some reasons why the issue of homelessness in Seattle 
        needs to be solved:"),
      br(),
      p("Basic Human Needs: Homelessness denies individuals the basic human 
        needs of shelter, safety, and security. Without a place to call home, 
        people are exposed to the elements, unsafe living conditions, and are 
        more vulnerable to crime, illness, and injury. Public Health: 
        Homelessness can exacerbate public health issues, including the spread 
        of diseases such as COVID-19, due to cramped living conditions, lack of 
        access to sanitation facilities, and limited healthcare resources. The 
        city’s homeless population is also disproportionately impacted by mental 
        illness, substance use disorders, and other chronic health conditions. 
        Economic Impacts: Homelessness can have economic impacts on the city, 
        including the cost of emergency services, increased healthcare costs, 
        and reduced productivity due to the effects of homelessness on 
        individuals’ physical and mental health. Social Equity: Homelessness is 
        a social equity issue as it disproportionately affects marginalized 
        communities such as people of color, LGBTQ+ individuals, and those with 
        disabilities. Addressing homelessness in Seattle is essential for 
        promoting social equity and ensuring that everyone has an opportunity to 
        thrive. Homelessness is solvable: While the issue of homelessness is 
        complex, it is not insurmountable. Seattle has the resources and the 
        capacity to address homelessness through innovative and comprehensive 
        solutions such as providing affordable housing, increasing access to 
        mental health and substance abuse treatment, and providing employment 
        opportunities."),
      p("The source of our dataset is the website data.world. The data was
        collected by All Home which the lead agency for the Seattle/King County
        Continuum of Care. This data was collected from the years 1989-2020."),
      imageOutput("map_image")
    ),
    
    tabPanel("Sample", h3("Population sample"),
             p("This app uses data collected on homelessness in ", 
               em("Seattle, WA"), "from", strong("1998-2020.")),
             br(),
             p("Below is a random sample of the homeless population in King 
               County along with its overall population. This is to show that
               the issue of homelessness affects the audience of this project.
               Homeless people aren't only in the city, but have been moving
               in to the King County areas too."),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("number", "Choose how many samples you want",
                             min = 1,
                             max = 23,
                             value = 4)
               ),
               mainPanel(tableOutput("random"))
             )
    ),
    
    tabPanel("Causes", h3("Causes"),
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
    )),
    
    tabPanel("Demographics", h3("Demographics"), 
      p("The demographics of homeless people in Seattle are diverse, but there are 
        some trends that have been observed in recent years. Here are some key 
        demographic characteristics of Seattle's homeless population. Here, you 
        may see the homeless data based on the demographic you choose."),
      sidebarLayout(
        sidebarPanel(
          p("Choose a demographic to see the type of people that may be affect."),
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
      ),
      p(strong("Age:"), "Seattle's homeless population skews towards older adults, with 
        about 72-75% of individuals experiencing homelessness being over the 
        age of 50. Homelessness also affects younger individuals, with about 
        9-19% of the homeless population being under the age of 24."),
      br(),
      p(strong("Gender"), "Men make up a larger proportion of Seattle's homeless 
        population, with around 56% of the homeless population being male. 
        However, homelessness also affects women, with around 41% of the 
        homeless population being female."),
      br(),
      p(strong("Race and ethnicity"), "Homelessness disproportionately affects 
        people of color in Seattle, with Black, Indigenous, and People of Color 
        (BIPOC) being overrepresented among the homeless population. According 
        to a 2020 count, 25% of Seattle's homeless population identified as 
        Black, 15% as Native American or Alaska Native, and 15% as 
        Hispanic/Latinx."),
      br(),
      p(strong("LGBTQ+ individuals"), "Homelessness disproportionately affects 
        LGBTQ+ individuals in Seattle, who may face discrimination, family 
        rejection, and other challenges that can lead to homelessness. According 
        to a 2019 survey, around 15% of Seattle's homeless population identified 
        as LGBTQ+."),
      br(),
      p(strong("Disability:"), "Many individuals experiencing homelessness in 
        Seattle have disabilities, including physical, mental, and developmental 
        disabilities. According to a 2020 count, around 45% of Seattle's 
        homeless population reported having a physical disability, while 36% 
        reported having a mental health condition.")
    ),
    
    tabPanel("Summary", h3("Key takeaways"),
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
             h3("Data quality"),
             p("We thought the dataset was of high quality and was clear to read. The dataset
                   was well organized and easy to navigate. We believe it has unbiased results since
                   the data is diverse and takes many factors into account. Future ideas to
                   advance this project could be to take some personal stories of homeless people
                   in Seattle. We could get the first-hand experience to better understand them."),
             imageOutput("bar_image")
    )
  )
)

server <- function(input, output) {
  ## Displays the main image
  output$main_image <- renderImage({
    list(alt= "Photo of homless man and a big dog", src = "mainphoto.jpg", width = "50%")
  }) 
  
  output$map_image <- renderImage({
    list(alt= "Map of homeless population by state", src= "homelessmap.jpg")
  })
  
  first_sample <- reactive({
    sample <- total %>% 
      sample_n(input$number)
    sample
  })
  
  output$random <- renderTable({
    first_sample()
  })
  
  cleaned_causes <- causes %>% 
    select_if(~!any(is.na(.)))
  
  ## Let's users pick a cause to see its data
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
  
  ## Makes the plot of the cause of homelessness
  output$plot <- renderPlot({
    causes_plot <- plot_sample() %>%
      ggplot(aes(x = factor(Year), y = Count, fill = factor(Cause))) +
      geom_col() +
      labs(title = "People Homeless Due to Cause Each Year" ,
           x = "Year", y = "Number of people homeless", fill = "Cause") +
      scale_fill_manual(values = input$color)
    
    if(nrow(plot_sample()) == 0) {
      causes_plot <- causes_plot + labs(title = "Please select a cause")
    } 
    causes_plot
  })
  
  ## Textual output that reacts to user's choices
  output$years <- renderText({
    years <- plot_sample() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from", min(years), "to", max(years))
    }
  })
  
  demographics <- demographic %>% 
    select_if(~ !any(is.na(.)))
  
  ## Lets users pick a demographic
  output$checkboxDemographics <- renderUI({
    radioButtons("Demographic", "Choose a demographic",
                 choices = unique(demographics$Demographic),
                 selected = NULL)
  })
  
  ## The demographic that will be displayed will be based on the user's choice
  selected_demographics <- reactive({
    demographics %>% 
      filter(Demographic == input$Demographic) %>% 
      filter(!is.na(Count))
  })
  
  ## Makes a table based on the user's selected demographic 
  output$data_table <- renderTable({
    selected_demographics()
  })
  
  ## Textual output that reacts to user's choices
  output$message <- renderText({
    num_observations <- nrow(selected_demographics())
    paste("Selected demographic contains", num_observations, "observations.")
  })
  
  ## Displays an image of a bar plot
  output$bar_image <- renderImage({
    list(alt= "Bar plot of the homeless count in 2006-2020 ", src= "homeless_bar.jpg",
         width = "50%")
  })
}

shinyApp(ui = ui, server = server)