library(shiny)
library(here)
library(tidyverse)

load(here::here("data", "unemp.Rda"))

viz_ur <- function(df,
                   min_date = min(df$date),
                   sex = c("Persons", "Males", "Females")) {

  df <- df %>%
    filter(date >= min_date,
           .data$sex %in% .env$sex)

  df %>%
    ggplot(aes(x = date, y = value, colour = sex)) +
    geom_line() +
    theme_minimal() +
    scale_x_date(date_labels = "%b\n%Y") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          legend.position = "top")

}


ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Our first Shiny app"),
    sidebarPanel(sliderInput('min_date',
                             label = "Minimum date",
                             min = min(unemp$date),
                             max = max(unemp$date),
                             value = min(unemp$date)),
                 checkboxGroupInput('sex', "Sex",
                                    choices = c("Persons", "Males", "Females"),
                                    selected = c("Persons", "Males", "Females"))),
    mainPanel(plotOutput('foo'))
  )

)

server <- function(input, output, session) {
  output$foo <- renderPlot({
    viz_ur(df = unemp,
           min_date = input$min_date,
           sex = input$sex)
  })

}

shinyApp(ui, server)
