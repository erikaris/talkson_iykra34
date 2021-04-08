library(shiny)
# library(shinydashboard)
library(tidyverse)
library(plotly)
library(wordcloud)

s_vaccinations <- read_csv('country_vaccinations.csv')

# str_vaccines <- s_vaccinations %>%
#                   drop_na(daily_vaccinations) %>%
#                   group_by() %>%
#                   summarize(text = str_c(vaccines, collapse = ", ")) 

# vaccines <- strsplit(str_vaccines$text, ",") %>%
#     map(trimws) %>%
#     unlist() %>%
#     unique()

##================================================================================
# reactive is just like a callback to reduce code duplication and computation
# usually is called from inside render in the server. 

vaccinations <- reactive({
  s_vaccinations %>%
    drop_na(daily_vaccinations)
})

vaccines <- reactive({
  str_vaccines <- vaccinations() %>%
    group_by() %>%
    summarize(text = str_c(vaccines, collapse = ", "))
  
  vaccines <- strsplit(str_vaccines$text, ",") %>% 
    map(trimws) %>% 
    unlist() %>% 
    unique()
})

# vaccine_countries <- reactive({
#   vaccines() %>% map_df(function(x) {
#     s <- country_vaccinations %>% filter(grepl(x, vaccines)) %>% group_by() %>% summarise(n=n())
#     data_frame(x = x, n = s$n)
#   },
#   .id = "idx"
#   )
# })

#===================
ui <- div(
  class="outer",
  tags$head(
    # Include our custom CSS
    includeCSS("bootstrap.min.css"),
    includeCSS("styles.css")
  ), 
  
  fluidPage(
    fluidRow(
      # Title, 12 grid (full width)
      column(width=12, class="box text-center title", "COVID-19 Vaccination Dashboard"),
      
      # 1 Column, 2 grid (1/6 width)
      column(
        width=3,
        fluidRow(
          column(
            width=12,
            class="box card text-center",
            div("People Vaccinated", class="key"),
            div(textOutput("sum_people_vaccinated"), class="value")
          ), 
          column(
            width=12,
            class="box card text-center",
            div("People Fully Vaccinated", class="key"),
            div(textOutput("sum_people_fully_vaccinated"), class="value")
          ),
          column(
            width=12,
            class="box card text-center",
            div("Total Vaccinations", class="key"),
            div(textOutput("sum_total_vaccinations"), class="value")
          ),
          column(
            width=12,
            class="box card",
            uiOutput("count_countries_by_vaccine")
          ),
          column(
            width=12,
            class="box card text-center",
            div("Last Updated at", class="key"),
            div(textOutput("csv_last_updated"), class="value")
          )
        )
      ),
      
      column(
        width=9,
        fluidRow(
          column(
            width=12,
            div(
              class="box card relative",
              plotlyOutput("sum_total_vaccinations_by_country"),
              div(
                class="box absolute right bottom",
                uiOutput("slider_year")
              )
            )
          ),
          
          column(
            width=4,
            div(
              class="box card",
              plotlyOutput("sum_progress_vaccinations_daily")
            ),
          ),
          
          column(
            width=4,
            div(
              class="box card",
              plotlyOutput("sum_progress_people_vaccinated_daily")
            ),
          ),
          
          column(
            width=4,
            div(
              class="box card",
              plotlyOutput("sum_progress_vaccinations_daily_by_vaccines")
            ),
          )
        )
      )
      
    )
  )
)


server <- function(input, output) { 
  
  output$sum_people_vaccinated <- renderText({
    x <- vaccinations() %>% 
      drop_na(people_vaccinated) %>% 
      group_by(country) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_people_vaccinated = sum(people_vaccinated)) %>% 
      group_by() %>% 
      summarize(sum_people_vaccinated = sum(sum_people_vaccinated))
    x$sum_people_vaccinated
  })
  
  output$sum_people_fully_vaccinated <- renderText({
    x <- vaccinations() %>% 
      drop_na(people_fully_vaccinated) %>% 
      group_by(country) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_people_fully_vaccinated = sum(people_fully_vaccinated)) %>% 
      group_by() %>% 
      summarize(sum_people_fully_vaccinated = sum(sum_people_fully_vaccinated))
    x$sum_people_fully_vaccinated
  })
  
  output$sum_total_vaccinations <- renderText({
    x <- vaccinations() %>% 
      drop_na(total_vaccinations) %>% 
      group_by(country) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_total_vaccinations = sum(total_vaccinations)) %>% 
      group_by() %>% 
      summarize(sum_total_vaccinations = sum(sum_total_vaccinations))
    x$sum_total_vaccinations
  })
  
  output$count_countries_by_vaccine <- renderUI({
    vaccines <- vaccines()
    
    count_vaccines <- vaccines %>%
      map(function(vac) {
        x <- s_vaccinations %>% filter(grepl(vac, vaccines)) %>% group_by() %>% summarise(n=n())
        x$n[[1]]
      })
    
    html_vaccines <- list()
    for (i in 1:length(vaccines)) {
      html_vaccines <- append(html_vaccines, paste0(
        "<div class=\"kvrow\">",
        "<div class=\"key\">", vaccines[[i]], "</div>", 
        "<div class=\"value\">", count_vaccines[[i]], "</div>",
        "</div>"
      ))
    }
    
    HTML(html_vaccines %>% paste(collapse = ''))
  })
  
  output$csv_last_updated <- renderText({
    as.character(file.info('country_vaccinations.csv')$ctime)
  })
  
  output$sum_total_vaccinations_by_country <- renderPlotly({
    data <- vaccinations() %>% 
      drop_na(total_vaccinations) %>%
      group_by(country) %>% 
      filter(date == max(date)) %>% 
      ungroup() %>%
      filter(date <= input$slider_year)
      # filter(vaccines == event_data("plotly_click", source="vaccines")$x) %>%
    
    data$hover <- with(
      data, paste(
        country, '<br>', 
        "Date", date, "<br>",
        "Total vaccinations", total_vaccinations, "<br>",
        "People Vaccinated", people_vaccinated, "<br>",
        "People Fully Vaccinated", people_fully_vaccinated, "<br>",
        "Vaccines", vaccines
      ))
      
    data %>% highlight_key(~iso_code) %>%
      plot_ly(
        source="sum_total_vaccinations_by_country", 
        type='choropleth', 
        locations=~iso_code, 
        z=~total_vaccinations, 
        # text=~country, 
        text=~hover,
        colorscale="Viridis"
      ) %>% 
      # https://plotly.com/python/templates/
      layout(
        paper_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        ),
        geo = list(
          showframe = FALSE,
          showcoastlines = FALSE,
          bgcolor = '#222327'
        )
      ) %>%
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  output$slider_year <- renderUI({
    data <- vaccinations() %>%
      drop_na(total_vaccinations)
    
    sliderInput(
      "slider_year", 
      "", 
      min=min(data$date), 
      max=max(data$date), 
      value=max(data$date), 
      animate=animationOptions(loop = TRUE, interval = 1000)
    )
  })
  
  output$sum_progress_vaccinations_daily <- renderPlotly({
    vaccinations() %>%
      drop_na(total_vaccinations) %>%
      group_by(date) %>%
      summarize(sum_total_vaccinations = sum(total_vaccinations)) %>%
      highlight_key(~date) %>%
      plot_ly(
        x = ~date, 
        y = ~sum_total_vaccinations,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      # add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')  %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  output$sum_progress_people_vaccinated_daily <- renderPlotly({
    vaccinations() %>%
      drop_na(people_vaccinated) %>%
      group_by(date) %>%
      summarize(sum_people_vaccinated = sum(people_vaccinated)) %>%
      highlight_key(~date) %>%
      plot_ly(
        x = ~date, 
        y = ~sum_people_vaccinated,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      # add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')  %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  output$sum_progress_vaccinations_daily_by_vaccines <- renderPlotly({
    vaccinations() %>%
      drop_na(total_vaccinations) %>%
      group_by(date, vaccines) %>%
      summarize(sum_total_vaccinations = sum(total_vaccinations)) %>%
      highlight_key(~date) %>%
      plot_ly(
        x = ~date, 
        y = ~sum_total_vaccinations,
        color = ~vaccines,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      hide_legend() %>%
      # add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')  %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
}

shinyApp(ui, server)
