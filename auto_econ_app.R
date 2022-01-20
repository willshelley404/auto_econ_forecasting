library(tidyverse)
library(shiny)
library(timetk)
library(plotly)

source("TNOTE_10.R")
rm(list= ls()[!(ls() %in% c('tnote_10'))])

source("BRENT.R")
rm(list= ls()[!(ls() %in% c('brent', 'tnote_10'))])

source("HOUST.R")
rm(list= ls()[!(ls() %in% c('houst', 'brent', 'tnote_10'))])

source("INCOME_PER_CAP.R")
rm(list= ls()[!(ls() %in% c('income_per_cap', 'houst', 'brent', 'tnote_10'))])

source("NONFARM.R")
rm(list= ls()[!(ls() %in% c('nonfarm', 'income_per_cap', 'houst', 'brent', 'tnote_10'))])

source("PCE.R")
rm(list= ls()[!(ls() %in% c('pce', 'nonfarm', 'income_per_cap', 'houst', 'brent', 'tnote_10'))])

source("RES_INVEST.R")
rm(list= ls()[!(ls() %in% c('res_invest', 'pce', 'nonfarm', 'income_per_cap', 'houst', 'brent', 'tnote_10'))])

source("IND_PROD_INDEX.R")
rm(list= ls()[!(ls() %in% c('ind_prod_index', 'res_invest', 'pce', 'nonfarm', 'income_per_cap', 'houst', 'brent', 'tnote_10'))])


external_data <- houst %>%
  inner_join(brent) %>%
  inner_join(income_per_cap) %>%
  inner_join(nonfarm) %>%
  inner_join(pce) %>%
  inner_join(nonfarm) %>%
  inner_join(ind_prod_index) %>%
  inner_join(tnote_10) %>%
  inner_join(res_invest)


df <- external_data %>%
  mutate(
    horizon = if_else(date < today(), "observed", "forecast")
  )

# # 
# # saveRDS(df, "exogenous_forecasts.rds")


 # df <- read_rds("exogenous_forecasts.rds")
 

# df %>%
#   select(date, houst) %>%
#   plot_time_series(date, houst)


table_df <- df %>%
  select(date, horizon, everything()) %>%
  pivot_longer(houst:res_invest) %>% 
 # arrange(desc(date)) %>% 
  pivot_wider(id_cols = c(date, name))

# df %>% 
#   select(houst:res_invest, date, horizon)


summary_table <- table_df %>% 
  pivot_longer(houst:res_invest) %>% 
  group_by(name) %>% 
  summarise_by_time(
    .date_var = date, 
    .by = "year",
    yearly_sum = sum(value)
  ) %>% 
  mutate(yoy_change = (yearly_sum - lag(yearly_sum))/lag(yearly_sum)) %>% 
  ungroup()   
  
  
# summary_table %>% 
#   na.omit() %>% 
#   mutate(Year = year(date),
#          yoy_change = scales::percent(yoy_change, accuracy = 0.2L)
#          ) %>% 
#   select(-date) %>% 
#   filter(name == input$vars) %>% #input$vars 
#   tail(10) 


# UI 

ui <- fluidPage(
  # Application title
  titlePanel("Economic Data Forecasts"),

# Sidebar with a slider input for number of bins 
sidebarLayout(
  sidebarPanel(
    varSelectInput(inputId = "vars",
               label =  "Economic Variable",
               data = df %>% select(-date, -horizon),
               multiple = F), width = 3
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotlyOutput("plot")
  )
),
splitLayout(
fluidRow(
  column(12,
         dataTableOutput('table')
  )) ,

fluidRow(
  column(2,
         tableOutput('table_summary')
  ))
) # close splitLayout
)




server <- function(input, output) {
  

  output$plot <- renderPlotly({
    
   p <-  df %>% 
      select(date, horizon, everything()) %>% 
      pivot_longer(houst:res_invest) %>% 
      filter(name == input$vars) %>% 
      ggplot(aes(date, value, color = horizon)) +
      scale_color_manual(values=c('purple','dodgerblue')) +
      geom_line(size = 1) +
      
      geom_smooth(method = "loess", span = 0.2, size = .5) +
      xlab("")+
      scale_y_continuous()
    
    ggplotly(p)
  
   
  })
  
  # table output ----
  output$table <- renderDataTable(table_df)
  
  
  # Need help here ----
  
  table_roc <- reactive({
    summary_table %>% 
      na.omit() %>% 
      mutate(Year = as.character(year(date)),
             yoy_change = scales::percent(yoy_change, accuracy = 0.2L)
      ) %>% 
      select(-date) %>% 
      filter(name == input$vars) %>% tail(10)  #input$vars
  
  })
  
  output$table_summary <- renderTable({
    table_roc()
    })
 
  
  
}




# Run the application ----
shinyApp(ui = ui, server = server)


  
  
  
