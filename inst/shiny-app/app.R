library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(bslib)

aus_map <- rnaturalearth::ne_states(country = "australia", returnclass = "sf") %>%
  dplyr::filter(!name %in% c("Ashmore and Cartier Islands", "Coral Sea Islands"))

ui <- navbarPage(
  "HousePack – Australian Housing Explorer",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  nav_spacer(),
  header = tags$style(HTML("
  .text-muted.small {
    font-style: italic;
    margin-top: 5px;
    display: block;
  }
")),

  # ----------------------- Overview Tab -----------------------
  tabPanel("Overview",
           sidebarLayout(

             # ---------------- LEFT SIDEBAR ----------------
             sidebarPanel(
               width = 3,
               h4("Controls"),
               selectInput(
                 "metric1", "Select variable:",
                 choices = c(
                   "Income per capita" = "income",
                   "Detached house price (Australia)" = "house",
                   "Compare: Income vs House Price" = "compare"
                 ),
                 selected = "compare"
               ),
               sliderInput(
                 "year_trend", "Select year:",
                 min = 1985, max = 2024, value = 2024,
                 step = 1, sep = ""
               ),
               hr(),
               helpText("Use these controls to explore trends in income, housing, and capital gains.")
             ),

             # ---------------- MAIN PANEL ----------------
             mainPanel(
               width = 9,
               tabsetPanel(
                 id = "main_tabs",
                 type = "pills",

                 # ----- TAB 1: National Trends -----
                 tabPanel("National Trends",
                          fluidRow(
                            column(
                              width = 6,
                              h4("Income and Housing Price Trends"),
                              plotlyOutput("trend_plot_left", height = "400px"),
                            p(class = "text-muted small",
                              "This plot compares how household disposable income and detached house prices
         have changed over time. The index is normalised to 100 in 1990 to highlight
         how housing costs have grown much faster than income.")
                          ),
                            column(
                              width = 6,
                              h4("Capital Gains from Property Sales"),
                              plotlyOutput("trend_plot_right", height = "400px"),
                              p(class = "text-muted small",
                                "This chart shows the average capital gains reported by taxable individuals
   each financial year. Bars represent average realised gains in thousands of AUD.
   Viewed side by side with the 'Income vs House Price' plot, it highlights how the surge
   in property prices aligns with rising capital gains - both moving in the same direction
   almost the whole period.")
                            )
                          )
                 ),

                 # ----- TAB 2: State Map -----
                 tabPanel("State Map",
                          h4("Regional Differences in Detached House Prices"),
                          plotlyOutput("map_plot", height = "400px"),
                          p(class = "text-muted small",
                            "Darker colours indicate higher detached house price indexes in each state for the
   selected year. For example, in 2024 New South Wales (around 250) stands out with
   much higher prices compared to other regions (typically between 170-190), showing how housing affordability pressures are
   concentrated in Sydney and nearby areas."),
                          br(),
                          h4("Trends by State"),
                          fluidRow(
                            column(
                              width = 4,
                              selectInput(
                                "selected_state", "Select State:",
                                choices = c("All", "New South Wales", "Victoria", "Queensland",
                                            "South Australia", "Western Australia", "Tasmania",
                                            "Northern Territory", "Australian Capital Territory"),
                                selected = "All"
                              )
                            )
                          ),
                          plotlyOutput("state_trend_plot", height = "300px"),
                          p(class = "text-muted small",
                            "This line chart shows how detached house prices have increased across states over time.
         Prices rose steadily nationwide, with New South Wales showing the most significant
         long-term growth among all regions. Selecting 'All' shows all states together or choose
         a specific state highlights its price trend across the years.")
                 )
               )
             )
           )
  ),

  # ----------------------- Data Source Tab -----------------------
  tabPanel("Data Source",
           fluidPage(
             h3("Datasets included in HousePack"),
             p("Below are the datasets included in the package. You can search, sort, or browse them interactively."),
             br(),
             tabsetPanel(
               id = "data_tabs",
               type = "pills",
               tabPanel("Income per Capita", DT::dataTableOutput("table_income")),
               tabPanel("Detached House Prices", DT::dataTableOutput("table_housing")),
               tabPanel("Capital Gains", DT::dataTableOutput("table_cgt"))
             )
           )
  ),

  # ----------------------- State Map Tab (Code view) -----------------------
  tabPanel("Code",
           fluidPage(
             h3("Server Code for Plots"),
             verbatimTextOutput("server_code")
           )
  )
)

server <- function(input, output, session) {
  income  <- housepack::income_pc
  housing <- housepack::detached_full
  cgt     <- housepack::cgt_full

  # Unified transparent theme for all plots
  theme_housepack <- ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0.5, size = 12
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = 10, colour = "grey30"
      ),
      axis.title = ggplot2::element_text(
        size = 10, face = "plain", colour = "grey25"
      ),
      axis.text = ggplot2::element_text(
        size = 9, colour = "grey30"
      ),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid       = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(colour = "grey50"),
      legend.title     = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 9)
    )


  output$map_plot <- renderPlotly({
    housing_year <- housing %>%
      dplyr::filter(Year == input$year_trend) %>%
      dplyr::select(Year, Quarter, Sydney:Canberra) %>%
      dplyr::summarise(across(Sydney:Canberra, mean, na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "State", values_to = "Index") %>%
      dplyr::mutate(State = dplyr::recode(State,
                                          "Sydney"    = "New South Wales",
                                          "Melbourne" = "Victoria",
                                          "Brisbane"  = "Queensland",
                                          "Adelaide"  = "South Australia",
                                          "Perth"     = "Western Australia",
                                          "Hobart"    = "Tasmania",
                                          "Darwin"    = "Northern Territory",
                                          "Canberra"  = "Australian Capital Territory"))
    output$state_trend_plot <- renderPlotly({
      housing_long <- housing %>%
        dplyr::select(Date, Year, Quarter, Sydney:Canberra) %>%
        tidyr::pivot_longer(
          cols = Sydney:Canberra,
          names_to = "City",
          values_to = "Index"
        ) %>%
        dplyr::mutate(State = dplyr::recode(City,
                                            "Sydney" = "New South Wales",
                                            "Melbourne" = "Victoria",
                                            "Brisbane" = "Queensland",
                                            "Adelaide" = "South Australia",
                                            "Perth" = "Western Australia",
                                            "Hobart" = "Tasmania",
                                            "Darwin" = "Northern Territory",
                                            "Canberra" = "Australian Capital Territory"
        ))

      if (input$selected_state != "All") {
        housing_long <- housing_long %>%
          dplyr::filter(State == input$selected_state)
      }

      p <- ggplot(housing_long, aes(x = Date, y = Index, color = State)) +
        geom_line(linewidth = 0.3) +
        scale_color_viridis_d(option = "mako") +
        labs(
          title = ifelse(input$selected_state == "All",
                         "Detached House Price Trends by State",
                         paste("Detached House Price Trend –", input$selected_state)),
          x = "", y = "Index (Base = 100)"
        ) +
        theme_housepack +
        theme(legend.text = element_text(size = 7))

      plotly::ggplotly(p) %>%
        plotly::layout(
          legend = list(orientation = "h", x = 0.5, y = -0.25, xanchor = "center")
        ) %>%
        plotly::config(displaylogo = FALSE)
    })

    map_data <- aus_map %>%
      dplyr::left_join(housing_year, by = c("name" = "State"))

    validate(
      need(sum(!is.na(map_data$Index)) > 0,
           "No data available for selected year.")
    )

    p <- ggplot(map_data) +
      geom_sf(aes(fill = Index, text = paste0(name, "<br>Index: ", round(Index, 1))),
              color = "white", linewidth = 0.3) +
      scale_fill_viridis_c(option = "mako", na.value = "grey90") +
      coord_sf(expand = FALSE) +
      labs(title = paste("Detached House Price Index by State –", input$year_trend),
           fill = "Index") +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank(),
            plot.title  = element_text(face = "bold", hjust = 0.5, size = 12),
            axis.title  = element_text(size = 10),
            axis.text   = element_text(size = 9),
            legend.text = element_text(size = 9))

    plotly::ggplotly(p) %>%
      plotly::layout(
        p,
        margin = list(l = 0, r = 0, t = 40, b = 0),
        geo = list(fitbounds = "locations", visible = FALSE)
      ) %>%
      plotly::config(displaylogo = FALSE)
  })


  output$trend_plot_left <- renderPlotly({
    if (input$metric1 == "compare") {
      p <- housepack::compare_trends(income %>% filter(Year <= input$year_trend),
                                     housing %>% filter(Year <= input$year_trend)) +
        theme_housepack
    } else if (input$metric1 == "income") {
      p <- income %>%
        filter(Year <= input$year_trend) %>%
        mutate(Date = as.Date(paste0(Year, "-", Quarter * 3, "-01"))) %>%
        ggplot(aes(x = Date, y = Income_per_capita)) +
        geom_line(color = "#6cb4e4", linewidth = 0.6, linetype = "dashed") +
        labs(title = "Real Household Disposable Income per Capita", y = "AUD", x = "") +
        theme_housepack
    } else if (input$metric1 == "house") {
      p <- housing %>%
        filter(Year <= input$year_trend) %>%
        mutate(Date = as.Date(paste0(Year, "-", Quarter * 3, "-01"))) %>%
        ggplot(aes(x = Date, y = Australia)) +
        geom_line(color = "#1c4c74", linewidth = 0.6) +
        labs(title = "Detached House Price Index", y = "Index", x = "") +
        theme_housepack
    }

    plotly::ggplotly(p) %>%
      plotly::layout(legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center")) %>%
      plotly::config(displaylogo = FALSE)
  })




  output$trend_plot_right <- renderPlotly({
    p <- cgt %>%
      filter(Year <= input$year_trend) %>%
      ggplot(aes(x = Year, y = Total_gain_adj)) +
      geom_col(fill = "#1c4c74") +
      labs(title = "Average Capital Gains",
           y = "AUD ('000)", x = "") +
      theme_housepack

    plotly::ggplotly(p) %>% plotly::config(displaylogo = FALSE)
  })

  # ----------------------- Data Tables -----------------------
  output$table_income <- DT::renderDataTable({
    DT::datatable(
      income,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })

  output$table_housing <- DT::renderDataTable({
    DT::datatable(
      housing,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })

  output$table_cgt <- DT::renderDataTable({
    DT::datatable(
      cgt,
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })


  output$server_code <- renderText({
    readLines("app.R") |> paste(collapse = "\n")
  })
}

shinyApp(ui, server)
