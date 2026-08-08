# Load required packages
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)

# Load in datasets
sa2_data <- readRDS("clean_data/sa2_data.rds")
map_data <- readRDS("clean_data/map_data.rds")
rank_data <- read_csv("clean_data/rank_data.csv")
time_data <- read_csv("clean_data/time_data.csv")

panel_box <- function(...) {
  div(
    style = "
    border: 1px solid #d9d9d9;
    border-radius: 8px;
    padding: 12px;
    margin-bottom: 15px;
    background-color: white;
    ",
    ...
  )
}

# UI.R
ui <- fluidPage(
  
  titlePanel(
    "Population Change and Public Transport Service Provision in Metropolitan Victoria"
  ),
  panel_box(
    fluidRow(
      column(
        12,
  h5("How to interact with this dashboard"),
  tags$details(
    tags$ul(
      tags$li("Select one or more SA4 regions to filter the analysis."),
      tags$li("Hover over a visual element on the map, scatterplot, or time-series chart to view detailed information"),
      tags$li("Click an SA2 on the map or scatterplot to view detailed information."),
      tags$li("Selected SA2s are highlighted in red across visualisations."),
      tags$li("Use the 'Clear selected SA2' button or click again on the selected SA2 to remove the selection."),
      tags$li("Adjust the year slider to explore changes over time.")
    )
  )))),
  
  panel_box(
  fluidRow(
    column(3),
    column(
      4,
      selectizeInput(
        "sa4",
        "SA4 Region",
        choice = sort(unique(sa2_data$`SA4 name`)),
        multiple = TRUE
      )
    ),
    column(
      4, div(style = "margin-top: 25px;",
             uiOutput("clear_button"))),
    column(1)
  )),
  
  panel_box(
  fluidRow(
    column(
      6,
      h4("Population Change by SA2", style = "text-align:center;"),
      sliderInput(
        "year",
        "Year",
        min = 2002,
        max = max(map_data$year, na.rm = TRUE),
        value = max(map_data$year, na.rm = TRUE),
        step = 1,
        sep = "",
        width = "100%"
        ),
      
      leafletOutput("map", height = 400),
      
      plotOutput("colour_legend", height = 50)
      ),

    column(
      6,
      h4("Overall Population Change vs Service Intensity", style = "text-align:center;"),
      
      plotlyOutput("scatter", height = 500)
      )
    )),
  panel_box(
  fluidRow(
    column(
      12,
      h4("Visualisation Insights (Click to Expand)"),
      tags$details(
        uiOutput("insights_panel")
      )
    )
  )),
  
  panel_box(
  fluidRow(
    column(
      6,
      h4("Population Trend for Selected SA2 (2001 - 2024)", style = "text-align:center;"),
      uiOutput("timeseries_ui", height = 400)
      ),
    
    column(
      6,
      h4("Service Intensity Ranking", style = "text-align:center;"),
      plotOutput("ranked_dot", height = 400)
      )
    )),
  
  panel_box(
  fluidRow(
    column(
      12,
      h4("SA2 Detail Panel", style = "text-align:center;"),
      uiOutput("detail_panel")
      )
    )
  ))

# server.R
server <- function(input, output) {
  selected_sa2 <- reactiveVal(NULL)
  
  toggle_sa2 <- function(clicked, current) {
  if (!is.null(current) && current == clicked) {
        return(NULL)
      } else {
        return(clicked)
      }
  }
  
  selected_year <- reactiveVal(max(map_data$year, na.rm = TRUE))
  
  observeEvent(input$year, {
    selected_year(input$year)
  })
  
  choro_map <- reactive({
    data <- map_data %>%
      filter(year == selected_year())
    
    if (!is.null(input$sa4)) {
      data <- data %>%
        filter(`SA4 name` %in% input$sa4)
    }
    data
  })
  
  observe({
    leafletProxy("map") %>%
      clearShapes()
  })
  
  max_abs <- max(abs(map_data$annual_change), na.rm = TRUE)
  
  pal <- colorNumeric(
    palette = "RdBu",
    domain = c(-max_abs, max_abs),
    na.color = "transparent"
  )
  
  output$map <- renderLeaflet({
    
    bbox <- st_bbox(map_data)
    
    leaflet() %>%
      fitBounds(
        lng1 = as.numeric(bbox["xmin"] + 0.186),
        lat1 = as.numeric(bbox["ymin"] + 0.186),
        lng2 = as.numeric(bbox["xmax"] - 0.186),
        lat2 = as.numeric(bbox["ymax"] - 0.186)
      )
  })
  
  observe({
    
    selected <- selected_sa2()
    
    year <- selected_year()
    
    data <- choro_map()
    
    if (!is.null(selected)) {
      data <- data %>%
        mutate(selected = SA2_CODE_2021 == selected)
    } else {
      data <- data %>%
        mutate(selected = FALSE)
    }
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        layerId = ~SA2_CODE_2021,
        fillColor = ~pal(annual_change),
        fillOpacity = ~ifelse(selected, 1, 0.6),
        color = ~ifelse(selected, "red", "black"),
        weight = ~ifelse(selected, 3.5, 0.5),
        label = ~paste0(
          "<b>", SA2_NAME_2021, "</b><br>",
          "Year: ", year, "<br>",
          "Annual Change: ", scales::comma(annual_change)
        ) %>% lapply(HTML)
      )
  })
  
  scatter_data <- reactive({
    data <- sa2_data %>%
      st_drop_geometry()
    
    if(length(input$sa4) > 0){
      data <- data %>%
        filter(
          `SA4 name` %in% input$sa4
        )
    }
    data
  })
  
  output$scatter <- renderPlotly({
    
    selected <- selected_sa2()
    
    data <- scatter_data() 
    
    data <- data %>%
      mutate(
        is_selected = if(!is.null(selected)) {
          SA2_CODE_2021 == selected
        } else {
          FALSE
        }
        )
    
    p <- ggplot(
      data,
      
      aes(x = absolute_change,
          y = service_intensity,
          key = SA2_CODE_2021,
          text = paste(
            SA2_NAME_2021,
            "<br>Population Change:",
            scales::comma(absolute_change),
            "<br>Service Intensity:",
            round(service_intensity, 1)
          ))
    ) +
      geom_point(
          colour = ifelse(data$is_selected, "red", "black"),
          size = ifelse(data$is_selected, 3, 1)
      ) +
      
      geom_hline(
        yintercept = median(sa2_data$service_intensity, na.rm = TRUE),
        linetype = "dashed"
      ) +
      
      labs(
        x = "Population Change",
        y = "Service Intensity",
        subtitle = paste("Showing", nrow(data), "SA2s")
      ) +
      theme_minimal() + 
      theme(legend.position = "none")
    
    p_plotly <- ggplotly(
      p,
      tooltip = "text",
      source = "scatter"
    )
    
    event_register(
      p_plotly,
      "plotly_click"
    )
    
    p_plotly
  })
  
  observeEvent(event_data("plotly_click", source = "scatter"), {
    
    click <- event_data("plotly_click", source = "scatter")
    req(click$key)
    
    selected_sa2(
      toggle_sa2(click$key, isolate(selected_sa2()))
    )
  })
  
  observeEvent(input$map_shape_click, {
    req(input$map_shape_click$id)
    
    selected_sa2(
      toggle_sa2(input$map_shape_click$id, isolate(selected_sa2()))
    )
  })
  
  output$timeseries_ui <- renderUI({
    
    if (is.null(selected_sa2())) {
      
      div(
        style = "
        height:400px;
        display:flex;
        align-items:center;
        justify-content:center;
        text-align:center;
        color:grey50;
      ",
        div(
          h5("No SA2 selected"),
          p("Select an SA2 from the map or scatterplot to view population trends.")
        )
      )
      
    } else {
      
      plotlyOutput("timeseries", height = 400)
      
    }
  })
  
  output$timeseries <- renderPlotly({
    
    req(selected_sa2())
    
    data <- time_data %>%
      filter(SA2_CODE_2021 == selected_sa2())
    
    current_year <- selected_year()
    
    p <- ggplot(
      data, 
      aes(
        year, 
        population,
        group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Population: ", scales::comma(population)
          )
        )
      ) +
      geom_line() +
      geom_point(
        data = data %>%
          filter(year == current_year),
        colour = "red",
        size = 3
      ) +
      labs(
        title = unique(data$SA2_NAME_2021), 
        x = "Year", 
        y = "Population"
        ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$ranked_dot <- renderPlot({
    data <- rank_data
    
    if (length(input$sa4) > 0) {
      data <- data %>%
        filter(`SA4 name` %in% input$sa4)
    }
    
    selected <- selected_sa2()
    
    if(is.null(selected)) {
      data <- data %>%
        arrange(desc(service_intensity)) %>%
        slice_head(n = 20)
    } else {
      
      selected_rank <- data %>%
        filter(SA2_CODE_2021 == selected) %>%
        pull(rank)
      
      if (length(selected_rank) == 0) {
        data <- data %>%
          arrange(desc(service_intensity)) %>%
          slice_head(n = 20)
      } else {
        data <- data %>%
          filter(
            rank >= selected_rank - 10,
            rank <= selected_rank + 10
          )
      }
    }
    
    data <- data %>%
      mutate(
        is_selected = if (!is.null(selected)) {
          SA2_CODE_2021 == selected
        } else {
          FALSE
        }
      )
    
    median_service <-median(rank_data$service_intensity, na.rm = TRUE)
    
    ggplot(
      data,
      aes(x = service_intensity, y = reorder(SA2_NAME_2021, service_intensity))
    ) +
      geom_point(colour = ifelse(data$is_selected, "red", "black"),
                 size = ifelse(data$is_selected, 3, 1)) +
      
      geom_vline(xintercept = median_service, linetype = "dashed") +
      labs(x = "Service Intensity",
           y = NULL,
           caption = "Dashed line shows metropolitan Victoria median service intensity") +
      theme_minimal() +
      theme(plot.caption = element_text(hjust = 0, size = 9, colour = "grey20"))
  })
  
  ranked_data <- reactive({
    data <- rank_data
    if (length(input$sa4) > 0) {
      data <- data %>%
        filter(`SA4 name` %in% input$sa4)
    }
    data
  })
  
  output$detail_panel <- renderUI({
    
    if (is.null(selected_sa2())) {
      return(
        div(
          style = "text-align:center; padding:20px; color:grey50",
          h5("No SA2 selected"),
          p("Select an SA2 from the map or scatterplot to view detailed information.")
        )
      )
    }
    data <- ranked_data()
    
    row <- sa2_data %>%
      st_drop_geometry() %>%
      filter(
        SA2_CODE_2021 == selected_sa2()
      )
    
    rank_row <- data %>%
      filter(SA2_CODE_2021 == selected_sa2())
    
    median_service <- median(rank_data$service_intensity, na.rm = TRUE)
    
    service_diff <- row$service_intensity - median_service
    
    service_pct_diff <- (service_diff / median_service) * 100
    
    insight <- case_when(
      row$service_intensity > median_service &
        row$percent_change > median(sa2_data$percent_change, na.rm = TRUE) ~
        "This SA2 has above-median service intensity and strong population growth,
      suggesting transport provision has broadly kept pace with demand.",
      row$service_intensity > median_service ~
        "This SA2 has above-median service intensity, indicating relatively strong
      public transport provision compared with other SA2s in metropolitan Victoria.",
      row$service_intensity < median_service &
        row$percent_change > median(sa2_data$percent_change, na.rm = TRUE) ~
        "This SA2 combines strong population growth with below-median service intensity,
      suggesting population growth may be outpacing transport service provision.",
      TRUE ~
        "This SA2 has below-median service intensity relative to other SA2s in metropolitan
      Victoria, indicating lower transport service provision than many comparable areas."
      
    )
    req(nrow(row) > 0)
    
    tagList(
      
      h4(row$SA2_NAME_2021),
      
      p(paste("SA4:", row$`SA4 name`)),
      
      p(paste("Absolute Population Change from 2001 to 2024:", scales::comma(row$absolute_change))),
      
      p(paste("Percent Population Change from 2001 to 2024:", round(row$percent_change, 1), "%")),
      
      p(paste("Service Intensity:", round(row$service_intensity))),
      
      p(paste("Service Intensity Per Capita:", round(row$service_per_capita, 3))),
      
      p(paste("Difference from Metro VIC Median:", 
              ifelse(service_diff > 0, "+", ""), 
              round(service_diff), 
              " (", 
              ifelse(service_pct_diff > 0, "+", ""), 
              round(service_pct_diff, 1), "%)"
              )
        ),
      
      p(paste("Rank:", rank_row$rank, "/", nrow(rank_data))),
      
      tags$hr(),
      
      h5("Key Insight"),
      
      p(insight)
      
    )
  })
  
  output$insights_panel <- renderUI({
    year_data <- map_data %>%
      filter(year == input$year)
    
    high_growth_share <- mean(year_data$annual_change > 0, na.rm = TRUE)
    
    top_growth_sa4 <- year_data %>%
      group_by(`SA4 name`) %>%
      summarise(avg_change = mean(annual_change, na.rm = TRUE)) %>%
      arrange(desc(avg_change)) %>%
      slice(1) %>%
      pull(`SA4 name`)
    
    tagList(
      h4("Key insights:"),
      p("- Service intensity and population change show a weak-to-moderate relationship, indicating service provision does not scale perfectly with growth across SA2s."),
      p("- High growth SA2s are concentrated in outer metropolitan and growth corridor regions, rather than inner metropolitan areas."),
      
      tags$hr(),
      
      h4(paste0("Context for ", input$year, ":")),
      
      p(paste0("- In ", input$year, ", approximately ", round(high_growth_share * 100, 1), "% of SA2s recorded positive population change.")),
      p(paste0("- The SA4 with the strongest average population growth this year is ", top_growth_sa4, "."))
    )
  })
  
  output$clear_button <- renderUI({
    req(selected_sa2())
    
    actionButton("clear_selection", "Clear selected SA2")
  })
  
  observeEvent(input$clear_selection, {selected_sa2(NULL)})
  
  output$colour_legend <- renderPlot({
    
    df <- data.frame(
      value = seq(-max_abs, max_abs, length.out = 100),
      y = 1
    )
    
    ggplot(df, aes(value, y, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
      scale_y_continuous(NULL, breaks = NULL) +
      labs(x = "Annual Population Change") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
}

shinyApp(ui, server)