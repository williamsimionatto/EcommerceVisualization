library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(googleVis)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)

# loading dfs
geo_df = read.csv("Shiny/data/geo_df.csv", stringsAsFactors = F)
time_df = read.csv("Shiny/data/time_df.csv", stringsAsFactors = F)
cat_df = read.csv("Shiny/data/cat_df.csv", stringsAsFactors = F)
cat_time_df = read.csv("Shiny/data/cat_time_df.csv", stringsAsFactors = F)
products = read.csv("Shiny/data/products.csv", stringsAsFactors = F)
states = geojsonio::geojson_read("Shiny/data/br-states.json", what = "sp")

geo_choices = list(
  "Total Sales" = names(geo_df)[[2]],
  "Average Order Value" = names(geo_df)[[3]],
  "Average Shipping Cost" = names(geo_df)[[4]],
  "Shipping Cost/Order Ratio" = names(geo_df)[[5]],
  "Average Delivery Days" = names(geo_df)[[6]],
  "Average Review Score" = names(geo_df)[[7]],
  "Est - Actual Deliver Time" = names(geo_df)[[8]]
)

trd_choices = sort(colnames(cat_time_df)[2:13])

catvalue_choices = list(
  "Total Sales" = names(cat_df)[[2]],
  "Unit Sales" = names(cat_df)[[3]],
  "Average Review Score" = names(cat_df)[[4]]
)

cats_choices = sort(cat_df$category)

bar_options = list(
  width = "automatic",
  height = "800px",
  bar = "{groupWidth: '80%'}",
  hAxis = "{title:'Sales (in $BRL)', format: 'short', scaleType: 'log'}",
  animation = "{startup: true}",
  legend = "none"
)

ui <- fluidPage(
  theme = "style.css", 
  shinyUI(
    dashboardPage(
      skin = "blue",
      dashboardHeader(title = "E-commerce"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Products", tabName = "product", icon = icon("table")),
          menuItem("Geographic", tabName = "geo", icon = icon("map")),
          menuItem("Trends", tabName = "time", icon = icon("line-chart")),
          menuItem( "Categories", tabName = "cat", icon = icon("dashboard"))
        )
      ),
      dashboardBody(
        tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
        tabItems(
          tabItem(tabName = "product",
                    fluidRow(
                      box(
                        title = "Products",
                        solidHeader = T,
                        collapsible = T,
                        width = NULL,
                        status = "info",
                        DT::DTOutput("product_table")
                      )
                    )
                  ),
          tabItem(tabName = "geo", 
            fluidRow(
              column(
                width = 9,
                box(
                  title = "Map",
                  solidHeader = T,
                  status = "info",
                  leafletOutput("geo", height = 800),
                  width = NULL,
                  height = "auto"
                )
              ),
              column(
                width = 3,
                box(
                  title = "Select to Plot",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  selectizeInput("geoin", label = NULL, geo_choices)
                ),
                box(
                  title = "Data",
                  solidHeader = T,
                  collapsible = T,
                  width = NULL,
                  status = "info",
                  tableOutput({"table"})
                )
              )
            )
          ),
          tabItem(
            tabName = "time",
            fluidRow(
              column(
                width = 9,
                box(
                  title = "Trends",
                  solidHeader = T,
                  width = NULL,
                  height = 1000,
                  status = "info",
                  htmlOutput("tim")
                )
              ),
              column(
                width = 3,
                box(
                  title = "Date Range Input",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  dateRangeInput(
                    "datein",
                    label = NULL,
                    start = head(time_df$purchase_date, 1),
                    end = tail(time_df$purchase_date, 1),
                    min = head(time_df$purchase_date, 1),
                    max = tail(time_df$purchase_date, 1)
                  )
                )
              ),
              column(
                width = 3,
                box(
                  title = "Categories To Plot",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  pickerInput(
                    inputId = "trdcats",
                    choices = trd_choices,
                    selected = trd_choices[12],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                  )
                )
              )
            )
          ),
          tabItem(
            tabName = "cat",
            fluidRow(
              column(
                width = 9,
                box(
                  title = "Bar Chart",
                  solidHeader = T,
                  width = NULL,
                  height = 500,
                  status = "info",
                  htmlOutput("cat")
                )
              ),
              column(
                width = 3,
                box(
                  title = "Variables Input",
                  solidHeader = T,
                  width = NULL,
                  status = "info",
                  selectInput(
                    "catvalue",
                    label = NULL,
                    choices = catvalue_choices,
                    selected = "total_sales"
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  bins = reactiveValues()
  labtxt = reactiveValues()
  
  # Switching labels for map
  observe({
    if (input$geoin == "sales") {
      labtxt$x = "<strong>%s</strong><br/><strong>Sales:</strong> $%s BRL"
      bins$y = c(0, 50000, 100000, 200000, 300000, 400000, 1000000, 2000000, 5000000, Inf)
    } else if (input$geoin == "avg_shcsratio") {
      labtxt$x = "<strong>%s</strong><br/><strong>Ratio:</strong> %s"
      bins$y = 9
    } else if (input$geoin == "avg_review") {
      labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %s"
      bins$y = 9
    } else if (input$geoin %in% c("avg_delidays", "avg_diffestdel")) {
      labtxt$x = "<strong>%s</strong><br/>%s Days"
      bins$y = 9
    } else {
      labtxt$x = "<strong>%s</strong><br/>$%s BRL"
      bins$y = 9
    }

    c = input$trdcats

    if (is.null(c))
      c = character(0)
    
    updateSelectInput(session, "catsfortable", choices = c, selected = head(c, 1))
  })
  
  geo_df_table = reactive({
    req(input$geoin)
    geo_df %>%
      select(state, value =  input$geoin) %>%
      arrange(desc(value))
  })
  
  cat_df_bar = reactive({
    req(input$catvalue)
    cat_df %>%
      select(category, value = input$catvalue) %>%
      arrange(., desc(value))
  })
  
  # Graph for Map
  output$geo = renderLeaflet({
    pal = colorBin("Reds", geo_df[, input$geoin], bins = bins$y, pretty = F)
    
    labels = sprintf(labtxt$x,
                     states$nome,
                     format(
                       geo_df[, input$geoin],
                       scientific = F,
                       big.mark = ","
                     )) %>% lapply(htmltools::HTML)

    geo = leaflet(states) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(geo_df[, input$geoin]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "15px",
          direction = "auto"
        )
      )

    geo %>%
      addLegend(
        pal = pal,
        values = geo_df[, input$geoin],
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
  })

  output$product_table = DT::renderDT({
    DT::datatable(
      products,
      extensions = "FixedColumns",
      options = list(
        fixedColumns = list(leftColumns = 3),
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = "t"
      )
    )
  })

  # Geo Data Output
  output$table = DT::renderDataTable(
    {head(geo_df_table(), 6)},
    striped = T,
    spacing = 'l',
    width = '100%',
    colnames = F,
    digits = 2
  )
  
  # Categories Bar Chart
  output$cat = renderGvis(
    gvisBarChart(
      cat_df_bar(),
      options = list(
        width = "automatic",
        height = "300px",
        bar = "{groupWidth: '60%'}",
        hAxis = "{title:'Sales (in $BRL)', format: 'short', scaleType: 'log'}",
        animation = "{startup: true}",
        legend = "none"
      )
    )
  )
}

shinyApp(ui = ui, server = server)
