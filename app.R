library(shiny)
library(shinydashboard)
library(tidyverse)
library(tibble)
library(dplyr)
library(ggplot2)
library(readr)

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "e-Commerce Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow()
            )
        )
    )
)

server <- function(input, output) {}

shinyApp(ui, server)