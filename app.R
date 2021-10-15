library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(readr)

listOrders <- read_csv("./List_of_Orders.csv")
orderDetails <- read_csv("./Order_Details.csv")
salesTarget <- read_csv("./Sales_target.csv")

dataOrders <- left_join(listOrders, orderDetails, by = "Order ID") %>% drop_na()

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "e-Commerce Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("data")),
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                fluidRow(
                    DTOutput('ordersTable')
                )
            ),
            tabItem(tabName = "dashboard",
                fluidRow(

                )
            )
        )
    )
)

server <- function(input, output) {
    output$ordersTable = renderDT(
        dataOrders, options = list(lengthChange = FALSE)
    )
}

shinyApp(ui, server)
