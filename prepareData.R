#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("readr")

library(tidyverse)
library(ggplot2)
library(readr)

listOrders <- read_csv("./List_of_Orders.csv")
orderDetails <- read_csv("./Order_Details.csv")
salesTarget <- read_csv("./Sales_target.csv")

