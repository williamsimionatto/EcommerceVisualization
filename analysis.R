library(tidyverse)
library(googleVis)
library(leaflet)
library(maps)
library(datasets)

reviews = read.csv('brazilian-ecommerce-data/olist_order_reviews_dataset.csv')
payments = read.csv('brazilian-ecommerce-data/olist_order_payments_dataset.csv')
orders = read.csv('brazilian-ecommerce-data/olist_orders_dataset.csv')
customer = read.csv('brazilian-ecommerce-data/olist_customers_dataset.csv')
products = read.csv('brazilian-ecommerce-data/olist_products_dataset.csv')
items = read.csv('brazilian-ecommerce-data/olist_order_items_dataset.csv')
sellers = read.csv('brazilian-ecommerce-data/olist_sellers_dataset.csv')
geolocation = read.csv('brazilian-ecommerce-data/olist_geolocation_dataset.csv')
categorynames = read.csv('brazilian-ecommerce-data/product_category_name_translation.csv')

# Cleaning category names
cleaned_categoria <- products %>%
          select(product_id, product_category_name) %>%
          left_join(categorynames, by = 'product_category_name') %>%
          select(-2)

# Cleaning Geographical data
cleaned_geo <- geolocation %>%
  select(geolocation_zip_code_prefix,
         geolocation_lat,
         geolocation_lng) %>%
  group_by(geolocation_zip_code_prefix) %>%
  summarise(lat = mean(geolocation_lat),
            lng = mean(geolocation_lng))



                    