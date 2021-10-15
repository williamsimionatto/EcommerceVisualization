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

# Cleaning Payment Table
cleaned_pay = payments %>%
    group_by(order_id) %>%
    summarise(value = sum(payment_value))

# Adding shipping amount per order
shipping_cost = items %>%
  group_by(order_id) %>%
  summarise(shipping_cost = sum(freight_value))

# Joining all tables
joined_order = orders %>%
  left_join(cleaned_pay, by = "order_id") %>%
  left_join(customer, by = "customer_id") %>%
  left_join(cleaned_geo,
            by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix")) %>%
  left_join(reviews, by = "order_id") %>%
  left_join(shipping_cost, by = "order_id") %>%
  left_join(items, by = "order_id") %>%
  left_join(cleaned_categoria, by = "product_id") %>%
  select(
    order_id,
    customer_id,
    order_status,
    order_purchase_timestamp,
    order_delivered_customer_date,
    order_estimated_delivery_date,
    value,
    customer_unique_id,
    customer_zip_code_prefix,
    lat,
    lng,
    customer_city,
    customer_state,
    review_score,
    product_id,
    product_category_name_english,
    shipping_cost
  )
