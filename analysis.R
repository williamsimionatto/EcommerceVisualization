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
  filter(order_delivered_customer_date != '',
         order_delivered_carrier_date != '',
         order_estimated_delivery_date != '') %>%
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

# Cleaning Table Data
final_order = joined_order %>%
  filter(order_status != "canceled" & value > 0) %>%
  rename(
    purchase_date = order_purchase_timestamp,
    delivered_date = order_delivered_customer_date,
    est_delivered_date = order_estimated_delivery_date,
    zip = customer_zip_code_prefix,
    city = customer_city,
    state = customer_state,
    product_category = product_category_name_english
  ) %>%
  mutate(
    purchase_date = lubridate::as_datetime(purchase_date, tz = "UTC"),
    delivered_date = lubridate::as_datetime(delivered_date, tz = "UTC"),
    est_delivered_date = lubridate::as_datetime(est_delivered_date, tz = "UTC")
  ) %>%
  distinct() %>%
  mutate(
    purchase_date = lubridate::date(purchase_date),
    delivered_date = lubridate::date(delivered_date),
    est_delivered_date = lubridate::date(est_delivered_date)
  ) %>%
  mutate(delivery_days = delivered_date - purchase_date,
         diff_estdel = est_delivered_date - delivered_date) %>%
  mutate(
    product_category = case_when(
      product_category == "home_appliances_2" ~ "home_appliances",
      product_category == "home_comfort_2" ~ "home_comfort",
      product_category == "home_confort" ~ "home_comfort",
      product_category == "fashio_female_clothing" ~ "fashion_female_clothing",
      product_category == "art" ~ "arts_and_craftmanship",
      product_category == "drinks" | product_category == "food" ~ "food_drink",
      TRUE ~ as.character(product_category)
    )
  )

# Generating df for geo analysis
geo_df = final_order %>%
  filter(is.na(shipping_cost) == F & is.na(delivery_days) == F & is.na(review_score) == F, is.na(diff_estdel) == F) %>%
  group_by(state) %>%
  summarise(
    sales = sum(value),
    salesperorder = round(sum(value) / n(), 2),
    avg_shipcost = round(mean(shipping_cost), 2),
    avg_shcsratio = round(mean(shipping_cost / value), 2),
    avg_delidays = round(mean(delivery_days), 2),
    avg_review = round(mean(review_score), 3),
    avg_diffestdel = round(mean(diff_estdel), 2)
  ) %>%
  mutate(
    state = case_when(
        state == "AC" ~ "Acre",
        state == "AL" ~ "Alagoas",
        state == "AP" ~ "Amapá",
        state == "AM" ~ "Amazonas",
        state == "BA" ~ "Bahia", 
        state == "CE" ~ "Ceará", 
        state == "DF" ~ "Distrito Federal",
        state == "ES" ~ "Espírito Santo",
        state == "GO" ~ "Goiás",
        state == "MA" ~ "Maranhão", 
        state == "MT" ~ "Mato Grosso",
        state == "MS" ~ "Mato Grosso do Sul",
        state == "MG" ~ "Minas Gerais",
        state == "PA" ~ "Pará",
        state == "PB" ~ "Paraíba",
        state == "PR" ~ "Paraná",
        state == "PE" ~ "Pernambuco",
        state == "PI" ~ "Piauí",
        state == "RJ" ~ "Rio de Janeiro",
        state == "RN" ~ "Rio Grande do Norte",
        state == "RS" ~ "Rio Grande do Sul",
        state == "RO" ~ "Rondônia",
        state == "RR" ~ "Roraima",
        state == "SC" ~ "Santa Catarina",
        state == "SP" ~ "São Paulo",
        state == "SE" ~ "Sergipe",
        state == "TO" ~ "Tocantins",
        TRUE ~ as.character(state)
      )
  )

# Generating analysis by time
time_df = final_order %>%
  filter(is.na(purchase_date) == F) %>%
  mutate(purchase_date = as.Date(purchase_date)) %>%
  group_by(purchase_date) %>%
  summarise(salesbyday = sum(value))

# Generating analysis by categories
detailed_cat_df = final_order %>%
  filter(is.na(product_category) == F) %>%
  group_by(product_category) %>%
  summarise(
    total_sales = sum(value),
    salesperitem = round(sum(value) / n(), 2),
    avg_review = round(mean(review_score), 3)
  )

detailed_cat_df = detailed_cat_df %>% 
  mutate(avg_review = if_else(is.na(avg_review), 0, avg_review))

detailed_cat_df = detailed_cat_df %>% 
  mutate(category = case_when(
      product_category %in% c("health_beauty", "perfumery", "diapers_and_hygiene") ~ "Chemists.Drgustores", 
      product_category %in% c("watches_gifts", "fashion_bags_accessories", "luggage_accessories", "fashion_shoes", 
                              "fashion_male_clothing", "fashion_female_clothing", "fashion_childrens_clothes", 
                              "fashion_underwear_beach") ~ "Clothing",
      product_category %in% c("cool_stuff", "construction_tools_construction", "home_construction", "construction_tools_lights", 
                              "construction_tools_safety", "arts_and_craftmanship", "costruction_tools_tools") ~ "DIY Goods",
      product_category %in% c("computers_accessories", "telephony", "computers", "electronics", "consoles_games", 
                              "fixed_telephony", "air_conditioning", "tablets_printing_image", "cine_photo") ~ "Eletrical Goods",
      product_category %in% c("food_drink", "la_cuisine", "flowers") ~ "Food.Consumables", 
      product_category %in% c("furniture_decor", "office_furniture", "furniture_living_room", "home_comfort", 
                              "kitchen_dining_laundry_garden_furniture", "small_appliances_home_oven_and_coffee", 
                              "furniture_bedroom", "furniture_mattress_and_upholstery") ~ "Furniture.Carpets",
      product_category %in% c("garden_tools", "costruction_tools_garden") ~ "Garden Products", 
      product_category %in% c("bed_bath_table", "housewares", "auto", "toys", "baby", "stationery", "pet_shop",
                              "pet_shop", "home_appliances", "small_appliances", "party_supplies", "christmas_supplies") ~ "Household.Textiles", 
      product_category %in% c("musical_instruments", "audio", "cds_dvds_musicals", "dvds_blu_ray", "music", "books_imported", 
                              "books_technical", "books_general_interest") ~ "Music, Films.Books",
      product_category %in% c("sports_leisure", "fashion_sport") ~ "Sports Equipments", 
      product_category %in% c("market_place", "agro_industry_and_commerce", "industry_commerce_and_business", "signaling_and_security",
                              "security_and_services") ~ "Services", 
      TRUE ~ "Other")) %>%
  rename(sub_category = product_category)

cat_df = detailed_cat_df %>% 
  group_by(category) %>%
  summarise(total_sales = mean(total_sales), 
            aov = mean(salesperitem), 
            avg_review = mean(avg_review))

# Categrorical sales by time
time_df2 = time_df %>% rename(sales = salesbyday) %>% mutate(category = "Total Sales") %>% 
  select(purchase_date, category, sales)

cat_time_df = final_order %>%
  filter(is.na(product_category) == F) %>%
  mutate(category = case_when(
      product_category %in% c("health_beauty", "perfumery", "diapers_and_hygiene") ~ "Chemists.Drgustores", 
      product_category %in% c("watches_gifts", "fashion_bags_accessories", "luggage_accessories", "fashion_shoes", 
                              "fashion_male_clothing", "fashion_female_clothing", "fashion_childrens_clothes", 
                              "fashion_underwear_beach") ~ "Clothing",
      product_category %in% c("cool_stuff", "construction_tools_construction", "home_construction", "construction_tools_lights", 
                              "construction_tools_safety", "arts_and_craftmanship", "costruction_tools_tools") ~ "DIY Goods",
      product_category %in% c("computers_accessories", "telephony", "computers", "electronics", "consoles_games", 
                              "fixed_telephony", "air_conditioning", "tablets_printing_image", "cine_photo") ~ "Eletrical Goods",
      product_category %in% c("food_drink", "la_cuisine", "flowers") ~ "Food.Consumables", 
      product_category %in% c("furniture_decor", "office_furniture", "furniture_living_room", "home_comfort", 
                              "kitchen_dining_laundry_garden_furniture", "small_appliances_home_oven_and_coffee", 
                              "furniture_bedroom", "furniture_mattress_and_upholstery") ~ "Furniture.Carpets",
      product_category %in% c("garden_tools", "costruction_tools_garden") ~ "Garden Products", 
      product_category %in% c("bed_bath_table", "housewares", "auto", "toys", "baby", "stationery", "pet_shop",
                              "pet_shop", "home_appliances", "small_appliances", "party_supplies", "christmas_supplies") ~ "Household.Textiles", 
      product_category %in% c("musical_instruments", "audio", "cds_dvds_musicals", "dvds_blu_ray", "music", "books_imported", 
                              "books_technical", "books_general_interest") ~ "Music, Films.Books",
      product_category %in% c("sports_leisure", "fashion_sport") ~ "Sports Equipments", 
      product_category %in% c("market_place", "agro_industry_and_commerce", "industry_commerce_and_business", "signaling_and_security",
                              "security_and_services") ~ "Services", 
      TRUE ~ "Other")) %>%
  group_by(purchase_date, category) %>%
  summarise(sales = sum(value)) %>%
  bind_rows(time_df2) %>%
  spread(key = "category", value = "sales")

# Export dfs
write.csv(final_order, file = "Shiny/data/final_order.csv", row.names = F)
write.csv(geo_df, file = "Shiny/data/geo_df.csv", row.names = F)
write.csv(time_df, file = "Shiny/data/time_df.csv", row.names = F)
write.csv(cat_df, file = "Shiny/data/cat_df.csv", row.names = F)
write.csv(cat_time_df, file = "Shiny/data/cat_time_df.csv", row.names = F)
write.csv(head(products, 50), file = "Shiny/data/products.csv", row.names = F)

