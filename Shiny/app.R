library(tidyverse)

# loading dfs
geo_df = read.csv("data/geo_df.csv", stringsAsFactors = F)
time_df = read.csv("data/time_df.csv", stringsAsFactors = F)
cat_df = read.csv("data/cat_df.csv", stringsAsFactors = F)
cat_time_df = read.csv("data/cat_time_df.csv", stringsAsFactors = F)
