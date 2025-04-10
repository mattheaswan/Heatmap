#load packages
rm(list=ls())
install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("rgdal")
install.packages("ggmap")
install.packages("ggplot2")
install.packages("rgeos")
install.packages("broom")
install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("readxl")
install.packages("tidyverse")
install.packages("rgdal")
install.packages("sf")
install.packages("devtools")
library(magrittr)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(dplyr)
library(readxl)
library(dplyr)
library(tidyverse)
library(rgdal)
library(sf)
library(devtools)


#read in postcode lookup and FRF
postcode_payments <- read_xlsx("C:/Users/ms000074/OneDrive - Defra/Beneficiary analysis/Postcodes_payments_map.xlsx")
postcode_mapping <- read.csv("C:/Users/ms000074/OneDrive - Defra/Beneficiary analysis/NSPL_FEB_2025_UK.csv")

#rename postcode column in postcode payments
postcode_payments <- postcode_payments %>% rename(pcd = Postcode)


#select columns from NSPL and FRF
postcode_mapping <- postcode_mapping %>% select(pcd, lat, long)
postcode_payments <- postcode_payments %>% select(pcd, "Total payment")



#match payment to location point data
postcode_payments_merge <- merge(postcode_payments, postcode_mapping, by = "pcd")

#Load the ONS counties shapefile and rename columns
counties_shapfile <- st_read("C:/Users/ms000074/OneDrive - Defra/Beneficiary analysis/CTYUA_DEC_2024_UK_BFC.shp")

counties_shapfile <- counties_shapfile %>% rename(long = LONG)
counties_shapfile <- counties_shapfile %>% rename(lat = LAT)

#join postcode data with shapefile
postcode_sf <- st_as_sf(postcode_payments_merge, coords = c("long", "lat"), crs = 4326)

postcode_sf <- st_transform(postcode_sf, crs = st_crs(counties_shapfile))

postcode_with_counties <- st_join(postcode_sf, counties_shapfile)

postcode_with_counties <- postcode_with_counties %>% rename(payment = "Total payment")

# Aggregate payments by county or unitary authority
payment_by_area <- postcode_with_counties %>%
  group_by(CTYUA24CD) %>%
  summarise(total_payment = sum(payment, na.rm = TRUE))

counties_payment_map <- counties_shapfile %>%
  +   left_join(payment_by_area, by = CTYUA24CD)


#plot map
ggplot(data = counties_payment_map) +
  geom_sf(aes(fill = total_payment)) +  # Fill each county based on the total payment
  scale_fill_viridis_c() +  # Color scale (you can choose other color scales)
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(title = "Total Payments by County/Unitary Authority", fill = "Total Payment") +
  theme(legend.position = "right")

#replace N/As with 0 so they are plotted on the map as no payment
counties_payment_map <- counties_payment_map %>% mutate_all(~replace(., is.na(.), 0))


#add bands to data
counties_payment_map$payment_bands <- cut(counties_payment_map$total_payment, 
                             breaks = c(0, 1, 2986, 100000, 1000000, 5000000),  # Customize these breakpoints as needed
                             labels = c("No Payment", "Minimum Payment", "Low", "Medium", "High"), 
                             include.lowest = TRUE)


# Check the new column
head(counties_payment_map$payment_bands)

# Plot the map using the discrete payment band
ggplot(data = counties_payment_map) +
  geom_sf(aes(fill = payment_bands)) +
  labs(title = "Map from Shapefile with Payment Bands") +
  scale_fill_manual(values = c("No Payment" = "grey", "Minimum Payment" = "yellow", "Low" = "palegreen", "Medium" = "olivedrab", "High" = "seagreen4")) + 
  theme_minimal()
