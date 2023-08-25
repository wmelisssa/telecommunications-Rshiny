#libraries
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(tidyr)
library(DT)
library(ggplot2)

#constants
id.name <- "id"
age.name <- "Age"
gender.name <- "Gender"
income.name <- "Income"
region.name <- "Region"
persona.name <- "Persona"

user.friendly.name <- "BP_User_Friendly_0_10"
fast.name <- "BP_Fast_0_10"
battery.life.name <- "BP_Battery_Life_0_10"
camera.name <- "BP_Camera_0_10"
sleek.name <- "BP_Sleek_0_10"
stylish.name <- "BP_Stylish_0_10"
status.symbol.name <- "BP_Status_Symbol_0_10"
good.screen.size.name <- "BP_Good_Screen_Size_0_10"
boring.name <- "BP_Boring_0_10"
bulky.name <- "BP_Bulky_0_10"
fragile.name <- "BP_Fragile_0_10"
expensive.name <- "BP_Expensive_0_10"

product.name <- "Product"

# state of engagement
awareness.name <- "Awareness"
consideration.name <- "Consideration"
consumption.name <- "Consumption"
satisfaction.name <- "Satisfaction"
advocacy.name <- "Advocacy"

bp.pattern <- "BP_"


age.group.name <- "Age Group"
income.group.name <- "Income Group"

value.max.satisfaction <- 10

## define
age_groups <-  c(17, 34, 49, 64, 75)
age_labels <- c('18-34', '35-49', '50-64', '65+')

income_groups <- c(-1, 49999, 69999, 99999, 149999, Inf)
income_labels <- c('Under 50,000', '50,000 - Under 75,000', '75,000 - Under 100,000', '100,000 - Under 150,000', '150,000+')
gender_labels <- c("Male", "Female")
region_labels <- c("South", "Midwest", "West", "Northeast")
persona_labels <- c("Precociously Preoccupied", "Technological Triumphalist", "Ambivalent Adventurer", "Outdoorsy Ombudsman",  "Consistent Compromiser", "Materialistic Meditator")

