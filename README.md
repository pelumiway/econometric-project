# econometric-project
I did a project  on From Cradles to Growth: The Intersection of Fertility Rates and Economic Development

This study explores the intricate relationship between fertility rates and economic development across various countries.
Utilizing international data, it identifies a positive correlation between declining fertility rates and advancements in economic development. 
The analysis reveals that socio-economic factors, such as increased education levels and healthcare access, significantly influence this relationship. 

Additionally, demographic shifts, particularly in urbanization rates, emerge as crucial mediators. 
These findings underscore the complex dynamics between population changes and economic growth, highlighting the need for policies that address
the evolving demographic landscape to foster sustainable development.



install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")
install.packages("maps")

library(ggplot2)
library(maps)

library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(stargazer)
library(psych)
getwd()
setwd("C:/Users//rebec/Downloads")
data1<- read_excel("C:/Users/rebec/Downloads/Births_by_Single_Age_Mother.xlsx")
head(data1)
data2 <- read_excel("C:/Users/rebec/Downloads/Age-specific_Fertility_Rates_(ASFR)_by_Single_Age.xlsx")
head(data2)
data3 <- read_excel("C:/Users/rebec/Downloads/Age-specific_Fertility_Rates_(ASFR)_by_Single_Age (2).xlsx")
head(data3)
data4 <- read_excel("C:/Users/rebec/Downloads/Births_by_Five-year_Age_Groups_of_Mother.xlsx" )
head(data4)
data5 <- read_excel("C:/Users/rebec/Downloads/Fertility_rate-total_(births-per-woman).xls" )
head(data5)
combined_data <- bind_rows(data1, data2, data3, data4, data5)
# Assuming your data frame is named combined_data
# Remove the first 11 rows
combined_data <- combined_data[-(1:11), ]

# View the combined dataset
head(combined_data)
# 1. Handling Missing Values
# Option 1: Remove rows with any NA values

cleaned_data <- na.omit(combined_data)

# Option 2: Replace NA with a specific value (e.g., the mean or median)
# For numerical columns
cleaned_data <- data %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .))



# 2. Removing Duplicate Rows
cleaned_data <- cleaned_data %>%
  distinct()

# 3. Correcting Data Types
# Example: Converting a column to a specific data type
# cleaned_data$column <- as.numeric(cleaned_data$column)
# cleaned_data$column <- as.factor(cleaned_data$column)


# This step is highly data-specific. As an example, you might cap values at a certain percentile
# cap_value <- quantile(cleaned_data$numeric_column, 0.99, na.rm = TRUE)
# cleaned_data$numeric_column <- pmin(cleaned_data$numeric_column, cap_value)



# Return the cleaned dataset
clean_data <- function(data) {
  # ... data cleaning steps ...
  return(cleaned_data)
}

# Use the function
result <- clean_data(your_data)


descriptive_stats <- combined_data %>%
  summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE),
                                      median = ~median(., na.rm = TRUE),
                                      sd = ~sd(., na.rm = TRUE),
                                      min = ~min(., na.rm = TRUE),
                                      max = ~max(., na.rm = TRUE))))

stargazer(descriptive_stats, type = "text", title = "Descriptive Statistics Table")

excel_file_path <- "C:/Users/rebec/Downloads/Births_by_Single_Age_Mother.xlsx"
data <- read_excel(excel_file_path)

# Check the class of 'data'
print(class(data))


# Let 'response_var' be your outcome variable and 
# 'predictor_var1', 'predictor_var2', etc., be your predictor variables
formula <- response_var ~ "Year" + "Type" + "Region" + ...

# Fit the unrestricted linear regression model
#unreg <- summary(q)$coefficients
#unreg <- round(data.frame(summary(q)$coefficients, VIF=c(NA,vif(q))), 3)
#write.csv(unreg, file="unreg.csv")
#model <- lm(formula, data = data)

# View the summary of the model
summary(model)
 

data2 <- data.frame(
  country = c("Sub-Saharan Africa", "Northern Africa and Western Asia", "Central and Southern Asia", "Eastern and South-Eastern Asia"),
  value = c(10, 20, 30, 40)  # Adjusted to have 4 elements, same as 'country'
)



# Get world map data
world_map <- map_data("world")
merged_data <- merge(data1, data2, by = "common_column_name")


# Merge your data with the world map data
combined_data <- merge(world_map, combined_data, by.x = "United Nations", by.y = "Year", all.x = TRUE)
head(world_map)
head(combined_data)

names(combined_data)
ggplot(combined_data, aes(x = order, y = Year, group = group)) +
  geom_polygon()

# Merge your data with the world map data
combined_data <- merge(world_map, combined_data, by.x = "region", by.y = "order", all.x = TRUE)

# Create the plot
#ggplot(combined_data, aes(x = long, y = lat, group = group, fill = value)) +
 # geom_polygon() +
  #expand_limits(x = world_map$long, y = world_map$lat) +
  #theme_minimal() +
  #labs(fill = "Value", title = "World Map with My Data")
names(combined_data)


