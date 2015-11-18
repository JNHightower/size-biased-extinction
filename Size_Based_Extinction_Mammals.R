# Load packages
library(dplyr)
library(RSQLite)
library(tidyr)
library(ggplot2)

#Problem 1
# Loads data, adds column headers, and removes historical data from data set
Mammal_data <- function(df){
  # Load data. Tab delimited, no header, -999 = N/A value
  data <- read.csv("MOMv3.3.txt", sep = "\t", header = FALSE, stringsAsFactors = FALSE, na.strings = "-999")
  # Add column headers to data frame
  colnames(data) <- c("continent", "status", "order", 
                      "family", "genus", "species", "log_mass", "combined_mass", 
                      "reference")
  # Remove historical data from data set
  data_not_historical <- subset(data, status != "historical")
  relevant_data <- subset(data_not_historical, status != "introduction")
  return(relevant_data)
}

MammalData <- Mammal_data()

#Problem 2

# Separated extinct vs. extant mammals
extinct_mammals <- filter(MammalData, status == "extinct")
extant_mammals <- filter(MammalData, status == "extant")

# Calculates average of weight of mammals
mammal_weights <- function(df){
    summarize(df,average_weight = mean(combined_mass, na.rm = "TRUE"))
}

mammal_weights(extinct_mammals)
mammal_weights(extant_mammals)

# Problem 3

# Function to calculate the average mass of extant and extinct mammals on each continent

Mammal_Weight_Continent <- function(df){
  df%>%
      group_by(continent, status)%>%
      summarize(average_weight = mean(combined_mass, na.rm="TRUE"))%>%
      spread(status, average_weight)%>%
      write.csv("continent_mass_differences.csv")
}
continent_mammals <- group_by(MammalData, continent)
continent_mammals


# Problem 4

Mammal_to_plot <- MammalData %>%
  filter(continent != "EU") %>%
  filter(continent != "Af") %>%
  filter(continent != "Oceanic")

ggplot(Mammal_to_plot, aes(x = log_mass))+
  geom_histogram(binwidth = 0.25)+
  facet_grid(continent~status)+
  ylab("No. Species")+
  xlab("log(mass)")
  


#github repository url final
# https://github.com/JNHightower/size-biased-extinction.git
