# Load packages
library(dplyr)
library(RSQLite)

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
  return(data_not_historical)
}


#Problem 2

# Filtered out extinct vs. extant mammals
extinct_mammals <- filter(MammalData, status == "extinct")
extant_mammals <- filter(MammalData, status == "extant")

# Calculates average of weight of mammals
mammal_weights <- function(df){
    summarize(df,average_weight = mean(combined_mass, na.rm = "TRUE"))
}


MammalData <- Mammal_data()
mammal_weights(extinct_mammals)
mammal_weights(extant_mammals)

