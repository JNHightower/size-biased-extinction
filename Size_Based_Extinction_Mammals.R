# Load packages
library(dplyr)
library(RSQLite)

# Loads data, adds column headers, and removes historical data from data set
Mammal_data <- function(df){
  # Load data. Tab delimited, no header, -999 = N/A value
  data <- read.csv("MOMv3.3.txt", sep = "\t", head = FALSE, stringsAsFactors = FALSE, na.strings = "-999")
  # Add column headers to data frame
  colnames(data) <- c("continent", "status", "order", 
                      "family", "genus", "species", "log_mass", "combined_mass", 
                      "reference")
  # Remove historical data from data set
  data_not_historical <- subset(data, status != "historical")
  return(data_not_historical)
}