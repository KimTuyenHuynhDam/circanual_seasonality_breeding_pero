library(openxlsx)
library(readxl)
library(dplyr)
library(broom)
library(tidyverse)
library(lubridate)

# Function to replace spaces with underscores in column names
clean_column_names <- function(df) {
  names(df) <- gsub(" ", "", names(df))
  return(df)
}

pero = read_excel("Peromyscus.xlsx") %>% select(1:5)  %>%
  clean_column_names %>% filter(!is.na(Birthday)) 
matingcage  = read_excel("Mating Records.xlsx")  %>% select(1:5) %>%
  clean_column_names

all_stock <- c("BW", "LL", "PO", "IS", "EP", "SM2")

wb <- createWorkbook()
sheet_name <- "Species_Summary"
addWorksheet(wb, sheet_name)

summary_df <- data.frame(Species = character(), 
                         Period = character(), 
                         TotalBirths = integer(), 
                         TotalPairs = integer(), 
                         TotalOffspring = integer())

# Processing for each species
for (species in all_stock) {
  
  IND = pero %>% filter(STOCK == species)
  DAMSIREori = matingcage %>% filter(STOCK == species)
  DAMSIRE <- DAMSIREori %>%
    semi_join(IND, by = "MatingNumber")
  
  
  start_year <- ifelse(species == "SM2", 2002,
                     ifelse(species == "LL", 1988, 
                            min(year(IND$Birthday), na.rm = TRUE)))
  
  end_year <- ifelse(species == "SM2", 2016, 2022)
  
 
  # Calculate total births for the period
  total_births <- IND %>%
    filter(year(Birthday) >= start_year & year(Birthday) <= end_year) %>%
    summarise(TotalBirths = n_distinct(Birthday)) %>%
    pull(TotalBirths)
  
  # Calculate total breeding pairs for the period
  total_pairs <- DAMSIRE %>%
    filter(year(DateofMating) >= start_year & year(DateofMating) <= end_year) %>%
    summarise(TotalPairs = n()) %>%
    pull(TotalPairs)
  
  # Calculate total offspring for the period
  total_offspring <- IND %>%
    filter(year(Birthday) >= start_year & year(Birthday) <= end_year) %>%
    summarise(TotalOffspring = n()) %>%
    pull(TotalOffspring)
  
  period_description <- paste(start_year, "to", end_year)
  
  summary_df <- summary_df %>%
    bind_rows(data.frame(
      Species = species,
      Period = period_description,
      TotalBirths = total_births,
      TotalPairs = total_pairs,
      TotalOffspring = total_offspring
    ))
}

writeData(wb, sheet_name, summary_df)
saveWorkbook(wb, "Total_Species_Data_Summary.xlsx", overwrite = TRUE)
