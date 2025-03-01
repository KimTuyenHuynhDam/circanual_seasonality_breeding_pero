library(openxlsx)
library(readxl)
library(tidyverse)
library(broom)
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


all_stock= c("BW", "LL", "PO", "IS", "EP", "SM2")






for ( species in all_stock) {
  
  
  ########################################################
  
  IND = pero %>% filter(STOCK == species)
  
  
  DAMSIRE = matingcage %>% filter(STOCK == species)
  
  
  IND2 = IND %>%
    mutate(MatingNumber = str_remove(MatingNumber, species)) %>%
    mutate(ID = str_remove(ID, species)) %>% 
    
    mutate(MatingNumber = str_replace_all(MatingNumber, "[^[:alnum:]]", "")) %>%
    
    mutate(ID = str_replace_all(ID, "[^[:alnum:]]", "")) 
  
  
  
  DAMSIRE2 = DAMSIRE %>%
    
    mutate(Dam = str_remove(Dam, species)) %>%
    mutate(Sire = str_remove(Sire, species)) %>%
    mutate(MatingNumber = str_remove(MatingNumber, species)) %>%
    mutate(Dam = str_replace_all(Dam, "[^[:alnum:]]", "")) %>%
    mutate(Sire = str_replace_all(Sire, "[^[:alnum:]]", "")) %>%
    mutate(MatingNumber = str_replace_all(MatingNumber, "[^[:alnum:]]", "")) 
  
 
  common_cols <- intersect(names(DAMSIRE2), names(IND2))
  common_cols <- setdiff(common_cols, "MatingNumber")
  

  DAMSIRE2 <- DAMSIRE2 %>% select(-all_of(common_cols))
  
  
  merged_df = merge(DAMSIRE2, IND2, by = 'MatingNumber')  %>%
    mutate(Birthday = as.Date(Birthday, format = "%Y-%m-%d"), # Adjust format as needed
           BirthMonth = month(Birthday),
           BirthYear = year(Birthday))
  
  
  dam_info <- merged_df %>%
    select(ID, Birthday, BirthMonth, BirthYear) %>%
    rename(Birthday_Dam = Birthday, BirthMonth_Dam = BirthMonth, BirthYear_Dam = BirthYear)
  
  
  sire_info <- merged_df %>%
    select(ID, Birthday, BirthMonth, BirthYear) %>%
    rename(Birthday_Sire = Birthday, BirthMonth_Sire = BirthMonth, BirthYear_Sire = BirthYear)
  
  
  merged_df2 <- merged_df %>%  
    left_join(dam_info, by = c("Dam" = "ID")) %>%
    left_join(sire_info, by = c("Sire" = "ID")) 
  
  
  #############
  
  prepare_dataset <- function(data, min_year, max_year) {
   
    summarized_data <- data %>%
      filter(BirthYear >= min_year & BirthYear <= max_year) %>%   
      group_by(BirthYear, BirthMonth) %>%
      summarise(Count = n(), .groups = 'drop')
    
    all_years <- paste0("Y", seq(min_year, max_year))
    
    wide_data <- summarized_data %>%
      pivot_wider(names_from = BirthYear, values_from = Count, 
                  values_fill = list(Count = 0), names_prefix = "Y") %>%
      complete(BirthMonth = 1:12, fill = list(Count = 0))
    
    missing_years <- setdiff(all_years, names(wide_data))
    for (year in missing_years) {
      wide_data[[year]] <- 0
    }
    
    wide_data <- wide_data %>%
      select(BirthMonth, sort(names(wide_data)[-1]))
    
    return(wide_data)
  }
  
  
  
  min_year <- ifelse(species == "SM2", 2002,
                     ifelse(species == "LL", 1988, 
                            min(merged_df2$BirthYear, na.rm = TRUE) +1))
  
  
  max_year <- ifelse(species == "SM2", 2016, 2024)
  
  
  all_mice <- prepare_dataset(merged_df2, min_year, max_year)
  
  wb <- createWorkbook()
  
  
  addWorksheet(wb, "All Mice")
  
  
  writeData(wb, "All Mice", all_mice)
  
  
  file_name <- paste0("all_F1_mice born from ", min_year, " to ", max_year, " - ", species, ".xlsx")
  
 
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  winter_months <- c(10, 11, 12, 1, 2, 3)
  summer_months <- setdiff(1:12, winter_months)
  
  
  
  #####################
  calculate_seasonal_births <- function(data, winter_months, summer_months, year_interval) {
    data_long <- data %>%
      pivot_longer(cols = -BirthMonth, names_to = "Year", values_to = "Count", 
                   names_prefix = "Y", names_transform = list(Year = as.integer)) %>%
      filter(Count > 0) 
    
    year_intervals <- seq(from = min_year, to = max_year + year_interval, by = year_interval)
    
    
    interval_labels <- sapply(1:(length(year_intervals)-1), function(i) {
      paste(year_intervals[i], year_intervals[i+1]-1, sep = "-")
    })
    
    data_long <- data_long %>%
      mutate(
        YearGroup = cut(Year, breaks = year_intervals, include.lowest = TRUE, labels = interval_labels),
        BirthSeason = case_when(
          BirthMonth %in% winter_months ~ "Winter",
          BirthMonth %in% summer_months ~ "Summer",
          TRUE ~ "Other"
        )
      )
    
    
    counts <- data_long %>%
      group_by(YearGroup, BirthSeason) %>%
      summarise(Count = sum(Count), .groups = 'drop') %>%
      filter(BirthSeason %in% c("Winter", "Summer")) 
    return(counts)
  }
  ###########################
  process_and_export <- function(data, winter_months, summer_months, year_intervals, min_year, max_year) {
    wb <- createWorkbook()
    
    for (year_interval in year_intervals) {
      result_counts <- calculate_seasonal_births(data, winter_months, summer_months, year_interval)
      
      # Extract end year from YearGroup and filter out invalid intervals
      result_counts <- result_counts %>%
        mutate(YearGroupEnd = as.numeric(str_extract(YearGroup, "\\d+$"))) %>%
        filter(YearGroupEnd <= 2024 & YearGroupEnd >= min_year & YearGroupEnd <= max_year) %>%
        select(-YearGroupEnd)  # Remove temporary column
      
      # Skip exporting if result_counts is empty
      if (nrow(result_counts) == 0) {
        next
      }
      
      # Pivot result_counts for Excel export
      result_counts_pivoted <- result_counts %>%
        pivot_wider(names_from = YearGroup, values_from = Count, values_fill = list(Count = 0))
      
      sheet_name <- paste("Interval", year_interval, "yr")  
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, result_counts_pivoted)
    }
    
    # Save workbook only if it contains data
    if (length(sheets(wb)) > 0) {
      file_name <- paste("Seasonal_Births_", species, "_", paste(year_intervals, collapse = "_"), "yr.xlsx")
      saveWorkbook(wb, file_name, overwrite = TRUE)
    }
  }
  
  
  # Define year_interval based on species
  if (species %in% c("IS", "EP")) {
    year_interval <- 4
  } else if (species == "SM2") {
    year_interval <- 3
  } else if (species %in% c("BW", "LL", "PO")) {
    year_interval <- 5
  } else {
    stop("Unknown species: ", species)  # Safety check
  }
  
  # Call process_and_export() with the correct interval
  process_and_export(all_mice, winter_months, summer_months, year_interval, min_year, max_year)
  
}
