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
  
  # Identify common columns (excluding 'MatingNumber')
  common_cols <- intersect(names(DAMSIRE2), names(IND2))
  common_cols <- setdiff(common_cols, "MatingNumber")
  
  # Remove duplicate columns from the second data frame (assuming df_pero)
  DAMSIRE2 <- DAMSIRE2 %>% select(-all_of(common_cols))
  
  
  merged_df = merge(DAMSIRE2, IND2, by = 'MatingNumber')  %>%
    mutate(Birthday = as.Date(Birthday, format = "%Y-%m-%d"), 
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
  
  # Function to filter dataset based on Dam and Sire birth months, then count and pivot
  prepare_dataset <- function(data, dam_months, sire_months, min_year, max_year) {
    
    data2 = data %>%
      filter(BirthMonth_Dam %in% dam_months & BirthMonth_Sire %in% sire_months) %>%
      group_by(BirthYear, BirthMonth, Birthday, MatingNumber) %>%
      summarise(Count = n(), .groups = 'drop')
    
    data2 %>%
      group_by(BirthYear, BirthMonth) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = BirthYear, values_from = Count, 
                  values_fill = list(Count = 0), names_prefix = "Y") %>%
      
      complete(BirthMonth = 1:12, fill = list(Count = 0)) %>%
      # Add missing years as columns with 0
      pivot_longer(cols = starts_with("Y"), names_to = "Year", values_to = "Count") %>%
      mutate(Year = as.numeric(sub("Y", "", Year))) %>% 
      filter(Year >= min_year & Year <= max_year) %>%
      pivot_wider(names_from = Year, values_from = Count, values_fill = list(Count = 0))
  }
  
  # Define winter and summer months
  winter_months <- c(10, 11, 12, 1, 2, 3)
  summer_months <- setdiff(1:12, winter_months)
  
  # Calculate the overall min and max BirthYear
  
  min_year <- ifelse(species == "SM2", 2002,
                     ifelse(species == "LL", 1988, 
                     min(merged_df2$BirthYear, na.rm = TRUE) +1))
  

  max_year <- ifelse(species == "SM2", 2016, 2024)
                     
  
  Dam_winter_Sire_winter <- prepare_dataset(merged_df2, winter_months, winter_months, min_year, max_year)
  Dam_summer_Sire_summer <- prepare_dataset(merged_df2, summer_months, summer_months, min_year, max_year)
  Dam_winter_Sire_summer <- prepare_dataset(merged_df2, winter_months, summer_months, min_year, max_year)
  Dam_summer_Sire_winter <- prepare_dataset(merged_df2, summer_months, winter_months, min_year, max_year)
  
  
  
  
  wb <- createWorkbook()
  
  
  addWorksheet(wb, "Dam_winter_Sire_winter")
  writeData(wb, "Dam_winter_Sire_winter", Dam_winter_Sire_winter)  
  
  addWorksheet(wb, "Dam_summer_Sire_summer")
  writeData(wb, "Dam_summer_Sire_summer", Dam_summer_Sire_summer) 
  
  addWorksheet(wb, "Dam_winter_Sire_summer")
  writeData(wb, "Dam_winter_Sire_summer", Dam_winter_Sire_summer) 
  
  addWorksheet(wb, "Dam_summer_Sire_winter")
  writeData(wb, "Dam_summer_Sire_winter", Dam_summer_Sire_winter) 
  
  # Save the workbook with a species-specific filename
  saveWorkbook(wb, paste0("Dam_Sire_Combinations_", species, ".xlsx"), overwrite = TRUE)
  
  
  #############################
  calculate_seasonal_births_adjusted <- function(data, winter_months, summer_months, year_interval) {
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
  
 
  combinations <- c("Dam_winter_Sire_winter", "Dam_summer_Sire_summer", "Dam_winter_Sire_summer", "Dam_summer_Sire_winter")
  
  
  process_and_export <- function(year_interval) {
    
    wb <- createWorkbook()
    
    for (combination in combinations) {
      
      data_table <- get(combination)
      
      # Calculate seasonal births
      result_counts <- calculate_seasonal_births_adjusted(data_table, winter_months, summer_months, year_interval)
      
      # Remove intervals exceeding 2024
      result_counts <- result_counts %>%
        mutate(YearGroupEnd = as.numeric(str_extract(YearGroup, "\\d+$"))) %>%
        filter(YearGroupEnd <= 2024) %>%
        select(-YearGroupEnd)
      
      # Skip exporting if empty
      if (nrow(result_counts) == 0) {
        next
      }
      
      # Pivot result_counts for Excel export
      result_counts_pivoted <- result_counts %>%
        pivot_wider(names_from = YearGroup, values_from = Count, values_fill = list(Count = 0))
      
      # Add a worksheet to the workbook
      addWorksheet(wb, combination)
      writeData(wb, combination, result_counts_pivoted)
    }
    
    # Save workbook only if it contains data
    if (length(sheets(wb)) > 0) {
      file_name <- paste("Seasonal_Births_", species, "_", year_interval, "yr.xlsx")
      saveWorkbook(wb, file_name, overwrite = TRUE)
    }
  }
  
  
  # year_intervals <- c(3,4,5)
  # 
  # for (interval in year_intervals) {
  #   process_and_export( interval)
  # }
  
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
  process_and_export(year_interval)
  
}



