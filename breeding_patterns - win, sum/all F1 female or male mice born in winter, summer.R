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
  
  
  # Filter out any rows where BirthYear is NA or not a finite number
  merged_df2 <- merged_df2 %>% filter(!is.na(BirthYear) & is.finite(BirthYear))
  
 
  # Normalize 'Sex' values to "F" or "M" and filter out other values
  merged_df2 <- merged_df2 %>%
    mutate(Sex = case_when(
      tolower(Sex) %in% c("f", "female", "females") ~ "F",
      tolower(Sex) %in% c("m", "male", "males") ~ "M",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Sex), !is.na(BirthMonth)) # Ensure BirthMonth is not NA as well
  
  # Define winter and summer months
  winter_months <- c(10, 11, 12, 1, 2, 3)
  summer_months <- setdiff(1:12, winter_months)
  
  
  calculate_year_group <- function(year, interval, min_year, max_year) {
    start_year <- ((year - min_year) %/% interval) * interval + min_year
    end_year <- start_year + interval - 1
    
    
    year_group <- ifelse(end_year <= max_year, paste(start_year, end_year, sep = "-"), "Out of Range")
    
    return(year_group)
  }
  
  calculate_counts <- function(data, interval, season = "all", max_year) {
    
    
    # Apply season filter based on 'season' argument
    data_filtered <- data %>%
      filter(
        if (season == "winter") BirthMonth %in% winter_months else
          if (season == "summer") BirthMonth %in% summer_months else TRUE
      ) %>%
      mutate(
        YearGroup = calculate_year_group(BirthYear, interval, min_year, max_year)
      )
    
    # Filter out "Out of Range" groups before summarizing
    data_filtered %>%
      filter(YearGroup != "Out of Range") %>%
      group_by(YearGroup, Sex) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Sex, values_from = Count, values_fill = list(Count = 0))
  }
  
  # Prepare a new workbook
  wb <- createWorkbook()
  
  year_intervals <- c(3)
  min_year <- ifelse(species == "SM2", min(merged_df2$BirthYear, na.rm = TRUE) + 2, min(merged_df2$BirthYear, na.rm = TRUE))
  #max_year <- max(merged_df2$BirthYear, na.rm = TRUE)  
  max_year <- 2024
  for (interval in year_intervals) {
    for (season in c("winter", "summer", "all")) {
      counts <- calculate_counts(merged_df2, interval, season, max_year)
      sheet_name <- paste(season, "Births", interval, "yr Interval")
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, counts)
    }
  }

  # Save the workbook
  saveWorkbook(wb, paste(species, "- Mice_Births_By_Sex_and_Season.xlsx"), overwrite = TRUE)
  
  
}

