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
    left_join(sire_info, by = c("Sire" = "ID"))  %>%
    mutate(
      
      BirthYear_Dam = year(Birthday_Dam),
      BirthYear_Sire = year(Birthday_Sire),
      
      
      interval_from_BirthdayDam_to_DeliveryDate = as.integer(difftime(Birthday, Birthday_Dam, units = "days")),
      interval_from_BirthdaySire_to_DeliveryDate = as.integer(difftime(Birthday, Birthday_Sire, units = "days"))
    )
  #############
  
  
  # Define winter and summer months
  winter_months <- c(10, 11, 12, 1, 2, 3)
  summer_months <- setdiff(1:12, winter_months)
  
 
  calculate_min_interval <- function(data, birth_months, parent_type = "Dam", max_year) {
    filter_column_birth_month <- ifelse(parent_type == "Dam", "BirthMonth_Dam", "BirthMonth_Sire")
    filter_column_birth_year <- ifelse(parent_type == "Dam", "BirthYear_Dam", "BirthYear_Sire")
    interval_column <- ifelse(parent_type == "Dam", "interval_from_BirthdayDam_to_DeliveryDate", "interval_from_BirthdaySire_to_DeliveryDate")
    
    data %>%
      filter(!!sym(filter_column_birth_month) %in% birth_months,
             !!sym(filter_column_birth_year) <= max_year) %>%
      group_by(Dam, BirthYear_Dam, BirthMonth_Dam) %>%
      summarise(MinInterval = min(!!sym(interval_column), na.rm = TRUE), .groups = 'drop') %>%
      filter(MinInterval >0 & MinInterval < 1096 ) ## filter problematic data, such as negative days, or days more than 3 years (1095 days)
  }
  

  max_year <- 2024  
  
 
  dam_winter <- calculate_min_interval(merged_df2, winter_months, "Dam", max_year)
  sire_winter <- calculate_min_interval(merged_df2, winter_months, "Sire", max_year)
  dam_summer <- calculate_min_interval(merged_df2, summer_months, "Dam", max_year)
  sire_summer <- calculate_min_interval(merged_df2, summer_months, "Sire", max_year)
  
 
  wb <- createWorkbook()
  addWorksheet(wb, "Dam Winter")
  writeData(wb, "Dam Winter", dam_winter)
  addWorksheet(wb, "Sire Winter")
  writeData(wb, "Sire Winter", sire_winter)
  addWorksheet(wb, "Dam Summer")
  writeData(wb, "Dam Summer", dam_summer)
  addWorksheet(wb, "Sire Summer")
  writeData(wb, "Sire Summer", sire_summer)
  saveWorkbook(wb, paste0(species, "- MinIntervals_ByParentBirthSeason_and_MaxYear.xlsx"), overwrite = TRUE)
  
 
  
}



