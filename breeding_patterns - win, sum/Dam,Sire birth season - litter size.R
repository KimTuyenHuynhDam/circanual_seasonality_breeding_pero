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
  
  
  merged_df2 <- merged_df %>%  filter( BirthYear <= 2024) %>%
    left_join(dam_info, by = c("Dam" = "ID")) %>%
    left_join(sire_info, by = c("Sire" = "ID")) 
  
 
  min_year <- ifelse(species == "SM2", 2002,
                     ifelse(species == "LL", 1988, 
                            min(merged_df2$BirthYear, na.rm = TRUE) + 1))
  
  max_year <- ifelse(species == "SM2", 2016, 2024)
  
  merged_df2 <- merged_df2 %>%
    filter(BirthYear >= min_year & BirthYear <= max_year)  
  
  #############
  

  
  # Define winter and summer months
  winter_months <- c(10, 11, 12, 1, 2, 3)
  summer_months <- setdiff(1:12, winter_months)
  
  # Calculate litter sizes for different scenarios
  calculate_litter_sizes <- function(data, dam_months, sire_months,offspring_months ) {
    data %>%
      filter(BirthMonth_Dam %in% dam_months, BirthMonth_Sire %in% sire_months) %>%
      group_by(BirthYear, BirthMonth, Birthday, MatingNumber) %>%
      summarise(litter_size = n(), .groups = 'drop') %>%
      filter(BirthMonth %in% offspring_months) %>%
      ungroup()
  }
  
  # Dam in winter, Sire in winter
  Dam_win_Sire_win_F1_win <- calculate_litter_sizes(merged_df2, winter_months, winter_months, winter_months)
  Dam_win_Sire_win_F1_sum <- calculate_litter_sizes(merged_df2, winter_months, winter_months, summer_months)
  
  # Dam in summer, Sire in summer
  Dam_sum_Sire_sum_F1_win <- calculate_litter_sizes(merged_df2, summer_months, summer_months, winter_months)
  Dam_sum_Sire_sum_F1_sum <- calculate_litter_sizes(merged_df2, summer_months, summer_months, summer_months)
  
  # Dam in winter, Sire in summer
  Dam_win_Sire_sum_F1_win <- calculate_litter_sizes(merged_df2, winter_months, summer_months, winter_months)
  Dam_win_Sire_sum_F1_sum <- calculate_litter_sizes(merged_df2, winter_months, summer_months, summer_months)
  
  # Dam in summer, Sire in winter
  Dam_sum_Sire_win_F1_win <- calculate_litter_sizes(merged_df2, summer_months, winter_months, winter_months)
  Dam_sum_Sire_win_F1_sum <- calculate_litter_sizes(merged_df2, summer_months, winter_months, summer_months)
  
  # Export to Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Dam_win_Sire_win_F1_win")
  writeData(wb, "Dam_win_Sire_win_F1_win", Dam_win_Sire_win_F1_win)
  addWorksheet(wb, "Dam_win_Sire_win_F1_sum")
  writeData(wb, "Dam_win_Sire_win_F1_sum", Dam_win_Sire_win_F1_sum)
  
  addWorksheet(wb, "Dam_sum_Sire_sum_F1_win")
  writeData(wb, "Dam_sum_Sire_sum_F1_win", Dam_sum_Sire_sum_F1_win)
  addWorksheet(wb, "Dam_sum_Sire_sum_F1_sum")
  writeData(wb, "Dam_sum_Sire_sum_F1_sum", Dam_sum_Sire_sum_F1_sum)
  
  
  addWorksheet(wb, "Dam_win_Sire_sum_F1_win")
  writeData(wb, "Dam_win_Sire_sum_F1_win", Dam_win_Sire_sum_F1_win)
  addWorksheet(wb, "Dam_win_Sire_sum_F1_sum")
  writeData(wb, "Dam_win_Sire_sum_F1_sum", Dam_win_Sire_sum_F1_sum)
  
  
  addWorksheet(wb, "Dam_sum_Sire_win_F1_win")
  writeData(wb, "Dam_sum_Sire_win_F1_win", Dam_sum_Sire_win_F1_win)
  addWorksheet(wb, "Dam_sum_Sire_win_F1_sum")
  writeData(wb, "Dam_sum_Sire_win_F1_sum", Dam_sum_Sire_win_F1_sum)
  
  saveWorkbook(wb, paste(species,  "- Litter_Sizes_born_sum_win_By_parent_Birth_Month.xlsx"), overwrite = TRUE)
  
 
}



