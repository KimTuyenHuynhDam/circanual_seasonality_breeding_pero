library(openxlsx)
library(readxl)
library(tidyverse)
library(broom)
library(lubridate)
library(writexl)

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

# Create lists to store aggregated data
dam_birth_list <- list()
sire_birth_list <- list()
dam_repro_list <- list()
sire_repro_list <- list()
dam_repro_by_birthmonth_list <- list()
sire_repro_by_birthmonth_list <- list()

for ( species in all_stock) {
  
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
    mutate(Birthday = as.Date(Birthday, format = "%m/%d/%Y"),
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
    left_join(sire_info, by = c("Sire" = "ID")) %>%
    filter(!is.na(Birthday), !is.na(Birthday_Sire), !is.na(Birthday_Dam))
  
  count_reproducing_parents_by_birthmonth <- function(data, parent_type, species) {
    parent_id_col <- ifelse(parent_type == "Dam", "Dam", "Sire")
    birth_month_col <- paste0("BirthMonth_", parent_type)
    birth_year_col <- paste0("BirthYear_", parent_type)
    
    min_year <- min(data[[birth_year_col]])
    max_year <- max(data[[birth_year_col]])
    
    seq_interval_start <- seq(min_year, max_year, by = 5)
    seq_interval_start <- seq_interval_start[seq_interval_start + 4 <= 2024]
    
    results <- list()
    
    for (interval_start in seq_interval_start) {
      interval_end <- interval_start + 4  
      
      summarized <- data %>%
        filter(between(BirthYear, interval_start, interval_end)) %>%
        distinct(.data[[parent_id_col]], .data[[birth_month_col]]) %>%
        group_by(.data[[birth_month_col]]) %>%
        summarise(total_parents = n(), .groups = 'drop') %>%
        pivot_wider(names_from = birth_month_col, values_from = total_parents, names_sort = TRUE) %>%
        mutate(Species = species, Year_Period = paste(interval_start, interval_end, sep = "-"))
      
      results[[paste(interval_start, interval_end, sep = "-")]] <- summarized
    }
    
    final_result <- bind_rows(results)
    
    return(final_result)
  }
  
  parents = c('Sire','Dam')
  
  for (parent in parents) {
    repro_by_birthmonth = count_reproducing_parents_by_birthmonth(merged_df2, parent, species)
    
    if (parent == "Dam") {
      dam_repro_by_birthmonth_list[[species]] <- repro_by_birthmonth
    } else {
      sire_repro_by_birthmonth_list[[species]] <- repro_by_birthmonth
    }
  }
}

# Combine data for parent reproduction counts by birth month
dam_repro_by_birthmonth_combined <- bind_rows(dam_repro_by_birthmonth_list)
sire_repro_by_birthmonth_combined <- bind_rows(sire_repro_by_birthmonth_list)

# Save parent reproduction counts by birth month
repro_wb <- createWorkbook()
addWorksheet(repro_wb, "Dam_Reproduction_By_BirthMonth")
writeData(repro_wb, "Dam_Reproduction_By_BirthMonth", dam_repro_by_birthmonth_combined)
addWorksheet(repro_wb, "Sire_Reproduction_By_BirthMonth")
writeData(repro_wb, "Sire_Reproduction_By_BirthMonth", sire_repro_by_birthmonth_combined)
saveWorkbook(repro_wb, "Parent_Reproduction_By_BirthMonth.xlsx", overwrite = TRUE)
