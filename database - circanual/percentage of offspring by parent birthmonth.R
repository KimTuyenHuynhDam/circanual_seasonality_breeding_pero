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
  
  process_parent <- function(data, parent) {
    data %>% 
      group_by(.data[[parent]]) %>%
      nest() %>% 
      mutate(data = map(data, ~ .x %>%
                          mutate(one_year_later = min(Birthday) + years(1)) %>%
                          filter(Birthday < one_year_later))) %>%
      unnest(cols = c(data)) %>%
      select(-one_year_later)
  }
  
  summarize_by_birthmonth_5_year_interval_to_excel <- function(data, parent_type) {
    birth_year_col <- paste0("BirthYear_", parent_type)
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    results <- list()
    
    seq_interval_start <- seq(min(data[[birth_year_col]]), max(data[[birth_year_col]]), by = 5)
    
    # Ensure we do not include intervals beyond 2024
    seq_interval_start <- seq_interval_start[seq_interval_start + 4 <= 2024]
    
    for (interval_start in seq_interval_start) {
      interval_end <- interval_start + 4  
      
      summarized <- data %>%
        filter(between(.data[[birth_year_col]], interval_start, interval_end)) %>%
        group_by(.data[[birth_month_col]], BirthMonth) %>%
        summarise(total_count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE) %>%
        mutate(Year_Period = paste(interval_start, interval_end, sep = "-"))
      
      results[[paste(interval_start, interval_end, sep = "-")]] <- summarized
    }
    
    final_result <- bind_rows(results)
    
    return(final_result)
  }
  
  ensure_continuous_birthmonth <- function(data, parent_type) {
    birth_month_col <- paste0("BirthMonth_", parent_type)
    all_months <- tibble(BirthMonth = 1:12)
    
    completed_data <- lapply(unique(data$Year_Period), function(year_period) {
      period_data <- filter(data, Year_Period == year_period)
      
      period_data <- period_data %>%
        rename(BirthMonth = !!sym(birth_month_col))
      
      period_data_full <- full_join(all_months, period_data, by = "BirthMonth") %>%
        replace_na(list(total_count = 0)) %>%
        mutate(Year_Period = year_period)
      
      period_data_full <- period_data_full %>%
        rename(!!sym(birth_month_col) := BirthMonth)
      
      period_data_full
    })
    
    completed_data <- bind_rows(completed_data)
    
    return(completed_data)
  }
  
  parents = c('Sire','Dam')
  
  for (parent in parents) {
    full_1_year <- process_parent(merged_df2, parent)
    
    summarize_by_birthmonth = summarize_by_birthmonth_5_year_interval_to_excel(full_1_year, parent)
    
    data = ensure_continuous_birthmonth(summarize_by_birthmonth, parent) 
    data2 = data %>%  select(-ncol(.)) 
    
    data_transformed <- data2 %>%
      mutate(RowSum = rowSums(select(., -1), na.rm = TRUE)) %>%
      rowwise() %>%
      mutate(across(-c(1, RowSum), ~ .x / RowSum * 100)) %>%
      ungroup() %>%
      select(-RowSum)
    
    birth_month_col <- paste0("BirthMonth_", parent)
    
    data_matrix <- as.matrix(data_transformed %>% select(-all_of(birth_month_col)))
    
    for (i in 1:nrow(data_matrix)) {
      if(any(data_matrix[i, ] > 20, na.rm = TRUE)) {
        data_matrix[i, ] <- NA
      }
    }
    
    data_adjusted <- as.data.frame(data_matrix)
    data_adjusted <- bind_cols(data_transformed %>% select(all_of(birth_month_col)), data_adjusted)
    
    colnames(data_adjusted)[-1] <- colnames(data_transformed)[-which(names(data_transformed) == birth_month_col)]
    
    data_adjusted2 <- data_adjusted %>% cbind(data %>%  select(ncol(.)) )
    
    write.xlsx(data_adjusted2, paste0('./percentage offspring/',species, '-',parent,'-percentage of offspring by parent birthmonth.xlsx'))
  }
}
