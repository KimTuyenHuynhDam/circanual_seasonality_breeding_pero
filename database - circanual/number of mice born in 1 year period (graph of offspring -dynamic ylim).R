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
    mutate(Birthday = as.Date(Birthday, format = "%m/%d/%Y"), # Adjust format as needed
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
  
  #############
  # Function to process each sire or dam
  process_parent <- function(data, parent) {
    data %>% 
      group_by(.data[[parent]]) %>% # Correctly refer to dynamic column names
      nest() %>% 
      mutate(data = map(data, ~ .x %>%
                          mutate(one_year_later = min(Birthday) + years(1)) %>%
                          filter(Birthday < one_year_later))) %>%
      unnest(cols = c(data)) %>%
      select(-one_year_later)
  }
  
  
  ##########################
  # Function to summarize data by birth month over 5-year intervals and write to Excel
  
  summarize_by_birthmonth_5_year_interval_to_excel <- function(data, parent_type) {
    birth_year_col <- paste0("BirthYear_", parent_type)
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    # Prepare an empty list to store results
    results <- list()
    
    # Calculate intervals based on the range of years in the dataset, divided into 5-year segments
    seq_interval_start <- seq(min(data[[birth_year_col]]), max(data[[birth_year_col]]), by = 5)
    
    # Determine the maximum year in the dataset for comparison
    max_year <- max(data[[birth_year_col]])
    
    # Iterate over each interval
    for (interval_start in seq_interval_start) {
      interval_end <- interval_start + 4  # Define the end of the interval
      
      # Filter and summarize data within the current interval
      summarized <- data %>%
        filter(between(.data[[birth_year_col]], interval_start, interval_end)) %>%
        group_by(.data[[birth_month_col]], BirthMonth) %>%
        summarise(total_count = n(), .groups = 'drop') %>%
        pivot_wider(names_from = BirthMonth, values_from = total_count, names_sort = TRUE) %>%
        #mutate(Year_Period = paste(interval_start, interval_end, sep = "-"))
        mutate(Year_Period = if_else(interval_end > max_year, "Out_of_Range", paste(interval_start, interval_end, sep = "-")))
      # Append summarized data to the results list
      results[[paste(interval_start, interval_end, sep = "-")]] <- summarized
    }
    
    # Combine all summarized data frames into one
    final_result <- bind_rows(results)
    
    # Remove "Out_of_Range" groups before writing to an Excel file
    final_result <- filter(final_result, Year_Period != "Out_of_Range")
    # Write to an Excel file
    #write_xlsx(final_result, output_file)
  }
  
  
  
  
  
  ensure_continuous_birthmonth <- function(data, parent_type) {
    birth_month_col <- paste0("BirthMonth_", parent_type)
    
    all_months <- tibble(BirthMonth = 1:12)
    
    completed_data <- lapply(unique(data$Year_Period), function(year_period) {
      period_data <- filter(data, Year_Period == year_period)
      
      # Use direct column name for renaming if necessary
      period_data <- period_data %>%
        rename(BirthMonth = !!sym(birth_month_col))
      
      # Ensure all months are present
      period_data_full <- full_join(all_months, period_data, by = "BirthMonth") %>%
        replace_na(list(total_count = 0)) %>%
        mutate(Year_Period = year_period)
      
      # Optionally, rename the BirthMonth column back to its original dynamic name
      period_data_full <- period_data_full %>%
        rename(!!sym(birth_month_col) := BirthMonth)
      
      period_data_full
    })
    
    completed_data <- bind_rows(completed_data)
    
    #write_xlsx(completed_data, output_file_path)
  }
  parent = 'Dam'
  
  parents = c('Sire','Dam')
  
  for (parent in parents) {
    full_1_year <- process_parent(merged_df2, parent)
    
    #write.xlsx(full_1_year, paste0(species,'-',parent,'- all mice born for 1 year from 1st delivery.xlsx'))
    
    summarize_by_birthmonth = summarize_by_birthmonth_5_year_interval_to_excel(full_1_year, parent)
    
    #output_file_path = paste0(species,"-",parent,"_by_birthmonth_5_year_interval.xlsx")
    
    data = ensure_continuous_birthmonth(summarize_by_birthmonth, parent) 
    data2 = data %>%  select(-ncol(.)) 
    # Transform the data to ratios and multiply by 100, and add the Month column
    data_transformed <- data2 %>%
      # Calculate the row sums and create a new column for it
      mutate(RowSum = rowSums(select(., -1), na.rm = TRUE)) %>%
      # Then calculate the ratios for each row
      rowwise() %>%
      mutate(across(-c(1, RowSum), ~ .x / RowSum * 100)) %>%
      ungroup() %>%
      
      # Remove the RowSum column as it's no longer needed
      select(-RowSum)
    
    birth_month_col <- paste0("BirthMonth_", parent)
    
    ################
    
    # Convert to a regular matrix for row-wise operation, excluding the month column
    data_matrix <- as.matrix(data_transformed %>% select(-all_of(birth_month_col)))
    
    # Apply the condition row-wise
   # for (i in 1:nrow(data_matrix)) {
      # Check if any value in the row exceeds 60
    #  if(any(data_matrix[i, ] > 100, na.rm = TRUE)) {
        # Replace entire row with NA
    #    data_matrix[i, ] <- NA
    #  }
   # }
    for (i in 1:nrow(data_matrix)) {
      # Check if the number of NA values in the row exceeds 10
      if(sum(is.na(data_matrix[i, ])) > 9) {
        # Replace entire row with NA
        data_matrix[i, ] <- NA
      }
    }
    
    # Convert back to a DataFrame
    data_adjusted <- as.data.frame(data_matrix)
    # Add the month column back
    data_adjusted <- bind_cols(data_transformed %>% select(all_of(birth_month_col)), data_adjusted)  %>%
      mutate(across(everything(), ~replace(., is.nan(.), NA)))
    
    # Update column names since they were lost in the conversion process
    colnames(data_adjusted)[-1] <- colnames(data_transformed)[-which(names(data_transformed) == birth_month_col)]
    
    data_adjusted2 <- data_adjusted %>% cbind(data %>%  select(ncol(.)) )
    
    write_xlsx(data_adjusted2, paste0('./percentage offspring/',species, '-',parent,'-percentage of offspring by parent birthmonth.xlsx'))
    
    # Step 2 and Step 3: Combine Rows Based on Month and Calculate Averages and Standard Deviations
   
    data_grouped <- data_adjusted %>%
      group_by(!!sym(birth_month_col)) %>%
      summarise(
        across(
          everything(),
          list(
            mean = ~mean(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE),
            n = ~sum(!is.na(.x))  # Count non-NA values for each group
          )
        ),
        .groups = 'drop'
      ) %>%
      ungroup() %>%
      # Calculate SEM for each group based on SD and N
      mutate(across(contains("_sd"), 
                    ~.x / sqrt(get(str_replace(cur_column(), "_sd", "_n"))), 
                    .names = "{.col}_sem"))%>%
      # Dropping the now unnecessary count columns
      select(-contains("_n"))
    
    
    # Capture the name of the first column to exclude it from renaming operations
    first_col_name <- names(data_grouped)[1]
    
    data_grouped <- data_grouped %>%
      rename_with(~str_replace_all(.x, "_sd_sem$", "_sem")) %>%
      # Ensure all columns now start with "X", taking care to not double-prefix already prefixed columns
      # Skipping the first column for this operation as well
      rename_with(
        ~case_when(
          .x == first_col_name ~ .x,  # Keep the first column name as is
          str_starts(.x, "X") ~ .x,  # Keep columns already starting with "X" as is
          TRUE ~ paste0("X", .x)  # Add "X" prefix to other columns
        ),
        .cols = -all_of(first_col_name)  # Apply to all columns except the first
      ) 
 
    
    
    
    
    
    data_mean = data_grouped %>% select(contains('mean')) %>% t() %>%
      as.data.frame() 
      
    
    data_sem = data_grouped %>% select(contains('sem')) %>% t() %>%
      as.data.frame() 

    ###graphs with SEM as error bar
   for (i in 1:12) {
      # Get the full month name
      full_month_name <- month.name[i]
      mean = data_mean[i] 
      colnames(mean) = 'mean'
        
      sem = data_sem[i] 
      colnames(sem) = 'sem'
      
      data = mean %>% cbind(sem) %>% cbind('BirthMonth'= 1:12)
      # Calculate the max ylim dynamically based on error bar heights
      max_y_with_error <- max(data$mean + data$sem, na.rm = TRUE)
      
      # Set dynamic upper limit for ylim, rounded to the nearest multiple of 5
  
      upper_ylim <- ceiling(max(max_y_with_error, 20) / 5) * 5
      
      # Create the plot with dynamic y values based on the month
      p <- ggplot(data, aes(x = BirthMonth, y = mean)) + 
        geom_point(color = "blue", size = 4) +
        geom_errorbar(color = "blue",
                      aes(
                        ymin = mean - sem, 
                        ymax =mean + sem
                      ),
                      width = 0.4
        ) +
        theme_minimal() +
        labs(
          title = full_month_name,
          x = "Litter in",
          y = "% offspring in month"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 28),  # Center and increase title font size
          axis.title = element_text(size = 22),  # Increase axis titles font size
          axis.line = element_line(linewidth = 1, color = "black"),  # Custom x and y axis lines
          axis.ticks = element_line(color = "black", size = 1),
          axis.text.x = element_text(margin = margin(t = 5)),  # Add space above x-axis text
          axis.text.y = element_text(margin = margin(r = 5)),  # Add space to the right of y-axis text
          axis.ticks.length = unit(0.3, "cm"), 
          axis.text = element_text(size = 22),  # Increase axis text font size
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank(),  # Remove minor grid lines
          panel.background = element_rect(fill = "white", colour = NA)  # White background
        ) +
        scale_x_discrete(limits = month.abb) + # Ensure all months are shown on the x-axis
        ylim(0, upper_ylim) # Use dynamic ylim rounded to the nearest multiple of 5 
      
      # Print the plot
      print(p)
      
      # Save the plot to a file with a dynamic file name based on the month
      ggsave(
        paste0('./graph/SEM/(SEM) ',species, ' - ', parent, ' born in ', i ,' (', full_month_name, ").png"), 
        plot = p, 
        width = 8, 
        height = 6
      )
    }
    
  
    
    
  }
  
}
