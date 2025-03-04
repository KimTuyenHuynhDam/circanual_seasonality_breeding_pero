library(readxl)
library(dplyr)
library(tidyverse)
library(broom)
#install.packages('circular')
library(circular)
library(ggplot2)
library(RColorBrewer)
#install.packages('janitor')
library(janitor)

#install.packages("reshape2 ")      
library("reshape2")
library(openxlsx)



normalized_betas_sesame =read_xlsx("normalized_betas_sesame.xlsx")



SS=read_xlsx("all_DNAm_mice_all_tissues.xlsx") %>% distinct() %>%
  filter(CanBeUsedForAgingStudies == 'yes' & Tissue == "Tail")


SS['SpeciesAbbreviation'][SS['SpeciesAbbreviation'] == "NA"] <- "Hybrid"

# Convert birth month to radians for circular representation
SS$Month_radians <- as.circular((SS$BirthMonth - 1) * 2 * pi / 12)

# Next, create sine and cosine variables for birth month
SS$Month_sin <- sin(2 * pi * SS$BirthMonth / 12)
SS$Month_cos <- cos(2 * pi * SS$BirthMonth / 12)


# merge the methylation results and mice info
nbs = normalized_betas_sesame %>%
  pivot_longer(!CGid, names_to = "Basename", values_to = "ProMet") %>%
  pivot_wider(names_from = CGid, values_from = ProMet) %>%
  inner_join(SS) %>%
  # there is NO universal naming scheme for "loci", but these three letters seem to cover it
  pivot_longer(starts_with(c("cg","rs","ch")),names_to = "CGnum" , values_to = "ProMet")

##########scannning
myscan = nbs %>%
  mutate(Sex = as.factor(Sex)) %>%

  mutate(SpeciesAbbreviation = as.factor(SpeciesAbbreviation))  %>%
  group_by(CGnum) %>%
  nest() %>% 
  mutate(logp = map(data, ~ -log(anova(
    lm(ProMet ~ Age + Sex + SpeciesAbbreviation, data = .),
    lm(ProMet ~ Month_sin + Month_cos + Age + Sex  + SpeciesAbbreviation, data = .)
  )[[6]][2])/log(10))) %>%
  unnest(logp)


myscan$p = 10^-myscan$logp

myscan$fdr = p.adjust(myscan$p, method = 'fdr') 

anno_BW = read.csv('Peromyscus_maniculatus_bairdii.hu_pman_2.1.100.HorvathMammalMethylChip40.v1.csv')

scan = myscan[, c(1,3,4,5)]

colnames(scan)[1] = 'CGid'

BW_scan = scan %>% left_join(anno_BW, by = 'CGid')


write.xlsx(BW_scan, './data/BWanno - CpGs - sin, cos - circannual birthmonth - all tails.xlsx')

########## select top CpGs with p-value and FDR < 0.1

top_cpg = myscan %>% filter(p < 0.05 & fdr < 0.1) 

nrow(top_cpg)

################

####### this one test the group by birth month of the first CpG, then calculate the average Promet of each month
group_by_birthmonth_test = top_cpg[[2]][[1]]  %>% group_by(BirthMonth)%>%
  summarise(mean_ProMet=mean(ProMet),.groups = 'drop') %>%
  as.data.frame()

#View(group_by_birthmonth_test)
############
###create empty value of each datatable with BirthMonth as first column
average_met = group_by_birthmonth_test[1]
high_september_met = group_by_birthmonth_test[1]
low_september_met = group_by_birthmonth_test[1]
middle_september_met = group_by_birthmonth_test[1]


########################

for (i in 1: nrow(top_cpg)) {
  each_cpg = top_cpg[[2]][[i]] 
  
  ### this for loop goes through each CpG, group them by BirthMonth, then get average methylation level (ProMet) of each birthmonth
  ### next normalize value of each ProMet over the maximum ProMet (of any month)
  
  group_by_birthmonth = each_cpg %>% group_by(BirthMonth) %>%
    summarise(mean_ProMet=mean(ProMet),.groups = 'drop') %>%
    as.data.frame() %>%
    mutate(percentage = mean_ProMet/max(mean_ProMet) * 100)
  
  #### assign the name of each specific CpG to each normalized percentage
  colnames(group_by_birthmonth)[3] = top_cpg[[1]][[i]] 
  
  
  #### combine all  CpGs together, we wiill get the summarized table of top CpGs after normalizing to percentage
  average_met = average_met %>% cbind(group_by_birthmonth[3])
  
  ####select CpGs having lowest methylation level in September
  
  if (group_by_birthmonth[9,2] == min(group_by_birthmonth[2])) {
    low_september_met = low_september_met %>% cbind(group_by_birthmonth[3]) 
    
  } 
  
  else{ 
    ####select CpGs having lowest methylation level in September 
    if (group_by_birthmonth[9,2] == max(group_by_birthmonth[2]))
    {high_september_met = high_september_met %>% cbind(group_by_birthmonth[3])}
    
    ####the remaing are CpGs having middle methylation level in September
    else {middle_september_met = middle_september_met %>% cbind(group_by_birthmonth[3])}
  }
  
}

#t_average_met = t(average_met) %>% row_to_names(row_number = 1)
#t(average_met)


#View(high_september_met)
#View(low_september_met)
#View(middle_september_met)

# number of CpGs would be ncol(average_met) -1 (as 1 column for Birthmonth)
ncol(average_met)
ncol(high_september_met)
ncol(low_september_met)
ncol(middle_september_met)

#############graph all CpGs as lines, each CpG/line
######for all top CpGs
data_long <- melt(average_met, id.vars = "BirthMonth", variable.name  = "cg")


plot_all <- ggplot(data_long, aes(BirthMonth,value)) + geom_line(aes(colour = cg)) + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),   # Custom font size for x-axis ticks
        axis.text.y = element_text(size = 12),                         # Custom font size for y-axis ticks
        axis.title.x = element_text(size = 14, face = "bold"),# Bold x-axis label with larger font
        axis.title.y = element_text(size = 14, face = "bold") 
  ) + 
  scale_x_continuous(breaks=seq(1,12,1)) +
  xlab('Birth Month') +ylab('Percentage of methylation level')
# View the plot in RStudio's Viewer Pane
print(plot_all)

# Save the plot with a full path specification
ggsave("./data/methylation level across birthmonth/all tails - all.png", plot = plot_all, width = 10, height = 8, dpi = 300)

############CpGs having highest signal in September

high_september_met_longdf <- melt(high_september_met, id.vars = "BirthMonth", variable.name  = "cg")


plot_high <- ggplot(high_september_met_longdf, aes(BirthMonth,value)) + geom_line(aes(colour = cg)) + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),   # Custom font size for x-axis ticks
        axis.text.y = element_text(size = 12),                         # Custom font size for y-axis ticks
        axis.title.x = element_text(size = 14, face = "bold"),# Bold x-axis label with larger font
        axis.title.y = element_text(size = 14, face = "bold") 
  ) + 
  scale_x_continuous(breaks=seq(1,12,1)) + 
  xlab('Birth Month') +ylab('Percentage of methylation level') 

# View the plot in RStudio's Viewer Pane
print(plot_high)

# Save the plot with a full path specification
ggsave("./data/methylation level across birthmonth/all tails - high in Sep.png", plot = plot_high, width = 10, height = 8, dpi = 300)

############CpGs having lowest signal in September

low_september_met_longdf <- melt(low_september_met, id.vars = "BirthMonth", variable.name  = "cg")


plot_low <- ggplot(low_september_met_longdf, aes(BirthMonth,value)) + 
  geom_line(aes(colour = cg)) + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),   # Custom font size for x-axis ticks
        axis.text.y = element_text(size = 12),                         # Custom font size for y-axis ticks
        axis.title.x = element_text(size = 14, face = "bold"),# Bold x-axis label with larger font
        axis.title.y = element_text(size = 14, face = "bold") 
  ) + 
  scale_x_continuous(breaks=seq(1,12,1)) +
  xlab('Birth Month') +ylab('Percentage of methylation level')

# View the plot in RStudio's Viewer Pane
print(plot_low)

# Save the plot with a full path specification
ggsave("./data/methylation level across birthmonth/all tails - low in Sep.png", plot = plot_low, width = 10, height = 8, dpi = 300)

#########################CpGs having middle signal in September
middle_september_met_longdf <- melt(middle_september_met, id.vars = "BirthMonth", variable.name  = "cg")


plot_middle <- ggplot(middle_september_met_longdf, aes(BirthMonth,value)) + geom_line(aes(colour = cg)) + theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 12, hjust = 1),   # Custom font size for x-axis ticks
        axis.text.y = element_text(size = 12),                         # Custom font size for y-axis ticks
        axis.title.x = element_text(size = 14, face = "bold"),# Bold x-axis label with larger font
        axis.title.y = element_text(size = 14, face = "bold") 
  ) + 
  scale_x_continuous(breaks=seq(1,12,1)) + xlab('Birth Month') +ylab('Percentage of methylation level')

# View the plot in RStudio's Viewer Pane
print(plot_middle)

# Save the plot with a full path specification
ggsave("./data/methylation level across birthmonth/all tails - middle in Sep.png", plot = plot_middle, width = 10, height = 8, dpi = 300)

# Create a data frame with the number of columns in each specified data frame
summary_table <- data.frame(
  Group = c("average_met", "high_september_met", "low_september_met", "middle_september_met"),
  Count = c(ncol(average_met)-1, ncol(high_september_met)-1, ncol(low_september_met)-1, ncol(middle_september_met)-1)
)

# Write the summary table to a CSV file
write.xlsx(summary_table, "./data/methylation level across birthmonth/summary_table - all tails.xlsx")

