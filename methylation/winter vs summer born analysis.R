library(readxl)
library(tidyverse)
library(broom)


#import the downloaded data
normalized_betas_sesame =read_xlsx("normalized_betas_sesame.xlsx")
ID=read_xlsx("Mice info-tails -win collection.xlsx")

#####this is the analysis for polygamous mice.
### for monogamous mice, just replace: SS = ID %>% filter(Monogamous =="yes")

SS= ID %>% filter(Monogamous =="no")


# merge the methylation results and mice info
nbs = normalized_betas_sesame %>%
  pivot_longer(!CGid, names_to = "Basename", values_to = "ProMet") %>%
  pivot_wider(names_from = CGid, values_from = ProMet) %>%
  inner_join(SS) %>%
  # there is NO universal naming scheme for "loci", but these three letters seem to cover it
  pivot_longer(starts_with(c("cg","rs","ch")),names_to = "CGnum" , values_to = "ProMet")

test = nbs %>%
  mutate(BirthSeason = as.factor(BirthSeason)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(SpeciesAbbreviation = as.factor(SpeciesAbbreviation))  %>%
  group_by(CGnum) %>%
  nest() 

tt = as.data.frame(test$data[1])

f0 = lm(ProMet ~ Age + Sex + SpeciesAbbreviation , data = tt)
f1 = lm(ProMet ~ BirthSeason + Age + Sex + SpeciesAbbreviation , data = tt) 
-log(anova(f0,f1)[[6]][2])/log(10)


########
## END test model fitting
########

#Run a scan
#note the model compares add BirthMonth (winter vs summer) to a model that includes
#Age, Sex, Species as co-variates
#the result of each scan is a -log10(p-value) ... analogous to a LOD score given 1 df test...

myscan = nbs %>%
  mutate(BirthSeason = as.factor(BirthSeason)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(SpeciesAbbreviation = as.factor(SpeciesAbbreviation))  %>%
  group_by(CGnum) %>%
  nest() %>% 
  mutate(logp = map(data, ~ -log(anova(
    lm(ProMet ~ Age + Sex + SpeciesAbbreviation, data = .),
    lm(ProMet ~ BirthSeason + Age + Sex + SpeciesAbbreviation, data = .)
  )[[6]][2])/log(10))) %>%
  unnest(logp)

# plot all the -log10(p-values), if there is signal you would expect a "hockey stick"
#   like response where larger p-value tend to rise above the straight fit to the p-values

ll = nrow(myscan)
mytheory = sort(-log((1:ll)/ll - 1/(2*ll)/log(10)))
myobs = sort(myscan$logp)
plot(mytheory,myobs,main="QQ-plot",xlab="theory -log10p",ylab="obs -log10p",pch=16,cex=0.25)
abline(lm(myobs~mytheory),col="red")


##the myscan result will be exported
#write.csv(myscan[,c(1,3)], "all CpGs -nonmonogamous -tails.csv")

