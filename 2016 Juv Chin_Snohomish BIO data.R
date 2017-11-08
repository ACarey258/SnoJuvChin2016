########### WDFW TBiOS 2016 Juvenile Chinook Study ############
################# SNOHOMISH BIOMETRIC DATA ###################
##############################################################

rm(list=ls(all=TRUE))

#load required packages/libraries
library(readxl)
library(doBy)
library(psych)
library(reshape2)
library(reshape)
library(plotrix)
library(plyr)
library(broom)
library(dplyr)
library(lsmeans) #program marked as being drepecated - use eemeans instead 11.6.17
library(stats)
library(emmeans) #new program for calculating least squared means 11.6.17

#set paths, make a list of text for the files to be used
paths = list("C:\\data\\CareyA\\PSEMP\\GitHub\\JuvChinook-2016\\2016 Juv Chin_Snohomish BIO data_forR.xlsx",
             "C:\\data\\CareyA\\PSEMP\\GitHub\\JuvChinook-2016\\Outputs\\")

outfile = paths[[2]]

SnoBio <- read_excel(paths[[1]],"SnoBIOData")

SnoRaw <- as.data.frame(SnoBio[ ,c(3:5,7,10:12,15,19:21,27:29)])#dataframe of only data needed for sum stats

SnoEarly <- subset(SnoRaw, CollectionTime =="Early") #separate out all Early data
SnoLate <- subset(SnoRaw, CollectionTime =="Late") #separate out all Late data

colnames(SnoEarly)[6] <- "ForkLength_mm"
colnames(SnoEarly)[7] <- "Weight_g"
colnames(SnoEarly)[12] <- "WBSampleID"
colnames(SnoEarly)[4] <- "Site"

colnames(SnoLate)[6] <- "ForkLength_mm"
colnames(SnoLate)[7] <- "Weight_g"
colnames(SnoLate)[12] <- "WBSampleID"
colnames(SnoLate)[4] <- "Site"


FLavgs <- summaryBy(ForkLength_mm~Site, data = SnoEarly, FUN = c(mean, sd, length)) #length = n
WTavgs <- summaryBy(Weight_g~Site, data = SnoEarly, FUN = c(mean, sd, length))

SnoEarly[is.na(SnoEarly)] <- 0 #converts all NAs to 0 so that origin data can be added
HatchN <- summaryBy(HATCHERY~Site, data = SnoEarly, FUN = sum) #sums Hatchery fish
WildN <- summaryBy(WILD~Site, data = SnoEarly, FUN = sum) #sums Wild fish
Origin <- full_join(HatchN, WildN, by = c("Site")) #merge Hatchery and Wild fish counts
Origin %>% mutate(total = (HATCHERY.sum + WILD.sum), PrcntNatural = (WILD.sum/total)*100) #calculates total fish-n and percent natural fish

