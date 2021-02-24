
##### SECTION 1: Import and Process Data

# List required packages
require(arules)
require(gains)
require(caret)
require(rpart)
require(rpart.plot)
require(readxl)
require(dplyr)
require(stringi)
require(stringr)
require(reshape2)
require(corrplot)
require(ggplot2)

#Set Working Directory

#Read in operational data. Note: Geocoding done previously using https://geocoding.geo.census.gov/ to find the census
#data associated with each address. Not all addresses were able to align to a census track.

base_data<- read.csv("Food_Establishment_Inspection_Data_2016-2018 - Geocoded2.csv")

#Need to remove data w/o census tract or with census track outside of King County, WA (#033, shown as 33)

fi_data<-filter(base_data,base_data$County.Code==33)
glimpse(fi_data) #Inspection.Date is not processing as a date, need to reformat

fi_data$Inspection.Year<- str_sub(fi_data$Inspection.Date, -4,-1)
unique(fi_data$Inspection.Year) #confirm years in all rows of data
#previous exploration of data discovered tract coding mismatch between currently available benchmark data and the available demographics coded data. Use Tract and drop Tract Code

fi_data <-fi_data[-39]

#56k lines of data remain. Have year added for match up to SADE & QCT data

#Read in other datasets

base_sade <- read.csv("ofm_pop_sade_tract_2015_to_2019_53033.csv", header=TRUE)
#reads in 1990 obs w/25 variables, data each year stored in obs vertically
base_qct <- read.csv("qct_data_extract Sea-Bell-Tac.csv", header=TRUE)
#721 obs w/42 variables, data each year it's own column, stored horizontally

unique(base_sade$Year) #years 2015-2019 included, need to trim to keep data consistnecy, 2016-2018


sade_data <- filter(base_sade,base_sade$Year %in% c(2016,2017,2018)) #exclude unneeded year data
#for simple join, need to align year data horizontally. 
## Not needed given multi filter function & w/Year value now in FI data
#sade_data2016 <- filter(base_sade,base_sade$Year ==2016)
#sade_data2017 <- filter(base_sade,base_sade$Year ==2017)
#sade_data2018 <- filter(base_sade,base_sade$Year ==2018)


#colnames(sade_data) #get colnames
#colnames(sade_data) <- c( "Area.Name","Area.ID",
                         # "Total_2016", "Male_2016", "Female_2016" , 
                         # "White.Total_2016"  ,"White.Male_2016" , "White.Female_2016",  
                         # "Black.Total_2016", "Black.Male_2016",  "Black.Female_2016",  
                         # "AIAN.Total_2016", "AIAN.Male_2016" ,  "AIAN.Female_2016"  ,
                         # "Asian.Total_2016",  "Asian.Male_2016", "Asian.Female_2016",
                         # "NHOPI.Total_2016", "NHOPI.Male", "NHOPI.Female" , 
                         # "Two.or.More.Races.Total","Two.or.More.Races.Male", "Two.or.More.Races.Female" )
glimpse(sade_data)



qct_data <-filter(base_qct, base_qct$county==33) #area includes data from counties to the north and south, trim

#It is the QCT data that needs to be restructured to isolate year as a column

colnames(qct_data) 

qct_data2016 <-qct_data
qct_data2017 <-qct_data
qct_data2018 <-qct_data

qct_data2016$Year=2016
qct_data2017$Year=2017
qct_data2018$Year=2018

#Drop other year columns
qct_data2016<- qct_data2016[-c(13:14,24:35,37:38,40:41)]
qct_data2017<- qct_data2017[-c(12,14,18:23,30:35,36,38,39, 41)]
qct_data2018<- qct_data2018[-c(12:13,18:29,36:37,39:40)]

#rename for recombo of data and meaning
names(qct_data2016)[12] <- "Per_Capita_Income"
names(qct_data2016)[13] <- "Total_Pop"
names(qct_data2016)[14] <- "Pop_GroupQuarters"
names(qct_data2016)[15] <- "Households"
names(qct_data2016)[16] <- "AssessedPop"
names(qct_data2016)[17] <- "ME_of_AssesedPop"
names(qct_data2016)[18] <- "BelowPov"
names(qct_data2016)[19] <- "ME_of_BelowPov"
names(qct_data2016)[20] <- "MedianIncome"
names(qct_data2016)[21] <- "ME_of_MedianIncome"
names(qct_data2016)[22] <- "PerCap/Median"
names(qct_data2016)[23] <- "PovertyRate"



names(qct_data2017)[12] <- "Per_Capita_Income"
names(qct_data2017)[13] <- "Total_Pop"
names(qct_data2017)[14] <- "Pop_GroupQuarters"
names(qct_data2017)[15] <- "Households"
names(qct_data2017)[16] <- "AssessedPop"
names(qct_data2017)[17] <- "ME_of_AssesedPop"
names(qct_data2017)[18] <- "BelowPov"
names(qct_data2017)[19] <- "ME_of_BelowPov"
names(qct_data2017)[20] <- "MedianIncome"
names(qct_data2017)[21] <- "ME_of_MedianIncome"
names(qct_data2017)[22] <- "PerCap/Median"
names(qct_data2017)[23] <- "PovertyRate"


names(qct_data2018)[12] <- "Per_Capita_Income"
names(qct_data2018)[13] <- "Total_Pop"
names(qct_data2018)[14] <- "Pop_GroupQuarters"
names(qct_data2018)[15] <- "Households"
names(qct_data2018)[16] <- "AssessedPop"
names(qct_data2018)[17] <- "ME_of_AssesedPop"
names(qct_data2018)[18] <- "BelowPov"
names(qct_data2018)[19] <- "ME_of_BelowPov"
names(qct_data2018)[20] <- "MedianIncome"
names(qct_data2018)[21] <- "ME_of_MedianIncome"
names(qct_data2018)[22] <- "PerCap/Median"
names(qct_data2018)[23] <- "PovertyRate"


#recombine QCT data w/year variable. Make data tall 
qct_data_all <-rbind(qct_data2016,qct_data2017,qct_data2018)

#Time to join data tables into single table by census track
#first evaluate census tract data type in each table




glimpse(qct_data_all[4:5])
glimpse(fi_data[39:40])
glimpse(sade_data[1:2])


#unique(fi_data[39:40])
#
#unique(qct_data_all[4:5], nmax=10)

#Some issues with datatype of census track ID. Excel files read them as INT vs varchar and dropped leading zeros. 
#Census tracks IDs are 6 digits. Need to extract proper tract ID from full code (STATE+COUNTY+TRACT, 2+3+6)
#SADE & QTC have full codes so grab right 6 digits, FI data will need Tract.Code evaluated and leading zeros added where needed

qct_data_all$tract<- str_sub(qct_data_all$tract_id,-6,-1)
sade_data$Area.Name <- str_sub(sade_data$Area.ID,-6,-1)
fi_data$Tract<- formatC(fi_data$Tract,width = 6,flag="0") 

print(fi_data$Tract, max = 10)
print(sade_data$Area.Name, max = 10)
print(qct_data_all$tract, max = 10)

#rename year and tract variables for easier joins

names(fi_data)[41]<-"Year"
names(fi_data)[40]<-"Tract"
names(qct_data_all)[4] <-"Tract"
names(sade_data)[1]<-"Tract"

#data type issue
data.class(qct_data_all$Year)
qct_data_all$Year<-paste0(qct_data_all$Year)
data.class(qct_data_all$Year)
sade_data$Year<-paste0(sade_data$Year)
data.class(sade_data$Year)
#now all character


all_data <- fi_data

all_data_1 <- left_join(all_data ,qct_data_all, by = c("Year","Tract"))
all_data_2 <-left_join(all_data_1, sade_data, by =c("Year", "Tract"))

all_data<-all_data_2

glimpse (all_data)
#87 variables (some duplicates from joined tables) with 56k obvervations



##### SECTION 2: Descriptive Statistics & Data Exploration

summary(all_data[1:5])
summary(all_data[6:10])
summary(all_data[11:15])
summary(all_data[16:20]) 
#Inspection Type, Inspection Score, and Inspecion Result will be important variables, 18:20
summary(all_data[21:25])
#Insection.Closed.Business, Violation.Type, 21:22
summary(all_data[26:30])
#Number of Violations is a calculated field showing violations per inspection
summary(all_data[31:35])
summary(all_data[36:40])
summary(all_data[41:45])
summary(all_data[46:50])
#Don't use Area_pop, appears to be for whole state QCT file
summary(all_data[51:55])
summary(all_data[56:60])
#Median income varies widely, $14k to $222k. Per capita range is much less, $55k-57k
summary(all_data[61:65])
#will need to exclude poverty rate = 0% as that is from the margin of error being too high, not a 0% rate
summary(all_data[66:70])
#After finishing review, should calculate percent of total for the demographics
summary(all_data[71:75])
summary(all_data[76:80])
summary(all_data[81:87])
#all_data$MinorityPop<-sum(all_data$Black.Total,all_data$AIAN.Total,all_data$Asian.Total,all_data$NHOPI.Total,all_data$Two.or.More.Races.Total)
all_data$MinorityPop<- rowSums( cbind(all_data$Black.Total,all_data$AIAN.Total,
                                      all_data$Asian.Total,all_data$NHOPI.Total,all_data$Two.or.More.Races.Total),
                                na.rm=TRUE)
all_data$MinorityPercent<-all_data$MinorityPop/all_data$Total
#Minority population and minority population as percent of total added. 
summary(all_data[88:89])
#A very wide range from 5% to 90%. Variance should help with determining relationship to other variables. 

#Get percent of each non-white race category

all_data$Per_Black <- all_data$Black.Total/all_data$Total
all_data$Per_AIAN <-all_data$AIAN.Total/all_data$Total
all_data$Per_Asian <-all_data$Asian.Total/all_data$Total
all_data$Per_NHOPI <-all_data$NHOPI.Total/all_data$Total
all_data$Per_Two.Or.More <- all_data$Two.or.More.Races.Total/all_data$Total

#names(all_data)[90]<- "Per_Black" #correcting prior typo



#Code in the Inspection Findings

all_data$Inspection.Result.Dummy <- 0

all_data$Inspection.Result.Dummy <- ifelse(all_data$Inspection.Result %in% c("Satisfactory", "Complete"), -1,1) 
#unique(all_data$Inspection.Result.Dummy)



unique(all_data$Inspection.Result.Dummy)
unique (all_data$Inspection.Result)                                                                                                  


###### SECTION 2: Histograms & Stats
                                                                                                   
summary(all_data$MinorityPercent)  #Min 5%, max 90%                                                                                                 
hist(all_data$MinorityPercent) #close to normal distribution, some outliers

summary(all_data$Per_Black)   #min 0.1%, max 42%                                                                                                
hist(all_data$Per_Black) #left skew

summary(all_data$Per_AIAN)         #min 0.1%, max 17%                                                                                          
hist(all_data$Per_AIAN) #left skew, high outlier (data validity?)

summary(all_data$Per_Asian) #min 0.8%, max 69%                                                                                                  
hist(all_data$Per_Asian) #closer to normal but still a left skew with some outliers to the right

summary(all_data$Per_NHOPI)  #min 0.0%, max 9%                                                                                                 
hist(all_data$Per_NHOPI) #heavy skew, mostly 0%s

summary(all_data$Per_Two.Or.More)    # min 2%, max 9%                                                                                               
hist(all_data$Per_Two.Or.More) #normal distribution


summary(all_data$Total) # lowest pop 1.3k, highest 121k #no density variable
hist(all_data$Total) #fairly normal distribution

summary(all_data$MedianIncome) # min $14k, max 222k
hist(all_data$MedianIncome) #Likely won't be used. Fairly normal distribution but with high level outliers. 

summary(all_data$Inspection.Score) #mostly 0s, min 0, max 141
hist(all_data$Inspection.Score) # left skew

summary(all_data$Number.of.Violations) #mostly 0s, min 0, max 18
hist(all_data$Number.of.Violations) # left skew
#No violations sees very common. Most businesses must pass

#Summary(all_data$Inspection.Result.Dummy)


hist(all_data$Inspection.Result.Dummy,labels = TRUE)
#close to a 50-50 split. Means that some inspections which likely had no violations may still have been unsatisfactory? 




#Inspection_Variables <- all_data [c(24,29,95)]
Test_Data <- all_data [c(24,29,89:95)]

glimpse(Test_Data)

#ggplot(Inspection_Variables, aes(x=Violation.Points, y=Number.of.Violations)) + 
 # geom_point(color = '#2980B9', size = 4) + geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')

cor.test(x=Test_Data$Violation.Points, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_Black, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_AIAN, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_Asian, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_NHOPI, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_Two.Or.More, method = 'spearman')


cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_Black, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_AIAN, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_Asian, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_NHOPI, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_Two.Or.More, method = 'spearman')


cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_Black, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_AIAN, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_Asian, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_NHOPI, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_Two.Or.More, method = 'spearman')


#Rerunning at lowest quartile of Median Incom
Test_Data <- filter(all_data, all_data$MedianIncome <58321)

Test_Data <- Test_Data [c(24,29,89:95)]

glimpse(Test_Data)


cor.test(x=Test_Data$Violation.Points, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_Asian, method = 'spearman')


cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_Asian, method = 'spearman')


cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_Asian, method = 'spearman')



#Rerunning at highest quartile of Median Income
Test_Data <- filter(all_data, all_data$MedianIncome > 96500 )

Test_Data <- Test_Data [c(24,29,89:95)]

glimpse(Test_Data)


cor.test(x=Test_Data$Violation.Points, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Violation.Points, y=Test_Data$Per_Asian, method = 'spearman')


cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Number.of.Violations, y=Test_Data$Per_Asian, method = 'spearman')


cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$MinorityPercent, method = 'spearman')
cor.test(x=Test_Data$Inspection.Result.Dummy, y=Test_Data$Per_Asian, method = 'spearman')


#cor.test(x=Test_Data$Violation.Points, y=Test_Data$MinorityPercent, method = 'pearson') #curious what Pearson looked like

model_MP<-lm(MinorityPercent ~  Number.of.Violations + Inspection.Result.Dummy + Violation.Points,  data=Test_Data)

summary(model_MP)
summary(model_MP)$coefficient
confint(model_MP)

sigma(model_MP)/mean(Test_Data$MinorityPercent)

model_nov <-lm(Number.of.Violations ~ MinorityPercent, data = Test_Data)
summary(model_nov)
summary(model_nov)$coefficient
confint(model_nov)


#Viewing correlations between inspection variables themselves
#cor.test(x=Inspection_Variables$Violation.Points, y=Inspection_Variables$Number.of.Violations, method = 'spearman')
#cor.test(x=Inspection_Variables$Violation.Points, y=Inspection_Variables$Inspection.Result.Dummy, method = 'spearman')
#cor.test(x=Inspection_Variables$Number.of.Violations, y=Inspection_Variables$Inspection.Result.Dummy, method = 'spearman')
