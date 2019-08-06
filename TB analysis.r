TB <- read.csv("C:/Users/Cherilyn/Desktop/BAN7100/Diseases Dataset/Tuberculosis-Clean minus 1800s.csv")

library(psych)
library(ggplot2)
library(pastecs)
library(dplyr)
library(hexbin)


levels(TB$State)
#56 states/territories

summary(TB)
#fatalities are either zero or one
#Period start year runs from 1900 to 2014
#median start year is 1923, means more cases in the low 1900s
#mean also low,  1937; Same mean and median for end year
#count values range from zero to 4744



#further look at count value
boxplot(TB$CountValue)
#all higher data points are outliers

#to read box plot quartiles
BPcv <- boxplot(TB$CountValue)
BPcv$stats
#min = 0
#q1 = 1
#median = 3
#q3 = 13
#max = 31
#all counts above 31 are outliers

describe(TB$CountValue)
#max val = 4744
#mean=43
#median=3

sum(TB$CountValue)
#total instances = 16263720


hist(TB$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#most count values are close to zero




BPF <- boxplot(TB$Fatalities)
BPF$stats
#first and second quartile at 0
#median, 3rd, and max at one


sum(TB$Fatalities)
#total fatalities = 195008

nrow(TB)-sum(TB$Fatalities)
#more than half of the rows have a fatality listed


sum(TB$Fatalities)/sum(TB$CountValue)
#only 1.2% cases of TB resulted in a fatality


hist(TB$Fatalities, col="Red", main = "Histogram of Fatalities", xlab = "Fatalities")
#not very useful since only two integer values, does show more rows with fatalities






ggplot(TB, aes(PeriodStartMonth,CountValue))+geom_jitter()
#start moths with the highest number of count values are December and January
#all other months have count values below 500


ggplot(TB, aes(PeriodEndMonth,CountValue))+geom_jitter()
#end months more spread out


ggplot(TB, aes(PeriodStartYear,CountValue))+geom_jitter()
#big gap in data, later years show higher count values

ggplot(TB, aes(PeriodStartYear,CountValue))+geom_hex()
#looking at the density of the data by start year
#although the later years have higher count values there is more data in the early 1900s

ggplot(TB, aes(PeriodStartYear,CountValue))+geom_density2d()
#shows the density of the count values between the years 1900 to 1945
#years with most rows of data are 1920-1921 and 1936-1940
#highest amount between 0 and 4


ggplot(TB, aes(PeriodEndYear,CountValue))+geom_jitter()
#Matches start year


(ggplot(TB, aes(State,CountValue))+geom_jitter(color=TB$PeriodStartYear)
+theme(axis.text = element_text(angle = 90)))
#Highest count values in California, followed by texas
#Some other states have a few sporadic higher count values
#most states have count values below 1000


ggplot(TB, aes(PeriodStartYear,Fatalities))+geom_jitter()
#no reported fatalities after 1942
#periods of time before 1910 and between 1924 to 1942 where all intervals of data
#a fatality (no non fatalities listed)

ggplot(TB, aes(PeriodStartYear,Fatalities))+geom_hex()
#Highest Density of fatalities around 1925, also highest density of non fatalities
#this is where most of the data is
ggplot(TB, aes(PeriodStartYear,Fatalities))+geom_density2d()
#High density around 1920


ggplot(TB, aes(Fatalities,CountValue))+geom_jitter()
#only intervals with lower count values (appears to be less than 200) have fatalities





levelsofstate <- summarise(group_by(TB,State),count =n())
levelsofstate
#gives number of rows by state
#highest states
# Massachusetts     44788 rows of data
# New Jersey        20749 rows
# OHIO              19621
# PENNSYLVANIA      18413
# NEW YORK          16152
# CALIFORNIA        14754

#lowest
# GUAM                      930
# VIRGIN ISLANDS, U.S.      757
# NORTHERN MARIANA ISLANDS  472
# AMERICAN SAMOA            261



levelsofSTmonth <- summarise(group_by(TB,PeriodStartMonth),count =n())
levelsofSTmonth
#gives number of rows by start month
#December and January have far more rows than any others
#this may explain why there are higher count values for those months


levelsofENDmonth <- summarise(group_by(TB,PeriodEndMonth),count =n())
levelsofENDmonth
#number of rows by end month
#all end months between 28k and 33k
#durations of the periods may vary
#this may explain the unevenness in the count values


levelsofSTyear <- summarise(group_by(TB,PeriodStartYear),count =n())
levelsofSTyear
#number of rows by start year
#although the later dates have higher count values, the dates in the
#late 1910s and early 1920s have the most rows of data
# 1919        14943
# 1922        14314
# 1920        14265
# 1923        14091
# 1921        14088

# few years where data is assumed to be incomplete
# 2013        89
# 2011        54
# 1972        52
# 2014        45


levelsofENDyear <- summarise(group_by(TB,PeriodEndYear),count =n())
levelsofENDyear
#mostly matches start year, some variations due to inconsistent durations


fatalityandstate <- summarise(group_by(TB,State,Fatalities),count =n())
fatalityandstate
#gives number of fatalities and non fatalities by state
#states with highest number of fatalities similar to highest number of rows
# MASSACHUSETTS     24337
# OHIO              11534
# NEW YORK          9562
# NEW JERSEY        9188
# PENNSYLVANIA      8699
# CALIFORNIA        8399

#lowest
#MISSISSIPPI        49
#WYOMING				    37
#HAWAII				      16

#states with no reported deaths: Alaska, American Samoa, Guam, Northern Mariana Islands,
#Puerto Rico, and the US Virgin Islands





############################################################
# Looking at specific states
# Chose states with highest count values or highest rows
############################################################

California <-subset(TB,State=="CALIFORNIA")
Texas <- subset(TB,State == "TEXAS")
Massachusetts <- subset(TB,State == "MASSACHUSETTS")
NewJersey <- subset(TB,State == "NEW JERSEY")
Pennsylvania <- subset(TB,State == "PENNSYLVANIA")
NewYork <- subset(TB,State == "NEW YORK")




#####Exploring by subsetted states#####

### California
summary(California)
CABPcv <- boxplot(California$CountValue)
CABPcv$stats
#Box plot values similar to US numbers, little higher max, Outliers upto 4744
#min =0
#q1 =2
#med =7
#q3 =22
#max =52
describe(California$CountValue)
sum(California$CountValue)
#2819609 total cases in California
sum(California$CountValue)/sum(TB$CountValue)
#17.3% of all cases are in california
hist(California$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#similar to Total TB hist

CABPF <- boxplot(California$Fatalities)
CABPF$stats
#same as for TB more rows with fatalities than not
sum(California$Fatalities)
#8399 deaths
sum(California$Fatalities)/sum(California$CountValue)
#0.3% fatality rate low considering they have the highest numbers of counts

sum(California$Fatalities)/sum(TB$Fatalities)
#4.3% of all TB deaths are in California




###Texas
summary(Texas)
TXBPcv <- boxplot(Texas$CountValue)
TXBPcv$stats
#Max is lower than US max with outliers upto 2481
#min =0
#q1 =1
#med =3
#q3 =8
#max =18
describe(Texas$CountValue)
sum(Texas$CountValue)
#1295933 total cases in Texas
sum(Texas$CountValue)/sum(TB$CountValue)
#7.97% of all cases are in Texas
hist(Texas$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#similar to Total TB hist, not as many values

TXBPF <- boxplot(Texas$Fatalities)
TXBPF$stats
#same as for TB more rows with fatalities than not
sum(Texas$Fatalities)
#6503 deaths
sum(Texas$Fatalities)/sum(Texas$CountValue)
#0.5% fatality rate low

sum(Texas$Fatalities)/sum(TB$Fatalities)
#3.3% of all TB deaths are in Texas




###Massachusetts
summary(Massachusetts)
MABPcv <- boxplot(Massachusetts$CountValue)
MABPcv$stats
#Max is lower than US max with outliers upto 717
#min =0
#q1 =1
#med =2
#q3 =4
#max =8
describe(Massachusetts$CountValue)
sum(Massachusetts$CountValue)
#456452 total cases in Massachusetts
sum(Massachusetts$CountValue)/sum(TB$CountValue)
#2.8% of all cases are in Massachusetts
hist(Massachusetts$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#High amounts less than 50

MABPF <- boxplot(Massachusetts$Fatalities)
MABPF$stats
#same as for TB more rows with fatalities than not
sum(Massachusetts$Fatalities)
#24337 deaths
sum(Massachusetts$Fatalities)/sum(Massachusetts$CountValue)
#5.3% fatality rate Higher than average 

sum(Massachusetts$Fatalities)/sum(TB$Fatalities)
#12.5% of all TB deaths are in Massachusetts, High percentage




###New Jersey
summary(NewJersey)
NJBPcv <- boxplot(NewJersey$CountValue)
NJBPcv$stats
#Max is lower than US max with outliers upto 1221
#min =0
#q1 =1
#med =3
#q3 =8
#max =18
describe(NewJersey$CountValue)
sum(NewJersey$CountValue)
#694521 total cases in NewJersey
sum(NewJersey$CountValue)/sum(TB$CountValue)
#4.27% of all cases are in NewJersey
hist(NewJersey$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#High amount between 0 and 100

NJBPF <- boxplot(NewJersey$Fatalities)
NJBPF$stats
#New Jersey has more rows with non fatalities
sum(NewJersey$Fatalities)
#9188 deaths
sum(NewJersey$Fatalities)/sum(NewJersey$CountValue)
#1.3% fatality rate

sum(NewJersey$Fatalities)/sum(TB$Fatalities)
#4.7% of all TB deaths are in NewJersey




###Pennsylvania
summary(Pennsylvania)
PABPcv <- boxplot(Pennsylvania$CountValue)
PABPcv$stats
##box plot slightly more spread out Max is higher than US max with outliers upto 1511
#min =0
#q1 =1
#med =3
#q3 =23
#max =56
describe(Pennsylvania$CountValue)
sum(Pennsylvania$CountValue)
#796017 total cases in Pennsylvania
sum(Pennsylvania$CountValue)/sum(TB$CountValue)
#4.89% of all cases are in Pennsylvania
hist(Pennsylvania$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#Highest count below 100

PABPF <- boxplot(Pennsylvania$Fatalities)
PABPF$stats
#less rows with deaths than US
sum(Pennsylvania$Fatalities)
#8699 deaths
sum(Pennsylvania$Fatalities)/sum(Pennsylvania$CountValue)
#1.09% fatality rate

sum(Pennsylvania$Fatalities)/sum(TB$Fatalities)
#4.5% of all TB deaths are in Pennsylvania




###New York
summary(NewYork)
NYBPcv <- boxplot(NewYork$CountValue)
NYBPcv$stats
#Most spear outbox plot, Ma less than US, with outliers upto 121
#min =0
#q1 =1
#med =2
#q3 =4
#max =8
describe(NewYork$CountValue)
sum(NewYork$CountValue)
#65831 total cases in NewYork
sum(NewYork$CountValue)/sum(TB$CountValue)
#0.4% of all cases are in NewYork
hist(NewYork$CountValue, col="green", main = "Histogram of Tuberculosis",
     xlab = "Count Value")
#Most count Values less than 10

NYBPF <- boxplot(NewYork$Fatalities)
NYBPF$stats
#same as for TB more rows with fatalities than not
sum(NewYork$Fatalities)
#9562 deaths
sum(NewYork$Fatalities)/sum(NewYork$CountValue)
#14.5% fatality rate Very High!

sum(NewYork$Fatalities)/sum(TB$Fatalities)
#4.9% of all TB deaths are in NewYork






######### looking at selected states by county #########
#all states except for New York had more data listed as NA for county so NA was excluded

(ggplot(data = subset(California, !is.na(County)), aes(County,CountValue))+geom_jitter()
  +theme(axis.text = element_text(angle = 90))+ggtitle("California"))
#highest cases appear in Los Angeles County and San Francisco County
#both are big cities
#lower count values because NAs were excluded

(ggplot(data = subset(Texas, !is.na(County)), aes(County,CountValue))+geom_jitter()
  +theme(axis.text = element_text(angle = 90))+ggtitle("Texas"))
#more counties with higher count values in Texas
#highest amounts are in Bexar, Dallas, and Harris county
#big cities of San Antonio, Dallas, and Houston

(ggplot(data = subset(Massachusetts, !is.na(County)), aes(County,CountValue))
  +geom_jitter()+theme(axis.text = element_text(angle = 90))+ggtitle("Massachusetts"))
#highest cases in Suffolk county, Big city of Boston in this county

(ggplot(data = subset(NewJersey, !is.na(County)), aes(County,CountValue))+geom_jitter()
  +theme(axis.text = element_text(angle = 90))+ggtitle("New Jersey"))
#highest cases in Essex and Hudson County
#big cities of Newark, The Oranges, Jersey City, North Bergen, Secaucus

(ggplot(data = subset(Pennsylvania, !is.na(County)), aes(County,CountValue))
  +geom_jitter()+theme(axis.text = element_text(angle = 90))+ggtitle("Pennsylvania"))
#highest Cases in Philadelphia County, Big City
#second highest in Allegheny where Pittsburgh is

(ggplot(data = subset(NewYork, !is.na(County)), aes(County,CountValue))+geom_jitter()
  +theme(axis.text = element_text(angle = 90))+ggtitle("New York"))
#highest cases in Erie County where Buffalo is
#also Monroe County where Rochester is
#data missing for NYC, maybe listed as NA




#different states by period start month

(ggplot(California, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("California"))
#matches state graph of most start months in December and January
#count Values upto and over 4000

(ggplot(Texas, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("Texas"))
#matches state graph of most start months in December and January
#count values upto 2500

(ggplot(Massachusetts, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("Massachusetts"))
#matches state graph of most start months in December and January
#count values upto 700

(ggplot(NewJersey, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("New Jersey"))
#matches state graph of most start months in December and January
#Count values upto 1250

(ggplot(Pennsylvania, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("Pennsylvania"))
#matches state graph of most start months in December and January
#count values upto 1500

(ggplot(NewYork, aes(PeriodStartMonth,CountValue))+geom_jitter()
+theme(axis.text = element_text(angle = 90))+ggtitle("New York"))
#period start months more spread out however none of the months have count 
#values as high ## Count values upto 125




#Count Value in Different states by start year

#California
ggplot(California, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("California")
#later years less densely packed than for all of US
ggplot(California, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("California")
#follows same Pattern as US with most data in the early 1900s
ggplot(California, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("California")
#Highest amount of data around 1920 around 0-1


#Texas
ggplot(Texas, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("Texas")
#later years less densely packed than for all of US
ggplot(Texas, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("Texas")
#follows same Pattern as US with most data in the early 1900s
ggplot(Texas, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("Texas")
#Highest point just after 1920, Also higher from about 1929-1939, around 0-1


#Massachusetts
ggplot(Massachusetts, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("Massachusetts")
#later years less densely packed than for all of US
#earlier years less flat, this could be due to highest count value being around 700
ggplot(Massachusetts, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("Massachusetts")
#follows same Pattern as US with most data in the early 1900s
ggplot(Massachusetts, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("Massachusetts")
#Interesting density, Many "7" around 1916 also high amounts of 0-1
#very dense around 1940


#New Jersey
ggplot(NewJersey, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("New Jersey")
#later years less densely packed than for all of US
ggplot(NewJersey, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("New Jersey")
#follows same Pattern as US with most data in the early 1900s
ggplot(NewJersey, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("New Jersey")
#High density just after 1920


#Pennsylvania
ggplot(Pennsylvania, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("Pennsylvania")
#later years less densely packed than for all of US
ggplot(Pennsylvania, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("Pennsylvania")
#follows same Pattern as US with most data in the early 1900s
ggplot(Pennsylvania, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("Pennsylvania")
#Less dense than total US With more of the density under 1920


#New York
ggplot(NewYork, aes(PeriodStartYear,CountValue))+geom_jitter()+ggtitle("New York")
#No data after 1941
ggplot(NewYork, aes(PeriodStartYear,CountValue))+geom_hex()+ggtitle("New York")
#Most data around 1920
ggplot(NewYork, aes(PeriodStartYear,CountValue))+geom_density2d()+ggtitle("New York")
#Higher Cases of "1" from around 1918 to 1923





#Fatalities in different states by start year

#California
ggplot(California, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("California")
#No listed Fatalities after 1940s, group of years where only fatalities listed 
ggplot(California, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("California")
#still shows highest count right around 1920
ggplot(California, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("California")
#Matches Highest density around 1920


#Texas
ggplot(Texas, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("Texas")
#No listed Fatalities after 1940s, group of years where only fatalities listed
ggplot(Texas, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("Texas")
#Higher Fatalities after 1925
ggplot(Texas, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("Texas")
#Highest from about 1918 to 1940, Dense fatalities


#Massachusetts
ggplot(Massachusetts, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("Massachusetts")
#No listed Fatalities after 1940s, group of years where only fatalities listed
ggplot(Massachusetts, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("Massachusetts")
#Higher counts below 1925
ggplot(Massachusetts, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("Massachusetts")
#Very Dense around 1920


#New Jersey
ggplot(NewJersey, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("New Jersey")
#No listed Fatalities after 1940s, group of years where only fatalities listed
ggplot(NewJersey, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("New Jersey")
#fatalities not as dense as non fatalities
ggplot(NewJersey, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("New Jersey")
#Highest around 1920


#Pennsylvania
ggplot(Pennsylvania, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("Pennsylvania")
#No listed Fatalities after 1940s, group of years where only fatalities listed
#gap in fatalities around 1924
ggplot(Pennsylvania, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("Pennsylvania")
#Highest density under 1925
ggplot(Pennsylvania, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("Pennsylvania")
#Highest density around 1910


#New York
ggplot(NewYork, aes(PeriodStartYear,Fatalities))+geom_jitter()+ggtitle("New York")
#group of years where only fatalities listed
ggplot(NewYork, aes(PeriodStartYear,Fatalities))+geom_hex()+ggtitle("New York")
#density more spread out highest just before 1920
ggplot(NewYork, aes(PeriodStartYear,Fatalities))+geom_density2d()+ggtitle("New York")
#Most dense are 1920

