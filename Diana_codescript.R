#loading the necessary libraries

library(tidyverse)
library(readr)

#reading of data in csv format
data1 <- read_csv("https://raw.githubusercontent.com/cema-uonbi/internship_task/main/data/cema_internship_task_2023.csv")
head(data1,5) # first 5 data rows

#checking the dimensions of the dataset(no of rows and columns)
dim(data1)
# the data has 1410 rows and 11 columns

# checking the attributes of the dataset
attributes(data1)

# checking the summary of the dataset
summary(data1)

#checking the datatypes/descriptions of columns
#typeof(data1$period)
#typeof(data1$county)
#typeof(data1$`Total Dewormed`)

#colnames(data1[,sapply(data1,is.numeric)])
#sapply(data1, class)
sapply(data1, typeof)

# moving on to exploratory finding counties with the highest and lowest no of deworming cases
print(max(data1$`Total Dewormed`)) #max no of deworming cases
print(min(data1$`Total Dewormed`)) #min no of deworming cases

 #county with the highest number
datamax <- subset(data1$county, data1$`Total Dewormed` == 392800)
datamax  

#county with the lowest number
datamin <- subset(data1$county, data1$`Total Dewormed` == 97)
datamin

colSums(is.na(data1))

#finding counties with the highest and lowest no of acute malnutrition
print(max(data1$`diarrhoea cases`)) #max no of diarrhea cases
print(min(data1$`diarrhoea cases`)) #min no of diarrhea cases

#county with the highest number
datamaxd <- subset(data1$county, data1$`diarrhoea cases` == 15795)
datamaxd  

#county with the lowest number
datamind <- subset(data1$county, data1$`diarrhoea cases` == 198)
datamind

datac <- data1[order(data1$county,data1$`Total Dewormed`),]
datac

sum(data1$`Total Dewormed`, na.rm = TRUE)

dsum <- sum(data1[which(data1$county =="Nairobi"),3])
dsum

sum(subset(data1,data1$county == "Nairobi"),data1$`Total Dewormed`)

d1 <- data1%>%filter(county == "Nairobi")%>%
  summarise(sum(`Total Dewormed`))
d1

require(dplyr)

n_distinct(data1$county)

sum(data1[which(data1$county=='Nairobi County'), 3])

sum(data1[which(data1$county=='Nairobi County' & data1$period=='Jan-23'), 3])

sum(data1[which(data1$period=='June-23'), 3]) 


# Load dplyr
library(dplyr)

# Group by sum using dplyr
dcount <- data1 %>% group_by(county) %>% 
  summarise(sum_deworm = sum(`Total Dewormed`),
            .groups = 'drop')
print(dcount, n=47)



# Convert tibble to df
df2 <- dcount %>% as.data.frame()
df2

df5 <- df3[order(-df2$sum_deworm),]
df5

#max(df2$sum_demorm)

# Load dplyr
library(dplyr)

# Group by sum using dplyr
dcountd <- data1 %>% group_by(county) %>% 
  summarise(sum_diarrhoea = sum(`diarrhoea cases`),
            .groups = 'drop')
print(dcountd, n=47)



# Convert tibble to df
df3 <- dcountd %>% as.data.frame()
df3 

#arrange in descending order
df4 <- df3[order(-df3$sum_diarrhoea),]
df4

library('ggplot2')
ggplot() + geom_histogram(data = data1, aes(x= "period",)) 



ggplot(data1, aes("county")) + theme_bw() +
geom_bar()+
  labs()

ggplot(data1, aes(x = county, y =`Total Dewormed`)) +    # geom_bar works fine
  geom_bar(stat = "identity") +  coord_flip()

ggplot(data1, aes(x=`Total Dewormed`,y = `stunted 24-59 months`,fill=`Total Dewormed`))


plot(x=data1$`Total Dewormed`,y=data1$`diarrhoea cases`,type = 'p', col ="red")

ggplot() + geom_boxplot(data = data1,aes(x= `Acute Malnutrition`,y=`stunted 6-23 months`, group = 1),outlier.colour = "red")



ggplot(data1, aes(`Acute Malnutrition`)) + theme_bw() +
  geom_histogram(fill = "blue", color = "black",bins = 20)+
  labs() # customize the x and y axis



#acute malunutrition vs county
ggplot(data1, aes(x=reorder(county, `Acute Malnutrition`), y=`Acute Malnutrition`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="Acute Malnutrition")+
  coord_flip() # flip our graph

#period vs acute malnutrition
ggplot(data1, aes(x=reorder(period, +`Acute Malnutrition`), y=`Acute Malnutrition`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="period", y="Acute Malnutrition")+
  coord_flip()

#period vs acute nutrition
ggplot(data1, aes(x=reorder(period, +`diarrhoea cases`), y=`diarrhoea cases`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="period", y="diarrhoea cases")+
  coord_flip()

#county vs stunted growth 6-23 months
ggplot(data1, aes(x=reorder(county, +`stunted 6-23 months`), y=`stunted 6-23 months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="stunted 6-23 months")+
  coord_flip()

#county vs stunted growth 0<6 months
ggplot(data1, aes(x=reorder(county, +`stunted 0-<6 months`), y=`stunted 0-<6 months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="stunted 0-<6 months")+
  coord_flip()

#county vs stunted 24-59 months
ggplot(data1, aes(x=reorder(county, +`stunted 24-59 months`), y=`stunted 24-59 months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="stunted 24-59 months")+
  coord_flip()

#county vs underweight 0<6 months
ggplot(data1, aes(x=reorder(county, +`Underweight 0-<6 months`), y=`Underweight 0-<6 months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="Underweight 0-<6 months")+
  coord_flip()

#county vs underweight 6-23months
ggplot(data1, aes(x=reorder(county, +`Underweight 6-23 months`), y=`Underweight 6-23 months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="Underweight 6-23 months")+
  coord_flip()

#county vs underweight 24 -59 months
ggplot(data1, aes(x=reorder(county, +`Underweight 24-59 Months`), y=`Underweight 24-59 Months`))+ # reorder function used here.
  geom_col()+
  theme_bw()+
  labs(x="county", y="Underweight 24-59 Months")+
  coord_flip()












#df2 %>% group_by(county) %>% slice(which.max(sum_demorm))








