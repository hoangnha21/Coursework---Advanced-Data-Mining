install.packages("ggplot2")
library(ggplot2)
data <- mtcars
str(mtcars)

#a Create a pie chart
cyl.fr <- table(data$cyl)
label <- names(cyl.fr)
label <- paste("Cylinder",label,"-")
percentage <- round(cyl.fr/sum(cyl.fr)*100)
labelpercentage <- paste(label, percentage)
lablepercentage <- paste(labelpercentage, "%")
pie(cyl.fr, labelpercentage,main = "Propotion of Cars with Cylinder Values")

#b Create a bar graph that shows the number of each carb type in mtcars.
ggplot(data=mtcars,aes(x=carb))+
  geom_bar(stat="count")+
  ggtitle("Car Counts by Carb Type")+ 
  labs(x="Number of carbs", y= "Car Counts")

#c Stacked bar graph

ggplot(data=mtcars, aes(x = factor(cyl), fill = factor(gear))) +  
  xlab("Number of Cylinders") +
  ylab("Gear Type") +
  geom_bar()

#d Scatterplot between wt vs mpg

ggplot(data= mtcars, aes(x=wt, y=mpg)) + 
  ggtitle("Relationship between Weight & Mpg") + 
  labs(x= "Weight",y= "Miles per gallon")+
  geom_point()


#e Scatterplot between horsepower and mileage 

ggplot(data=mtcars, aes(x=hp, y=mpg))+ 
  geom_point()+
  geom_smooth(method='lm')+
  labs(x='horsepower', y='mileage') +
  ggtitle("Relationship between horsepower and mileage")

##Problem 2
install.packages('tidyverse')
library(tidyverse)
install.packages('babynames')
library(babynames)
install.packages('dplyr')
library(dplyr)

#a Create a graph showing the popularity of these four names from 1880 to 2014.
#Use the filter() function to select four girl baby names - Alexandra, Beverly, Emily, and Kathleen
d <- babynames %>% 
  filter(name %in% c("Alexandra","Beverly","Emily","Kathleen"), sex=="F")
#a create a graph
ggplot(data=d,aes(x = year,y = prop, color=name))+
  geom_line()  

#b Use the select() function to only select the columns year, sex, name and prop.
select(.data= babynames, year, sex, name, prop)

#c Sort all data points first based on the variable “year”, then based on “sex” in a descending order.
arrange(babynames, desc(year), desc(sex))
        
#d Select a subset of baby names that are NOT in the set “Mary Elizabeth, and Victoria.”
filter(babynames, !(name %in% c("Mary","Elizabeth","Victoria")))

#e For each name in this dataset, summarize the total number of babies who have the same name.
group_babies <- group_by(.data= babynames,name)
summarise(.data=group_babies, total=sum(n))

#f. Add two new columns that respectively contain the mean and median number of babies given each name every year.
summarise(group_babies,mean_number = mean(n), median_number = median(n))

##Problem 3

#a Write a function that receives a temperature in Fahrenheit and converts it into Celsius.
f2c <- function(fahr) {
  Celsius <- (fahr - 32) * 5 / 9
  return(Celsius)
}
#Example f2c(98)
#b Write a function that receives a vector “data” and a midpoint “m” and centers the data around m.
center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

#c
x <- as.numeric()
dif <- function(x) {
  diff <- max(x) - min(x)
  return(diff)
}

##Problem 4

library(MASS)
library(plyr)
library(dplyr)
library(tibble)
library(ggplot2)

p4data <- birthwt
#a converts all the integer variables to factors (categorical).
p4data_factor <- birthwt %>%
  mutate_if(is.integer, as.factor)

#b Repeat part (a) using mutate() and mapvalues() functions.
p4data_factor <- birthwt %>%
  mutate(smoke = mapvalues(smoke, from = c(0,1), to = c("No", "Yes")),
         race = mapvalues(race, from = c(1,2,3), to = c("White", "Black", "Other")))

#c
p4data$race <- as.factor(p4data$race)
p4data$smoke <- as.factor(p4data$smoke)
output <- tapply(X=p4data$bwt,INDEX = list(p4data$race, p4data$smoke),FUN = mean)
output
#the output shows that smoking status does have a negative impact on birth weight regardless of the race of the mother. if the mother smokes, the infant weighs lighter than average.
#The loss of birth weight due to smoking is consistent among races 
#The mothers whose race is white seem to have heavier babies than any other races. So while controlling for smoking during pregnancy, white mothers seem to have bigger babies in terms of weight.
#d
kable(output)

#e
p4data_ddply <- ddply(p4data, .(race, smoke), summarise, mean.bwt = mean(bwt))
tapply(p4data$bwt, list(p4data$race, p4data$smoke), mean)

#f
ggplot(p4data_ddply, aes(x=race, y=mean.bwt, fill=race)) +
  geom_bar(stat="identity")+
  ggtitle("Average Birthweight for each group race")


#g
p4data_ddply <- ddply(p4data, .(smoke), summarise, mean.bwt = mean(bwt),
                       low_prob_bwt = sum(low)/sum(count(low)))
p4data_ddply

#h
#since there is no "mother smoke" variable in the dataset, I'm unable to add the column.
#i
cor(p4data$age, p4data$bwt)
# There is a positive relationship between age and birth weight, however the relationship is quite low (0.09031781).
cor(p4data$age[p4data$smoke==0], p4data$bwt[p4data$smoke==0])

cor(p4data$age[p4data$smoke==1], p4data$bwt[p4data$smoke==1])
# The correlation does vary (not substantially) with smoking status.It seems birthweight and smoking status have very little correlation with age. 

