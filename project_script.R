# link to data
# https://www.kaggle.com/blitzr/gfp2017

# Change path to wherever the .csv file is located on other computers
# Importing the data from .csv file by 'File -> Import Dataset -> From Text' changes the numerical data into categorical
global_firepower = 
  read.csv(file="C:\\Users\\Asus\\Desktop\\Programming\\R_project_62151\\GlobalFirePower.csv", header=TRUE, sep=",")

global_firepower_by_population = global_firepower[,c(1,4,8,9)]
rm(global_firepower)

colnames(global_firepower_by_population) = c("Country","Total_Population",
                                       "Total_Military_Personnel","Active_Personnel")

# making easier to read variables
countries=global_firepower_by_population$Country

population= global_firepower_by_population$Total_Population
population_log=log(population)

total_personnel=global_firepower_by_population$Total_Military_Personnel
total_personnel_log=log(total_personnel)

active_personnel=global_firepower_by_population$Active_Personnel
active_personnel_log=log(active_personnel)

#histograms 
hist(population,xlab  = "Total population",main = "Total population")
hist(population_log,xlab  = "Log of total population",main = "Total population")
hist(total_personnel_log,xlab = "Log of total military personnel",main = "Total military personnel")
hist(active_personnel_log,xlab = "Log of active military personnel",main = "Active military personnel")

#function to optimise code

func_stats= function(a)
{
  stats=NULL
  
  stats$median=median(a)
  stats$mean=mean(a)
  stats$sd=sd(a)
  
  stats
}

#population analysis

summary(population)
summary(population_log)

func_stats(population)
func_stats(population_log)

#total military personnel analysis

summary(total_personnel)
summary(total_personnel_log)

func_stats(total_personnel)
func_stats(total_personnel_log)

#active military personnel analysis

summary(active_personnel)
summary(active_personnel_log)

func_stats(active_personnel)
func_stats(active_personnel_log)

#link between countries and population
boxplot(population/10 ~ countries,xlab = "Countries",ylab = "Population divided by 10")
boxplot(population_log ~ countries,xlab = "Countries",ylab = "Population")

#link between countries and total military personnel
boxplot(total_personnel ~ countries,xlab = "Countries",ylab = "Total military personnel")
boxplot(total_personnel_log ~ countries,xlab = "Countries",ylab = "Total military personnel")

#link between population and total military personnel
qqplot(population,total_personnel,xlab = "Population",ylab = "Total military personnel")
abline(lm(total_personnel~population))

qqplot(population_log,total_personnel_log,xlab = "Population",ylab = "Total military personnel")
abline(lm(total_personnel_log~population_log))

cor(population,total_personnel,method = "spearman")
cor(population_log,total_personnel_log,method = "pearson")

#link between total military personnel and active military personnel
qqplot(total_personnel,active_personnel,xlab = "Total military personnel",ylab = "Active military personnel")
abline(lm(active_personnel~total_personnel))

qqplot(total_personnel_log,active_personnel_log,xlab = "Total military personnel",ylab = "Active military personnel")
abline(lm(active_personnel_log~total_personnel_log))

cor(total_personnel,active_personnel,method = "spearman")
cor(total_personnel_log,active_personnel_log,method = "pearson")
