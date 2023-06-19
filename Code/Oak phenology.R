#Read in bud burst score data
leaf<-read.csv("~/Downloads/oaks-data-2022/BudBurst2007_2021.csv")
class(leaf$Date)
library(dplyr)

# Extract month values in a new column
leaf <- leaf %>%
  mutate(Month = as.numeric(format(as.Date(Date, format = "%d/%m/%Y"), "%m")))

# Print the updated dataset
print(leaf)

# Add year values to a new column for 2010-2021
start_year <- 2010
end_year <- 2021
leaf <- leaf %>%
  mutate(Year = as.numeric(format(as.Date(Date, format = "%d/%m/%Y"), "%Y"))) %>%
  filter(Year >= start_year & Year <= end_year)

# Subset data for a specific month for 12 years
desired_month <- 4  # April

April<-subset(leaf,Month==desired_month)
April
year_range<-subset(April,Year>=2010&Year<=2021)
year_range
write.csv(year_range,"~/Downloads/buds.csv")

# Data processing, remove "NAs" and ">"
new<-read.csv("~/Downloads/buds.csv")
new
new$Score<-na.omit(new$Score)
new$Score<-gsub(">","",new$Score)

# Write the selected bud burst data in a new CSV file
write.csv(new,"~/Downloads/Bud_bursts.csv")

# Print bud burst scores mean from 2010-2021
year<-2010
for (i in 1:12){
  particular_year<-subset(new,Year==year)
  year_mean<-mean(as.numeric(particular_year$Score),na.rm=TRUE)
  print(year_mean)
  year=year+1
}

# Read in air temperature data
temp<-read.csv("~/Downloads/oaks-data-2022/SilwoodWeatherDaily_2009-2022.csv")
library(dplyr)

# Extract month values in a new column
temp <- temp %>%
  mutate(Month = as.numeric(format(as.Date(TIMESTAMP, format = "%d/%m/%Y"), "%m")))

# Print the updated dataset
print(temp)

# Add year values to a new column for 2010-2021
start_year <- 2010
end_year <- 2021
temp <- temp %>%
  mutate(Year = as.numeric(format(as.Date(TIMESTAMP, format = "%d/%m/%Y"), "%Y"))) %>%
  filter(Year >= start_year & Year <= end_year)

# Subset data for April for 12 years
desired_month <- 4  # April

Fourth_month<-subset(temp,Month==desired_month)
Fourth_month

# Write the selected air temperature data in a new CSV file
write.csv(Fourth_month,"~/Downloads/Temperature.csv")

# Data processing, remove "NAs"
temp_processing<-read.csv("~/Downloads/Temperature.csv")
temp_processing$Air_Temp...Deg.C...Smp.<-na.omit(temp_processing$Air_Temp...Deg.C...Smp.)

# Print mean air temperature for each year between 2010 and 2021
year=2010
for (i in 1:12){
  particular_year<-subset(temp_processing,Year==year)
  year_mean<-mean(as.numeric(particular_year$Air_Temp...Deg.C...Smp.),na.rm=TRUE)
  print(year_mean)
  year=year+1
}

# The mean bud burst scores and air temperatures are recorded in "Oak phenology data.csv"
# Plotting mean bud burst score variation every April from 2010-2021
plotting_2021<-read.csv("~/Downloads/Oak phenology data.csv")
plot(plotting_2021$Year,plotting_2021$Mean.leaf.bursts.score,xlab="Years",ylab="Mean bud burst scores",col="darkgreen",pch=4,cex.lab=1.5)
linear_2021<-lm(plotting_2021$Mean.leaf.bursts.score~plotting_2021$Year)
abline(linear_2021,col="darkgreen", lty="dashed")

# Plotting mean air temperature variation each April from 2010-2021
plotting_2021<-read.csv("~/Downloads/Oak phenology data.csv")
plot(plotting_2021$Year,plotting_2021$Mean.air.temperature....C,xlab="Years",ylab="Mean air temperatures (°C)",col="darkred",pch=4,cex.lab=1.5)
linear_2021<-lm(plotting_2021$Mean.air.temperature....C~plotting_2021$Year)
abline(linear_2021,col="darkred", lty="dashed")

# Plotting mean bud burst scores against mean air temperatures
plotting_2021<-read.csv("~/Downloads/Oak phenology data.csv")
str(plotting_2021)
labels<-plotting_2021$Year
plot(plotting_2021$Mean.air.temperature....C,plotting_2021$Mean.leaf.bursts.score,xlab="Mean air temperatures (°C)",ylab="Mean bud burst scores",pch=4,cex.lab=1.5)
text(plotting_2021$Mean.air.temperature....C,plotting_2021$Mean.leaf.bursts.score,labels, pos = 3)
linear_2021<-lm(plotting_2021$Mean.leaf.bursts.score~plotting_2021$Mean.air.temperature....C)
abline(linear_2021,lty="dashed")

# Plotting diganostic plots for the linear regression
par(mfrow = c(2, 2), mar = c(5, 5, 1.5, 1.5))
plot(linear_2021,pch=4)

# Summarise the linear regression 
summary(linear_2021)

# Pearson's correlation test
cor.test(plotting_2021$Mean.leaf.bursts.score,plotting_2021$Mean.air.temperature....C,use="pairwise")
