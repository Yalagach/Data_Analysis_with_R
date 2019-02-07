library(dplyr)
data <- read.csv("/home/naksh/Documents/DA/asst1/athlete_events.csv")

# Problem Statement 1
retval <- subset( data, Team=="India")
print(retval)

win<-subset(retval$Name, retval$Medal!="NA")
print(unique(win))

new<-subset(retval,Medal!="NA" & Year>=1960)
sum<-distinct(new, Year ,Sport)
print(count(sum)) 

all<-subset(retval,Medal!="NA")
if(FALSE){
med<-distinct(all,Year,Sport)
all_med<-(count(med,Sport))
print(all_med)
high<-max(all_med$n)
print(high)
s<-subset(all_med$Sport,n==high)
print(s)}

medalwin<-subset(retval,Medal!="NA")
print(tail(names(sort(table(all$Sport))),n=1)) 


# Problem Statement 2
part<-distinct(data,Games,Name)
part<-count(part,Name)
part<-subset(part,part$n>1)
print(part$Name)

part<-distinct(data,Year,Name)
part<-count(part,Name)
part<-subset(part,part$n>1)
print(part$Name)

medalist<- subset( data,Medal!="NA")
part<-distinct(medalist,Year,Name)
part<-count(part,Name)
part<-subset(part,part$n>1)
print(part$Name)


part1<-count(medalist,Name,Year)
high<-max(part1$n)
part1<-(subset(part1,part1$n==high))
print(part1)

# Problem Statement 3
male<-subset(data,Sex=="M")
male<-distinct(male,Name,Year,Sex,Team)
maleno<-count(male,Team,Year)
print(maleno)

female<-subset(data,Sex=="F")
female<-distinct(female,Name,Year,Sex,Team)
femaleno<-count(female,Team,Year)
print(femaleno)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]

print("Male")
print(mean(maleno$n, na.rm = TRUE))
print(median(maleno$n, na.rm = TRUE))
print(getmode(maleno$n))
print(IQR(maleno$n))
print(sd(maleno$n))
print(quantile(maleno$n, c(.90)))
print(" ")
print("Female")
print(mean(femaleno$n, na.rm = TRUE))
print(median(femaleno$n, na.rm = TRUE))
print(getmode(femaleno$n))
print(IQR(femaleno$n))
print(sd(femaleno$n))
print(quantile(femaleno$n, c(.90)) )

boxplot(femaleno$n,maleno$n,names=c("Females","Males"),ylab="No of people",
	main="Number of Males and Females per country per edition")

# Problem Statement 4
medal_winners<-subset(retval, retval$Medal!="NA")
year_medal_name <- subset(medal_winners, select=c(Year,Medal,Name))
medalists <- summarize(group_by(year_medal_name, Year), medalist_count = n_distinct(Name))
barplot(medalists$medalist_count,names=medalists$Year,
	main= "Indian Medal Winners against Year", xlab="year", ylab = "medal_winners",col = "yellow")












 












