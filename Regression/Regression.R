library(dplyr)
library(plotly)
library(zoo)
library(Metrics)
library(caret)
library(stringr)
library(caret)

# Problem Statement 1
data<-data.frame(read.csv("tips_tailored.csv"))
orig<-data.frame(read.csv("tips.csv"))

data1 <- data2 <- data3 <- data4 <-data
data1[is.na(data1[,9]), 9] <- mean(data1[,9], na.rm = TRUE)
x1<- data[is.na(data[,9]),1]


data2[is.na(data2[,9]), 9] <- median(data2[,9], na.rm = TRUE)

data3$Odds[is.na(data3$Odds)] <- with(data3, ave(Odds,Horse,FUN = function(x) mean(x, na.rm = TRUE)))[is.na(data3$Odds)]
data3$Odds[is.na(data3[,9])]<-0

data4$Odds<-na.approx(data4$Odds)


print(rmse(orig$Odds, data1$Odds))
print(rmse(orig$Odds, data2$Odds))
print(rmse(orig$Odds, data3$Odds))
print(rmse(orig$Odds, data4$Odds))
p <- plot_ly(orig, x = ~c(1:8), y =~orig[x1,8], name = 'original', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~data1[x1 ,9], name = 'Mean', mode = 'lines')  %>%
  add_trace(y = ~data2[x1 ,9], name = 'Median', mode = 'lines')  %>%
  add_trace(y = ~data3[x1 ,9], name = 'Mean_Horse', mode = 'lines')  %>%
  add_trace(y = ~data4[x1 ,9], name = 'Imputed', mode = 'lines')
p

# Problem Statement 2
mydata <- read.csv("tips_tailored.csv",TRUE,",")
tp <- subset(mydata, mydata$Result=="Win" & mydata$Predicted.Results=="Win")
fn <- subset(mydata, mydata$Result=="Win" & mydata$Predicted.Results=="Lose")
fp <- subset(mydata, mydata$Result=="Lose" & mydata$Predicted.Results=="Win")
tn <- subset(mydata, mydata$Result=="Lose" & mydata$Predicted.Results=="Lose")

a<-confusionMatrix(factor(mydata$Predicted.Results),factor(mydata$Result),positive = "Win")
# extracting only the confusion matrix
a$table

#calculating the metrics
accuracy1 <- (nrow(tp)+nrow(tn))/(nrow(tp)+nrow(fn)+nrow(fp)+nrow(tn))
accuracy1
precision1 <- (nrow(tp))/(nrow(tp)+nrow(fp))
precision1
recall1 <- (nrow(tp))/(nrow(tp)+nrow(fn))
recall1
misclassification_rate1 <- (nrow(fp)+nrow(fn))/(nrow(tp)+nrow(fn)+nrow(fp)+nrow(tn))
misclassification_rate1
f1_score <- (2* recall1 * precision1)/(recall1 + precision1)
f1_score

# calculating the f-beta scores for 2 and half
b <- 2
f_score_2 <- ((1+(2^2))* nrow(tp))/(((1+(2^2))* nrow(tp)) +((2^2)*nrow(fn))+nrow(fp))
f_score_2
b <- 0.5
f_score_half <- ((1+(0.5^2))* nrow(tp))/(((1+(0.5^2))* nrow(tp)) +((0.5^2)*nrow(fn))+nrow(fp))
F_score_half

# Problem Statement 3
df_15<-data.frame(read.csv("Data2015.csv"))
df_16<-data.frame(read.csv("Data2016.csv"))
df_17<-data.frame(read.csv("Data2017.csv"))

df<-rbind(df_15,df_16)
df<-rbind(df,df_17)

x<-count(df,Country)
x<-(subset(x$Country,x$n>=3))

test<-subset(df,Country %in% x & year==2017 )
print(nrow(test))
train<-subset(df,year!=2017 & Country %in% x)
print(nrow(train))

model1<- lm(formula=Happiness.Score~Economy..GDP.per.Capita.+Family+year+Health..Life.Expectancy.,data=train)
predmodel1<-predict(model1,test)
errormodel1<-test$Happiness.Score - predmodel1
rmse_usingmodel1<-sqrt(mean((errormodel1)^2))
summary(model1)

model2<- lm(formula=Happiness.Score~Economy..GDP.per.Capita.+year+Health..Life.Expectancy.,data=train)
predmodel2<-predict(model2,test)
errormodel2<-test$Happiness.Score - predmodel2
rmse_usingmodel2<-sqrt(mean((errormodel2)^2))
summary(model2)


# Problem Statement 4
tab<-(read.csv("School_Data.csv"))

tab[1:10]<-lapply(tab[1:10],as.numeric)
table(tab$Pass)
t1<- factor(tab$Pass, levels = c(0, 1))
tabup<-upSample(x=tab[,c("Day1","Day2","Day3","Day4","Day5",
                           "Senior","Class_Prefect","Athlete",
                           "popularity")],y=tab$Pass)

copyup<-upSample(x=copy[,c("Day1","Day2","Day3","Day4","Day5",
                           "Senior","Class_Prefect","Athlete",
                           "popularity")],y=copy$Pass) 

logitmodel1<-glm(formula=Pass~(Day1+Day2+Day3+Day4+Day5+Senior+Class_Prefect+Athlete),family = binomial(link="logit"),data=tabup[,c("Day1","Day2","Day3","Day4","Day5",
                           "Senior","Class_Prefect","Athlete",
                           "popularity")])
logitmodel1$aic

logitmodel2<-glm(formula=Pass~(Day1+Day3+Day4+Day5+Athlete+popularity+Pass),family = binomial(link="logit"),data=tabup[,c("Day1","Day3","Day4","Day5","Athlete",
	"popularity","Pass")])
logitmodel2$aic
