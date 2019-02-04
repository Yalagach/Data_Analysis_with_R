
# Problem Statement 1
first_data<-first_data%>%replace_with_na(replace = list(Value="Not Available"))
first_data<-transform(first_data,Year=substr(YYYYMM,1,4),Month=substr(YYYYMM,5,6))
first_data$date<-(paste(first_data$Year,first_data$Month,sep="-"))
first_data$date<-as.Date(paste(first_data$date,"-01",sep=""))
newd <- first_data[!(is.na(first_data$date)),]
newd$Value<-as.numeric(as.character(newd$Value))
p<-plot_ly(newd,type="scatter",mode="lines",x=~newd$date,y=~newd$Value,
           name=newd$Description)%>%
  layout(title="Rainfall data",xaxis=list(title='Date',
                    rangeslider=list(type="date")),yaxis=list(title='Rainfall'),
         updatemenus=list(
    list(
      x=0,y=1.3,
      buttons=list(
        list(method="restyle",
             args=list("visible",
                       list(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label='Arunachal'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label='Bihar'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label='Karnataka'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label='Kerala'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)),
             label='Madhya Pradesh'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
             label='Maharashtra'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)),
             label='Orissa'),
        
         list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
             label='TN'),
        list(method="restyle",
             args=list("visible",
                       list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)),
             label='WB'),
        list(method="restyle",
             args=list("visible",
                       list(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
             label='All')
        
      )
    )
  ))


# Problem Statement 2
d <- data.frame(read.csv("TNAgri.csv"))

#To find the top 5 crops grown in TN
final <- d %>% group_by(Crop) %>%  summarise(value = sum(Area, na.rm=TRUE))
top_5<- final[order(final$value,decreasing=T)[1:5],]
top_5_crops <- top_5$Crop
#for (i in top_5_crops) print(i)

#DataFrame having only top 5 crops
df <- subset(d, Crop %in% top_5_crops)

rice <- subset(df, Crop == top_5_crops[1])
rice_stats <- rice %>% group_by(Crop_Year) %>% summarise(value1 = sum(Area, na.rm=TRUE))

groundnut <- subset(df, Crop == top_5_crops[2])
groundnut_stats <- groundnut %>% group_by(Crop_Year) %>% summarise(value2 = sum(Area, na.rm=TRUE))

foodgrains <- subset(df, Crop == top_5_crops[3])
foodgrains_stats <- foodgrains %>% group_by(Crop_Year) %>% summarise(value3 = sum(Area, na.rm=TRUE))

sugarcane <- subset(df, Crop == top_5_crops[4])
sugarcane_stats <- sugarcane %>% group_by(Crop_Year) %>% summarise(value4 = sum(Area, na.rm=TRUE))

jowar <- subset(df, Crop == top_5_crops[5])
jowar_stats <- jowar %>% group_by(Crop_Year) %>% summarise(value5 = sum(Area, na.rm=TRUE))


foodgrains_stats <- rbind(foodgrains_stats,c(1999, NA), c(2000, NA),c(2001,NA) , c(2002, NA), c(2003, NA),c(2004, NA),c(2005, NA),
  c(2006, NA), c(2007, NA) ,c(2008, NA),c(2009, NA),c(2010, NA),c(2011, NA),c(2012, NA), c(2013, NA))
jowar_stats <- rbind(jowar_stats[ (1:13),],c(2010, NA),jowar_stats[ 14,],c(2012, NA),jowar_stats[15,])

dataset = data.frame(cbind(Year = groundnut_stats$Crop_Year, Value1 = rice_stats$value1, Value2 = groundnut_stats$value2, 
  Value3 = foodgrains_stats$value3, Value4 = sugarcane_stats$value4,  Value5 = jowar_stats$value5))
#print(rice_stats)
#print(groundnut_stats)
#print(foodgrains_stats)
#print(sugarcane_stats)
#print(jowar_stats)
#print(dataset)

p<- plot_ly(dataset, x = ~Year, y = ~Value1, name = 'Rice', type = 'scatter', mode = 'lines',
        line = list(color = 'rgb(255,0,0)', width = 2, dash='dot'), connectgaps = TRUE) %>%
  add_trace(y = ~Value2, name = 'Groundnut', line = list(color = 'rgb(0,255,0)', width = 2, dash = 'dot'), connectgaps = TRUE) %>%
  add_trace(y = ~Value3, name = 'Foodgrains', line = list(color = 'rgb(0,0,255)', width = 2, dash = 'dot'), connectgaps = TRUE) %>%
  add_trace(y = ~Value4, name = 'Sugarcane', line = list(color = 'rgb(255, 0 ,127)', width = 2, dash = 'dot'), connectgaps = TRUE) %>%
  add_trace(y = ~Value5, name = 'Jowar', line = list(color = 'rgb(0,0,0)', width = 2, dash = 'dot'), connectgaps = TRUE) %>%
  layout(title = "Tamil Nadu Agriculture",
         xaxis = list(title = "Crop_Year"),
         yaxis = list (title = "Area"))

# Problem Statement 3
  "importing necessary libraries"
library(ggmap)
library(tidyverse)
library(maptools)
library(plyr)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(rgeos)

"loading and saving the map"
map<-get_map(location='united states', zoom = 4, maptype = "terrain",
             source='google',color='color')
saveRDS(map, file = "usa_map.rds")
plot(map)
#print("MAP LOADED")

# Problem Statement 4
"importing necessary libraries"
library(ggmap)
library(tidyverse)
library(maptools)
library(plyr)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(SnowballC)
library(tm)
library(wordcloud)
library(corpus)
library(RColorBrewer)

"reading csv files"
dataUSA = read.csv("NewYearResolution15.csv", header = TRUE)
head(dataUSA,n = 1)
map = readRDS("usa_map.rds")

loc = dataUSA[c("tweet_state","tweet_id")]
loc = aggregate(tweet_id ~ tweet_state, loc, FUN = length)


for (i in 1:nrow(loc)) {
  print(loc[i,0])
  latlon = geocode(as.character(loc[i,0]))
  loc$lon[i] = as.numeric(latlon[1])
  loc$lat[i] = as.numeric(latlon[2])
}

circle_scale_amt = 0.03
ggmap(map) + geom_point(
             aes(x=lon, y=lat), data=loc, col="orange", alpha=0.4, 
             size=circle_scale_amt)

"most popular resolution category"
categories = count(dataUSA, "Resolution_Category")
categories = subset(dataUSA$Resolution_Category, 
                    count(dataUSA, "Resolution_Category")$freq > 10)

ggplot(data.frame(categories), 
       aes(x = categories)) + geom_bar()

"most used word"
dataUSATEXT = read.csv("NewYearResolution15.csv", 
                       stringsAsFactors = FALSE)
#df = do.call('rbind', lapply(dataUSATEXT$text, as.data.frame))

#docs = data.frame(dataUSATEXT)
docs = Corpus(VectorSource(dataUSATEXT$text))
#summary(docs)

print(docs[[1]]$content)
"replacing some other characters and numbers"
docs = tm_map(docs, tolower)
print(docs[[1]]$content)
"done"
docs = tm_map(docs, removeNumbers)
#docs = tm_map(docs, removeWords, stopwords('english'))
docs = tm_map(docs, removeWords, c("newyearresolution","newyearsresolut","year"))
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument)
print(docs[[1]]$content)
docs = tm_map(docs, PlainTextDocument)


"data preprocessing"

for(j in seq(docs)){
  docs[[j]] = gsub("/", "", docs[[j]])
  docs[[j]] = gsub("@", "", docs[[j]])
  docs[[j]] = gsub("#", "", docs[[j]])
}


wordcloud(docs, min.freq = 20, max.words = 300,
          random.color = TRUE, random.order = FALSE)



