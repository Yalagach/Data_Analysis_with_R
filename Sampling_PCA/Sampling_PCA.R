library(splitstackshape)
library(dplyr)
library(corrplot)
data<-data.frame(read.csv("kc_house_data.csv"))

# Problem Statement 1
sample_1<-sample_n(data,0.75*nrow(data),replace=TRUE)
print(sample_1) 
print(nrow(sample_1))
png(file = "simple.png")
hist(sample_1$price, main="Simple Random Sampling with 75% data",xlab = "price",col = "cyan",border = "black") # xlim = (min(d$price)max(d$price)), ylim = c(0,1),breaks = 5)

sample_2<-data[seq(1,nrow(data), by=4), ]
print(sample_2) 
print(nrow(sample_2))
png(file = "systematic.png")
hist(sample_2$price,main="systematic Sampling ",xlab = "price",col = "cyan",border = "black") 

x<-unique(data$bedrooms)
cluster<-sample(x,0.6*length(x),replace=TRUE) 

sample_3<-subset(data,bedrooms %in% cluster)
print((sample_3))
print(nrow(sample_3))  
png(file = "cluster.png")
hist(sample_3$price,main="Cluster Sampling ",xlab = "price",col = "cyan",border = "black") 

y<-unique(data$floors)
sample_4<-data.frame()
for (i in  y) {
strat<-subset(data,data$floors==i)
temp<-sample_n(strat,0.7*nrow(strat),replace=TRUE)
sample_4<-rbind(sample_4,temp)
}
print(sample_4)
print(nrow(sample_4))
png(file = "stratify.png")
hist(sample_4$price, main="Stratified Sampling ",xlab = "price",col = "cyan",border = "black")
  

# Problem Statement 2
corr<-data[,-2] 
res<-cor(corr, method = "pearson", use = "complete.obs")                                                                             
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45) 
cols_sd<-apply(corr, 2, sd)
print(cols_sd)
print( " ")
print(tail(sort(cols_sd), n = 5)) 
if(FALSE){
corr<-data[,-2]
pca = prcomp(corr,center = TRUE,retx = TRUE, scale. = TRUE)
print(pca)
eigs <- pca$sdev^2 
print(nrow(pca$x))
#  print(pca$sdev)  Prints sd of all pca's 
#  print(summary(pca1)) 
print(eigs[1] / sum(eigs))
print(eigs[2] / sum(eigs))    
print((eigs[1]+eigs[2])/sum(eigs)*100)
y<-unique(data$floors)
y<-select(data,floors)
strat<-stratified(data,c("floors"),.7) 
print(head(strat,n=10))  }





