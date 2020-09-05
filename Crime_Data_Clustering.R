########  Understanding the data   ###########

crime_data<- fread("C:/Users/home/Desktop/Data Science Assignments/Clustering/crime_data.csv")
?fread


ncol(crime_data)#no of columns in the data set
?ncol
nrow(crime_data)#no. of rows in the dataset

summary(crime_data)

#########   Data preparation   #########

crime_data_01 <- crime_data[,2:5]##excluding the location or area column





install.packages("plyr")
library(plyr)

?aggregate

install.packages("VIM")
library(VIM)

aggr(crime_data_01)###### No missing values,to find any missing values in the dataset

plot(crime_data_01)#excludng the x values of the matrix

##kmeans clustering##

set.seed(1)
#clustering02 <- kmeans(crime_data_01[,0:4],5)

clustering03<- kmeans(crime_data_01,4)# 4- clusters

class(clustering03)

#######crime_data_01$Murder<-as.factor(clustering03$cluster)



?kmeans.ani

clustering03$centers



clustering04 <- kmeans(crime_data_01,8)#8 clusters


#wss<-(nrow(crime_data_01)-1)*sum(apply(crime_data_01,2,var))
#for(i in 2:15) wss[i]<-sum(kmeans(crime_data_01,centers = i)$withinss)
#plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "distortion")

?apply#Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix.

kmeans.wss.k<-function(crime_data_01,k)
{
  km=kmeans(crime_data_01,k)
  return(km$tot.withiness)
}

kmeans.wss.k(crime_data_01,5)### for value of k=5 puting in the function
kmeans.wss.k(crime_data_01,10)#putting value k=10 to check the difference
#vlaue of k increases distortion increases
kmeans.dis <- function(crime_data_01, maxk){
  dis=(nrow(crime_data_01)-1)*sum(apply(crime_data_01,2,var))
  dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, crime_data_01=crime_data_01)
  return(dis)
}
maxk = 10
dis = kmeans.dis(crime_data_01 , maxk);
plot(1:maxk, dis, type='b', xlab="Number of Clusters",ylab="Distortion",
     col="blue")

install.packages("animation")
library(animation)

AB<- kmeans.ani(crime_data_01,5)
AB$centers

######################## normalize the data#####################

norm_crime_data_01<-scale(crime_data_01[1:50,])

###Hclustering

d <- dist(norm_crime_data_01, method = "euclidean") # distance matrix
?dist

fit <- hclust(d, method="complete")
?hclust
plot(fit) # displaying dendrogram, a Hclustering ("hierarchical clustering")

plot(fit, hang=-1)


?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust
groups <- cutree(fit, k=3) # cut tree into 3 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data_01, membership)

View(final)

