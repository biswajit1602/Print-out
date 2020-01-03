install.packages("plyr")
library(plyr)

x <-  runif(50) # generating 50 random numbers
x

y <-  runif(50) # generating 50 random numbers 
y

data <- data.frame(cbind(x,y) )
View(data)

plot(data)

plot(data, type="n")
text(data, rownames(data))

km <- kmeans(data,4) #kmeans clustering
str(km)

# install.packages(ggplot2)
# Please explore ggplot2 package which is widely used for getting
# Custom Visualizations

library(ggplot2)
# Creating new column in a data frame
data["groups"] <- NULL
data$groups <- km$cluster
qplot(x=data$x,y=data$y,color=data$groups)

plot(data$x,data$y,col=data$groups,lwd=3)
# install.packages("animation")
# library(animation)
# 
# km <- kmeans.ani(data, 6)
# km$centers

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# Creating a empty variable to store total within sum of sqares of clusters
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normalized_data,i)$tot.withinss)
}

twss
plot(2:15,twss,type="o")

# From the elbow curve we choose 6 as number of cluseters or 5 

# Other method to choose to get optimum number of clusters

# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
?kselection

# To implement the parallel processing in getting the optimum number of
# clusters using kselection method 
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k
library(readxl)
mydata<-read_excel(file.choose(),1)
str(mydata)
normalized_data<-scale(mydata[,3:8])
fit <- kmeans(normalized_data, 5) # 5 cluster solution
final2<- data.frame(mydata, fit$cluster) # append cluster membership
final2
aggregate(mydata[,3:8], by=list(fit$cluster), FUN=mean)

# # k clustering alternative for large dataset - Clustering Large Applications (Clara)
# install.packages("cluster")
# library(cluster)
# xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
# xcl <- clara(xds, 2, sample = 100)
# clusplot_model <- clusplot(xcl)
# str(xcl)
# 
# #Partitioning around medoids
# xpm <- pam(xds, 2)
# clusplot(xpm)
