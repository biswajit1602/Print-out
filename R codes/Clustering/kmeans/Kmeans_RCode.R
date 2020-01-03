Uni <- read.csv(file.choose())

# Normalizing continuous columns to bring them under same scale
normalized_data<-scale(Uni[,2:7]) #excluding the university name columnbefore normalizing
View(normalized_data)
wss = NULL

k_3 <- kmeans(normalized_data,3)
str(k_3)

twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(normalized_data,i)$tot.withinss)
  
}

plot(2:15, twss,type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

# Creating a empty variable to store total within sum of sqares of clusters
twss <- NULL
for (i in 2:15){
  twss <- c(twss,kmeans(data,i)$tot.withinss)
}
twss
plot(2:15,twss,type="o")

# Choosing the best cluster as 5
k_5 <- kmeans(normalized_data,5)
Uni["Cluster"] <- k_5$clusters
aggregate(Uni[,-c(1,8)],by=list(Uni$cluster),mean)
