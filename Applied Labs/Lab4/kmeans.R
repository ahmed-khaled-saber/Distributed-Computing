install.packages("cluster")        
library(cluster)           
setwd("D:/BigData/Labs/Lab4")
newiris=read.csv('iris.csv') 
newiris$variety <- NULL  

###################################################
#  Execute the Model
###################################################


#Take a look at the k-means function documentation
help(kmeans)

#Fit the k-means cluster with 3 initial cluster centers
km <- kmeans (newiris,3)

###################################################
# Review the Output
###################################################
km
km$center

###################################################
# Plot the Results
###################################################
plot(newiris$sepal.length,newiris$sepal.width, col = km$cluster)

#Plot centers
points(km$centers, col = 1:3, pch = 8)

###################################################
# Find the Appropriate Number of Clusters
###################################################

#Plot the within-group-sum of squares and 
#look for an "elbow" of the plot. The elbow 
#(if you can find one) tells you what the 
#appropriate number of clusters probably is.
wss <- numeric(15) 
for (i in 1:15) wss[i] <- sum(kmeans(newiris, 
     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares")

