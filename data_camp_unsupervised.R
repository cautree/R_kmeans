#unsupervised learning

data(iris)
x=iris[,1:4]
names(iris)
colMeans(x)

# Create the k-means model: km.out
km.out = kmeans(x, centers=3, nstart =20)

# Inspect the result
summary(km.out)

# Print the cluster membership component of the model
km.out$cluster


# Print the km.out object
km.out
xx=x[,1]
# Scatter plot of x
plot(xx, col = km.out$cluster,
     main = "k-means with 3 clusters", 
     xlab = "", ylab = "")

# Set up 2 x 3 plotting grid
par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
  # Run kmeans() on x with three clusters and one start
  km.out <- kmeans(x,center=3,nstart=1)}
  
  # Plot clusters
  plot(xx, col =km.out$cluster, 
       main = km.out$tot.withinss, 
       xlab = "", ylab = "")
  
  
  
  # Initialize total within sum of squares error: wss
  wss <- 0
  
  # For 1 to 15 cluster centers
  for (i in 1:15) {
    km.out <- kmeans(x, centers = i, nstart = 20)
    # Save total within sum of squares to wss variable
    wss[i] <- km.out$tot.withinss
  }
  
  # Plot total within sum of squares vs. number of clusters
  plot(1:15, wss, type = "b", 
       xlab = "Number of Clusters", 
       ylab = "Within groups sum of squares")
  
  # Set k equal to the number of clusters corresponding to the elbow location
  k <- 2  # 3 is probably OK, too
  
  
  #h cluster
  
?hclust
data("iris")
x = iris[,1:2]
iris_h =hclust(dist(x))
# methods says how the distance between different clusters are calculated
#complete and average method produce more balanced trees
#iris_h =hclust(dist(x),method="complete")
#iris_h =hclust(dist(x),method="average")
#iris_h =hclust(dist(x),method ="single")
summary(iris_h)
  
#dengrogram plot in R
plot(iris_h)
abline(h=2,col="red")  # abline to cut

#tree cutting in R by height
cutree(iris_h, h=2)

#cut by the number of cluster
cutree(iris_h, k=3)

# scaling is important before doing clustering
# checking if need to scale
colMeans(iris[,1:4])
apply(iris[,1:4], 2, sd)  # check the standard deviation of each row


#produce matrix of mean is 1 and sd is 0
scaled_iris = scale(iris[,1:4])
#check again the mean and standard deviation
colMeans(scaled_iris)
apply(scaled_iris,2,sd)

####################################
#unsupervised project with breast cancer
# Download the data: wisc.df
wisc.df = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv")

# Convert the features of the data: wisc.data
wisc.data = as.matrix(wisc.df[,3:32])

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")
dim(wisc.data)[1]

dim(wisc.data[,grep("_mean", colnames(wisc.data)) ])[2]
names(wisc.df)


dim(subset(wisc.data, diagnosis==1))

# Check column means and standard deviations
colMeans(wisc.data)

apply(wisc.data, 2,sd)

colnames(wisc.data)

# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr = prcomp(wisc.data, scale=TRUE, center=TRUE)


# Look at summary of results
summary(wisc.pr)

# Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[,c(1,3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var = wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve = pr.var/sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cummulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

wisc.pr$rotation[,1:3]

# Scale the wisc.data data: data.scaled
data.scaled = scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist= dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust = hclust(data.dist, method = "complete")

# find at which hight there is 4 cluster
plot(wisc.hclust)

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters = cutree(wisc.hclust, k=4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters,diagnosis)

# Create a k-means model on wisc.data: wisc.km
wisc.km =kmeans(wisc.data, centers=2, nstart=20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)

# Create a k-means model on wisc.data: wisc.km
#notice first scale the data
wisc.km =kmeans(scale(wisc.data), centers=2, nstart=20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)

# Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters= cutree(wisc.pr.hclust, k=4)

# Compare to actual diagnoses
table(wisc.pr.hclust.clusters, diagnosis)

# Compare to k-means and hierarchical
table(wisc.hclust.clusters,diagnosis)
table(wisc.km$cluster,diagnosis)
