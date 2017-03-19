#Kmeans on Pokemon dataset
# Initialize total within sum of squares error: wss
getwd()
setwd("/home/yanyan/R_essential/R-Programming")
list.files()
pokemon = read.csv("Pokemon.csv")
wss <- 0
head(pokemon)
names(pokemon)
pokemon=pokemon[,5:11]
pokemon=pokemon[complete.cases(pokemon[,5:6]),]
dim(pokemon)
colMeans(pokemon)

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
  print (wss[i])
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 4

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = 4, nstart = 1, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")