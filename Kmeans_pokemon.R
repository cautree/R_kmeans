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

#hclust

# View column means
colMeans(pokemon)
# View column standard deviations
apply(pokemon,2,sd)

# Scale the data
pokemon.scaled = scale(pokemon)

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon = hclust(dist(pokemon.scaled), method="complete")
plot(hclust.pokemon)


#Looking at the table, it looks like the hierarchical clustering 
#model assigns most of the observations to cluster 1, 
#while the k-means algorithm distributes the observations relatively evenly 
#among all clusters. It's important to note that there's no consensus on 
#which method produces better clusters. 
#The job of the analyst in unsupervised clustering is 
#to observe the cluster assignments and make a judgment call as to which method provides more insights into the data
# Apply cutree() to hclust.pokemon: cut.pokemon
cut.pokemon=cutree(hclust.pokemon,k=3)

# Compare methods
table(cut.pokemon, km.out$cluster)

#add PCA
# Perform scaled PCA: pr.out
pr.out = prcomp(pokemon, scale=TRUE)

# Inspect model output
summary(pr.out)
dim(pr.out$x)
dim(pokemon)

dim(pr.out$rotation)
pr.out$scale
colMeans(pokemon)

biplot(pr.out)
