#Load all relevant libraries
library(readr)
library(cluster)
library(ggplot2)
library(dendextend)
library(dplyr)
library(NbClust)
library(kohonen)
library(class)
library(MASS)
ProductData <- read.csv("~/Documents/homework/ITM_6285/products2.csv", na.strings = "undefined",
                      colClasses = c("character", "character", rep("numeric", 48)))
#Note colClasses specifies the variable types - first 2 are character, the rest (48) are numberic
# Interpret undefines as NA's

# Delete first column
ProductData$prodid <-NULL

# Select Product Data Subset that has less than 25 NA's
ProdDataSubset <- ProductData[rowSums(is.na(ProductData))<25,]
summary(ProdDataSubset)


# Identify columns that are mostly empty (more than 1000 of 1688 rows are empty)
Enames <- sapply(ProductData, function(x) sum(is.na(x))<1000)
# Subset of 17 variables from above
Cd2 <- ProductData[,which(Enames)]

# Remove duplicates
CleanProductData <- ProdDataSubset %>% distinct(prodname, .keep_all = TRUE)

# Set NA's to zero
CleanProductData[is.na(CleanProductData)] <- 0

# Set productname as row names
rownames(CleanProductData) <- CleanProductData[,1]
CleanProductData$prodname <- NULL
summary(CleanProductData)

# Scale the data
ScaledProdData <- scale(CleanProductData)
ScaledProdData[is.na(ScaledProdData)] <- 0

# 1) Hierarchical clustering
#create a distance matrix
prodhieclusters <- agnes(ScaledProdData, method = "complete", metric = "euclidean")
?agnes

plot(prodhieclusters, which.plots=2, cex = 0.9)

# 2) k-means clustering
prodkclusters <- kmeans(ScaledProdData, 5, nstart = 25)
plot(ScaledProdData[,c(2,3)],  col = prodkclusters$cluster)

plot(prodkclusters$withinss)
prodkclusters$withinss
#shows the values within the centroids and each data point in a cluster summed. If clusters is 5,
#then there will be 5 values

NbClust(data = ScaledProdData, diss = NULL, distance = "euclidean", min.nc = 5, max.nc = 15, 
        method = "kmeans", index = "all", alphaBeale = 0.1)

NbClust(data = ScaledProdData, diss = NULL, distance = "euclidean", min.nc = 5, max.nc = 15, 
        method = "kmeans", index = "silhouette", alphaBeale = 0.1)
bestK <- NbClust(ScaledProdData, min.nc=5, max.nc=15, method="kmeans")


# 3) Kohonen SOM
kohsom <- som(data=ScaledProdData, grid = somgrid(5, 4, "hexagonal"))
#Here the hexagon will have a matrix 5 * 4 = 20 nodes. The nodes with similar signatures are 
#clustered together.

# Plot map
plot(kohsom, type="mapping", labels = rownames(ScaledProdData))

# Look at codes
plot(kohsom)
#related weights of input
