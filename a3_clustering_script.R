library(readr)
library(cluster)
library(ggplot2)
library(dendextend)
library(dplyr)
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

# Hierarchical