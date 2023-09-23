library(readxl)
library(factoextra)

# Step 1: Data Preparation
dataset <- read_excel("D:/Kuliah/BINUS UNIVERSITY/SEMESTER 6/Multivariate Statistics/uas/data03.xlsx")
df3 = as.data.frame(dataset)
df3
str(df3)

#Labels
df3.labels = df3$PROVINSI
table(df3.labels)

# Value Unlabel
df3_value = df3[2:6]
df3_value

# Scale Data
data_scale = scale(df3_value)
data_scale

# Distance
df3_value = dist(data_scale)

# Calculate how many clusters you need
# within Sum Squares = wss
fviz_nbclust(data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

# Kmeans - clustering
km.out = kmeans(data_scale, centers = 3, nstart = 100)
print(km.out)

# Visualize the clustering algorithm results
km.clusters = km.out$cluster
rownames(data_scale) = paste(df3$PROVINSI, 1:dim(df3)[1], sep = "_")

fviz_cluster(list(data=data_scale, cluster = km.clusters))
table(km.clusters, df3$PROVINSI)
