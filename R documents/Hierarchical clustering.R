
data("USArrests")
Data <- USArrests
attach(Data)
?USArrests

# Is there any missing values?
any(is.na(Data))

# Normalizing data
library(dplyr)

myscale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

df <- Data %>% mutate_if(is.numeric, myscale)

# Distance matrix
distance <- dist(df, method = "euclidean")
head(distance)

# Hierarchical clustering using hclust
# Complete link
hcomplete <- hclust(distance, method = "complete")
plot(hcomplete, cex = 0.7, hang = -2, main = "Dendrogram for hclust - complete")

# Single link
hsingle <- hclust(distance, method = "single")
plot(hsingle, cex = 0.7, hang = -2, main = "Dendrogram for hclust - single")

# Cutting the dendrogram
clusters <- cutree(hcomplete, k =4)

# Size of each cluster
table(clusters)

# Looking at 4 clusters in dendrogram
plot(hcomplete, cex = 0.6)
rect.hclust(hcomplete, k =4, border = 2:6)

# Visualizing data
library(factoextra)
fviz_cluster(list(data = df, cluster = clusters))

# Hierarchical clustering using agnes
library(cluster)
hagnes <- agnes(df, method = "complete")

# Agglomerative coefficient
hagnes$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

library(purrr)
map_dbl(m, ac)


hagnesWard <- agnes(df, method = "ward")
pltree(hagnesWard , cex = 0.6, hang = -2, main = "Dendrogram of agnes") 


# compute divisive hierarchical clustering
hdiana <- diana(df)

# Divise coefficient; amount of clustering structure found
hdiana$dc

pltree(hdiana, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Cut agnes() tree into 4 groups
cutree(as.hclust(hagnesWard), k = 4)

# Compute 2 hierarchical clusterings
hc1 <- hclust(distance, method = "complete")
hc2 <- hclust(distance, method = "ward.D2")

library(dendextend)
# Create two dendrograms
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)

# Align and plot two dendrograms side by side
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()                       # Draw the two dendrograms

# Compute alignment quality. Lower value = good alignment quality
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement() 

dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% 
  tanglegram(
    highlight_distinct_edges = FALSE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE # Color common branches 
  )

fviz_nbclust(df, FUN = hcut, method = "wss")

fviz_nbclust(df, FUN = hcut, method = "silhouette")
