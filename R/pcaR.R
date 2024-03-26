#Using factoextra for PCA-Replicated from Statistics Globe          
library("factoextra")
library(cluster)
library(stats)

#pick a data set
data(package = .packages(all.available = TRUE))
data(CASchools, package = 'AER')

#x must be numeric
cas_num = CASchools[,12:14]
cas_num = scale(CASchools[,12:14]) #normalize
head(cas_num)

#change the data set
my_pca = stats::prcomp(cas_num, scale = TRUE)
summary(my_pca)

#number of clusters based on 3 numeric variables
my_pca_data = data.frame(my_pca$x[ , 1:3])
head(my_pca_data)

#optimal number of clusters
fviz_nbclust(my_pca_data,                    
             FUNcluster = stats::kmeans,
             method = "wss")

#cluster gap statistic
gap_stat <- clusGap(cas_num,
                    FUN = stats::kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)
fviz_gap_stat(gap_stat)

#perform kmeans clustering
set.seed(123)                             
my_kmeans = stats::kmeans(my_pca_data,             
                    centers = 3)
fviz_cluster(my_kmeans, data = cas_num)

#visualize pca
fviz_pca_ind(my_pca,                        
             habillage = my_kmeans$cluster,
             label = "none",
             addEllipses = TRUE)

#aggregate results
stats::aggregate(cas_num, by=list(cluster=my_kmeans$cluster), mean)
cas <- cbind(CASchools, cluster_subject = my_kmeans$cluster)
head(cas)

#save the new data set for further analysis
save(cas, file = "CASchools.Rdata")
saveRDS(cas, file = "CASchools.Rds")
write.table(cas, file = "CASchools.csv",
            sep = ",", row.names = F)
#load data
load(file = "CASchools.Rdata")
CASchools = readRDS(file = "CASchools.Rds")

#Cons
#sensitive to outliers
#Clusters must be specified before analysis
