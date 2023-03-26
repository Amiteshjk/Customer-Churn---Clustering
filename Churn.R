library(cluster)
library(readxl)
library(tidyverse)
setwd("/Users/amiteshjk/Desktop/JGBS/Term 3/Predictive Analytics/Datasets/")

churn = read_excel("churn1.xlsx")
View(churn)
library(outliers)
chur=churn|>select(-1,-4)
s=scores(chur,type="iqr",lim="iqr")
table(s)

# perform normalization

library(caret)
process <- preProcess(as.data.frame(churn), method=c("range"))

norm<- predict(process, as.data.frame(churn))

churn_data <- norm |> select(2,5,6,7,8,11,14,17)

#scree plot: number of clusters
library(factoextra)

fviz_nbclust(churn_data,kmeans, method="wss")
# K Mean

km_churn = kmeans(churn_data, centers = 3)
km_churn


#biplot
fviz_cluster((kmeans(churn_data, centers = 3)),churn_data, labelsize = 6)


#add cluster to churn data
churn1=churn%>%mutate(clusters=km_churn$cluster)
churn1

view(churn1)
setwd("/Users/amiteshjk/Desktop/")
write.csv(churn1)
write_excel_csv(churn1,"churn.csv")
#write file on hard disk
#library(xlsx)


#pie chart w.r.t international call
table1=table(churn1$clusters,churn1$`Int'l Plan`)
table1

library(graphics)
labels=c("no", "yes")
par(mfrow = c(1,3))
for (i in 1:nrow(table1)) {
  

  pie(table1[i,],labels,col=c("red", "green"), main=paste("Cluster", i))
  
}

#pie chart w.r.t voice calls
table2=table(churn1$clusters,churn1$`VMail Plan`,dnn=c("Cluster", "Vmail Plan"))
table2

par(mfrow = c(1,3))


for (i in 1:nrow(table2)) {
  
  
  pie(table2[i,], labels,col=c("red", "green"), main=paste("Cluster", i))
  
}

