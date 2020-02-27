library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)



#### Identify outliers ####

TRUE_in_Vector<-function(vector){return (TRUE %in% vector)}
identify_outlier_rows<-function(column){
  
  column<-as.numeric(column)
  lower_quartile<-quantile(column,probs=0.25, na.rm=T)
  upper_quartile<-quantile(column,probs=0.75, na.rm=T)
  
  IQR<-upper_quartile-lower_quartile
  
  lower_outliers<-(column<(lower_quartile-1.5*IQR))
  upper_outliers<-(column>(upper_quartile+1.5*IQR))
  na_values<-is.na(column)
  
  indexing_data_frame<-data.frame(lower_outliers,upper_outliers,na_values)
  indexing_outliers<-apply(indexing_data_frame, MARGIN = 1,TRUE_in_Vector)

  return (indexing_outliers)
                                    

}


plot(density(data$LandCultivated, na.rm = T)) #plotting whole column with outliers
plot(density(data$LandCultivated[identify_outlier_rows(data$LandCultivated)==FALSE], na.rm = T)) #plotting column without outliers



data<-read.csv("data/processed/point_data_processed.csv", na.strings = c("NaN", "n/a"))
AEZ_categories<-read.csv("data/AEZ_Categories/AEZ_categories.csv", na.strings = c("NaN", "n/a"))
AEZ_categories$X<-NULL

data<-merge(data, AEZ_categories, by.x="raster_level_AEZ", by.y="AEZ_Numbers")
most_important_variables<-read.csv("exploratory_outputs/LandCultivated_50_Most_important_RFE.csv")

colnames(data)<-gsub("AEZ_Names","raster_level_AEZ_names", colnames(data))

dim(data)
str(data)
head(data)




colSums(sapply(data, is.na))

X=data[,data%>%colnames%in%most_important_variables$variable[1:20]]
Y=data$LandCultivated




density(y)

table(data$raster_level_AEZ_names)

#### Splitting into the training and test set ####

train_size<-floor(nrow(X)*0.8) #floor function takes the 'round down' integer
set.seed(123) #setting the seed means that people can reproduce this example see explanation: https://www.tutorialgateway.org/r-floor-function/
train_index <- sample(seq_len(nrow(mtcars)), size = smp_size) # seq_len is equivalent to colon but can handle empty sequences, see reverse sequence trap: http://www.win-vector.com/blog/2018/02/r-tip-use-seq_len-to-avoid-the-backwards-sequence-bug/

X_train <- X[train_index, ]
Y_train<-Y[train_index,]
X_test <- X[-train_ind, ]
Y_test <- Y[-train_ind, ]

### Exploring
correlationMatrix <- cor(X)
correlationMatrix

dim(X)
