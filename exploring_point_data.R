library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(reshape)


#### Identify outliers ####


LSMS_data<-read.csv("data/LSMS/Farm sizes/LSMS_Tanzania_landsizes.csv")


TRUE_in_Vector<-function(vector){return (TRUE %in% vector)}
identify_outlier_rows_and_NAs<-function(column){
  
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




# read in data
data<-read.csv("data/processed/point_data_processed.csv", na.strings = c("NaN", "n/a"))
AEZ_categories<-read.csv("data/AEZ_Categories/AEZ_categories.csv", na.strings = c("NaN", "n/a"))
AEZ_categories$X<-NULL

# Converting Numerical AEZ categories to Names
data<-merge(data, AEZ_categories, by.x="raster_level_AEZ", by.y="AEZ_Numbers")
most_important_variables<-read.csv("exploratory_outputs/LandCultivated_50_Most_important_RFE.csv")
colnames(data)<-gsub("AEZ_Names","raster_level_AEZ_names", colnames(data))

write.csv(data,"data/processed/point_data_processed.csv")

data$raster_level_AEZ_names

plot(density(data$LandCultivated, na.rm = T)) #plotting whole column with outliers
plot(density(data$LandCultivated[identify_outlier_rows_and_NAs(data$LandCultivated)==FALSE], na.rm = T)) #plotting column without outliers


# Examining the data set
dim(data)
str(data)
head(data)
colSums(sapply(data, is.na))



correlationMatrix <- cor(data)

correlationMatrix[,highlyCorrelated]


variable_to_predict<-"LandCultivated"

all_predictive_variables<-c(grep("raster_level_", colnames(data)),grep("COUNTRY_LEVEL", colnames(data)))


#X=data[,colnames(data)%in%most_important_variables$variable[1:20]]
X=data[,all_predictive_variables]
Y=data.frame(data[,colnames(data)==variable_to_predict])
outliers<-identify_outlier_rows_and_NAs(Y[,1])

X<-X[outliers==F,]
Y<-data.frame(Y[outliers==F,])
colnames(Y)<-c(variable_to_predict)


lapply(X, function(x) sum((is.na(x))))

data_types<-data.frame("data_class"=sapply(X, class))
table(data_types$data_class)
non_factor_columns<-data_types$data_class!="factor"
X<-X[,non_factor_columns]

melt(data.frame(lapply(X, function(x) var(x))))

# Find std of the column. If there is no standard deviation then it will not be useful as a predictor


X_Y<-cbind(X,Y)



checking_for_na_values<-melt(data.frame(lapply(X_Y, function(x) sum((is.na(x))))))
checking_for_na_values[checking_for_na_values$value>0,]
#all variables are non-na


checking_for_na_values<-melt(data.frame(lapply(correlationMatrix, function(x) sum((is.na(x))))))
checking_for_na_values[checking_for_na_values$value>0,]

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

cor_x_y<-cor_x_y[,colnames(cor_x_y)==variable_to_predict]

pairs(X) # can produce a pairplot to look at correlations between values


outliers<-



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
