library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(gptk)
library(rstan)


# following the following introduction to rstan:
# https://nbviewer.jupyter.org/github/QuantEcon/QuantEcon.notebooks/blob/master/IntroToStan_basics_workflow.ipynb

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### Identify outliers ####

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


X_Y<-data.frame("X1"=data$TVA_USD_PPP_pmae_pday, "X2"=data$LandCultivated,"X3"=data$HHsizeMAE,"Y"=data$Livestock_Orientation)

sum(identify_outlier_rows_and_NAs(X_Y$X)) #number of outliers

X_Y<-X_Y[identify_outlier_rows_and_NAs(X_Y$X1)==F,]
X_Y<-X_Y[identify_outlier_rows_and_NAs(X_Y$X2)==F,]
X_Y<-X_Y[identify_outlier_rows_and_NAs(X_Y$X3)==F,]

X_Y<-X_Y[identify_outlier_rows_and_NAs(X_Y$Y)==F,]

str(X_Y)
X_Y<- data.frame(lapply(X_Y, function(x) as.numeric(as.character(x))))

#plot(x=X_Y$X,y=X_Y$Y)


X_Y<-X_Y[complete.cases(X_Y),]

cor(X_Y, use="complete.obs")



### -------------------------------- Creating a stan string to define model ---------------------
# This stan model is implemented based on the "Doing bayesian analysis" book by Kruschke

#' //** starts a multi-line comment and */ seems to end it
#' int<lower=0> N ; N us an integer with lower bound 0
#' int y[N] ; y is a length N vector of integers.
#'  // is a short comment
#'  

modelString = "
  data {
    int<lower=0> N ; // N is an integer with lower bound 0
    vector[N] x ; //x is a length N vector 
    vector[N] y ;  //y is a length N vector 

  }
  
  parameters {
  //real<lower=0,upper=5> theta ;
  real alpha;
  real beta;
  real<lower=0> sigma;
  }
  
  model {
  //theta ~ beta(2,2) ; // probability distributions denoted as beta
  //y ~ bernoulli(theta) ; // indicates that every y-value comes from the bernoulli distribution. this would have to be done in a for loop in JAGS
  y ~ normal(alpha + beta * x, sigma);
  }
"

# The stan string above then needs to be compiled into C++ code by making it into a dynamic shared object (DSO), this is done below
stanDso=stan_model(model_code = modelString)


#N=50;z=10;y=c(rep(1,z),rep(0,N-z))



plot(density(y))

#test_data<-list(y=y,N=N)
test_data<-X_Y[!is.na(X_Y$Y),]
test_data<-list(x=test_data$X1,y=test_data$Y, N=length(test_data$Y))

stanFit=sampling(object = stanDso, data=test_data, chains=3, iter=1000, warmup=200, thin=1)
# Now need to create some data to pass into the sampling string
plot(stanFit)
plot(stanFit, show_density = TRUE, ci_level = 0.95, fill_color = "blue")
plot(stanFit, plotfun = "hist", pars = "theta", include = FALSE)
plot(stanFit, plotfun = "trace", inc_warmup = T)
plot(stanFit, plotfun = "rhat") + ggtitle("split r-hat statistic")


