install.packages("regclass")
library(regclass)
library(dplyr)
library(tidyr)
data <- read.csv("C:/Armin/School/Fall 2020/Stat 350/cleaned_dataset.csv")
data2 <- read.csv("C:/Armin/School/Fall 2020/Stat 350/additional.csv")
data<- data[!(data$Selling_Price <450),]
data2<- data2[!(data2$Selling_Price <450),]
Car_name <- data$Car_Name
Car_name2 <- data2$Car_Name
obs <- length(data$Year)
obs2 <- length(data2$Year)
Car_Name_and_model <- sub("\\s*(\\S+\\s+\\S+).*", "\\1",Car_name)
Car_brand <-  sub("\\s*(\\S+\\s+).*", "\\1",Car_name)

car_model<- sub("\\s*(\\S+\\s+\\S+).*", "\\1",Car_name2)
#---------------------------------------------------------------------------------
# in the next few parts we set up our categorical variables 
Unique_car_builds2 <- matrix(0, nrow = length(data2$Year), ncol= length(unique(car_model)))
Unique_car_builds2<- as.data.frame(Unique_car_builds2)
colnames(Unique_car_builds2)<- c(unique(car_model))

for ( i in 1:obs2)
{
  for (j in 1:204)
  {
    if ( isTRUE(car_model[i]==names(Unique_car_builds2)[j]))
    {
      Unique_car_builds2[i,j]=1
    }
    else
    {
      Unique_car_builds2[i,j]=0
    }
  }
}
colnames(Unique_car_builds2)
Unique_car_builds2<- subset(Unique_car_builds2, select = -c(1))
#--------------------------------------------------------------------------------------
owners2 <- matrix(0, nrow=obs2, ncol=5)
owners2<- as.data.frame(owners2)
colnames(owners2)<- c("First Owner", "Second Owner", "Third Owner", "Fourth & Above Owner", "Test Drive Car")

for (i in 1:obs2)
{
  for ( j in 1:5)
  {
    if (isTRUE(names(owners2)[j]==data2$Owner[i]))
    {
      owners2[i,j]=1
    }
    else{
      owners2[i,j]=0
    }
  }
}
owners2<- select(owners2, select = c(1,2,3,5))
colnames(owners2)<- c("First Owner", "Second Owner", "Third Owner","Test Drive Car")
names(owners2)
#----------------------------------------------------------------------------------------------------------------------------
transmission_type2 <- matrix(0, nrow=obs2, ncol=2)
transmission_type2 <- as.data.frame(transmission_type2)
colnames(transmission_type2)<- c("Automatic", "Manual")

for (i in 1:obs2)
{
  for ( j in 1:2)
  {
    if (isTRUE(names(transmission_type2)[j]==data2$Transmission[i]))
    {
      transmission_type2[i,j]=1
    }
    else{
      transmission_type2[i,j]=0
    }
  }
}
transmission_type2<- subset(transmission_type2, select=c(1))
colnames(transmission_type2)<- c("Automatic")

#----------------------------------------------------------------------------------------------------------------------------

seller2<- matrix(0, nrow=obs2, ncol =2)
seller2 <- as.data.frame(seller2)
colnames(seller2)<- c("Dealer", "Individual")

for (i in 1:obs2)
{
  for ( j in 1:3)
  {
    if (isTRUE(names(seller2)[j]==data2$Selling_Price[i]))
    {
      seller2[i,j]=1
    }
    else{
      seller2[i,j]=0
    }
  }
}
seller2<- subset(seller2,select= c(1) )
colnames(seller2)<- c("Dealer")
#----------------------------------------------------------------------------------------------------------------------------
Fuel2<- matrix(0, nrow=obs2, ncol=4)
Fuel2<- as.data.frame(Fuel2)
colnames(Fuel2)<- c("CNG", "Diesel", "LPG", "Petrol")

for (i in 1:obs2)
{
  for ( j in 1:5)
  {
    if (isTRUE(names(Fuel2)[j]== data2$Fuel_Type[i]))
    {
      Fuel2[i,j]=1
    }
    else{
      Fuel2[i,j]=0
    }
  }
}
Fuel2 <- subset(Fuel2, select= c(1,2,3))
colnames(Fuel2)<- c("CNG", "Diesel", "LPG")
#----------------------------------
#--------------------------------------
Unique_car_builds <- matrix(0, nrow = length(data$Year), ncol= length(unique(Car_Name_and_model)))
Unique_car_builds<- as.data.frame(Unique_car_builds)
colnames(Unique_car_builds)<- c(unique(Car_Name_and_model))

for ( i in 1:obs)
{
  for (j in 1:204)
  {
    if ( isTRUE(Car_Name_and_model[i]==names(Unique_car_builds)[j]))
    {
      Unique_car_builds[i,j]=1
    }
    else
    {
      Unique_car_builds[i,j]=0
    }
  }
}
colnames(Unique_car_builds)
Unique_car_builds<- subset(Unique_car_builds, select = -c(1))
#----------------------------------------------------------------------------------------------------------------------------
#Unique_car_brand <- matrix(0, nrow = length(data$Year), ncol= length(unique(Car_brand)))
#Unique_car_brand<- as.data.frame(Unique_car_brand)
#colnames(Unique_car_brand)<- c(unique(Car_brand))

#for ( i in 1:obs)
#{
#  for (j in 1:34)
 # {
#    if ( isTRUE(Car_brand[i]==names(Unique_car_brand)[j]))
 #   {
 #     Unique_car_brand[i,j]=1
 #   }
 #   else
 #   {
 #     Unique_car_brand[i,j]=0
 #   }
 # }
#}
#Unique_car_brand<- subset(Unique_car_brand, select = -c(27,28))
#names(Unique_car_brand)

#----------------------------------------------------------------------------------------------------------------------------
owners <- matrix(0, nrow=obs, ncol=5)
owners<- as.data.frame(owners)
colnames(owners)<- c("First Owner", "Second Owner", "Third Owner", "Fourth & Above Owner", "Test Drive Car")

for (i in 1:obs)
{
  for ( j in 1:5)
  {
    if (isTRUE(names(owners)[j]==data$Owner[i]))
    {
      owners[i,j]=1
    }
    else{
      owners[i,j]=0
    }
  }
}
owners<- select(owners, select = c(1,2,3,5))
colnames(owners)<- c("First Owner", "Second Owner", "Third Owner","Test Drive Car")
names(owners)
#----------------------------------------------------------------------------------------------------------------------------
transmission_type <- matrix(0, nrow=obs, ncol=2)
transmission_type <- as.data.frame(transmission_type)
colnames(transmission_type)<- c("Automatic", "Manual")

for (i in 1:obs)
{
  for ( j in 1:2)
  {
    if (isTRUE(names(transmission_type)[j]==data$Transmission[i]))
    {
      transmission_type[i,j]=1
    }
    else{
      transmission_type[i,j]=0
    }
  }
}
transmission_type<- subset(transmission_type, select=c(1))
colnames(transmission_type)<- c("Automatic")

#----------------------------------------------------------------------------------------------------------------------------

seller<- matrix(0, nrow=obs, ncol =2)
seller <- as.data.frame(seller)
colnames(seller)<- c("Dealer", "Individual")

for (i in 1:obs)
{
  for ( j in 1:3)
  {
    if (isTRUE(names(seller)[j]==data$Selling_Price[i]))
    {
      seller[i,j]=1
    }
    else{
      seller [i,j]=0
    }
  }
}
seller<- subset(seller,select= c(1) )
colnames(seller)<- c("Dealer")
#----------------------------------------------------------------------------------------------------------------------------
Fuel<- matrix(0, nrow=obs, ncol=4)
Fuel<- as.data.frame(Fuel)
colnames(Fuel)<- c("CNG", "Diesel", "LPG", "Petrol")

for (i in 1:obs)
{
  for ( j in 1:5)
  {
    if (isTRUE(names(Fuel)[j]== data$Fuel_Type[i]))
    {
      Fuel[i,j]=1
    }
    else{
      Fuel[i,j]=0
    }
  }
}
Fuel <- subset(Fuel, select= c(1,2,3))
colnames(Fuel)<- c("CNG", "Diesel", "LPG")
#----------------------------------------------------------------------------------------------------------------------------
#function to standardize parameters
f<- function(x)
{
  return((x-mean(x))/sd(x))
}
#--------------------------------------------------------------------------------------------
#evaluating the model
df1<- cbind(data$Selling_Price, data$Year, data$Kms_Driven, data$mileage, data$engine, data$max_power, Unique_car_builds)
df2<- cbind(data$seats, owners, transmission_type, seller, Fuel, df1 )
df_add<- cbind(data2$seats, owners2, transmission_type2, seller2, Fuel2,data2$Selling_Price, data2$Year, data2$Kms_Driven, data2$mileage, data2$engine, data2$max_power, Unique_car_builds2) 
df_test<- cbind(df_add$`First Owner`, df_add$`Second Owner`, df_add$`Test Drive Car`, df_add$Automatic, df_add$Diesel, df_add$`data2$Year`, df_add$`data2$Kms_Driven`, df_add$`data2$mileage`, df_add$`data2$engine`, df_add$`data2$max_power`, Unique_car_builds2)
lm_test<- lm(log(df_add$`data2$Selling_Price`)~., data = df_test)
summary(lm_test)
#generating the pairs plot
pairs(df1)
#generating the predictor data set
df1<- as.data.frame(df1)
df2<- as.data.frame(df2)
#my_sample <- sample(c(1:length(df1$`data$Selling_Price`)), size=length(df1$`data$Selling_Price`)-7650, replace = FALSE)

prediction.df<- df1
predict.df2<- df2
#generating the model
lm1<- lm(log(prediction.df$`data$Selling_Price`) ~., data = prediction.df)
lm2<- lm(log(predict.df2$`data$Selling_Price`)~., data =predict.df2)
summary(lm2)
df3<- cbind(predict.df2$`First Owner`, predict.df2$`Second Owner`, predict.df2$Automatic, predict.df2$`Test Drive Car`, predict.df2$Diesel, predict.df2$`data$Year`, log(predict.df2$`data$Kms_Driven`), predict.df2$`data$mileage`, predict.df2$`data$max_power`, Unique_car_builds)
df4<- cbind(predict.df2$`First Owner`, predict.df2$`Second Owner`, predict.df2$Automatic, predict.df2$`Test Drive Car`, predict.df2$Diesel, f(predict.df2$`data$Year`), f(log(predict.df2$`data$Kms_Driven`)), f(predict.df2$`data$mileage`), f(predict.df2$`data$max_power`), Unique_car_builds)
lm3<- lm(log(predict.df2$`data$Selling_Price`)~., data = df3)
lm4<- lm(log(predict.df2$`data$Selling_Price`)~., data = df4)
plot(lm3)
summary(lm3)
plot(lm1)
summary(lm4)
step(lm1, direction = 'backward')
step(lm1, direction = 'forward') #for this we need to change lm to be an empty model
------------------------------------------------------------------------------------------------
#finding the highest hii values and comparing them with the residuals to see how high those are
x<-as.matrix(cbind(rep(1,length(prediction.df$`data$Selling_Price`)),prediction.df[,-c(1)]))
hii<- diag(x%*%solve(t(x)%*%x)%*%t(x))
ncol(x)
n<- nrow(x)
outliers <- which(hii>2*47/n)
resid<- summary(lm1)$residuals
reisd <- resid[c(outliers)]
----------------------------------------------------------------------------------------------------------
#partial Regression plots
# these y-test and t-tests were changed mannualy to test each non categorical variable
y_test <- lm(only_brand_no_error_predictor$data.Selling_Price~only_brand_no_error_predictor$data.mileage  +only_brand_no_error_predictor$data.Kms_Driven + only_brand_no_error_predictor$data.engine + only_brand_no_error_predictor$data.max_power + only_brand_no_error_predictor$data.seats)
test <- lm(only_brand_no_error_predictor$data.year~ only_brand_no_error_predictor$data.mileage+only_brand_no_error_predictor$data.Kms_Driven+only_brand_no_error_predictor$data.engine+only_brand_no_error_predictor$data.max_power+only_brand_no_error_predictor$data.seats)
test1<- lm(data.Selling_Price ~., data = only_brand_no_error_predictor)
summary(test1)
res1=summary(y_test)$residuals
res2=summary(test)$residuals
plot(y=res1, x=res2)
