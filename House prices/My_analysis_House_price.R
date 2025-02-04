library(plyr)
library(dplyr)
#First we load the data, the train and the test set seperately
setwd("C:/Users/User/Desktop/�������/R/Data analysis/House prices/house-prices-advanced-regression-techniques")
train<-read.csv("train.csv",stringsAsFactors = FALSE)
test<-read.csv("test.csv",header=TRUE,stringsAsFactors = FALSE)
#We can add the SalePrice variable in the test set as NA
test$SalePrice<-rep(NA,1459)
#Now we combine the test and train set
data<-rbind(train,test)

#Let's explore SalePrice variable,which is the one we want to predict
summary(data$SalePrice) #we can see that the median for the sales price is 163000
data$SalePrice<-as.numeric(data$SalePrice) #This variable is numeric
hist(data$SalePrice,breaks=30) #we can see a slightly skewed distribution to the right

#We check for missing values
str(data)
sort(sapply(data, function(x) sum(is.na(x))),decreasing=TRUE)
plot(sort(sapply(data, function(x) sum(is.na(x))),decreasing=TRUE),type='h')

#There are some columns with lots of missing values.We will decide later what to do with these.
#Basically,in most cases there are not missing values,but an indication that the apartment
#doesn't have these amenities. So let's remove these Na's with none and explore these variables

#PoolQC:  Pool quality
data$PoolQC[is.na(data$PoolQC)] <- "None"
table(data$PoolQC)
Qual_Cond <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$PoolQC<-revalue(data$PoolQC,Qual_Cond)
data$PoolQC<-as.integer(data$PoolQC)

#MiscFeature: Miscellaneous feature not covered in other categories   
table(data$MiscFeature)
data$MiscFeature[is.na(data$MiscFeature)] <- "None"
table(data$MiscFeature)
data$MiscFeature<-as.factor(data$MiscFeature) 
plot(data$MiscFeature)

#Alley: Type of alley access to property  
table(data$Alley)
data$Alley[is.na(data$Alley)] <- "None"
data$Alley<-as.factor(data$Alley)
plot(data$Alley,data$SalePrice) #Seems to affect the price
table(data$Alley)

#Fence
table(data$Fence)
data$Fence[is.na(data$Fence)] <- "None"
data$Fence<-as.factor(data$Fence)
plot(data$Fence,data$SalePrice) #Seems to affect the price,but no fence has the highest median!
table(data$Fence)

#FireplaceQu
table(data$FireplaceQu)
data$FireplaceQu[is.na(data$FireplaceQu)] <- "None"
data$FireplaceQu<-revalue(data$FireplaceQu,Qual_Cond) #replace the values with Quality/Condition values
data$FireplaceQu<-as.integer(data$FireplaceQu)
plot(data$FireplaceQu,data$SalePrice) #Seems to affect the price!
abline(lm(data$SalePrice~data$FireplaceQu))
table(data$FireplaceQu)

#LotFrontage: Linear feet of street connected to property. there are 259 NA's
summary(data$LotFrontage)
cor(data$LotFrontage,data$SalePrice, use = "pairwise.complete.obs")
hist(data$LotFrontage,breaks=30,xlim = c(0,200))
plot(data$LotFrontage,data$SalePrice)
abline(lm(data$SalePrice~data$LotFrontage))
summary(lm(data$SalePrice~data$LotFrontage))
#R squared is very low so it doesn't fit the data well.
#maybe we will replace na's with mean
data$LotFrontage[is.na(data$LotFrontage)] <- mean(na.omit(as.numeric(data$LotFrontage)))
#Good idea to implement is fill Na's with median from Neighborhood. Some other time

# GarageYrBlt
summary(data$GarageYrBlt)
plot(data$GarageYrBlt) #there is a wrong observation unless it is a house from the future - 2207.
#We replace it with 2007
which(data[,"GarageYrBlt"]>2019)
data[2593,"GarageYrBlt"]<-2007
plot(as.factor(data$GarageYrBlt),data$SalePrice) #Find out by another variable,YearBuilt
head(data.frame(data$YearBuilt,data$GarageYrBlt),10)
#indeed the year the house was built is in most cases the same as the year the garage was built,
#so we will use the observations from YearBuilt variable
data$GarageYrBlt[is.na(data$GarageYrBlt)]<-data$YearBuilt[is.na(data$GarageYrBlt)]
#we can see the correlation 
cor(data$GarageYrBlt,data$YearBuilt)
#We'll probably drop the variable and keep the YearBuilt variable.
data <- subset(data, select = -GarageYrBlt)

# GarageFinish:Interior finish of the garage
plot(as.factor(data$GarageFinish))
plot(as.factor(data$GarageFinish),data$SalePrice)
data$GarageFinish[is.na(data$GarageFinish)] <- "None"
plot(as.factor(data$GarageFinish),data$SalePrice)
#it seems ordinal
levels(as.factor(data$GarageFinish))
Finish<-c('None' = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)
data$GarageFinish<-revalue(data$GarageFinish,Finish)
data$GarageFinish<-as.integer(data$GarageFinish)  
plot(data$GarageFinish,data$SalePrice)

# GarageQual   
table(data$GarageQual)
plot(as.factor(data$GarageQual),data$SalePrice)
data$GarageQual[is.na(data$GarageQual)] <- "None"
plot(as.factor(data$GarageQual),data$SalePrice)
data$GarageQual<-revalue(data$GarageQual,Qual_Cond)
table(data$GarageQual)
data$GarageQual<-as.integer(data$GarageQual)


# GarageCond    
table(data$GarageCond)
plot(as.factor(data$GarageCond),data$SalePrice)
data$GarageCond[is.na(data$GarageCond)] <- "None"
plot(as.factor(data$GarageCond),data$SalePrice)
data$GarageCond<-revalue(data$GarageCond,Qual_Cond)
table(data$GarageCond)
data$GarageCond<-as.integer(data$GarageCond)
#we can see the correlation 
cor(data$GarageCond,data$GarageQual)
#It is a huge correlation,we must drop one of 2 variables
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageCond'])
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageQual'])
#We'll drop the variable less correlated with sales price
data <- subset(data, select = -GarageCond)

# GarageType
table(data$GarageType)
plot(as.factor(data$GarageType),data$SalePrice)
data$GarageType[is.na(data$GarageType)] <- "None"
plot(as.factor(data$GarageType),data$SalePrice)
data$GarageType<-as.factor(data$GarageType)
plot(data$GarageType,data$SalePrice)

#Maybe classify together the basement and garage variables later?

# BsmtCond:Evaluates the general condition of the basement
table(data$BsmtCond)
plot(as.factor(data$BsmtCond),data$SalePrice)
data$BsmtCond[is.na(data$BsmtCond)] <- "None"
plot(as.factor(data$BsmtCond),data$SalePrice)
data$BsmtCond<-revalue(data$BsmtCond,Qual_Cond)
table(data$BsmtCond)
data$BsmtCond<-as.integer(data$BsmtCond)
plot(as.factor(data$BsmtCond),data$SalePrice)



# BsmtExposure:Refers to walkout or garden level walls
table(data$BsmtExposure)
plot(as.factor(data$BsmtExposure),data$SalePrice)
data$BsmtExposure[is.na(data$BsmtExposure)] <- "None"
plot(as.factor(data$BsmtExposure),data$SalePrice)
Exposure<-c("None"=0,"No"=1,'Mn'=2,'Av'=3,'Gd'=4)
data$BsmtExposure<-revalue(data$BsmtExposure,Exposure)
table(data$BsmtExposure)
data$BsmtExposure<-as.integer(data$BsmtExposure)
plot(as.factor(data$BsmtExposure),data$SalePrice)




# BsmtQual:Evaluates the height of the basement
table(data$BsmtQual)
plot(as.factor(data$BsmtQual),data$SalePrice)
data$BsmtQual[is.na(data$BsmtQual)] <- "None"
plot(as.factor(data$BsmtQual),data$SalePrice)
data$BsmtQual<-revalue(data$BsmtQual,Qual_Cond)
table(data$BsmtQual)
data$BsmtQual<-as.integer(data$BsmtQual)
plot(as.factor(data$BsmtQual),data$SalePrice)


# BsmtFinType1:Rating of basement finished area
table(data$BsmtFinType1)
plot(as.factor(data$BsmtFinType1),data$SalePrice)
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "None"
plot(as.factor(data$BsmtFinType1),data$SalePrice)
Bsm_type<-c("None"=0,"Unf"=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
data$BsmtFinType1<-revalue(data$BsmtFinType1,Bsm_type)
table(data$BsmtFinType1)
data$BsmtFinType1<-as.integer(data$BsmtFinType1)
plot(as.factor(data$BsmtFinType1),data$SalePrice)



#BsmtFinType2:Rating of basement finished area (if multiple types)
table(data$BsmtFinType2)
plot(as.factor(data$BsmtFinType2),data$SalePrice)
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "None"
plot(as.factor(data$BsmtFinType2),data$SalePrice)
data$BsmtFinType2<-revalue(data$BsmtFinType2,Bsm_type)
table(data$BsmtFinType2)
data$BsmtFinType2<-as.integer(data$BsmtFinType2)
plot(as.factor(data$BsmtFinType2),data$SalePrice)

# MasVnrType: Masonry veneer type (walls)  Replace all Na's with 'none'
table(data$MasVnrType)
plot(as.factor(data$MasVnrType),data$SalePrice)
data$MasVnrType[is.na(data$MasVnrType)] <- "None"
Masonry <- c('None'=0, 'BrkCmn'=1, 'BrkFace'=2, 'Stone'=3)
data$MasVnrType<-revalue(data$MasVnrType,Masonry)
table(data$MasVnrType)
data$MasVnrType<-as.integer(data$MasVnrType)
plot(as.factor(data$MasVnrType),data$SalePrice)

# MasVnrArea
table(data$MasVnrArea)
plot(data$MasVnrArea,data$SalePrice)
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0
table(data$MasVnrArea)
data$MasVnrArea<-as.numeric(data$MasVnrArea)
plot(data$MasVnrArea,data$SalePrice)


# MSZoning: Identifies the general zoning classification of the sale.
table(data$MSZoning)
plot(as.factor(data$MSZoning),data$SalePrice)
data[is.na(data$MSZoning),] #How we can find the missing values? Let's check out the MSSubClass
data[is.na(data$MSZoning),c("MSZoning",'MSSubClass')]
table(data[,c("MSZoning",'MSSubClass')])
plot(as.factor(data$MSZoning),as.factor(data$MSSubClass))
plot(as.factor(data$MSSubClass),as.factor(data$MSZoning))
#and now the correlations
library(corrplot)
cor(data$MSSubClass,data[,sapply(data, is.numeric)]) #it is not higly correlated with other numeric variables
#We will replace MSSubClass=20 with RL , MSSubClass=70 and MSSubClass=30 with RM 
data$MSZoning[is.na(data$MSZoning)]<-c('RM','RL','RM','RL')
data[is.na(data$MSZoning),c("MSZoning",'MSSubClass')]
data$MSZoning<-as.factor(data$MSZoning)

# Utilities: Type of utilities available
table(data$Utilities)
data[is.na(data$Utilities),]
data[(data$Utilities=='NoSeWa'),]
data[945,]
#We don't need this variable for prediction , as there is only one house "NoSeWa"
data <- subset(data, select = -Utilities)


# BsmtFullBath:Basement full bathrooms
table(data$BsmtFullBath)
data[is.na(data$BsmtFullBath),]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$BsmtFullBath),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$BsmtFullBath[is.na(data$BsmtFullBath)]<-0
plot(as.factor(data$BsmtFullBath),data$SalePrice)
data$BsmtFullBath<-as.integer(data$BsmtFullBath)

# BsmtHalfBath
table(data$BsmtHalfBath)
data[is.na(data$BsmtHalfBath),]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$BsmtHalfBath),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$BsmtHalfBath[is.na(data$BsmtHalfBath)]<-0
plot(as.factor(data$BsmtHalfBath),data$SalePrice)
data$BsmtHalfBath<-as.integer(data$BsmtHalfBath)
#we can see the correlation 
cor(data$BsmtHalfBath,data$BsmtFullBath) #It is not correlated


# Functional: Home functionality (Assume typical unless deductions are warranted)
table(data$Functional)
plot(as.factor(data$Functional),data$SalePrice)
data[is.na(data$Functional),]
data$Functional[is.na(data$Functional)]<-7
Functional<-c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
data$Functional <- as.integer(revalue(data$Functional, Functional))
table(data$Functional)
plot(data$Functional,data$SalePrice)

#GarageCars Size of garage in car capacity  

table(as.factor(data$GarageCars))
plot(as.factor(data$GarageCars))
plot(as.factor(data$GarageCars),data$SalePrice)
paste(colnames(select(data,contains("Garage"))),collapse="','")
data[is.na(data$GarageCars),c('GarageType','GarageFinish','GarageCars','GarageArea','GarageQual')]
data$GarageCars[is.na(data$GarageCars)]<-0
data$GarageCars<-as.integer(data$GarageCars)
 


#GarageArea
plot(as.factor(data$GarageArea))
plot(data$GarageArea,data$SalePrice)
paste(colnames(select(data,contains("Garage"))),collapse="','")
data[is.na(data$GarageArea),c('GarageType','GarageFinish','GarageCars','GarageArea','GarageQual')]
data$GarageArea[is.na(data$GarageArea)]<-0
data$GarageArea<-as.integer(data$GarageArea)
plot(data$GarageArea,data$SalePrice)
cor(data[,sapply(data, is.numeric)],data$GarageArea) 
#drop GarageArea and keep GarageCars?
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageArea'])
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageCars']) 
#We'll indeed drop the GarageArea variable since it is less corellated with sales Price 
data <- subset(data, select = -GarageArea)


# TotalBsmtSF : Total square feet of basement area
plot(as.factor(data$TotalBsmtSF))
plot(data$TotalBsmtSF,data$SalePrice)
cor(data[1:1459,'SalePrice'],data[1:1459,'TotalBsmtSF'])
data$TotalBsmtSF[is.na(data$TotalBsmtSF)]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$TotalBsmtSF),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$TotalBsmtSF[is.na(data$TotalBsmtSF)]<-0

#  BsmtFinSF1    
plot(as.factor(data$BsmtFinSF1))
plot(data$BsmtFinSF1,data$SalePrice)
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtFinSF1'])
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$BsmtFinSF1),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]<-0

#BsmtUnfSF
plot(data$BsmtUnfSF)
plot(data$BsmtUnfSF,data$SalePrice)
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtUnfSF'])
data$BsmtUnfSF[is.na(data$BsmtUnfSF)]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$BsmtUnfSF),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$BsmtUnfSF[is.na(data$BsmtUnfSF)]<-0
cor(data$BsmtUnfSF,data$BsmtFinSF2)
#Let's see if it is correlated with another
cor(data$BsmtUnfSF,data[,sapply(data, is.numeric)]) #correlated with BsmtFinSF1

#BsmtFinSF2     
plot(data$BsmtFinSF2)
plot(data$BsmtFinSF2,data$SalePrice)
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtFinSF2'])
data$BsmtFinSF2[is.na(data$BsmtFinSF2)]
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
data[is.na(data$BsmtFinSF2),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
data$BsmtFinSF2[is.na(data$BsmtFinSF2)]<-0
cor(data$BsmtFinSF1,data$BsmtFinSF2)
#We will probably drop this variable,let's see if it is correlated with another
cor(data$BsmtFinSF2,data[,sapply(data, is.numeric)]) #correlated with BsmtFinType2
#We'll probably drop these 2 basement variables
#Now we see the correlations between basement variables

paste(colnames(select(data,contains("Bsmt"))),collapse="','")
cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')])
corrplot(cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')])
,method = "square")
#we drop the variables BsmtFinSF1 and BsmtFinSF2
data <- subset(data, select = -BsmtFinSF1)
data <- subset(data, select = -BsmtFinSF2)
#check again
corrplot(cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]),method='square')
data <- subset(data, select = -TotalBsmtSF)   #MAYBE NOT THIS VARIABLE?     
corrplot(cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType2','BsmtUnfSF','BsmtFullBath','BsmtHalfBath')]),method='square')


################################################################################

#Now that we have taken care of Na's let's see what the numeric variables are
colnames(data[,sapply(data, is.numeric)]) #check out which columns are numeric 

#But first let's see some correlations
corrplot(cor(na.omit(data[,sapply(data, is.numeric)])),method = "square")
paste(which(cor(data[1:1459,sapply(data, is.numeric)],data[1:1459,'SalePrice'])>0.5 | cor(data[1:1459,sapply(data, is.numeric)],data[1:1459,'SalePrice'])<(-0.5)),collapse=',')
colnames(data[,sapply(data, is.numeric)][c(5,7,8,11,17,20,23,27,30,31,32,44)])
#So these are the variables that have the highest correlation with the Saleprice variable.
#We will examine these further.

#Id
#We will get rid of the Id column and keep it in a vector called Id
Id<-data$Id[1461:nrow(data)]
data <- data[,-1]


#MSSubClass: Identifies the type of dwelling involved in the sale.
plot(data$MSSubClass,data$SalePrice)
#In reality this is a factor and not numeric.
table(as.factor(data$MSSubClass))
data$MSSubClass<-as.factor(data$MSSubClass)


#OverallQual: Rates the overall material and finish of the house
table(as.factor(data$OverallQual))
plot(as.factor(data$OverallQual),data$SalePrice)
data$OverallQual<-as.integer(data$OverallQual)
table(data$OverallQual)


#OverallCond: Rates the overall condition of the house
table(as.factor(data$OverallCond))
plot(as.factor(data$OverallCond),data$SalePrice)
data$OverallCond<-as.integer(data$OverallCond)
table(data$OverallCond)


#YearBuilt: Original construction date
plot(as.factor(data$YearBuilt))
table(as.factor(data$YearBuilt))
plot(as.factor(data$YearBuilt),data$SalePrice)
cor(data[1:1459,'SalePrice'],data[1:1459,'YearBuilt'])


# YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)
plot(as.factor(data$YearRemodAdd))
table(as.factor(data$YearRemodAdd))
plot(as.factor(data$YearRemodAdd),data$SalePrice)
plot(data$YearRemodAdd,data$YearBuilt)
#We can see from the plot that all remodelings started on 1950. Is it true or there is some error?
cor(data$YearRemodAdd>1950,data$YearBuilt>1950) #Highly correlated with Yearbuilt



# X1stFlrSF"  
# GrLivArea"   
# FullBath"   
# TotRmsAbvGrd" 
# FireplaceQu" 
# GarageFinish"
 
 
 
 
 
#Pool Area
table(data$PoolArea) #we'll drop this variable
data <- subset(data, select = -c(PoolArea))

#PoolQC
table(data$PoolQC) #we'll drop this variable
data <- subset(data, select = -c(PoolQC))



################################################################################
colnames(data[,sapply(data, is.character)]) #check out which columns are character

#Street: Type of road access to property 
plot(as.factor(data$Street))
table(data$Street)
#Only 12 Gravels      
#removing Street variable 
data <- subset(data, select = -Street)

# Electrical
data$Electrical<-as.factor(data$Electrical)
plot(as.factor(data$Electrical))
table(data$Electrical)


numeric_columns<-colnames(data[,sapply(data, is.numeric)])
integer_columns<-colnames(data[,sapply(data, is.integer)])



library(caret)
library(glmnet)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train, y=data$SalePrice[!is.na(data$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune



##################################################################################
#scale all variables except saleprice change it ,make a vector salePrice
datanew<-data
datanew<-data[,-ncol(data)]
  

datanew[,sapply(datanew, is.numeric)]<-scale(datanew[,sapply(datanew, is.numeric)])



paste(colnames(data), collapse ="+",sep=" ")
datanew$SalePrice<-data$SalePrice

train<-datanew[1:1460,]
test<-datanew[1461:nrow(datanew),]

fit<-step(lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+Alley+LotShape+LandContour+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinType2+BsmtUnfSF+Heating+HeatingQC+CentralAir+Electrical+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType+GarageFinish+GarageCars+GarageQual+PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+Fence+MiscFeature+MiscVal+MoSold+YrSold+SaleType+SaleCondition, data=na.omit(train)),direction = "both")  
summary(fit)
test$SalePrice<-predict(fit,test)
df<-data.frame(Id,test[,"SalePrice"])

which(is.na(df))
colnames(df)<-c("Id","SalePrice")

which(is.na(df))
#But we have 2 NA's (further investigate for now use mean)
for(i in 1:2){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
}
write.csv(df,"C:/Users/User/Desktop/�������/R/Data analysis/House prices/house-prices-advanced-regression-techniques\\submission.csv", row.names = FALSE)
getwd()


