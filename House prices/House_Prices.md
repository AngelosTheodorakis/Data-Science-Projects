House Prices: Prediction and Data analysis
==========================================

-   First, we will load the libraries we need.

``` r
library(plyr)
library(dplyr)
```

-   Then we load the data, the train and the test set seperately.

``` r
setwd("C:/Users/User/Desktop/Άγγελος/R/Data analysis/House prices/house-prices-advanced-regression-techniques")
train<-read.csv("train.csv",stringsAsFactors = FALSE)
test<-read.csv("test.csv",header=TRUE,stringsAsFactors = FALSE)
```

-   We can add the SalePrice variable in the test set as NA.

``` r
test$SalePrice <- NA
```

-   Now we combine the test and train set and explore our data.

``` r
data <- rbind(train,test)
str(data)
```

    ## 'data.frame':    2919 obs. of  81 variables:
    ##  $ Id           : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ MSSubClass   : int  60 20 60 70 60 50 20 60 50 190 ...
    ##  $ MSZoning     : chr  "RL" "RL" "RL" "RL" ...
    ##  $ LotFrontage  : int  65 80 68 60 84 85 75 NA 51 50 ...
    ##  $ LotArea      : int  8450 9600 11250 9550 14260 14115 10084 10382 6120 7420 ...
    ##  $ Street       : chr  "Pave" "Pave" "Pave" "Pave" ...
    ##  $ Alley        : chr  NA NA NA NA ...
    ##  $ LotShape     : chr  "Reg" "Reg" "IR1" "IR1" ...
    ##  $ LandContour  : chr  "Lvl" "Lvl" "Lvl" "Lvl" ...
    ##  $ Utilities    : chr  "AllPub" "AllPub" "AllPub" "AllPub" ...
    ##  $ LotConfig    : chr  "Inside" "FR2" "Inside" "Corner" ...
    ##  $ LandSlope    : chr  "Gtl" "Gtl" "Gtl" "Gtl" ...
    ##  $ Neighborhood : chr  "CollgCr" "Veenker" "CollgCr" "Crawfor" ...
    ##  $ Condition1   : chr  "Norm" "Feedr" "Norm" "Norm" ...
    ##  $ Condition2   : chr  "Norm" "Norm" "Norm" "Norm" ...
    ##  $ BldgType     : chr  "1Fam" "1Fam" "1Fam" "1Fam" ...
    ##  $ HouseStyle   : chr  "2Story" "1Story" "2Story" "2Story" ...
    ##  $ OverallQual  : int  7 6 7 7 8 5 8 7 7 5 ...
    ##  $ OverallCond  : int  5 8 5 5 5 5 5 6 5 6 ...
    ##  $ YearBuilt    : int  2003 1976 2001 1915 2000 1993 2004 1973 1931 1939 ...
    ##  $ YearRemodAdd : int  2003 1976 2002 1970 2000 1995 2005 1973 1950 1950 ...
    ##  $ RoofStyle    : chr  "Gable" "Gable" "Gable" "Gable" ...
    ##  $ RoofMatl     : chr  "CompShg" "CompShg" "CompShg" "CompShg" ...
    ##  $ Exterior1st  : chr  "VinylSd" "MetalSd" "VinylSd" "Wd Sdng" ...
    ##  $ Exterior2nd  : chr  "VinylSd" "MetalSd" "VinylSd" "Wd Shng" ...
    ##  $ MasVnrType   : chr  "BrkFace" "None" "BrkFace" "None" ...
    ##  $ MasVnrArea   : int  196 0 162 0 350 0 186 240 0 0 ...
    ##  $ ExterQual    : chr  "Gd" "TA" "Gd" "TA" ...
    ##  $ ExterCond    : chr  "TA" "TA" "TA" "TA" ...
    ##  $ Foundation   : chr  "PConc" "CBlock" "PConc" "BrkTil" ...
    ##  $ BsmtQual     : chr  "Gd" "Gd" "Gd" "TA" ...
    ##  $ BsmtCond     : chr  "TA" "TA" "TA" "Gd" ...
    ##  $ BsmtExposure : chr  "No" "Gd" "Mn" "No" ...
    ##  $ BsmtFinType1 : chr  "GLQ" "ALQ" "GLQ" "ALQ" ...
    ##  $ BsmtFinSF1   : int  706 978 486 216 655 732 1369 859 0 851 ...
    ##  $ BsmtFinType2 : chr  "Unf" "Unf" "Unf" "Unf" ...
    ##  $ BsmtFinSF2   : int  0 0 0 0 0 0 0 32 0 0 ...
    ##  $ BsmtUnfSF    : int  150 284 434 540 490 64 317 216 952 140 ...
    ##  $ TotalBsmtSF  : int  856 1262 920 756 1145 796 1686 1107 952 991 ...
    ##  $ Heating      : chr  "GasA" "GasA" "GasA" "GasA" ...
    ##  $ HeatingQC    : chr  "Ex" "Ex" "Ex" "Gd" ...
    ##  $ CentralAir   : chr  "Y" "Y" "Y" "Y" ...
    ##  $ Electrical   : chr  "SBrkr" "SBrkr" "SBrkr" "SBrkr" ...
    ##  $ X1stFlrSF    : int  856 1262 920 961 1145 796 1694 1107 1022 1077 ...
    ##  $ X2ndFlrSF    : int  854 0 866 756 1053 566 0 983 752 0 ...
    ##  $ LowQualFinSF : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ GrLivArea    : int  1710 1262 1786 1717 2198 1362 1694 2090 1774 1077 ...
    ##  $ BsmtFullBath : int  1 0 1 1 1 1 1 1 0 1 ...
    ##  $ BsmtHalfBath : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ FullBath     : int  2 2 2 1 2 1 2 2 2 1 ...
    ##  $ HalfBath     : int  1 0 1 0 1 1 0 1 0 0 ...
    ##  $ BedroomAbvGr : int  3 3 3 3 4 1 3 3 2 2 ...
    ##  $ KitchenAbvGr : int  1 1 1 1 1 1 1 1 2 2 ...
    ##  $ KitchenQual  : chr  "Gd" "TA" "Gd" "Gd" ...
    ##  $ TotRmsAbvGrd : int  8 6 6 7 9 5 7 7 8 5 ...
    ##  $ Functional   : chr  "Typ" "Typ" "Typ" "Typ" ...
    ##  $ Fireplaces   : int  0 1 1 1 1 0 1 2 2 2 ...
    ##  $ FireplaceQu  : chr  NA "TA" "TA" "Gd" ...
    ##  $ GarageType   : chr  "Attchd" "Attchd" "Attchd" "Detchd" ...
    ##  $ GarageYrBlt  : int  2003 1976 2001 1998 2000 1993 2004 1973 1931 1939 ...
    ##  $ GarageFinish : chr  "RFn" "RFn" "RFn" "Unf" ...
    ##  $ GarageCars   : int  2 2 2 3 3 2 2 2 2 1 ...
    ##  $ GarageArea   : int  548 460 608 642 836 480 636 484 468 205 ...
    ##  $ GarageQual   : chr  "TA" "TA" "TA" "TA" ...
    ##  $ GarageCond   : chr  "TA" "TA" "TA" "TA" ...
    ##  $ PavedDrive   : chr  "Y" "Y" "Y" "Y" ...
    ##  $ WoodDeckSF   : int  0 298 0 0 192 40 255 235 90 0 ...
    ##  $ OpenPorchSF  : int  61 0 42 35 84 30 57 204 0 4 ...
    ##  $ EnclosedPorch: int  0 0 0 272 0 0 0 228 205 0 ...
    ##  $ X3SsnPorch   : int  0 0 0 0 0 320 0 0 0 0 ...
    ##  $ ScreenPorch  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PoolArea     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ PoolQC       : chr  NA NA NA NA ...
    ##  $ Fence        : chr  NA NA NA NA ...
    ##  $ MiscFeature  : chr  NA NA NA NA ...
    ##  $ MiscVal      : int  0 0 0 0 0 700 0 350 0 0 ...
    ##  $ MoSold       : int  2 5 9 2 12 10 8 11 4 1 ...
    ##  $ YrSold       : int  2008 2007 2008 2006 2008 2009 2007 2009 2008 2008 ...
    ##  $ SaleType     : chr  "WD" "WD" "WD" "WD" ...
    ##  $ SaleCondition: chr  "Normal" "Normal" "Normal" "Abnorml" ...
    ##  $ SalePrice    : int  208500 181500 223500 140000 250000 143000 307000 200000 129900 118000 ...

-   Let’s explore SalePrice variable, which is the variable we want to
    predict.

``` r
summary(data$SalePrice) # We can see that the median for the sales price is 163000
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   34900  129975  163000  180921  214000  755000    1459

``` r
data$SalePrice<-as.numeric(data$SalePrice) # Change to numeric
hist(data$SalePrice, breaks=30, xlab = 'Price', main = 'Histogram of Sale Price') # There is a slightly skewed distribution to the right, possibly because of the extreme values.
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-5-1.png)

-   We will now check for missing values.

``` r
sort(sapply(data, function(x) sum(is.na(x))),decreasing=TRUE)
```

    ##        PoolQC   MiscFeature         Alley         Fence     SalePrice 
    ##          2909          2814          2721          2348          1459 
    ##   FireplaceQu   LotFrontage   GarageYrBlt  GarageFinish    GarageQual 
    ##          1420           486           159           159           159 
    ##    GarageCond    GarageType      BsmtCond  BsmtExposure      BsmtQual 
    ##           159           157            82            82            81 
    ##  BsmtFinType2  BsmtFinType1    MasVnrType    MasVnrArea      MSZoning 
    ##            80            79            24            23             4 
    ##     Utilities  BsmtFullBath  BsmtHalfBath    Functional   Exterior1st 
    ##             2             2             2             2             1 
    ##   Exterior2nd    BsmtFinSF1    BsmtFinSF2     BsmtUnfSF   TotalBsmtSF 
    ##             1             1             1             1             1 
    ##    Electrical   KitchenQual    GarageCars    GarageArea      SaleType 
    ##             1             1             1             1             1 
    ##            Id    MSSubClass       LotArea        Street      LotShape 
    ##             0             0             0             0             0 
    ##   LandContour     LotConfig     LandSlope  Neighborhood    Condition1 
    ##             0             0             0             0             0 
    ##    Condition2      BldgType    HouseStyle   OverallQual   OverallCond 
    ##             0             0             0             0             0 
    ##     YearBuilt  YearRemodAdd     RoofStyle      RoofMatl     ExterQual 
    ##             0             0             0             0             0 
    ##     ExterCond    Foundation       Heating     HeatingQC    CentralAir 
    ##             0             0             0             0             0 
    ##     X1stFlrSF     X2ndFlrSF  LowQualFinSF     GrLivArea      FullBath 
    ##             0             0             0             0             0 
    ##      HalfBath  BedroomAbvGr  KitchenAbvGr  TotRmsAbvGrd    Fireplaces 
    ##             0             0             0             0             0 
    ##    PavedDrive    WoodDeckSF   OpenPorchSF EnclosedPorch    X3SsnPorch 
    ##             0             0             0             0             0 
    ##   ScreenPorch      PoolArea       MiscVal        MoSold        YrSold 
    ##             0             0             0             0             0 
    ## SaleCondition 
    ##             0

``` r
plot(sort(sapply(data, function(x) sum(is.na(x))),decreasing=TRUE),type='h')
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-6-1.png)

There are some columns with lots of missing values. However, in most
cases these are not missing values, but an indication that the apartment
doesn’t have these amenities. So let’s replace these Na’s with ‘None’
and explore these variables.

**PoolQC: Pool quality**
------------------------

<br> This is the first variable we will examine. From the documentation,
we observe that indeed, the ‘NA’ value in this variable means that there
is no pool. We will also replace the values with integers from 0 to 5 as
long as it is a scaling variable.<br> We will continue in a similar way
with the other variables that have missing values.

``` r
data$PoolQC[is.na(data$PoolQC)] <- "None"
table(data$PoolQC)
```

    ## 
    ##   Ex   Fa   Gd None 
    ##    4    2    4 2909

``` r
Qual_Cond <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$PoolQC<-revalue(data$PoolQC,Qual_Cond)
data$PoolQC<-as.integer(data$PoolQC)
```

**MiscFeature: Miscellaneous feature not covered in other categories**
----------------------------------------------------------------------

``` r
table(data$MiscFeature)
```

    ## 
    ## Gar2 Othr Shed TenC 
    ##    5    4   95    1

``` r
data$MiscFeature[is.na(data$MiscFeature)] <- "None"
table(data$MiscFeature)
```

    ## 
    ## Gar2 None Othr Shed TenC 
    ##    5 2814    4   95    1

``` r
data$MiscFeature<-as.factor(data$MiscFeature) 
plot(data$MiscFeature)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-8-1.png)

**Alley: Type of alley access to property**
-------------------------------------------

``` r
table(data$Alley)
```

    ## 
    ## Grvl Pave 
    ##  120   78

``` r
data$Alley[is.na(data$Alley)] <- "None"
data$Alley<-as.factor(data$Alley)
plot(data$Alley,data$SalePrice) #Seems to affect the price
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
table(data$Alley)
```

    ## 
    ## Grvl None Pave 
    ##  120 2721   78

**Fence: Fence quality**
------------------------

``` r
table(data$Fence)
```

    ## 
    ## GdPrv  GdWo MnPrv  MnWw 
    ##   118   112   329    12

``` r
data$Fence[is.na(data$Fence)] <- "None"
data$Fence<-as.factor(data$Fence)
plot(data$Fence,data$SalePrice) # Seems to affect the price. The 'no fence' variable has the highest median!
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
table(data$Fence)
```

    ## 
    ## GdPrv  GdWo MnPrv  MnWw  None 
    ##   118   112   329    12  2348

**FireplaceQu: Fireplace quality**
----------------------------------

``` r
table(data$FireplaceQu)
```

    ## 
    ##  Ex  Fa  Gd  Po  TA 
    ##  43  74 744  46 592

``` r
data$FireplaceQu[is.na(data$FireplaceQu)] <- "None"
data$FireplaceQu<-revalue(data$FireplaceQu,Qual_Cond) #replace the values with Quality/Condition values
data$FireplaceQu<-as.integer(data$FireplaceQu)
plot(data$FireplaceQu,data$SalePrice) #Seems to affect the price!
abline(lm(data$SalePrice~data$FireplaceQu))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
table(data$FireplaceQu)
```

    ## 
    ##    0    1    2    3    4    5 
    ## 1420   46   74  592  744   43

**LotFrontage: Linear feet of street connected to property.**
-------------------------------------------------------------

There are 259 NA’s. We will replace the missing values with the median
and not the mean, because of the extreme values (outliers).

``` r
summary(data$LotFrontage)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   21.00   59.00   68.00   69.31   80.00  313.00     486

``` r
cor(data$LotFrontage,data$SalePrice, use = "pairwise.complete.obs")
```

    ## [1] 0.3517991

``` r
hist(data$LotFrontage,breaks=30,xlim = c(0,200))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
plot(data$LotFrontage,data$SalePrice) # There are 2 extreme values
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-12-2.png)

``` r
data$LotFrontage[is.na(data$LotFrontage)]<-median(na.omit(as.numeric(data$LotFrontage)))
```

**GarageYrBlt**
---------------

``` r
summary(data$GarageYrBlt)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1895    1960    1979    1978    2002    2207     159

``` r
plot(data$GarageYrBlt) 
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-13-1.png)

There is a wrong observation unless it is a house from the future
(2207)! We replace it with the year 2007.

``` r
which(data[,"GarageYrBlt"]>2019)
```

    ## [1] 2593

``` r
data[2593,"GarageYrBlt"]<-2007 # Replace the observation
plot(as.factor(data$GarageYrBlt),data$SalePrice,xlab = 'Year the garage was built', ylab = 'House Price')
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-14-1.png)

We will find out the NA’s by examining another variable, YearBuilt.
Let’s see if they have a correlation.

``` r
head(data.frame(data$YearBuilt,data$GarageYrBlt),15)
```

    ##    data.YearBuilt data.GarageYrBlt
    ## 1            2003             2003
    ## 2            1976             1976
    ## 3            2001             2001
    ## 4            1915             1998
    ## 5            2000             2000
    ## 6            1993             1993
    ## 7            2004             2004
    ## 8            1973             1973
    ## 9            1931             1931
    ## 10           1939             1939
    ## 11           1965             1965
    ## 12           2005             2005
    ## 13           1962             1962
    ## 14           2006             2006
    ## 15           1960             1960

``` r
length(which(data$YearBuilt!=data$GarageYrBlt)) # Number of different rows
```

    ## [1] 544

Indeed the year the house was built is in most cases the same as the
year the garage was built, so we will drop the variable and keep the
YearBuilt variable.

``` r
data <- subset(data, select = -GarageYrBlt)
```

**GarageFinish:Interior finish of the garage**
----------------------------------------------

``` r
plot(as.factor(data$GarageFinish))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
plot(as.factor(data$GarageFinish),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-17-2.png)

``` r
data$GarageFinish[is.na(data$GarageFinish)] <- "None"
plot(as.factor(data$GarageFinish),data$SalePrice) # It seems ordinal 
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-17-3.png)

``` r
levels(as.factor(data$GarageFinish))
```

    ## [1] "Fin"  "None" "RFn"  "Unf"

``` r
Finish <- c('None' = 0, "Unf" = 1, "RFn" = 2, "Fin" = 3)
data$GarageFinish<-revalue(data$GarageFinish,Finish)
data$GarageFinish<-as.integer(data$GarageFinish)  
plot(data$GarageFinish,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-17-4.png)

**GarageQual: Garage quality**
------------------------------

``` r
table(data$GarageQual)
```

    ## 
    ##   Ex   Fa   Gd   Po   TA 
    ##    3  124   24    5 2604

``` r
plot(as.factor(data$GarageQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
data$GarageQual[is.na(data$GarageQual)] <- "None"
plot(as.factor(data$GarageQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
data$GarageQual<-revalue(data$GarageQual,Qual_Cond)
table(data$GarageQual)
```

    ## 
    ##    0    1    2    3    4    5 
    ##  159    5  124 2604   24    3

``` r
data$GarageQual<-as.integer(data$GarageQual)
```

**GarageCond: Garage condition**
--------------------------------

``` r
table(data$GarageCond)
```

    ## 
    ##   Ex   Fa   Gd   Po   TA 
    ##    3   74   15   14 2654

``` r
plot(as.factor(data$GarageCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
data$GarageCond[is.na(data$GarageCond)] <- "None"
plot(as.factor(data$GarageCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-19-2.png)

``` r
data$GarageCond<-revalue(data$GarageCond,Qual_Cond)
table(data$GarageCond)
```

    ## 
    ##    0    1    2    3    4    5 
    ##  159   14   74 2654   15    3

``` r
data$GarageCond<-as.integer(data$GarageCond)
```

We can see the correlation between garage condition and quality.

``` r
cor(data$GarageCond,data$GarageQual)
```

    ## [1] 0.9466563

It is a huge correlation,we must drop one of 2 variables

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageCond'])
```

    ## [1] 0.2632897

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageQual'])
```

    ## [1] 0.2739379

We’ll drop the variable less correlated with sales price

``` r
data <- subset(data, select = -GarageCond)
```

**GarageType: Garage location**
-------------------------------

``` r
table(data$GarageType)
```

    ## 
    ##  2Types  Attchd Basment BuiltIn CarPort  Detchd 
    ##      23    1723      36     186      15     779

``` r
plot(as.factor(data$GarageType),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
data$GarageType[is.na(data$GarageType)] <- "None"
plot(as.factor(data$GarageType),data$SalePrice)
data$GarageType<-as.factor(data$GarageType)
plot(data$GarageType,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-23-2.png)

**BsmtCond:Evaluates the general condition of the basement**
------------------------------------------------------------

``` r
table(data$BsmtCond)
```

    ## 
    ##   Fa   Gd   Po   TA 
    ##  104  122    5 2606

``` r
plot(as.factor(data$BsmtCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
data$BsmtCond[is.na(data$BsmtCond)] <- "None"
plot(as.factor(data$BsmtCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-24-2.png)

``` r
data$BsmtCond<-revalue(data$BsmtCond,Qual_Cond)
table(data$BsmtCond)
```

    ## 
    ##    0    1    2    3    4 
    ##   82    5  104 2606  122

``` r
data$BsmtCond<-as.integer(data$BsmtCond)
plot(as.factor(data$BsmtCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-24-3.png)
There seems to be a correlation with the house price.

**BsmtExposure:Refers to walkout or garden level walls**
--------------------------------------------------------

``` r
table(data$BsmtExposure)
```

    ## 
    ##   Av   Gd   Mn   No 
    ##  418  276  239 1904

``` r
plot(as.factor(data$BsmtExposure),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
data$BsmtExposure[is.na(data$BsmtExposure)] <- "None"
plot(as.factor(data$BsmtExposure),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-25-2.png)

``` r
Exposure<-c("None"=0,"No"=1,'Mn'=2,'Av'=3,'Gd'=4)
data$BsmtExposure<-revalue(data$BsmtExposure,Exposure)
table(data$BsmtExposure)
```

    ## 
    ##    0    1    2    3    4 
    ##   82 1904  239  418  276

``` r
data$BsmtExposure<-as.integer(data$BsmtExposure)
plot(as.factor(data$BsmtExposure),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-25-3.png)

**BsmtQual:Evaluates the height of the basement**
-------------------------------------------------

``` r
table(data$BsmtQual)
```

    ## 
    ##   Ex   Fa   Gd   TA 
    ##  258   88 1209 1283

``` r
plot(as.factor(data$BsmtQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
data$BsmtQual[is.na(data$BsmtQual)] <- "None"
plot(as.factor(data$BsmtQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-26-2.png)

``` r
data$BsmtQual<-revalue(data$BsmtQual,Qual_Cond)
table(data$BsmtQual)
```

    ## 
    ##    0    2    3    4    5 
    ##   81   88 1283 1209  258

``` r
data$BsmtQual<-as.integer(data$BsmtQual)
plot(as.factor(data$BsmtQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-26-3.png)

**BsmtFinType1:Rating of basement finished area**
-------------------------------------------------

``` r
table(data$BsmtFinType1)
```

    ## 
    ## ALQ BLQ GLQ LwQ Rec Unf 
    ## 429 269 849 154 288 851

``` r
plot(as.factor(data$BsmtFinType1),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "None"
plot(as.factor(data$BsmtFinType1),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-27-2.png)

``` r
Bsm_type<-c("None"=0,"Unf"=1,'LwQ'=2,'Rec'=3,'BLQ'=4,'ALQ'=5,'GLQ'=6)
data$BsmtFinType1<-revalue(data$BsmtFinType1,Bsm_type)
table(data$BsmtFinType1)
```

    ## 
    ##   0   1   2   3   4   5   6 
    ##  79 851 154 288 269 429 849

``` r
data$BsmtFinType1<-as.integer(data$BsmtFinType1)
plot(as.factor(data$BsmtFinType1),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-27-3.png)

**BsmtFinType2:Rating of basement finished area (if multiple types)**
---------------------------------------------------------------------

``` r
table(data$BsmtFinType2)
```

    ## 
    ##  ALQ  BLQ  GLQ  LwQ  Rec  Unf 
    ##   52   68   34   87  105 2493

``` r
plot(as.factor(data$BsmtFinType2),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "None"
plot(as.factor(data$BsmtFinType2),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-28-2.png)

``` r
data$BsmtFinType2<-revalue(data$BsmtFinType2,Bsm_type)
table(data$BsmtFinType2)
```

    ## 
    ##    0    1    2    3    4    5    6 
    ##   80 2493   87  105   68   52   34

``` r
data$BsmtFinType2<-as.integer(data$BsmtFinType2)
plot(as.factor(data$BsmtFinType2),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-28-3.png)

**MasVnrType: Masonry veneer type (walls).**
--------------------------------------------

``` r
table(data$MasVnrType)
```

    ## 
    ##  BrkCmn BrkFace    None   Stone 
    ##      25     879    1742     249

``` r
plot(as.factor(data$MasVnrType),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
data$MasVnrType[is.na(data$MasVnrType)] <- "None" #Replace all Na's with 'none'
Masonry <- c('None'=0, 'BrkCmn'=1, 'BrkFace'=2, 'Stone'=3)
data$MasVnrType<-revalue(data$MasVnrType,Masonry)
table(data$MasVnrType)
```

    ## 
    ##    0    1    2    3 
    ## 1766   25  879  249

``` r
data$MasVnrType<-as.integer(data$MasVnrType)
plot(as.factor(data$MasVnrType),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-29-2.png)

**MasVnrArea: Masonry veneer area in square feet**
--------------------------------------------------

``` r
plot(data$MasVnrArea,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0
data$MasVnrArea<-as.numeric(data$MasVnrArea)
plot(data$MasVnrArea,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-30-2.png)

**MSZoning: Identifies the general zoning classification of the sale.**
-----------------------------------------------------------------------

``` r
table(data$MSZoning)
```

    ## 
    ## C (all)      FV      RH      RL      RM 
    ##      25     139      26    2265     460

``` r
plot(as.factor(data$MSZoning),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-31-1.png)

How we can find the missing values? Let’s check out the MSSubClass

``` r
data[is.na(data$MSZoning),c("MSZoning",'MSSubClass')]
```

    ##      MSZoning MSSubClass
    ## 1916     <NA>         30
    ## 2217     <NA>         20
    ## 2251     <NA>         70
    ## 2905     <NA>         20

``` r
table(data[,c("MSZoning",'MSSubClass')])
```

    ##          MSSubClass
    ## MSZoning    20   30   40   45   50   60   70   75   80   85   90  120  150
    ##   C (all)    3    8    0    0    7    0    4    0    0    0    0    0    0
    ##   FV        34    0    0    0    0   43    0    0    0    0    0   19    0
    ##   RH         4    2    0    1    2    0    3    0    0    0    4    6    0
    ##   RL      1016   61    4    6  159  529   57    9  115   47   92  117    1
    ##   RM        20   67    2   11  119    3   63   14    3    1   13   40    0
    ##          MSSubClass
    ## MSZoning   160  180  190
    ##   C (all)    0    0    3
    ##   FV        43    0    0
    ##   RH         0    0    4
    ##   RL        21    0   31
    ##   RM        64   17   23

And now the correlations with numeric variables.

``` r
library(corrplot)
cor(data$MSSubClass,data[,sapply(data, is.numeric)]) #
```

    ##               Id MSSubClass LotFrontage  LotArea OverallQual OverallCond
    ## [1,] 0.008930622          1   -0.389469 -0.20173  0.03363797 -0.06562504
    ##       YearBuilt YearRemodAdd  MasVnrType  MasVnrArea   BsmtQual
    ## [1,] 0.03440874   0.04331491 0.007170029 0.006309137 0.06525105
    ##          BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## [1,] -0.003662076   0.05929913   0.05162957         NA  -0.03622638
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF  X1stFlrSF X2ndFlrSF LowQualFinSF
    ## [1,]         NA        NA          NA -0.2486415 0.3093091   0.02648179
    ##       GrLivArea BsmtFullBath BsmtHalfBath  FullBath  HalfBath BedroomAbvGr
    ## [1,] 0.07167745           NA           NA 0.1391396 0.1787502 -0.008796152
    ##      KitchenAbvGr TotRmsAbvGrd  Fireplaces FireplaceQu GarageFinish
    ## [1,]    0.2601555   0.04050946 -0.05515069 -0.05154243  -0.03689839
    ##      GarageCars GarageArea  GarageQual  WoodDeckSF OpenPorchSF
    ## [1,]         NA         NA -0.09573865 -0.01765413  -0.0159232
    ##      EnclosedPorch  X3SsnPorch ScreenPorch     PoolArea       PoolQC
    ## [1,]   -0.02086725 -0.03752892 -0.04918147 -0.003079582 -0.001679514
    ##          MiscVal       MoSold     YrSold SalePrice
    ## [1,] -0.02886686 -0.001231139 -0.0150278        NA

It is not higly correlated with other numeric variables.<br> So by
examining the above table, we will replace MSSubClass=20 with RL ,
MSSubClass=70 and MSSubClass=30 with RM.

``` r
data$MSZoning[is.na(data$MSZoning)]<-c('RM','RL','RM','RL')
data[is.na(data$MSZoning),c("MSZoning",'MSSubClass')]
```

    ## [1] MSZoning   MSSubClass
    ## <0 rows> (or 0-length row.names)

``` r
data$MSZoning<-as.factor(data$MSZoning)
```

**Utilities: Type of utilities available**
------------------------------------------

We don’t need this variable for prediction , as there is only one house
“NoSeWa”.

``` r
table(data$Utilities)
```

    ## 
    ## AllPub NoSeWa 
    ##   2916      1

``` r
data <- subset(data, select = -Utilities)
```

**BsmtFullBath:Basement full bathrooms**
----------------------------------------

``` r
table(data$BsmtFullBath)
```

    ## 
    ##    0    1    2    3 
    ## 1705 1172   38    2

Let’s find all the basement variables below.

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$BsmtFullBath),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0         NA            0
    ## 2189        0        0            0            0          0            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA        NA          NA           NA           NA
    ## 2189          0         0           0           NA           NA

We conclude that these variables are higly corralated and so we will
replace the Na’s with zero value

``` r
data$BsmtFullBath[is.na(data$BsmtFullBath)]<-0 # Replace missing value with zero
plot(as.factor(data$BsmtFullBath),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-39-1.png)

``` r
data$BsmtFullBath<-as.integer(data$BsmtFullBath)
```

**BsmtHalfBath: Basement half bathrooms**
-----------------------------------------

We proceed with the same way as above.

``` r
table(data$BsmtHalfBath)
```

    ## 
    ##    0    1    2 
    ## 2742  171    4

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$BsmtHalfBath),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0         NA            0
    ## 2189        0        0            0            0          0            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA        NA          NA            0           NA
    ## 2189          0         0           0            0           NA

``` r
data$BsmtHalfBath[is.na(data$BsmtHalfBath)]<-0 # Replace missing value with zero
plot(as.factor(data$BsmtHalfBath),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-40-1.png)

``` r
data$BsmtHalfBath<-as.integer(data$BsmtHalfBath)
#we can see the correlation 
cor(data$BsmtHalfBath,data$BsmtFullBath) #It is not correlated
```

    ## [1] -0.1486548

**Functional: Home functionality (Assume typical unless deductions are warranted)**
-----------------------------------------------------------------------------------

``` r
table(data$Functional)
```

    ## 
    ## Maj1 Maj2 Min1 Min2  Mod  Sev  Typ 
    ##   19    9   65   70   35    2 2717

``` r
plot(as.factor(data$Functional),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-41-1.png)

We will replace the Na’s with the most common value 7, which is typical
functionality

``` r
data$Functional[is.na(data$Functional)]<-7
Functional<-c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)
data$Functional <- as.integer(revalue(data$Functional, Functional))
table(data$Functional)
```

    ## 
    ##    1    2    3    4    5    6    7 
    ##    2    9   19   35   70   65 2719

**GarageCars Size of garage in car capacity**
---------------------------------------------

``` r
table(as.factor(data$GarageCars))
```

    ## 
    ##    0    1    2    3    4    5 
    ##  157  776 1594  374   16    1

``` r
plot(as.factor(data$GarageCars))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-43-1.png)

``` r
plot(as.factor(data$GarageCars),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-43-2.png)

``` r
paste(colnames(select(data,contains("Garage"))),collapse="','")
```

    ## [1] "GarageType','GarageFinish','GarageCars','GarageArea','GarageQual"

``` r
data[is.na(data$GarageCars),c('GarageType','GarageFinish','GarageCars','GarageArea','GarageQual')]
```

    ##      GarageType GarageFinish GarageCars GarageArea GarageQual
    ## 2577     Detchd            0         NA         NA          0

``` r
data$GarageCars[is.na(data$GarageCars)]<-0 # Replace missing value with zero
data$GarageCars<-as.integer(data$GarageCars)
```

**GarageArea**
--------------

``` r
paste(colnames(select(data,contains("Garage"))),collapse="','")
```

    ## [1] "GarageType','GarageFinish','GarageCars','GarageArea','GarageQual"

``` r
data[is.na(data$GarageArea),c('GarageType','GarageFinish','GarageCars','GarageArea','GarageQual')]
```

    ##      GarageType GarageFinish GarageCars GarageArea GarageQual
    ## 2577     Detchd            0          0         NA          0

``` r
data$GarageArea[is.na(data$GarageArea)]<-0 # Replace missing value with zero
data$GarageArea<-as.integer(data$GarageArea)
plot(data$GarageArea,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-44-1.png)

Let’s find its correlation with other numeric values

``` r
cor(data[,sapply(data, is.numeric)],data$GarageArea)
```

    ##                       [,1]
    ## Id            -0.009851054
    ## MSSubClass    -0.103534194
    ## LotFrontage    0.338418737
    ## LotArea        0.213180376
    ## OverallQual    0.565179205
    ## OverallCond   -0.154311762
    ## YearBuilt      0.481328133
    ## YearRemodAdd   0.375892265
    ## MasVnrType     0.368002725
    ## MasVnrArea     0.371071383
    ## BsmtQual       0.422730492
    ## BsmtCond       0.136238741
    ## BsmtExposure   0.280947231
    ## BsmtFinType1   0.235311964
    ## BsmtFinSF1              NA
    ## BsmtFinType2   0.004557102
    ## BsmtFinSF2              NA
    ## BsmtUnfSF               NA
    ## TotalBsmtSF             NA
    ## X1stFlrSF      0.491996421
    ## X2ndFlrSF      0.127463905
    ## LowQualFinSF  -0.053430694
    ## GrLivArea      0.484547335
    ## BsmtFullBath   0.185151379
    ## BsmtHalfBath  -0.021217905
    ## FullBath       0.407519909
    ## HalfBath       0.179396010
    ## BedroomAbvGr   0.073723102
    ## KitchenAbvGr  -0.057647570
    ## TotRmsAbvGrd   0.328627638
    ## Functional     0.068753326
    ## Fireplaces     0.295175204
    ## FireplaceQu    0.342810880
    ## GarageFinish   0.511991097
    ## GarageCars     0.889890224
    ## GarageArea     1.000000000
    ## GarageQual     0.552595491
    ## WoodDeckSF     0.237384942
    ## OpenPorchSF    0.232875830
    ## EnclosedPorch -0.108382360
    ## X3SsnPorch     0.029493114
    ## ScreenPorch    0.062552079
    ## PoolArea       0.053053039
    ## PoolQC         0.063360935
    ## MiscVal        0.008506144
    ## MoSold         0.041181698
    ## YrSold        -0.012986182
    ## SalePrice               NA

It is correlated with GarageCars (Rsq = 0.889) <br> Should we drop
GarageArea or GarageCars? <br> We will examine which is less corellated
with Sales Price.

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageArea'])
```

    ## [1] 0.6233849

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'GarageCars']) 
```

    ## [1] 0.6403833

We’ll drop the GarageArea variable, since it is less corellated with
sales Price

``` r
data <- subset(data, select = -GarageArea)
```

**KitchenQual: Kitchen quality**
--------------------------------

``` r
table(data$KitchenQual)
```

    ## 
    ##   Ex   Fa   Gd   TA 
    ##  205   70 1151 1492

``` r
data$KitchenQual[is.na(data$KitchenQual)] <- 'TA' #replace with most common value
KitchenQual <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$KitchenQual<-revalue(data$KitchenQual,KitchenQual)
data$KitchenQual<-as.integer(data$KitchenQual)
```

**TotalBsmtSF : Total square feet of basement area**
----------------------------------------------------

``` r
plot(data$TotalBsmtSF,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-49-1.png)

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'TotalBsmtSF'])
```

    ## [1] 0.6137915

``` r
data$TotalBsmtSF[is.na(data$TotalBsmtSF)]
```

    ## [1] NA

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$TotalBsmtSF),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0         NA            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA        NA          NA            0            0

``` r
data$TotalBsmtSF[is.na(data$TotalBsmtSF)]<-0 # Replace missing value with zero
```

**BsmtFinSF1: Type 1 finished square feet**
-------------------------------------------

``` r
plot(data$BsmtFinSF1,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-50-1.png)

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtFinSF1'])
```

    ## [1] 0.3867829

``` r
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]
```

    ## [1] NA

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$BsmtFinSF1),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0         NA            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA        NA           0            0            0

``` r
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]<-0 # Replace missing value with zero
```

**BsmtUnfSF: Unfinished square feet of basement area**
------------------------------------------------------

``` r
plot(data$BsmtUnfSF,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-51-1.png)

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtUnfSF'])
```

    ## [1] 0.2142805

``` r
data$BsmtUnfSF[is.na(data$BsmtUnfSF)]
```

    ## [1] NA

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$BsmtUnfSF),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0          0            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA        NA           0            0            0

``` r
data$BsmtUnfSF[is.na(data$BsmtUnfSF)]<-0 # Replace missing value with zero
```

Let’s see if it is correlated with another variable

``` r
cor(data$BsmtUnfSF,data[,sapply(data, is.numeric)])
```

    ##               Id MSSubClass LotFrontage    LotArea OverallQual OverallCond
    ## [1,] -0.01479023 -0.1255609   0.1063813 0.02158978   0.2756429  -0.1386875
    ##      YearBuilt YearRemodAdd MasVnrType MasVnrArea  BsmtQual  BsmtCond
    ## [1,] 0.1307862    0.1657697  0.1086214 0.08817341 0.2358125 0.1748441
    ##      BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2 BsmtFinSF2
    ## [1,]  -0.04344334   -0.3881147 -0.4767712   -0.1902104         NA
    ##      BsmtUnfSF TotalBsmtSF X1stFlrSF     X2ndFlrSF LowQualFinSF GrLivArea
    ## [1,]         1   0.4128104 0.2967876 -3.236541e-05   0.04694399 0.2343882
    ##      BsmtFullBath BsmtHalfBath  FullBath    HalfBath BedroomAbvGr
    ## [1,]   -0.3976477   -0.1068404 0.2735304 -0.03545913    0.1836307
    ##      KitchenAbvGr KitchenQual TotRmsAbvGrd Functional  Fireplaces
    ## [1,]   0.06505947   0.1892703    0.2480153 0.03766712 0.005216017
    ##      FireplaceQu GarageFinish GarageCars   GarageQual  WoodDeckSF
    ## [1,]   0.1097402    0.0847243  0.1807326 -0.009866555 -0.03896365
    ##      OpenPorchSF EnclosedPorch   X3SsnPorch ScreenPorch    PoolArea
    ## [1,]   0.1200272   0.005161546 -0.005763676 -0.04901855 -0.03223645
    ##           PoolQC     MiscVal     MoSold      YrSold SalePrice
    ## [1,] -0.02994952 -0.01045017 0.02295447 -0.03807293        NA

It is correlated with BsmtFinSF1 (-0.476). <br> We continue with the
next variable

**BsmtFinSF2: Type 2 finished square feet**
-------------------------------------------

``` r
plot(data$BsmtFinSF2,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-53-1.png)

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'BsmtFinSF2'])
```

    ## [1] -0.01095189

``` r
data$BsmtFinSF2[is.na(data$BsmtFinSF2)]
```

    ## [1] NA

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
data[is.na(data$BsmtFinSF2),c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')]
```

    ##      BsmtQual BsmtCond BsmtExposure BsmtFinType1 BsmtFinSF1 BsmtFinType2
    ## 2121        0        0            0            0          0            0
    ##      BsmtFinSF2 BsmtUnfSF TotalBsmtSF BsmtFullBath BsmtHalfBath
    ## 2121         NA         0           0            0            0

``` r
data$BsmtFinSF2[is.na(data$BsmtFinSF2)]<-0 # Replace missing value with zero
cor(data$BsmtFinSF1,data$BsmtFinSF2)
```

    ## [1] -0.05493841

We will probably drop this variable,let’s see if it is correlated with
another

``` r
cor(data$BsmtFinSF2,data[,sapply(data, is.numeric)])
```

    ##              Id  MSSubClass LotFrontage    LotArea OverallQual OverallCond
    ## [1,] 0.01817004 -0.07243126  0.04032958 0.08410715  -0.0426052  0.04135923
    ##        YearBuilt YearRemodAdd  MasVnrType  MasVnrArea    BsmtQual
    ## [1,] -0.02750704  -0.06195898 -0.02374806 -0.01457983 -0.01267815
    ##        BsmtCond BsmtExposure BsmtFinType1  BsmtFinSF1 BsmtFinType2
    ## [1,] 0.07567332    0.1025202   0.02628983 -0.05493841    0.7992987
    ##      BsmtFinSF2  BsmtUnfSF TotalBsmtSF  X1stFlrSF   X2ndFlrSF LowQualFinSF
    ## [1,]          1 -0.2380433  0.08956092 0.08438942 -0.09765352  -0.00491318
    ##        GrLivArea BsmtFullBath BsmtHalfBath    FullBath    HalfBath
    ## [1,] -0.01774713    0.1629569   0.09953007 -0.07531358 -0.03236835
    ##      BedroomAbvGr KitchenAbvGr KitchenQual TotRmsAbvGrd  Functional
    ## [1,]   -0.0311113   -0.0377576  -0.0453763  -0.04824545 -0.06372228
    ##      Fireplaces FireplaceQu GarageFinish  GarageCars GarageQual WoodDeckSF
    ## [1,] 0.06570734 0.004498087  0.009004049 -0.01447615 0.06432276  0.0984622
    ##       OpenPorchSF EnclosedPorch  X3SsnPorch ScreenPorch   PoolArea
    ## [1,] -0.005804508    0.03277453 -0.02326797  0.06332908 0.04452989
    ##          PoolQC      MiscVal     MoSold      YrSold SalePrice
    ## [1,] 0.02187298 -0.005129682 -0.0095096 0.008866935        NA

Correlated with BsmtFinType2 <br> Now we see the correlations between
basement variables

``` r
paste(colnames(select(data,contains("Bsmt"))),collapse="','")
```

    ## [1] "BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath"

``` r
cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')])
```

    ##                  BsmtQual   BsmtCond BsmtExposure BsmtFinType1
    ## BsmtQual      1.000000000 0.62515749   0.43626210   0.41987055
    ## BsmtCond      0.625157488 1.00000000   0.26430401   0.30704435
    ## BsmtExposure  0.436262102 0.26430401   1.00000000   0.36706771
    ## BsmtFinType1  0.419870554 0.30704435   0.36706771   1.00000000
    ## BsmtFinSF1    0.337365829 0.16986992   0.39857979   0.70298124
    ## BsmtFinType2  0.101212924 0.22661106   0.13262795   0.09950210
    ## BsmtFinSF2   -0.012678151 0.07567332   0.10252021   0.02628983
    ## BsmtUnfSF     0.235812550 0.17484410  -0.04344334  -0.38811470
    ## TotalBsmtSF   0.578576765 0.37870825   0.40769434   0.34937955
    ## BsmtFullBath  0.259072937 0.16954774   0.33859998   0.58455430
    ## BsmtHalfBath -0.006908995 0.04755341   0.08910495   0.08452306
    ##                BsmtFinSF1 BsmtFinType2  BsmtFinSF2   BsmtUnfSF TotalBsmtSF
    ## BsmtQual      0.337365829  0.101212924 -0.01267815  0.23581255  0.57857677
    ## BsmtCond      0.169869918  0.226611061  0.07567332  0.17484410  0.37870825
    ## BsmtExposure  0.398579791  0.132627953  0.10252021 -0.04344334  0.40769434
    ## BsmtFinType1  0.702981238  0.099502102  0.02628983 -0.38811470  0.34937955
    ## BsmtFinSF1    1.000000000 -0.008849972 -0.05493841 -0.47677116  0.53665002
    ## BsmtFinType2 -0.008849972  1.000000000  0.79929873 -0.19021038  0.10785668
    ## BsmtFinSF2   -0.054938405  0.799298733  1.00000000 -0.23804333  0.08956092
    ## BsmtUnfSF    -0.476771163 -0.190210384 -0.23804333  1.00000000  0.41281039
    ## TotalBsmtSF   0.536650022  0.107856678  0.08956092  0.41281039  1.00000000
    ## BsmtFullBath  0.639038348  0.187877971  0.16295695 -0.39764765  0.32625176
    ## BsmtHalfBath  0.078500167  0.109564023  0.09953007 -0.10684037  0.01277996
    ##              BsmtFullBath BsmtHalfBath
    ## BsmtQual        0.2590729 -0.006908995
    ## BsmtCond        0.1695477  0.047553407
    ## BsmtExposure    0.3386000  0.089104955
    ## BsmtFinType1    0.5845543  0.084523061
    ## BsmtFinSF1      0.6390383  0.078500167
    ## BsmtFinType2    0.1878780  0.109564023
    ## BsmtFinSF2      0.1629569  0.099530071
    ## BsmtUnfSF      -0.3976477 -0.106840367
    ## TotalBsmtSF     0.3262518  0.012779965
    ## BsmtFullBath    1.0000000 -0.148654812
    ## BsmtHalfBath   -0.1486548  1.000000000

We will create a corrplot for better understanding the correlations

``` r
corrplot(cor(data[,c('BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF','TotalBsmtSF','BsmtFullBath','BsmtHalfBath')])
,method = "square")
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-56-1.png)

The basement quality and basement condition (which are important
variables) are not correlated with BsmtHalfBath and BsmtFinSF2. So we
will drop these two variables

``` r
data <- subset(data, select = -BsmtFinSF2)
data <- subset(data, select = -BsmtHalfBath)
```

**Now that we have taken care of Na’s let’s see which are the numeric variables.**
----------------------------------------------------------------------------------

``` r
colnames(data[,sapply(data, is.numeric)]) #check out which columns are numeric 
```

    ##  [1] "Id"            "MSSubClass"    "LotFrontage"   "LotArea"      
    ##  [5] "OverallQual"   "OverallCond"   "YearBuilt"     "YearRemodAdd" 
    ##  [9] "MasVnrType"    "MasVnrArea"    "BsmtQual"      "BsmtCond"     
    ## [13] "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"    "BsmtFinType2" 
    ## [17] "BsmtUnfSF"     "TotalBsmtSF"   "X1stFlrSF"     "X2ndFlrSF"    
    ## [21] "LowQualFinSF"  "GrLivArea"     "BsmtFullBath"  "FullBath"     
    ## [25] "HalfBath"      "BedroomAbvGr"  "KitchenAbvGr"  "KitchenQual"  
    ## [29] "TotRmsAbvGrd"  "Functional"    "Fireplaces"    "FireplaceQu"  
    ## [33] "GarageFinish"  "GarageCars"    "GarageQual"    "WoodDeckSF"   
    ## [37] "OpenPorchSF"   "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"  
    ## [41] "PoolArea"      "PoolQC"        "MiscVal"       "MoSold"       
    ## [45] "YrSold"        "SalePrice"

Let’s see also some correlations. <br> We will find the numeric
variables that have the highest correlation with the Saleprice variable
and examine them further.

``` r
corrplot(cor(na.omit(data[,sapply(data, is.numeric)])),method = "square")
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-59-1.png)

``` r
paste(which(cor(data[1:1459,sapply(data, is.numeric)],data[1:1459,'SalePrice'])>0.5 | cor(data[1:1459,sapply(data, is.numeric)],data[1:1459,'SalePrice'])<(-0.5)),collapse=',') # Check for high or low correlations
```

    ## [1] "5,7,8,11,18,19,22,24,28,29,32,33,34,46"

So the numeric variables that have the highest correlation with the
Saleprice are the following:

``` r
colnames(data[,sapply(data,is.numeric)][c(5,7,8,11,18,19,22,24,28,31,32,33,45)])
```

    ##  [1] "OverallQual"  "YearBuilt"    "YearRemodAdd" "BsmtQual"    
    ##  [5] "TotalBsmtSF"  "X1stFlrSF"    "GrLivArea"    "FullBath"    
    ##  [9] "KitchenQual"  "Fireplaces"   "FireplaceQu"  "GarageFinish"
    ## [13] "YrSold"

**MSSubClass: Identifies the type of dwelling involved in the sale.**
---------------------------------------------------------------------

``` r
plot(data$MSSubClass,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-61-1.png)

**In reality this is a factor and not numeric.**

``` r
table(as.factor(data$MSSubClass))
```

    ## 
    ##   20   30   40   45   50   60   70   75   80   85   90  120  150  160  180 
    ## 1079  139    6   18  287  575  128   23  118   48  109  182    1  128   17 
    ##  190 
    ##   61

``` r
data$MSSubClass<-as.factor(data$MSSubClass) # Change into factor variable
```

**OverallQual: Rates the overall material and finish of the house**
-------------------------------------------------------------------

``` r
table(as.factor(data$OverallQual))
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ##   4  13  40 226 825 731 600 342 107  31

``` r
plot(as.factor(data$OverallQual),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-63-1.png)

``` r
data$OverallQual<-as.integer(data$OverallQual) # Change into integer variable
table(data$OverallQual)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10 
    ##   4  13  40 226 825 731 600 342 107  31

This seems very important variable for our predictions. Let’s see what
will happen if we run a regression just with this variable.

``` r
linreg_qual <- lm(SalePrice ~ OverallQual, data=data)
summary(linreg_qual)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ OverallQual, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -198152  -29409   -1845   21463  396848 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -96206.1     5756.4  -16.71   <2e-16 ***
    ## OverallQual  45435.8      920.4   49.36   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 48620 on 1458 degrees of freedom
    ##   (1459 observations deleted due to missingness)
    ## Multiple R-squared:  0.6257, Adjusted R-squared:  0.6254 
    ## F-statistic:  2437 on 1 and 1458 DF,  p-value: < 2.2e-16

We have a very high R-sqaure of 0.66! Just this variable is able to
explain much variance in our model.

**OverallCond: Rates the overall condition of the house**
---------------------------------------------------------

``` r
table(as.factor(data$OverallCond))
```

    ## 
    ##    1    2    3    4    5    6    7    8    9 
    ##    7   10   50  101 1645  531  390  144   41

``` r
plot(as.factor(data$OverallCond),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-65-1.png)

``` r
data$OverallCond<-as.integer(data$OverallCond)
table(data$OverallCond)
```

    ## 
    ##    1    2    3    4    5    6    7    8    9 
    ##    7   10   50  101 1645  531  390  144   41

**YearBuilt: Original construction date**
-----------------------------------------

``` r
plot(as.factor(data$YearBuilt))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-66-1.png)

``` r
table(as.factor(data$YearBuilt))
```

    ## 
    ## 1872 1875 1879 1880 1882 1885 1890 1892 1893 1895 1896 1898 1900 1901 1902 
    ##    1    1    1    5    1    2    7    2    1    3    1    1   29    2    1 
    ## 1904 1905 1906 1907 1908 1910 1911 1912 1913 1914 1915 1916 1917 1918 1919 
    ##    1    3    1    1    2   43    1    5    1    8   24   10    3   10    5 
    ## 1920 1921 1922 1923 1924 1925 1926 1927 1928 1929 1930 1931 1932 1934 1935 
    ##   57   11   16   17   16   34   19    9    9    8   26    7    5    5   13 
    ## 1936 1937 1938 1939 1940 1941 1942 1945 1946 1947 1948 1949 1950 1951 1952 
    ##   11    9   13   20   36   23    6   15   15   11   27   18   38   18   18 
    ## 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 
    ##   24   43   34   39   35   48   43   37   34   35   35   33   34   35   41 
    ## 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 
    ##   45   28   42   39   40   21   23   25   54   57   39   21   23    9    7 
    ## 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 
    ##    8   19    7   10    8   15    8   19   12   27   39   37   31   34   35 
    ## 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 
    ##   46   52   48   35   47   88   99  142  138  109   49   25    3

``` r
plot(as.factor(data$YearBuilt),data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-66-2.png)

``` r
cor(data[1:1459,'SalePrice'],data[1:1459,'YearBuilt'])
```

    ## [1] 0.5228769

**YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)**
----------------------------------------------------------------------------------------

``` r
plot(as.factor(data$YearRemodAdd))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-67-1.png)

``` r
table(as.factor(data$YearRemodAdd))
```

    ## 
    ## 1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 
    ##  361   14   15   20   28   25   30   20   34   30   29   24   26   30   26 
    ## 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 
    ##   28   27   34   39   26   44   31   35   21   19   30   48   46   36   24 
    ## 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 
    ##   26   12    9   11   19   14   12   16   15   18   29   29   32   43   53 
    ## 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 
    ##   56   59   49   77   60  104   49   82   99  111  141  202  164   81   34 
    ## 2010 
    ##   13

Interestingly, we observe that after year 2006 there is a sudden
decrease in remodelings.

``` r
plot(data$YearRemodAdd,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-68-1.png)

**Pool Area**
-------------

``` r
table(data$PoolArea) # We'll drop this variable (most observations belong to one parameter)
```

    ## 
    ##    0  144  228  368  444  480  512  519  555  561  576  648  738  800 
    ## 2906    1    1    1    1    1    1    1    1    1    1    1    1    1

``` r
data <- subset(data, select = -c(PoolArea))
```

### **PoolQC**

``` r
table(data$PoolQC) #we'll drop this variable
```

    ## 
    ##    0    2    4    5 
    ## 2909    2    4    4

``` r
data <- subset(data, select = -c(PoolQC))
```

**FullBath: Full bathrooms above grade**
----------------------------------------

We will convert this to integer

``` r
table(data$FullBath) 
```

    ## 
    ##    0    1    2    3    4 
    ##   12 1309 1530   64    4

``` r
FullBath <- as.integer(data$FullBath)
```

**TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)**
----------------------------------------------------------------------

We will convert this to integer

``` r
table(data$TotRmsAbvGrd) 
```

    ## 
    ##   2   3   4   5   6   7   8   9  10  11  12  13  14  15 
    ##   1  25 196 583 844 649 347 143  80  32  16   1   1   1

``` r
TotRmsAbvGrd <- as.integer(data$TotRmsAbvGrd)
```

**GarageCars: Size of garage in car capacity**
----------------------------------------------

We will convert this to integer

``` r
table(data$GarageCars) 
```

    ## 
    ##    0    1    2    3    4    5 
    ##  158  776 1594  374   16    1

``` r
GarageCars <- as.integer(data$GarageCars)
```

**GrLivArea: Above grade (ground) living area square feet**
-----------------------------------------------------------

``` r
plot(data$GrLivArea,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-74-1.png)

``` r
which(data[,"GrLivArea"]>4600) # outliers
```

    ## [1]  524 1299 2550

**Afer examining the above numeric variables, we also need to examine the remaining character variables.**
----------------------------------------------------------------------------------------------------------

``` r
colnames(data[,sapply(data, is.character)]) 
```

    ##  [1] "Street"        "LotShape"      "LandContour"   "LotConfig"    
    ##  [5] "LandSlope"     "Neighborhood"  "Condition1"    "Condition2"   
    ##  [9] "BldgType"      "HouseStyle"    "RoofStyle"     "RoofMatl"     
    ## [13] "Exterior1st"   "Exterior2nd"   "ExterQual"     "ExterCond"    
    ## [17] "Foundation"    "Heating"       "HeatingQC"     "CentralAir"   
    ## [21] "Electrical"    "PavedDrive"    "SaleType"      "SaleCondition"

**Street: Type of road access to property**
-------------------------------------------

``` r
plot(as.factor(data$Street))
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-76-1.png)

``` r
table(data$Street)
```

    ## 
    ## Grvl Pave 
    ##   12 2907

``` r
#Only 12 Gravels      
#removing Street variable 
data <- subset(data, select = -Street)
```

**Neighborhood**
----------------

``` r
data$Neighborhood <- as.factor(data$Neighborhood) # Convert to factor
table(data$Neighborhood)
```

    ## 
    ## Blmngtn Blueste  BrDale BrkSide ClearCr CollgCr Crawfor Edwards Gilbert 
    ##      28      10      30     108      44     267     103     194     165 
    ##  IDOTRR MeadowV Mitchel   NAmes NoRidge NPkVill NridgHt  NWAmes OldTown 
    ##      93      37     114     443      71      23     166     131     239 
    ##  Sawyer SawyerW Somerst StoneBr   SWISU  Timber Veenker 
    ##     151     125     182      51      48      72      24

``` r
plot(data$Neighborhood,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-77-1.png) As
expected, Neighborhood seems to affect the price of a house.

**Foundation: Type of foundation**
----------------------------------

``` r
data$Foundation <- as.factor(data$Foundation) # Convert to factor
table(data$Foundation)
```

    ## 
    ## BrkTil CBlock  PConc   Slab  Stone   Wood 
    ##    311   1235   1308     49     11      5

``` r
plot(data$Foundation,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-78-1.png)

**Condition1: Proximity to various conditions**
-----------------------------------------------

``` r
data$Condition1 <- as.factor(data$Condition1) # Convert to factor
table(data$Condition1)
```

    ## 
    ## Artery  Feedr   Norm   PosA   PosN   RRAe   RRAn   RRNe   RRNn 
    ##     92    164   2511     20     39     28     50      6      9

``` r
plot(data$Condition1,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-79-1.png)

**LotShape: General shape of property**
---------------------------------------

``` r
data$LotShape<-as.integer(revalue(data$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3))) # Convert to integer
table(data$LotShape)
```

    ## 
    ##    0    1    2    3 
    ##   16   76  968 1859

``` r
plot(data$LotShape,data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-80-1.png)

**ExterQual: Evaluates the quality of the material on the exterior**
--------------------------------------------------------------------

``` r
table(data$ExterQual)
```

    ## 
    ##   Ex   Fa   Gd   TA 
    ##  107   35  979 1798

``` r
ExterQual <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
data$ExterQual<-revalue(data$ExterQual,ExterQual)
data$ExterQual<-as.integer(data$ExterQual)
```

**Prediction**
==============

**Using multiple regression**
-----------------------------

Now that we have taken care of most variables, we will use a multiple
regression to predict the prices on the test set and submit our
predictions to Kaggle. <br>

First, let’s examine the skewness of variable we want to predict

``` r
qqnorm(data$SalePrice)
qqline(data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-82-1.png)

Our variable is not normally distributed so we will take the log of
SalePrice.

``` r
data$SalePrice <- log(data$SalePrice)
qqnorm(data$SalePrice)
qqline(data$SalePrice)
```

![](House_Prices_files/figure-markdown_github/unnamed-chunk-83-1.png)

The log transformation seems to have solved this problem.

**Id**
------

We will keep id in a vector called Id

``` r
Id<-data[is.na(data$SalePrice),"Id"]
```

**We will now split our dataset in train and test set**
-------------------------------------------------------

``` r
train_data <- data[!is.na(data$SalePrice),]
test_data <- data[is.na(data$SalePrice),]
```

``` r
fit <- lm(SalePrice ~. , data=train_data)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ ., data = train_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.71073 -0.04598  0.00000  0.05643  0.71073 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           8.997e+00  4.756e+00   1.892 0.058723 .  
    ## Id                   -4.495e-06  7.107e-06  -0.632 0.527179    
    ## MSSubClass30         -5.778e-02  2.160e-02  -2.675 0.007570 ** 
    ## MSSubClass40         -1.034e-01  8.005e-02  -1.291 0.196806    
    ## MSSubClass45         -2.370e-01  1.038e-01  -2.283 0.022607 *  
    ## MSSubClass50          3.369e-02  3.993e-02   0.844 0.398917    
    ## MSSubClass60         -3.389e-02  3.523e-02  -0.962 0.336285    
    ## MSSubClass70          9.339e-03  3.802e-02   0.246 0.806001    
    ## MSSubClass75         -5.817e-02  6.801e-02  -0.855 0.392598    
    ## MSSubClass80         -4.877e-03  5.282e-02  -0.092 0.926455    
    ## MSSubClass85          1.894e-02  4.820e-02   0.393 0.694414    
    ## MSSubClass90         -2.136e-02  3.271e-02  -0.653 0.513864    
    ## MSSubClass120        -7.602e-02  6.722e-02  -1.131 0.258319    
    ## MSSubClass160        -1.607e-01  8.023e-02  -2.003 0.045435 *  
    ## MSSubClass180        -5.314e-02  8.928e-02  -0.595 0.551825    
    ## MSSubClass190        -3.885e-02  1.179e-01  -0.329 0.741893    
    ## MSZoningFV            4.655e-01  5.547e-02   8.393  < 2e-16 ***
    ## MSZoningRH            4.599e-01  5.472e-02   8.405  < 2e-16 ***
    ## MSZoningRL            4.492e-01  4.695e-02   9.567  < 2e-16 ***
    ## MSZoningRM            4.053e-01  4.408e-02   9.196  < 2e-16 ***
    ## LotFrontage           5.187e-04  1.979e-04   2.621 0.008862 ** 
    ## LotArea               2.312e-06  4.623e-07   5.001 6.51e-07 ***
    ## AlleyNone            -9.947e-03  1.944e-02  -0.512 0.608972    
    ## AlleyPave             2.839e-02  2.878e-02   0.986 0.324158    
    ## LotShape              1.998e-03  6.245e-03   0.320 0.749106    
    ## LandContourHLS        3.653e-02  2.368e-02   1.542 0.123235    
    ## LandContourLow        4.408e-03  2.916e-02   0.151 0.879871    
    ## LandContourLvl        2.715e-02  1.701e-02   1.596 0.110757    
    ## LotConfigCulDSac      3.361e-02  1.518e-02   2.214 0.027009 *  
    ## LotConfigFR2         -2.605e-02  1.855e-02  -1.404 0.160478    
    ## LotConfigFR3         -9.116e-02  5.802e-02  -1.571 0.116391    
    ## LotConfigInside      -1.137e-02  8.190e-03  -1.388 0.165457    
    ## LandSlopeMod          2.208e-02  1.827e-02   1.208 0.227221    
    ## LandSlopeSev         -1.153e-01  4.863e-02  -2.370 0.017945 *  
    ## NeighborhoodBlueste   5.866e-02  9.060e-02   0.647 0.517468    
    ## NeighborhoodBrDale    2.409e-02  5.335e-02   0.452 0.651581    
    ## NeighborhoodBrkSide   4.565e-02  4.299e-02   1.062 0.288581    
    ## NeighborhoodClearCr   4.888e-02  4.222e-02   1.158 0.247157    
    ## NeighborhoodCollgCr   2.006e-03  3.297e-02   0.061 0.951482    
    ## NeighborhoodCrawfor   1.253e-01  3.933e-02   3.186 0.001479 ** 
    ## NeighborhoodEdwards  -3.999e-02  3.641e-02  -1.098 0.272345    
    ## NeighborhoodGilbert  -1.986e-03  3.522e-02  -0.056 0.955051    
    ## NeighborhoodIDOTRR    1.828e-02  4.898e-02   0.373 0.709052    
    ## NeighborhoodMeadowV  -1.086e-01  5.598e-02  -1.940 0.052609 .  
    ## NeighborhoodMitchel  -2.015e-02  3.709e-02  -0.543 0.587001    
    ## NeighborhoodNAmes    -1.091e-02  3.550e-02  -0.307 0.758690    
    ## NeighborhoodNoRidge   5.278e-02  3.855e-02   1.369 0.171143    
    ## NeighborhoodNPkVill   1.807e-02  6.355e-02   0.284 0.776248    
    ## NeighborhoodNridgHt   1.216e-01  3.277e-02   3.710 0.000216 ***
    ## NeighborhoodNWAmes   -1.140e-02  3.660e-02  -0.311 0.755484    
    ## NeighborhoodOldTown  -1.060e-02  4.386e-02  -0.242 0.809070    
    ## NeighborhoodSawyer    6.923e-04  3.691e-02   0.019 0.985036    
    ## NeighborhoodSawyerW   8.429e-03  3.558e-02   0.237 0.812769    
    ## NeighborhoodSomerst   5.742e-02  4.012e-02   1.431 0.152630    
    ## NeighborhoodStoneBr   1.575e-01  3.765e-02   4.183 3.08e-05 ***
    ## NeighborhoodSWISU     1.118e-02  4.423e-02   0.253 0.800469    
    ## NeighborhoodTimber    1.389e-02  3.739e-02   0.371 0.710400    
    ## NeighborhoodVeenker   7.102e-02  4.769e-02   1.489 0.136689    
    ## Condition1Feedr       2.687e-02  2.270e-02   1.184 0.236794    
    ## Condition1Norm        7.384e-02  1.883e-02   3.922 9.28e-05 ***
    ## Condition1PosA        1.869e-02  4.575e-02   0.409 0.682959    
    ## Condition1PosN        7.354e-02  3.389e-02   2.170 0.030211 *  
    ## Condition1RRAe       -3.059e-02  4.173e-02  -0.733 0.463731    
    ## Condition1RRAn        5.653e-02  3.161e-02   1.788 0.073939 .  
    ## Condition1RRNe       -3.926e-03  8.098e-02  -0.048 0.961343    
    ## Condition1RRNn        4.796e-02  5.759e-02   0.833 0.405165    
    ## Condition2Feedr       2.181e-01  1.162e-01   1.877 0.060750 .  
    ## Condition2Norm        1.873e-01  1.026e-01   1.825 0.068287 .  
    ## Condition2PosA        3.231e-01  1.756e-01   1.840 0.065993 .  
    ## Condition2PosN       -6.633e-01  1.337e-01  -4.960 8.03e-07 ***
    ## Condition2RRAe       -4.797e-01  3.282e-01  -1.462 0.144113    
    ## Condition2RRAn        1.079e-01  1.522e-01   0.709 0.478349    
    ## Condition2RRNn        1.924e-01  1.314e-01   1.464 0.143414    
    ## BldgType2fmCon        2.005e-02  1.144e-01   0.175 0.860881    
    ## BldgTypeDuplex               NA         NA      NA       NA    
    ## BldgTypeTwnhs         1.634e-03  7.203e-02   0.023 0.981906    
    ## BldgTypeTwnhsE        2.345e-02  6.845e-02   0.343 0.731968    
    ## HouseStyle1.5Unf      2.671e-01  1.031e-01   2.589 0.009729 ** 
    ## HouseStyle1Story      3.930e-02  4.020e-02   0.978 0.328489    
    ## HouseStyle2.5Fin     -9.273e-03  7.513e-02  -0.123 0.901794    
    ## HouseStyle2.5Unf      1.254e-01  7.111e-02   1.763 0.078084 .  
    ## HouseStyle2Story      4.064e-02  3.716e-02   1.094 0.274248    
    ## HouseStyleSFoyer      2.925e-03  5.368e-02   0.054 0.956553    
    ## HouseStyleSLvl        4.346e-02  5.908e-02   0.736 0.462131    
    ## OverallQual           4.127e-02  4.673e-03   8.831  < 2e-16 ***
    ## OverallCond           3.530e-02  3.953e-03   8.931  < 2e-16 ***
    ## YearBuilt             1.655e-03  3.730e-04   4.437 9.93e-06 ***
    ## YearRemodAdd          5.757e-04  2.473e-04   2.328 0.020074 *  
    ## RoofStyleGable       -3.251e-02  8.334e-02  -0.390 0.696562    
    ## RoofStyleGambrel     -7.619e-02  9.185e-02  -0.830 0.406946    
    ## RoofStyleHip         -2.658e-02  8.359e-02  -0.318 0.750547    
    ## RoofStyleMansard     -6.681e-03  9.749e-02  -0.069 0.945376    
    ## RoofStyleShed         3.950e-01  1.758e-01   2.247 0.024841 *  
    ## RoofMatlCompShg       2.453e+00  1.475e-01  16.628  < 2e-16 ***
    ## RoofMatlMembran       2.746e+00  2.073e-01  13.251  < 2e-16 ***
    ## RoofMatlMetal         2.590e+00  2.091e-01  12.386  < 2e-16 ***
    ## RoofMatlRoll          2.417e+00  1.868e-01  12.940  < 2e-16 ***
    ## RoofMatlTar&Grv       2.451e+00  1.692e-01  14.487  < 2e-16 ***
    ## RoofMatlWdShake       2.385e+00  1.645e-01  14.501  < 2e-16 ***
    ## RoofMatlWdShngl       2.549e+00  1.527e-01  16.688  < 2e-16 ***
    ## Exterior1stAsphShn   -6.127e-02  1.521e-01  -0.403 0.687121    
    ## Exterior1stBrkComm   -3.123e-01  1.286e-01  -2.429 0.015270 *  
    ## Exterior1stBrkFace    7.845e-02  5.867e-02   1.337 0.181419    
    ## Exterior1stCBlock    -8.377e-02  1.172e-01  -0.715 0.474956    
    ## Exterior1stCemntBd   -6.812e-02  8.736e-02  -0.780 0.435699    
    ## Exterior1stHdBoard   -2.566e-02  5.913e-02  -0.434 0.664470    
    ## Exterior1stImStucc   -4.934e-02  1.284e-01  -0.384 0.700799    
    ## Exterior1stMetalSd    2.510e-02  6.716e-02   0.374 0.708624    
    ## Exterior1stPlywood   -1.219e-02  5.846e-02  -0.208 0.834892    
    ## Exterior1stStone      3.333e-02  1.099e-01   0.303 0.761626    
    ## Exterior1stStucco     3.700e-02  6.479e-02   0.571 0.568031    
    ## Exterior1stVinylSd    2.140e-03  6.174e-02   0.035 0.972362    
    ## Exterior1stWd Sdng   -4.120e-02  5.688e-02  -0.724 0.468966    
    ## Exterior1stWdShing    1.241e-02  6.146e-02   0.202 0.839963    
    ## Exterior2ndAsphShn    9.195e-02  1.017e-01   0.904 0.366151    
    ## Exterior2ndBrk Cmn    1.114e-01  9.452e-02   1.178 0.238971    
    ## Exterior2ndBrkFace   -2.970e-02  6.004e-02  -0.495 0.620940    
    ## Exterior2ndCBlock            NA         NA      NA       NA    
    ## Exterior2ndCmentBd    1.229e-01  8.580e-02   1.432 0.152265    
    ## Exterior2ndHdBoard    3.133e-02  5.625e-02   0.557 0.577655    
    ## Exterior2ndImStucc    5.791e-02  6.473e-02   0.895 0.371124    
    ## Exterior2ndMetalSd    1.416e-02  6.521e-02   0.217 0.828110    
    ## Exterior2ndOther     -1.011e-01  1.251e-01  -0.808 0.419318    
    ## Exterior2ndPlywood    2.730e-02  5.480e-02   0.498 0.618508    
    ## Exterior2ndStone     -2.211e-02  7.845e-02  -0.282 0.778102    
    ## Exterior2ndStucco    -1.540e-02  6.213e-02  -0.248 0.804280    
    ## Exterior2ndVinylSd    2.827e-02  5.916e-02   0.478 0.632896    
    ## Exterior2ndWd Sdng    5.358e-02  5.440e-02   0.985 0.324815    
    ## Exterior2ndWd Shng   -1.524e-03  5.688e-02  -0.027 0.978628    
    ## MasVnrType            4.326e-03  4.188e-03   1.033 0.301871    
    ## MasVnrArea            1.227e-05  2.484e-05   0.494 0.621454    
    ## ExterQual             2.362e-03  9.667e-03   0.244 0.807018    
    ## ExterCondFa          -7.216e-02  8.333e-02  -0.866 0.386712    
    ## ExterCondGd          -7.314e-02  7.971e-02  -0.918 0.359015    
    ## ExterCondPo          -3.906e-02  1.417e-01  -0.276 0.782855    
    ## ExterCondTA          -5.176e-02  7.950e-02  -0.651 0.515127    
    ## FoundationCBlock      6.375e-03  1.464e-02   0.436 0.663242    
    ## FoundationPConc       3.174e-02  1.575e-02   2.016 0.044014 *  
    ## FoundationSlab       -2.497e-03  4.160e-02  -0.060 0.952144    
    ## FoundationStone       9.462e-02  5.135e-02   1.843 0.065631 .  
    ## FoundationWood       -1.125e-01  6.821e-02  -1.649 0.099339 .  
    ## BsmtQual              5.110e-03  7.825e-03   0.653 0.513907    
    ## BsmtCond              6.541e-04  9.789e-03   0.067 0.946735    
    ## BsmtExposure          1.197e-02  3.860e-03   3.100 0.001976 ** 
    ## BsmtFinType1          3.179e-03  2.321e-03   1.369 0.171101    
    ## BsmtFinSF1            3.436e-05  3.252e-05   1.057 0.290844    
    ## BsmtFinType2         -1.628e-03  5.809e-03  -0.280 0.779286    
    ## BsmtUnfSF            -3.174e-05  3.216e-05  -0.987 0.323918    
    ## TotalBsmtSF           9.812e-05  3.613e-05   2.716 0.006706 ** 
    ## HeatingGasA           1.418e-01  1.143e-01   1.241 0.215000    
    ## HeatingGasW           2.008e-01  1.173e-01   1.713 0.087008 .  
    ## HeatingGrav           1.053e-02  1.248e-01   0.084 0.932753    
    ## HeatingOthW           5.621e-02  1.424e-01   0.395 0.693133    
    ## HeatingWall           1.975e-01  1.330e-01   1.485 0.137856    
    ## HeatingQCFa          -2.306e-02  2.127e-02  -1.084 0.278525    
    ## HeatingQCGd          -2.034e-02  9.562e-03  -2.127 0.033585 *  
    ## HeatingQCPo          -1.184e-01  1.229e-01  -0.964 0.335325    
    ## HeatingQCTA          -2.785e-02  9.435e-03  -2.952 0.003219 ** 
    ## CentralAirY           6.186e-02  1.786e-02   3.463 0.000552 ***
    ## ElectricalFuseF      -2.344e-02  2.670e-02  -0.878 0.380100    
    ## ElectricalFuseP      -8.882e-03  7.687e-02  -0.116 0.908026    
    ## ElectricalMix         2.525e-02  1.200e-01   0.210 0.833360    
    ## ElectricalSBrkr      -1.995e-02  1.365e-02  -1.461 0.144207    
    ## X1stFlrSF             2.481e-04  2.426e-05  10.228  < 2e-16 ***
    ## X2ndFlrSF             2.411e-04  2.508e-05   9.613  < 2e-16 ***
    ## LowQualFinSF          2.461e-04  8.373e-05   2.940 0.003347 ** 
    ## GrLivArea                    NA         NA      NA       NA    
    ## BsmtFullBath          2.515e-02  8.496e-03   2.960 0.003133 ** 
    ## FullBath              2.316e-02  1.003e-02   2.310 0.021063 *  
    ## HalfBath              2.679e-02  9.574e-03   2.798 0.005214 ** 
    ## BedroomAbvGr         -3.859e-03  6.315e-03  -0.611 0.541273    
    ## KitchenAbvGr         -4.334e-02  2.754e-02  -1.573 0.115881    
    ## KitchenQual           1.991e-02  7.502e-03   2.655 0.008038 ** 
    ## TotRmsAbvGrd          6.481e-03  4.324e-03   1.499 0.134169    
    ## Functional            3.044e-02  5.230e-03   5.819 7.50e-09 ***
    ## Fireplaces            1.588e-02  1.009e-02   1.574 0.115839    
    ## FireplaceQu           2.935e-03  3.660e-03   0.802 0.422796    
    ## GarageTypeAttchd      9.461e-02  4.949e-02   1.912 0.056128 .  
    ## GarageTypeBasment     8.532e-02  5.799e-02   1.471 0.141457    
    ## GarageTypeBuiltIn     8.408e-02  5.183e-02   1.622 0.105031    
    ## GarageTypeCarPort     9.119e-02  6.477e-02   1.408 0.159376    
    ## GarageTypeDetchd      9.752e-02  4.940e-02   1.974 0.048574 *  
    ## GarageTypeNone        1.860e-01  6.583e-02   2.825 0.004801 ** 
    ## GarageFinish          5.407e-03  5.561e-03   0.972 0.331037    
    ## GarageCars            4.290e-02  7.374e-03   5.817 7.61e-09 ***
    ## GarageQual            3.799e-02  1.382e-02   2.750 0.006047 ** 
    ## PavedDriveP           1.620e-03  2.550e-02   0.064 0.949361    
    ## PavedDriveY           2.345e-02  1.551e-02   1.513 0.130603    
    ## WoodDeckSF            9.720e-05  2.664e-05   3.648 0.000275 ***
    ## OpenPorchSF           5.475e-05  5.293e-05   1.034 0.301234    
    ## EnclosedPorch         1.221e-04  5.739e-05   2.128 0.033528 *  
    ## X3SsnPorch            1.640e-04  1.030e-04   1.592 0.111665    
    ## ScreenPorch           2.895e-04  5.632e-05   5.141 3.17e-07 ***
    ## FenceGdWo            -3.553e-02  2.211e-02  -1.607 0.108386    
    ## FenceMnPrv           -5.675e-03  1.783e-02  -0.318 0.750340    
    ## FenceMnWw            -1.811e-02  3.733e-02  -0.485 0.627647    
    ## FenceNone            -3.859e-05  1.615e-02  -0.002 0.998094    
    ## MiscFeatureNone      -3.422e-01  4.522e-01  -0.757 0.449373    
    ## MiscFeatureOthr      -3.181e-01  4.098e-01  -0.776 0.437806    
    ## MiscFeatureShed      -3.341e-01  4.327e-01  -0.772 0.440088    
    ## MiscFeatureTenC      -4.946e-01  4.168e-01  -1.187 0.235541    
    ## MiscVal              -1.947e-05  2.840e-05  -0.685 0.493241    
    ## MoSold               -8.654e-04  1.128e-03  -0.767 0.443113    
    ## YrSold               -3.047e-03  2.327e-03  -1.309 0.190738    
    ## SaleTypeCon           9.328e-02  8.139e-02   1.146 0.251971    
    ## SaleTypeConLD         1.286e-01  4.475e-02   2.875 0.004110 ** 
    ## SaleTypeConLI        -3.095e-02  5.324e-02  -0.581 0.561118    
    ## SaleTypeConLw         1.804e-02  5.486e-02   0.329 0.742304    
    ## SaleTypeCWD           6.613e-02  5.909e-02   1.119 0.263227    
    ## SaleTypeNew           1.216e-01  7.103e-02   1.711 0.087245 .  
    ## SaleTypeOth           6.664e-02  6.740e-02   0.989 0.322995    
    ## SaleTypeWD           -9.253e-03  1.921e-02  -0.482 0.630163    
    ## SaleConditionAdjLand  1.107e-01  6.668e-02   1.661 0.096991 .  
    ## SaleConditionAlloca   6.807e-02  3.937e-02   1.729 0.084076 .  
    ## SaleConditionFamily   1.117e-02  2.806e-02   0.398 0.690524    
    ## SaleConditionNormal   6.785e-02  1.314e-02   5.163 2.83e-07 ***
    ## SaleConditionPartial -1.525e-02  6.837e-02  -0.223 0.823560    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1055 on 1245 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9405, Adjusted R-squared:  0.9303 
    ## F-statistic: 92.34 on 213 and 1245 DF,  p-value: < 2.2e-16

**Let’s remove some variables with low p-value that don’t seem important
and run the regression again**

``` r
fit <- lm(SalePrice ~ OverallQual+FullBath+KitchenQual+FireplaceQu+OverallCond+YearBuilt+GrLivArea+GarageCars+TotalBsmtSF+RoofMatl+LotArea, data=train_data)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = SalePrice ~ OverallQual + FullBath + KitchenQual + 
    ##     FireplaceQu + OverallCond + YearBuilt + GrLivArea + GarageCars + 
    ##     TotalBsmtSF + RoofMatl + LotArea, data = train_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.66903 -0.06724  0.00552  0.07875  0.47665 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      7.629e-01  4.145e-01   1.841   0.0659 .  
    ## OverallQual      6.590e-02  4.795e-03  13.744  < 2e-16 ***
    ## FullBath        -1.331e-02  9.947e-03  -1.338   0.1811    
    ## KitchenQual      4.576e-02  7.993e-03   5.725 1.26e-08 ***
    ## FireplaceQu      1.883e-02  2.466e-03   7.634 4.11e-14 ***
    ## OverallCond      6.001e-02  3.748e-03  16.011  < 2e-16 ***
    ## YearBuilt        3.586e-03  1.932e-04  18.563  < 2e-16 ***
    ## GrLivArea        2.603e-04  1.182e-05  22.014  < 2e-16 ***
    ## GarageCars       6.342e-02  6.834e-03   9.280  < 2e-16 ***
    ## TotalBsmtSF      1.616e-04  1.120e-05  14.430  < 2e-16 ***
    ## RoofMatlCompShg  2.573e+00  1.541e-01  16.696  < 2e-16 ***
    ## RoofMatlMembran  2.735e+00  2.085e-01  13.118  < 2e-16 ***
    ## RoofMatlMetal    2.798e+00  2.099e-01  13.330  < 2e-16 ***
    ## RoofMatlRoll     2.527e+00  2.088e-01  12.104  < 2e-16 ***
    ## RoofMatlTar&Grv  2.581e+00  1.591e-01  16.224  < 2e-16 ***
    ## RoofMatlWdShake  2.515e+00  1.652e-01  15.222  < 2e-16 ***
    ## RoofMatlWdShngl  2.627e+00  1.617e-01  16.246  < 2e-16 ***
    ## LotArea          3.128e-06  4.006e-07   7.809 1.10e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.141 on 1442 degrees of freedom
    ## Multiple R-squared:  0.8768, Adjusted R-squared:  0.8754 
    ## F-statistic: 603.7 on 17 and 1442 DF,  p-value: < 2.2e-16

``` r
fit$xlevels[["MSSubClass"]] <- union(fit$xlevels[["MSSubClass"]], levels(train_data$MSSubClass))
pred <- predict(fit, test_data)
pr <- exp(pred)
```

``` r
summary(pr)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   51398  126390  157582  177083  206346 1522057

``` r
df <- data.frame(Id,pr)
colnames(df)<-c("Id","SalePrice")
which(is.na(df))
```

    ## integer(0)

``` r
write.csv(df,"C:/Users/User/Desktop/Άγγελος/R/Data analysis/House prices/house-prices-advanced-regression-techniques\\submission.csv", row.names = FALSE)
```
