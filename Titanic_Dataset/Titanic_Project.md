TITANIC DATASET EXPLORATION
---------------------------

### In this project we will explore the Titanic dataset (downloaded from Kaggle.com) by making some plots and observations about the data. Also, we will make a prediction about whether the passengers on the test set survived or not./

**First, we read the data**

``` r
setwd("C:/Users/User/Desktop/Άγγελος/R/Data analysis/Titanic/titanic")
test<-read.csv("test.csv")
train<-read.csv("train.csv")
```

**Then we load some libraries**

``` r
library('ggplot2') 
library('ggthemes')
library('scales') 
library('dplyr') 
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

**Now we will create a new column which we want to predict, for the test
matrix.**

``` r
Survived<-rep(NA,nrow(test)) # Make a vector of NA's
test<-as.data.frame(cbind(Survived,test)) # Test matrix with the new column
data<-rbind(train,test) # Join the 2 matrices together (Train and test set)
rm(Survived) #remove the survived vector
```

**We are ready to explore our data. We will make some plots to visualise
our data and explore all the columns of this dataset. Let’s look at a
summary of our variables.**

``` r
str(data)
```

    ## 'data.frame':    1309 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : Factor w/ 1307 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : Factor w/ 929 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : Factor w/ 187 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
    ##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

``` r
summary(data)
```

    ##   PassengerId      Survived          Pclass     
    ##  Min.   :   1   Min.   :0.0000   Min.   :1.000  
    ##  1st Qu.: 328   1st Qu.:0.0000   1st Qu.:2.000  
    ##  Median : 655   Median :0.0000   Median :3.000  
    ##  Mean   : 655   Mean   :0.3838   Mean   :2.295  
    ##  3rd Qu.: 982   3rd Qu.:1.0000   3rd Qu.:3.000  
    ##  Max.   :1309   Max.   :1.0000   Max.   :3.000  
    ##                 NA's   :418                     
    ##                                Name          Sex           Age       
    ##  Connolly, Miss. Kate            :   2   female:466   Min.   : 0.17  
    ##  Kelly, Mr. James                :   2   male  :843   1st Qu.:21.00  
    ##  Abbing, Mr. Anthony             :   1                Median :28.00  
    ##  Abbott, Mr. Rossmore Edward     :   1                Mean   :29.88  
    ##  Abbott, Mrs. Stanton (Rosa Hunt):   1                3rd Qu.:39.00  
    ##  Abelson, Mr. Samuel             :   1                Max.   :80.00  
    ##  (Other)                         :1301                NA's   :263    
    ##      SibSp            Parch            Ticket          Fare        
    ##  Min.   :0.0000   Min.   :0.000   CA. 2343:  11   Min.   :  0.000  
    ##  1st Qu.:0.0000   1st Qu.:0.000   1601    :   8   1st Qu.:  7.896  
    ##  Median :0.0000   Median :0.000   CA 2144 :   8   Median : 14.454  
    ##  Mean   :0.4989   Mean   :0.385   3101295 :   7   Mean   : 33.295  
    ##  3rd Qu.:1.0000   3rd Qu.:0.000   347077  :   7   3rd Qu.: 31.275  
    ##  Max.   :8.0000   Max.   :9.000   347082  :   7   Max.   :512.329  
    ##                                   (Other) :1261   NA's   :1        
    ##              Cabin      Embarked
    ##                 :1014    :  2   
    ##  C23 C25 C27    :   6   C:270   
    ##  B57 B59 B63 B66:   5   Q:123   
    ##  G6             :   5   S:914   
    ##  B96 B98        :   4           
    ##  C22 C26        :   4           
    ##  (Other)        : 271

**Now, let’s find the missing values.**

``` r
Missing_values<-data.frame(sort(sapply(data,function(x) sum(is.na(x))),decreasing = TRUE))
Missing_values
```

    ##             sort.sapply.data..function.x..sum.is.na.x.....decreasing...TRUE.
    ## Survived                                                                 418
    ## Age                                                                      263
    ## Fare                                                                       1
    ## PassengerId                                                                0
    ## Pclass                                                                     0
    ## Name                                                                       0
    ## Sex                                                                        0
    ## SibSp                                                                      0
    ## Parch                                                                      0
    ## Ticket                                                                     0
    ## Cabin                                                                      0
    ## Embarked                                                                   0

**Survived is the variable we want to predict in our test set. So we
will later look at its correlation with the other variables.**

``` r
ggplot(train, aes(x = factor(Survived))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Survived))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust =-.3)+
  scale_y_continuous(labels = percent) +
  labs(title='Passengers survived', x='Survived',y='Percent')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  scale_x_discrete(labels=c('NO','YES'))
```

![](Titanic_Project_files/figure-markdown_github/unnamed-chunk-6-1.png)

**We can see from the plot above that approximately 62% of the
passengers didn’t survive.**

**Next, we will convert to factors some of our variables and make some
plots to get an idea of our data.**

``` r
train$Survived <- as.factor(train$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$SibSp<-as.factor(data$SibSp)
data$Parch<-as.factor(data$Parch)

par(mfrow=c(2,4))
plot(train$Survived,col="red",main="Survived")
plot(data$Pclass,col="green",main="Pclass")
plot(data$Sex,col="brown",main="Gender")
hist(data$Age,col="yellow",breaks = 15)

plot(data$SibSp,col="purple",main="Siblings")
plot(data$Parch,col="pink",main="Parents/Children")
hist(data$Fare,col="blue",main="Fare")
plot(data$Embarked,main="Port Embarked") 
```

![](Titanic_Project_files/figure-markdown_github/unnamed-chunk-7-1.png)

**From the plots above we can make some observations:**

-   The third class passengers were the majority.
-   There was almost a double number of men than women in the ship.
-   Most of the passengers were around 30 years old.
-   Most of the passengers travelled alone.
-   We can see that there is an outlier for the ticket fair.
-   There is a small number of missing values for the embarkation port,
    that we did’n find before,because they are not stored as NA’s.

**We will try to find the missing cabins**

``` r
levels(data$Embarked)
```

    ## [1] ""  "C" "Q" "S"

``` r
which(data$Embarked=="") # 2 Cabins are unknown
```

    ## [1]  62 830

``` r
subset(data,Embarked=="")
```

    ##     PassengerId Survived Pclass                                      Name
    ## 62           62        1      1                       Icard, Miss. Amelie
    ## 830         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn)
    ##        Sex Age SibSp Parch Ticket Fare Cabin Embarked
    ## 62  female  38     0     0 113572   80   B28         
    ## 830 female  62     0     0 113572   80   B28

**We can see that the 2 persons were on the same cabin (B28),they were
both females,they both survived and belonged to the first class.They
also traveled alone (so they were not relatives) and had the same number
of ticket. So we could guess that they embarked from the same port.**

**How will we find the port from which the 2 persons embarged? Maybe it
has some correlation with some other variable, like the ticket price or
the Cabin for example. We will first find if there is correlation wih
the Cabin variable**

``` r
levels(data$Cabin)[1:20] # First 20 levels
```

    ##  [1] ""     "A10"  "A14"  "A16"  "A19"  "A20"  "A23"  "A24"  "A26"  "A31" 
    ## [11] "A32"  "A34"  "A36"  "A5"   "A6"   "A7"   "B101" "B102" "B18"  "B19"

``` r
length(levels(data$Cabin)) # Number of different levels
```

    ## [1] 187

``` r
length(which(data$Cabin== "")) # Missing values
```

    ## [1] 1014

**First we can see that Cabin starts with letters. Also, there is an
empty level and we find that there are 1014 missing values for this
level that are not stored as NA’s. We will use just the first letter of
the cabins, to reduce the levels,and hopefully make some conclusions.**

``` r
Cabin_levels = substr(data$Cabin, start = 1, stop = 1)
Cabin_levels = as.factor(Cabin_levels)

# Cabin levels (Percentages)
sapply(table(Cabin_levels),function(x) round((x/nrow(data)*100),2))
```

    ##           A     B     C     D     E     F     G     T 
    ## 77.46  1.68  4.97  7.18  3.51  3.13  1.60  0.38  0.08

``` r
# Table of Cabin levels Percentages, seperately for each Port
prop.table(table(Cabin_levels,data$Embarked), 1)*100
```

    ##             
    ## Cabin_levels                     C          Q          S
    ##                0.000000  14.990138  11.637081  73.372781
    ##            A   0.000000  50.000000   0.000000  50.000000
    ##            B   3.076923  49.230769   0.000000  47.692308
    ##            C   0.000000  43.617021   3.191489  53.191489
    ##            D   0.000000  43.478261   0.000000  56.521739
    ##            E   0.000000  26.829268   2.439024  70.731707
    ##            F   0.000000  14.285714   4.761905  80.952381
    ##            G   0.000000   0.000000   0.000000 100.000000
    ##            T   0.000000   0.000000   0.000000 100.000000

**We can see that for the Cabins starting with B there is an equal
probability that people have embarged from C and S.**

**What about the correlation with the ticket Fare?**

``` r
plot(data$Embarked,data$Fare,ylim=c(0,300),xlab = "Port", ylab = "Ticket Price", main="Ticket Price for each port")
```

![](Titanic_Project_files/figure-markdown_github/unnamed-chunk-11-1.png)

**From the plot above it seems that the ticket fare is more likely to
belong to the C port. Let’s investigate more specifically for females
that belong to the first class.**

``` r
median(data[which(data$Embarked=="C" & data$Pclass==1 & data$Sex == "female"),"Fare"]) # median fare for the first class female passengers, that have embarged from the port of Charbourg.
```

    ## [1] 83.1583

``` r
median(data[which(data$Embarked=="S" & data$Pclass==1 & data$Sex == "female"),"Fare"]) # # median fare for the first class female passengers, that have embarged from the port of Southampton.
```

    ## [1] 78.85

**From the results above we cannot make a conclusion about the
embarkation port. We won’t stay longer here and we will replace the 2
missing values with the most common port (Southampton). Maybe we will
come back later.**

``` r
data[which(data$Embarked==""),"Embarked"] <- "S"
```

**Now for the Fare variable there is only one missing value.**

``` r
data[which(is.na(data$Fare)),] 
```

    ##      PassengerId Survived Pclass               Name  Sex  Age SibSp Parch
    ## 1044        1044     <NA>      3 Storey, Mr. Thomas male 60.5     0     0
    ##      Ticket Fare Cabin Embarked
    ## 1044   3701   NA              S

**We can maybe find it by again looking the other attributes of the
passenger. One variable we could use is the Pclass variable, because the
higher the class is , the higher the ticket cost. Let’s confirm that.**

``` r
par(mfrow=c(1,1))
plot(factor(data$Pclass),data$Fare,ylim=c(0,300),xlab = "Class", ylab = "Ticket Price", main="Ticket Price for each class") #price of ticket for Pclass
```

![](Titanic_Project_files/figure-markdown_github/unnamed-chunk-15-1.png)

**There is certainly a correlation. We will replace the missing value
with the median fare of male passengers who belong to the third class
and embarked from Southampton port.**

``` r
median(data[which(data$Embarked=="S" & data$Pclass==3 & data$Sex == "male"),"Fare"],na.rm = TRUE) #median
```

    ## [1] 8.05

``` r
data[which(is.na(data$Fare)),"Fare"] <- median(data[which(data$Embarked=="S" & data$Pclass==3 & data$Sex == "male"),"Fare"],na.rm = TRUE) #Replace the missing value
```

**The remaining missing values are for the Age variable. We have seen
before a plot regarding the age of the passengers. Let’s make some plots
and matrices to get a better sense of how the variable is connected to
other attributes of a passenger.**

``` r
ggplot(data, aes(x = Sex, y=Age, fill=factor(Sex))) +  
  labs(title='Median Age for each class', x='Gender',y='Age')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_summary(fun.y="median", geom="bar")+
  facet_wrap(~factor(Pclass))
```

    ## Warning: Removed 263 rows containing non-finite values (stat_summary).

![](Titanic_Project_files/figure-markdown_github/unnamed-chunk-17-1.png)

**From the plot above, we can see clearly that the Pclass variable is
correlated with age. Maybe the older people have more wealth and so they
belong to a higher class. Let’s see more specificaly the numbers in the
matrix that follows:**

``` r
data %>% 
  group_by(Pclass,Sex) %>% 
  summarize(Median_Age = median(Age,na.rm = TRUE)) 
```

    ## # A tibble: 6 x 3
    ## # Groups:   Pclass [3]
    ##   Pclass Sex    Median_Age
    ##   <fct>  <fct>       <dbl>
    ## 1 1      female       36  
    ## 2 1      male         42  
    ## 3 2      female       28  
    ## 4 2      male         29.5
    ## 5 3      female       22  
    ## 6 3      male         25

**Now we will replace the misiing values with the median age for each
class**

plot(train*S**e**x*, *t**r**a**i**n*Survived) \#maybe better with ggplot

list \<-
strsplit(as.character(data$Name),(', ')) head(list) title \<- as.factor(sapply(list, function(x) x\[2\])) list\<-strsplit(as.character(title),("\\\\.")) title \<- as.factor(sapply(list, function(x) x\[1\])) data$Title\<-title
rm(title) rm(list)

levels(data*T**i**t**l**e*)*t**a**b**l**e*(*d**a**t**a*Sex,data*T**i**t**l**e*)*p**l**o**t*(*t**a**b**l**e*(*d**a**t**a*Sex,data$Title)) \#maybe with ggplot2 table(data$Pclass,data*T**i**t**l**e*)*p**a**s**t**e*(*a**s*.*c**h**a**r**a**c**t**e**r*(*l**e**v**e**l**s*(*d**a**t**a*Title)),collapse
= “‘,’”)
rare\_title\<-c(‘Capt’,‘Col’,‘Don’,‘Dona’,‘Dr’,‘Jonkheer’,‘Lady’,‘Major’,‘Mlle’,‘Mme’,‘Ms’,‘Rev’,‘Sir’,‘the
Countess’)

\#replace with rare title.. how?

length(which(data*T**i**t**l**e*Title)
data*T**i**t**l**e* \<  − *a**s*.*c**h**a**r**a**c**t**e**r*((*d**a**t**a*Title))
data*T**i**t**l**e*\[*w**h**i**c**h*(*d**a**t**a*Title %in%
rare\_title)\]\<-“Rare”
data*T**i**t**l**e* \<  − *a**s*.*f**a**c**t**o**r*(*d**a**t**a*Title)
plot(data*T**i**t**l**e*, *d**a**t**a*Pclass) \#plot showing that title
is related in some cases with pclass
plot(data*T**i**t**l**e*, *d**a**t**a*Age) \#plot showing that title is
related with age! table(data*S**e**x*, *d**a**t**a*Title)

\#Now we are going to split the names so we can have the surnames

list \<-
strsplit(as.character(data*N**a**m**e*), (′, ′))*h**e**a**d*(*l**i**s**t*)*t**i**t**l**e* \<  − *a**s*.*f**a**c**t**o**r*(*s**a**p**p**l**y*(*l**i**s**t*, *f**u**n**c**t**i**o**n*(*x*)*x*\[1\]))*d**a**t**a*Surname\<-title
rm(title) rm(list) levels(data$Surname)

Create a family size variable including the passenger themselves
================================================================

as.numeric(data*S**i**b**S**p*) − 1*d**a**t**a*Fsize\<-as.numeric(as.character(data*S**i**b**S**p*)) + *a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*d**a**t**a*Parch))+1
data\[,c(7,8,15)\]
data*F**s**i**z**e* \<  − *a**s*.*f**a**c**t**o**r*(*d**a**t**a*Fsize)
head(data*F**s**i**z**e*)*p**l**o**t*(*d**a**t**a*Fsize)

Create a family variable
========================

data*F**a**m**i**l**y* \<  − *p**a**s**t**e*(*d**a**t**a*Surname,data$Fsize,sep="\_")
subset(data,Fsize==11)

Use ggplot2 to visualize the relationship between family size & survival
------------------------------------------------------------------------

ggplot(data\[1:891,\], aes(x =as.numeric(as.character(Fsize)), fill =
Survived)) + geom\_bar(stat=‘count’, position=‘dodge’) +
scale\_x\_continuous(breaks=c(1:11)) + labs(x = ‘Family Size’) +
theme\_few()

Discretize family size
======================

data*F**s**i**z**e**D* \<  − *r**e**p*(*N**A*, *n**r**o**w*(*d**a**t**a*))*d**a**t**a*FsizeD\[which(as.numeric(as.character(data$Fsize))
\> 4)\]\<- “Large”
data*F**s**i**z**e**D*\[*w**h**i**c**h*(*a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*d**a**t**a*Fsize))
==1)\]\<- “Alone”
data*F**s**i**z**e**D*\[*w**h**i**c**h*(*a**s*.*n**u**m**e**r**i**c*(*a**s*.*c**h**a**r**a**c**t**e**r*(*d**a**t**a*Fsize))\>1
& (as.numeric(as.character(data$Fsize)))\<=4 )\]\<- “Medium”

Show family size by survival using a mosaic plot
================================================

mosaicplot(table(data*F**s**i**z**e**D*, *d**a**t**a*Survived),
main=‘Family Size by Survival’, shade=TRUE)

\#Now we’ll work with the Cabin variable . How?

data*C**a**b**i**n* \<  − *a**s*.*c**h**a**r**a**c**t**e**r*(*d**a**t**a*Cabin)
list\<-strsplit(data*C**a**b**i**n*, *N**U**L**L*)*d**a**t**a*Deck\<-sapply(list,function(x)
x\[1\]) rm(list)

data*D**e**c**k* \<  − *a**s*.*f**a**c**t**o**r*(*d**a**t**a*Deck)

Use ggplot2 for (data*P**c**l**a**s**s*, *d**a**t**a*Deck) How?
---------------------------------------------------------------

\#lets find the missing values from port embarked
plot(data*E**m**b**a**r**k**e**d*, *d**a**t**a*Fare,ylim=c(0,300))

Use ggplot2 to visualize embarkment, passenger class, & median fare
===================================================================

ggplot(data, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
geom\_boxplot() + geom\_hline(aes(yintercept=80), colour=‘black’,
linetype=‘dashed’, lwd=1) +
scale\_y\_continuous(labels=dollar\_format()) + theme\_few() \#So we
assume that the passengers embarged from C port \#How about this one?
data\[1044, \]

\#\#This is a third class passenger who departed from Southampton (‘S’).
\#\#Let’s visualize Fares among all others sharing their class and
embarkment ggplot(data\[data$Pclass == '3' & data$Embarked == ‘S’, \],
aes(x = Fare)) + geom\_density(fill = ‘blue’, alpha=0.2) +
geom\_vline(aes(xintercept=median(Fare, na.rm=T)), colour=‘red’,
linetype=‘dashed’, lwd=1) +
scale\_x\_continuous(labels=dollar\_format()) + theme\_few()

median(data\[data$Pclass == '3' & data$Embarked == ‘S’, “Fare”
\],na.rm=TRUE) \#so we assume that he paid 8.05$ fare

data\[1044,“Fare”\]\<-8.05 summary(data$Age) \#as we saw there are 263
NA’s

Make variables factors into factors
===================================

paste(colnames(data),collapse=“‘,’”)

factors \<- c(‘PassengerId’,‘Pclass’,‘Sex’,‘Embarked’,
‘Title’,‘Surname’,‘Family’,‘FsizeD’)

data\[factors\] \<- lapply(data\[factors\], function(x) as.factor(x))
str(data\[factors\]) set.seed(129)

       # Perform mice imputation, excluding certain less-than-useful variables:
       mice_mod <- mice(data[, !names(data) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

md.pattern(data) \#map of the missing values and pairs

Save the complete output
========================

mice\_output \<- complete(mice\_mod) \#No more missing values

Plot age distributions
======================

par(mfrow=c(1,2))
hist(data*A**g**e*, *f**r**e**q* = *F*, *m**a**i**n* = ′*A**g**e* : *O**r**i**g**i**n**a**l**D**a**t**a*′, *c**o**l* = ′*d**a**r**k**b**l**u**e*′, *y**l**i**m* = *c*(0, 0.04))*h**i**s**t*(*m**i**c**e*<sub>*o*</sub>*u**t**p**u**t*Age,
freq=F, main=‘Age: MICE Output’, col=‘lightblue’, ylim=c(0,0.04))

Replace Age variable from the mice model.
=========================================

data*A**g**e* \<  − *m**i**c**e*<sub>*o*</sub>*u**t**p**u**t*Age

Show new number of missing Age values
=====================================

sum(is.na(data$Age)) par(mfrow=c(1,1)) \# First we’ll look at the
relationship between age & survival ggplot(data\[1:891,\], aes(Age, fill
= factor(Survived))) + geom\_histogram() + \# I include Sex since we
know (a priori) it’s a significant predictor facet\_grid(.\~Sex) +
theme\_few()

\#create a mother variable
data$Mother\<- rep("Not\_Mother",nrow(data)) data$Mother\[data*S**e**x*Age\>=18
& !data*T**i**t**l**e*Parch!=0\]\<-“Mother”
data*M**o**t**h**e**r* \<  − *f**a**c**t**o**r*(*d**a**t**a*Mother)
table(data*M**o**t**h**e**r*, *d**a**t**a*Survived)

data*C**h**i**l**d* \<  − *r**e**p*(*N**A*, *n**r**o**w*(*d**a**t**a*))*d**a**t**a*Child\[which(data$Age\<18)\]\<-c(“Child”)
data*C**h**i**l**d*\[*w**h**i**c**h*(*d**a**t**a*Age\>=18)\]\<-c(“Adult”)
data\[,c(“Child”,“Age”)\]
data*C**h**i**l**d* \<  − *a**s*.*f**a**c**t**o**r*(*d**a**t**a*Child)
summary(data*C**h**i**l**d*)*s**a**p**p**l**y*(*s**u**m**m**a**r**y*(*d**a**t**a*Child),function(x)
x/nrow(data)) \#percent of children
table(data*C**h**i**l**d*, *d**a**t**a*Survived)

md.pattern(data)
\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#

Split the data back into a train set and a test set
===================================================

train \<- data\[1:891,\] test \<- data\[892:1309,\]

Set a random seed
=================

set.seed(754)

Build the model (note: not all possible variables are used)
===========================================================

rf\_model \<- randomForest(factor(Survived) \~ Pclass + Sex + Age +
SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data
= train)

Show model error
================

plot(rf\_model, ylim=c(0,0.36)) legend(‘topright’,
colnames(rf\_model$err.rate), col=1:3, fill=1:3)

Get importance
==============

importance \<- importance(rf\_model) varImportance \<-
data.frame(Variables = row.names(importance), Importance =
round(importance\[ ,‘MeanDecreaseGini’\],2))

Create a rank variable based on importance
==========================================

rankImportance \<- varImportance %\>% mutate(Rank =
paste0(‘\#’,dense\_rank(desc(Importance)))) \#\#How do I do it with
another way?

Use ggplot2 to visualize the relative importance of variables
=============================================================

ggplot(rankImportance, aes(x = reorder(Variables, Importance), y =
Importance, fill = Importance)) + geom\_bar(stat=‘identity’) +
geom\_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0,
vjust=0.55, size = 4, colour = ‘red’) + labs(x = ‘Variables’) +
coord\_flip() + theme\_few()

Predict using the test set
==========================

prediction \<- predict(rf\_model, test)

Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
========================================================================================

solution \<- data.frame(PassengerId = test$PassengerId, Survived =
prediction)

Write the solution to file
==========================

write.csv(solution, file = ‘Titanic\_Solution.csv’, row.names = F)
