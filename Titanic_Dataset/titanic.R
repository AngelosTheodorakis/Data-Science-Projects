setwd("C:/Users/User/Desktop/¢„„ÂÎÔÚ/R/Data analysis/Titanic/titanic")

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# First, we read the data.

test<-read.csv("test.csv")
train<-read.csv("train.csv")

# Create a new column which we want to predict, for the test matrix.

Survived<-rep(NA,nrow(test)) # Make a vector of NA's
test<-as.data.frame(cbind(Survived,test)) # Test matrix with the new column

data<-rbind(train,test) # Join the 2 matrices together (Train and test set)
rm(Survived)

str(train) 
str(test)
str(data)
summary(data)

Missing_values<-data.frame(sort(sapply(data,function(x) sum(is.na(x))),decreasing = TRUE))

#sum(data[,"Embarked"] == "")


data$Survived<-as.factor(data$Survived)
train$Survived<-as.factor(train$Survived)
data$Pclass<-as.factor(data$Pclass)
data$SibSp<-as.factor(data$SibSp)
data$Parch<-as.factor(data$Parch)

# data$Age<-as.factor(data$Age) (maybe)
par(mfrow=c(2,4))
plot(train$Survived,col="red",main="Survived")
plot(data$Pclass,col="green",main="Pclass")
plot(data$Sex,col="brown",main="Gender")
hist(data$Age,col="yellow",breaks = 15)
# summary(data$Age) 263 NA's in age!
plot(data$SibSp,col="purple",main="Siblings")
plot(data$Parch,col="pink",main="Parents/Children")
hist(data$Fare,col="blue",main="Fare")
summary((data$Fare))
plot(data$Embarked,main="Port Embarked") #we can see NA's
levels(data$Embarked)

which(data$Embarked=="") # 2 Cabins are unknown
subset(data,Embarked=="")
# How will we find the port from which the 2 persons embarged? Maybe it has
# to do with some attribute, like the ticket price or the Cabin for example.
# We will first find if there is correlation with the Cabin variable

levels(data$Cabin) #Cabin number starts with letters.
length(levels(data$Cabin))
length(which(data$Cabin== "")) # We have many missing values


# We will use just the first letter of the cabins, to reduce the levels,
# and hopefully make some conclusions with those new levels.

Cabin_levels = substr(data$Cabin, start = 1, stop = 1)
Cabin_levels = as.factor(Cabin_levels)

# Cabin levels (Percentages)
sapply(table(Cabin_levels),function(x) round((x/nrow(data)*100),2))
# Education level for each country
tapply(data$Embarked, Cabin_levels, summary)
# Table of education level Percentages, seperately for each country
table(Cabin_levels,data$Embarked)/rowSums(table(Cabin_levels,data$Embarked))*100
prop.table(table(Cabin_levels,data$Embarked), 1)*100
prop.table(table(data$Embarked,Cabin_levels), 2)*100

# We can see that for the Cabins starting with B there is an equal probability
# for people that have embarged from C and S.
# We will investigate the ticket variable

length(which(is.na(data$Ticket))) 
length(which(data$Ticket== "")) # We have no missing values

Ticket_levels <- strsplit(as.character(data$Ticket),split=" ")
Ticket_levels <- sapply(Ticket_levels, function(x) x[1])
levels(as.factor(Ticket_levels))
#CONTINUE#


ggplot(data, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

median(data[which(data$Embarked=="C" & data$Pclass==1 & data$Sex == "female"),"Fare"])
median(data[which(data$Embarked=="S" & data$Pclass==1 & data$Sex == "female"),"Fare"])
length(data[which(data$Embarked=="C" & data$Pclass==1 & data$Sex == "female"),"Fare"])
length((data[which(data$Embarked=="S" & data$Pclass==1 & data$Sex == "female"),"Fare"]))

data 
data[which(data$Fare >= 80),c("Fare","Embarked")]
data %>% 
  group_by(Embarked) %>% 
  summarize(median = median(Fare,na.rm = TRUE))

data[which(is.na(data$Fare)),] 


par(mfrow=c(1,1))
plot(factor(data$Pclass),data$Fare,ylim=c(0,300)) #price of ticket for Pclass
plot(data$Embarked,data$Fare,ylim=c(0,300))

median(data[which(data$Embarked=="S" & data$Pclass==3 & data$Sex == "male"),"Fare"],na.rm = TRUE)

plot(factor(train$Sex),factor(train$Survived)) #maybe better with ggplot

# The remaining missing values are for the Age variable. We have seen before 
# a plot regarding the age of the passengers. Let's make some plots to get a
# better sense of how the variable is connected to other attributes of a
# passenger.

df <- data %>% 
  group_by(Pclass,Sex) %>% 
  summarize(Median_Age = median(Age,na.rm = TRUE)) 
  

ggplot(data, aes(x=Sex, y=Age,fill=factor(Sex))) + 
  stat_summary(fun.y="median", geom="bar")+
  facet_wrap(~factor(Pclass))

ggplot(data, aes(x = Sex, y=Age, fill=factor(Sex))) +  
  labs(title='Passengers survived', x='Gender',y='Age')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  stat_summary(fun.y="median", geom="bar")+
  facet_wrap(~factor(Pclass))

df<-data %>% 
  group_by(Pclass,Sex) %>% 
  summarize(Median_Age = median(Age,na.rm = TRUE)) 


data[which(data$Pclass==1 & data$Sex == "male"),"Age"] <- df[1,"Median_Age"] #Replace the missing value

data %>%
  group_by(Pclass) %>%
  mutate(
    Age = impute.mean(Age)
  )

#d2<-data %>% 
#  group_by(Pclass) %>% 
#  mutate_if(is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))



# CONTINUE#

list <- strsplit(as.character(data$Name),(', '))
head(list)
title <- as.factor(sapply(list, function(x) x[2]))
list<-strsplit(as.character(title),("\\."))
title <- as.factor(sapply(list, function(x) x[1]))
data$Title<-title
rm(title)
rm(list)

levels(data$Title)
table(data$Sex,data$Title)
plot(table(data$Sex,data$Title)) #maybe with ggplot2
table(data$Pclass,data$Title)
paste(as.character(levels(data$Title)),collapse = "','")
rare_title<-c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Mlle','Mme','Ms','Rev','Sir','the Countess')

#replace with rare title.. how?

length(which(data$Title %in% rare_title))
str(data$Title)
data$Title<-as.character((data$Title))
data$Title[which(data$Title %in% rare_title)]<-"Rare"
data$Title<-as.factor(data$Title)
plot(data$Title,data$Pclass) #plot showing that title is related in some cases with pclass
plot(data$Title,data$Age) #plot showing that title is related with age!
table(data$Sex,data$Title)

#Now we are going to split the names so we can have the surnames

list <- strsplit(as.character(data$Name),(', '))
head(list)
title <- as.factor(sapply(list, function(x) x[1]))
data$Surname<-title
rm(title)
rm(list)
levels(data$Surname)

# Create a family size variable including the passenger themselves
as.numeric(data$SibSp)-1
data$Fsize<-as.numeric(as.character(data$SibSp))+as.numeric(as.character(data$Parch))+1
  data[,c(7,8,15)]
data$Fsize<-as.factor(data$Fsize) 
head(data$Fsize)
plot(data$Fsize)

# Create a family variable 
data$Family<-paste(data$Surname,data$Fsize,sep="_")
subset(data,Fsize==11)

## Use ggplot2 to visualize the relationship between family size & survival
ggplot(data[1:891,], aes(x =as.numeric(as.character(Fsize)), fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


# Discretize family size
data$FsizeD<-rep(NA,nrow(data))
data$FsizeD[which(as.numeric(as.character(data$Fsize)) > 4)]<- "Large"
data$FsizeD[which(as.numeric(as.character(data$Fsize)) ==1)]<- "Alone"
data$FsizeD[which(as.numeric(as.character(data$Fsize))>1 & (as.numeric(as.character(data$Fsize)))<=4 )]<- "Medium"

# Show family size by survival using a mosaic plot
mosaicplot(table(data$FsizeD, data$Survived), main='Family Size by Survival', shade=TRUE)

#Now we'll work with the Cabin variable . How?

data$Cabin<-as.character(data$Cabin)
list<-strsplit(data$Cabin,NULL)
data$Deck<-sapply(list,function(x) x[1])
rm(list)

data$Deck<-as.factor(data$Deck)

## Use ggplot2 for (data$Pclass,data$Deck) How?
#lets find the missing values from port embarked
plot(data$Embarked,data$Fare,ylim=c(0,300))


# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(data, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='black', linetype='dashed', lwd=1) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
#So we assume that the passengers embarged from C port
#How about this one?
data[1044, ]

##This is a third class passenger who departed from Southampton (ëSí). 
##Letís visualize Fares among all others sharing their class and embarkment
ggplot(data[data$Pclass == '3' & data$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = 'blue', alpha=0.2) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

median(data[data$Pclass == '3' & data$Embarked == 'S', "Fare" ],na.rm=TRUE)
#so we assume that he paid 8.05$ fare

data[1044,"Fare"]<-8.05
summary(data$Age) #as we saw there are 263 NA's

# Make variables factors into factors
paste(colnames(data),collapse="','") 

factors <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

data[factors] <- lapply(data[factors], function(x) as.factor(x))
       str(data[factors])
       set.seed(129)
       
       # Perform mice imputation, excluding certain less-than-useful variables:
       mice_mod <- mice(data[, !names(data) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
md.pattern(data) #map of the missing values and pairs

# Save the complete output 
mice_output <- complete(mice_mod) #No more missing values

# Plot age distributions
par(mfrow=c(1,2))
hist(data$Age,freq=F , main='Age: Original Data', 
     col='darkblue', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightblue', ylim=c(0,0.04))

# Replace Age variable from the mice model.
data$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(data$Age))
par(mfrow=c(1,1))
# First we'll look at the relationship between age & survival
ggplot(data[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

#create a mother variable
data$Mother<- rep("Not_Mother",nrow(data))
data$Mother[data$Sex %in% "female" & data$Age>=18 & !data$Title %in% "Miss" & data$Parch!=0]<-"Mother"
data$Mother<-factor(data$Mother)
table(data$Mother, data$Survived)

data$Child<-rep(NA,nrow(data))
data$Child[which(data$Age<18)]<-c("Child")
data$Child[which(data$Age>=18)]<-c("Adult")
data[,c("Child","Age")]
data$Child<-as.factor(data$Child)
summary(data$Child)
sapply(summary(data$Child),function(x) x/nrow(data)) #percent of children
table(data$Child, data$Survived)

md.pattern(data)
#################################################

# Split the data back into a train set and a test set
train <- data[1:891,]
test <- data[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
##How do I do it with another way? 

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'Titanic_Solution.csv', row.names = F)