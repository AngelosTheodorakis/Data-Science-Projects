getwd()
setwd("C:/Users/User/Desktop/’γγελος/ΕΛΚΕ/Έρευνα/sodanet")
data<-read.csv('My-encoding.csv',sep=';',header=TRUE)
str(data$emailstatus)
summary(data)

Missing_values<-data.frame(sort(sapply(data,function(x) sum(is.na(x))),decreasing = TRUE))

#Q7
test<-data.frame(cbind(data$Q6,data$Q7))
 #prepei na simpiptoun ta Na's tis erotisis 6 me ta na's ths 7     
which((test[,1]<=15) & is.na(test[,2]==TRUE))
#Ara vriskoume missing values stis paratiriseis 121 kai 463 ta opoia
#aforoun atoma pou ergazontai alla den apantisan.
data[c(121,463),]
#Twra tha vroume poia atoma den exoun apantisei stin erotisi 6 
# kai an afta simpiptoun me afta tis erotisis 7
which(is.na(data$Q6) & is.na(data$Q7))
identical(which(is.na(data$Q6)),which(is.na(data$Q6) & is.na(data$Q7)))
#epomenws ontws ta missing values ths Q6 antistoixoun stis idies paratiriseis
#tis q7. autes einai  68  72 169 184 243 338 351 360 391 430 445 453

#Ara sinolika ta missing values ths Q7 einai
# 68 72 121 169 184 243 338 351 360 391 430 445 453 kai 463
length(which(is.na(data$Q6) & is.na(data$Q7))) + 
 length( which((test[,1]<=15) & is.na(test[,2]==TRUE)))
#14 missing values gia tin Q7


#Q13
data[which(is.na(data$Q13)),]
which(is.na(data$Q13)) #Paratiriseis me missing values gia tin Q13

#Q10_1
data[which(is.na(data$Q10_1)),]
which(is.na(data$Q10_1)) #Paratiriseis me missing values gia tin Q10_1
#Q10_2
data[which(is.na(data$Q10_2)),]
which(is.na(data$Q10_2)) #Paratiriseis me missing values gia tin Q10_2
#Q10_3
data[which(is.na(data$Q10_3)),]
which(is.na(data$Q10_3)) #Paratiriseis me missing values gia tin Q10_3
#Q10_4
data[which(is.na(data$Q10_4)),]
which(is.na(data$Q10_4)) #Paratiriseis me missing values gia tin Q10_4
#Q10_5
data[which(is.na(data$Q10_5)),]
which(is.na(data$Q10_5)) #Paratiriseis me missing values gia tin Q10_5
#Q10_6
data[which(is.na(data$Q10_6)),]
which(is.na(data$Q10_6)) #Paratiriseis me missing values gia tin Q10_6
#Q10_7
data[which(is.na(data$Q10_7)),]
which(is.na(data$Q10_7)) #Paratiriseis me missing values gia tin Q10_7
#Q10_8
data[which(is.na(data$Q10_8)),]
which(is.na(data$Q10_8)) #Paratiriseis me missing values gia tin Q10_8


#Gia tin Q10 oi missing values gia oles tis ipoerotiseis
#einai apo ta idia atoma.auta einai
# 12  68  72  85 121 169 179 184 230 243 255 323 338 351 360 389 391 395 403
# 411 430 445 448 453 462 463


#Q9
data[which(is.na(data$Q9_SQ010)),]
which(is.na(data$Q9_SQ010)) #Paratiriseis me missing values gia tin Q9
length(which(is.na(data$Q9_SQ010)))
#Gia tin Q9 oi missing values gia tis ipoerotiseis 10,12,13,16,17
#einai apo ta idia atoma . Auta einai
#12  68  72  85 121 169 184 230 243 338 351 360 389 391 403 411 430 445 448
# 453 463

#Paratiroume omws oti iparxoun paratiriseis midenikes pou den anikoun se kapoia 
#katigoria.Ti kanoume me autes? tis kataxwroume san missing values?
data[which(data$Q9_SQ001=='0'),]
length(which(data$Q9_SQ001=='0'))
#Exoun kataxwrithei ontws san missing values


#Q8
data[which(is.na(data$Q8)),]
which(is.na(data$Q8)) #Paratiriseis me missing values gia tin Q8
length(which(is.na(data$Q8)))

#Q6
data[which(is.na(data$Q6)),]
which(is.na(data$Q6)) #Paratiriseis me missing values gia tin Q6
length(which(is.na(data$Q6)))

#Emplmnt
data[which(is.na(data$Emplmnt)),]
which(is.na(data$Emplmnt)) #Paratiriseis me missing values gia tin Emplmnt
length(which(is.na(data$Emplmnt)))

#Tha analisoume giati tosa missing values stin Q13 . Pithanon na ipirkse
#sigxisi logw erotimatologiou sta ellinika (Ean nai,parakalw epilekste ola
#osa isxioun). As to anakalipsoume .

data[which(is.na(data$Q13)),'language']
#Den exei na kanei i glwsa erwtimatologiou
data[which(is.na(data$Q13)),]

#filter_.
which(is.na(data$filter_.))
data[which(is.na(data$filter_.)),]
#Paratiroume oti iparxoun 2 paratiriseis pou den mporoume na apantisoume 
#me vasi tis proigoumenes apantiseis an anikoun stin katigoria filter.
#O logos einai oti den fainetai an ergazontai i oxi
#Ta missing values einai oi paratiriseis 72 kai 243



#Q4
which(is.na(data$Q4))
data[which(is.na(data$Q4)),]
#Iparxei mia paratirisi i opoia exei missing values sto Q4 (etos apofoitisis)
#kai kat epektasi sto Gradyear kai Grad2cats
#H paratirisi einai me auth me arithmo 353

########################################################################
#MY STATS
library(dplyr)
data %>% 
  group_by(Q3) %>% 
  summarise(Q13 = mean(Q13,na.rm=TRUE))

#OR
tapply(data$Q13, data$Q3, FUN=mean, na.rm=TRUE)

tmima2<-subset(data, data$Q3 == "2")

library(ggplot2)
library(scales)
source("http://peterhaschke.com/Code/multiplot.R")

ggplot(data, aes(x = factor(Gradyear))) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FF6666") + 
  scale_y_continuous(labels = percent)+
  labs(title='Έτος Αποφοίτησης', x='Graduation Year',y='Percent')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('1997-2002','2003-2007','2008-2012','2013-2017'))



p1<-ggplot(data, aes(x = factor(Q1))) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "red") + 
  scale_y_continuous(labels = percent) +
  labs(title='Φύλο', x='Φύλο',y='Ποσοστό')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Γυναίκες','’νδρες'))

p2<-ggplot(data, aes(x = factor(Age))) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "blue") + 
  scale_y_continuous(labels = percent)+
  labs(title='Ηλικία', x='Ηλικιακή ομάδα',y='Ποσοστό')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('25-29','30-34','35-39','40+'))

multiplot(p1, p2, cols=2)


table(data$Q6)
round(sapply(table(as.factor(data$Emplmnt)),function(x) x/nrow(data)*100),1)

#To idio afairwntas ta na's
round(sapply(table(as.factor(data$Emplmnt)),function(x) x/length(which(!is.na(data$Q6)))*100),1)







#As kanoume ena grafima pou afora tin aksiologisi ana filo


data$Q13


plot(as.factor(data$Q13))
table(as.factor(data$Q13))
sapply(table(as.factor(data$Q13)),function(x) x/nrow(data)*100)
#######
plot(factor(data$Q1))

sapply(table(as.factor(data$Q1)),function(x) x/nrow(data)*100)
ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
  geom_bar(width=0.5) + 
  labs(fill='Gender')


ggplot(data, aes(x = factor(Q13),fill=factor(Age))) +  
  geom_bar(width=0.5) + 
  labs(fill='Age group')


ggplot(data, aes(x = factor(Q13),fill=factor(language))) +  
  geom_bar(width=0.5) + 
  labs(fill='Language group')


ggplot(data, aes(x = factor(Q13),fill=factor(Q3))) +  
  geom_bar(width=0.5) + 
  labs(fill='Tmima group')

ggplot(data, aes(x = factor(Q13),fill=factor(Q5_SQ001))) +  
  geom_bar(width=0.5) + 
  labs(fill='Satisfaction group')

ggplot(data, aes(x = factor(Q13),fill=factor(Q8))) +  
  geom_bar(width=0.5) + 
  labs(fill='Sinafeia me ergasia group')

ggplot(data, aes(x = factor(Q13),fill = factor(Q8))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+
  labs(title='Συνάφεια ΠΜΣ με εργασία και σύσταση ΠΜΣ', x='Θα προτείνατε το ΠΜΣ;',y='Ποσοστό',
       fill='Συνάφεια με εργασία')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
scale_fill_manual(values=c("darkred","red","blue","green4","darkgreen","yellow"),
  labels = c("Πολύ ", "Αρκετά ", "Όχι και τόσο ", "Καθόλου ", "Δεν έχω εργαστεί",'NA'))

