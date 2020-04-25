getwd()
setwd("C:/Users/User/Desktop/’γγελος/ΕΛΚΕ/Έρευνα/sodanet")
data<-read.csv('My-encoding.csv',sep=';',header=TRUE)
str(data)
summary(data)

Missing_values<-data.frame(sort(sapply(data,function(x) sum(is.na(x))),decreasing = TRUE))
library(grid)

#Q7
#In variable Q7 
#test<-data.frame(cbind(data$Q6,data$Q7))
 #prepei na simpiptoun ta Na's tis erotisis 6 me ta na's ths 7     
#which((test[,1]<=15) & is.na(test[,2]==TRUE))
#Ara vriskoume missing values stis paratiriseis 121 kai 463 ta opoia
#aforoun atoma pou ergazontai alla den apantisan.
#data[c(121,463),]
#Twra tha vroume poia atoma den exoun apantisei stin erotisi 6 
# kai an afta simpiptoun me afta tis erotisis 7
#which(is.na(data$Q6) & is.na(data$Q7))
#identical(which(is.na(data$Q6)),which(is.na(data$Q6) & is.na(data$Q7)))
#epomenws ontws ta missing values ths Q6 antistoixoun stis idies paratiriseis
#tis q7. autes einai  68  72 169 184 243 338 351 360 391 430 445 453

#Ara sinolika ta missing values ths Q7 einai
# 68 72 121 169 184 243 338 351 360 391 430 445 453 kai 463
#length(which(is.na(data$Q6) & is.na(data$Q7))) + 
 #length( which((test[,1]<=15) & is.na(test[,2]==TRUE)))
#14 missing values gia tin Q7


#Q13
# data[which(is.na(data$Q13)),]
# which(is.na(data$Q13)) #Paratiriseis me missing values gia tin Q13

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
data$Q3<-as.factor(data$Q3)
#data$Q13<-as.numeric(data$Q13)
data %>% group_by(Q3)%>%group_by(Gradyear)%>%
  summarise(mean(na.omit(Q5_SQ001)))

test<-data.frame(data %>% group_by(Q3,Gradyear)%>%
  summarise(mean(na.omit(Q5_SQ001))))

plot(x=test[1:4,"Gradyear"],y=test[1:4,"mean.na.omit.Q5_SQ001.."],ylim = c(0,10),type = 'b',pch=as.character(as.numeric(test[1:4,"mean.na.omit.Q5_SQ001.."])))
#Plot που απεικονίζει τον μ.ο της βαθμολόγησης του ΠΜΣ ανα χρονική περίοδο
plot(x=test[1:4,"Gradyear"],y=test[1:4,"mean.na.omit.Q5_SQ001.."],ylim = c(0,10),type = 'b')
text(test[1:4,"Gradyear"],test[1:4,"mean.na.omit.Q5_SQ001.."],label=round(test[1:4,"mean.na.omit.Q5_SQ001.."],2),col='blue',cex=.8,pos = 3)

plot(x=test[5:8,"Gradyear"],y=test[5:8,"mean.na.omit.Q5_SQ001.."],ylim = c(0,10),type = 'b')
text(test[5:8,"Gradyear"],test[5:8,"mean.na.omit.Q5_SQ001.."],label=round(test[5:8,"mean.na.omit.Q5_SQ001.."],2),col='blue',cex=.8,pos = 3)

plot(x=test[9:12,"Gradyear"],y=test[9:12,"mean.na.omit.Q5_SQ001.."],ylim = c(0,10),type = 'b')
text(test[9:12,"Gradyear"],test[9:12,"mean.na.omit.Q5_SQ001.."],label=round(test[9:12,"mean.na.omit.Q5_SQ001.."],2),col='blue',cex=.8,pos = 3)

plot(x=test[13:16,"Gradyear"],y=test[13:16,"mean.na.omit.Q5_SQ001.."],ylim = c(0,10),type = 'b')
text(test[13:16,"Gradyear"],test[13:16,"mean.na.omit.Q5_SQ001.."],label=round(test[13:16,"mean.na.omit.Q5_SQ001.."],2),col='blue',cex=.8,pos = 3)



#OR
tapply(data$Q13, data$Q3, FUN=mean, na.rm=TRUE)

#tmima2<-subset(data, data$Q3 == "2")

library(ggplot2)
library(scales)
source("http://peterhaschke.com/Code/multiplot.R")



p3<-ggplot(data, aes(x = factor(Gradyear))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Gradyear))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) +
  labs(title='Έτος Αποφοίτησης', x='Graduation Year',y='Percent')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('1997-2002','2003-2007','2008-2012','2013-2017'))+
  scale_fill_discrete(name = "Φύλο", 
                      labels=c('1997-2002','2003-2007','2008-2012','2013-2017'))


p1<-ggplot(data, aes(x = factor(Q1))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Q1))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) +
  labs(title='Ποσοστό Φύλων', x='Φύλο',y='Ποσοστό')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Γυναίκες','’νδρες'))+
  scale_fill_discrete(name = "Φύλο", 
                       labels=c("Γυναίκες", "’νδρες"))


p2<-ggplot(data, aes(x = factor(Age))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Age))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) +
  labs(title='Ποσοστό Ηλικίας', x='Ηλικία',y='Ποσοστό')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('25-29','30-34','35-39','40+'))+
  scale_fill_discrete(name = "Ηλικία", 
                      labels=c('25-29','30-34','35-39','40+'))



multiplot(p1, p2, cols=2)


table(data$Q6)
round(sapply(table(as.factor(data$Emplmnt)),function(x) x/nrow(data)*100),1)

#To idio afairwntas ta na's
round(sapply(table(as.factor(data$Emplmnt)),function(x) x/length(which(!is.na(data$Q6)))*100),1)

data.frame(attr(data$Emplmnt,"value.labels"))






#As kanoume ena grafima pou afora tin aksiologisi ana filo


#data$Q13
#install.packages('egg')
library(egg)

ggplot(data, aes(x = factor(Q13),fill=factor(Q13))) +  
  geom_bar(width=0.5) + 
  labs(fill='Θα συστήνατε 
το μεταπτυχιακό?')

ggplot(data, aes(x = factor(Q13))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Q13))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) + 
  labs(title='Θα συστήνατε το μεταπτυχιακό?', x='Απάντηση',y='Πλήθος',
       fill='Θα συστήνατε 
το μεταπτυχιακό?')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
scale_fill_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))


data$Q3Factor <- factor(data$Q3, labels = c("Πολιτική Επιστήμη και Κοινωνιολογία", "Διεθνείς και Ευρωπαϊκές σπουδές", "Κράτος και Δημόσια Πολιτική", "Σπουδές Νοτ/ης Ευρώπης"))


ggplot(data, aes(x = factor(Q13),fill=factor(Q13))) +  
  geom_bar(width=0.5) + 
  labs(title='Θα συστήνατε το μεταπτυχιακό;
(Διαχωρισμός ανα τμήμα)',
       x='Απάντηση',y='Πλήθος')+
  facet_wrap(~factor(Q3Factor))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
  scale_fill_discrete(name='Απάντηση',labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))

ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
  geom_bar(width=0.5) + 
  labs(title='Θα συστήνατε το μεταπτυχιακό;
(Διαχωρισμός ανα τμήμα και φύλο)',
       x='Απάντηση',y='Πλήθος')+
  facet_wrap(~factor(Q3Factor))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
  scale_fill_discrete(name='Φύλο',labels=c('Γυναίκες','’νδρες'))





#ME POSOSTO stat(density)
# library(dplyr)
# dfl <- data %>% 
#   group_by(Q13,Q3Factor) %>% 
#   summarise(n=n()) %>% 
#   group_by(factor(Q13)) %>% 
#   mutate(perc=100*n/sum(n))
# 
# ggplot(dfl, aes(x = factor(Q13), y=perc, fill= factor(Q13))) +
#   geom_bar(stat="identity") +
#   ylab("percent") + 
#   facet_wrap(~ Q3Factor)



#ggarrange(p1,p2,ncol = 1)

plot(as.factor(data$Q13))
table(as.factor(data$Q13))
sapply(table(as.factor(data$Q13)),function(x) x/nrow(data)*100)

#######
plot(factor(data$Q1))

sapply(table(as.factor(data$Q1)),function(x) x/nrow(data)*100)

ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
  geom_bar(width=0.5) + 
  facet_wrap(~factor(Q5_SQ001))+
  labs(fill='Gender')
  


ggplot(data, aes(x = factor(Q13),fill=factor(Q13))) +  
  geom_bar(width=0.5) + 
  labs(title='Πρόταση ΠΜΣ
(Διαχωρισμός ανα Βαθμό Ικανοποίησης από τις σπουδές)',
       x='Βαθμολογία',y='Πλήθος')+
  facet_wrap(~factor(Q5_SQ001))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_discrete(name="Θα συστήνατε 
το ΠΜΣ;",
                      labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))













  ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
    geom_bar(width=0.5) + 
    facet_wrap(~factor(Age))+
  labs(fill='Gender')
  
  #LABELS AGE#
  data$AgeFactor <- factor(data$Age, labels = c('25-29','30-34','35-39','40+'))
 
   ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
    geom_bar(width=0.5) + 
    labs(title='Γνώμη για σύσταση ΠΜΣ ανά Ηλικίακή ομάδα', x='Γνώμη',y='Πλήθος')+
    facet_wrap(~factor(AgeFactor))+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_x_discrete(labels = c("Σίγουρα ναι", "Μάλλον ναι", "Ούτε ναι ούτε οχι", "Μάλλον όχι", "Σίγουρα όχι"))+
    scale_fill_discrete(name = "Φύλο", 
                        labels=c("Γυναίκες", "’νδρες"))
  
  
  
  
  
  # ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
  #   geom_bar(width=0.5) + 
  #   facet_wrap(~factor(Age2cats))+
  # labs(fill='Gender')

  # Στο παρακάτω γράφημα παρατηρούμε η τελευταία κατηγορία 
  # αποτελείται κυρίως από τα τα άτομα που δεν εργάζονται
  
   data$Q7Factor <- factor(data$Q7, labels = c("Πολύ ευχαριστημένος", "Αρκετά ευχαριστημένος", "Ούτε ευχαριστημένος ούτε δυσαρεστημένος", "Αρκετά δυσαρεστημένος ", "Πολύ δυσαρεστημένος"))
  ggplot(data, aes(x = factor(Q13),fill=factor(Q13))) +  
    geom_bar(width=0.5) + 
    facet_wrap(~factor(Q7Factor))+
  scale_fill_discrete(name = "Θα συστήνατε
το μεταπτυχιακό;",labels = c("Σίγουρα ναι", "Μάλλον ναι", "Ούτε ναι ούτε οχι", "Μάλλον όχι", "Σίγουρα όχι"))+
    labs(title='Σύσταση ΠΜΣ ανα ικανοποίηση από τρέχουσα απασχόληση', x='Θα συστήνατε το ΠΜΣ;',y='Πλήθος',
         fill='Συνάφεια με εργασία')+
    theme(plot.title = element_text(hjust = 0.5))
    
  
  
  # ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
  #   geom_bar(width=0.5) + 
  #   facet_wrap(~factor(language))+
  #   labs(fill='Gender')
  
  ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
    geom_bar(width=0.5) + 
    facet_wrap(~ factor(Q3))+
    labs(fill='Gender')
 
  
  ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
    geom_bar(width=0.5) + 
    facet_wrap(~ factor(Q8))+
    labs(fill='Gender')
  
  
ggplot(data, aes(x = factor(Q13),fill=factor(Age))) +  
  geom_bar(width=0.5) + 
  labs(fill='Age group')


# ggplot(data, aes(x = factor(Q13),fill=factor(language))) +  
#   geom_bar(width=0.5) + 
#   labs(fill='Language group')


ggplot(data, aes(x = factor(Q13),fill=factor(Q3))) +  
  geom_bar(width=0.5) + 
  labs(fill='Tmima group')

# ggplot(data, aes(x = factor(Q13),fill=factor(Q5_SQ001))) +
#   geom_bar(width=0.5) +
#   labs(fill='Satisfaction group')

# ggplot(data, aes(x = factor(Q13),fill=factor(Q8))) +  
#   geom_bar(width=0.5) + 
#   labs(fill='Sinafeia me ergasia group')

ggplot(data, aes(x = factor(Q13),fill = factor(Q8))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+
  labs(title='Σχέση ΠΜΣ με τωρινή/πρόσφατη εργασία και σύσταση ΠΜΣ', x='Θα προτείνατε το ΠΜΣ;',y='Ποσοστό',
       fill='Σχέση ΠΜΣ
με εργασία')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
scale_fill_discrete(
  labels = c("Πολύ σχετικό ", "Αρκετά σχετικό", "Όχι και τόσο ", "Καθόλου σχετικό ", "Δεν έχω εργαστεί",'NA'))


# ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
#   geom_bar(width=0.5) + 
#   facet_wrap(~factor(Age2cats))+
#   labs(fill='Gender')

ggplot(data, aes(x = factor(Q13),fill = factor(Q13))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+
  labs(title='Συνάφεια ΠΜΣ με εργασιακή κατάσταση και σύσταση ΠΜΣ', x='Θα προτείνατε το ΠΜΣ;',y='Συνολικό Ποσοστό',
       fill='Απάντηση')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(work))+
  scale_fill_discrete( labels = c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι',"NA"))




qplot(as.factor(data$age2017),bins=30) #further investigate for normal distribution


ggplot(data, aes(x = factor(age2017),fill =factor(age2017))) +  
  geom_bar(aes(y = (..count..))) + 
  labs(title='Πίνακας συχνότητας αποφοίτων ανα ηλικία', x='Ηλικία',y='Αριθμός αποφοίτων')+
  theme(plot.title = element_text(hjust = 0.5),legend.position="none")+
  geom_vline(xintercept=11)

?geom_vline
?stat_summary
median(data$age2017)
mean(data$age2017)
paste("Η Διάμεσος της ηλικίας είναι", median(data$age2017))


data$work <- rep('’νεργοι',nrow(data))
data[(data$Q6<=15) & (!is.na(data$Q6)),'work'] <- 'Εργαζόμενοι'
data[is.na(data$Q6),'work'] <- NA

length(data[(data$Q6<=15) & (!is.na(data$Q6)),'work'])
length(data[(data$Q6>15) & (!is.na(data$Q6)),'work'])
length(data[is.na(data$Q6),'work'])
summary(factor(data$work))

# 
# ggplot(data, aes(x = factor(Q13),fill=factor(Q1))) +  
#   geom_bar(width=0.5) + 
#   facet_wrap(~factor(work))+
#   labs(fill='Gender')

ggplot(data, aes(x = factor(Q13),fill = factor(Q1))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+
  labs(title='Συνάφεια ΠΜΣ με εργασιακή κατάσταση και σύσταση ΠΜΣ', x='Θα προτείνατε το ΠΜΣ;',y='Ποσοστό',
       fill='Φύλο')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(work))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
  scale_fill_manual(values=c("darkred","blue"),
                    labels = c("Γυναίκες","’νδρες "))






ggplot(data, aes(x = factor(Q3Factor),fill=factor(Q1))) +  
  geom_bar(width=0.5) + 
  labs(title='Τμήμα ΠΜΣ ανά Ηλικίακή ομάδα και φύλο', x='Τμήμα',y='Πλήθος')+
  facet_wrap(~factor(AgeFactor))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("ΠΕ", "ΔΣ", "ΚΔΠ", "ΣΝΕ"))+
  scale_fill_discrete(name = "Φύλο", 
                      labels=c("Γυναίκες", "’νδρες"))

ggplot(data, aes(x = factor(Q3Factor),fill=factor(work))) +  
  geom_bar(width=0.5) + 
  labs(title='Τμήμα ΠΜΣ ανά Ηλικίακή ομάδα και εργασιακή κατάσταση', x='Τμήμα',y='Πλήθος')+
  facet_wrap(~factor(AgeFactor))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("Πολ.Επ.", "Διεθ.Σπ", "Κρ.Δημ.Πολ.", "Σπ.Νοτ.Ευρ."))+
  scale_fill_discrete(name = "Κατάσταση")

# Στο παρακάτω γράφημα παρατηρούμε οτι το τμήμα πολιτικής επιστήμης έχει 
# συγκριτικά με τα υπόλοιπα τμήματα μεγαλύτερη συμβολή στην εύρεση εργασίας
ggplot(data, aes(x = factor(Q9_SQ002),fill=factor(work))) +  
  geom_bar(width=0.5) + 
  labs(title='Συμβολή στην εύρεση εργασίας ανα Τμήμα ΠΜΣ και εργασιακή κατάσταση', x='Τμήμα',y='Πλήθος')+
  facet_wrap(~factor(Q3Factor))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = c("NA","Πολύ", "Αρκετά", "Ούτε πολύ ούτε λίγο", "Λίγο","Καθόλου"))+
  scale_fill_discrete(name = "Κατάσταση")





ggplot(data, aes(x = factor(Q11_SQ001),fill = factor(Q1))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+
  labs(title='Μετά την ολοκλήρωση του ανωτέρω ΠΜΣ, συνεχίσατε τις σπουδές σας;', x='',y='Ποσοστό',
       fill='Φύλο')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Ναι','Όχι'))+
  scale_fill_manual(values=c("darkred","blue"),
                    labels = c("Γυναίκες","’νδρες "))


ggplot(data, aes(x = factor(Q11_SQ001),fill = factor(Q1))) +  
  geom_bar(width=0.5) + 
  labs(title='Συνέχιση σπουδών μετά το ΠΜΣ ανα τμήμα και φύλο;', x='',y='Πλήθος',
       fill='Φύλο')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(Q3Factor))+
  scale_x_discrete(labels=c('Ναι','Όχι'))+
  scale_fill_manual(values=c("darkred","blue"),
                    labels = c("Γυναίκες","’νδρες "))


ggplot(data, aes(x = factor(Q11_SQ001),fill = factor(Q12_SQ001))) +  
  geom_bar(width=0.5) + 
  labs(title='Συνέχιση σπουδών μετά το ΠΜΣ ανα Hλικία', x='',y='Πλήθος',
       fill='Συγγραφική
δραστηριότητα')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(AgeFactor))+
  scale_x_discrete(labels=c('Μη συνέχιση σπουδών','Συνέχιση σπουδών'))+
  scale_fill_manual(values=c("darkred","darkblue"),
                    labels = c("Όχι","Ναι"))


data$Q1Factor <- factor(data$Q1, labels = c("Γυναίκες",  "’νδρες"))

ggplot(data, aes(x = factor(Q11_SQ001),fill = factor(Q11_SQ001))) +  
  geom_bar(width=0.5) + 
  labs(title='Συνέχιση σπουδών μετά το ΠΜΣ ανα Φύλο;', x='Συνέχιση',y='Πλήθος',
       fill='Συνέχιση')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(Q1Factor),labeller=label_parsed)+
  scale_x_discrete(labels=c('Ναι','Όχι'))+
  scale_fill_discrete(labels = c("Ναι","Όχι"))


ggplot(data, aes(x = factor(Q5_SQ001))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(work))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) + 
  labs(title='Βαθμός Ικανοποίησης', x='',y='Πλήθος',
       fill='')+
  theme(plot.title = element_text(hjust = 0.5))
 
ggplot(data, aes(x = factor(Q5_SQ001))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(AgeFactor))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) + 
  labs(title='Βαθμός Ικανοποίησης', x='',y='Πλήθος',
       fill='Ηλ.Ομάδα')+
  theme(plot.title = element_text(hjust = 0.5))




ggplot(data, aes(x = factor(Q5_SQ001))) +  
  geom_bar(aes(y = (..count..), fill = factor(Q1Factor))) +
  geom_text(stat='count', aes(label=..count..), vjust=-.3) +
  labs(title='Βαθμός Ικανοποίησης', x='',y='Πλήθος',
       fill='Φύλο')+
  theme(plot.title = element_text(hjust = 0.5))



ggplot(data, aes(x = factor(Q5_SQ001))) +  
  geom_bar(aes(y = (..count..), fill = factor(AgeFactor))) +
  geom_text(stat='count', aes(label=..count..), vjust=-.3) +
  labs(title='Βαθμός Ικανοποίησης ανά τμήμα και ηλ.ομάδα', x='Βαθμός Ικανοποίησης',y='Πλήθος',
       fill='Ηλ.Ομάδα')+
  theme(plot.title = element_text(hjust = 0.5))+
 facet_wrap(~factor(Q3Factor))

 
ggplot(data, aes(x = factor(Q5_SQ001))) +  
  geom_bar(aes(y = (..count..), fill = factor(Gradyear))) +
  geom_text(stat='count', aes(label=..count..), vjust=-.3) +
  labs(title='Βαθμός Ικανοποίησης', x='Βαθμός',y='Πλήθος',
       fill='Έτος')+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(Q3))


round(sapply(table(as.factor(data$Q5_SQ001)),function(x) x/length(which(!is.na(data$Q5_SQ001)))*100),1)


