    ## Warning: package 'foreign' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

ΔΙΑΓΡΑΜΜΑΤΑ
===========

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-3-1.png)

    ## Loading required package: grid

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-3-2.png)

Γραφήματα που αφορούν την επιλογή των απόφοιτων για τη σύσταση του μεταπτυχιακού σε τρίτους
-------------------------------------------------------------------------------------------

``` r
ggplot(data, aes(x = factor(Q13))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(Q13))) + 
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y=(..count..)/sum(..count..) ), stat= "count", vjust = -.3) +
  scale_y_continuous(labels = percent) + 
  labs(title='Θα συστήνατε το μεταπτυχιακό;', x='Απάντηση',y='Πλήθος',
       fill='Θα συστήνατε 
το μεταπτυχιακό?')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))+
scale_fill_discrete(labels=c('Σίγουρα ναι','Μάλλον ναι','Ούτε ναι ούτε όχι','Μάλλον όχι','Σίγουρα όχι'))
```

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-4-1.png)

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-5-1.png)![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-5-2.png)

### Το επόμενο διάγραμμα μας δείχνει την απάντηση για τη σύσταση ΠΜΣ σε τρίτους, ανα βαθμό ικανοποίησης από τις σπουδές των αποφοίτων.Υπάρχει όπως φαίνεται μια σύνδεση ανάμεσα στις 2 μεταβλητές.

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Στο επόμενο γράφημα παρατηρούμε την απάντηση για σύσταση του ΠΜΣ σε τρίτους ανά ηλικιακή ομάδα και φύλο

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Στο παρακάτω γράφημα παρατηρούμε το πως η ικανοποίηση από την τρέχουσα απασχόληση επηρεάζει την απάντηση για σύσταση του ΠΜΣ σε τρίτους

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-8-1.png)

### Στο παρακάτω γράφημα ελέγχουμε αν η εργασιακή κατάσταση επηρεάζει την απόφαση για την σύσταση του ΠΜΣ σε τρίτους

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-10-1.png)

### Στο παρακάτω γράφημα παρατηρούμε τον αριθμό των αποφοίτων σε κάθε τμήμα , ανα ηλικιακή ομάδα και φύλο. Οι γυναίκες υπερισχύουν στο τμήμα Κράτος και Δημόσια Πολιτική ,σε κάθε ηλικιακή ομάδα

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-11-1.png)

### Το ίδιο γράφημα, αυτή τη φορά ανα ηλικιακή ομάδα και εργασιακή κατάσταση.

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-12-1.png)

### Στο παρακάτω γράφημα παρατηρούμε οτι το Τμήμα Πολιτικής Επιστήμης έχει συγκριτικά με τα υπόλοιπα τμήματα μεγαλύτερη συμβολή στην εύρεση εργασίας

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-13-1.png)

### Στο παρακάτω γράφημα μπορούμε να παρατηρήσουμε αν η σχέση του ΠΜΣ με την απασχόληση των αποφοίτων, επηρεάζει την απόφασή τους για τη σύσταση του ΠΜΣ

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-14-1.png)
