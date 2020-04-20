This is my first Markdown Document! \#md\_document: \#variant:
markdown\_github

Let’s load some libraries
-------------------------

    ## Warning: package 'foreign' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Now let’s load the data

PLOTS
=====

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

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-6-1.png)

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-7-1.png)

### Στο παρακάτω γράφημα η τελευταία κατηγορία (ΝΑ)

### αποτελείται κυρίως από τα άτομα που δεν εργάζονται

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-8-1.png)

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-10-1.png)

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-11-1.png)![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-11-2.png)

### Στο παρακάτω γράφημα παρατηρούμε οτι το τμήμα πολιτικής επιστήμης έχει συγκριτικά με τα υπόλοιπα τμήματα μεγαλύτερη συμβολή στην εύρεση εργασίας

![](Markdown_presentation_files/figure-markdown_github/unnamed-chunk-12-1.png)
