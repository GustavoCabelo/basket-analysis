# 1 Introduction
## 1.1 Goal

Conduct a market basket analysis through R to discover any interesting relationships (or associations) between customer’s transactions and the item(s) they’ve purchased. These associations can then be used to drive sales-oriented initiatives such as recommender systems like the ones used by Amazon and other eCommerce sites.

-----
# 2 Dataset explanation
> 2 documents were used for this analysis:

## 2.1 Transaction File
> A CSV dataset with 9835 online transactions

## 2.2 List of products
> A PDF file listing all the products available

  - 125 products
  - 17 products type
-----
# 3 Preprocessing
Loading the libraries

    require(dplyr)
    require(magrittr)
    require(arules)
    require(arulesViz)
    require(ggplot2)
    require(gridExtra)
    require(graphics)
    require(corrplot)

Loading the transaction file in a "transaction" extension. Using this extension instead as normal DF enables us to analyze the composition of each transaction and create association rules with apriori.

    transactions <- read.transactions(paste0(getwd(),"/Files/ElectronidexTransactions2017.csv"), format = c("basket","single"), sep = ",", rm.duplicates = F)


Understanding the transaction file

    summary(transactions)

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  125 columns (items) and a density of 0.03506172 
    ## 
    ## most frequent items:
    ##                     iMac                HP Laptop CYBERPOWER Gamer Desktop 
    ##                     2519                     1909                     1809 
    ##            Apple Earpods        Apple MacBook Air                  (Other) 
    ##                     1715                     1530                    33622 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14 
    ##    2 2163 1647 1294 1021  856  646  540  439  353  247  171  119   77   72 
    ##   15   16   17   18   19   20   21   22   23   25   26   27   29   30 
    ##   56   41   26   20   10   10   10    5    3    1    1    3    1    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   2.000   3.000   4.383   6.000  30.000 
    ## 
    ## includes extended item information - examples:
    ##                             labels
    ## 1 1TB Portable External Hard Drive
    ## 2 2TB Portable External Hard Drive
    ## 3                   3-Button Mouse

As saw in the summary, there were observations with no transactions (min = 0). 

    which(size(transactions) == 0)
    ## [1] 8707 9506

I have removed these transaction with 0 items.

    transactions <- transactions[-which(size(transactions) == 0),]

Double check if Rows were removed

    which(size(transactions) == 0) # Now we are sure there is no longer transactions with items <=0
    ## integer(0)
-----
# 4 Data Analysis
## 4.1 Visualization

### 4.1.1 Amount of items per transaction

> Histogram

    hist(size(transactions), breaks = 50, col = 'grey', xlab = "items x transactions", main = "Histogram")
![histogram](https://user-images.githubusercontent.com/19245251/37205415-030b9910-2395-11e8-9722-12f1e6cff132.png)

> Box Plot

    boxplot(size(transactions), outline = FALSE, horizontal=TRUE, main="BoxPlot", xlab="items x transactions")
![boxplot](https://user-images.githubusercontent.com/19245251/37205562-7e298f30-2395-11e8-807c-9e8e391a4252.png)

In the histogram and in the BoxPlot we can clearly see that the standard transactions have in average 3 products. This threshold will be adopted for the basket analysis composition.

-----
### 4.1.2 Best Seller items

> TOP 10 best seller products

    itemFrequencyPlot(transactions, type = c("absolute"), topN = 10, ylab = "item frequency")
![top10bestsellerproduct](https://user-images.githubusercontent.com/19245251/37205703-06ca1526-2396-11e8-819e-a459c0ec8ca1.png)

> TOP10 best sellers as standalone in the purchase basket

The following table shows the purchases made as single items compared to the total times this items has been sold.
    ##                          standalone standalone_perc total
    ## iMac                            121            4.8%  2519
    ## HP Laptop                        63            3.3%  1909
    ## CYBERPOWER Gamer Desktop        109           6.03%  1809
    ## Apple Earpods                   156            9.1%  1715
    ## Apple MacBook Air               383          25.03%  1530
    ## Lenovo Desktop Computer          41           2.82%  1456
    ## Dell Desktop                     35           2.66%  1318
    ## Apple MacBook Pro                67           6.16%  1087
    ## ViewSonic Monitor                25            2.3%  1085
    ## Acer Desktop                     60           5.99%  1002

Was observed that in 9 cases out of the TOP 10 Best Seller items the stand alone sells represent less than 10% of the total sells of these items, what means that in more than 90% of these sales there is a clear opportunity for cross-selling them with other items.

> Probability of two products appearing together in the basket

Overview about how the bestseller items correlate in the basket purchases with a second item. Which of them tend to sell as standalone or together with another item.

The values where the item crossed with itself represent its ocurrence (%) as standalone items in the purchase basket.
![corrmatrix](https://user-images.githubusercontent.com/19245251/37205932-f1827e78-2396-11e8-9e4a-93a49eaeb5b7.png)

 
# 5 Association rules
## 5.1 Create rules

*Rules threshold:*

  - support > 0.02
  - confidence > 0.2
  - lift > 1.5

*Metrics Legend:*
  - LHS: Antecedent 
  - RHS: Consequent 
  - support: indicates how frequently the item / combination appear in the dataset 
  - confidence: indicates the number of times the IF/THEN statement on the data are true. 
  - lift: indicates the ratio between the observed divided by the expected number of occurences.

> Visualize rules The 50 strongest rules after applied the rules threshold
![scatter_plot_for_50_rules](https://user-images.githubusercontent.com/19245251/37206083-86c81902-2397-11e8-9783-4d2140bd6d19.png)


> Strongest rules  
- 41 rules with 2 products
- 9 rules with 3 products
![two_key_plot](https://user-images.githubusercontent.com/19245251/37206294-5ad47452-2398-11e8-8233-3d3962077b9e.png)

> List TOP 20 rules

    ##      lhs                                         rhs         support    confidence lift     count
    ## [1]  {HP Laptop,Lenovo Desktop Computer}      => {iMac}      0.02308553 0.5000000  1.951767 227  
    ## [2]  {ASUS Monitor}                           => {iMac}      0.02766195 0.4990826  1.948185 272  
    ## [3]  {Dell Desktop,HP Laptop}                 => {iMac}      0.02227194 0.4954751  1.934104 219  
    ## [4]  {HP Laptop,ViewSonic Monitor}            => {iMac}      0.02369572 0.4936441  1.926956 233  
    ## [5]  {ASUS 2 Monitor}                         => {iMac}      0.02806875 0.4867725  1.900133 276  
    ## [6]  {iMac,ViewSonic Monitor}                 => {HP Laptop} 0.02369572 0.4794239  2.469447 233  
    ## [7]  {Microsoft Office Home and Student 2016} => {iMac}      0.03101800 0.4663609  1.820455 305  
    ## [8]  {Apple Magic Keyboard}                   => {iMac}      0.03234008 0.4510638  1.760743 318  
    ## [9]  {ViewSonic Monitor}                      => {iMac}      0.04942540 0.4479263  1.748495 486  
    ## [10] {ViewSonic Monitor}                      => {HP Laptop} 0.04800163 0.4350230  2.240745 472 
    ## [11] {Epson Printer}                          => {iMac}      0.02044137 0.4249471  1.658795 201  
    ## [12] {Belkin Mouse Pad}                       => {iMac}      0.02420421 0.4131944  1.612918 238  
    ## [13] {Dell Desktop,iMac}                      => {HP Laptop} 0.02227194 0.4078212  2.100632 219  
    ## [14] {Dell Desktop}                           => {iMac}      0.05461202 0.4074355  1.590438 537  
    ## [15] {ASUS Chromebook}                        => {iMac}      0.02135666 0.4069767  1.588647 210  
    ## [16] {Apple Magic Keyboard}                   => {HP Laptop} 0.02888233 0.4028369  2.074958 284  
    ## [17] {Lenovo Desktop Computer}                => {iMac}      0.05878165 0.3969780  1.549617 578  
    ## [18] {iMac,Lenovo Desktop Computer}           => {HP Laptop} 0.02308553 0.3927336  2.022917 227  
    ## [19] {HP Laptop}                              => {iMac}      0.07556188 0.3892090  1.519290 743  
    ## [20] {Logitech 3-button Mouse}                => {iMac}      0.02522119 0.3887147  1.517361 248

In the TOP 20 rules, all of them have either "iMac" or "HP Laptop" as the LHS product, the TOP 2 best-sellers. Although they are set as the consequence, in case they are were the Antecedent (RHS), the difference would be the "confidence" level, however it already gives important insights about the strenght of the rules concerning these items in the basket.


## 5.2 Association between 2 Products

The next step was to run the “apriori” function in R in order to identify the main basket composition rules associated with the Best Seller items as the RHS and the following item to be added in the basket.

The following metrics were adopted as threshold:

- Parameters applied
  - rules threshold (support, confidence and lift)
  - Minimum quantity of items = 2
  - Maximum quantity of items = 2

### 5.2.1 Reccomended products for baskets that contain as first product

**Metrics Legend**
  - LHS: Antecedent
  - RHS: Consequent
  - support: indicates how frequently the item / combination appear in the dataset
  - confidence: indicates the number of times the IF/THEN statement on the data are true.
  - lift: indicates the ratio between the observed divided by the expected number of occurences.

> 1- iMac

    # set of 4 rules
    ##     lhs       rhs                        support    confidence lift     count
    ## [1] {iMac} => {HP Laptop}                0.07556188 0.2949583  1.519290 743  
    ## [2] {iMac} => {Lenovo Desktop Computer}  0.05878165 0.2294561  1.549617 578  
    ## [3] {iMac} => {CYBERPOWER Gamer Desktop} 0.05674769 0.2215165  1.204075 558  
    ## [4] {iMac} => {Dell Desktop}             0.05461202 0.2131798  1.590438 537

    #Plot association rules    
![rules1](https://user-images.githubusercontent.com/19245251/37206662-f50db258-2399-11e8-9061-ff6d6ed2ecf9.png)
  
> 2- HPLaptop

    # set of 5 rules
    ##     lhs            rhs                        support    confidence lift     count
    ## [1] {HP Laptop} => {iMac}                     0.07556188 0.3892090  1.519290 743  
    ## [2] {HP Laptop} => {ViewSonic Monitor}        0.04800163 0.2472499  2.240745 472  
    ## [3] {HP Laptop} => {Lenovo Desktop Computer}  0.04617106 0.2378208  1.606107 454  
    ## [4] {HP Laptop} => {Dell Desktop}             0.04495068 0.2315348  1.727376 442  
    ## [5] {HP Laptop} => {CYBERPOWER Gamer Desktop} 0.04261161 0.2194866  1.193042 419

    #Plot association rules    
![rules2](https://user-images.githubusercontent.com/19245251/37206807-7315dc84-239a-11e8-81b0-44d597e35c23.png)

> 3- CyberPower

    # set of 3 rules
    ##     lhs                           rhs             support    confidence lift     count
    ## [1] {CYBERPOWER Gamer Desktop} => {iMac}          0.05674769 0.3084577  1.204075 558  
    ## [2] {CYBERPOWER Gamer Desktop} => {HP Laptop}     0.04261161 0.2316197  1.193042 419  
    ## [3] {CYBERPOWER Gamer Desktop} => {Apple Earpods} 0.03834028 0.2084024  1.194881 377

    #Plot association rules    
![rules3](https://user-images.githubusercontent.com/19245251/37206857-a14d9524-239a-11e8-85d7-119a078bb0dc.png)

> 4- AppleEarpods

    # set of 2 rules
    ##     lhs                rhs                        support    confidence lift      count
    ## [1] {Apple Earpods} => {iMac}                     0.04027255 0.2309038  0.9013406 396  
    ## [2] {Apple Earpods} => {CYBERPOWER Gamer Desktop} 0.03834028 0.2198251  1.1948811 377

    #Plot association rules    
![rules4](https://user-images.githubusercontent.com/19245251/37206897-d048e77a-239a-11e8-9264-1461980df758.png)

> 5- MacBookAir

    # set of 4 rules
    ##     lhs                    rhs                        support    confidence lift      count
    ## [1] {Apple MacBook Air} => {Apple Earpods}            0.03000102 0.1928105  1.1054841 295  
    ## [2] {Apple MacBook Air} => {iMac}                     0.02847554 0.1830065  0.7143721 280  
    ## [3] {Apple MacBook Air} => {CYBERPOWER Gamer Desktop} 0.02410251 0.1549020  0.8419851 237  
    ## [4] {Apple MacBook Air} => {HP Laptop}                0.02410251 0.1549020  0.7978790 237

    #Plot association rules    
![rules5](https://user-images.githubusercontent.com/19245251/37206938-ee440f3e-239a-11e8-8882-d00ae044e894.png)

    **Confidence level exception** (Otherwise no rule would be created.)
    - confidence > 0.15

> 6- Lenovo Desktop

    # set of 4 rules
    ##     lhs                          rhs                        support    confidence lift     count
    ## [1] {Lenovo Desktop Computer} => {iMac}                     0.05878165 0.3969780  1.549617 578  
    ## [2] {Lenovo Desktop Computer} => {HP Laptop}                0.04617106 0.3118132  1.606107 454  
    ## [3] {Lenovo Desktop Computer} => {Dell Desktop}             0.03671311 0.2479396  1.849765 361  
    ## [4] {Lenovo Desktop Computer} => {CYBERPOWER Gamer Desktop} 0.03600122 0.2431319  1.321568 354

    #Plot association rules    
![rules6](https://user-images.githubusercontent.com/19245251/37206980-250c9004-239b-11e8-8903-836353c87c7e.png)

> 7- DellDesktop

    # set of 6 rules
    ##     lhs               rhs                        support    confidence lift     count
    ## [1] {Dell Desktop} => {iMac}                     0.05461202 0.4074355  1.590438 537  
    ## [2] {Dell Desktop} => {HP Laptop}                0.04495068 0.3353566  1.727376 442  
    ## [3] {Dell Desktop} => {Lenovo Desktop Computer}  0.03671311 0.2738998  1.849765 361  
    ## [4] {Dell Desktop} => {CYBERPOWER Gamer Desktop} 0.03040781 0.2268589  1.233114 299  
    ## [5] {Dell Desktop} => {Acer Desktop}             0.02766195 0.2063733  2.025218 272  
    ## [6] {Dell Desktop} => {Apple Earpods}            0.02715346 0.2025797  1.161496 267

    #Plot association rules    
![rules7](https://user-images.githubusercontent.com/19245251/37207010-4a96a8e6-239b-11e8-99de-7aac0f6553a0.png)

## 5.3 Association between 3 Products

The following metrics were adopted as threshold:

  - Parameters applied
    - support > 0.015
    - confidence > 0.2
    - Minimum quantity of items = 3
    - Maximum quantity of items = 3
    
### 5.3.1 Reccomended products for baskets that contain as first product

  **Metrics Legend**
    - LHS: Antecedent
    - RHS: Consequent
    - support: indicates how frequently the item / combination appear in the dataset
    - confidence: indicates the number of times the IF/THEN statement on the data are true.
    - lift: indicates the ratio between the observed divided by the expected number of occurences.

> 1- iMac & HP Laptop

    # set of 5 rules
    ##     lhs                 rhs                        support    confidence lift     count
    ## [1] {HP Laptop,iMac} => {ViewSonic Monitor}        0.02369572 0.3135935  2.841996 233  
    ## [2] {HP Laptop,iMac} => {Lenovo Desktop Computer}  0.02308553 0.3055182  2.063297 227  
    ## [3] {HP Laptop,iMac} => {Dell Desktop}             0.02227194 0.2947510  2.199004 219  
    ## [4] {HP Laptop,iMac} => {CYBERPOWER Gamer Desktop} 0.01800061 0.2382234  1.294887 177  
    ## [5] {HP Laptop,iMac} => {Acer Desktop}             0.01596664 0.2113055  2.073620 157

    #Plot association rules 
 ![3rules](https://user-images.githubusercontent.com/19245251/37207253-6e1926b2-239c-11e8-8c58-e74009e8dd70.png)
