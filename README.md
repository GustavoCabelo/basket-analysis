# 1 Introduction
## 1.1 Goal

Conduct a market basket analysis through R to discover any interesting relationships (or associations) between customer’s transactions and the item(s) they’ve purchased. These associations can then be used to drive sales-oriented initiatives such as recommender systems like the ones used by Amazon and other eCommerce sites..

# 2 Dataset explanation
- 2 documents were used for this analysis:

## 2.1 Transaction File
- A CSV dataset with 9835 online transactions

## 2.2 List of products
A PDF file listing all the products available

  - 125 products
  - 17 products type

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

As saw in the summary, there were observations with no transactions (min = 0). I have removed these transaction with 0 items

which(size(transactions) == 0)
transactions <- transactions[-which(size(transactions) == 0),]

Double check if Rows were removed


which(size(transactions) == 0) # Now we are sure there is no longer transactions with items <=0

Data Analysis
Visualization
Amount of items per transaction
Histogram

  hist(size(transactions), breaks = 50, col = 'grey', xlab = "items x transactions", main = "Histogram")


Box Plot

boxplot(size(transactions), outline = FALSE, horizontal=TRUE, main="BoxPlot", xlab="items x transactions")

In the histogram and in the BoxPlot we can clearly see that the standard transactions have in average between 2 to 6 products 
Best Seller items
TOP 10 best seller products

itemFrequencyPlot(transactions, type = c("absolute"), topN = 10, ylab = "item frequency")
#Vector with the TOP 10 items
top10 <- c(unlist(attributes(head(sort(itemFrequency(transactions, type = c("relative")), decreasing=T), n=10L)))) 
TOP10 best sellers as standalone in the purchase basket

The following table shows the purchases made as single items compared to the total times this items has been sold.

## Total transactions
    bestsellers <- as.data.frame(head(sort(itemFrequency(transactions, type = c("absolute")), decreasing=T), n=10L))
    colnames(bestsellers)[1] <- "total"

## Stand alone transactions
  
    standalone = c() # vector to store values
    # Loop over my_matrix
      for(row in 1:10) {
        for(col in 1:10) {
          tbl2 <- transactions[which(size(transactions)==1),]
          tbl2 <-  crossTable(tbl2,sort=T)
          tbl2 <- tbl2[top10,top10]
          tbl2 <- tbl2[row, col]
          
          standalone <- c(standalone,c(tbl2))
        }
      }

    # function to retrun only values different than 0
      isGoodNumber <- function(X) 
      {
        if (X!=0) return(TRUE) else return(FALSE)
      }
    
    # create column in 'bestsellers' with the number of standalone transactions per item 
      isGoodNumber = Vectorize(isGoodNumber)
      standalone <- standalone[isGoodNumber(standalone)]
      bestsellers$standalone <- standalone

      
## Stand alone transactions (%) 
      standalone_perc <- c() 
      # Loop to create % of standalone transactions
      for(row in 1:10) {
        v <- (bestsellers[row,2]/bestsellers[row,1])*100
        v <-  paste(round(v,digits = 2),"%", sep = "")
        standalone_perc <- c(standalone_perc,c(v))
      }

    # create column in 'bestsellers' with the (%) of standalone transactions per item 
    bestsellers$standalone_perc <- standalone_perc

## Product legend (shortner) 
    # create vector to col and row names in the table
      # names <- c(paste0("i",seq_along(1:10)))
    # create column in 'bestsellers' 
      # bestsellers$legend <- names 

bestsellers <- bestsellers[,c(2,3,1)]            
bestsellers

Was observed that in 9 cases out of the TOP 10 Best Seller items the stand alone sells represent less than 10% of the total sells of these items (Table 1), what means that in more than 90% of these sales there is a clear opportunity for cross-selling them with other items.

Probability of two products appearing together in the basket

Overview about how the bestseller items correlate in the basket purchases with a second item. Which of them tend to sell as standalone or together with another item.

The values where the item crossed with itself represent its ocurrence (%) as standalone items in the purchase basket.

  tbl <- transactions[which(size(transactions)>=1),]
  tbl <- crossTable(tbl, 
                     #measure = "support", #<--- comment this row to see the absolute numbers
                     sort=T)
  tbl <- tbl[top10,top10]

  # Loop to replace the diagonal by the standalone value for each product
  for (i in 1:10) tbl[i,i] <- standalone[i]
  
  # Function to transform matrix from absolute to relative(%) values from the total transactions
  support <- function(x){  
  round((x/length(transactions)*100), digits=2)
  }
  
  tbl <- apply(tbl,2,support)
  
  corrplot(tbl, is.corr = FALSE, method = "number", type = "lower")
rm(list = c("standalone", "col", "row", "tbl2", "standalone_perc", "v"))
Association rules
Create rules
Rules threshold:

support > 0.02
confidence > 0.2
lift > 1.5
Metrics Legend: - LHS: Antecedent - RHS: Consequent - support: indicates how frequently the item / combination appear in the dataset - confidence: indicates the number of times the IF/THEN statement on the data are true. - lift: indicates the ratio between the observed divided by the expected number of occurences.

  rules = apriori(transactions, 
                  parameter=list(support = 0.02, confidence = 0.2))
  
  subrules = rules[quality(rules)$lift > 1.5]
  
  subrules
Visualize rules The 50 strongest rules after applied the threshold


      plot(subrules, measure=c("support","lift"), shading="confidence")
41 rules with 2 products
9 rules with 3 products
      plot(subrules, shading="order", control=list(main ="Two-key plot"))
List rules

options(width = 800)
  inspect(head(sort(subrules, 
                  by=c("confidence", "support", "lift")),20));
In the TOP 20 rules, all of them have either "iMac" or "HP Laptop" as the LHS product, the TOP 2 best-sellers. Although they are set as the consequence, in case they are were the Antecedent (RHS), the difference would be the "confidence" level, however it already gives important insights about the strenght of the rules concerning these items in the basket.

Association between 2 Products
The next step was to run the “apriori” function in R in order to identify the main basket composition rules associated with the Best Seller items as the RHS and the following item to be added in the basket.

The following metrics were adopted as threshold:

Parameters applied
rules threshold (support, confidence and lift)
Minimum quantity of items = 2
Maximum quantity of items = 2
Reccomended products for baskets that contain as first product
**Metrics Legend**
- LHS: Antecedent
- RHS: Consequent
- support: indicates how frequently the item / combination appear in the dataset
- confidence: indicates the number of times the IF/THEN statement on the data are true.
- lift: indicates the ratio between the observed divided by the expected number of occurences.
1- iMac

  # Create apriori rule
  # For the following items, the code will be hidden. However, to get its results is just to replace "lhs" syntax by the item analyzed.
    rules_iMac = apriori(transactions, 
                        parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                        appearance = list(default="rhs" , "lhs" = c("iMac")))
    
options(width = 800)
rules_iMac
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_iMac, 
                      by=c("confidence", "lift")),10))
  #Plot association rules    
  plot(rules_iMac, method="graph")#, 
       #control=list(type="items"))
  
2- HPLaptop

# Create apriori rule "HP Laptop"
rules_HPLaptop = apriori(transactions, 
                     parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                     appearance = list(default="rhs" , "lhs" = c("HP Laptop")))

options(width = 800)
rules_HPLaptop
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_HPLaptop, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_HPLaptop, method="graph")#, 
#control=list(type="items"))

3- CyberPower

# Create apriori rule "CYBERPOWER Gamer Desktop"
rules_CyberPower = apriori(transactions, 
                         parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                         appearance = list(default="rhs" , "lhs" = c("CYBERPOWER Gamer Desktop")))

options(width = 800)
rules_CyberPower
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_CyberPower, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_CyberPower, method="graph")#, 
#control=list(type="items"))

4- AppleEarpods

# Create apriori rule "Apple Earpods"
rules_AppleEarpods = apriori(transactions, 
                         parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                         appearance = list(default="rhs" , "lhs" = c("Apple Earpods")))

options(width = 800)
rules_AppleEarpods
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_AppleEarpods, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_AppleEarpods, method="graph")#, 
#control=list(type="items"))

5- MacBookAir

# Create apriori rule "Apple MacBook Air"
rules_MacBookAir = apriori(transactions, 
                         parameter=list(support = 0.02, confidence = 0.15, minlen = 2, maxlen = 2),
                         appearance = list(default="rhs" , "lhs" = c("Apple MacBook Air")))

options(width = 800)
rules_MacBookAir
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_MacBookAir, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_MacBookAir, method="graph")#, 
#control=list(type="items"))

**Confidence level exception** 
* confidence > 0.15

Otherwise no rule would be created.
6- Lenovo Desktop

# Create apriori rule "Lenovo Desktop Computer"
rules_Lenovo = apriori(transactions, 
                         parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                         appearance = list(default="rhs" , "lhs" = c("Lenovo Desktop Computer")))

options(width = 800)
rules_Lenovo
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_Lenovo, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_Lenovo, method="graph")#, 
#control=list(type="items"))

7- DellDesktop

# Create apriori rule "Dell Desktop"
rules_DellDesktop = apriori(transactions, 
                         parameter=list(support = 0.02, confidence = 0.2, minlen = 2, maxlen = 2),
                         appearance = list(default="rhs" , "lhs" = c("Dell Desktop")))

options(width = 800)
rules_DellDesktop
#review the rules and analyse it to filter the rules down to a manageable subset
inspect(head(sort(rules_DellDesktop, 
                  by=c("confidence", "lift")),10))
#Plot association rules    
plot(rules_DellDesktop, method="graph")#, 
#control=list(type="items"))

Association between 3 Products
The following metrics were adopted as threshold:

Parameters applied
support > 0.015
confidence > 0.2
Minimum quantity of items = 3
Maximum quantity of items = 3
Reccomended products for baskets that contain as first product
**Metrics Legend**
- LHS: Antecedent
- RHS: Consequent
- support: indicates how frequently the item / combination appear in the dataset
- confidence: indicates the number of times the IF/THEN statement on the data are true.
- lift: indicates the ratio between the observed divided by the expected number of occurences.
1- iMac & HP Laptop

# Create apriori rule "iMac & HP Laptop "
          rules_iMac_HPLaptop = apriori(transactions, 
                                 parameter=list(support = 0.015, confidence = 0.2, minlen=3),
                                 appearance = list("lhs" = c("iMac", "HP Laptop")))

options(width = 800)
          rules_iMac_HPLaptop

  #review the rules and analyse it to filter the rules down to a manageable subset
          inspect(head(sort(rules_iMac_HPLaptop, by=c("confidence", "lift")),5))
# Plot ####
          plot(rules_iMac_HPLaptop, measure=c("support","confidence"), shading="lift", engine = 'htmlwidget')
    
