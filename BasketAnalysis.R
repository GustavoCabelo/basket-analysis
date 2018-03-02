#Library####
require("arules")
require("arulesViz")

#Loading file####

transactions <- read.transactions(paste0(getwd(),"/Files/ElectronidexTransactions2017.csv"), format = c("basket","single"), sep = ",", rm.duplicates = F)

# Data Visualization ####
length(transactions)
  # [1] 9835
summary(transactions)
    # most frequent items:
      #   iMac                HP Laptop CYBERPOWER Gamer Desktop            Apple Earpods 
      # 2519                     1909                     1809                     1715 
      # Apple MacBook Air                  (Other) 
      # 1530                    33622 
    
    # element (itemset/transaction) length distribution:
      #   sizes
      # 0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21 
      # 2 2163 1647 1294 1021  856  646  540  439  353  247  171  119   77   72   56   41   26   20   10   10   10 
      # 22   23   25   26   27   29   30 
      # 5    3    1    1    3    1    1 
    
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    # 0.000   2.000   3.000   4.383   6.000  30.000 

## Histogram
hist(size(transactions), breaks = 50, col = 'grey', xlab = "items x transactions", main = "Histogram")
## Box Plot
boxplot(size(transactions), outline = FALSE, horizontal=TRUE, main="BoxPlot", xlab="items x transactions")

#List items in a new DF
itemLabels <- as.data.frame(itemLabels(transactions))

####-------------------------------------------Pre-process-------------------------------------------####

#Check Best Seller items
{itemFrequencyPlot(transactions, type = c("absolute"), topN = 10, ylab = "item frequency")
itemFrequency <- sort(itemFrequency(transactions, 
                                    type = c("absolute")),
                      decreasing=T)
itemFrequency <- as.data.frame(itemFrequency)

itemFrequency <- subset(itemFrequency,
                        itemFrequency > 1000)
}

#Add brand and ProductType info
{ProductType <- c('Desktop','Laptop','Desktop', 'Active Headphones','Laptop','Desktop','Desktop','Laptop','Monitors','Acer Desktop')
  Brand <- c('Apple','HP','CYBERPOWER','Apple','Apple','Lenovo','Dell','Apple','View Sonic','Acer')
  
  Productinfo <- data.frame(ProductType,Brand)
  
  itemFrequency$ProductType <- Productinfo$ProductType
  itemFrequency$Brand <- Productinfo$Brand}


#Check Best Seller items sold alone
{ tbl1 <- transactions[which(size(transactions)==1),]
  tbl <- crossTable(tbl1, sort=T)
  x <- c('iMac','HP Laptop','CYBERPOWER Gamer Desktop','Apple Earpods','Apple MacBook Air','Lenovo Desktop Computer','Dell Desktop','Apple MacBook Pro','ViewSonic Monitor','Acer Desktop')
  x <- c('iMac','HP Laptop','CYBERPOWER Gamer Desktop','Apple Earpods','Apple MacBook Air','Lenovo Desktop Computer','Dell Desktop','Apple MacBook Pro','ViewSonic Monitor','Acer Desktop')
  tbl[x,x]
  }

#Check Best Seller items crossSell among them
{size(transactions)
  tbl2 <- transactions[which(size(transactions)>=1),]
  tbl2 <- crossTable(tbl2, sort=T)
  tbl2[1:10,1:10]
}
#--------------------------------------------------------------------------------------------#
# Modeling    ####
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------#
  ## Association between 2 products ####
#--------------------------------------------------------------------------------------------       
            # The next step was to run the “apriori” function in R in order to identify the main basket composition rules associated with the Best Seller items as the "RHS" and the following item to be added in the basket.
            
#--------------------------------------------------------------------------------------------#
    ### General rules  ####
#-------------------------------------------------------------------------------------------- 

            # Creating a general rule to understand the correlation strength between items in the transactional set.

      #### General Rule: strongest correlations in the basket ####

              # The following metrics were adopted as threshold:
              
              # Parameters applied
              # - support > 0.02 
              # - confidence > 0.2
              # - lift > 1.5
              # - minlen = 2

        # Create:
            rules = apriori(transactions, 
                            parameter=list(support = 0.02, confidence = 0.2, minlen = 2))
            
            rules
        
        # Subrules: trim down to only those with meaningful lift
            subrules = rules[quality(rules)$lift > 1.5]
            
            subrules
        
          # Inspect Subrules
        
            # Ordered according to the metrics' sequence given.
                inspect(head(sort(subrules, 
                              by=c("support", "confidence", "lift")),20)) 
          # Plot Subrules 
            
            # rules distribution regarding the main metrics   
                plot(subrules, measure=c("support","lift"), shading="confidence")
            
            # items amount per rule   
                plot(subrules, shading="order", control=list(main ="Two-key plot"))

                
      #### General rules2: looking for other correlations ####
                
        # Looking for other interesting correlations, the top 2 best-seller items were disregarded to avoid biases results as they appear in more than 50% of the first rules 
             
              # The following metrics were adopted as threshold:
                  
              # Parameters applied
              # - support > 0.015 
              # - confidence > 0.2
              # - lift > 1.5
              # - minlen = 2
              # - appearance: none "iMac" or "HP Laptop"
                
        # Create:      
            rules2 = apriori(transactions, 
                                     parameter=list(support = 0.015, confidence = 0.2, minlen = 2),
                                     appearance = list("none" = c("iMac", "HP Laptop")))
            rules2
            
        # Subrules2: trim down to only those with meaningful lift
            subrules2 = rules2[quality(rules2)$lift > 1.5]
            
            subrules2
            
          # Inspect Subrules2
            
            # Ordered according to the metrics' sequence given.
                inspect(head(sort(subrules2, 
                              by=c("support", "confidence", "lift")),20)) 
          # Plot Subrules2 
            
            # rules2 distribution regarding the main metrics   
                plot(subrules2, measure=c("support","lift"), shading="confidence")
            
            # items amount per rule   
                plot(subrules2, shading="order", control=list(main ="Two-key plot"))
                
#-------------------------------------------------------------------------------------------- 
    ### Reccomended products for baskets that contain as first product (LHS) ####
#-------------------------------------------------------------------------------------------- 
            # The following metrics were adopted as threshold:
            
            # Parameters applied
            #   - same threshold as general rules (support, confidence and lift)
            #   -	Minimum quantity of items = 2
            #   -	Maximum quantity of items = 2

            # Metrics Legend

            #   - LHS: Antecedent
            #   - RHS: Consequent
            #   - support: indicates how frequently the item / combination appear in the dataset
            #   - confidence: indicates the number of times the IF/THEN statement on the data are true.
            #   - lift: indicates the ratio between the observed divided by the expected number of occurences.
  
    # "LHS" Rules ####  
        # "iMac" #1 ####
          rules_iMac = apriori(transactions, 
                        parameter=list(support = 0.02, confidence = 0.2, maxlen = 3),
                        appearance = list(default="rhs" , "lhs" = c("iMac")))
          rules_iMac
    
        # "HP Laptop" #2 ####
          rules_HPLaptop = apriori(transactions, 
                        parameter=list(support = 0.02, confidence = 0.2, minlen=2, maxlen = 3),
                        appearance = list("lhs" = "HP Laptop"))
          rules_HPLaptop
    
        # "CYBERPOWER Gamer Desktop" #3 ####
          rules_CyberPower = apriori(transactions, 
                        parameter=list(support = 0.02, confidence = 0.2, minlen=2, maxlen = 3),
                        appearance = list("lhs" = "CYBERPOWER Gamer Desktop"))
          rules_CyberPower
    
        # "Apple Earpods" #4 ####
          rules_AppleEarpods = apriori(transactions, 
                        parameter=list(support = 0.01, confidence = 0.2, minlen=2, maxlen = 3),
                        appearance = list(default="rhs" , "lhs" = "Apple Earpods"))
          rules_AppleEarpods
    

        # "Apple MacBook air" #5 ####
          rules_MacBookAir = apriori(transactions, 
                        parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                        appearance = list(default="rhs" , "lhs" = "Apple MacBook Air"))
          rules_MacBookAir
    
        # "Lenovo Desktop Computer" #6 ####
          rules_Lenovo = apriori(transactions, 
                        parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                        appearance = list(default="rhs" , "lhs" = "Lenovo Desktop Computer"))
          rules_Lenovo
    
        # "Dell Desktop" #7 ####
          rules_DellDesktop = apriori(transactions, 
                        parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                        appearance = list(default="rhs" , "lhs" = "Dell Desktop"))
          rules_DellDesktop
    
    # Inspect ####
          inspect(head(sort(rules_iMac, by=c("confidence", "lift")),10))
          inspect(head(sort(rules_HPLaptop, by=c("confidence", "lift")),10))
          inspect(head(sort(rules_CyberPower, by=c("confidence", "lift")),10))
          inspect(head(sort(rules_AppleEarpods, by=c("confidence", "lift")),10))
          inspect(head(sort(rules_MacBookAir, by=c("confidence", "lift")),10))
          inspect(head(sort(rules_Lenovo, by=c("confidence", "lift")),3))
          inspect(head(sort(Rules_DellDesktop, by=c("confidence", "lift")),3))
          
    # Plot ####
          plot(rules_iMac, measure=c("support","confidence"), shading="lift", engine = 'htmlwidget')
          plot(rules_HPLaptop, method="graph", control=list(type="items"))
          plot(rules_CyberPower, method="graph", control=list(type="items"))
          plot(rules_AppleEarpods, method="graph", control=list(type="items"))
          plot(rules_MacBookAir, method="graph", control=list(type="items"))
          plot(rules_Lenovo, method="graph", control=list(type="items"))
          plot(Rules_DellDesktop, method="graph", control=list(type="items"))



#--------------------------------------------------------------------------------------------
  ## Association between 3 products ####
#--------------------------------------------------------------------------------------------          
          # Step up in the basket analysis for relationship of 3 products in the basket.

#--------------------------------------------------------------------------------------------#
    ### General rules ####
#-------------------------------------------------------------------------------------------- 

      #### General Rule: strongest correlations in the basket ####
          
              # The following metrics were adopted as threshold:
              
              # Parameters applied
              # - support > 0.015 
              # - confidence > 0.2
              # - lift > 1.3
              # - minlen = 3
          
        # Create:          
            rules_3items = apriori(transactions, 
                          parameter=list(support = 0.015, confidence = 0.2, minlen = 3))
            rules_3items
    
        # Subrules: trim down to only those with meaningful lift
            subrules_3items = rules_3items[quality(rules_3items)$lift > 1.3]
            
            subrules_3items
            
          # Inspect Subrules
            
            # Ordered according to the metrics' sequence given.
              inspect(head(sort(subrules_3items, 
                              by=c("support", "confidence", "lift")),20)) 

          # Plot Subrules
              plot(subrules_3items, measure=c("support","confidence"), shading="lift", engine = 'htmlwidget')

#-------------------------------------------------------------------------------------------- 
    ### Reccomended products for baskets that contain as first product (LHS) ####
#-------------------------------------------------------------------------------------------- 
              # The following metrics were adopted as threshold:
              
              # Parameters applied
              #   - same threshold as general rules (support, confidence and lift)
              #   -	Minimum quantity of items = 2
              #   -	Maximum quantity of items = 2
              
              # Metrics Legend
              
              #   - LHS: Antecedent
              #   - RHS: Consequent
              #   - support: indicates how frequently the item / combination appear in the dataset
              #   - confidence: indicates the number of times the IF/THEN statement on the data are true.
              #   - lift: indicates the ratio between the observed divided by the expected number of occurences.
              
    # "LHS" Rules ####
        # {iMac","HP Laptop"}####
          rules_iMac_HPLaptop = apriori(transactions, 
                                 parameter=list(support = 0.015, confidence = 0.2, minlen=3),
                                 appearance = list("lhs" = c("iMac", "HP Laptop")))
          rules_iMac_HPLaptop
  
    # Inspect ####
          inspect(head(sort(rules_iMac_HPLaptop, by=c("confidence", "lift")),5))
  
    # Plot ####
          plot(rules_iMac_HPLaptop, measure=c("support","confidence"), shading="lift", engine = 'htmlwidget')
    