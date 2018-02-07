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

#List items####
itemLabels <- as.data.frame(itemLabels(transactions))

#Pre-process####

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

#Modeling####
#Create Rules regarding transaction in the basket
{#General Rules: the strongest basket correlations
  rules = apriori(transactions, 
                   parameter=list(support = 0.02, confidence = 0.2))
                   #appearance = list(default="rhs" , "lhs" = "iMac"))
  rules
  
  #trim down the rules to the ones that are more important (****DON't APPLY****)
    subrules = rules[quality(rules)$lift > 1.5]
    
    subrules
    
    plot(subrules, measure=c("support","lift"), shading="confidence");

  #review the rules and analyse it to filter the rules down to a manageable subset
  inspect(head(sort(subrules, 
                    by=c("confidence", "support", "lift")),30));
  
  plot(subrules, shading="order", control=list(main ="Two-key plot"));
  
  #Show the reccomended products for baskets that contain "iMac"
  {rules_iMac = apriori(transactions, 
                        parameter=list(support = 0.02, confidence = 0.2, maxlen = 3),
                        appearance = list(default="rhs" , "lhs" = c("iMac")))
    rules_iMac
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_iMac, 
                      by=c("confidence", "lift")),10))
    plot(rules_iMac, method="graph", control=list(type="items"))
  }
  #Show the reccomended products for baskets that contain "HP Laptop" #2
  {
    rules_HPLaptop = apriori(transactions, 
                             parameter=list(support = 0.02, confidence = 0.2, minlen=2, maxlen = 3),
                             appearance = list("lhs" = "HP Laptop"))
    rules_HPLaptop
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_HPLaptop, 
                      by=c("confidence", "lift")),10))
    plot(rules_HPLaptop, method="graph", control=list(type="items"))    
  }
  #Show the reccomended products for baskets that contain "CYBERPOWER Gamer Desktop" #3
  {
    rules_CyberPower = apriori(transactions, 
                               parameter=list(support = 0.02, confidence = 0.2, minlen=2, maxlen = 3),
                               appearance = list("lhs" = "CYBERPOWER Gamer Desktop"))
    rules_CyberPower
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_CyberPower, 
                      by=c("confidence", "lift")),10))
    plot(rules_CyberPower, method="graph", control=list(type="items"))
  }
  #Show the reccomended products for baskets that contain "Apple Earpods" #4
  {rules_AppleEarpods = apriori(transactions, 
                                parameter=list(support = 0.01, confidence = 0.2, minlen=2, maxlen = 3),
                                appearance = list(default="rhs" , "lhs" = "Apple Earpods"))
    rules_AppleEarpods
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_AppleEarpods, 
                      by=c("confidence", "lift")),10))
    plot(rules_AppleEarpods, method="graph", control=list(type="items"))
  }
  #Show the reccomended products for baskets that contain "Apple MacBook air" #5
  {rules_MacBookAir = apriori(transactions, 
                              parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                              appearance = list(default="rhs" , "lhs" = "Apple MacBook Air"))
    rules_MacBookAir
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_MacBookAir, 
                      by=c("confidence", "lift")),10))
    plot(rules_MacBookAir, method="graph", control=list(type="items"))
  }
  #Show the reccomended products for baskets that contain "Lenovo Desktop Computer" #6
  {rules_Lenovo = apriori(transactions, 
                          parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                          appearance = list(default="rhs" , "lhs" = "Lenovo Desktop Computer"))
    rules_Lenovo
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_Lenovo, 
                      by=c("confidence", "lift")),3))
    plot(rules_Lenovo, method="graph", control=list(type="items"))
  }
  #Show the reccomended products for baskets that contain "Dell Desktop" #7
  {Rules_DellDesktop = apriori(transactions, 
                               parameter=list(support = 0.01, confidence = 0.15, minlen=2, maxlen = 2),
                               appearance = list(default="rhs" , "lhs" = "Dell Desktop"))
    Rules_DellDesktop
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(Rules_DellDesktop, 
                      by=c("confidence", "lift")),3))
    plot(Rules_DellDesktop, method="graph", control=list(type="items"))
  }
  }

{#Most Powerful itemset relations
  #1Show the reccomended products for baskets that contain itemset = iMac","HP Laptop"
  {Rules_iMac_HPLaptop = apriori(transactions, 
                                       parameter=list(support = 0.01, confidence = 0.15, minlen=3, maxlen = 3),
                                       appearance = list(default="rhs" , lhs = -c("iMac","HP Laptop")))
  Rules_iMac_HPLaptop
  
  #review the rules and analyse it to filter the rules down to a manageable subset
  inspect(head(sort(Rules_iMac_HPLaptop, 
                    by=c("confidence", "lift")),5))
  plot(Rules_iMacr_HPLaptop, method="graph", control=list(type="items"))
  }

  #2Show the reccomended products for baskets that contain 2 itemset in lhs, support > 2% and confidence > 20% 
  {rules_threeitems = apriori(transactions, 
                   parameter=list(support = 0.02, confidence = 0.2, minlen = 3));
    rules_threeitems
    
    #review the rules and analyse it to filter the rules down to a manageable subset
    inspect(head(sort(rules_threeitems, 
                      by=c("confidence", "lift")),19));
  }
  }


