# Module 4 Lab 2: Association Rules
# from "Data Science and Big Data Analytics" 

###################################################
# Step 2: Set the Working Directory and install the packages
###################################################
setwd("~/LAB05")
install.packages('arules') 
install.packages('arulesViz')
library('arules')
library('arulesViz')

###################################################
#Step 3: Read in the Data for Modeling
###################################################
txn <- read.transactions("MBAdata.csv",rm.duplicates = FALSE,format="single",sep=",",cols=c(1,2))


###################################################
#Step 4:Review Transaction Data
###################################################
txn@transactionInfo 
txn@itemInfo

###################################################
#Step 5:Plot Transactions
###################################################
image(txn) #What does image do?

###################################################
#Step 6: Mine the Association Rules
###################################################
help(apriori) #Take a look at the apriori function documentation

basket_rules <- apriori(txn,parameter=list(sup=0.5,conf=0.9,target="rules"))
inspect(basket_rules)

###################################################
#Step 7: Read in Groceries Dataset
###################################################
data(Groceries)
Groceries
Groceries@itemInfo

###################################################
#Step 8: Mine the Rules for the Groceries Dataset
###################################################
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))

###################################################
#Step 9: Extract Rules with Confidence > 0.8
###################################################
rules
subrules <- rules[quality(rules)$confidence > 0.8]
inspect(subrules)

# Visualize rules as a scatter plot (with jitter to reduce occlusion)
plot (subrules, control=list(jitter=2))

#Extract the top three rules with high lift 
rules_high_lift <- head(sort(rules, by="lift"), 3)
inspect(rules_high_lift)
plot(rules_high_lift, method="graph", control=list(type="items"))