setwd(".../Tugas 3 - Asosiasi")
library(readr)
data <- read_delim("Tugas Klp #1 Data.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
View(data)
sapply(data, class)

#====Praproses====
data_mod <- data
data_mod$age <- cut(data_mod$age,
                    breaks = c(-Inf, 
                               quantile(data_mod$age, 0.33), 
                               quantile(data_mod$age, 0.67), 
                               Inf),
                    labels = c("Young", "Mature", "Senior"))

data_mod$marital <- ifelse(data_mod$marital == "married" ,1,0)

data_mod$balance <- cut(data_mod$balance,
                        breaks = c(-Inf, 
                                   quantile(data_mod$balance, 0.33), 
                                   quantile(data_mod$balance, 0.67), 
                                   Inf),
                        labels = c("low", "med", "high"))

data_mod$day <- cut(data_mod$day,
                    breaks = c(-Inf, 7, 14, 21, Inf),
                    labels = c("Week1", "Week2", "Week3", "Week4"))

data_mod$duration <- cut(data_mod$duration,
                         breaks = c(-Inf, 
                                    quantile(data_mod$duration, 0.33), 
                                    quantile(data_mod$duration, 0.67), 
                                    Inf),
                         labels = c("short", "medium", "long"))

data_mod$campaign <- cut(data_mod$campaign,
                         breaks = c(-Inf, 
                                    quantile(data_mod$campaign, 0.33), 
                                    quantile(data_mod$campaign, 0.67), 
                                    Inf),
                         labels = c("low", "med", "high"))

data_mod$pdays <- ifelse(data_mod$pdays == "-1" ,0,1)

data_mod$previous <- ifelse(data_mod$previous == "0" ,0,1)

#----Casting ke Factor----
data_mod$job <- as.factor(data_mod$job)
data_mod$marital <- as.factor(data_mod$marital)
data_mod$education <- as.factor(data_mod$education)
data_mod$default <- as.factor(data_mod$default)
data_mod$housing <- as.factor(data_mod$housing)
data_mod$loan <- as.factor(data_mod$loan)
data_mod$contact <- as.factor(data_mod$contact)
data_mod$month <- as.factor(data_mod$month)
data_mod$y <- as.factor(data_mod$y)
data_mod$pdays <- as.factor(data_mod$pdays)
data_mod$previous <- as.factor(data_mod$previous)
data_mod$poutcome <- as.factor(data_mod$poutcome)

for (i in 1:ncol(data_mod)) {
  cat("Unique Value $", colnames(data_mod[i]),
      " = ", nrow(unique(data_mod[i])), "\n")
}

#====Frequent Itemset====
#----FI_Apriori----
library(arules)
datatr <- as(data_mod, "transactions")
datatr2 <- as(data, "transactions")

#cari tau frekuensi tertinggi
itemFrequencyPlot(datatr, topN=10, type="absolute", main="Item Frequency")

####minsup 0,004 minlen 4####
itemset <- apriori(data_mod, parameter = list(support=0.008, minlen=4, target="frequent"))
itemset
inspect(itemset)
#Sort
sort.itemset <- sort(itemset, by="support")
inspect(sort.itemset[1:10])
#Visualisasi
library(arulesViz)
plot(sort.itemset[1:10], method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))
plot(sort.itemset)

####minsup 0,3 minlen 4####
itemset2 <- apriori(data_mod, parameter = list(support=0.3, minlen=4, target="frequent"))
itemset2
inspect(itemset2)
#Sort
sort.itemset2 <- sort(itemset2, by="support")
inspect(sort.itemset2[1:10])
#Visualisasi
library(arulesViz)
plot(sort.itemset2[1:10], method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))
plot(sort.itemset2)

####minsup 0,4 minlen 4####
itemset3 <- apriori(data_mod, parameter = list(support=0.4, minlen=4, target="frequent"))
itemset3
inspect(itemset3)
#Sort
sort.itemset3 <- sort(itemset3, by="support")
inspect(sort.itemset2[1:10])
#Visualisasi
library(arulesViz)
plot(sort.itemset2[1:10], method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))
plot(sort.itemset3)


#----FI_FP Growth----
####minsup 0,004 maxlen 4####
library(rJava)
library(rCBA)

itemset_fp1 <- rCBA::fpgrowth(data_mod, frequent, support=0.3, maxLength=4, 
                              parallel=FALSE)
itemset_fp1
inspect(itemset_fp1)
# Sort
sort.itemset_fp1 <- sort(itemset_fp1, by="support")
inspect(sort.itemset_fp1 [1:10])

####minsup 0,3 maxlen 4####
itemset_fp2 <- rCBA::fpgrowth(data_mod, support=0.3, maxLength=4,
                              consequent="y", parallel=FALSE)
itemset_fp2
inspect(itemset_fp2)
# Sort
sort.itemset_fp2 <- sort(itemset_fp2, by="support")
inspect(sort.itemset_fp2 [1:10])

####minsup 0,4 maxlen 4####
itemset_fp3 <- rCBA::fpgrowth(data_mod, support=0.4, maxLength=4,
                              consequent="y", parallel=FALSE)
itemset_fp3
inspect(itemset_fp1)
# Sort
sort.itemset_fp3 <- sort(itemset_fp3, by="support")
inspect(sort.itemset_fp3 [1:10])

#====Rules Generation====
#----RG_Apriori----
rules_ap <- apriori(data_mod)
rules_ap
inspect(rules_ap)

####minsup 0,2|minlen 4|conf 0,8####
rules_ap <- apriori(data_mod, control=list(verbose=F),
                     parameter=list(minlen=4, supp=0.2, conf=0.8),
                     appearance=list(rhs=c("y=no","y=yes"),
                                     default="lhs"))
rules_ap
#sort
rules_ap.sorted <- sort(rules_ap, by="confidence")
inspect(rules_ap.sorted[1:10])

#visualisasi
library(arulesViz)
dev.new()
plot(rules_ap.sorted [1:10], method="graph", control=list(nodeCol="red", edgeCol="blue",alpha=1))
dev.new()
plot(rules_ap.sorted)
dev.new()
plot(rules_ap.sorted, method="grouped",control = list(col=2))

####minsup 0,2|minlen 4|conf 0,9####
rules_ap2 <- apriori(data_mod, control=list(verbose=F),
                        parameter=list(minlen=4, supp=0.2, conf=0.9),
                        appearance=list(rhs=c("y=no","y=yes"),
                                        default="lhs"))
rules_ap2
#sort
rules_ap2.sorted <- sort(rules_ap2, by="confidence")
inspect(rules_ap2.sorted[1:10])

#visualisasi
library(arulesViz)
dev.new()
plot(rules_ap2.sorted[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue",alpha=1))
dev.new()
plot(rules_ap2.sorted)
dev.new()
plot(rules_ap2.sorted, method="grouped",control = list(col=2))

####minsup 0,3|minlen 4|conf 0,8####
rules_ap3 <- apriori(data_mod, control=list(verbose=F),
                     parameter=list(minlen=4, supp=0.3, conf=0.8),
                     appearance=list(rhs=c("y=no","y=yes"),
                                     default="lhs"))
rules_ap3
#sort
rules_ap3.sorted <- sort(rules_ap3, by="confidence")
inspect(rules_ap3.sorted[1:10])

#visualisasi
library(arulesViz)
dev.new()
plot(rules_ap3.sorted[1:10], method="graph", control=list(nodeCol="red", edgeCol="blue",alpha=1))
dev.new()
plot(rules_ap3.sorted)
dev.new()
plot(rules_ap3.sorted, method="grouped",control = list(col=2))

####minsup 0,3|minlen 4|conf 0,9####
rules_ap4 <- apriori(data_mod, control=list(verbose=F),
                     parameter=list(minlen=4, supp=0.3, conf=0.9),
                     appearance=list(rhs=c("y=no","y=yes"),
                                     default="lhs"))
rules_ap4
#sort
rules_ap4.sorted <- sort(rules_ap4, by="confidence")
inspect(rules_ap4.sorted[1:10])

#visualisasi
library(arulesViz)
dev.new()
plot(rules_ap4.sorted [1:10], method="graph", control=list(nodeCol="red", edgeCol="blue",alpha=1))
dev.new()
plot(rules_ap4.sorted)
dev.new()
plot(rules_ap4.sorted, method="grouped",control = list(col=2))

#----RG_FP Growth----
library(rJava)
library(rCBA)

rules_fp <- rCBA::fpgrowth(data_mod, support=0.04, #minsup
                           confidence=0.8, maxLength=4,
                           consequent="y", parallel=FALSE)
rules_fp
inspect(rules_fp)

#sort
rules_fp.sorted <- sort(rules_fp, by="confidence")
inspect(rules_fp.sorted)

#visualisasi
dev.new()
plot(rules_fp.sorted, method="graph", 
     control=list(nodeCol="red", edgeCol="blue",
                  alpha=1))
dev.new()
plot(rules_fp.sorted)

dev.new()
plot(rules_fp.sorted, method="grouped",
     control = list(col=2))

#====Interestingness==== (RAW)
#mengubah rule yang ada menjadi dataframe
rules_ap2.df = data.frame(lhs = labels(lhs(rules_ap2)),
                          rhs = labels(rhs(rules_ap2)), 
                          rules_ap2@quality)
head(rules_ap2.df)

#mencari nilai interestingnes measure dari rule
interest_rules_ap2 <- interestMeasure(rules_ap2, c("support","confidence","lift","gini","jmeasure", "count")) 
head(interest_rules_ap2)

#menggabungkan dataset rule dengan interestingnes measure 
interest_rules_ap2 <- cbind(rules_ap2.df[1:2], interest_rules_ap2)
interest_rules_ap2
