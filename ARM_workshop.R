shop_data <-read.csv("e-shop clothing 2008.csv",header=T,
                        colClasses="factor")


names(shop_data)
head(shop_data)
tail(shop_data)
summary(shop_data)
str(shop_data) 
dim(shop_data)


library(tidyverse) # data manipulation
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics


# Support and confidence values
supportLevels <- c(0.1, 0.05, 0.06, 0.07, 0.08)
confidenceLevels <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Empty integers 
rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup6 <- integer(length=9)
rules_sup7 <- integer(length=9)
rules_sup8 <- integer(length=9)

# Apriori algorithm with a support level of 10%
for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(shop_data, parameter=list(sup=supportLevels[1], 
                                                         conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 5%
for (i in 1:length(confidenceLevels)){
  
  rules_sup5[i] <- length(apriori(shop_data, parameter=list(sup=supportLevels[2], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 6%
for (i in 1:length(confidenceLevels)){
  
  rules_sup6[i] <- length(apriori(shop_data, parameter=list(sup=supportLevels[3], 
                                                        conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 7%
for (i in 1:length(confidenceLevels)){
  
  rules_sup7[i] <- length(apriori(shop_data, parameter=list(sup=supportLevels[4], 
                                                          conf=confidenceLevels[i], target="rules")))
  
}

# Apriori algorithm with a support level of 8%
for (i in 1:length(confidenceLevels)){
  
  rules_sup8[i] <- length(apriori(shop_data, parameter=list(sup=supportLevels[5], 
                                                              conf=confidenceLevels[i], target="rules")))
  
}

# Number of rules found with a support level of 10%
plot1 <- qplot(confidenceLevels, rules_sup10, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 10%") +
  theme_bw()

# Number of rules found with a support level of 5%
plot2 <- qplot(confidenceLevels, rules_sup5, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 5%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 6%
plot3 <- qplot(confidenceLevels, rules_sup6, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 6%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 7%
plot4 <- qplot(confidenceLevels, rules_sup7, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 7%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Number of rules found with a support level of 8%
plot5 <- qplot(confidenceLevels, rules_sup8, geom=c("point", "line"), 
               xlab="Confidence level", ylab="Number of rules found", 
               main="Apriori with a support level of 8%") + 
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  theme_bw()

# Subplot
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol=2)

#Join the four lines to improve the visualization.


# Data frame
num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup6, rules_sup7, rules_sup8, confidenceLevels)

# Number of rules found with a support level of 10%, 5%, 6%, 7% and 8%
ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  # Plot line and points (support level of 10%)
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  # Plot line and points (support level of 5%)
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  # Plot line and points (support level of 6%)
  geom_line(aes(y=rules_sup6, colour="Support level of 6%")) + 
  geom_point(aes(y=rules_sup6, colour="Support level of 6%")) +
  
  # Plot line and points (support level of 7%)
  geom_line(aes(y=rules_sup7, colour="Support level of 7%")) +
  geom_point(aes(y=rules_sup7, colour="Support level of 7%")) +
  
  # Plot line and points (support level of 8%)
  geom_line(aes(y=rules_sup8, colour="Support level of 8%")) +
  geom_point(aes(y=rules_sup8, colour="Support level of 8%")) +
  
  # Labs and theme
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

rules <- apriori(shop_data,
                 parameter=list(sup=supportLevels[5],
                 conf=confidenceLevels[2],
                 target="rules"),
                 appearance= list(rhs=c("Product.category=blouses", "Product.category=trousers",
                                                    "Product.category=skirts"),default="lhs"))

inspect(rules)
summary(rules)

plot(rules, measure=c("support", "lift"), shading="confidence")

plot(rules, method="graph")

plot(rules, method="graph",control=list(layout=igraph::in_circle()))

plot(rules, method="grouped")

