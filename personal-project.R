product <- read.csv("products.csv")

#1
boxplot(product$product_price ~ product$department,
        main = "Product Price for all department",
        ylab = "Product Price",
        xlab = "Department",
        col = rainbow(length(unique(product$department))))

#2
top5 <- table(product$department)
x <- round(100 * top5 / sum(top5), 2)
y <- sort(x, decreasing = TRUE)
Z <- head(y, 5)
other <- sum(tail(y, length(y) - 5))
final <- c(Z, other)
names(final)[6] <- "other"
leb <- paste(names(final), "(", final, "%)")

pie(final, main ="apakek",
    labels = leb,
    col = rainbow(length(final)))

# 3
froze <- subset(product, product$department == "frozen")
aisle <- table(froze$aisle)
x <- sort(aisle, decreasing = FALSE)
Y <- head(x, 3)

barplot(Y,
        main = "hah top 3?", 
        col = rainbow(length(Y)))


###################################
market <- read.csv("market_data.csv")

market.sub <- subset(market, market$department == "beverages" & market$aisle != "soft drinks")

market.sub <- market.sub[!duplicated(market.sub),]

install.packages('arules')
library("arules")

transactions.list <- split(market.sub$product_name, market.sub$order_id)

transactions.list.final <- as(transactions.list, "transactions")

frequent.itemsets <- apriori(transactions.list.final,
                             parameter = 
                               list(supp = 0.02,
                                    target = "frequent itemsets"))
inspect(frequent.itemsets)

rules <- apriori(transactions.list.final,
                 parameter = 
                   list(supp = 0.02,
                        target = "rules",
                        conf = 0.05))
inspect(rules)
