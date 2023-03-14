library(data.table)
library(tidyverse)
library(ggplot2)

expense_2022 <- read.csv("transaction_2022.CSV")
View(expense_2022)

#================================
#=======Pre-processing===========
#================================
expense_2022 <- subset(expense_2022, select = -c(Post.Date, Memo))
View(expense_2022)

#=======Lower case the Description column values========#
expense_2022$Description <- tolower(expense_2022$Description)
View(expense_2022)

#==========Analysis===============#
#==========Category spending===========#

unique(expense_2022$Category)

#========Bar chart by category===========#
# The positive spending is the payment I made to the card #
# Let's get rid of the blank values
expense_2022 <- expense_2022[!is.na(expense_2022$Category) & expense_2022$Category != "", ]
View(expense_2022)

agg_spending <- aggregate(Amount ~ Category, expense_2022, sum)

ggplot(data = agg_spending, aes(x = Category, y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Category", y = "Total Spending", title = "Aggregated Spending by Category")

#=============Food expenses only==============#
food_spending <- aggregate(Amount ~ Category, data = expense_2022[expense_2022$Category == "Food & Drink", ], sum)

ggplot(food_spending, aes(x = Category, y = Amount)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Food Spending by Month")

View(expense_2022)
