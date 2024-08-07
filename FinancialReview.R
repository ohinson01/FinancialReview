# Financial Review 
# Date: 3/19/2024

getwd()
# Windows
#setwd("C:\\Users\\ohins\\OneDrive\\Documents\\Udemy\\R_Programming-Advanced_Analytics_in_R_for_Data_Science\\Section_2-Data_Preparation")
getwd()

# Import dataset
# Basic: fin <- read.csv("Udemy\\R_Programming-Advanced_Analytics_in_R_for_Data_Science\\Section_2-Data_Preparation\\P3-Future-500-The-Dataset.csv", stringsAsFactors = T)
# Replace empty values with NA
fin <- read.csv("Udemy\\R_Programming-Advanced_Analytics_in_R_for_Data_Science\\Section_2-Data_Preparation\\P3-Future-500-The-Dataset.csv", na.strings = c(""), stringsAsFactors = T)
fin 

# Explore dataset
head(fin, 20)
tail(fin)
tail(fin, 10)
str(fin)
summary(fin)

# Changing from non-factor to factor
fin$ID <- factor(fin$ID)
summary(fin)
str(fin)

fin$Inception <- factor(fin$Inception)
str(fin)
summary(fin)

# -------------------------------------------

# Factor Variable Trap (FVT) (Changing from a factor to a non-factor)
# Converting into Numerics for Characters
a <- c("12", "13", "14", "12", "12")
a
typeof(a)
# Convert character vector into a numeric vector
b <- as.numeric(a)
b
typeof(b)
# Converting into Numerics For Factors
z <- factor(c("12", "13", "14", "12", "12"))
z
# Factorization of integer
typeof(z) 
# Picked up actual factorization of variable
# rather than factors themselves
# This means that when you try to convert, 
# it instead converts the "Levels"
# Levels: 12 13 14 ---> 1 2 3
y <- as.numeric(z)
y
typeof(y)
# --------- Correct way
# Convert Factor to Character, then to a numeric
x <- as.numeric(as.character(z))
x
typeof(x)

# -------------------------------------------

# FVT Example
str(fin)
#fin$Profit <- factor(fin$Profit) Dangerous

head(fin)
str(fin)
summary(fin)

# fin$Profit <- as.numeric(fin$Profit) Dangerous

# -------------------------------------------

# sub() and gsub()
fin$Expenses <- gsub(" Dollars", "", fin$Expenses)
fin$Expenses <- gsub(",", "", fin$Expenses)
head(fin)
str(fin)

fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
head(fin)
str(fin)

fin$Growth <- gsub("%", "", fin$Growth)
head(fin)
str(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)
str(fin)
summary(fin)

# -------------------------------------------

# What is an NA? (missing value)

#?NA

#TRUE   # 1
#FALSE  # 0
#NA    

#TRUE == FALSE
#TRUE == 5
#TRUE == 1 # TRUE
#FALSE == 4
#FALSE == FALSE
#FALSE == 0 # TRUE
# NULL in SQL
# Cannot compare NA to anything else 
#NA == TRUE
#NA == FALSE
#NA == 15
#15 == NA
#NA == NA

# -------------------------------------------

# Locating Missing Data
# Updated import to: fin <- read.csv("Udemy\\R_Programming-Advanced_Analytics_in_R_for_Data_Science\\Section_2-Data_Preparation\\P3-Future-500-The-Dataset.csv", na.strings = c(""), stringsAsFactors = T)

head(fin, 24)
# Is rows complete? 
# Check if NA in row
# Check all rows that do have NA (turn TRUE to FALSE)
fin[!complete.cases(fin),]
str(fin)

# -------------------------------------------

# Filtering: using which() for non-missing data
head(fin)
# Also shows NA values 
# Protect us from making incorrect analysis
fin[fin$Revenue == 9746272,]

# Look through vector and only picks out TRUE values
which(fin$Revenue == 9746272)
?which
# Correspond to row number
fin[which(fin$Revenue == 9746272),]

head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employees == 45),]

head(fin)
fin[fin$State == "WI",]
fin[which(fin$State == "WI"),]

# -------------------------------------------

# Filtering: using is.na() for missing data
head(fin)

fin$Expenses == NA
fin[fin$Expenses == NA,]

is.na()
a <- c(1, 24, 543, NA, 76, 45, NA)
is.na(a)

# Pick out rows with NA
is.na(fin$Expenses) 
fin[is.na(fin$Expenses),]

fin[is.na(fin$State),]

# -------------------------------------------

# Removing records with missing data
# Columns to remove
#     Industry - Need column for analysis (cannot research values or keep empty)
#     Inception - Not rely heavily on information (Keep value empty)
# Always make backup of current dataset before cleaning dataset
fin_backup <- fin
#fin <- fin_backup
# Rows with NA
fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]
# Rows without NA
fin[!is.na(fin$Industry),] # Opposite
fin <- fin[!is.na(fin$Industry),]
fin

fin[!complete.cases(fin),]

# -------------------------------------------

# Resetting the dataframe index
fin
rownames(fin) <- 1:nrow(fin)
fin
# Faster way (and better!)
fin
rownames(fin) <- NULL
fin

# -------------------------------------------

# Replacing Missing Data: Factual Analysis 
# Restore data with 100% certainty
# Columns
#    State - know city, restore state
#    Expenses - assuming no fraud, no errors (revenue - profit)
fin[!complete.cases(fin),]
# Find rows with NA
fin[is.na(fin$State),]
# Correct NA values
fin[is.na(fin$State) & fin$City == "New York",]
fin[is.na(fin$State) & fin$City == "New York","State"] <- "NY"
# Check
fin[c(11,377),]

fin[is.na(fin$State) & fin$City == "San Francisco",]
fin[is.na(fin$State) & fin$City == "San Francisco","State"] <- "CA"
# Check
fin[c(82,265),]

fin[!complete.cases(fin),]

# -------------------------------------------

# Replacing Missing Data: Median Imputation Method (Part 1)
# Columns
#    Employees - Take companies in retail sector 
fin[!complete.cases(fin),]
# Remove NAs as this will cause inaccurate analysis
# Good practice: 
#    Save statistics in different variables/vectors before overriding data
med_empl_retail <- median(fin[fin$Industry == "Retail","Employees"], na.rm = TRUE)
med_empl_retail

fin[is.na(fin$Employees) & fin$Industry == "Retail",]
fin[is.na(fin$Employees) & fin$Industry == "Retail", "Employees"] <- med_empl_retail
# Check
fin[3,]

fin[is.na(fin$Employees),]
median(fin[,"Employees"], na.rm = TRUE)
#mean(fin[,"Employees"], na.rm = TRUE)
med_empl_fin_services <- median(fin[fin$Industry == "Financial Services","Employees"], na.rm = TRUE)
med_empl_fin_services

fin[is.na(fin$Employees) & fin$Industry == "Financial Services",]
fin[is.na(fin$Employees) & fin$Industry == "Financial Services","Employees"] <- med_empl_fin_services
# Check
fin[330,]

fin[!complete.cases(fin),]

# -------------------------------------------

# Replacing Missing Data: Median Imputation Method (Part 2)
# Column(s)
#    Growth
fin[!complete.cases(fin),]

med_growth_constr <- median(fin[fin$Industry == "Construction","Growth"], na.rm = TRUE)
# Compare: median(fin[,"Growth"], na.rm = TRUE)
med_growth_constr

fin[is.na(fin$Growth) & fin$Industry == "Construction",]
fin[is.na(fin$Growth) & fin$Industry == "Construction","Growth"] <- med_growth_constr
# Check
fin[8,]

fin[!complete.cases(fin),]

# -------------------------------------------

# Replacing Missing Data: Median Imputation Method (Part 3)
# Column(s)
#    Revenue
#    Expenses

# Mini-Exercise
# Revenue
fin[!complete.cases(fin),]

fin_backup_2 <- fin

median(fin[,"Revenue"], na.rm = TRUE)
#mean(fin[,"Revenue"], na.rm = TRUE)
med_rev_constr <- median(fin[fin$Industry == "Construction","Revenue"], na.rm = TRUE)
med_rev_constr

fin[is.na(fin$Revenue) & fin$Industry == "Construction",]
fin[is.na(fin$Revenue) & fin$Industry == "Construction","Revenue"] <- med_rev_constr
# Check
fin[c(8,42),]

fin[!complete.cases(fin),]

# Expenses
# Be careful here. Only for certain ones
# We don't want to replace that one that's by itself (because then that row won't add up)
med_exp_constr <- median(fin[fin$Industry == "Construction", "Expenses"], na.rm = TRUE)
med_exp_constr
fin[is.na(fin$Expenses) & fin$Industry == "Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry == "Construction" & is.na(fin$Profit),"Expenses"] <- med_exp_constr

# Check
fin[c(8, 42),]

fin[!complete.cases(fin),]

# -------------------------------------------

# Replacing Missing Data: Deriving Values
# Revenue - Expenses = Profit
# Expenses = Revenue - Profit
fin[!complete.cases(fin),]

fin[is.na(fin$Profit),"Profit"] <- fin[is.na(fin$Profit),"Revenue"] - fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]

fin[!complete.cases(fin),]
fin[is.na(fin$Expenses), "Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]
fin[15,]

fin[!complete.cases(fin),]

# -------------------------------------------

# Visualization:
# install.packages("ggplot2)
library(ggplot2)
# A scatterplot classified by industry showing revenue, expenses, profit
p <- ggplot(data=fin)
p 
p + geom_point(aes(x=Revenue, y=Expenses,
                   color=Industry, size=Profit))
# A scatterplot that includes industry trends for the expenses~revenue relationship
d <- ggplot(data=fin, aes(x=Revenue, y=Expenses, 
                          color=Industry))
d + geom_point() + geom_smooth(fill=NA, linewidth=1.2)
# BoxPlots showing growth by industry
f <- ggplot(data=fin, aes(x=Industry, y=Growth, 
                          color=Industry))
f + geom_boxplot(size=1)

# Extra
f + geom_jitter() + 
  geom_boxplot(size=1, alpha=0.5, outlier.color=NA)


