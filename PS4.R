install.packages("rvest")
library(rvest)  
library(plyr)
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")
# Take only table 2
data <- html_table(temp [2], header=TRUE)
table <- data[[1]]
# Remove empty rows
table <- table[-c(1,2),]
rownames(table) <- NULL
# Take out unused columns and rename columns
colnames(table) [3] <- "Winner"
table [8] <- NULL
table[6] <- NULL
colnames(table)[7] <- "Runner Up"
# Function to pull out names of losers and winners 
split_names <- function(x) substring(x,(nchar(x)/2)+2 )
table$Winner<- sapply(table$Winner, split_names)
table$`Runner Up` <- sapply(table$`Runner Up`, split_names)
# Turns percent values from character to numeric
split_percent <- function(x) substring(x, 1, nchar(x)-1)
table$`Popular vote (%)` <- as.numeric(sapply(table$`Popular vote (%)`, split_percent))
table$Turnout <- as.numeric(sapply(table$Turnout, split_percent))
table$Turnout [c(16,46)] <- c(50,60)
# count total number of victories of each party, and their victories after 1800
num_victories <-count(table, 'Party')
new_table <- arrange(table, table$Year)
num_1900_victories <- count(new_table[20:48,], 'Party')
# Puts total victories next to 1900 victories
victories_total <- c(num_victories$freq[1],0,
                     num_victories$freq[2], num_1900_victories$freq[1],
                     num_victories$freq[3], num_1900_victories$freq[2],
                     num_victories$freq[4], num_1900_victories$freq[3]
                     )
X_names <- c("D.-R.", "Post-1900", "Democrats","Post-1900", "Republicans", "Post-1900", "Whigs", "Post-1900")

par(mfrow=c(2,1))
# Creates a boxplot of popular vote percents by party
# We can see that Republicans have done slightly better than democrats on average
# and whigs/DR have been worse off. Lincoln was a true outlier and Democrats achieved
# the highest percent vote ever.
par(mar=c(5,5,3,3))
boxplot(table$`Popular vote (%)` ~ table$Party, 
        ylab="Popular Vote %", 
        xlab="Party",
        border=c("purple4", "navy", "darkred", "yellow4"),
        col=c("purple", "blue", "red", "yellow"),
        main="Boxplot of Popular Vote by Party",
        pch="L"
)
mtext("L is Lincoln in 1860",side=4)

# Plot each party based on their total number of victories, and their victories after 1900
# We can see Republicans and Democrats have been equal in both centuries, but whigs and D-R
# were only successful in the 1800's
par(mar=c(7,5,3,1))
barplot(victories_total,
        col=c("purple","purple", "blue", "darkblue", "red","darkred", "yellow","yellow4"),
        names.arg = X_names,
        xlab="",
        ylab="Number of Victories",
        main="Success of Political Parties",
        las=2
)
mtext("Party", side=1)



