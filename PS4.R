install.packages("rvest")
library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")
 
data <- html_table(temp [2], header=TRUE)
table <- data[[1]]
# Remove empty rows
table <- table[-c(1,2),]
rownames(table) <- NULL
str(table)
colnames(table) [3] <- "Winner"
table [8] <- NULL
table[6] <- NULL
colnames(table)[7] <- "Runner Up"
# Function to pull out names of losers and winners 
split_names <- function(x) substring(x,(nchar(x)/2)+2 )
# Apply functions to the data frame
table$Winner<- sapply(table$Winner, split_names)
table$`Runner Up` <- sapply(table$`Runner Up`, split_names)

