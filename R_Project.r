install.packages("RCurl")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/JohnAndrieu/R_Project/master/athlete_events.csv")
y <- read.csv(text = x)

