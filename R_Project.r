###############################################
### Analyse Exploratoire Final Project #######
### Jonathan Andrieu & Laura Burlon--Roux ###
###               5 SDBD                 ###

################################
### Imports and Preparation ###
##############################

install.packages("RCurl")
library(RCurl)

install.packages("dplyr")
library(dplyr)    

install.packages("plyr")
library(plyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)

library(gridExtra)

url_content <- getURL("https://raw.githubusercontent.com/JohnAndrieu/R_Project/master/athlete_events.csv")
dataset_JO <- read.csv(text = url_content)

summer_recent_JO <- filter(filter(dataset_JO, Season == "Summer"), Year >= 1980)

########################
### Medal Frequence ###
######################

sport_freq <- count(summer_recent_JO$Sport)
medal_freq <- count(filter(summer_recent_JO, Medal != 'NA')$Sport) 
percent_medal <- (medal_freq$freq/sport_freq$freq)*100
df <- cbind(sport_freq, percent_medal)
top_medal_sport <- arrange(df, -df$percent_medal)[1:5,]

ggplot(top_medal_sport, aes(x = x, y = percent, fill = x, label = round(percent))) + 
  geom_histogram(stat = "identity") +
  xlab("Sports") +
  ylab("% de chance d'obtenir une médaille") +
  guides(fill=guide_legend("Sports")) +
  geom_text(size = 3, position = position_stack(vjust = 0.85)) +
  ggsave(filename = "/Users/jonathan/Desktop/R_Project/Plot/PercentVictoire.pdf")

#######################
###     IMC        ###
#####################

imc <- summer_recent_JO$Weight/((summer_recent_JO$Height/100)*(summer_recent_JO$Height/100))
imc_df <- cbind(summer_recent_JO, imc)     
imc_male <- filter(filter(imc_df, Sport %in% top_medal_sport$x), Sex == "M")
imc_female <- filter(filter(imc_df, Sport %in% top_medal_sport$x), Sex == "F")

mean_m <- ddply(imc_male, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_f <- ddply(imc_female, "Sport", summarise, mean_imc=mean(na.omit(imc)))

graph_imc_male <- ggplot(imc_male, aes(x=imc, color=Sport)) + 
  geom_density(size = 0.7) +
  geom_vline(data=mean_m, aes(xintercept=mean_imc, color=Sport),linetype="dashed") +
  xlab("IMC Masculin") +
  ylab("Densité") +
  guides(color=guide_legend("Sports")) +
  xlim(20, 30) 

graph_imc_female <- ggplot(imc_female, aes(x=imc, color=Sport)) + 
  geom_density(size = 0.7) +
  geom_vline(data=mean_f, aes(xintercept=mean_imc, color=Sport),linetype="dashed") +
  xlab("IMC Féminin") +
  ylab("Densité") +
  guides(color=guide_legend("Sports")) +
  xlim(20, 30)

g <- arrangeGrob(graph_imc_male, graph_imc_female, ncol=1, nrow=2)
ggsave(file="/Users/jonathan/Desktop/R_Project/Plot/IMC.pdf", g)

