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

install.packages("dummies")
library(dummies)

install.packages("webshot")
library(webshot)

install.packages("highcharter")
library(highcharter) 

install.packages("reshape2")
library(reshape2)

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

imc_all_male <- filter(imc_df, Sex == "M")
imc_all_female <- filter(imc_df, Sex == "F")

mean_all_m <- ddply(imc_all_male, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_all_f <- ddply(imc_all_female, "Sport", summarise, mean_imc=mean(na.omit(imc)))

mean_m <- ddply(imc_male, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_f <- ddply(imc_female, "Sport", summarise, mean_imc=mean(na.omit(imc)))

graph_imc_male <- ggplot(imc_male, aes(x=imc, color=Sport)) + 
  geom_density(size = 0.7) +
  geom_vline(data=mean_m, aes(xintercept=mean_imc, color=Sport),linetype="dashed") +
  xlab("IMC Masculin") +
  ylab("Densité") +
  guides(color=guide_legend("Sports")) +
  xlim(18.5, 30) 

graph_imc_female <- ggplot(imc_female, aes(x=imc, color=Sport)) + 
  geom_density(size = 0.7) +
  geom_vline(data=mean_f, aes(xintercept=mean_imc, color=Sport),linetype="dashed") +
  xlab("IMC Féminin") +
  ylab("Densité") +
  guides(color=guide_legend("Sports")) +
  xlim(18.5, 30)

g <- arrangeGrob(graph_imc_male, graph_imc_female, ncol=1, nrow=2)
ggsave(file="/Users/jonathan/Desktop/R_Project/Plot/IMC.pdf", g)

#######################################
### Age for the top 5 medal sports ###
#####################################

topSport_M <- filter(filter(summer_recent_JO, Sport %in% top_medal_sport$x), Sex == "M")
groupby <- topSport_M %>%
  group_by(Sport) %>%
  select(Age)

meds <- ddply(na.omit(groupby), .(Sport), summarise, med = median(Age))

bp_male <- ggplot(groupby, aes(x=Age, y=Sport, color=Sport)) + 
  geom_boxplot() +
  xlab("Age Homme") +
  geom_text(data = meds, aes(x = med, y = Sport, label = med), size = 2, vjust = -0.5, hjust = -0.6)

topSport_F <- filter(filter(summer_recent_JO, Sport %in% top_medal_sport$x), Sex == "F")
groupby <- topSport_F %>%
  group_by(Sport) %>%
  select(Age)

meds <- ddply(na.omit(groupby), .(Sport), summarise, med = median(Age))

bp_female <- ggplot(groupby, aes(x=Age, y=Sport, color=Sport)) + 
  geom_boxplot() +
  xlab("Age Femme") +
  geom_text(data = meds, aes(x = med, y = Sport, label = med), size = 2, vjust = -0.5, hjust = -0.6)

g <- arrangeGrob(bp_male, bp_female, ncol=1, nrow=2)
ggsave(file="/Users/jonathan/Desktop/R_Project/Plot/Age.pdf", g)


##################################
### Which country won the most ###
#################################

dummy <- data.frame(dummy(summer_recent_JO$Medal))
keeps <- c("Medal.Bronze","Medal.Silver", "Medal.Gold")
dummies <- dummy[keeps]
sum_dummy <- data.frame(rowSums(dummies))

country_dummy <- data.frame(cbind(summer_recent_JO$Team,sum_dummy))
res <- ddply(country_dummy, "summer_recent_JO.Team", summarise, sum_medal=sum(rowSums.dummies.))

keeps <- c("summer_recent_JO.Team","sum_medal")
country_medals = res[keeps]

names(country_medals)[1] <- "country"
names(country_medals)[2] <- "medals"

#TODO: renommer usa

couleurs <- colorRampPalette(c('white', 'red'))

data(worldgeojson, package = "highcharter")

hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, country_medals, value = "medals", joinBy = c('name','country'),
    name = "Nombre de médaillés"
  )  %>% 
  hc_colorAxis(minColor="white", maxColor="#b12134") %>% 
  hc_title(text = "Nombre de médaillés", style=list(color="#b12134", fontSize = "50px")) %>% 
  hc_subtitle(text = "en baseball, handball, hockey, softball & water polo depuis 1980", style=list(color="black", fontSize = "20px"))

hc #visualisation

htmlwidgets::saveWidget(widget = hc, file = "~/plot.html")
setwd("~")
webshot::install_phantomjs()
webshot::webshot(url = "plot.html", 
                 file = "/Users/jonathan/Desktop/R_Project/Plot/World.pdf")


######################################
### Does the organizer win more ? ###
####################################

pays_organisateur <- c("Soviet Union","United States", "South Korea", "Spain", "Australia", "Greece", "China", "Great Britain", "Brazil")

count_medals <- function(MyData) {
  keeps <- c("Year","Medal")
  JO_Filtered <- MyData[keeps]
  JO_Dummies <- data.frame(dummy(JO_Filtered$Medal))
  JO_Filtered <- JO_Dummies[1:3]
  JO_SumDum <- data.frame(rowSums(JO_Filtered))
  names(JO_SumDum)[1] <- "Medals"
  keeps <- c("Year")
  JO_Year <- MyData[keeps]
  JO_Medals <- cbind(JO_Year,JO_SumDum)
  return(ddply(JO_Medals, "Year", summarise, sum_medals=sum(Medals))) 
}

count_participant <- function(MyData) {
  df <- data.frame(Year=character(),Nb_Participant=integer())
  for (year in c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) {
    filter1 <- filter(MyData, Year == year)
    participants <- unique(filter1$Name)
    nb_participant <- sum(count(participants)$freq)
    df[nrow(df) + 1,] = list(year,nb_participant)
  }
  return(df)
}

years <- data.frame(Year=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016))

JO_URSS <- filter(summer_recent_JO, Team == "Soviet Union")
urss_nb_medal <- count_medals(JO_URSS)
urss_nb_participant <- count_participant(JO_URSS)
urss_ratio <- data.frame(urss_ratio=urss_nb_medal$sum_medals / urss_nb_participant$Nb_Participant)
years <- cbind(years,urss_ratio)

JO_USA <- filter(summer_recent_JO, Team == "United States")
usa_nb_medal <- count_medals(JO_USA)
usa_nb_participant <- count_participant(JO_USA)
usa_ratio <- data.frame(usa_ratio=usa_nb_medal$sum_medals / usa_nb_participant$Nb_Participant)
years <- cbind(years,usa_ratio)

JO_Korea <- filter(summer_recent_JO, Team == "South Korea")
korea_nb_medal <- count_medals(JO_Korea)
korea_nb_participant <- count_participant(JO_Korea)
korea_ratio <- data.frame(korea_ratio=korea_nb_medal$sum_medals / korea_nb_participant$Nb_Participant)
years <- cbind(years,korea_ratio)

JO_Spain <- filter(summer_recent_JO, Team == "Spain")
spain_nb_medal <- count_medals(JO_Spain)
spain_nb_participant <- count_participant(JO_Spain)
spain_ratio <- data.frame(spain_ratio=spain_nb_medal$sum_medals / spain_nb_participant$Nb_Participant)
years <- cbind(years,spain_ratio)

JO_Australia <- filter(summer_recent_JO, Team == "Australia")
australia_nb_medal <- count_medals(JO_Australia)
australia_nb_participant <- count_participant(JO_Australia)
australia_ratio <- data.frame(australia_ratio=australia_nb_medal$sum_medals / australia_nb_participant$Nb_Participant)
years <- cbind(years,australia_ratio)

JO_Greece <- filter(summer_recent_JO, Team == "Greece")
greece_nb_medal <- count_medals(JO_Greece)
greece_nb_participant <- count_participant(JO_Greece)
greece_ratio <- data.frame(greece_ratio=greece_nb_medal$sum_medals / greece_nb_participant$Nb_Participant)
years <- cbind(years,greece_ratio)

JO_China <- filter(summer_recent_JO, Team == "China")
china_nb_medal <- count_medals(JO_China)
china_nb_participant <- count_participant(JO_China)
china_ratio <- data.frame(china_ratio=china_nb_medal$sum_medals / china_nb_participant$Nb_Participant)
years <- cbind(years,china_ratio)

JO_GB <- filter(summer_recent_JO, Team == "Great Britain")
gb_nb_medal <- count_medals(JO_GB)
gb_nb_participant <- count_participant(JO_GB)
gb_ratio <- data.frame(gb_ratio=gb_nb_medal$sum_medals / gb_nb_participant$Nb_Participant)
years <- cbind(years,gb_ratio)

JO_Brazil <- filter(summer_recent_JO, Team == "Brazil")
brazil_nb_medal <- count_medals(JO_Brazil)
brazil_nb_participant <- count_participant(JO_Brazil)
brazil_ratio <- data.frame(brazil_ratio=brazil_nb_medal$sum_medals / brazil_nb_participant$Nb_Participant)
years <- cbind(years,brazil_ratio)

df_plot <- melt(years ,  id.vars = 'Year', variable.name = 'Country')

ggplot(df_plot, aes(Year,value)) + geom_line(aes(colour = Country))

#remove all inf
#format x axe

