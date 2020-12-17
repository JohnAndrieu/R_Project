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

# chargement du dataset depuis le repository git
url_content <- getURL("https://raw.githubusercontent.com/JohnAndrieu/R_Project/master/athlete_events.csv")
dataset_JO <- read.csv(text = url_content)

# filtrage des données : on récupère les lignes relatives aux JO d'été depuis ceux de 1980
summer_recent_JO <- filter(filter(dataset_JO, Season == "Summer"), Year >= 1980)

########################
### Medal Frequence ###
######################

# Trouver les 5 sports avec le ratio nombre de médaillés / nombre de participants est le plus élevé

sport_freq <- count(summer_recent_JO$Sport) # nombre de participants pour chaque sport
medal_freq <- count(filter(summer_recent_JO, Medal != 'NA')$Sport) # nombre de médaillés par sport
percent_medal <- (medal_freq$freq/sport_freq$freq)*100 # calcul du pourcentage de médaillés pour chaque sport
df <- cbind(sport_freq, percent_medal)
top_medal_sport <- arrange(df, -df$percent_medal)[1:5,] # on garde le top 5

# on affiche les résultats sous forme d'histogramme et on le sauvegarde au format pdf

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

# répartition de l'imc pour les 5 sports avec le ratio nombre de médaillés / nombre de participants le plus élevé

imc <- summer_recent_JO$Weight/((summer_recent_JO$Height/100)*(summer_recent_JO$Height/100)) # calcul de l'imc pour tous les sportifs
imc_df <- cbind(summer_recent_JO, imc)     

# on filtre imc homme et femme
imc_male <- filter(filter(imc_df, Sport %in% top_medal_sport$x), Sex == "M") 
imc_female <- filter(filter(imc_df, Sport %in% top_medal_sport$x), Sex == "F")
imc_all_male <- filter(imc_df, Sex == "M")
imc_all_female <- filter(imc_df, Sex == "F")

# on calcule la moyenne des imc pour homme et femme par sport
mean_all_m <- ddply(imc_all_male, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_all_f <- ddply(imc_all_female, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_m <- ddply(imc_male, "Sport", summarise, mean_imc=mean(na.omit(imc)))
mean_f <- ddply(imc_female, "Sport", summarise, mean_imc=mean(na.omit(imc)))

# on affiche les résultats sous forme de distribution de densité pour homme et femme puis on sauvegarde au format pdf
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

# on calcule la distribution de l'age pour les 5 sports avec le ratio nombre de médaillés / nombre de participants le plus élevé

# pour les hommes
topSport_M <- filter(filter(summer_recent_JO, Sport %in% top_medal_sport$x), Sex == "M")
groupby <- topSport_M %>%
  group_by(Sport) %>%
  select(Age)

# calcul de la médianne de l'age pour les hommes
meds <- ddply(na.omit(groupby), .(Sport), summarise, med = median(Age))

# création des boites à moustache pour les hommes
bp_male <- ggplot(groupby, aes(x=Age, y=Sport, color=Sport)) + 
  geom_boxplot() +
  xlab("Age Homme") +
  geom_text(data = meds, aes(x = med, y = Sport, label = med), size = 2, vjust = -0.5, hjust = -0.6)

# pour les femmes
topSport_F <- filter(filter(summer_recent_JO, Sport %in% top_medal_sport$x), Sex == "F")
groupby <- topSport_F %>%
  group_by(Sport) %>%
  select(Age)

# calcul de la médianne de l'age pour les femmes
meds <- ddply(na.omit(groupby), .(Sport), summarise, med = median(Age))

# création des boites à moustache pour les femmes
bp_female <- ggplot(groupby, aes(x=Age, y=Sport, color=Sport)) + 
  geom_boxplot() +
  xlab("Age Femme") +
  geom_text(data = meds, aes(x = med, y = Sport, label = med), size = 2, vjust = -0.5, hjust = -0.6)

# affichage des graphiques et sauvegarde au format pdf
g <- arrangeGrob(bp_male, bp_female, ncol=1, nrow=2)
ggsave(file="/Users/jonathan/Desktop/R_Project/Plot/Age.pdf", g)

### Les sports où on est le plus agé et le plus jeune

TopAge <- summer_recent_JO %>%
  group_by(Sport) %>%
  select(Age)

AgesMeds <- ddply(na.omit(TopAge), .(Sport), summarise, mea = mean(Age))

##################################
### Which country won the most ###
#################################

# on calcule combien de médailles ont gagné chaque pays

# transformation des médailles en dummies variables afin de les compter
dummy <- data.frame(dummy(summer_recent_JO$Medal))
keeps <- c("Medal.Bronze","Medal.Silver", "Medal.Gold")
dummies <- dummy[keeps]
sum_dummy <- data.frame(rowSums(dummies))

# on merge les dummies variables avec la liste de pays
country_dummy <- data.frame(cbind(summer_recent_JO$Team,sum_dummy))

# on fait la somme des médailles pour chaque pays
res <- ddply(country_dummy, "summer_recent_JO.Team", summarise, sum_medal=sum(rowSums.dummies.))

# on sélectionne les colonnes qui nous intéressent
keeps <- c("summer_recent_JO.Team","sum_medal")
country_medals = res[keeps]

# on renomme les colonnes 1 et 2
names(country_medals)[1] <- "country"
names(country_medals)[2] <- "medals"

# on modifie le label des USA afin d'afficher sa valeur sur la heat map
country_medals$country[which(country_medals$country == "United States")] <- "United States of America"

# chargement des couleurs
couleurs <- colorRampPalette(c('white', 'red'))

# chargement de la carte du monde
data(worldgeojson, package = "highcharter")

# création du highchart 
hc <- highchart() %>%
  hc_add_series_map(
    worldgeojson, country_medals, value = "medals", joinBy = c('name','country'),
    name = "Nombre de médaillés"
  )  %>% 
  hc_colorAxis(minColor="white", maxColor="#b12134") %>% 
  hc_title(text = "Nombre de médaillés", style=list(color="#b12134", fontSize = "50px")) %>% 
  hc_subtitle(text = "dans tous les sports depuis 1980", style=list(color="black", fontSize = "20px"))

hc #visualisation

# on sauvegarde le highchart au format pdf 
htmlwidgets::saveWidget(widget = hc, file = "~/plot.html")
setwd("~")
webshot::install_phantomjs()
webshot::webshot(url = "plot.html", 
                 file = "/Users/jonathan/Desktop/R_Project/Plot/World.pdf")

######################################
### Does the organizer win more ? ###
####################################

# on calcule et affiche l'évolution du ratio nombre de médaillés / nombre de participants au fil des années 
# sur les 10 derniers JO
# pour les pays organisateurs 

# liste des pays organisateurs sur les 10 derniers JO
pays_organisateur <- c("Soviet Union","United States", "South Korea", "Spain", "Australia", "Greece", "China", "Great Britain", "Brazil")

# fonction qui compte le nombre de médaillés par JO
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

# fonction qui compte le nombre de participants par JO
count_participant <- function(MyData) {
  df <- data.frame(Year=integer(),Nb_Participant=integer())
  for (year in c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) {
    filter1 <- filter(MyData, Year == year)
    participants <- unique(filter1$Name)
    nb_participant <- sum(count(participants)$freq)
    df[nrow(df) + 1,] = list(year,nb_participant)
  }
  return(df)
}

# liste des années
years <- data.frame(Year=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016))

# pour chaque pays organisateur, on calcule le ratio et on l'ajoute à la data frame
JO_URSS <- filter(summer_recent_JO, Team == "Soviet Union")
urss_nb_medal <- count_medals(JO_URSS)
urss_nb_participant <- count_participant(JO_URSS)
URSS <- data.frame(URSS=urss_nb_medal$sum_medals / urss_nb_participant$Nb_Participant)
years <- cbind(years,URSS)

JO_USA <- filter(summer_recent_JO, Team == "United States")
usa_nb_medal <- count_medals(JO_USA)
usa_nb_participant <- count_participant(JO_USA)
USA <- data.frame(USA=usa_nb_medal$sum_medals / usa_nb_participant$Nb_Participant)
years <- cbind(years,USA)

JO_Korea <- filter(summer_recent_JO, Team == "South Korea")
korea_nb_medal <- count_medals(JO_Korea)
korea_nb_participant <- count_participant(JO_Korea)
Corée <- data.frame(Corée=korea_nb_medal$sum_medals / korea_nb_participant$Nb_Participant)
years <- cbind(years,Corée)

JO_Spain <- filter(summer_recent_JO, Team == "Spain")
spain_nb_medal <- count_medals(JO_Spain)
spain_nb_participant <- count_participant(JO_Spain)
Espagne <- data.frame(Espagne=spain_nb_medal$sum_medals / spain_nb_participant$Nb_Participant)
years <- cbind(years,Espagne)

JO_Australia <- filter(summer_recent_JO, Team == "Australia")
australia_nb_medal <- count_medals(JO_Australia)
australia_nb_participant <- count_participant(JO_Australia)
Australie <- data.frame(Australie=australia_nb_medal$sum_medals / australia_nb_participant$Nb_Participant)
years <- cbind(years,Australie)

JO_Greece <- filter(summer_recent_JO, Team == "Greece")
greece_nb_medal <- count_medals(JO_Greece)
greece_nb_participant <- count_participant(JO_Greece)
Grèce <- data.frame(Grèce=greece_nb_medal$sum_medals / greece_nb_participant$Nb_Participant)
years <- cbind(years,Grèce)

JO_China <- filter(summer_recent_JO, Team == "China")
china_nb_medal <- count_medals(JO_China)
china_nb_participant <- count_participant(JO_China)
Chine <- data.frame(Chine=china_nb_medal$sum_medals / china_nb_participant$Nb_Participant)
years <- cbind(years,Chine)

JO_GB <- filter(summer_recent_JO, Team == "Great Britain")
gb_nb_medal <- count_medals(JO_GB)
gb_nb_participant <- count_participant(JO_GB)
RoyaumeUni <- data.frame(RoyUni=gb_nb_medal$sum_medals / gb_nb_participant$Nb_Participant)
years <- cbind(years,RoyaumeUni)

JO_Brazil <- filter(summer_recent_JO, Team == "Brazil")
brazil_nb_medal <- count_medals(JO_Brazil)
brazil_nb_participant <- count_participant(JO_Brazil)
Brésil <- data.frame(Brésil=brazil_nb_medal$sum_medals / brazil_nb_participant$Nb_Participant)
years <- cbind(years,Brésil)

JO_Russia <- filter(summer_recent_JO, Team == "Russia")
russia_nb_medal <- count_medals(JO_Russia)
russia_nb_participant <- count_participant(JO_Russia)
Russie <- data.frame(Russie=russia_nb_medal$sum_medals / russia_nb_participant$Nb_Participant)
years <- cbind(years,Russie)

# on sépare les résultats en deux paquets par soucis de visibilité
keeps <- c("Year","URSS","Russie","Chine","USA", "Australie")
batch1 = years[keeps]
keeps <- c("Year","Corée","Grèce","Brésil","RoyUni", "Espagne")
batch2 = years[keeps]

# on melt les datasets
batch1_melt <- melt(batch1 ,  id.vars = 'Year', variable.name = 'Pays')
batch2_melt <- melt(batch2 ,  id.vars = 'Year', variable.name = 'Pays')

# on retire les valeurs infinies de notre dataset afin de l'afficher
df1_plot <- batch1_melt[batch1_melt$value != Inf,]
df2_plot <- batch2_melt[batch2_melt$value != Inf,]

# on liste les couleurs à utiliser pour les deux graphiques
color1 = c("red", "red", "yellow", "blue", "black")
color2 = c("pink", "orange", "darkgreen", "purple", "turquoise")

# on trace le graphique du premier paquet de données
b1 <- ggplot(df1_plot, aes(Year,value, colour = Pays, shape = Pays)) + 
  scale_shape_manual(values=1:nlevels(df1_plot$Pays)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1980, 2016, 4)) +
  scale_colour_manual(values = color1) +
  ylab("Ratio") +
  ylim(0,1) +
  theme(axis.text.x=element_text(size=8),axis.title.x=element_text(size=10),axis.text.y=element_text(size=8),axis.title.y=element_text(size=10),legend.text = element_text(size=8))

# on trace le graphique du deuxième paquet de données
b2 <- ggplot(df2_plot, aes(Year,value, colour = Pays, shape = Pays)) + 
  scale_shape_manual(values=1:nlevels(df2_plot$Pays)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1980, 2016, 4)) +
  scale_colour_manual(values = color2) +
  ylab("Ratio") +
  ylim(0,0.5) +
  theme(axis.text.x=element_text(size=8),axis.title.x=element_text(size=10),axis.text.y=element_text(size=8),axis.title.y=element_text(size=10),legend.text = element_text(size=8))

# on affiche les deux graphiques sur la même figure et on sauvegarde au format pdf
g <- arrangeGrob(b1, b2, ncol=1, nrow=2)
ggsave(file="/Users/jonathan/Desktop/R_Project/Plot/RatioCountries.pdf", g)


#### Info sur Johnny Weissmuller l'acteur de Tarzan !
dataJohnny <- filter(dataset_JO, Name == "Peter Johann \"Johnny\" Weissmuller")
  

