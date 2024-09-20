



#
# About Dataset
#
# This dataset provides detailed information about 120 different video games. Each entry in the dataset represents a video game with the following attributes:
#  
#  Title: The name of the video game.
#  Genre: The category or type of gameplay, such as Action-Adventure, First-Person Shooter, RPG, etc.
#  Platform: The gaming system(s) on which the game can be played, such as PC, PlayStation, Xbox, Switch, or Multi--platform.
#  ReleaseYear: The year in which the game was released.
#  NumPlayers: The maximum number of players that can play the game simultaneously.
#  AvgRating: The average rating of the game, typically on a scale from 0 to 10






# Importation des packages 

{
  library(readr)
  library(skimr)
  library(ggplot2)
  library(gganimate)
  library(plotly)
  library(dplyr)
  library(vtable)
}



# Importation des bases de donnees


DataVG <- read_csv("Data/Video_Game_Information.csv", col_types = cols(ReleaseYear = col_number(), NumPlayers = col_number(), AvgRating = col_number()))


# Description de la base de données

{
  labelsVG <- c("Le nom du jeu vidéo",
            "La catégorie ou le type de gameplay du jeu concerné",
            "La plateforme du jeu vidéo concerné",
            "L'année de sortie du jeu vidéo",
            "Le nombre maximum de joueur en simultané",
            "La notation du jeu concerné comprise entre 0 et 10")
  vtable(DataVG,labels=labelsVG,index=TRUE,factor.limit=0,data.title = "Base de données sur les jeux vidéo",missing = TRUE,lush = TRUE)
  vtable(DataVG,file = "DataVGbasedescription.html",labels=labelsVG,index=TRUE,factor.limit=0,data.title = "Base de données sur les jeux vidéo",lush = TRUE,missing = TRUE)
}





# Réalisation de Script pour anomalie de la donnée

ANOMALIE <- function(DATA) {
  D1 <- DATA %>% count(Title)
  DF <- na.omit(D1) %>% filter(n != 1) %>% kable("pipe")
  return(DF)
}

ANOMALIE(DataVG)

DataVG %>% filter(Title == "Celeste")

DataVG1 <- DataVG %>% distinct(Title, .keep_all = TRUE)
DataVG <- DataVG %>% distinct_all(keep_all = TRUE)

TestData <- DataVG %>% group_by(Genre) %>% mutate(NumCumul = sum(NumPlayers),AvgGenre = mean(AvgRating),Nombre = n()) %>% ungroup()



# Ajout des nouvelles variables


AddVariable <- function(DATA) {
  Var <- as.numeric(length(DATA[[1]]))
  P1 <- c(1:Var)
  P2 <- c(1:Var)
  P3 <- c(1:Var)
  for(i in 1:Var)
  if (DATA[i,"Platform"] == "NES") {
    if (DATA[i,"ReleaseYear"] < 1990) {
      P1[i] <- 1986
      P2[i] <- "Salon"
      P3[i] <- "Nintendo Entertainement System"
    }
  }
  else if (DATA[i,"Platform"] == "Switch") {
    if (DATA[i,"ReleaseYear"] >= 2017) {
      P1[i] <- 2017
      P2[i] <- "Hybrid"
      P3[i] <- "Nintendo Switch"
    }
  }
  else if (DATA[i,"Platform"] == "Xbox") {
    if (DATA[i,"ReleaseYear"] < 2005) {
      P1[i] <- 2002
      P2[i] <- "Salon"
      P3[i] <- "Xbox"
    }
    if (DATA[i,"ReleaseYear"] >= 2005 & DATA[i,"ReleaseYear"] < 2013) {
      P1[i] <- 2005
      P2[i] <- "Salon"
      P3[i] <- "Xbox 360"
    }
    if (DATA[i,"ReleaseYear"] >= 2013 & DATA[i,"ReleaseYear"] < 2017) {
      P1[i] <- 2013
      P2[i] <- "Salon"
      P3[i] <- "Xbox One"
    }
    if (DATA[i,"ReleaseYear"] >= 2017 & DATA[i,"ReleaseYear"] < 2020) {
      P1[i] <- 2017
      P2[i] <- "Salon"
      P3[i] <- "Xbox One X"
    }
    if (DATA[i,"ReleaseYear"] >= 2020) {
      P1[i] <- 2020
      P2[i] <- "Salon"
      P3[i] <- "Xbox Series"
    }
  }
  else if (DATA[i,"Platform"] == "PlayStation") {
    if (DATA[i,"ReleaseYear"] < 2000) {
      P1[i] <- 1995
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 1"
    }
    if (DATA[i,"ReleaseYear"] >= 2000 & DATA[i,"ReleaseYear"] < 2007) {
      P1[i] <- 2000
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 2"
    }
    if (DATA[i,"ReleaseYear"] >= 2007 & DATA[i,"ReleaseYear"] < 2013) {
      P1[i] <- 2007
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 3"
    }
    if (DATA[i,"ReleaseYear"] >= 2013 & DATA[i,"ReleaseYear"] < 2016) {
      P1[i] <- 2013
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 4"
    }
    if (DATA[i,"ReleaseYear"] >= 2016 & DATA[i,"ReleaseYear"] < 2020) {
      P1[i] <- 2016
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 4 Pro"
    }
    if (DATA[i,"ReleaseYear"] >= 2020) {
      P1[i] <- 2020
      P2[i] <- "Salon"
      P3[i] <- "PlayStation 5"
    }
  }
  else {
    P1[i] <- NA
    P2[i] <- NA
    P3[i] <- NA}
  DateConsole <- P1
  TypeConsole <- P2
  RefConsole <- P3
  Newdata <- data.frame(DATA,DateConsole,TypeConsole,RefConsole)
  return(Newdata)
}

NewData <- AddVariable(DataVG)


# Autre méthode


Newdata2 <- NewData %>% mutate(DateConsole1 = case_when(Platform == "NES" & ReleaseYear < 1990 ~ 1986,
                                                        Platform == "Switch" & ReleaseYear >= 2017 ~ 2017,
                                                        Platform == "Xbox" & ReleaseYear < 2005 ~ 2002,
                                                        Platform == "Xbox" & ReleaseYear >= 2005 & ReleaseYear < 2013 ~ 2005,
                                                        Platform == "Xbox" & ReleaseYear >= 2013 & ReleaseYear < 2017 ~ 2013,
                                                        Platform == "Xbox" & ReleaseYear >= 2017 & ReleaseYear < 2020 ~ 2017,
                                                        Platform == "Xbox" & ReleaseYear >= 2020 ~ 2020,
                                                        Platform == "PlayStation" & ReleaseYear < 2000 ~ 1995,
                                                        Platform == "PlayStation" & ReleaseYear >= 2000 & ReleaseYear < 2007 ~ 2000,
                                                        Platform == "PlayStation" & ReleaseYear >= 2007 & ReleaseYear < 2013 ~ 2007,
                                                        Platform == "PlayStation" & ReleaseYear >= 2013 & ReleaseYear < 2016 ~ 2013,
                                                        Platform == "PlayStation" & ReleaseYear >= 2016 & ReleaseYear < 2020 ~ 2016,
                                                        Platform == "PlayStation" & ReleaseYear >= 2020 ~ 2020),
                              TypeConsole1 = case_when(Platform == "NES" & ReleaseYear < 1990 ~ "Salon",
                                                        Platform == "Switch" & ReleaseYear >= 2017 ~ "Hybrid",
                                                        Platform == "Xbox" & ReleaseYear < 2005 ~ "Salon",
                                                        Platform == "Xbox" & ReleaseYear >= 2005 & ReleaseYear < 2013 ~ "Salon",
                                                        Platform == "Xbox" & ReleaseYear >= 2013 & ReleaseYear < 2017 ~ "Salon",
                                                        Platform == "Xbox" & ReleaseYear >= 2017 & ReleaseYear < 2020 ~ "Salon",
                                                        Platform == "Xbox" & ReleaseYear >= 2020 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear < 2000 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear >= 2000 & ReleaseYear < 2007 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear >= 2007 & ReleaseYear < 2013 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear >= 2013 & ReleaseYear < 2016 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear >= 2016 & ReleaseYear < 2020 ~ "Salon",
                                                        Platform == "PlayStation" & ReleaseYear >= 2020 ~ "Salon"),
                              RefConsole1 = case_when(Platform == "NES" & ReleaseYear < 1990 ~ "Nintendo Entertainement System",
                                                       Platform == "Switch" & ReleaseYear >= 2017 ~ "Nintendo Switch",
                                                       Platform == "Xbox" & ReleaseYear < 2005 ~ "Xbox",
                                                       Platform == "Xbox" & ReleaseYear >= 2005 & ReleaseYear < 2013 ~ "Xbox 360",
                                                       Platform == "Xbox" & ReleaseYear >= 2013 & ReleaseYear < 2017 ~ "Xbox One",
                                                       Platform == "Xbox" & ReleaseYear >= 2017 & ReleaseYear < 2020 ~ "Xbox One X",
                                                       Platform == "Xbox" & ReleaseYear >= 2020 ~ "Xbox Series",
                                                       Platform == "PlayStation" & ReleaseYear < 2000 ~ "PlayStation 1",
                                                       Platform == "PlayStation" & ReleaseYear >= 2000 & ReleaseYear < 2007 ~ "PlayStation 2",
                                                       Platform == "PlayStation" & ReleaseYear >= 2007 & ReleaseYear < 2013 ~ "PlayStation 3",
                                                       Platform == "PlayStation" & ReleaseYear >= 2013 & ReleaseYear < 2016 ~ "PlayStation 4",
                                                       Platform == "PlayStation" & ReleaseYear >= 2016 & ReleaseYear < 2020 ~ "PlayStation 4 Pro",
                                                       Platform == "PlayStation" & ReleaseYear >= 2020 ~ "PlayStation 5"))




# Réalisation de SQL avec le package dplyr de R


DataGroup <- function(DATA,type = c("Categorie","Plat")) {
  if(type == "Categorie"){
    NewData <- DATA %>% group_by(Genre) %>% mutate(NumCumul = sum(NumPlayers),AvgGenre = mean(AvgRating),Nombre = n()) %>% distinct(Genre,.keep_all = TRUE) %>% select(Genre,NumCumul,AvgGenre,Nombre) %>% ungroup()
  }
  else if(type == "Plat"){
    NewData <- DATA %>% group_by(Platform) %>% mutate(NumCumul = sum(NumPlayers),AvgGenre = mean(AvgRating),Nombre = n()) %>% distinct(Platform,.keep_all = TRUE) %>% select(Platform,NumCumul,AvgGenre,Nombre) %>% ungroup()
  }
  return(NewData)
}


DataG <- DataGroup(DataVG,"Categorie")
DataP <- DataGroup(DataVG,"Plat")





skim(DataVG)
skim(DataG)
skim(DataP)







# Partie Graphique avec ggplot2 et ggplotly





p <- ggplot(DataVG, aes(x=ReleaseYear, fill=Platform)) +
  geom_histogram(binwidth=1, position="stack") +
  labs(
    title    = "Histogramme des consoles de jeu vidéo",
    subtitle = "Période entre 1984 et 2020",
    x        = "Temps en années",
    y        = "Nombre",
    caption  = "Graphique sous ggplot2")
p
ggplotly(p)



p1 <- ggplot(DataVG, aes(ReleaseYear, fill = Platform)) +
  geom_bar(alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  labs(title = 'Date: {frame_time}', x = 'Temps') +
  transition_time(DataVG$ReleaseYear) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()
p1
animate(p1, renderer = gifski_renderer('TestGIF.gif'),duration=30,nframes=800)






plot_ly(DataG,x = ~Genre,y = ~Nombre,type = "bar",color = ~Genre) %>% 
  layout(xaxis = list(title = "Répartition des genres", zerolinewidth = 2, gridcolor = 'ffff',rangeslider = list(visible = T)),
         showlegend = T, title='Représentation graphique des genres',
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6', margin = 0.5,
         legend = list(title = list(text="<br> Genre <br>"))) %>%
  config(scrollZoom = TRUE,modeBarButtonsToAdd = c("drawline",
                                                   "drawopenpath",
                                                   "drawclosedpath",
                                                   "drawcircle", 
                                                   "drawrect", 
                                                   "eraseshape",
                                                   "editInChartStudio",
                                                   "toggleSpikesLines"),locale = "fr")




plot_ly(DataP,x = ~Platform,y = ~Nombre,type = "bar",color = ~Platform) %>% 
  layout(xaxis = list(title = "Répartition des plateformes", zerolinewidth = 2, gridcolor = 'ffff',rangeslider = list(visible = T)),
         showlegend = T, title='Représentation graphique des plateformes',
         yaxis = list(zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6', margin = 0.5,
         legend = list(title = list(text="<br> Plateforme <br>"))) %>%
  config(scrollZoom = TRUE,modeBarButtonsToAdd = c("drawline",
                                                   "drawopenpath",
                                                   "drawclosedpath",
                                                   "drawcircle", 
                                                   "drawrect", 
                                                   "eraseshape",
                                                   "editInChartStudio",
                                                   "toggleSpikesLines"),locale = "fr")






ggformula
learnR
r4ds tutorials
tutorials helpers

library(validate)
library(DataExplorer)
library(pointblank)
library(dlookr)
library(GWalkR)
gwalkr(DataVG,dark = "dark")
create_report(DataVG,output_file = "ReportDataVG.html")

scan_data(DataVG)

diagnose_paged_report(DataVG)
diagnose_report(DataVG)
diagnose_web_report(DataVG)


