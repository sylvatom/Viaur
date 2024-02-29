## script pour traiter les données telemetrie sur le Viaur ##

library(tidyverse)
# language avec %>% ("ensuite")
# sujet %>% verbe (complement)
# ex. 0:10 %>% mean() 
# ex.
# data %>%
#   mutate(annee = year(date_prelevement)) %>%
#   group_by(annee)%>% # la suite des operation fait par année#
#   summarise(moyenne = mean(resultat,na.rm=TRUE)), mesure_totl = sum(resultat, na.rm=TRUE)

## data ##

#detection des poissons
#marquage poissons
#mouchards
#temperature
#Ijinus
#debit dreal

detection <-read.csv("./data/Detections.csv", sep=";")%>% 
  mutate(date_heure = lubridate::dmy_hm(Date_heure)) %>% 
  distinct()

marquage <-read.csv("./data/Marquage.csv", sep=";")
marquage2 <-distinct(marquage, Code, .keep_all = TRUE)# elimine les poissons recapturés n+1

mouchard <-read.csv("./data/Mouchard.csv", sep=";")

Temp <-read.csv("./data/Temperature.csv", sep=";")%>% 
  mutate(date_heure = lubridate::dmy_hms(Date_heure))

Haut <-read.csv("./data/Ijinus.csv", sep=";")%>% 
  mutate(date_heure = lubridate::dmy_hms(Date_heure))

Q <-read.csv("./data/Debit_dreal.csv", sep=";") %>% 
  mutate(date_heure = lubridate::dmy_hm(Date_heure))

# Q$Date_heure = as.POSIXct(substr(Q$Date_heure,1,16),format="%d/%m/%Y %H:%M",tz="GMT")
# 
# detection$Date_heure = as.POSIXct(substr(detection$Date_heure,1,16),format="%d/%m/%Y %H:%M",tz="GMT")
# 
# Temp$Date_heure = as.POSIXct(substr(Temp$Date_heure,1,16),format="%d/%m/%Y %H:%M",tz="GMT")
# 
# Haut$Date_heure = as.POSIXct(substr(Haut$Date_heure,1,16),format="%d/%m/%Y %H:%M",tz="GMT")


# data2<-filter(data, taille>1000)# selection

# enlever les doublons
# detection<-detection[!duplicated(detection[c("Date_heure","Antenne","Code")]),] # 242648 => 105612 à la minute



  ## ANALYSE DES MOUCHARDS ##

# data detection des Mouchards
Det_mou<-inner_join(detection,mouchard,by="Code")#

datum <- as.data.frame(seq(min(as.Date(Det_mou$date)), max(as.Date(Det_mou$date)), by="day"))
  colnames(datum) <- "date"

## RIVIERE ##
riv<-Det_mou %>%
  filter(Emplacement =="RIVIERE") %>%
  mutate(date = as.Date(substr(date_heure,1,10))) %>%
  group_by(date) %>%
  summarise(nombre=table(date)) %>%
  mutate(taux=nombre/48*100) 
  
riv2 <- datum %>% 
  left_join(riv) %>% 
  mutate(taux = ifelse(is.na(taux), 0, taux),
         taux = ifelse(taux > 100, 100, taux))


A_riv <- ggplot(data=riv2, aes(date, y=taux)) + 
  geom_line(size = 0.8) + 
  coord_cartesian(ylim = c(0, 100))+
  labs(title="Viaur - Antenne Rivière"
       , x="date", y="Fonctionnement de l'antenne -mouchard (%)")

scale_y_continuous(expand=c(0,0))+
geom_hline(yintercept = 100, color="black", size = 1.5)+

## ENTREE ##
Entree<-Det_mou %>%
  filter(Emplacement =="AVAL") %>%
  mutate(date = as.Date(substr(date_heure,1,10))) %>%
  group_by(date) %>%
  summarise(nombre=table(date)) %>%
  mutate(taux=nombre/48*100)

Entree2 <- datum %>% 
  left_join(Entree) %>% 
  mutate(taux = ifelse(is.na(taux), 0, taux),
         taux = ifelse(taux > 100, 100, taux))

A_entree <- ggplot(data=Entree2, aes(date, y=taux)) + 
  geom_line(size = 0.8) + 
  coord_cartesian(ylim = c(0, 100))+
  labs(title="Viaur - Antenne Entrée passe"
       , x="date", y="Fonctionnement de l'antenne -mouchard (%)")

## SORTIE ##
Sortie<-Det_mou %>%
  filter(Emplacement =="AMONT") %>%
  mutate(date = as.Date(substr(date_heure,1,10))) %>%
  group_by(date) %>%
  summarise(nombre=table(date)) %>%
  mutate(taux=nombre/48*100)

Sortie2 <- datum %>% 
  left_join(Sortie) %>% 
  mutate(taux = ifelse(is.na(taux), 0, taux),
         taux = ifelse(taux > 100, 100, taux))

A_sortie= ggplot(data=Sortie2, aes(date, y=taux)) + 
  geom_line(size = 0.8) + 
  coord_cartesian(ylim = c(0, 100))+
  labs(title="Viaur - Antenne Sortie passe"
       , x="date", y="Fonctionnement de l'antenne -mouchard (%)")




#Enlever les mouchards ==> On ne conserve que les codes qui correspondent au marquage des poissons, les mouchards sont donc supprim?s
# Marquage<-subset(Marquage,Marquage$Ann?e=="2018"|Marquage$Ann?e=="2019") # 963 poissons marqu?s en 2018-2019
library(dplyr)
data<-inner_join(detection,marquage,by="Code")# Tous les mouchards, + codes bizaroide ont  ete enleves, 28279L
# = jointure interne, ne sont conserv?s que les individus communs aux deux jeux de donn?es

data<-detection %>% 
  inner_join(marquage2) %>% 
  select(date_heure, Antenne, Code, Esp, Secteur, Taille..mm.)

## 

  ## JOINTURE DES DONNEES AVEC TEMPERATURE ET DEBIT ##


# install.packages("fuzzyjoin")
# OU devtools::install_github("dgrtwo/fuzzyjoin")
library(fuzzyjoin)

# je joins par Date_heure en chr
jointure1 <- data %>% 
  stringdist_inner_join(Temp)



