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

 ##### DATA #####

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


##### Hauteur / temperature#####

  ## Hauteur + temperature

dh<- rbind(Haut %>% select(date_heure), Temp %>% select(date_heure)) %>% 
  distinct()

dh_h <- left_join(dh, Haut)
dh_h_t <- left_join(dh_h, Temp)

dh_h_t <- dh_h_t %>% 
  select(date_heure, H_amont, T_amont) %>% 
  arrange(date_heure) %>% 
  filter(date_heure > as.Date("2022-09-08"))

coef_a<-50
coef_b<-800

Temp_Deb <- ggplot() + 
  geom_line(data=dh_h_t, aes(x=date_heure, y = H_amont), col="darkred") + 
  geom_line(data=dh_h_t %>%
              filter(!is.na(T_amont)),
            aes(x=date_heure, y = T_amont*coef_a + coef_b), color="steelblue")+
  scale_y_continuous(name = "Hauteur d'eau amont (mm)",
                     sec.axis = sec_axis(~(.-coef_b)/coef_a, name="Temperature (°C)"))+
  theme(
    axis.title.y = element_text(color = "darkred", size=13),
    axis.title.y.right = element_text(color = "steelblue", size=13)
) 


 

 


  ## GRAPHIQUES ##

## graphes a deux axes Y

## Débit et T au cours de suivi ##
# plot first plot 
with(deb_temp_J, plot(moyQ ~ date, type = "l",lwd=2, col = "black"))
# set new plot
par(new = T) 
# plot second plot, but without axis
with(deb_temp_J, plot(moyT ~ date, type = "l",lwd=2, col = "grey", xaxt = "n", yaxt = "n", xlab = "", ylab = ""))
# define y-axis and put y-labs
axis(4)
# with(deb_temp_J, mtext("moyT", side = 4))


  #####ANALYSE DES MOUCHARDS #####

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
  geom_line(size = 0.6, color="#CB2027") + 
  coord_cartesian(ylim = c(0, 100))+
  labs(title="Viaur - Antenne Rivière"
       , x="date", y="Fonctionnement de l'antenne -mouchard (%)")



# scale_y_continuous(expand=c(0,0))+
# geom_hline(yintercept = 100, color="black", size = 1.5)+

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
  geom_line(size = 0.6, color="#CB2027") + 
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
  geom_line(size = 0.6, color="#CB2027") + 
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




  ##### PASSAGE SUR ANTENNE - GLOBA L#####

  ## Nombre de diff poissons sur site / jour

Nb_pois <- data %>% 
  mutate(date= as.Date(substr(date_heure,1,10))) %>% 
  select(date, Code) %>% 
  distinct() %>% 
  group_by(date) %>% 
  mutate(nombre=1) %>% 
  select(date, nombre) %>% 
  summarise(nb_poissons=sum(nombre))

min(Nb_pois$date)
max(Nb_pois$date)

Nb_poissons<- ggplot(data = Nb_pois, aes(x=date, y=nb_poissons)) + 
  geom_bar(stat="identity") +
  labs(title="Nb de poisson détectés par jour sur site de Viaur - 3 antennes"
       , x="date", y="Nb de poissons")
  
  
  coord_cartesian(ylim = c(0, 100))+
  labs(title="Viaur - Antenne Sortie passe"
       , x="date", y="Fonctionnement de l'antenne -mouchard (%)")



  ##### EFFICACITE #####

passage<- data %>% 
  select(Antenne, Code, Esp) %>% 
  distinct() %>% 
  mutate(nombre=1) %>% 
  pivot_wider(names_from=Antenne,
              values_from =nombre, values_fill = 0)

# verification des detection et cremplissage des ratés
passage1 <- passage %>% 
  mutate(aval2 = case_when(AMONT==1 & AVAL ==0 ~ 1,
                              TRUE ~ AVAL),
         riviere2 = case_when(aval2==1 & RIVIERE ==0 ~ 1,
                              TRUE ~ RIVIERE))

Pass <- passage1 %>% 
  select(Esp, Code, riviere=riviere2, aval = aval2, amont = AMONT)

  # Nb de poissons presents sur les antennes / espece

Effi<- Pass %>% 
  select (Esp, riviere, aval, amont) %>% 
  group_by(Esp) %>% 
  summarise (nb_rivière=sum(riviere),
             nb_aval=sum(aval),
             nb_amont= sum(amont)) %>% 
  mutate(attractivité = nb_aval/nb_rivière*100, 
         franchissabilité = nb_amont/nb_aval*100, 
         efficacité = nb_aval/nb_rivière*100)


  ## RESULTAT PAR ESPECE ##
# pour mise en forme des tableaux

library(kableExtra)
library(knitr)
library(tinytex)

# OU

library(flextable)

head(Effi) %>%
  flextable::flextable() %>% 
  theme_booktabs() %>% 
  autofit() %>% 
  font(fontname="Rage Italic", part = "all") %>% 
  colformat_double(j=5:7, digits = 2) %>% 
  align(align = "center", part ="all")

theme_vanilla()
theme_zebra() 
  
  

  ##### RDAta pour sortie sous Rmd #####

# sauvegarder sous RData
save(A_riv, A_entree,A_sortie, Effi, Nb_poissons, Temp_Deb, coef_a, coef_b,file=("./RData/Analyse_mouchards.RData"))



##### DECOUPAGE DES DONNEES #####

# data = 

# ordonner par date et Code
Det_mou <- Det_mou %>% 
  arrange(date_heure, Code)



# créer une colonne pour eliminer les detections sur les mêmes antennes



# vectoriser, library purrr

pour effectuer la meme chose pour une liste des espece p ex.
mes_especes <- unique(iris$Species)

en tete de Rmd, tu mets: params:
                          mon_espece: "setosa"
library(purrr) # a regarder
purrr::map(.x= mes-especes,
           .f = rmarkdown::render(input- = "scripts/rmd_test.Rmd",
                                  params = list(mon_espece= .x),
                                  output_file = paste0(.x, "html")))


  ## JOINTURE DES DONNEES AVEC TEMPERATURE ET DEBIT ##


# install.packages("fuzzyjoin")
# OU devtools::install_github("dgrtwo/fuzzyjoin")
library(fuzzyjoin)

data1<-data[1:20,]

# je joins par Date_heure en chr
jointure1 <- data1 %>% 
  regex_left_join(Temp)
  

save(Temp_Deb,file="RData/QT.RData")

