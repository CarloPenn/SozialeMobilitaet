# Soziale Mobilität an den Habsburger Kaiserhöfen
# Projektpartner: Mark Hengerer
# Betreuer: Cornelius Fritz, Daniel Radzek
# Studenten: Lukas Gansneder, Yifan Zhao, Jakob Winkler, Carlo Penn

library(dplyr)
library(ggplot2)
# Datensätze einlesen
# Datensätze bereinigen
amt = read.table("raw/amt.csv",header = F,sep = ",")
amt <- amt[c(1,2, 4)]
names(amt) <- c("Kürzel", "Amt", "Kategorie" )

beziehung = read.table("raw/beziehung.csv",header = F,sep = ",")
beziehung <- beziehung[2:nrow(beziehung), ]
names(beziehung) <- c("id", "Verwandtschaftsgrad", "Person2", "Hochzeitsdatum")

hofstaat = read.table("raw/hofstaat.csv",header = F,sep = ",")
names(hofstaat) <- c("Kürzel", "ID", "Name")

liste = read.table("raw/liste.csv",header = T,sep = ",")
liste <- liste[1:12]

person = read.table("raw/person.csv",header = F,sep = ",")
person <- person[1:7]
names(person) = c("id", "name", "birth", "loc_birth","death","loc_death","sex")
#########Bereinigt bis hierhin


event_list = read.table("raw/event_list.csv",header = F,sep = ",")
linkage = read.table("raw/linkage.csv",header = F,sep = ",")

names = read.table("raw/names.csv",header = F,sep = ",")
ort = read.table("raw/ort.csv",header = F,sep = ",")
schema = read.table("raw/schema.csv",header = F,sep = ",")
zliste = read.table("raw/zliste.csv",header = F,sep = ",")


# Persondatensatz: Vater, Mutter und Ehepartner hinzufügen
beziehung$id <- as.integer(beziehung$id)

person <- person %>% 
  left_join(beziehung, by = "id")


# Grafik zu ANzahl Positionen
ggplot(data = liste) +
  geom_histogram(mapping = aes(x = ))
