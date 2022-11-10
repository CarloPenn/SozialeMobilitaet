# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ------                            Kaiserhof                           ------ #
# ------                          (Preprocess)                          ------ #
# ------                           Masterthesis                         ------ #
# ------                                                                ------ #
# ------                          Cornelius Fritz                       ------ #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# https://pma.gwi.uni-muenchen.de:8888/
library(pROC)
library(stringr)
library(survival)
library(timereg)
library(KernSmooth)
library(ggpubr)
library(anytime)
library(data.table)
library(eha)
library(glmnet)
library(chron)
library(gridExtra)
library(statnet)
library(igraph)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Actors ----

actors = read.table("raw/person.csv",header = F,sep = ",")
names(actors) = c("id", "name", "birth", "loc_birth","death","loc_death","sex","fast","comment","change","complier_change")

actors$birth_year = as.numeric(substr(actors$birth ,start = 1, stop = 4))
actors$death_year = as.numeric(substr(actors$death,start = 1, stop = 4))
actors$age_year = abs(actors$death_year - actors$birth_year)
#actors = actors[!is.na(actors$death)& !is.na(actors$birth),]

# Relationships -----

relationships = read.table("raw/beziehung.csv",header = F,sep = ",")
names(relationships) = c("from","type","to","date")
relationships$date = as.character(relationships$date)
relationships$year = as.numeric(substr(relationships$date,start = 0,stop = 4))
save(relationships, file = "rdata/relationships.Rdata")

# Marriages -----

marriage = relationships[relationships$type == 2,]
marriage$type = NULL
save(marriage, file = "rdata/marriage.Rdata")

# Father/Mother-Child Relationships -----

children = relationships[relationships$type == 1,]
children$type = NULL
names(children)[1:2] = c("f_m","child")
children$year = actors$birth_year[match(children$child,actors$id)]
children$birth_father = actors$birth_year[match(children$father,actors$id)]
# Check if there are some data errors
sum(children$birth_father>children$year,na.rm = T) # Seems to be all fine (up until now ;-) )

save(children, file = "rdata/children.Rdata")

# Naming abbreviations of jobs with ranks -----

abbreviations = read.table("raw/amt.csv",header = F,sep = ",")
names(abbreviations) = c("abbrev","long_name","gnd", "rank", "active")
save(abbreviations, file = "rdata/abbreviations.Rdata")

# Royal households -----

royal = read.table("raw/hofstaat.csv",header = F,sep = ",")
names(royal) = c("king", "id", "name")
save(royal, file = "rdata/royal.Rdata")


# Work history----

history = fread("raw/liste.csv")
# history = read.table("raw/liste.csv",header = F,sep = ",")
names(history) = c("entry_id", "source", "source_page", "job", "king", "work_id",
                   "name","start","mentioning", "end", "promotion",
                   "compiler_id", "source_comment", "compiler_comment", "comment", "sort")

history$mentioning = NULL
linkage = read.table("raw/linkage.csv",header = F,sep = ",")
names(linkage) = c("entry_id", "id")
history$id = linkage$id[match(history$entry_id,linkage$entry_id)]
# history$id_name= actors$name[match(history$id,actors$id)]
# Add ranking of jobs
history$job = str_trim(as.character(history$job))
history$rank = abbreviations$rank[match(history$job,abbreviations$abbrev)]
history= history[order(history$id),]
history$start_year =  as.numeric(substr(history$start ,start = 0,stop = 4))
history$end_year =  as.numeric(substr(history$end ,start = 0,stop = 4))
history = history[order(history$id,history$start),]

save(history, file = "rdata/history.Rdata")

# Coordinates of the places ----

places = read.table("raw/ort.csv",header = T,sep = ",")
names(places) = c("lat","lon", "found", "name", "la", "en", "fr", "nl", "es","it","ro", "pl", "cs", "sk", "hu", "si", "hr", "xy")
save(places, file = "rdata/places.Rdata")



# Network of the relationships ----


# Network with information on coworkers/marriages/children

# Add info in work in the actor data.frame
history_split  = split(history, history$id, drop = FALSE)


affiliation_per_actor = rbindlist(lapply(history_split, FUN = function(x){
  data.frame(paste(unique(x$king),collapse = " "),x$id[1])
}))
names(affiliation_per_actor) = c("affiliation", "id")

major_affiliation_per_actor = unlist(lapply(history_split, FUN = function(x){
  names(which.max(table(x$king)))
}))
major_affiliation_per_actor = data.table("id" = as.numeric(names(history_split)),"major_affiliation" = major_affiliation_per_actor)

actors$affiliation = NA
actors$affiliation = affiliation_per_actor$affiliation[match(actors$id,affiliation_per_actor$id)]
actors$affiliation = as.character(actors$affiliation)

# Add the affiliations of the emperors by hand
royal$full_name = actors$name[match(royal$id, actors$id)]
actors$affiliation[is.na(actors$affiliation)]= as.character(royal$king[match(actors$id[is.na(actors$affiliation)],royal$id)])
actors$major_affiliation = NA
actors$major_affiliation = major_affiliation_per_actor$major_affiliation[match(actors$id,major_affiliation_per_actor$id)]
actors$major_affiliation[is.na(actors$major_affiliation)]= as.character(royal$king[match(actors$id[is.na(actors$major_affiliation)],royal$id)])

missing_afil = actors$id[is.na(actors$major_affiliation)]
missing_df = data.frame("actor_id" = missing_afil)
missing_df$married_to = marriage$to[match(missing_df$actor_id,marriage$to)]
missing_df$married_to[is.na(missing_df$married_to)] = marriage$to[match(missing_df$actor_id[is.na(missing_df$married_to)],marriage$from)]
missing_df$affil_to = actors$major_affiliation[match(missing_df$married_to,actors$id)]
actors$major_affiliation[match(missing_df$actor_id,actors$id)] = actors$major_affiliation[match(missing_df$married_to,actors$id)]

actor_network = matrix(data = 0,nrow = nrow(actors),ncol = nrow(actors))
colnames(actor_network) = actors$id
rownames(actor_network) = actors$id

actor_network[cbind(match(children$f_m,colnames(actor_network)),match(children$child,colnames(actor_network)))] = 1
actor_network[cbind(match(marriage$from,colnames(actor_network)),match(marriage$to,colnames(actor_network)))] = 1
actor_network[cbind(match(marriage$to,colnames(actor_network)),match(marriage$from,colnames(actor_network)))] = 1

actor_network = network(actor_network, directed = F)

actor_network %v% "vertex.id" = actors$id
actor_network %v% "affiliation" = actors$major_affiliation
actor_network %v% "name" = as.character(actors$name)
actor_network %v% "birth_year" = actors$birth_year
actor_network %v% "death_year" = actors$death_year
actor_network %v% "sex" = as.character(actors$sex)

geodist_network = geodist(actor_network,count.paths = F)
geodist_table = table(as.numeric(geodist_network$gdist))
geodist_table = as.data.frame(geodist_table)
names(geodist_table) = c("dist", "count")
geodist_table$dist = as.character(geodist_table$dist)
geodist_table$dist[geodist_table$dist == "Inf"] = 80
geodist_table$dist = as.numeric(geodist_table$dist)

ggplot(data = geodist_table, aes(x = dist,y = count)) +
  geom_line() +
  geom_point() +
  theme_pubr() +
  scale_x_continuous(breaks = c(0,20,40,60,74), labels = c(0,20,40,60,"Inf"))


degreedist_network =as.data.frame(degreedist(actor_network))
names(degreedist_network) = "Count"
degreedist_network$Degree = 0:13

ggplot(data = degreedist_network, aes(x = Degree,y = Count)) +
  geom_line() +
  geom_point() +
  theme_pubr()


actor_network_comp = component.largest(actor_network,connected = "weak")
actors$largest_comp = actor_network_comp

edge_list = as.edgelist(actor_network)
edge_list = as.data.frame(edge_list)
names(edge_list) = c("Source", "Target")
write.csv(edge_list,file = "rdata/edge_list_all.csv",row.names = F)

actors_all = actors
actors_all$Id = match(actors_all$name,actor_network %v% "name")
actors_all$id = NULL
write.csv(actors_all,file = "rdata/actors_all.csv",row.names = F)

actors$is_king = actors$id %in% royal$id

save(actors, file = "rdata/actors.Rdata")


