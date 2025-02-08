
library(tidyverse)
library(igraph)
library(ggraph)
setwd("C:/Users/Bruno/Desktop/Advanced_Data_Science/Esame")

grafici=F  # if true print all graphics

#BACI 1999-------------------------------------------------------

baci<- read.csv("dataset/BACI_HS92_Y1999_V202401b.csv") # load raw data

baci_example <- baci[c(1:100,5994474:5994574),]

names(baci)<- c("year","fromCode","toCode","product","value","quantity") # rename variables

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))    #create a variable with sum of all export product from a country to another

baci99 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci99clean <- baci99 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci99clean <- baci99clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))


#eliminate geographic areas and region that are not countries
baci99clean <- baci99clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci99clean$to), unique(baci99clean$toName))
cbind(unique(baci99clean$from), unique(baci99clean$fromName))

rm(baci)
rm(baci_c)
rm(baci99)
baci99clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci99clean <- baci99clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)

top_exports99 <- baci99clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)

#control that only countries are in the dataset
length(unique(top_exports99$fromName))
length(unique(top_exports99$toName))

nodi_99 <- data.frame(cbind(name = unique(baci99clean$to))) 

long <- read.csv("dataset/country-coord.csv")
names(long)[c(3,5,6)]<- c("name", "latitude","longitude")

conti<- read.csv("dataset/continents.csv")
names(conti)[c(1,5)]<- c("cont","name")

conti <- conti %>% distinct(name, .keep_all = T)

nodi_99 <- nodi_99 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_99 <- nodi_99 %>%
  left_join(conti %>% select(name,cont), join_by(name))

export_99 <- data.frame(from = baci99clean$from, to = baci99clean$to, 
                        value= baci99clean$value)

g_99 = graph_from_data_frame(export_99, directed=T, vertices = nodi_99)

E(g_99)$weight= baci99clean$value

exportop5_99 <- data.frame(from = top_exports99$from, to = top_exports99$to, 
                           value= top_exports99$value)
nodi5_99 <- data.frame(name =unique(c( c(top_exports99$from),c(top_exports99$to)  ))) 
nodi5_99 <- nodi5_99 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi5_99 <- nodi5_99 %>%
  left_join(conti %>% select(name,cont), join_by(name))


g5_99 <- graph_from_data_frame(exportop5_99, directed=T, vertices = nodi5_99)

E(g5_99)$weight = E(g5_99)$value

node_pos_99 <- nodi5_99 %>% select(longitude, latitude) %>% rename(x = longitude, y = latitude)
lay <- create_layout(g5_99, 'manual', x=node_pos_99$x, y=node_pos_99$y)

if(grafici==TRUE){
  x11()
  ggraph(lay) + 
    geom_edge_link(aes(alpha=value), arrow = arrow(length = unit(4, 'mm'), type="closed"),
                   show.legend = F, end_cap = circle(10, 'mm'), # Fine degli archi si ferma al bordo del nodo
                   start_cap = circle(5, 'mm'))+
    geom_node_point( aes(color = cont, 
                         size =(degree(g5_99, mode = "in")),
                         alpha=(degree(g5_99, mode = "in"))), show.legend = F) +
    scale_size_continuous(range = c(7, 21), guide = F) +
    scale_alpha_continuous(range=c(0.5,1)) +
    geom_node_text(aes(label=nodi5_99$name)) +
    scale_edge_width(range = c(1, 6)) +
    scale_color_manual(values = c("yellow", "red","blue", "green", "pink", "grey"))
}

##BACI 2004 --------------------------------------


baci<- read.csv("dataset/BACI_HS92_Y2004_V202401b.csv")

names(baci)<- c("year","fromCode","toCode","product","value","quantity")

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))

baci04 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci04clean <- baci04 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci04clean <- baci04clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))

baci04clean <- baci04clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci04clean$to), unique(baci04clean$toName))
cbind(unique(baci04clean$from), unique(baci04clean$fromName))

rm(baci)
rm(baci_c)
rm(baci04)
baci04clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci04clean <- baci04clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)

top_exports04 <- baci04clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)

length(unique(top_exports04$fromName))
length(unique(top_exports04$toName))


nodi_04 <- data.frame(cbind(name = unique(baci04clean$to))) 

long <- read.csv("dataset/country-coord.csv")
names(long)[c(3,5,6)]<- c("name", "latitude","longitude")

conti<- read.csv("dataset/continents.csv")
names(conti)[c(1,5)]<- c("cont","name")

conti <- conti %>% distinct(name, .keep_all = T)

nodi_04 <- nodi_04 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_04 <- nodi_04 %>%
  left_join(conti %>% select(name,cont), join_by(name))



export_04 <- data.frame(from = baci04clean$from, to = baci04clean$to, 
                        value= baci04clean$value)

g_04 = graph_from_data_frame(export_04, directed=T, vertices = nodi_04)

E(g_04)$weight= baci04clean$value


#BACI 2009-------------------------------------------------------


baci<- read.csv("dataset/BACI_HS07_Y2009_V202401b.csv")

names(baci)<- c("year","fromCode","toCode","product","value","quantity")

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))

baci09 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci09clean <- baci09 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci09clean <- baci09clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))

baci09clean <- baci09clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci09clean$to), unique(baci09clean$toName))
cbind(unique(baci09clean$from), unique(baci09clean$fromName))

rm(baci)
rm(baci_c)
rm(baci09)
baci09clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci09clean <- baci09clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)

top_exports09 <- baci09clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)

length(unique(top_exports09$fromName))
length(unique(top_exports09$toName))


nodi_09 <- data.frame(cbind(name = unique(baci09clean$to))) 

long <- read.csv("dataset/country-coord.csv")
names(long)[c(3,5,6)]<- c("name", "latitude","longitude")

conti<- read.csv("dataset/continents.csv")
names(conti)[c(1,5)]<- c("cont","name")

conti <- conti %>% distinct(name, .keep_all = T)

nodi_09 <- nodi_09 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_09 <- nodi_09 %>%
  left_join(conti %>% select(name,cont), join_by(name))



export_09 <- data.frame(from = baci09clean$from, to = baci09clean$to, 
                        value= baci09clean$value)

g_09 = graph_from_data_frame(export_09, directed=T, vertices = nodi_09)

E(g_09)$weight= baci09clean$value


##BACI 2014 --------------------------------------


baci<- read.csv("dataset/BACI_HS92_Y2014_V202401b.csv")

names(baci)<- c("year","fromCode","toCode","product","value","quantity")

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))

baci14 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci14clean <- baci14 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci14clean <- baci14clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))

baci14clean <- baci14clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci14clean$to), unique(baci14clean$toName))
cbind(unique(baci14clean$from), unique(baci14clean$fromName))

rm(baci)
rm(baci_c)
rm(baci14)
baci14clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci14clean <- baci14clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)

top_exports14 <- baci14clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)

length(unique(top_exports14$fromName))
length(unique(top_exports14$toName))




nodi_14 <- data.frame(cbind(name = unique(baci14clean$to))) 

long <- read.csv("dataset/country-coord.csv")
names(long)[c(3,5,6)]<- c("name", "latitude","longitude")

conti<- read.csv("dataset/continents.csv")
names(conti)[c(1,5)]<- c("cont","name")

conti <- conti %>% distinct(name, .keep_all = T)

nodi_14 <- nodi_14 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_14 <- nodi_14 %>%
  left_join(conti %>% select(name,cont), join_by(name))



export_14 <- data.frame(from = baci14clean$from, to = baci14clean$to, 
                        value= baci14clean$value)

g_14 = graph_from_data_frame(export_14, directed=T, vertices = nodi_14)

E(g_14)$weight= baci14clean$value

### BACI 2019 ----------------------------------------

baci<- read.csv("dataset/BACI_HS17_Y2019_V202401b.csv")

names(baci)<- c("year","fromCode","toCode","product","value","quantity")

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))


baci19 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci19clean <- baci19 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci19clean <- baci19clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))

baci19clean <- baci19clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci19clean$to), unique(baci19clean$toName))
cbind(unique(baci19clean$from), unique(baci19clean$fromName))

rm(baci)
rm(baci_c)
rm(baci19)

baci19clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci19clean <- baci19clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)



top_exports19 <- baci19clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)

length(unique(top_exports19$fromName))
length(unique(top_exports19$toName))

nodi_19 <- data.frame(cbind(name = unique(baci19clean$to))) 

nodi_19 <- nodi_19 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_19 <- nodi_19 %>%
  left_join(conti %>% select(name,cont), join_by(name))

export_19 <- data.frame(from = baci19clean$from, to = baci19clean$to, 
                        value= baci19clean$value)

g_19 = graph_from_data_frame(export_19, directed=T, vertices = nodi_19)

E(g_19)$weight= baci19clean$value




# BACI 2023 ----------------------------------

baci<- read.csv("dataset/BACI_HS22_Y2023_V202501.csv")

names(baci)<- c("year","fromCode","toCode","product","value","quantity")

baci <- baci %>% group_by(fromCode) %>%
  mutate(valuetot = sum(value))

baci23 <- baci %>%
  group_by(fromCode, toCode, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

baci_c<- read.csv("dataset/BACI_CCODES.csv")
names(baci_c)<- c("fromCode","fromName", "iso2", "from")

baci23clean <- baci23 %>%
  left_join(baci_c %>% select(from, fromCode, fromName), join_by(fromCode))

names(baci_c)<- c("toCode","toName", "iso2", "to")

baci23clean <- baci23clean %>%
  left_join(baci_c %>% select(to, toCode, toName), join_by(toCode))

baci23clean <- baci23clean %>%
  filter(!to %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                    "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                    "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                    "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                    "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                    "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                    "GIB","VCT", "BES"
  )) %>%
  filter(!from %in% c("W00", "O19", "A79", "A59", "X1", "UMI", "HMD", 
                      "ATF","SGS","PYF","GRL", "MAC", "S19", "MNP", "_X ",
                      "CYM", "E19", "F19", "MHL", "TCA", "CUW", "SXM", "X2",
                      "ATA", "CXR", "FLK", "NFK", "ESH", "X1 ", "WLF", "FRO",
                      "XX ", "UMI", "HKG", "IOT", "SPM", "BLM", "BVT", "TKL",
                      "SHN", "PCN", "NIU", "NCL", "COK", "CCK", "X2 ","ABW","MSR", "STP",
                      "GIB","VCT", "BES"
  )) 

cbind(unique(baci23clean$to), unique(baci23clean$toName))
cbind(unique(baci23clean$from), unique(baci23clean$fromName))

rm(baci)
rm(baci_c)
rm(baci23)

baci23clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))

baci23clean <- baci23clean %>%
  group_by(from) %>%
  mutate(totalExp = sum(value), relValue = value/totalExp)

top_exports23 <- baci23clean %>%
  group_by(from) %>%
  slice_max(value, n = 5)


length(unique(top_exports23$fromName))
length(unique(top_exports23$toName))


nodi_23 <- data.frame(cbind(name = unique(baci23clean$to))) 

long <- read.csv("dataset/country-coord.csv")
names(long)[c(3,5,6)]<- c("name", "latitude","longitude")

conti<- read.csv("dataset/continents.csv")
names(conti)[c(1,5)]<- c("cont","name")

conti <- conti %>% distinct(name, .keep_all = T)

nodi_23 <- nodi_23 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi_23 <- nodi_23 %>%
  left_join(conti %>% select(name,cont), join_by(name))



export_23 <- data.frame(from = baci23clean$from, to = baci23clean$to, 
                        value= baci23clean$value)

g_23 = graph_from_data_frame(export_23, directed=T, vertices = nodi_23)

E(g_23)$weight= baci23clean$value


exportop5_23 <- data.frame(from = top_exports23$from, to = top_exports23$to, 
                           value= top_exports23$value)
nodi5_23 <- data.frame(name =unique(c( c(top_exports23$from),c(top_exports23$to)  ))) 
nodi5_23 <- nodi5_23 %>%
  left_join(long %>% select(name, latitude, longitude), join_by(name))

nodi5_23 <- nodi5_23 %>%
  left_join(conti %>% select(name,cont), join_by(name))


g5_23 <- graph_from_data_frame(exportop5_23, directed=T, vertices = nodi5_23)

E(g5_23)$weight <- E(g5_23)$value
node_pos23 <- nodi5_23 %>% select(longitude, latitude) %>% rename(x = longitude, y = latitude)
lay <- create_layout(g5_23, 'manual', x=node_pos23$x, y=node_pos23$y)


if(grafici==TRUE){

  x11()
  ggraph(lay) + 
    geom_edge_link(edge_alpha=0.2,arrow = arrow(length = unit(2, 'mm'), type="closed"), end_cap= circle(5 ,"mm"), show.legend = NA) + #, arrow = arrow(length = unit(4, 'mm'), type="closed"), show.legend = F, end_cap = circle(10, 'mm'),  start_cap = circle(5, 'mm'))+
    geom_node_point( aes(fill=cont,size =(strength(g5_23, mode = "all"))),alpha=0.8, 
                     color = "black", shape=21,show.legend = T) +
    scale_size_continuous(range = c(6, 25), guide = F) +
    geom_node_text(aes(label=nodi5_23$name)) +
    scale_edge_width(range = c(1, 6), guide=F) +
    scale_fill_manual(values = c("yellow", "red","blue", "green", "white"
    , "darkviolet"), name="Continent")   +
    ggtitle("Mappa dei paesi come network geografico, maggiori 5 esportazioni per ogni stato") + 
    theme(legend.text = element_text(size = 16),  # Ingrandisce il testo della legenda
          legend.title = element_text(size = 18),
          plot.title = element_text(size = 23, face = "bold", hjust = 0.5))+
    guides(fill = guide_legend(override.aes = list(size = 8)))
  
  
  x11()
  set.seed(5)
  ggraph(g5_23, layout = 'fr', weights = log(E(g5_23)$value)) +  # Force-directed layout
    geom_edge_link(alpha=0.2, arrow = NULL,show.legend = F)+            # Draw edges
    geom_node_point(alpha=.8, aes(fill=cont,size =(strength(g5_23, mode = "all"))), 
                     color = "black", shape=21,show.legend = T) +  # Draw nodes
    scale_size_continuous(range = c(6, 25), guide = F) +
    geom_node_text(aes(label = nodi5_23$name))+  # Add node labels
    scale_fill_manual(values = c("yellow", "red","blue", "green", "white"
                                   , "darkviolet"), name="Continent")   +
    ggtitle("Mappa dei paesi nel 2023 come network force oriented, maggiori 5 esportazioni per ogni stato") + 
    theme(legend.text = element_text(size = 16),  # Ingrandisce il testo della legenda
          legend.title = element_text(size = 18),
          plot.title = element_text(size = 23, face = "bold", hjust = 0.5))+
    guides(fill = guide_legend(override.aes = list(size = 8)))
  

  
  }


### Calcolo centralità basata sul degree rete totale---------------


indegree <- degree(g5_23, mode = "in")
print("Indegree:")
indegree[order(indegree, decreasing = T)]

# Outdegree for all nodes
outdegree <- degree(g_23, mode = "out")
print("Outdegree:")
outdegree[order(outdegree, decreasing = T)]


totaldegree <- degree(g_23, mode = "total")
print("Totaldegree:")

totaldegree[order(totaldegree, decreasing = T)]


#centralità pesata per il grado del nodo normalizzato
#round((strength(g_23, mode="out"))[order(strength(g, mode="out"))]/max((strength(g, mode="out"))),3)
#round((strength(g_23, mode="in"))[order(strength(g, mode="in"))]/max((strength(g, mode="in"))),3)
round((strength(g_23, mode="all"))[order(strength(g_23, mode="all"), decreasing = T)]/max((strength(g_23, mode="all"))),3)[1:10]



baci23clean %>% 
  distinct(fromName, totalExp) %>%
  arrange(desc(totalExp))


baci23clean %>%
  group_by(to) %>%
  mutate(totalImp = sum(value), relValueImp = value/totalImp) %>%
  distinct(toName, totalImp) %>%
  arrange(desc(totalImp))


(betweenness(g5_23)[order(betweenness(g5_23), decreasing = T)])[1:10]
(betweenness(g_23)[order(betweenness(g_23), decreasing = T)])[1:10]
#meglio bet su grafo ridotto

damp <- 0.85

page_rank(g_23)$vector[order(page_rank(g_23,damping=damp)$vector, decreasing = T)][1:10]

eigen_centrality(g_23)$vector[order(eigen_centrality(g_23)$vector,decreasing = T)][1:10]
eigen_centrality(g5_23)$vector[order(eigen_centrality(g5_23)$vector,decreasing = T)][1:10]

is_connected(g_23)
is_connected(g5_23)

alfa2<- 1/ max(abs(eigen((diag(1/strength(g_23, mode="all")))%*%as_adjacency_matrix(g_23, attr="weight"))$values))

### COMMUNITY DETECTION ---------------------


ceigen <-cluster_leading_eigen(as_undirected(g5_23, mode="collapse"))
modularity(ceigen)
nodi5_23$com_eigen <- ceigen$membership

clouv <-cluster_louvain(as_undirected(g5_23, mode="collapse"))
modularity(clouv)
nodi5_23$com_lou <- clouv$membership

cwalk <-cluster_walktrap(g5_23, steps=5)
modularity(cwalk)
nodi5_23$com_walk <- cwalk$membership






custom_colors <- c("red", "blue", "green", "purple", "brown","lightblue","yellow")
if(grafici==TRUE){
  x11()
  ggraph(lay) + 
    geom_edge_link(edge_alpha=0.2,arrow = arrow(length = unit(2, 'mm'), type="closed"), end_cap= circle(5 ,"mm"), show.legend = NA) + #, arrow = arrow(length = unit(4, 'mm'), type="closed"), show.legend = F, end_cap = circle(10, 'mm'),  start_cap = circle(5, 'mm'))+
    geom_node_point( aes(fill=as.factor(nodi5_23$com_lou),size =(strength(g5_23, mode = "all"))),alpha=0.8, 
                     color = "black", shape=21,show.legend = F) +
    scale_size_continuous(range = c(6, 25), guide = F) +
    geom_node_text(aes(label=nodi5_23$name)) +
    scale_edge_width(range = c(1, 6), guide=F) +
    scale_fill_manual(values = c("yellow", "red","blue","darkviolet","green","pink","darkgreen","black","white","orange","brown"
                                ), guide=F)   +
    ggtitle("Mappa delle communità, maggiori 5 esportazioni per ogni stato") + 
    theme(legend.text = element_text(size = 16),  # Ingrandisce il testo della legenda
          legend.title = element_text(size = 18),
          plot.title = element_text(size = 23, face = "bold", hjust = 0.5))+
    guides(fill = guide_legend(override.aes = list(size = 8)))
  
  
   x11()
  ggraph(lay) + 
    geom_edge_link(aes(alpha = value), 
                   arrow = arrow(length = unit(4, 'mm'), type = "closed"),
                   show.legend = F, 
                   end_cap = circle(10, 'mm'), 
                   start_cap = circle(5, 'mm')) +
    geom_node_point(aes(color = as.factor(nodi5_23$com_walk)),size=10, 
                    show.legend = T)  +
    geom_node_text(aes(label = nodi5_23$name)) +
    scale_edge_width(range = c(1, 6)) +#
    scale_color_manual(values =custom_colors )+
    theme(
      legend.text = element_text(size = 14),  # Ingrandisce il testo della legenda
      legend.title = element_text(size = 16)  # Ingrandisce il titolo della legenda
    )# Usa colori distinti per comunità
  
  
  c25_23<- cluster_louvain(as.undirected(g5_23, mode="collapse"))
  modularity(c25_23)
  c25_23$membership
  nodi5_23$com_lou<-c25_23$membership
}

gc()

modularity(g5_23, membership= as.factor(nodi5_23$cont), directed = T,
           weights=E(g5_23)$weight)



### POWER --------------

power = function(A, t) {
  n = dim(A)[1];
  # x_2k
  x0 = rep(0, n);
  # x_2k+1
  x1 = rep(1, n);
  # x_2k+2
  x2 = rep(1, n);
  diff = 1
  eps = 1/10^t;
  iter = 0;
  while (diff > eps) {
    x0 = x1;
    x1 = x2;
    x2 = (1/x2) %*% A;
    diff = sum(abs(x2 - x0));
    iter = iter + 1;
  } 
  # it holds now: alpha x2 = (1/x2) A
  alpha = ((1/x2) %*% A[,1]) / x2[1];
  # hence sqrt(alpha) * x2 = (1/(sqrt(alpha) * x2)) A
  x2 = sqrt(alpha) %*% x2;
  return(list(vector = as.vector(x2), iter = iter))
}  



### RESILIENCE --------------

# Function to simulate node removal
simulate_resilience <- function(graph, strategy, fraction_steps = seq(1, vcount(graph)-1, by = 1)) {
  results <- data.frame(fraction_removed = numeric(),
                        lcc_size = numeric(),
                        strategy = character())
  
  original_size <- vcount(graph)
  
  for (f in fraction_steps) {
    # Number of nodes to remove
    nodes_to_remove <- f
    # Select nodes to remove based on strategy
    if (strategy == "random") {
      nodes <- sample(V(graph), nodes_to_remove)
    } else if (strategy == "strength") {
      nodes <- order(-strength(graph, mode="all"))[1:nodes_to_remove]
    } else if (strategy == "betweenness") {
      nodes <- order(-betweenness(graph, weights=1/E(graph)$weigth))[1:nodes_to_remove]
    } else if (strategy == "Page-Rank") {
      nodes <- order(-page_rank(graph, weights=E(graph)$weigth)$vector)[1:nodes_to_remove]
    } else if (strategy == "in-degree") {
      nodes <- order(-degree(graph, mode="in"))[1:nodes_to_remove]
    }
    

    
    
    # Remove nodes
    graph_removed <- delete_vertices(graph, nodes)
    
    
    
    
    # Compute the size of the largest connected component
    components <- components(graph_removed)
    lcc_size <- max(components$csize, na.rm = TRUE)
    conness<- is_connected(graph_removed)
     #Store results
    results <- rbind(results, data.frame(fraction_removed =  f,
                                         lcc_size = lcc_size,
                                         connesso = conness,
                                         strategy = strategy))
  }
  
  return(results)
}


# Simulate resilience testing
strategies <- c("random", "strength", "betweenness", "Page-Rank", "in-degree")
all_results <- data.frame()

for (strategy in strategies) {
  results <- simulate_resilience(g_23, strategy)
  all_results <- rbind(all_results, results)
}





if(grafici==T){
x11()
ggplot(all_results, aes(x = fraction_removed/vcount(g_23), y = lcc_size/vcount(g_23), color = strategy)) +
  geom_line(linewidth=1.5) +
  geom_hline(yintercept = .50, color = "black", linetype = "dashed", size = 1)+
  geom_vline(xintercept =.50, color = "black", linetype = "dashed", size = 1)+
  labs(title = "Network Resilience under Different Removal Strategies",
       x = "Fraction of Nodes Removed",
       y = "Largest Connected Component Size",
       color = "Strategy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  
    axis.title.x = element_text(size = 18),  #
    axis.title.y = element_text(size = 18),  
    axis.text = element_text(size = 16),  
    legend.title = element_text(size = 18),  
    legend.text = element_text(size = 16))  
}

#### END & SAVE RESULTS-----------

save(list=ls(), file = "analisi3.Rdata")


#load("analisi3.Rdata")





