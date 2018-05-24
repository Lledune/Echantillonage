
remove(list = ls())

data = load(file = "c:/users/lucien/desktop/sondage/Poivron_legoland.rda")

# Chargement des librairies
library(sampling)
library(samplingbook) # Si chargement du package nécessaire : install.packages("samplingbook")
library(datasets)

#ordonne la db en fonction de la province et enseigne
Poivron_Legoland = Poivron_Legoland[order(Poivron_Legoland$Province, Poivron_Legoland$Enseigne),]

#plan aléatoire sans remise
N = length(Poivron_Legoland$Province) #total pop
n = 15000
srs = srswor(n, N)
dataSrswor = Poivron_Legoland[srs,]

#échantillonage à deux degrès, province et einseigne, pour la contrainte 
totProv = NULL #nord, centre, sud
for(i in levels(Poivron_Legoland$Province)){
  totProv = c(totProv, sum(Poivron_Legoland$Province == i))
}

totEns = NULL #grancub, ptirond, toupla
for(i in levels(Poivron_Legoland$Enseigne)){
  totEns = c(totEns, sum(Poivron_Legoland$Enseigne == i))
}

#Ne sachant pas si nous pouvons utiliser les totaux croisés pour les provinces et les 
#enseignes, je me contente de faire les totaux respectifs de ceux-ci. 
#Nous pourrons les utiliser par la suite pour générer dans nos échantillons les différents.

#proportion pour échantillonage
nsP = n/N*table(Poivron_Legoland$Province)
nsE = n/N*table(Poivron_Legoland$Enseigne) 

#dataTwoDegrees = mstage(Poivron_Legoland, stage = c("stratified", "stratified"), varnames = list("Province", "Enseigne"), size = list(c(25,25,25), c(25,25,25)), method = "srswr")

#Quel type de poivron est le plus consommé à legoland ? 
#Pour répondre à cette question nous ne nous soucions pas des provinces ou des enseignes donc nous pouvons nous contenter
#d'un srswor 

#Fonction de cout d'un échantillon : 
cost = function(tab){
  cost = 0
  levels = levels(Poivron_Legoland$Enseigne)
  for(i in length(tab[,1])){
    if(tab[i,2] == levels[1]){
      cost = cost + 1 
    }else if(tab[i,2] == levels[2]){
      cost = cost + 1.2
    }else{
      cost = cost + 1.5
    }
  }  
  return(cost)
}
cost(dataSrswor)
cost(Poivron_Legoland)







