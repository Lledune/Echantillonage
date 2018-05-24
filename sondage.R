
remove(list = ls())

data = load(file = "c:/users/lucien/desktop/sondage/Poivron_legoland.rda")

# Chargement des librairies
library(sampling)
library(samplingbook) # Si chargement du package nécessaire : install.packages("samplingbook")
library(datasets)
library(pander)
library(knitr)
#ordonne la db en fonction de la province et enseigne
Poivron_Legoland = Poivron_Legoland[order(Poivron_Legoland$Province, Poivron_Legoland$Enseigne),]

#échantillonage à deux degrès, province et einseigne, pour la contrainte TODO
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
#Nous allons tout de même vérifier si les proportions sont biens respectées puisque nous disposons des données. 
#Dans le cas où celles=ciu ne le seraient pas nous pourrons changer de plan de sondage pour répondre à cette question. 

set.seed(5)
#plan aléatoire sans remise
N = length(Poivron_Legoland$Province) #total pop
n = 19250
srs = srswor(n, N)
dataSrswor = Poivron_Legoland[srs == 1,]

#Fonction de cout d'un échantillon : 
cost = function(tab){
  cost = 0
  levels = levels(Poivron_Legoland$Enseigne)
  for(i in 1:length(tab[,1])){
    if(tab[i,2] == levels[1]){
      cost = cost + 1 
    }else if(tab[i,2] == levels[2]){
      cost = cost + 1.2
    }else{
      cost = cost + 1.5
    }
  }  
  return(cost*1.05)
}
cost(dataSrswor) #24913$

#Vérification que chaque province et chaque enseigne sont bien représentés. 
table(dataSrswor$Province, dataSrswor$Enseigne)/N

#calcul des proportions dans les variables Province et Enseigne de la db
propP = prop.table(table(Poivron_Legoland$Province))
propE = prop.table(table(Poivron_Legoland$Enseigne))

#calcul dans notre échantillon 
propPe = prop.table(table(dataSrswor$Province))
propEe = prop.table(table(dataSrswor$Enseigne))

#affichage
pander(propP)
pander(propPe)
pander(propE)
pander(propEe)
pander(propP - propPe)
pander(propE - propEe)

#En effet il semble que les provinces soient bien représentées ainsi que les enseignes. 

#Nous pouvons maintenant nous intéresser à la question puisque l'échantillon est constitué. 
poivrons = 0
for(i in 4:6){
  poivrons[i-3] = sum(dataSrswor[,i])
}
poivrons = t(poivrons)
poivrons = as.data.frame(poivrons)
colnames(poivrons) = c("vert", "jaune", "rouge")
pander(poivrons)

#Nous voyons ici qu'avec 333k, les poivrons rouges sont largement les plus consommés. 
#Les deux autres types de poivrons sont quant à eux relativement similaires dans leur consommation. 161k et 163k
#Il semble que les poivrons rouges soient autant consommés que les deux autres variétés réunies. 







