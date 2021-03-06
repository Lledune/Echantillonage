
remove(list = ls())

data = load(file = "c:/users/lucien/desktop/sondage/Poivron_legoland.rda")

# Chargement des librairies
library(sampling)
library(samplingbook) # Si chargement du package n�cessaire : install.packages("samplingbook")
library(datasets)
library(pander)
library(knitr)
#ordonne la db en fonction de la province et enseigne
Poivron_Legoland = Poivron_Legoland[order(Poivron_Legoland$Province, Poivron_Legoland$Enseigne),]

#�chantillonage � deux degr�s, province et einseigne, pour la contrainte TODO
totProv = NULL #nord, centre, sud
for(i in levels(Poivron_Legoland$Province)){
  totProv = c(totProv, sum(Poivron_Legoland$Province == i))
}

totEns = NULL #grancub, ptirond, toupla
for(i in levels(Poivron_Legoland$Enseigne)){
  totEns = c(totEns, sum(Poivron_Legoland$Enseigne == i))
}

#Ne sachant pas si nous pouvons utiliser les totaux crois�s pour les provinces et les 
#enseignes, je me contente de faire les totaux respectifs de ceux-ci. 
#Nous pourrons les utiliser par la suite pour g�n�rer dans nos �chantillons les diff�rents.

#proportion pour �chantillonage
nsP = n/N*table(Poivron_Legoland$Province)
nsE = n/N*table(Poivron_Legoland$Enseigne) 

#dataTwoDegrees = mstage(Poivron_Legoland, stage = c("stratified", "stratified"), varnames = list("Province", "Enseigne"), size = list(c(25,25,25), c(25,25,25)), method = "srswr")

#Quel type de poivron est le plus consomm� � legoland ? 
#Pour r�pondre � cette question nous ne nous soucions pas des provinces ou des enseignes donc nous pouvons nous contenter
#d'un srswor 
#Nous allons tout de m�me v�rifier si les proportions sont biens respect�es puisque nous disposons des donn�es. 
#Dans le cas o� celles=ciu ne le seraient pas nous pourrons changer de plan de sondage pour r�pondre � cette question. 

set.seed(5)
#plan al�atoire sans remise
N = length(Poivron_Legoland$Province) #total pop
n = 19250
srs = srswor(n, N)
dataSrswor = Poivron_Legoland[srs == 1,]

#Fonction de cout d'un �chantillon : 
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

#V�rification que chaque province et chaque enseigne sont bien repr�sent�s. 
table(dataSrswor$Province, dataSrswor$Enseigne)/N

#calcul des proportions dans les variables Province et Enseigne de la db
propP = prop.table(table(Poivron_Legoland$Province))
propE = prop.table(table(Poivron_Legoland$Enseigne))

#calcul dans notre �chantillon 
propPe = prop.table(table(dataSrswor$Province))
propEe = prop.table(table(dataSrswor$Enseigne))

#affichage
pander(propP)
pander(propPe)
pander(propE)
pander(propEe)
pander(propP - propPe)
pander(propE - propEe)

#En effet il semble que les provinces soient bien repr�sent�es ainsi que les enseignes. 

#Nous pouvons maintenant nous int�resser � la question puisque l'�chantillon est constitu�. 
poivrons = 0
for(i in 4:6){
  poivrons[i-3] = sum(dataSrswor[,i])
}
poivrons = t(poivrons)
poivrons = as.data.frame(poivrons)
colnames(poivrons) = c("vert", "jaune", "rouge")
pander(poivrons)

#Nous voyons ici qu'avec 333k, les poivrons rouges sont largement les plus consomm�s. 
#Les deux autres types de poivrons sont quant � eux relativement similaires dans leur consommation. 161k et 163k
#Il semble que les poivrons rouges soient autant consomm�s que les deux autres vari�t�s r�unies. 







