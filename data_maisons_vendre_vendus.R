
#***************************************************************************************************
##                             Les jeux de données data_maisons_vendreet data_maisons_vendus
#*                                      R studio 
#**************************************************************************************************/



#***************************************************************************************************
##  Question 1:                                                                                    *
##  Veuillez importer les 2 tables de données data_maisons_vendre.txt et data_maisons_vendus.txt   *
#***************************************************************************************************/
M_vendre <- read.delim("https://zonecours2.hec.ca/access/content/group/MATH30602.H2022/Version%20corrig%C3%A9e/data/data_maisons_vendre.txt")
head(M_vendre)


M_vendre=read.table(choose.files(),header = T, sep = ";")
M_vendus=read.table(choose.files(data_maisons_vendus),header = T, sep = "\t")

# ou bien:
setwd("C:/Users/gita/Desktop/HEC_COURS/ZCH21/ATL2021/BD")

M_vendre=read.table("data_maisons_vendre.txt",header = T, sep = ";")
M_vendus=read.table("data_maisons_vendus.txt",header = T, sep = "\t")


#***************************************************************************************************
##  Question 2:                                                                                    *
##  Veuillez vous familiariser avec les données                                                    *
#***************************************************************************************************/
head(M_vendre)
head(M_vendus)

summary(M_vendre)
summary(M_vendus)




#***************************************************************************************************
##Pour ce qui est de la structure:                   *
#***************************************************************************************************/
str(M_vendre)
str(M_vendus)



#***************************************************************************************************
##Y a-t-il des différences entre les 2 tables?                                                     *
#***************************************************************************************************/
# Il y a une difference au niveau de la colonne numero_id qui est maison_id dans data_maisons_vendus       *
# Deplus, la colonne jardin est en factor dans data_maison_vendus alors qu'il serait mieux que ce          *
#soit de type vector numerique pour qu'on soit capable d'avoir la proportion des maisons avec jardin       *
# De plus il y a des observations 'Zero' qu'il serait mieux d'avoir '0'.                                   *
#La colonne date dans les deux data frame est en format factor, ce serait mieux de l'avoir en format Date. *


#***************************************************************************************************
# Question 3:                                                                                        *
# Veuillez afficher les différentes valeurs contenues de la colonne «jardin» de la table de données  *
#«data_maisons_vendus».                                                                              *
#***************************************************************************************************/

summary(M_vendus$jardin)
M_vendus$jardin=ifelse(M_vendus$jardin=="zero",0,as.numeric(as.character(M_vendus$jardin)))


summary(M_vendus$jardin)



#***************************************************************************************************
# Question 4:                                                                                      *
# Veuillez ajouter le suffixe «_vendre» pour chaque nom de colonne                                 *
#***************************************************************************************************/


names(M_vendre)=paste(names(M_vendre),"vendre",sep="_")
names(M_vendus)=paste(names(M_vendus),"vendus",sep="_")

#***************************************************************************************************
# Question 5:                                                                                      *
# Veuillez effectuer une jointure complète (full join)                                             *
# Combien y a-t-il de colonnes et d'observations dans cette nouvelle table de  données?            *
#***************************************************************************************************/

data_all_maisons=merge(x=M_vendre,y=M_vendus,by.x ="numero_id_vendre",by.y = "maisons_id_vendus",all = TRUE )
ncol(data_all_maisons) #11
nrow(data_all_maisons) #1600




#***************************************************************************************************
# Question 6:                                                                                      *
# Dans une nouvelle table de données que vous appellerez «data_maisons_vendre_vendus»,              * 
# Veuillez extraire toutes les maisons qui ont été à vendre en 2015 et qui ont été vendues en 2016 *                         *
# D'utiliser la fonction is.na().                                                                  *
#***************************************************************************************************/

data_maisons_vendre_vendus=data_all_maisons[!(is.na(data_all_maisons$prix_vendus)| is.na(data_all_maisons$prix_vendre)),]

sum(is.na(data_maisons_vendre_vendus$jardin.y))


nrow(data_maisons_vendre_vendus) # 399 rows

# ou bien:

data_maisons_vendre_vendus=merge(x=M_vendre,y=M_vendus,by.x ="numero_id_vendre",by.y = "maisons_id_vendus")
nrow(data_maisons_vendre_vendus) # 399 rows



#***************************************************************************************************
# Question 7:                                                                                      *
# Combien de maisons avec un jardin ont été à vendre puis vendues?                                 *
#***************************************************************************************************/


nrow(data_maisons_vendre_vendus[data_maisons_vendre_vendus$jardin_vendre==1,])
# 311



#***************************************************************************************************
# Question 8:                                                                                      *
# afficher le prix de vente et le prix vendu pour la maison ayant comme numéro d'identifiant le    *
#tr962269?                                                                                         *
#***************************************************************************************************/

data_maisons_vendre_vendus[data_maisons_vendre_vendus$numero_id_vendre=="tr962269", c("prix_vendre","prix_vendus")]




#***************************************************************************************************
# Question 9:                                                                                      *
# Veuillez créer dans la table «data_maisons_vendre_vendus» la variable «difference» qui sera la   *
# différence entre le prix de vente affiché et le prix de vente réel                               *
#***************************************************************************************************/

data_maisons_vendre_vendus$difference=data_maisons_vendre_vendus$prix_vendre-data_maisons_vendre_vendus$prix_vendus


#***************************************************************************************************
# Question 10:                                                                                     *
# veuillez afficher la moyenne, la médiane ainsi que l'écart-type de nos observations.             *
#***************************************************************************************************/
mean(data_maisons_vendre_vendus$difference)
median(data_maisons_vendre_vendus$difference)
sd(data_maisons_vendre_vendus$difference)


#***************************************************************************************************
# Question 11:                                                                                     *
# Créer les deux variables suivantes:                                                              *
#rta: Les trois premiers caractères du code postal de la maison.                                   *
#type: Le type de la maison; si cette maison est une maison, un duplex ou un triplex.              *
#***************************************************************************************************/
data_maisons_vendre_vendus$rta=substr(data_maisons_vendre_vendus$code_postal_vendre,1,3)
data_maisons_vendre_vendus$type=substr(data_maisons_vendre_vendus$numero_id_vendre,1,2)


#***************************************************************************************************
# Question 12   :                                                                                  *
# Supprimer les colonnes suivantes de notre table de données «data_maisons_vendre_vendus»          *
#nbr_pieces_vendus, jardin_vendus, code_postal_vendus, date_poste_vendus                           *
#***************************************************************************************************/
data_maisons_vendre_vendus$nbr_pieces_vendus=NULL
data_maisons_vendre_vendus$jardin_vendus=NULL
data_maisons_vendre_vendus$code_postal_vendus=NULL
data_maisons_vendre_vendus$date_poste_vendus=NULL



#***************************************************************************************************
# Question 13   :                                                                                  *
# Découvrir un autre type de données                                                               *
# En utilisant la fonction as.Date()                                                               *
#***************************************************************************************************/
data_maisons_vendre_vendus$date_poste_vendre=as.Date(data_maisons_vendre_vendus$date_poste_vendre,"%Y/%m/%d")
summary(data_maisons_vendre_vendus$date_poste_vendre)



#***************************************************************************************************
# Question 14   :                                                                                  *
# Pour la colonne prix_vendus, la colonne prix_vendre et la colonne difference                     *
# Veuillez afficher la moyenne en groupant par le type de maison.                                  *
# Quel est le type ayant la plus grande valeur sur le marché?                                      *
#***************************************************************************************************/
aggregate(data_maisons_vendre_vendus[,c("prix_vendus","prix_vendre","difference")],by=list(data_maisons_vendre_vendus$type),FUN="mean")


#***************************************************************************************************
# Question 15   :                                                                                  *
# Veuillez créer une fonction                                                                      *
#***************************************************************************************************/
agg_fct=function(data_x,x,mesure){
  list_resultat=list()
  list_resultat[[1]]=aggregate(data_x[,c("prix_vendus","prix_vendre","difference")],by=list(data_maisons_vendre_vendus[,x]),FUN=mesure)
  list_unique_group=unique(data_maisons_vendre_vendus[,x])
  for(i in 1:length(list_unique_group))
  {
    pct=sum(data_maisons_vendre_vendus[,x]==list_unique_group[i])/nrow(data_x)
    list_resultat[[i+1]]=paste("Le pourcentage de ",list_unique_group[i], "dans la variable ",x, "est de ",round(pct*100,2),"%")
  }
  list_resultat
}

fonction_1=agg_fct(data_maisons_vendre_vendus,"type","mean")
fonction_1

fonction_2=agg_fct(data_maisons_vendre_vendus,"jardin_vendre","median")
fonction_2


#***************************************************************************************************
# Question 16   :                                                                                  *
# Veuillez créer une fonction qui va extraire toutes les observations d'une table de données avec  *
#seulement toutes les colonnes de types numériques.                                                *
#***************************************************************************************************/


take_num=function(data_x){
  data_x[,names(data_x)[sapply(data_x,is.numeric)]]
}

essaie_1=take_num(M_vendre)
str(essaie_1)

essaie_2=take_num(data_maisons_vendre_vendus)
str(essaie_2)
