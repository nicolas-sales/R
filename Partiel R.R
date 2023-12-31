#1.1
data=read.csv(choose.files(), sep=";")
View(data)

#1.2 Caract�res qualitatifs:

#M.tier
tab=table(data$M.tier) # R�pertorie le nombre de personnes par m�tiers/ tableau de contingence
tab

summary(data$M.tier) # Il y a 467 variables m�tier

prop.table(table(data$M.tier)) # Donne la proportion de personnes par m�tiers

freq=round(prop.table(tab)*100,1) # Donne la fr�quence de personnes par m�tiers
freq

mean(tab) # La moyenne de personnes par m�tiers est de 5,69
var(tab) # La variance est de 0,31
sd(tab) # L'�cart type est de 0,55
median(tab) # La mediane est de 81
min(tab) # Le minimum de personne est de 65
max(tab) # Le maximum de personne est de 82
summary(tab) # Il y a 467 variables M.tier

boxplot(tab) # Il n'y a pas de valeurs aberrantes 
plot(tab , col="RED") # Diagramme baton/tuyau d'orgue 
pie(table(data$M.tier), col = rainbow(100)) # Diagramme camembert 

#Grand.secteur

tab=table(data$Grand.secteur) # R�pertorie le nombre de personnes par secteur d'activit�
tab

summary(data$Grand.secteur) # Il y a 467 variables Grand.secteur

prop.table(table(data$Grand.secteur)) # Donne la proportion de personnes par secteurs d'activit�

freq=round(prop.table(tab)*100,1) # Donne la fr�quence de personnes par secteurs d'activit�
freq

mean(tab) # La moyenne de personnes par secteurs d'activit� est de 77,83
var(tab) # La variance est de 44,57
sd(tab) # L'�cart type est de 6,68
summary(tab) # Il y a 467 variables Grand.secteur

boxplot(tab) # Il y a une valeur aberrante qui est 65 
plot(tab , col="RED") # Diagramme baton/tuyau d'orgue
pie(table(data$M.tier), col = rainbow(100)) # Diagramme camembert

# 1.3 Caract�res quantitatifs

# Nombre.de.projets

x=data$Nombre.de.projets
x

mean(x) # La moyenne du nombre de projet est de 5766,852
var(x) # La variance est de 306177187
median(x) # La mediane est de 540
sd(x) # L'�cart type est de 17497,92
quantile(x) # Le nombre de projets � 25% est de 90, � 50% est de 540, � 75% est de 3045, � 100% est de 157010
min(x) # Le minimum de projet est de 0
max(x) # Le maximum de projet est de 157010
summary(x) # La valeur minimale est de 0, le nombre de projet � 25% est de 90, ma mediane est de 540, la moyenne est de 5767, le nombre de projet � 75% est de 3045, le nombre maximum de projet est de 157010

boxplot(x) # Il y a des valeurs aberrantes en partie sup�rieure 
barplot(x) # diagramme en barre
plot(x) # nuage de points en fonction de la fr�quence
hist(x) # histogramme en fonction de la fr�quence

# Nombre.de.projets.difficile

x=data$Nombre.de.projets.difficile
x

mean(x) # La moyenne du nombre de projet est de 2891,37
var(x) # La variance est de 75877611
median(x) # La mediane est de 280
sd(x) # L'�cart type est de 8710,776
quantile(x) # Le nombre de projets difficile � 25% est de 40, � 50% est de 280, � 75% est de 1700, � 100% est de 78080
min(x) # Le minimum de projet difficile est de 0
max(x) # Le maximum de projet difficile est de 78080
summary(x) # La valeur minimale est de 0, le nombre de projet difficile � 25% est de 40, la mediane est de 280, la moyenne est de 2891, le nombre de projet difficile � 75% est de 1700, le nombre maximum de projet difficile est de 78080

boxplot(x) # Il y a des valeurs aberrantes en partie sup�rieure
barplot(x) # diagramme en barre
plot(x) # nuage de points 
hist(x) # histogramme 



# Nombre.de.projets.saisonniers

x=data$Nombre.de.projets.saisonniers
x

mean(x) # La moyenne du nombre de projet saisonniers est de 1947,045
var(x) # La variance est de 106405956
median(x) # La mediane est de 60
sd(x) # L'�cart type est de 10315,33
quantile(x) # Le nombre de projets saisonniers � 25% est de 10, � 50% est de 60, � 75% est de 410, � 100% est de 150740
min(x) # Le minimum de projet saisonniers est de 0
max(x) # Le maximum de projet saisonniers est de 150740
summary(x) # La valeur minimale est de 0, le nombre de projet saisonniers � 25% est de 10, ma mediane est de 60, la moyenne est de 1947, le nombre de projets saisonniers � 75% est de 410, le nombre maximum de projets saisonniers est de 150740

boxplot(x) # Il y a des valeurs aberrantes 
barplot(x) # diagramme en barre
plot(x) # nuage de points en fonction de la fr�quence
hist(x) # histogramme en fonction de la fr�quence


# 2.1 Regression multiple

# Nombre de projets saisonniers/ Nombre projets difficiles

cor(data$Nombre.de.projets.saisonniers, data$Nombre.de.projets.difficiles)
# Le cofficient de corr�lation est de 0,77. Les variables sont corr�l�es.
plot(data$Nombre.de.projets.saisonniers, data$Nombre.de.projets.difficiles) # nuage de point
lm(data$Nombre.de.projets.saisonniers~data$Nombre.de.projets.difficiles) 
# y=0,91-684
abline(lm(data$Nombre.de.projets.saisonniers~data$Nombre.de.projets.difficiles)$coefficients, lwd=5, col="RED") 

# Nombre de projet saisonniers/ Nombre de projets 

cor(data$Nombre.de.projets.saisonniers, data$Nombre.de.projets)
# Le cofficient de corr�lation est de 0,88. Les variables sont corr�l�es
plot(data$Nombre.de.projets.saisonniers, data$Nombre.de.projets) # nuage de point
lm(data$Nombre.de.projets.saisonniers~data$Nombre.de.projets) 
# y=0,5163-1030,4673
abline(lm(data$Nombre.de.projets.saisonniers~data$Nombre.de.projets.difficiles)$coefficients, lwd=5, col="RED") 

# 3.1 Intervalle de confiance

# interval de confiance
t.test(data$Nombre.de.projets.saisonniers,conf.level = 0.95)
# interal de confiance � 95% : 1009,047 et 2885,043

# Test Khi-deux
tab=table(data$M.tier,data$Grand.secteur)
tab
chisq.test(tab) # la p value est de 1 donc l'association entre les deux valeurs sont significatives

# Test anova

summary(aov(data$Nombre.de.projets~data$Grand.secteur))
# Df    Sum Sq   Mean Sq F value
#data$Grand.secteur   5 5.949e+09 1.190e+09   4.011
#Residuals          461 1.367e+11 2.966e+08        
#Pr(>F)   
#data$Grand.secteur 0.00142 **
 # Residuals                    

#Test normalit�

rnorm(data$Nombre.de.projets)

simu <- rnorm(data$Nombre.de.projets)
{hist(simu, prob=T, breaks="FD", 
      main="Histogramme de projet")}
curve(dnorm(x), add=T)

# Les variables projet observent une loi normale

rnorm(data$Nombre.de.projets.difficiles)

simu <- rnorm(data$Nombre.de.projets.difficiles)
{hist(simu, prob=T, breaks="FD", 
      main="Histogramme de projets difficiles")}
curve(dnorm(x), add=T)

# Les variables projets difficiles observent une loi normale

rnorm(data$Nombre.de.projets.saisonniers)

simu <- rnorm(data$Nombre.de.projets.saisonniers)
{hist(simu, prob=T, breaks="FD", 
      main="Histogramme de projet")}
curve(dnorm(x), add=T)

# Les variables projets saisonniers observent une loi normale