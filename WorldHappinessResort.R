data=read.csv(choose.files())
View(data)

#A. Reporting
install.packages('dataMaid')
library(dataMaid)
makeCodebook(data)

#B. Statistique univarié
#2. GDP:Gross Domestic Product (per capita)
#C'est le Produit Intérieur Brut par habitant

#3. 
summary(data$GDP)
#La valeur minimum GDP est de 6,494, la valeur à 25% des valeurs est de 8,497, la médiane est de 9,492, la moyenne est de 9,298, la valeur à 75% des valeurs est de 10,279 et la valeur maximum est de 11,454
#4. 
boxplot(data$GDP)
# La visualisation du boxplot montre l'absence de données aberrantes en minimum et maximum.

#5.
rnorm(data$GDP)

simu <- rnorm(data$GDP)
{hist(simu, prob=T, breaks="FD", 
      main="Histogramme de GDP")}
curve(dnorm(x), add=T)
# Après visualisation de l'histogramme puis de la courbe, nous pouvons affirmer que cette valeur suit une loi normale.

#6. 
t.test(data$GDP,conf.level = 0.9)
# La moyenne du GDP est de 9,298 avec un intervalle de confiance à 90% de [9,13 - 9,46].

#7. 

install.packages("dplyr")
library(dplyr)

colnames(data)
d=data%>%select(Country,GDP)
d

d2=d%>%filter(GDP<11.454)
d2

d3=d%>%arrange(desc(GDP))%>%slice(1:10)
d3


#8. 
tapply(data$GDP, data$Region, mean)


barplot(tapply(data$GDP, data$Region, mean), col=rainbow(10))


#C Statistique générale et inferentielle

#9. 
#Country
table(data$Country) # Le nombre de pays est identique.

#Proportion
prop.table(table(data$Country)) 

#Fréquence
x=table(data$Country)
x
freq=round(prop.table(x)*100,1) 
freq # Les fréquences sont identiques

#Graphiques
plot(table(data$Country), col = c(1, "blue")) #Le nombre de pays est identique
barplot(table(data$Country), col = c(1,"blue")) 
pie(table(data$Country), col = rainbow(100))

plot(freq, col =c(1, "blue")) # Les fréquences sont identiques
barplot(freq, col =c(1, "blue"))
pie(freq, col = c(1, "blue"))


#Region
table(data$Region)

#Proportion
prop.table(table(data$Region))

#Fréquence
x=table(data$Region)
x
freq=round(prop.table(x)*100,1) 
freq


#Graphiques

plot(table(data$Region), col = c(1, "blue"))
barplot(table(data$Region), col = c(1,"blue"))
plot(prop.table(table(data$Region)), col = c(1, "blue"))
barplot(prop.table(table(data$Region)), col = c(1, "blue"))

#10.
#Health
x=data$Health
x

summary(x)
# La valeur minimale est de 45,2, la valeur à 25% des valeurs est de 58,7, la médiane est de 66, la moyenne est de 64,19, la valeur à 75% des valeurs est de 68,9, la valeur maximale est de 76,5 et la valeur de 2 pays n'est pas communiquée.

mean(x, na.rm=TRUE) #moyenne=64,19
min(x, na.rm=TRUE) #valeur minimale= 45,2
max(x, na.rm=TRUE) #valeur maximale= 76,5
quantile(x, na.rm=TRUE)# vu au dessus
var(x, na.rm=TRUE) # La variance est de 50,5425
sd(x, na.rm=TRUE) # l'écart type est de 7,11

#Graphique
boxplot(x)
barplot(x, col=rainbow(100))
plot(x, col=rainbow(100))
hist(x)
pie(tab, col=rainbow(100))

#Happiness

x=data$Happiness
x

summary(x)
# La valeur minimale est de 2,662, la valeur à 25% des valeurs est de 4,619, la médiane est de 5,553, la moyenne est de 5,460, la valeur à 75% des valeurs est de 6,252, la valeur maximale est de 7,788.

mean(x) #moyenne= 5,46
min(x) #valeur minimale=2,66
max(x) #valeur maximale=7,79
quantile(x) # vu au dessus
var(x) # la variance est de 1,3
sd(x) # l'écart type est de 1,14

#Graphique
boxplot(x)
barplot(x, col=rainbow(100))
plot(x, col=rainbow(100))
hist(x)

#11.
#Variables qualitatives: Country, Region

tab=table(data$Country,data$Region)
tab

#loi du Khi-deux

chisq.test(tab)
#La p-value est de 0,4254 donc l'association entre les valeurs n'est pas significative.

#Loi de student
t.test(tab)
#La valeur absolue t est de 12,776 mais pas de valeur critique.

#Loi de Fisher
f.test(tab) # Ne fonctionne pas.

barplot(tab,col=rainbow(147))

#12.
#Variables quantitatives: Happiness, Freedom

#Corrélation
cor.test(data$Happiness, data$Freedom)
#Le coefficient de corrélation est de 0,53 donc les variables sont plus ou moins liées.

plot(data$Happiness, data$Freedom)

#13.
#Variable quantitative: Health Variable qualitative: Country

#Test anova
summary(aov(data$Health~data$Country))

#Test de student
t.test(data$Health~data$Country)
tab3=table(data$Health,data$Country)
tab3
t.test(tab3)
#La valeur absolue t est de 12,096 mais pas de valeur critique.

#D Modèle de regression

#14.
#Happinness, Region

#Anova
summary(aov(data$Happiness~data$Region))
#p-value<5%

summary(aov(data$Happiness~data$Country))

#Coefficient de corrélation
cor.test(data$Happiness,data$GDP)
#Coefficient de corrélation 0,76, donc les valeurs sont pratiquement liées 

#15.
#Nuage de points
plot(data$Happiness, data$GDP)


#Equation droite 
droite=lm(data$Happiness~data$GDP)

a1=coef(droite)[1]
a2=coef(droite)[2]
eq=paste0("y=",round(a2,3),"*x+",round(a1,3))
plot(data$Happiness,data$GDP,main="Droite de régression",sub=eq)
abline(droite, col="red")
#Equation de la droite:   y=0,714x-1,144

#15.
#Variables Happiness et variables Health

droite2=lm(data$Happiness~data$Health)

a1=coef(droite2)[1]
a2=coef(droite2)[2]
eq=paste0("y=",round(a2,3),"*x+",round(a1,3))
plot(data$Happiness,data$Health,main="Droite de régression",sub=eq)
abline(droite, col="red")
#Equation de la droite: y=0,122x-2,376

#16.
#Livrairie rworldmap avec variable Happiness

install.packages("rworldmap")
library("rworldmap")
help("rworldmap")

a=joinCountryData2Map(data, joinCode="ISO3", nameJoinColumn = "Country")

mapCountryData(a, nameColumnToPlot = "Health")



