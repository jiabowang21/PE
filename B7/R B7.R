## Lectura de les dades
dades <- read.table("clipboard", header=TRUE, sep='\t')

#############################################################################
## BLOC5. Comparació de mitjanes(𝝁1=𝝁2). Mostres aleatòries aparellades ###
#############################################################################

# Hipòtesi ##################################################### 
## H0: Diferència de temps de renderització és 0, mu diferència = 0
## H1:  mu diferència != 0, ja que és una prova bilateral

# Premisses ##################################################### 
## Abans de la prova de comparació de mitjanes, és molt important assegurar 
## les mostres obtingudes compleixen les premisses, a una anàlisi de dades 
## aparellades és necessària comprovar l'efecte additiu constant i Normalitat de la D
TDR <-dades$Temps_Davinci_Resolve #30 mostres de temps de renderització amb l'editor Davinci Resolve
TAP <- dades$Temps_Adobe_Premiere  #30 mostres de temps de renderització amb l'editor Adobe Premiere
mTDR <- mean(TDR)
mTAP <- mean(TAP)
sTDR <- sd(TDR)
sTAP <-sd(TAP)
summary(TDR)
summary(TAP)

## Variable Diferència ##
D <- TDR - TAP #Variable diferència de temps de renderització Davinci-Premiere
boxplot(D, col="lightgreen", main = "Diferència de temps de renderització")
mD <-mean(D)
### Normalitat
qqnorm(D)
qqline(D, col=2)
#### La distribució de les diferències segueix una distribució normal. 
### Efecte additiu
library(PairedData)
p <- paired(TAP, TDR)
plot(p, type='BA') 
boxplot(p, col = 6:5 , names=c("Adobe Premiere", "Davinci Resolve"), main = "Temps de renderització" )

#### La diferència de mitjana puntual estimada és de -19.63
#### Amb un interval de confiança de 95% -> IC(𝝁D, 0.95) = [-154, 115]
#### L'efecte no és constant, per valors grans, l'efecte és més gran

#### Com que l'efecte additiu no és constant, provarem de solucionar-lo fent servir
#### la transformació logarítmica sobre les variables 


## Variable Diferència de logaritmes ###
logTDR <-log(TDR)
logTAP <- log(TAP)
logD <- logTDR -logTAP
### Normalitat
qqnorm(logD)
qqline(logD,col=2)
#### La distribució de les diferències es pot assumir com a normal, ja que la variable
#### diferència s'ajusten força bé als quantils teòrics de la Normal

### Efecte additiu
library(PairedData)
p2 <- paired(logTAP, logTDR)
plot(p2, type='BA')
#### L'efecte additiu constant, més o menys es manté constant que s'augmenta el valor de l'eix horitzontal.

### Comparació de mitjanes ####################################
mlogD <- mean(logD) #Mitjana mostral de logD 
slogD <- sd(logD) #Desviació típica mostral de logD
nlogD <- length(D) #Nombre de mostres de dades
elogD <- slogD/sqrt(nlogD) #Estimació de l'error tipus 

t <- (mlogD)/elogD #Càlcul estadístic
# Com que és una prova bilateral (ja que volem saber si en dos programes d'editors diferents 
# el temps de renderització són iguals o no), és necessàri calcular en el p-valor la 
# probabilitat: P(|t29| > |t|) on en aquest cas el valor t és positiu, per tant utilitzem la funció següent: 
p_valor <- 2*(1-pt(t, nlogD-1)) 

#Punt crític 
punt_critic <- qt(0.975, nlogD-1)

#Interval de confiança
IC_logD <- c(mlogD - punt_critic*elogD, mlogD + punt_critic*elogD) #IC(𝝁logD, 0.95)

##No rebutgem la hipòtesi nul·la, ja que el pvalor > 0.05(0.79) o que el punt crític és major que la t (0.269 < 2.045)
t.test(logD)

