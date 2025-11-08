# Questo dataset proviene dalla banca dati delle malattie cardiovascolari
# della Duke University ed ? composto da 3504 pazienti e 6 variabili.
# Di cui sono stati tolti 3 pazienti per mancanza di dati (TOTALE PAZIENTI = 3501)

# I pazienti sono stati indirizzati al Duke University Medical Center per dolore toracico.

# Alcune analisi interessanti includono la previsione della probabilit? 
# di una malattia coronarica significativa
# (>= 75% di restringimento del diametro in almeno un'arteria coronaria importante) 
# e la previsione della probabilit? di una malattia coronarica grave 
# dato che qualche malattia significativa ? "esclusa".

# L'analisi utilizzerebbe sigdz come VARIABILE RISPOSTA
                                                                                                                
# Fattori di rischio, incluso sesso (sesso=0 per i maschi, 1 per le femmine),
# et?, durata dei sintomi (cad_dur).


# sex = (0 = maschi, 1 = femmine) -> dicotomica

# age = et? in anni (quantitativa)

# cad.dur = durata dei sintomi del dolore toracico in giorni (quantitativa)

# sigdz = malattia coronarica significativa rilevata durante il cateterismo cardiaco
# (0=no, 1=s?) -> dicotomica -> VARIABILE RISPOSTA
# La variabile sigdz ? un indicatore della presenza di un'ostruzione di almeno il 75% 
# in una qualsiasi delle arterie coronarie principali di sinistra
# o in una qualsiasi delle tre arterie di distribuzione: LAD, LCA e RCA.

# tvdlm = malattia dell'arteria coronaria principale sinistra o dei tre vasi riscontrata
# a livello cardiaco,  cateterizzazione: 0=no, 1=s?
# (I 3 PAZIENTI MANCANTI SONO STATI ELIMINATI DAL DATASET !!!)
library(readxl)
acath2 <- read_excel("acath2.xlsx")
View(acath2)
acath2
print(acath2,n=200)

attach(acath2)
dim(acath2)  # 3501 pazienti e 5 variabili (perch? ne abbiamo eliminati 3)


# ANALISI ESPLORATIVA UNIVARIATA

summary(acath2)

###### sex ######

summary(sex)
table(sex)  # 0 = M = 2404 ; 1 = F = 1097
100*table(sex)/length(sex)
# essendo variabili dicotomiche per sex la media ci fornisce gi? una frequenza relativa,
# quindi abbiamo il 68.7 % (69) di MASCHI e il 31.3 % (31) di FEMMINE  (TOT = 100 %)

# grafico a torta
valoriG <- c(2404,1097)
labelG <- c("MASCHI","FEMMINE")
perc <- round(valoriG/sum(valoriG)*100, digits=1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,cex=1.5,col=c("skyblue","deeppink"), main="GRAFICO A TORTA PAZIENTI PER SESSO")
# oppure in 3D
library(plotrix)
valoriG <- c(2404,1097)
labelG <- c("MASCHI","FEMMINE")
perc <- round(valoriG/sum(valoriG)*100,digits=1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie3D(valoriG,labels=labelG,explode=0.1,main="GRAFICO A TORTA PAZIENTI PER SESSO")

# barplot frequenze relative
barplot(xtabs(~sex)/length(sex), col=c("skyblue","deeppink"),beside=T,main="BARPLOT PER SESSO",
        names.arg=c("0 = MASCHI","1 = FEMMINE"))

# prop.test (F,M)
# H0: p = 0.5
# H1: p != 0.5

# z = (hat(p)-p0)/sqrt(po*(1-p0)/n) ~ N(0,1)
# p = 2*(1-phi(|zoss|))

prop.test(1097,3501)  
# X-squared = 487.19, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#     0.2980450    0.3290461
# sample estimates: p = 0.313339 
## RIFIUTO H0, CIOE' LE 2 PROPORZIONI SONO SIGNIFICATIVAMENTE DIVERSE


###### age ######

summary(age)
sd(age)
IQR(age)  
mean(age)    
median(age)  # possiamo dire che media e mediana per variabile age coincidono
quantile(age,0.05)  

par(mfrow=c(1,3))
# boxplot
boxplot(age, main="age",col="green")   # notiamo molti outliers (valori anomali)

# istogramma
hist(age,col="green")       # R dvide gi? in classi la distribuzione
hist(age, prob=T,col="green")   # prob serve per trasformare da numerico in freq. relative
lines(density(age), col=2, pch=2, lwd=2)  # per agg. curva distribuzione su hist
curve(dnorm(x,mean(age),sd(age)),add=T)

# Funzione di ripartizione solo age
plot(ecdf(age),main="Funzione di ripartizione",xlim=c(0,100),col=3)

# NORMALITA' + RESIDUI age
shapiro.test(age)    # W = 0.99782, p-value = 7.988e-05
qqnorm(age)
qqline(age,col=2,lwd=3)
# essendo che il campione ? molto elevato possiamo invocare il TLC per assumere la normalit?


###### cad.dur ######
# (durata dei sintomi del dolore toracico in giorni)

summary(cad.dur)
sd(cad.dur)
IQR(cad.dur)  
mean(cad.dur)    
median(cad.dur)  # possiamo dire che media e mediana per variabile age coincidono
quantile(cad.dur,0.05)  

# boxplot
boxplot(cad.dur, main="cad.dur",col=4)   # molti valori anomali 

# istogramma
hist(cad.dur,col=4)       # forte asimmetria 
hist(cad.dur, prob=T,col=4)   # prob serve per trasformare da numerico in freq. relative
lines(density(cad.dur), col=2, pch=2, lwd=2)  # per agg. curva distribuzione su hist
curve(dnorm(x,mean(cad.dur),sd(cad.dur)),add=T)

# NORMALITA' + RESIDUI cad.dur
shapiro.test(cad.dur)   # rifiutiamo ipotesi di normalit? (DA CHIEDERE)
qqnorm(cad.dur)
qqline(cad.dur,col=2,lwd=3)

# provando la trasformata logaritmica...
shapiro.test(log(cad.dur))   # ci da errore con logaritmo quindi da scartare


###### sigdz ######
# sigdz = malattia coronarica significativa rilevata durante il cateterismo cardiaco
# dicotomica (0=no , 1=si)

summary(sigdz)
table(sigdz)  # 0=assenza=1169 ; 1=presenza=2332
100*table(sigdz)/length(sigdz)  
# essendo variabili dicotomiche per sigdz la media ci fornisce gi? una frequenza relativa,
# quindi abbiamo il 33.4% (33) di ASSENZA e il 66.6% (67) di PRESENZA di malattia coronarica significativa (TOT = 100 %)

# grafico a torta
valoriG <- c(1169,2332)
labelG <- c("ASSENZA","PRESENZA")
perc <- round(valoriG/sum(valoriG)*100,digits=1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,cex=1.5,col=c("green","red"), main="GRAFICO A TORTA MALATTIA CORONARICA SIGNIFICATIVA (sigdz)")
# oppure in 3D
library(plotrix)valoriG <- c(1169,2332)
labelG <- c("ASSENZA","PRESENZA")
perc <- round(valoriG/sum(valoriG)*100,digits=1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie3D(valoriG,labels=labelG,explode=0.1,main="GRAFICO A TORTA MALATTIA CORONARICA SIGNIFICATIVA (sigdz)")


# barplot frequenze relative
barplot(xtabs(~sigdz)/length(sigdz), col=c("green","red"),beside=T,main="BARPLOT sigdz",
        names.arg=c("0 = ASSENZA","1 = PRESENZA"))

# prop.test (ASSENZA,PRESENZA)
# H0: p = 0.5
# H1: p != 0.5

# z = (hat(p)-p0)/sqrt(po*(1-p0)/n) ~ N(0,1)
# p = 2*(1-phi(|zoss|))

prop.test(1169,3501)   
# X-squared = 385.67, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#     0.3183313   0.3498453
# sample estimates: p = 0.3339046 
## RIFIUTO H0, CIOE' LE 2 PROPORZIONI SONO SIGNIFICATIVAMENTE DIVERSE


###### tvdlm ######

summary(tvdlm)
table(tvdlm)   # 0 = assenza = 2372 , 1 = presenza = 1129 
100*table(tvdlm)/length(tvdlm)
# essendo variabili dicotomiche per tvdlm la media ci fornisce gi? una frequenza relativa,
# quindi abbiamo il 67.8 % (68) di ASSENZA e il 32.2% (32) di PRESENZA  (TOT = 100 %)

# grafico a torta
valoriG <- c(2372,1129)
labelG <- c("ASSENZA","PRESENZA")
perc <- round(valoriG/sum(valoriG)*100,1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,cex=1.5,col=c("lightgreen","red"), main="GRAFICO A TORTA tvdlm")
# oppure in 3D
library(plotrix)
valoriG <- c(2372,1129)
labelG <- c("ASSENZA","PRESENZA")
perc <- round(valoriG/sum(valoriG)*100,1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie3D(valoriG,labels=labelG,explode=0.1,main="GRAFICO A TORTA tvdl")

# barplot frequenze relative
barplot(xtabs(~tvdlm)/length(tvdlm), col=c("lightgreen","red"),beside=T,main="BARPLOT tvdlm",
        names.arg=c("0 = ASSENZA","1 = PRESENZA"))

# prop.test (ASSENZA,PRESENZA)
# H0: p = 0.5
# H1: p != 0.5

# z = (hat(p)-p0)/sqrt(po*(1-p0)/n) ~ N(0,1)
# p = 2*(1-phi(|zoss|))

prop.test(1129,3501)  
# X-squared = 440.61, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#     0.3070569   0.3382944
# sample estimates: p = 0.3224793 
## RIFIUTO H0, CIOE' LE 2 PROPORZIONI SONO SIGNIFICATIVAMENTE DIVERSI

library(readxl)
acath3_prova <- read_excel("acath3_prova.xlsx")
View(acath3_prova) 
################################################################################
################################################################################
####### (tvdlm3 | sigdz = 1) ######### ESPLORATIVA npazienti = 2332

table(tvdlm[sigdz=="1"])  # TOT=2332 (0=1203,1=1129)
100*table(tvdlm[sigdz=="1"])/length(tvdlm[sigdz=="1"]) # 0 = 51.6% , 1 = 48.4%
# grafico a torta
valoriG <- c(1203,1129)
labelG <- c("ASSENZA","PRESENZA")
perc <- round(valoriG/sum(valoriG)*100,1)
labelG <- paste(labelG, perc)
labelG <- paste(labelG,"%",sep="")
pie(valoriG, labels = labelG,col=rainbow(length(labelG)), main="GRAFICO A TORTA (tvdlm|sigdz=1)")

# barplot frequenze relative
barplot(xtabs(~tvdlm3)/length(tvdlm3), col=c("lightgreen","red"),beside=T,main="BARPLOT tvdlm|sigdz=1",
        names.arg=c("ASSENZA","PRESENZA"))

prop.test(1129,2332) 
# X-squared = 2.2852, df = 1, p-value = 0.1306
# 95 percent confidence interval:
#    0.4636796   0.5046407
# sample estimates: p = 0.4841338 
## ACCETTO H0, CIOE'LE 2 PROPORZIONI SONO SIGNIFICATIVAMENTE UGUALI

# sex3
table(sex3)
barplot(xtabs(~sigdz)/length(sigdz), col=c("lightblue","deeppink"),beside=T,main="BARPLOT sex|sigdz=1",
        names.arg=c("Maschi","Femmine"))
prop.test(461,2332)  # rifiuto ipotesi, proporzioni significativam. diverse

# age3
summary(age3)
sd(age3)
IQR(age3)
boxplot(age3,col="lightgreen",main="Boxplot age")

hist(age3, prob=T,col="lightgreen",main="Histogram of age",xlab="age")   # prob serve per trasformare da numerico in freq. relative
lines(density(age3), col="2", pch=2, lwd=2)  # per agg. curva distribuzione su hist
curve(dnorm(x,mean(age3),sd(age3)),add=T)

shapiro.test(age3)   # W = 0.99631, p-value = 1.776e-05
qqnorm(age3,main="Normal Q-Q Plot age")
qqline(age3,col=2,lwd=2)

# cad.dur3
summary(cad.dur3)
sd(cad.dur3)
IQR(cad.dur3)

boxplot(cad.dur3,col=4,main="Boxplot cad.dur")

hist(cad.dur3, prob=T,col=4,main="Histogram of cad.dur",xlab="cad.dur")   # prob serve per trasformare da numerico in freq. relative
lines(density(cad.dur3), col="2", pch=2, lwd=2)  # per agg. curva distribuzione su hist
curve(dnorm(x,mean(cad.dur3),sd(cad.dur3)),add=T)

shapiro.test(cad.dur3)   # W = 0.75339, p-value < 0.0001
qqnorm(cad.dur3,main="Normal Q-Q Plot cad.dur")
qqline(cad.dur3,col=2,lwd=2)


#### tvdlm3 vs sex3 ##########

table(tvdlm3,sex3)   # tabella di contingenza
# (0,0) = assenza malattia maschi = 930
# (0.1) = assenza malattia femmine = 273
# (1,0) = presenza malattia maschi = 941
# (1.1) = presenza malattia femmine = 188
chisq.test(tvdlm3,sex3) # X-squared = 13.024, df = 1, p-value = 0.0003075
# ASSOCIAZIONE SIGNIFICATIVA tra tvdlm3 e sex3 (LA TENGO COME COVARIATA (SEX3))
prop.test(c(941,188),c(1871,461)) # conferma il chi quadro
fisher.test(tvdlm3,sex3) # odds ratio = 0.6807135

###### tvldm3 vs age3 #######

boxplot((age3~tvdlm3),main="tvdlm VS age",
        col=c("green","red"),names=c("0","1"),xlab="tvdlm",ylab="age")


# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(age3[tvdlm3=="0"])  # W = 0.99564, p-value = 0.001611
shapiro.test(age3[tvdlm3=="1"])  #  W = 0.99608, p-value = 0.0057
qqnorm(age3[tvdlm3=="0"])
qqline(age3[tvdlm3=="0"],col=2,lwd=2)
qqnorm(age3[tvdlm3=="1"])
qqline(age3[tvdlm3=="1"],col=2,lwd=2)
# ACCETTO NORMALITA' PER TLC

## STEP 2: VERIFICARE OMOSCHEDASTICITA'
# H0: sigma^2_1=sigma^2_2
var.test(age3~tvdlm3)    # F = 1.043, num df = 1202, denom df = 1128, p-value = 0.473
# ACCETTO H0, ipotesi di omoschedasticit?

## STEP 3: TEST t A 2 CAMPIONI INDIPENDENTI
# H0: mu_1=mu_2 (mu_1-mu_2=0)
t.test(age3[tvdlm3=="0"],age3[tvdlm3=="1"],var.equal=T) 
# t = -8.8843, df = 2330, p-value < 2.2e-16
# rifiuto H0, quindi vi ? una differenza significativa dell'et?
# tra i 2 gruppi di pazienti divisi per presenza/assenza di malattia grave (tvdlm3)

# oppure potevamo applicare il test non parametrico di Mann-Whitney 
wilcox.test(age3[tvdlm3=="0"],age3[tvdlm3=="1"])  # W = 538352, p-value < 2.2e-16
# otteniamo stessa conclusione
# rifiuto H0, quindi vi ? una differenza significativa dell'et?
# tra i 2 gruppi di pazienti divisi per presenza/assenza di malattia grave (tvdlm3)


######## tvdlm3 vs cad.dur3 ##################

boxplot((cad.dur3~tvdlm3),main="tvdlm VS cad.dur",
        col=c("green","red"),names=c("0","1"),xlab="tvdlm",ylab="cad.dur")

# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(cad.dur3[tvdlm3=="0"])   # W = 0.71241, p-value < 2.2e-16
shapiro.test(cad.dur3[tvdlm3=="1"])   # W = 0.80207, p-value < 2.2e-16
qqnorm(cad.dur3[tvdlm3=="0"])
qqline(cad.dur3[tvdlm3=="0"],col=2,lwd=2)
qqnorm(cad.dur3[tvdlm3=="1"])
qqline(cad.dur3[tvdlm3=="1"],col=2,lwd=2)
# NON ABBIAMO NORMALITA' QUINDI PROCEDIAMO CON TEST NON-PARAMETRICO (MANN-WHITNEY)

## STEP 2: TEST A 2 CAMPIONI INDIPENDENTI MANN-WHITNEY
# H0: Fx1(x) = Fx2(x)
wilcox.test(cad.dur3[tvdlm3=="0"],cad.dur3[tvdlm3=="1"]) # W = 503767, p-value < 2.2e-16
# RIFIUTO H0, allora vi ? un tempo di durata dei sintomi significativamente 
# diverso tra i 2 gruppi (di PRESENZA/ASSENZA tvdlm3)

# OPPURE CON TLC (DA CONFERMARE) ACCETTIAMO LA NORMALITA' NELLE MEDIE E VERIFICHIAMO 
# OMOSCHEDASTICITA' E T.TEST
var.test(cad.dur3~tvdlm3)  # F = 0.52055, num df = 1202, denom df = 1128, p-value < 2.2e-16
# RIFIUTIAMO IPOTESI DI OMOSCHED.
# ---> TEST DI WELCH
t.test(cad.dur3[tvdlm3=="0"],cad.dur3[tvdlm3=="1"]) # t = -9.8363, df = 2042, p-value < 2.2e-16
# ci conferma conlusione del test NP


########## age3 vs sex3 ###############
# age3 quantitativa
# sex3 dicotomica (0,1)

summary(age3[sex3=="0"])    # summary di age3 per maschi3
sd(age3[sex3=="0"])         # deviazione standard di age3 per maschi3
IQR(age3[sex3=="0"])        # scarto interquantilico di age3 per maschi3
summary(age3[sex3=="1"])    # summary di age3 per femmine3
sd(age3[sex3=="1"])         # deviazione standard di age3 per femmine3
IQR(age3[sex3=="1"])        # scarto interquantilico di age3 per femmine3


boxplot((age3~sex3), main="age VS sex",
        col=c("Blue","red"),names=c("0","1"))  #  et? rispetto al sesso

# funzione di ripartizione age vs sex
ageM3 = age3[sex3=="0"]
ageF3 = age3[sex3=="1"]
cdfM3 <- ecdf(ageM3)
cdfF3 <- ecdf(ageF3)
plot(cdfM3,col="blue",main="Funzione di ripartizione cumulativa per et? divisa per sesso",
     xlab="Et?3",ylab="Fn(x)")
lines(cdfF3,col="red")
legend("bottomright",legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=3)

# grafico densit? appaiata (PREFERIBILE)
ageM3 = age3[sex3=="0"]
ageF3 = age3[sex3=="1"]
plot(density(ageM3), col = "blue",main = "Grafico di Densit? per Et?", xlab = "Et?3", ylab = "Densit?",
     xlim = c(min(age3), max(age3)),lwd=2)
lines(density(ageF3), col = "red",lwd=2)
legend("topright", legend = c("Maschi", "Femmine"), col = c("blue", "red"), lty = 1,lwd=2)


# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(age3[sex3=="0"])  # maschi  ->  W = 0.99585, p-value = 5.006e-05
shapiro.test(age3[sex3=="1"])  # femmine ->  W = 0.99025, p-value = 0.003782
qqnorm(age3[sex3=="0"])
qqline(age3[sex3=="0"],col=2,lwd=2)
qqnorm(age3[sex3=="1"])
qqline(age3[sex3=="1"],col=2,lwd=2)
# posso accettare ipotesi di normalit? per entrambe visto la dimensione del campione con TLC

## STEP 2: VERIFICARE OMOSCHEDASTICITA'
# H0: sigma^2_1=sigma^2_2
var.test(age3~sex3)  # F = 0.8696, num df = 1870, denom df = 460, p-value = 0.05246 
# ACCETTO H0, ipotesi di omoschedasticit?

## STEP 3: TEST t A 2 CAMPIONI INDIPENDENTI
# H0: mu_1=mu_2 (mu_1-mu_2=0)
t.test(age3[sex3=="0"],age3[sex3=="1"],var.equal=T)  
# t = -8.4669, df = 2330, p-value < 2.2e-16
# RIFIUTO H0, quindi vi ? una differenza significativa dell'et?3
# tra i 2 gruppi di pazienti divisi per sesso3 (riferito tutto a tvdlm3)

# Se non la consideriamo normale allora potevamo applicare il test non parametrico di Mann-Whitney 
wilcox.test(age3[sex3=="0"],age3[sex3=="1"])   # W = 323211, p-value < 2.2e-16
# otteniamo stessa conclusione --> RIFIUTO H0


###### age3 vs cad.dur3 ######

library(ggplot2)
ggplot(acath3_prova,aes(x=age3,y=cad.dur3,color=as.factor(sex3))) +
  scale_color_manual(values = c("0"="blue","1"="red")) +
  geom_point()+geom_smooth(method="lm") + 
  ggtitle("Diagramma di dispersione tra age e cad.dur per sesso")

# indice di correlazione di Pearson generale (senza divisione per sesso)
cor(age3,cad.dur3)    # 0.2886913
# proviamo con spearman
 

cor(age3,cad.dur3,method="spearman")  
cor.test(age3[sex=="0"],cad.dur3[sex=="0"],method="spearman")  
cor.test(age3[sex=="1"],cad.dur3[sex=="1"],method="spearman")

# ATTENZIONE, IL COR.TEST RICHIEDE NORMALITA' QUINDI FORSE NON VA BENE (FORSE SBAGLIATO!!!)
# H0: rho=0
# H1: rho!=0
cor.test(age3,cad.dur3) # t = 14.555, df = 2330, p-value < 2.2e-16
# RIFIUTIAMO H0, quindi correlazione significativamente diversa da zero

# per ottenere i valori di correlazione tra le 2 variabili con divisione per sesso
cor.test(age3[sex3=="0"],cad.dur3[sex3=="0"])   # cor = 0.3090121
cor.test(age3[sex3=="1"],cad.dur3[sex3=="1"])   # cor = 0.2391441


###### sex3 vs cad.dur3 #####

boxplot((cad.dur3~sex3),main = "cad.dur VS sex", col = c("skyblue","deeppink"),
        names=c("0","1"))  
# durata rispetto al sesso (notiamo molti outliers)

# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(cad.dur3[sex3=="0"])  # maschi  -> W = 0.76032, p-value < 2.2e-16
shapiro.test(cad.dur3[sex3=="1"])  # femmine  -> W = 0.71893, p-value < 2.2e-16
qqnorm(cad.dur3[sex3=="0"])
qqline(cad.dur3[sex3=="0"],col=2,lwd=2)
qqnorm(cad.dur3[sex3=="1"])
qqline(cad.dur3[sex3=="1"],col=2,lwd=2)
# RIFIUTO IPOTESI DI NORMALITA'

# TEST A 2 CAMIONI NON PARAMETRICO DI MANN WHITNEY
wilcox.test(cad.dur3[sex3=="0"],cad.dur3[sex3=="1"]) 
# W = 423288, p-value = 0.5377
# le medie dei due gruppi tra maschi e femmine in termini di durata dei sintomi sono
# signidicativamente uguali

var.test(cad.dur3~sex3)  #F = 0.9864, num df = 1870, denom df = 460, p-value = 0.8407
# ACCETTO H0, ipotesi di omoschedasticit?

## STEP 3: TEST t A 2 CAMPIONI INDIPENDENTI
# H0: mu_1=mu_2 (mu_1-mu_2=0)
t.test(cad.dur3[sex3=="0"],cad.dur3[sex3=="1"],var.equal=T)  

#############################################################################
#############################################################################
#############################################################################

#########################
### ANALISI BIVARIATA ###
#########################

#### sigdz vs sex ####   a) + tab a.1)

table(sigdz,sex)   # tabella di contingenza
# (0,0) = assenza malattia maschi = 533
# (0.1) = assenza malattia femmine = 636
# (1,0) = presenza malattia maschi = 1871
# (1.1) = presenza malattia femmine = 461
chisq.test(sigdz,sex) # X-squared = 432.58, df = 1, p-value < 2.2e-16
# ASSOCIAZIONE SIGNIFICATIVA tra sigdz e sex (LA TENGO COME COVARIATA (SEX))
1871/(533+1871)   # prop. maschi con malattia significativa = 0.778
461/(636+461)     # prop. femmine con malattia significativa = 0.42
prop.test(c(1871,461),c(2404,1097))
fisher.test(sigdz,sex)   
# ODDS RATIO = 0.2 quindi c'? una quota di 0.2 per le femmine rispetto ai maschi (?)

# TENGO SEX COME COVARIATA


#### sigdz vs tvdlm #### (DA NON METTERE!!!)

table(sigdz,tvdlm)   
# (0,0) = assenza malattia significativa senza tvdlm = 1169
# (0.1) = assenza malattia significativa con tvdlm = 0
# (1,0) = presenza malattia significativa senza tvdlm = 1203
# (1.1) = presenza malattia significativa con tvdlm = 1129
chisq.test(sigdz,tvdlm) # -> X-squared = 833.11, df = 1, p-value < 2.2e-16
# ASSOCIAZIONE SIGNIFICATIVA tra sigdz e tvdlm (LA TENGO COME COVARIATA (tvdlm))
prop.test(c(1203,1129),c(2372,1129))
fisher.test(sigdz,tvdlm)

# TENGO tvdlm COME COVARIATA


#### sigdz vs age ####   b)
# 0 = assenza malattia coronarica significativa , 1 = presenza

boxplot((age~sigdz),main="sigdz VS age",
        col=c("green","red"),names=c("Assenza","Presenza"))


# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(age[sigdz=="0"])  # W = 0.99739, p-value = 0.0557
shapiro.test(age[sigdz=="1"])  # W = 0.99631, p-value = 1.776e-05
qqnorm(age[sigdz=="0"])
qqline(age[sigdz=="0"],col=2,lwd=2)
qqnorm(age[sigdz=="1"])
qqline(age[sigdz=="1"],col=2,lwd=2)
# ACCETTO NORMALITA' PER TLC

## STEP 2: VERIFICARE OMOSCHEDASTICITA'
# H0: sigma^2_1=sigma^2_2
var.test(age~sigdz)    # F = 1.0686, num df = 1168, denom df = 2331, p-value = 0.1869 
# ACCETTO H0, ipotesi di omoschedasticit?

## STEP 3: TEST t A 2 CAMPIONI INDIPENDENTI
# H0: mu_1=mu_2 (mu_1-mu_2=0)
t.test(age[sigdz=="0"],age[sigdz=="1"],var.equal=T) 
# t = -15.207, df = 3499, p-value < 2.2e-16
# rifiuto H0, quindi vi ? una differenza significativa dell'et?
# tra i 2 gruppi di pazienti divisi per presenza/assenza di malattia coronarica significativa
# (sigdz)

# oppure potevamo applicare il test non parametrico di Mann-Whitney 
wilcox.test(age[sigdz=="0"],age[sigdz=="1"]) # W = 968097, p-value < 2.2e-16
# otteniamo stessa conclusione
# rifiuto H0, quindi vi ? una differenza significativa dell'et?
# tra i 2 gruppi di pazienti divisi per presenza/assenza di malattia coronarica significativa
# (sigdz)


# ETA'POTREBBE ESSERE FATTORE DI RISCHIO


#### sigdz vs cad.dur ####   (DA CHIEDERE???)   c)

boxplot((cad.dur~sigdz),main="sigdz VS cad.dur",
        col=c("green","red"),names=c("Assenza","Presenza"))

# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(cad.dur[sigdz=="0"])   # W = 0.67046, p-value < 2.2e-16
shapiro.test(cad.dur[sigdz=="1"])   # W = 0.75339, p-value < 2.2e-16
qqnorm(cad.dur[sigdz=="0"])
qqline(cad.dur[sigdz=="0"],col=2,lwd=2)
qqnorm(cad.dur[sigdz=="1"])
qqline(cad.dur[sigdz=="1"],col=2,lwd=2)
# NON ABBIAMO NORMALITA' QUINDI PROCEDIAMO CON TEST NON-PARAMETRICO (MANN-WHITNEY)

## STEP 2: TEST A 2 CAMPIONI INDIPENDENTI MANN-WHITNEY
# H0: Fx1(x) = Fx2(x)
wilcox.test(cad.dur[sigdz=="0"],cad.dur[sigdz=="1"]) # W = 1237914, p-value = 9.033e-06
# RIFIUTO H0, allora vi ? un tempo di durata dei sintomi significativamente 
# diverso tra i 2 gruppi (di PRESENZA/ASSENZA sigdz) --> differenza significativa in tempo per i 2 gruppi

# DURATA DOVREBBE ESSERE SIGNIFICATIVA PER SIGDZ

# OPPURE CON TLC (DA CONFERMARE) ACCETTIAMO LA NORMALITA' NELLE MEDIE E VERIFICHIAMO 
# OMOSCHEDASTICITA' E T.TEST
var.test(cad.dur~sigdz)  # F = 0.80764, num df = 1168, denom df = 2331, p-value = 3.254e-05
# RIFIUTIAMO IPOTESI DI OMOSCHED.
# ---> TEST DI WELCH
t.test(cad.dur[sigdz=="0"],cad.dur[sigdz=="1"]) # t = -4.908, df = 2571.5, p-value = 9.778e-07
# ci conferma conlusione del test NP (wilcox)

#### age vs sex ####  d)
# age quantitativa
# sex dicotomica (0,1)

summary(age[sex=="0"])   # summary per et? dei maschi
sd(age[sex=="0"])        # deviazione standard per et? dei maschi
IQR(age[sex=="0"])       # Scarto interquantilico per et? dei maschi
summary(age[sex=="1"])   # summary per et? delle femmine
sd(age[sex=="1"])        # deviazione standard per et? delle femmine
IQR(age[sex=="1"])       # Scarto interquantilico per et? delle femmine

boxplot((age~sex), main="Age VS sex",
        col=c("skyblue","deeppink"),names=c("Maschi","Femmine"))  #  et? rispetto al sesso

# funzione di ripartizione age vs sex
ageM = age[sex=="0"]
ageF = age[sex=="1"]
cdfM <- ecdf(ageM)
cdfF <- ecdf(ageF)
plot(cdfM,col="blue",main="Funzione di ripartizione cumulativa per et? divisa per sesso",
     xlab="Et?",ylab="Fn(x)")
lines(cdfF,col="red")
legend("bottomright",legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=3)

# grafico densit? appaiata (PREFERIBILE)
ageM = age[sex=="0"]
ageF = age[sex=="1"]
plot(density(ageM), col = "blue",main = "Grafico di Densit? per Et?", xlab = "Et?", ylab = "Densit?",
     xlim = c(min(age), max(age)),lwd=2)
lines(density(ageF), col = "red",lwd=2)
legend("topright", legend = c("Maschi", "Femmine"), col = c("blue", "red"), lty = 1,lwd=2)

# oppure con ggplot2
library(ggplot2)
ggDensity2 <- ggplot(data=acath2, aes(age)) + 
  geom_density(aes(fill=factor(sex)), alpha=0.5) +
  scale_fill_manual(values=c("blue2", "deeppink")) +
  labs(title="Grafico di densit? per et?",
       x="Et?",
       y="")
plot(ggDensity2)

# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(age[sex=="0"])  # maschi  ->  W = 0.99764, p-value = 0.001108
shapiro.test(age[sex=="1"])  # femmine ->  W = 0.99683, p-value = 0.02684
qqnorm(age[sex=="0"])
qqline(age[sex=="0"],col=2,lwd=2)
qqnorm(age[sex=="1"])
qqline(age[sex=="1"],col=2,lwd=2)
# posso accettare ipotesi di normalit? per entrambe visto la dimensione del campione con TLC

## STEP 2: VERIFICARE OMOSCHEDASTICITA'
# H0: sigma^2_1=sigma^2_2
var.test(age~sex)  # F = 0.96857, num df = 2403, denom df = 1096, p-value = 0.5306 
# ACCETTO H0, ipotesi di omoschedasticit?

## STEP 3: TEST t A 2 CAMPIONI INDIPENDENTI
# H0: mu_1=mu_2 (mu_1-mu_2=0)
t.test(age[sex=="0"],age[sex=="1"],var.equal=T)  
# t = -5.9727, df = 3499, p-value = 2.567e-09
# RIFIUTO H0, quindi vi ? una differenza significativa dell'et?
# tra i 2 gruppi di pazienti divisi per sesso

# Se non la consideriamo normale allora potevamo applicare il test non parametrico di Mann-Whitney 
wilcox.test(age[sex=="0"],age[sex=="1"]) # W = 1154953, p-value = 3.604e-09
# otteniamo stessa conclusione




#### cad.dur vs sex ####   (DA CHIEDERE ???)  e)

boxplot((cad.dur~sex),main = "cad.dur VS sex", col = c("skyblue","deeppink"),
        names=c("Maschi","Femmine"))  
# durata rispetto al sesso (notiamo molti outliers)

# H0: dist_F = dist_M (medie)

## STEP 1: VERIFICARE LA NORMALITA' DE 2 GRUPPI
shapiro.test(cad.dur[sex=="0"])  # maschi  -> W = 0.73497, p-value < 2.2e-16
shapiro.test(cad.dur[sex=="1"]) # femmine  -> W = 0.71032, p-value < 2.2e-16
qqnorm(cad.dur[sex=="0"])
qqline(cad.dur[sex=="0"],col=2,lwd=2)
qqnorm(cad.dur[sex=="1"])
qqline(cad.dur[sex=="1"],col=2,lwd=2)
# RIFIUTO IPOTESI DI NORMALITA'

var.test(cad.dur~sex)

t.test(cad.dur[sex=="0"],cad.dur[sex=="1"],var.equal=T)  

# TEST A 2 CAMIONI NON PARAMETRICO DI MANN WHITNEY
wilcox.test(cad.dur[sex=="0"],cad.dur[sex=="1"]) # W = 1270993, p-value = 0.08602
# ACCETTO H0, quindi NON vi ? una differenza significativa della durata dei sintomi
# tra i 2 gruppi di pazienti divisi per sesso

# I 2 GRUPPI SONO SIGNIFICATIVAMENTE UGUALI IN TERMINI DI DSITRIBUZIONI

library(ggplot2)
ggDensity2 <- ggplot(data=acath2, aes(cad.dur)) + 
  geom_density(aes(fill=factor(sex)), alpha=0.5) +
  scale_fill_manual(values=c("blue2", "deeppink")) +
  labs(title="Grafico di densit? per durata sintomi",
       x="Durata sintomi",
       y="")
plot(ggDensity2)


## AGE VS CAD.DUR   f)-g)
plot(age,cad.dur)

#plot(cad.dur~age,pch=as.integer(sex))
#legend("topleft",legend=c("maschi","femmine"),
#       cor(age[sex=="0"],cad,dur[sex=="0"]))
#cor(age[sex=="1"],cad.dur[sex=="1"])   
#ggplot(acath2,aes(x=age,y=cad.dur))+geom_point(color="blue","red",size=0.7)
#ggplot(acath2,aes(x=age,y=cad.dur,color=sex))+geom_point()

# il comando che usiamo ? questo
library(ggplot2)
ggplot(acath2,aes(x=age,y=cad.dur,color=as.factor(sex))) +
  scale_color_manual(values = c("0"="blue","1"="red")) +
  geom_point()+geom_smooth(method="lm") + 
  ggtitle("Diagramma di dispersione tra age e cad.dur per sesso")

# indice di correlazione di Pearson generale (senza divisione per sesso)
cor(age,cad.dur)   # 0.2863604
# proviamo con spearman
cor(age,cad.dur,method="spearman")  # 0.2334833 genere
cor.test(age[sex=="0"],cad.dur[sex=="0"],method="spearman")  # 0.260 , p-value<0.0001
cor.test(age[sex=="1"],cad.dur[sex=="1"],method="spearman")  # 0.168 , p<0.0001

# ATTENZIONE, IL COR.TEST RICHIEDE NORMALITA' QUINDI FORSE NON VA BENE (FORSE SBAGLIATO!!!)
# H0: rho=0
# H1: rho!=0
cor.test(age,cad.dur) # t = 17.679, df = 3499, p-value < 2.2e-16
# RIFIUTIAMO H0, quindi correlazione significativamente diversa da zero

# per ottenere i valori di correlazione tra le 2 variabili con divisione per sesso
cor.test(age[sex=="0"],cad.dur[sex=="0"])  # p-value < 2.2e-16 , 0.3127411
cor.test(age[sex=="1"],cad.dur[sex=="1"])  # p-value = 3.791e-15, 0.2343167

round(cor(acath2[,-5]),3)
# il cor.test si pu? applicare se c'? almeno la variabile risposta che richiede la noralit?,
# in questo caso abbiamo applicato il cor.test con e senza divisione per sesso ed ? risultato
# per entrambi che la correlazione ? significativa poich? rifiutiamo H0, cio? l'ipotesi di rho = 0
# (???)

################################################################################

# ANALISI MULTIVARIATA
# REGRESSIONE LOGISTICA

# DA SPECIFICARE: analisi in avanti/analisi all'indietro
# se facciamo analisi all'indietro possiamo usare anche test alla Wald
# proviamo a fare un'analisi all'indietro perch? n ? abbastanza elevato rispetto a p
# e tutte le variabili sono correlate alla variabile risposta Y (sigdz)

# modello di partenza 
# sigdz ~ ber(pi)
# eta = b0 + b1*sex + b2*age + b3*cad.dur + b4*tvdlm
# logit(pi)=eta

# logit   # SBAGLIATO!!!
fitT = glm(sigdz ~ as.factor(sex)+age+cad.dur+as.factor(tvdlm), family=binomial, data=acath2)
summary(fitT)
# nel summary abbiamo che b4 ovvero "as.factor(tvdlm)1" presenta un SE molto alto
# rispetto ai precedenti che hanno SE quasi nullo, e presenta anche p-value non significativo
# questo infatti non ? coerente ed ? un sintomo di non convergenza

fit0 = glm(sigdz~1,binomial)  
anova(fit0,fitT,test="Chisq")  # RIFIUTIAMO POTESI MODELLO NULLO-> MEGLIO CORRENTE

# DA RIVEDERE !!! (FORSE SBAGLIATO)
res=residuals(fitT)
qqnorm(res)
qqline(res)

# Y = -2.37 - b1*1.68 + 0.061*age - 0.0021*cad.dur + 19.12*tvdlm
# AIC = 2879.6

# probit
#fitT2 = glm(sigdz ~ as.factor(sex)+age+cad.dur+as.factor(tvdlm), binomial(link="probit")) 
#summary(fitT2)    # ma b4 non ? significativo
# AIC = 2881.66

####################################################################
####################################################################


# PROVIAMO QUINDI CON LA REGRESSIONE LOGISTICA PENALIZZATA # IN TEORIA SBAGLIATO!!!
library(logistf)
fitpenalized <- logistf(sigdz ~ as.factor(sex)+age+cad.dur+as.factor(tvdlm),
                        family=binomial,data=acath2)
summary(fitpenalized)
modb = backward(fitpenalized,data=acath2)   # questa funzione toglie gi? le variabili
summary(modb)  # MODELLO FINALE modb


# AIC = 1589.167

# MODELLO STIMATO
# Y = -2.366 - 1.683*sex + 0.061*age - 0.002*cad.dur + 7.392*tvdlm


# FUNZIONE DI LEGAME (logit) (GUARADRE SUL LIBRO FORMULA COMPLETA)

# (exp(-2.366 - 1.683*sex + 0.061*age - 0.002*cad.dur + 7.392*tvdlm))
# ______________________________________________________________________

# (1+exp(-2.366 - 1.683*sex + 0.061*age - 0.002*cad.dur + 7.392*tvdlm))

# AIC = 1589.167

########################################################################################
############################ al livello 0.01 ci toglie la variabile cad.dur ############
modb_alpha001 = backward(fitpenalized,data=acath2,slstay=0.01)   
summary(modb_alpha001)
########################################################################################

confint(modb)   # IDC per il modello modb
#                      Lower 95%     Upper 95%
# (Intercept)        -2.854420528  -1.8859586268
# as.factor(sex)1    -1.879010115  -1.4911068876
# age                 0.051526686   0.0715856310
# cad.dur            -0.003939208  -0.0003793082
# as.factor(tvdlm)1   5.476953541   12.2274914677


# ogni fattore di rischio aumenta la classificazione
##  QUOTA PER SEX --> ODDS RATIO
exp(-1.683) # la quota per le femmine ? 0.1858157,
# cio? ? inferiore dell'81.5% rispetto alla quota dei maschi
exp(-1.683 - 1.96*0.0988)  # 0.1531024
exp(-1.683 + 1.96*0.0988)  # 0.2255187
# IDC ODDS RATIO = [0.153 ; 0.225] 

## QUOTA PER AGE
exp(0.061) # la quota per gli anni ? 1.062
# cio? ad ogni anno in pi? la quota aumenta di 1.062
# quindi mantenendo tutte le altre variabili costanti un OR di 1.06 
# suggerisce che l'odds di avere la malattia aumenta del 6% 
# per ogni incremento unitario dell'et? (anni)
exp(0.061 - 1.96*0.0051)  # 1.052327
exp(0.061 + 1.96*0.0051)  # 1.073577
# IDC AGE = [1.052 ; 1.073] --> quindi dal 5 al 7 %

## QUOTA PER CAD.DUR
exp(-0.002152835) # la quota per la durata (cad.dur) ? pari a 0.9979 (=1)
exp(-0.0021 - 1.96*0.0009)  # 0.9961435
exp(-0.0021 + 1.96*0.0009)  # 0.9996641
# IDC CAD.DUR = [0.9961 ; 0.9996]

# DA RIVEDERE !!!
## QUOTA PER tvdlm
exp(7.392) # la quota per tvdlm ? 1622.949 -> PUO' ESSERE COERENTE??? ###
exp(7.392 - 1.96*1.413)  # 101.7501
exp(7.392 + 1.96*1.413)  # 25886.58


table(sigdz,predict(modb)>0.5)
# sigdz   FALSE     TRUE
#    0   942(TN)  227(FP)
#    1   593(FN)  1739(TP)

## ACCURATEZZA DEL TEST
# (TP+TN)/(TP+FN+FP+TN)
(1739+942)/(942+227+593+1739) # = 0.7657812 = 76.6 %

## SENSIBILITA'
# TP/(TP+FN)
1739/(1739+593) # = 0.7457118  -->  1-0.7457118 = 0.254 --> SI SBAGLIA DEL 25.4%
# NB: Un test diagnostico ? sensibile al 100% quando tutti i malati risultano positivi

## SPECIFICITA'
# TN/(FP+TN)
942/(227+942) # = 0.8058169 --> 1-0.8058169 = 0.1941 --> SI SBAGLIA DEL 19.4 %
# NB: Un test disgnostico  ? specifico al 100% quando tutti i sani risultano negativi

## PREVALENZA
# (TP+FN)/(TP+FN+FP+TN)
(1739+593)/(942+227+593+1739) # = 0.6660954 
# NB: ? il rapporto tra il numero di malati e la totalit? della popolazione


table(sigdz,predict(modb,type="response")>0.5)
# sigdz    FALSE      TRUE
#   0     744(TN)    425(FP)
#   1     324(FN)   2008(TP)

## ACCURATEZZA DEL TEST
# (TP+TN)/(TP+FN+FP+TN)
(2008+744)/(744+425+324+2008) # = 0.7860611 = 78.6 %

## SENSIBILITA'
# TP/(TP+FN)
2008/(2008+324) # = 0.8610635  -->  1-0.8610635 = 0.138 --> SI SBAGLIA DEL 13.8%
# NB: Un test diagnostico ? sensibile al 100% quando tutti i malati risultano positivi

## SPECIFICITA'
# TN/(FP+TN)
744/(425+744) # = 0.6364414 --> 1-0.6364414 = 0.363 --> SI SBAGLIA DEL 36.3 %
# NB: Un test disgnostico  ? specifico al 100% quando tutti i sani risultano negativi

## PREVALENZA
# (TP+FN)/(TP+FN+FP+TN)
(2008+324)/(744+425+324+2008) # = 0.6660954 --> 1-0.6660954 = 0.333 --> SI SBAGLIA DEL 33.3%
# NB: ? il rapporto tra il numero di malati e la totalit? della popolazione

# CURVA ROC
predfit=predict(modb)
library(verification)
roc.plot(sigdz,predfit)
roc.area(sigdz,predfit)   # area sotto la curva = 0.863903

# DA CHIEDERE !!!
library(ResourceSelection)         
hoslem.test(modb$y,fitted(modb)) 

# (exp(-2.366 - 1.683*sex + 0.061*age - 0.002*cad.dur + 7.392*tvdlm))
# ______________________________________________________________________

# (1+exp(-2.366 - 1.683*sex + 0.061*age - 0.002*cad.dur + 7.392*tvdlm))



# PROBABILITA' DI RISCHIO IN FUNZIONE DELL'ETA' E SESSO
plot(age,sigdz,main="Grafico probabilit? malattia age in funzione di sex")
# maschi
curve((exp(-2.366 + 0.061*x)/(1+exp(-2.366 + 0.061*x))),add=T,col="blue",lwd=2)
# femmine
curve((exp(-2.366 - 1.683 + 0.061*x)/(1+exp(-2.366 - 1.683 + 0.061*x))),add=T,col=2,lwd=2)
legend("bottomright",legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)


# PROBABILITA' DI RISCHIO IN FUNZIONE DELLA DURATA (cad.dur)
plot(cad.dur,sigdz)
curve((exp(-2.366 - 0.002*x)/(1+exp(-2.366 - 0.002*x))),add=T)
curve((exp(-2.366 - 1.683 - 0.002*x)/(1+exp(-2.366 - 1.683 - 0.002*x))),add=T)


###################### MODELLO CON TUTTE DICOTOMICHE ###########################

# MODELLO SOLO DI PROVA
age1=age<median(age)
age1
cad.dur1=cad.dur<median(cad.dur)
cad.dur1
age_01=as.numeric(age1)
age_01
cad.dur_01=as.numeric(cad.dur1)
cad.dur_01
fit1 = glm(sigdz ~ as.factor(sex)+as.factor(age1)+as.factor(cad.dur1), family=binomial, data=acath2)
summary(fit1)

drop1(fit1,test="Chisq")
fit2=update(fit1,.~.-as.factor(cad.dur_01))
summary(fit2)

fit3=update(fit2,.~.-as.factor(tvdlm))
summary(fit3)   # AIC = 3821.1 

########
# MODELLO CON REGRESSIONE LOGISTICA PENALIZZATA TUTTE DICOTOMICHE # DA TOGLIERE
########
library(logistf)
fitpenalized1 <- logistf(sigdz ~ as.factor(sex)+as.factor(age_01)+as.factor(cad.dur_01)+tvdlm,
                         family=binomial,data=acath2)
summary(fitpenalized1)
modb1 = backward(fitpenalized1,data=acath2)   # questa funzione toglie gi? le variabili
summary(modb1) 
# AIC = 1524.831

# MODELLO STIMATO (modb1)
# Y = 1.113 - 1.59*sex(1) -0.893*age_01(1) + 7.451*tvdlm

confint(modb1)

table(sigdz,predict(modb1)>0.5)
# sigdz    FALSE      TRUE
# 0       1027(TN)   142(FP)
# 1       749(FN)    1583(TP)

## ACCURATEZZA DEL TEST
# (TP+TN)/(TP+FN+FP+TN)
(1583+1027)/(1027+142+749+1583) # = 0.7455013 = 74.5 %

## SENSIBILITA'
# TP/(TP+FN)
1583/(1583+749) # = 0.6788165 -->  1-0.6788165 = 0.3211  --> SI SBAGLIA DEL 32.1 %
# NB: Un test diagnostico ? sensibile al 100% quando tutti i malati risultano positivi

## SPECIFICITA'
# TN/(FP+TN)
1027/(142+1027) # = 0.8785287 --> 1-0.8785287 = 0.1214 --> SI SBAGLIA DEL 12.1 %
# NB: Un test disgnostico  ? specifico al 100% quando tutti i sani risultano negativi

## PREVALENZA
# (TP+FN)/(TP+FN+FP+TN)
(1583+749)/(1027+142+749+1583) # = 0.6660954 
# NB: ? il rapporto tra il numero di malati e la totalit? della popolazione

# CURVA ROC
predfit1=predict(modb1)
library(verification)
roc.plot(sigdz,predfit1)
roc.area(sigdz,predfit1)  # AUC = 0.8499656


# (exp(1.113 - -1.59*sex(1) - 0.893*age_01(1) + 7.451*tvdlm)
# ______________________________________________________________________

# (1 + (exp(1.113 - -1.59*sex(1) - 0.893*age_01(1) + 7.451*tvdlm)))

plot(age_01,sigdz)
curve((exp(1.113 - 0.893*x)/(1+exp(1.113 - 0.893*x))),add=T)


# ogni fattore di rischio aumenta la classificazione
##  QUOTA PER SEX(1) --> ODDS RATIO
exp(-1.59) # la quota per le femmine ? 0.2039256,
# cio? l'80% di rischio in meno rispetto ai maschi
exp(-1.59 - 1.96*0.0961)  # 0.1689156
exp(-1.59 + 1.96*0.0961)  # 0.2461919
# IDC ODDS RATIO = [0.168 ; 0.246] 

## QUOTA PER AGE_01(1) 1=age<52
exp(-0.8935)  # la quota per gli anni ? 0.409221
# cio? la quota per i pazienti con meno di 52 anni ? di 0.4
exp(-0.8935 - 1.96*0.092)  # 0.3417007
exp(-0.8935 + 1.96*0.092)  # 0.4900833
# IDC AGE_01 = [0.341 ; 0.49]

# DA RIVEDERE !!! -> FORSE SBAGLIATO !!!
## QUOTA PER tvdlm
exp(7.451) # la quota per tvdlm ? 1721.584 -> PUO' ESSERE COERENTE??? ###
exp(7.451 - 1.96*1.414)  # 107.7227
exp(7.451 + 1.96*1.414)  # 27513.72
# IDC = [107.7 ; 27513.72]

###########################################################################
###########################################################################
# MOELLO SENZA tvdlm --> ANALISI ALL'INDIETRO (GIUSTO)

m1 = glm(sigdz ~ as.factor(sex)+age+cad.dur, family=binomial, data=acath2)
summary(m1)

# ANALISI ALL'INDIETRO

drop1(m1,test="Chisq")
m2 = update(m1,.~.-cad.dur)
drop1(m2,test="Chisq")

summary(m2)  # modello finale

# Y = -2.575 - 1.936*sex + 0.077*age

# funzione di legame logit
# exp(-2.57 - 1.936*sex(1) + 0.077*age)/(1+exp(-2.57 - 1.936*sex(1) + 0.077*age))

confint(m2)

# Confronto con modello sola intercetta
m0 = glm(sigdz~1,binomial)  
anova(m0,m2,test="Chisq")   # Rifiuto ipotesi di modello con sola intercetta
# quinid MEGLIO MODELLO CORRENTE

# BONTA' DEL MODELLO (VALIDAZIONE DEL MODELLO)
# (dev res < n-p)

# test con modello nullo
woss = 4459.6-3694.9   # dev_null - dev_res
woss
1-pchisq(woss,2)  # rifiuto modello nullo p=0


# TABELLA DI CORRETTA CLASSIFICAZIONE
table(sigdz,fitted(m2,type="response")>0.5)
# sigdz     FALSE      TRUE
# 0        559(TN)    610(FP)
# 1        268(FN)    2064(TP)

## ACCURATEZZA DEL TEST
# (TP+TN)/(TP+FN+FP+TN)
(2064+559)/(559+610+268+2064) # = 0.7492145 = 74.9 %

## SENSIBILITA'
# TP/(TP+FN)
2064/(2064+268) # = 0.8850772 -->  1-0.8850772 = 0.1149228  --> SI SBAGLIA DEL 11.5 %
# NB: Un test diagnostico ? sensibile al 100% quando tutti i malati risultano positivi

## SPECIFICITA'
# TN/(FP+TN)
559/(610+559) # = 0.4781865 --> 1-0.4781865 =  --> SI SBAGLIA DEL 0.5218135 %
# NB: Un test disgnostico  ? specifico al 100% quando tutti i sani risultano negativi

## PREVALENZA
# (TP+FN)/(TP+FN+FP+TN)
(2064+268)/(559+610+268+2064) # = 0.6660954 
# NB: ? il rapporto tra il numero di malati e la totalit? della popolazione

predfit2=predict(m2,type="response")
library(verification)
roc.plot(sigdz,predfit2,xlab="False Positive Rate (1-Specificity)",
         ylab="True Positive Rate (Sensitivity")
roc.area(sigdz,predfit2)  # AUC = 0.7738641
text(0.95,0.01,"AUC = 0.774", add=T)

library(ResourceSelection)         
x2 = hoslem.test(m2$y,fitted(m2))  # X-squared = 18.526, df = 8, p-value = 0.01761
x2
x2$observed
x2$expected
# da chiedere???

# OSSERVO I RESIDUI (che devono essere NORMALI)
res=residuals(m2)
qqnorm(res)
qqline(res)  # da verificare

shapiro.test(res)

plot(res,predfit2)  # in teoria ? corretto

residui_devianza <- residuals(m2, type = "deviance")
plot(predict(m2, type = "response"), residui_devianza, 
     ylab = "Residui Devianza", xlab = "Valori Previsti", 
     main = "Grafico Residui Devianza")


# ogni fattore di rischio aumenta la classificazione
summary(m2)

##  QUOTA PER SEX(1) --> ODDS RATIO
exp(-1.937) # la quota per le femmine ? 0.1442799,
# cio? l'85.6% di rischio in meno rispetto ai maschi
exp(-1.937 - 1.96*0.088)  # 0.1214224
exp(-1.937 + 1.96*0.088)  # 0.1714403
# IDC ODDS RATIO = [0.121 ; 0.171] 

## QUOTA PER AGE
exp(0.077) # la quota per gli anni ? 1.08015
# cio? ad ogni anno in pi? la quota aumenta di 1.08
# quindi mantenendo tutte le altre variabili costanti un OR di 1.08 
# suggerisce che l'odds di avere la malattia aumenta del 8% 
# per ogni incremento unitario dell'et? (anni)
exp(0.077 - 1.96*0.0045)  # 1.070875
exp(0.077 + 1.96*0.0045)  # 1.089506
# IDC AGE = [1.07 ; 1.09] --> quindi dal 7 al 9 %


# funzione di legame logit
# exp(-2.57 - 1.936*sex(1) + 0.077*age)/(1+exp(-2.57 - 1.936*sex(1) + 0.077*age))


# PROBABILITA' MALATTIA FEMMINA DI 70 ANNI
exp(-2.57 - 1.936 + 0.077*70)/(1+exp(-2.57 - 1.936 + 0.077*70)) # 70.76 %
# PROBABILITA' MALATTIA MASCHIO DI 70 ANNI
exp(-2.57 + 0.077*70)/(1+exp(-2.57 + 0.077*70))  # 94.4 %


# Grafico probabilit? di malattia in funzione di age in funzione di sex
plot(age,sigdz,main="Grafico probabilit? malattia age in funzione di sex")
# maschi
curve((exp(-2.57 + 0.077*x)/(1+exp(-2.57 + 0.077*x))),add=T,col="blue",lwd=2) 
# femmine
curve((exp(-2.57 - 1.936 + 0.077*x)/(1+exp(-2.57 - 1.936 + 0.077*x))),add=T,col=2,lwd=2)
legend("bottomright",legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)


#########################################################################
# MODELLO CON tvdlm COME VARIABILE RISPOSTA E SENZA SIGDZ
detach(acath2)
attach(acath3_prova)

# sottoinsieme pazienti solo con sigdz=1
# acath3 <- subset(acath2, sigdz == 1)
# acath3
dim(acath3_prova)   # 2332 pazienti con sigdz=1
table(sigdz,tvdlm) # pazienti con sigdz=1 sono 2332 (OK CORRETTO)

m3 = glm(tvdlm3 ~ as.factor(sex3)+age3+cad.dur3, family=binomial, data=acath3_prova)
summary(m3)

# Y(tvdlm | sigdz=1) = -2.079 - 0.546*sex + 0.034*age + 0.0059*cad.dur

confint(m3)

# Confronto con modello sola intercetta
m0_m3 = glm(tvdlm3~1,binomial,data=acath3_prova)  
anova(m0_m3,m3,test="Chisq")   # Rifiuto ipotesi di modello con sola intercetta
# quini MEGLIO MODELLO CORRENTE

# test con modello nullo
woss = 3230.5-3066.8   # dev_null - dev_res
woss
1-pchisq(woss,3)  # rifiuto modello nullo p=0

# TABELLA DI CORRETTA CLASSIFICAZIONE
table(tvdlm3,fitted(m3,type="response")>0.5)
# tvdlm      FALSE        TRUE
# 0          841(TN)     362(FP)
# 1          546(FN)     583(TP)

## ACCURATEZZA DEL TEST
# (TP+TN)/(TP+FN+FP+TN)
(583+841)/(841+362+546+583) # = 0.6106346 = 61 %

## SENSIBILITA'
# TP/(TP+FN)
583/(583+546) # = 0.5163862 -->  1-0.5163862 =   --> SI SBAGLIA DEL 48.3 %
# NB: Un test diagnostico ? sensibile al 100% quando tutti i malati risultano positivi

## SPECIFICITA'
# TN/(FP+TN)
841/(362+841) # = 0.6990856 --> 1-0.6990856 =  --> SI SBAGLIA DEL 30 %
# NB: Un test disgnostico  ? specifico al 100% quando tutti i sani risultano negativi

## PREVALENZA
# (TP+FN)/(TP+FN+FP+TN)
(583+546)/(841+362+546+583) # = 0.4841338
# NB: ? il rapporto tra il numero di malati e la totalit? della popolazione

predfit3=predict(m3,type="response")  # da mettere response
library(verification)
roc.plot(tvdlm3,predfit3,xlab="False Positive Rate (1-Specificity)",
         ylab="True Positive Rate (Sensitivity)")
roc.area(tvdlm3,predfit3) # AUC = 0.6476203
text(0.95,0.01,"AUC = 0.648", add=T)

library(ResourceSelection)         
x3 = hoslem.test(m3$y,fitted(m3)) # X-squared = 5.8381, df = 8, p-value = 0.6654
# IN TEORIA E' UN BUON MODELLO (?)
x3
x3$observed
x3$expected

# OSSERVO I RESIDUI (che devono essere NORMALI)
res3=residuals(m3)
qqnorm(res3)
qqline(res3)  # da verificare
shapiro.test(res3)

plot(res3,predfit3)  # in teoria ? corretto

summary(m3)

##  QUOTA PER SEX(1) --> ODDS RATIO
exp(-0.546) # la quota per le femmine in presenza di sigdz ? 0.5792622,
# cio? il 42%% di rischio in meno rispetto ai maschi
exp(-0.546 - 1.96*0.111)  # 0.4660045
exp(-0.546 + 1.96*0.111)  # 0.7200461
# IDC ODDS RATIO = [0.466 ; 0.720] 

## QUOTA PER AGE
exp(0.034) # la quota per gli anni in presenza di sigdz ? 1.034895
# cio? ad ogni anno in pi? la quota aumenta di 1.03
# quindi mantenendo tutte le altre variabili costanti con un OR di 1.03 
# suggerisce che l'odds di avere la malattia aumenta del 3% 
# per ogni incremento unitario dell'et? (anni)
exp(0.034 - 1.96*0.005)  # 1.025204
exp(0.034 + 1.96*0.005)  # 1.044677
# IDC AGE = [1.02 ; 1.04] --> quindi dal 2 al 4 %

## QUOTA PER cad.dur
exp(0.006) # la quota per la durata dei sintomi ? 1.005975
# cio? ad ogni anno in pi? la quota aumenta di 1.005
# quindi mantenendo tutte le altre variabili costanti con un OR di 1.005 
# suggerisce che l'odds di avere la malattia aumenta del 0.05% 
# per ogni incremento unitario della durata dei sintomi (anni)
exp(0.006 - 1.96*0.001)  # 1.004349
exp(0.006 + 1.96*0.001)  # 1.007603
# IDC cad.dur = [1.004 ; 1.007] --> quindi dallo 0.4 allo 0.7 %


## GRAFICO PROBABILITA' DI TVDLM IN FUNZIONE DI AGE3 PER MASCHI E FEMMINE 
## CON VALORE DI CAD.DUR FISSATO

## 22 valore dei giorni di sintomi (mediana)
plot(age3,tvdlm3,main="Probabilit? tvdlm in funzione di age per sesso\ncon valore fissato cad.dur = 22 giorni (mediana)",
     xlab = "age",ylab = "tvdlm")
# maschi
curve((exp(-2.079 + 0.0343*x + 0.0059*22)/(1+exp(-2.079 + 0.0343*x + 
                                                   0.0059*22))),add=T,col="blue",lwd=2) 
# femmine
curve((exp(-2.079 - 0.546153 + 0.0343*x + 0.0059*22)/(1+exp(-2.079 - 0.546153 +
                                                              0.0343*x + 0.0059*22))),add=T,col=2,lwd=2)
legend(70.26,0.25,legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)


## 400 valore dei giorni di sintomi (valore massimo)
plot(age3,tvdlm3,main="Probabilit? (tvdlm|sigdz = 1) in funzione di age per sesso\ncon valore fissato cad.dur=400 giorni")
# maschi
curve((exp(-2.079 + 0.0343*x + 0.0059*400)/(1+exp(-2.079 + 0.0343*x + 
                                                   0.0059*400))),add=T,col="blue",lwd=2) 
# femmine
curve((exp(-2.079 - 0.546153 + 0.0343*x + 0.0059*400)/(1+exp(-2.079 - 0.546153 +
                                                              0.0343*x + 0.0059*400))),add=T,col=2,lwd=2)
legend(70.26,0.25,legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)


## GRAFICO PROBABILITA' DI TVDLM IN FUNZIONE DI CAD.DUR3 PER MASCHI E FEMMINE 
## CON VALORE DI AGE3 FISSATO

## 54 anni (mediana)
plot(cad.dur3,tvdlm3,main="Probabilit? tvdlm in funzione di cad.dur per sesso\ncon valore fissato age = 54 anni (mediana)",
     xlab = "cad.dur", ylab = "tvdlm")
# maschi
curve((exp(-2.079 + 0.0343*54 + 0.0059*x)/(1+exp(-2.079 + 0.0343*54 + 
                                                    0.0059*x))),add=T,col="blue",lwd=2) 
# femmine
curve((exp(-2.079 - 0.546153 + 0.0343*54 + 0.0059*x)/(1+exp(-2.079 - 0.546153 +
                                                               0.0343*54 + 0.0059*x))),add=T,col=2,lwd=2)
legend(330.6,0.25,legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)


## 80 anni (valore massimo)
plot(cad.dur3,tvdlm3,main="Probabilit? (tvdlm|sigdz = 1) in funzione di cad.dur per sesso\ncon valore fissato age=80 anni")
curve((exp(-2.079 + 0.0343*80 + 0.0059*x)/(1+exp(-2.079 + 0.0343*80 + 
                                                   0.0059*x))),add=T,col="blue",lwd=2) 
# femmine
curve((exp(-2.079 - 0.546153 + 0.0343*80 + 0.0059*x)/(1+exp(-2.079 - 0.546153 +
                                                              0.0343*80 + 0.0059*x))),add=T,col=2,lwd=2)
legend(330.6,0.25,legend=c("Maschi","Femmine"),col=c("blue","red"),lty=1,lwd=2)





#######################################################
# mod 1 sigdz normale con probit

m1probit = glm(sigdz ~ as.factor(sex)+age+cad.dur, family=binomial(link="probit"),
               data=acath2)
summary(m1probit)
drop1(m1probit,test="Chisq")
m11probit = update(m1probit,.~.-cad.dur)
drop1(m11probit,test="Chisq")
summary(m11probit)   # AIC = 3703.6 a fronte di 3700.9 (mod.definitivo)


# mod 1 sigdz con tutte dicotomiche (SOLO PER AIC IN CONCLUSIONI)

fit4 = glm(sigdz ~ as.factor(sex)+as.factor(age1)+as.factor(cad.dur1), 
           family=binomial, data=acath2)
summary(fit4)    # AIC =3820.7 a fronte di 3700.9 (mod.definitivo)


# mod 2 tvdlm normale con probit
m3probit = glm(tvdlm3 ~ as.factor(sex3)+age3+cad.dur3, 
               family=binomial(link="probit"), data=acath3_prova)
summary(m3probit)   # AIC = 3075.1  a fronte dei 3074.8 del modello logit (definitivo)


# modello 2 tvdlm con tutte dicotomiche (SOLO PER AIC IN CONCLUSIONI!!!)

age5=age3<median(age3)
age5
cad.dur5=cad.dur3<median(cad.dur3)
cad.dur5
age_05=as.numeric(age5)
age_05
cad.dur_05=as.numeric(cad.dur5)
cad.dur_05
fit5 = glm(tvdlm3 ~ as.factor(sex3)+as.factor(age_05)+as.factor(cad.dur_05),
           family=binomial, data=acath3_prova)
summary(fit5)    # AIC = 3107.7 a fronte dei 3074.8 del modello logit (definitivo)




