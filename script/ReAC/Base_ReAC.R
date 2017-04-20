setwd("~/Documents/M2/Memoire/Base")
Base <- read.csv("~/Documents/M2/Memoire/Base/Base.csv", sep=";", na.strings = c("NA",-1))
# type d'arret cardiaque
type<- read.csv("~/Documents/M2/Memoire/Base/type.csv", sep=";", na.strings = c("NA",-1))
der<- read.csv("~/Documents/M2/Memoire/Base/derbase.csv", sep=";", na.strings = c("NA",-1))
# merge des 2 tables
Base<- merge(Base, type, by.x = "X.2..1....ID.Fiche.", by.y = "X.1..1....ID.Fiche.")
Base <- merge(Base, der, by.x = "X.2..1....ID.Fiche.", by.y = "ID_Fiche", all.x = F)
source('~/Documents/R/Fonctions/fonction.R', encoding = 'UTF-8')


base<- Base[Base$X.1..Réanimation.entreprise.==1 & !is.na(Base$X.1..Réanimation.entreprise.),]


####################################################################################################
########################################## CREATION DE LA BASE DE TRAVAIL ##########################
####################################################################################################
require(chron) # manipulation des dates
ReAC<- NULL


################# FICHE ID #######################
base$X.2..1....ID.Fiche.<- as.numeric(base$X.2..1....ID.Fiche.)
ReAC$ID.fiche<- as.numeric(base$X.2..1....ID.Fiche.)
str(ReAC)
sum(duplicated(ReAC$ID.fiche)==T) # pas de doublon
sum(is.na(ReAC$ID.fiche)) # pas de NA
ReAC<- data.frame(ReAC) # data.frame de la base

################# VILLE SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.8..1....SMUR.de..")],
  nom ="Ville.SMUR")
table(is.na(ReAC$Ville.SMUR)) # 0 dpt SMUR manquant
# 215 villes de SMUR différentes, ne semble pas y avoir de pb

################### VILLE SMUR GPS ##################
lieux_SMUR <- read.csv("~/Documents/M2/Memoire/Base/lieux_SMUR.csv")
str(lieux_SMUR)
lieux_SMUR<-lieux_SMUR[,c("Var1","lat.SMUR","lon.SMUR")]
names(lieux_SMUR)
lieux_SMUR$Var1<- as.character(lieux_SMUR$Var1)
# points gps des SMUR
ReAC<- merge(ReAC, lieux_SMUR, by.x = "Ville.SMUR", by.y = "Var1")
str(ReAC)

################# VILLE INTER #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.12..1....CommuneI.")],
  nom ="Ville.inter")
table(is.na(ReAC$Ville.inter)) # 0  inter manquant
##### ATTENTION : sum(ReAC$Ville.inter=="false") = 53...
# dans la base d'interet : sum(ReAC2$Ville.inter=="false") = 52

################### VILLE INTER GPS ##################
# 9867 villes d'inter différentes, attention ville idem avec majuscules différentes
Ville_inter <- read.csv("~/Documents/M2/Memoire/Base/Villes.csv")
# points gps des inter
str(Ville_inter)
Ville_inter$Var1<- as.character(Ville_inter$Var1)
Ville_inter<- Ville_inter[,c("Var1","lat.inter","lon.inter")]
sum(is.na(Ville_inter$lat.inter))
ReAC2<- merge(ReAC, Ville_inter, by.x = "Ville.inter", by.y = "Var1")
sum(duplicated(ReAC2$ID.fiche))
ReAC<- ReAC2[which(duplicated(ReAC2$ID.fiche)==F),]
names(ReAC)
ReAC<- ReAC[,c("ID.fiche", "Ville.SMUR", "lat.SMUR", "lon.SMUR","Ville.inter","lat.inter","lon.inter")]

################# SEXE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.13..2....Sexe.")],
  nom ="Sexe")
ReAC$Sexe<- factor(ReAC$Sexe,levels=c("1","2"), labels=c("M","F"))
table(is.na(ReAC$Sexe)) # 0 patients sans Sexe

################# AGE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.14..2....OU.Âge.estimé..")],
  nom ="Age")
ReAC$Age<- as.numeric(ReAC$Age)
boxplot(ReAC$Age~ReAC$Sexe)
table(is.na(ReAC$Age)) # 9 patients sans âge

################# TYPE AC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.3..3.0...Type.d.Arrêt.cardiaque.")],
  nom ="Type.AC")
ReAC$Type.AC<- factor(ReAC$Type.AC, levels=c(1,2), labels=c("Medical", "Traumatique"))
table(is.na(ReAC$Type.AC)) # 0 NA
table(ReAC$Type.AC) # 4910 AC traumatiques

################# APPELANT #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.15..2.1.Appelant...Appelant.")],
  nom ="Appelant")
ReAC$Appelant<- factor(ReAC$Appelant, levels=c(1:5), labels=c("Patient", "Famille", "Prof. de Santé", "Prof. Secours", "Autre"))
table(is.na(ReAC$Appelant))  # 4148 données manquantes

################# NUMERO COMPOSE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.16..2.1.Appelant...N..composé.en.1er.")],
  nom ="Num.comp")
table(ReAC$Num.comp, useNA = "ifany") #0

################# DATE AC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.17..2.2.Horaires.de.la.RCP...Date.de.l.AC.")],
  nom ="Date.AC")
ReAC$Date.AC<- dates(as.character(ReAC$Date.AC), format= "d/m/y")
sum(is.na(ReAC$Date.AC)) # 0

################# HEURE AC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.18..2.2.Horaires.de.la.RCP...Heure.de.l.AC.")],
  nom ="Heure.AC")
ReAC$Heure.AC<- times(ReAC$Heure.AC)
table(is.na(ReAC$Heure.AC)) # 9124 NA

################# HEURE AC ESTIMEE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.22..2.2.Horaires.de.la.RCP...Estimée.")],
  nom ="Heure.AC.estim")
ReAC$Heure.AC.estim<- as.factor(ReAC$Heure.AC.estim)
table(is.na(ReAC$Heure.AC.estim)) # 12015 NA


################# HEURE APPEL #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.25..2.2.Horaires.de.la.RCP...Heure.1er.appel..15.18....T0..")],
  nom ="Heure.appel")
ReAC$Heure.appel<- times(ReAC$Heure.appel)
table(is.na(ReAC$Heure.appel)) # 2 NA

################# PRESENCE TEMOIN #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.23..2.2.Horaires.de.la.RCP...Devant.témoin.")],
  nom ="Temoin")
ReAC$Temoin<- as.integer(ReAC$Temoin)
table(is.na(ReAC$Temoin)) # 2 NA
table(ReAC$Temoin, useNA = "ifany")


################# SI RCP TEMOIN : RCP TEMOIN IMMEDIATE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.66..3.3.Témoins...Si.AC.devant.témoins...RCP.immédiate.")],
  nom ="Temoin.RCP.immediate")
table(is.na(ReAC$Temoin.RCP.immediate)) # 4168 NA
table(ReAC$Temoin==0 & ReAC$Temoin.RCP.immediate==1)# 12 patients massés immediatement sans arrêt devant temoin
table(ReAC$Temoin==1 & is.na(ReAC$Temoin.RCP.immediate))#pas de NA chez les patients avec temoin
# ReAC$Temoin <- ifelse(!is.na(Temoin.RCP.immediate) & ReAC$Temoin.RCP.immediate==1,1,ReAC$Temoin)
table(ReAC$Temoin)
#### POINT A REGARDER #####

################# SI TEMOIN : HEURE RCP TEMOIN #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.27..2.2.Horaires.de.la.RCP...Heure.1er.geste.témoin.")],
  nom ="Heure.RCP.temoin")
ReAC$Heure.RCP.temoin<- times(ReAC$Heure.RCP.temoin)
table(is.na(ReAC$Heure.RCP.temoin), ReAC$Temoin.RCP) # 32553 NA


################# SI RCP TEMOIN : RCP TEMOIN NON IMMEDIATE  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.67..3.3.Témoins...RCP.Témoin.")],
  nom ="Temoin.RCP")

################# SI RCP TEMOIN : RCP TEMOIN NON IMMEDIATE  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.64..3.3.Témoins...Gestes.temoin.")],
  nom ="Type.temoin")

################# CHOIX DU T0  #######################
ReAC$T0<- times(ifelse(ReAC$Heure.AC.estim==0 & !is.na(ReAC$Heure.AC) & !is.na(ReAC$Heure.AC.estim) |
                         is.na(ReAC$Heure.appel) | 
                         ReAC$Heure.AC.estim==1 & !is.na(ReAC$Temoin) & !is.na(ReAC$Heure.AC) & !is.na(ReAC$Heure.AC.estim),
                       ReAC$Heure.AC, ReAC$Heure.appel))
ReAC$T0<- times(ifelse(ReAC$Heure.RCP.temoin<ReAC$T0 & !is.na(ReAC$Heure.RCP.temoin) & !is.na(ReAC$Heure.AC), ReAC$Heure.AC, ReAC$T0 ))
ReAC$T0<- times(ifelse(is.na(ReAC$Heure.AC.estim)&!is.na(ReAC$Temoin)&ReAC$Temoin==1&!is.na(ReAC$Heure.AC), ReAC$Heure.AC, ReAC$T0))
sum(is.na(ReAC$T0))
# On prend l'heure de l'AC si :
# - si l'heure de l'AC n'est pas estimee 
# - si on a pas l'heure d'appel
# - si AC devant temoin, meme si heure de l'AC est notee comme estimee
# - si on a une heure de RCP temoin et que celle ci precede le T0 de l'algorithme precedent
# Sinon on prend l'heure de l'appel

################# CORRECTION HEURE RCP TEMOIN #######################
ReAC$Heure.RCP.temoin<- times(ifelse(ReAC$Temoin==1&!is.na(ReAC$Temoin)&ReAC$Temoin.RCP.immediate==1&!is.na(ReAC$Temoin.RCP.immediate), ReAC$T0, ReAC$Heure.RCP.temoin))
# si AC devant temoin et patient massé immédiatement, heure de RCP temoin = heure de AC
table(is.na(ReAC$Heure.RCP.temoin)) # 26595 NA

################# HEURE DECLENCHEMENT SMUR  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.26..2.2.Horaires.de.la.RCP...Heure.départ.SMUR.")],
  nom ="Heure.decl.SMUR")
require(chron)
ReAC$Heure.decl.SMUR <- times(as.character(ReAC$Heure.decl.SMUR))
table(is.na(ReAC$Heure.decl.SMUR)) # 7019 NA

################# HEURE ARRIVEE SP  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.28..2.2.Horaires.de.la.RCP...Heure.arrivée.SP..ou.secours.professionnel..")],
  nom ="Heure.arr.SP")
ReAC$Heure.arr.SP<- times(ReAC$Heure.arr.SP)
table(is.na(ReAC$Heure.arr.SP)) # 15171 NA

################# HEURE ARRIVEE SMUR  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.29..2.2.Horaires.de.la.RCP...Heure.arrivée.SMUR.")],
  nom ="Heure.arr.SMUR") 
ReAC$Heure.arr.SMUR<- times(ReAC$Heure.arr.SMUR)
table(is.na(ReAC$Heure.arr.SMUR)) # 1 NA
# table(is.na(ReAC2$Heure.arr.SMUR)) # pas dans la base concerne
ReAC[which(is.na(ReAC$Heure.arr.SMUR)),] # Patient ID fiche 50461 : NA sur quasi-toute la ligne
# exclusion du patient?

################# HEURE RASC  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.32..2.2.Horaires.de.la.RCP...Heure.RACS..si.oui.perçu...1.min..")],
  nom ="Heure.RASC")
ReAC$Heure.RASC<- times(ReAC$Heure.RASC)


################# RASC  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.163..4.5.RACS...RACS..pouls.perçu..1min..")],
  nom ="RASC")
table(is.na(ReAC$Heure.RASC), ReAC$RASC)

################# HEURE DECES  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.35..2.2.Horaires.de.la.RCP...Heure.arrêt.réa.décès.")],
  nom ="Heure.DC") 
ReAC$Heure.DC<- times(ReAC$Heure.DC)
sum(is.na(ReAC$Heure.DC)&is.na(ReAC$Heure.RASC)) # 652
# sum(is.na(ReAC2$Heure.DC)&is.na(ReAC2$Heure.RASC)) # 569
# Patients allant de 2013 à 2016...

################# RCP SP  #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.70..3.4.RCP.non.spécialisée.par.premier.intervenant...SP.")],
  nom ="RCP.SP")
table(is.na(ReAC$RCP.SP)) # 8901 NA

################# PRESENCE DE DEA AVANT ARRIVEE DU SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "DEA_avant_SMUR")],
  nom ="DEA.avant.SMUR")
table(is.na(ReAC$DEA.avant.SMUR)) # 0 NA
table(ReAC$DEA.avant.SMUR)

################# CHOC AVANT SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "Choc_avant_SMUR")],
  nom ="Choc.avant.SMUR")
table(is.na(ReAC$Choc.avant.SMUR)) # 18432 NA
table(ReAC$Choc.avant.SMUR, useNA = "ifany")
ReAC$Choc.avant.SMUR<- ifelse(is.na(ReAC$Choc.avant.SMUR), base$X.84..3.5.Défibrillation.avant.l.arrivée.du.SMUR...Choc.s..délivrés., ReAC$Choc.avant.SMUR)
ReAC$Choc.avant.SMUR<- ifelse(is.na(ReAC$Choc.avant.SMUR), base$X.87..3.5.Défibrillation.avant.l.arrivée.du.SMUR...Choc.s..délivrés., ReAC$Choc.avant.SMUR)

################# RYTHME A L'ARRIVEE DU SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.92..4..Prise.en.charge.SMUR...Rythme.initial.")],
  nom ="Rythme.SMUR")
sum(is.na(ReAC$Rythme.SMUR)) #1847 NA

################# REANIMATION PAR SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.96..4..Prise.en.charge.SMUR...Réanimation.SMUR.")],
  nom ="Rea.SMUR")
sum(is.na(ReAC$Rea.SMUR)) # 34
table(ReAC$Rea.SMUR)

################# RIGIDITE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.99..4..Prise.en.charge.SMUR...Rigidité.cadavérique.")],
  nom ="Rigidite")
sum(is.na(ReAC$Rigidite)) # 35
table(ReAC$Rigidite, ReAC$Type.AC, ReAC$Rea.SMUR, useNA = "ifany") # aucun

################# DECES SUR PLACE AVEC SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.167..4.6.Décès...Décès.")],
  nom ="DC.SMUR")
sum(is.na(ReAC$DC.SMUR)) #34
table(ReAC$DC.SMUR)

################# RCP NON SPECIALIEE AVANT SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.69..3.4.RCP.non.spécialisée.par.premier.intervenant...RCP.non.spécialisée.débutée.")],
  nom ="RCP.non.spé")
sum(is.na(ReAC$RCP.non.spé))
table(ReAC$RCP.non.spé, useNA = "ifany")

################# TYPE DE LIEU DE L'AC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.40..3.1.Lieu.de.l.AC...Lieu.")],
  nom ="Type.lieu")
table(is.na(ReAC$Type.lieu)) # 1469 NA

################# ATCD #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.47..3.2.Antécédents.et.Contexte...Cardiovasculaire.")],
  nom ="ATCD.CV")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.48..3.2.Antécédents.et.Contexte...Respiratoire.")],
  nom ="ATCD.Respi")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.49..3.2.Antécédents.et.Contexte...Diabète.")],
  nom ="ATCD.Diabete")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.50..3.2.Antécédents.et.Contexte...Fin.de.vie.")],
  nom ="ATCD.FDV")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.52..3.2.Antécédents.et.Contexte...Aucun.")],
  nom ="ATCD.Aucun")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.51..3.2.Antécédents.et.Contexte...Autre....")],
  nom ="ATCD.Autre")
nb_mq<-function(vecteur){x<-sum(!is.na(vecteur))
return(x)}
ReAC$ATCD<- apply(ReAC[,c("ATCD.CV","ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Autre")],1,nb_mq)
ReAC$ATCD<- ifelse(ReAC$ATCD==0 & is.na(ReAC$ATCD.Aucun), NA, ReAC$ATCD)
sum(is.na(ReAC$ATCD))

################# CONSEILS TELEPHONIQUES #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.65..3.3.Témoins...Conseil.téléphonique...RCP.")],
  nom ="Conseil.tel")
table(is.na(ReAC$Conseil.tel)) # 24244 NA

################# TRANSPORT SMUR #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.176..5..Transport...Patient.transporté.")],
  nom ="Transport.SMUR")
table(ReAC$Transport.SMUR, useNA = "ifany")

################# DECES A ADMISSION #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.189..6....Statut.vital.J0.")],
  nom ="DC.Admission")
table(is.na(ReAC$DC.Admission)) #34 avec 
# - NA sur transport SMUR
# - NA sur CPC
# - NA DC.SMUR
ReAC[which(is.na(ReAC$DC.Admission)),]
table(ReAC$DC.Admission) # 1 vivant / 2 mort

################# CERTIFICAT DE DECES #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.168..4.6.Décès...Certificat.de.décès.")],
  nom ="Certif.DC")

################# PRELEVEMENT COEUR ARRETE #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.244..7....Prélèvement.à.coeur.arrêté.")],
  nom ="prelevement.coeur.arrete")

################# DATE EVALUATION CPC EN REANIMATION #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.209..7....Évaluation.réalisée.le....")],
  nom ="Date.CPC")
ReAC$Date.CPC<- dates(as.character(ReAC$Date.CPC), format= "d/m/y")

################# ETAT PATIENT AU MOMENT CPC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.210..7....correspondant.à....")],
  nom ="Etat.eval.CPC")
ReAC$Etat.eval.CPC <- as.factor(ReAC$Etat.eval.CPC)

################# CPC PATIENT #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.225..7....Cerebral.Performance.Categories..CPC..Vivant.")],
  nom ="CPC")
ReAC$CPC<- as.numeric(ReAC$CPC)
sum(is.na(ReAC$CPC)) #354 NA
table(ReAC$CPC, useNA = "ifany")
table(base$X.225..7....Cerebral.Performance.Categories..CPC..Vivant.)

################# DECES A J30 #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.211..7.1.Décès...Décès.J30.")],
  nom ="DC.J30")
ReAC$DC.J30 <- as.factor(ReAC$DC.J30)

################# CAUSE AC #######################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.212..7....Préciser.la.cause.de.l.A.C..")],
  nom ="Cause.AC.J30")
ReAC$Cause.AC.J30<- as.factor(ReAC$Cause.AC.J30)
table(ReAC$Cause.AC.J30, useNA = "ifany")

################ CAUSE A J0 #####################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.54..3.2.Antécédents.et.Contexte...Cardiaque.")],
  nom ="Contexte.coeur")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.55..3.2.Antécédents.et.Contexte...Neurologique.")],
  nom ="Contexte.neuro")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.56..3.2.Antécédents.et.Contexte...Respiratoire.dyspnée..asphyxie.")],
  nom ="Contexte.respi")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.57..3.2.Antécédents.et.Contexte...Fausse.route.")],
  nom ="Contexte.FR")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.58..3.2.Antécédents.et.Contexte...Intoxication.")],
  nom ="Contexte.intox")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.59..3.2.Antécédents.et.Contexte...Noyade.")],
  nom ="Contexte.noyade")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.60..3.2.Antécédents.et.Contexte......Non.connu....")],
  nom ="Contexte.NC")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.61..3.2.Antécédents.et.Contexte...Autre....")],
  nom ="Contexte.autre")

ReAC$Contexte<- apply(ReAC[,c("Contexte.coeur", "Contexte.neuro", "Contexte.respi", "Contexte.FR", "Contexte.intox", "Contexte.noyade", "Contexte.NC", "Contexte.autre")],1,nb_mq)
table(ReAC$Contexte, useNA = "ifany")
ReAC[which(ReAC$Contexte==4),]

#################### CAUSE A J30 ########################
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.213..7....Coronarien.")],
  nom ="Contexte.J30.coeur")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.214..7....TR.isolé.")],
  nom ="Contexte.J30.TR")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.215..7....Myocardiopathie.")],
  nom ="Contexte.J30.Myocardiopathie")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.216..7....Dysplasie.arythmogène.du.VD.")],
  nom ="Contexte.J30.dysplasieVD")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.218..7....Autre.")],
  nom ="Contexte.J30.coeur.autre")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.219..7....Respiratoire.")],
  nom ="Contexte.J30.respi")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.220..7....Neurologique.")],
  nom ="Contexte.J30.neuro")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.221..7....Noyade.")],
  nom ="Contexte.J30.noyade")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.222..7....Intoxication.")],
  nom ="Contexte.J30.intox")
ReAC<- f(
  m = base[,c("X.2..1....ID.Fiche.",
              "X.223..7....Autre.")],
  nom ="Contexte.J30.autre")
base$X.222..7....Intoxication.
ReAC$Contexte.J30<- apply(ReAC[,c("Contexte.J30.coeur","Contexte.J30.TR","Contexte.J30.Myocardiopathie","Contexte.J30.dysplasieVD","Contexte.J30.coeur.autre","Contexte.J30.respi","Contexte.J30.neuro","Contexte.J30.noyade","Contexte.J30.intox","Contexte.J30.autre")],1,nb_mq)

table(ReAC$Contexte.J30, useNA = "ifany")
ReAC$Cause <- NA
ReAC$Cause<- ifelse(ReAC$Contexte.coeur==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.coeur==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.Myocardiopathie==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.dysplasieVD==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.coeur.autre==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.TR==1, "coeur", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.intox==1, "intox", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.intox==1, "intox", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.noyade==1, "noyade", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.noyade==1, "noyade", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.neuro==1, "neuro", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.neuro==1, "neuro", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.FR==1, "FR", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.FR==1, "FR", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.respi==1, "respi", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.respi==1, "respi", ReAC$Cause)
ReAC$Cause<- ifelse(ReAC$Type.AC=="Traumatique", "trauma", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.autre==1, "autre", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.J30.autre==1, "autre", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte==0&ReAC$Contexte.J30==0, "NC", ReAC$Cause)
ReAC$Cause<- ifelse(is.na(ReAC$Cause)&ReAC$Contexte.NC==1, "NC", ReAC$Cause)
table(ReAC$Cause, useNA = "ifany")
ReAC[ReAC$Contexte.J30==3,]
ReAC[which(ReAC$Contexte.FR==1&ReAC$Contexte.noyade==1),]

################# CORRECTION HEURE DE DECES #######################
sum(!is.na(ReAC$Heure.RASC)&!is.na(ReAC$Heure.DC)&is.na(ReAC$CPC))

which(!is.na(ReAC$Heure.DC)&is.na(ReAC$CPC)&ReAC$DC.Admission==1)
ReAC[1,]
# 8 patients avec !is.na de heure de DC et CPC (1:5) et non DCD avec le SAMU
ReAC$Heure.DC<- times(ifelse(!is.na(ReAC$Heure.DC) & (ReAC$CPC==1|ReAC$CPC==2|ReAC$CPC==3|ReAC$CPC==4|ReAC$CPC==5) & !is.na(ReAC$CPC)  & ReAC$DC.SMUR==0 & !is.na(ReAC$DC.SMUR), NA, ReAC$Heure.DC ))
sum(is.na(ReAC$Heure.DC))
sum(ReAC$DC.Admission==1&ReAC$CPC.dich==1,na.rm=T)

################# CORRECTION DECES A ADMISSION #######################
ReAC$DC.Admission<- ifelse(is.na(ReAC$DC.Admission)&!is.na(ReAC$Heure.DC)&is.na(ReAC$Heure.RASC),2,ReAC$DC.Admission )
# si les patients ont une heure de DC sans heure de RASC alors on considère qu'ils sont DCD
ReAC[which(is.na(ReAC$DC.Admission)),] # il reste 9 patients dont 1 trauma
###### QUE FAIRE DE CES 8 PATIENTS #######

################# CORRECTION CPC #######################
ReAC$CPC<- ifelse(is.na(ReAC$CPC)&ReAC$DC.Admission==2&!is.na(ReAC$DC.Admission), 0,ReAC$CPC)
sum(is.na(ReAC$CPC)) #242 NA
sum(ReAC$CPC==1&!is.na(ReAC$CPC)&ReAC$DC.Admission==2)

################# HEURE DE DEBUT BLS #######################
############################################################
################ A VOIR A DEFINIR ABSOLUEMENT ##############
ReAC$Heure.BLS<- times(ReAC$Heure.arr.SP) 
sum(is.na(ReAC$Heure.BLS)) # 15171
sum(ReAC$Date.arr.SP<ReAC$Date.RCP.temoin, na.rm=T)
ReAC$Heure.BLS<- times(ifelse(!is.na(ReAC$Heure.RCP.temoin),ReAC$Heure.RCP.temoin, ReAC$Heure.arr.SP))
sum(is.na(ReAC$Heure.BLS)) # 8593 NA (si on considere la RCP temoin comme BLS)
ReAC$Heure.BLS<- times(ifelse(is.na(ReAC$Heure.BLS)&!is.na(ReAC$Temoin)&ReAC$Temoin==1&!is.na(ReAC$Temoin.RCP.immediate)&ReAC$Temoin.RCP.immediate==1, ReAC$Heure.AC, ReAC$Heure.BLS))
sum(is.na(ReAC$Heure.BLS)) # 8587 NA
ReAC$Heure.BLS<- times(ifelse(is.na(ReAC$Heure.BLS)&!is.na(ReAC$Temoin)&ReAC$Temoin==1, ReAC$Heure.AC, ReAC$Heure.BLS))
sum(is.na(ReAC$Heure.BLS)) # 6074 NA
ReAC$Heure.BLS<- times(ifelse(is.na(ReAC$Heure.BLS)&(ReAC$Heure.AC > ReAC$Heure.arr.SMUR)&!is.na(ReAC$Heure.AC)&!is.na(ReAC$Heure.arr.SMUR), ReAC$Heure.AC, ReAC$Heure.BLS))
sum(is.na(ReAC$Heure.BLS)) # 5988 NA
ReAC$Heure.BLS<- times(ifelse(is.na(ReAC$Heure.BLS), ReAC$Heure.appel, ReAC$Heure.BLS))


################# HEURE DE DEBUT ALS #######################
ReAC$Heure.ALS<- times(ReAC$Heure.arr.SMUR)
sum(is.na(ReAC$Heure.ALS)) # il en manque toujours 1

################# DATE AC #######################
ReAC$Date.AC<- dates(as.character(base$X.17..2.2.Horaires.de.la.RCP...Date.de.l.AC.), format= "d/m/y")
table(is.na(ReAC$Date.AC)) # pas de NA

################# DATE APPEL #######################
ReAC$Date.appel <- dates(ifelse(((ReAC$Heure.appel-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.appel), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.appel <- dates(ifelse(((ReAC$Heure.appel-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.appel), ReAC$Date.AC+1, ReAC$Date.appel))
ReAC$Date.appel<- dates(ifelse(is.na(ReAC$Heure.appel), NA, ReAC$Date.appel))


################# DATE RCP TEMOIN  #######################
ReAC$Date.RCP.temoin <- dates(ifelse(((ReAC$Heure.RCP.temoin-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.RCP.temoin), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.RCP.temoin <- dates(ifelse(((ReAC$Heure.RCP.temoin-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.RCP.temoin), ReAC$Date.AC+1, ReAC$Date.RCP.temoin))
ReAC$Date.RCP.temoin<- dates(ifelse(is.na(ReAC$Heure.RCP.temoin), NA, ReAC$Date.RCP.temoin))
# algorithme : 
# - pour tout delais entre T0 et RCP témoin de plus de 800 min,  cad que le patient est massé avant son arrêt, date de RCP témoin = date AC - 1
# - pour tout delais entre T0 et RCPtemoin de moins de -800 min, date RCP est date AC + 1
### ATTENTION QUE FAIRE DE CES PATIENTS : ReAC[which(((ReAC$Heure.RCP.temoin-ReAC$T0)*60*24)>800),] ###
# pour les patients masses avant AC, on prend quand meme heure de l'AC?

################# DATE DECLENCHEMENT SMUR  #######################
ReAC$Date.decl.SMUR <- dates(ifelse(((ReAC$Heure.decl.SMUR-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.decl.SMUR), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.decl.SMUR <- dates(ifelse(((ReAC$Heure.decl.SMUR-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.decl.SMUR), ReAC$Date.AC+1, ReAC$Date.decl.SMUR))
ReAC$Date.decl.SMUR<- dates(ifelse(is.na(ReAC$Heure.decl.SMUR), NA, ReAC$Date.decl.SMUR))

################# DATE ARRIVEE SP  #######################
ReAC$Date.arr.SP <- dates(ifelse(((ReAC$Heure.arr.SP-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.arr.SP), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.arr.SP <- dates(ifelse(((ReAC$Heure.arr.SP-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.arr.SP), ReAC$Date.AC+1, ReAC$Date.arr.SP))
ReAC$Date.arr.SP<- dates(ifelse(is.na(ReAC$Heure.arr.SP), NA, ReAC$Date.arr.SP))

################# DATE ARRIVEE SMUR  #######################
ReAC$Date.arr.SMUR <- dates(ifelse(((ReAC$Heure.arr.SMUR-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.arr.SMUR), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.arr.SMUR <- dates(ifelse(((ReAC$Heure.arr.SMUR-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.arr.SMUR), ReAC$Date.AC+1, ReAC$Date.arr.SMUR))
ReAC$Date.arr.SMUR<- dates(ifelse(is.na(ReAC$Heure.arr.SMUR), NA, ReAC$Date.arr.SMUR))

################# DATE RASC  #######################
ReAC$Date.RASC <- dates(ifelse(((ReAC$Heure.RASC-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.RASC), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.RASC <- dates(ifelse(((ReAC$Heure.RASC-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.RASC), ReAC$Date.AC+1, ReAC$Date.RASC))
ReAC$Date.RASC<- dates(ifelse(is.na(ReAC$Heure.RASC), NA, ReAC$Date.RASC))

################# DATE DECES  #######################
ReAC$Date.DC <- dates(ifelse(((ReAC$Heure.DC-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.DC), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.DC <- dates(ifelse(((ReAC$Heure.DC-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.DC), ReAC$Date.AC+1, ReAC$Date.DC))
ReAC$Date.DC<- dates(ifelse(is.na(ReAC$Heure.DC), NA, ReAC$Date.DC))

################# DATE DE DEBUT BLS #######################
ReAC$Date.BLS <- dates(ifelse(((ReAC$Heure.BLS-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.BLS), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.BLS <- dates(ifelse(((ReAC$Heure.BLS-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.BLS), ReAC$Date.AC+1, ReAC$Date.BLS))
ReAC$Date.BLS<- dates(ifelse(is.na(ReAC$Heure.BLS), NA, ReAC$Date.BLS))


################# DATE DE DEBUT ALS #######################
ReAC$Date.ALS <- dates(ifelse(((ReAC$Heure.ALS-ReAC$T0)*60*24)>800 & !is.na(ReAC$Heure.ALS), ReAC$Date.AC-1, ReAC$Date.AC))
ReAC$Date.ALS <- dates(ifelse(((ReAC$Heure.ALS-ReAC$T0)*60*24)<(-800) & !is.na(ReAC$Heure.ALS), ReAC$Date.AC+1, ReAC$Date.ALS))
ReAC$Date.ALS<- dates(ifelse(is.na(ReAC$Heure.ALS), NA, ReAC$Date.ALS))


################# DATE SOUS FORME DE JOUR #######################
ReAC$Jour.AC<- chron(dates=ReAC$Date.AC, times=ReAC$T0)
ReAC$Jour.appel<- chron(dates=ReAC$Date.appel, times=ReAC$Heure.appel)
ReAC$Jour.RCP.temoin<- chron(dates=ReAC$Date.RCP.temoin, times=ReAC$Heure.RCP.temoin)
ReAC$Jour.decl.SMUR<- chron(dates=ReAC$Date.decl.SMUR, times=ReAC$Heure.decl.SMUR)
ReAC$Jour.arr.SMUR<- chron(dates=ReAC$Date.arr.SMUR, times=ReAC$Heure.arr.SMUR)
ReAC$Jour.arr.SP<- chron(dates=ReAC$Date.arr.SP, times=ReAC$Heure.arr.SP)
ReAC$Jour.RASC<- chron(dates=ReAC$Date.RASC, times=ReAC$Heure.RASC)
ReAC$Jour.DC<- chron(dates=ReAC$Date.DC, times=ReAC$Heure.DC)
ReAC$Jour.BLS<- chron(dates=ReAC$Date.BLS, times=ReAC$Heure.BLS)
ReAC$Jour.ALS<- chron(dates=ReAC$Date.ALS, times=ReAC$Heure.ALS)

################# CALCUL DES DELAIS D'ARRIVEE #######################
sum(is.na(ReAC$Date.BLS))
ReAC$del.BLS<- (ReAC$Jour.BLS-ReAC$Jour.AC)*24*60
ReAC$del.BLS<- ifelse(ReAC$del.BLS<0, 0, ReAC$del.BLS)
ReAC$del.ALS<- (ReAC$Jour.ALS-ReAC$Jour.AC)*24*60
ReAC$del.ALS<- ifelse(ReAC$del.ALS<0, 0, ReAC$del.ALS)
ReAC$del<- ReAC$del.ALS - ReAC$del.BLS
ReAC$del<- ifelse(ReAC$del<0, 0, ReAC$del)

################# NO FLOW #######################
ReAC$NF<- ReAC$del.BLS
sum(is.na(ReAC$NF))
summary(ReAC$NF)
ReAC[which(ReAC$NF<0),]
hist(ReAC[which(ReAC$NF<20),"NF"], breaks = (40), xlim=c(0,20), probability = T)
hist(ReAC[which(ReAC$LF<20&ReAC$LF>0),"LF"], breaks = (40), xlim=c(0,20), probability = T)

################# LOW FLOW #######################
ReAC$LF<- (ReAC$Jour.RASC-ReAC$Jour.AC)*24*60
summary(ReAC$LF)
sum(ReAC$LF<0, na.rm=T)
ReAC$LF<- ifelse(ReAC$LF<0, ReAC$NF, ReAC$LF)

################# CORRECTION HEURE DE DC #######################
ReAC$Heure.DC<- times(ifelse(ReAC$CPC==1&!is.na(ReAC$CPC), NA, ReAC$Heure.DC))
ReAC$Heure.DC<- times(ifelse(ReAC$CPC==2&!is.na(ReAC$CPC), NA, ReAC$Heure.DC))
ReAC$Heure.DC<- times(ifelse(ReAC$CPC==3&!is.na(ReAC$CPC), NA, ReAC$Heure.DC))
ReAC$Heure.DC<- times(ifelse(ReAC$CPC==4&!is.na(ReAC$CPC), NA, ReAC$Heure.DC))
ReAC$Heure.DC<- times(ifelse(ReAC$CPC==5&!is.na(ReAC$CPC), NA, ReAC$Heure.DC))
ReAC$Date.DC<- dates(ifelse(is.na(ReAC$Heure.DC), NA, ReAC$Date.DC))
ReAC$Jour.DC<- chron(dates=ReAC$Date.DC, times=ReAC$Heure.DC)


################# CPC D'INTERET #######################
ReAC$CPC.dich<- ifelse(ReAC$CPC==0 & !is.na(ReAC$CPC), 0, NA)
ReAC$CPC.dich<- ifelse(ReAC$CPC==1 & !is.na(ReAC$CPC), 1, ReAC$CPC.dich)
ReAC$CPC.dich<- ifelse(ReAC$CPC==2 & !is.na(ReAC$CPC), 1, ReAC$CPC.dich)
ReAC$CPC.dich<- ifelse(ReAC$CPC==3 & !is.na(ReAC$CPC), 0, ReAC$CPC.dich)
ReAC$CPC.dich<- ifelse(ReAC$CPC==4 & !is.na(ReAC$CPC), 0, ReAC$CPC.dich)
ReAC$CPC.dich<- ifelse(ReAC$CPC==5 & !is.na(ReAC$CPC), 0, ReAC$CPC.dich)
table(ReAC$CPC.dich)

################# DELAIS D'APPEL #######################
ReAC$del.appel<- (ReAC$Jour.appel-ReAC$Jour.AC)*24*60
summary(ReAC$del.appel)
ReAC$del.appel<- ifelse(ReAC$del.appel<0, 0, ReAC$del.appel)

################# ORGANISATION DEL ROSC ET DC #######################
ReAC$RASC<- ifelse(!is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC), 1, NA)
table(ReAC$RASC, useNA = "ifany")
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&!is.na(ReAC$Heure.DC),0,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&!is.na(ReAC$Heure.RASC)&!is.na(ReAC$Heure.DC),0,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==0
                   ,0,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==1
                   ,1,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==2
                   ,1,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==3
                   ,1,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==4
                   ,1,ReAC$RASC)
ReAC$RASC<- ifelse(is.na(ReAC$RASC)&is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC==5
                   ,1,ReAC$RASC)
# les 10 NA restant sont tous DCD à l'admission donc
ReAC$RASC<- ifelse(is.na(ReAC$RASC), 0 , ReAC$RASC)

sum(is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC)&ReAC$CPC.dich==1, na.rm=T)
sum(is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC))
table(ReAC[is.na(ReAC$Heure.RASC)&is.na(ReAC$Heure.DC),"CPC"], useNA = "ifany")
names(ReAC)
################# ORGANISATION DEL ROSC ET DC #######################
ReAC$delais<- ifelse(ReAC$RASC==1, (ReAC$Jour.RASC-ReAC$Jour.AC)*24*60, NA)
ReAC$delais<- ifelse(ReAC$RASC==0, (ReAC$Jour.DC-ReAC$Jour.AC)*24*60, ReAC$delais)
sum(is.na(ReAC2$delais))
boxplot(ReAC$delais~ReAC$RASC)#, ylim=c(0,70))
ReAC[which(ReAC$delais< 0 & ReAC$delais> (- 10)),]
# exlusion des patients avec un NF négatif = erreur de saisie
ReAC[which(ReAC$delais>60&ReAC$RASC==1),c("Heure.AC", "Heure.appel","T0","Temoin", "Heure.AC.estim","Heure.RASC", "delais"  )]
# exclusion des patients avec un LF>150 = erreur de saisie




####################################################################################################
########################################## BASE DE TRAVAIL DEFINITIVE ##########################
####################################################################################################


################# BASE DE TRAVAIL : CRITERE D'INCLUSION #######################
# ADULTE? ENFANT? : les deux sont pris en compte pour le moment
############## AC MEDICAL
ReAC2<- ReAC[which(ReAC$Type.AC=="Medical"&!is.na(ReAC$CPC)),]
# il reste 41702 patients
############## ADULTE
ReAC2<- ReAC2[which(ReAC2$Age>17), ]
# il en reste 40983
############# AC REANIME PAR LE SMUR
# ReAC2<- ReAC2[which(ReAC2$Rea.SMUR==1),]
# il reste 35787 patients
a<-ReAC2[which(ReAC2$CPC.dich==1 & ReAC2$Rea.SMUR==0),]
############# NF < 3 MINUTES 
ReAC2<- ReAC2[which(ReAC2$NF<=3),]
ReAC[which(ReAC$CPC.dich==1 & ReAC$NF>20),]
# il reste 36232 patients
############# 0 < delais < 150 (considérés comme non AC)
ReAC2<- ReAC2[which(ReAC2$delais > 0 & ReAC2$delais < 150 | is.na(ReAC2$delais)),]
# il reste 35972 patients

# MOYENNE DES AC DEPOSE VIVANTS A L'HOPITAL : 
mean(ReAC2[ReAC2$DC.Admission==1, "NF"], na.rm=T)

################# FORMATION GROUPE BLS + ALS vs BLS + ALS ou ALS seule #######################
ReAC2$Groupe<- ifelse((ReAC2$del.ALS-ReAC2$del.BLS)<=0, 1, NA)
ReAC2$Groupe<- ifelse((ReAC2$del.ALS-ReAC2$del.BLS)>0, 0, ReAC2$Groupe)
ReAC2$Groupe<- ifelse(is.na(ReAC2$Groupe)&ReAC2$del.ALS<=0&!is.na(ReAC2$del.ALS), 1, ReAC2$Groupe)
table(ReAC2$Groupe, ReAC2$CPC.dich, useNA = "ifany")
table(ReAC2$Groupe)
barplot(prop.table(table(ReAC2$CPC.dich,ReAC2$Groupe),2))
chisq.test(ReAC2$DC.Admission,ReAC2$Groupe)

ReAC2$Groupe2<- ifelse((ReAC2$del.ALS-ReAC2$del.BLS)<=5, 1, NA)
ReAC2$Groupe2<- ifelse((ReAC2$del.ALS-ReAC2$del.BLS)>5, 0, ReAC2$Groupe2)
ReAC2$Groupe2<- ifelse(is.na(ReAC2$Groupe2)&ReAC2$del.ALS<=5&!is.na(ReAC2$del.ALS), 1, ReAC2$Groupe2)
table(ReAC2$Groupe2, ReAC2$CPC.dich, useNA = "ifany")
table(ReAC2$Groupe2)
barplot(prop.table(table(ReAC2$CPC.dich,ReAC2$Groupe2),2))
chisq.test(ReAC2$DC.Admission,ReAC2$Groupe2)

require(mgcv)
b<- gam(CPC.dich~s(del.ALS), family = binomial, data=ReAC2, na.action = "na.omit")
plot(b, lwd=2)                                                            


setwd("/Users/Temp/Documents/M2/Memoire/Base2/ReAC2")
saveRDS(ReAC, file = "Base_ReAC.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
saveRDS(ReAC2, file = "Base_ReAC2.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

