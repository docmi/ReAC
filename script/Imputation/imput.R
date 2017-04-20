library(mice)
library(VIM)

setwd("~/Documents/M2/Memoire/Base2")
# imp<- readRDS("Imputation/imp.RDS")
ReAC2<- readRDS("ReAC/Base_ReAC2.rds")

boxplot(ReAC2$delais~ReAC2$RASC, ylim= c(0, 50))
sum(ReAC2$LF==0, na.rm = T)
names(ReAC2)
imput<- ReAC2[, c("ID.fiche", "Ville.SMUR", "lat.SMUR", "lon.SMUR", "Ville.inter", "lat.inter",
                  "lon.inter", "Sexe", "Age", "Type.AC", "Appelant", "Num.comp", "Temoin", 
                  "Temoin.RCP.immediate", "Type.temoin", "DEA.avant.SMUR", 
                  "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé", "Type.lieu", "ATCD.CV",
                  "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre", "ATCD",
                  "DC.Admission", "CPC.dich", "Cause", "Jour.AC", "Jour.appel",
                  "Jour.RCP.temoin", "Jour.RASC", "Jour.DC", "Jour.BLS", "Jour.ALS",
                  "NF", "LF", "Groupe", "delais", "RASC")]
require(mgcv)
ReAC2$del<- ReAC2$del.ALS-ReAC2$del.BLS
ReAC2$del<- ifelse(ReAC2$del<0, 0, ReAC2$del)
hist(ReAC2$del, xlim = c(0,50), breaks = 50)
spline1<- gam(CPC.dich[NF<3]~s(del[NF<3]), family = binomial, data=ReAC2, na.action = "na.omit")
plot(spline1, xlim=c(0,30))
str(ReAC2$del.BLS)
hist(ReAC2$NF)
par(mfrow=c(1,1))
spline2<- gam(CPC.dich~s(del.ALS), family = binomial, data=ReAC, na.action = "na.omit")
plot(spline1, xlim=c(0,30))
par(new = T)
spline3<- gam(CPC.dich~s(del.BLS), family = binomial, data=ReAC, na.action = "na.omit")
plot(spline1, xlim=c(0,30), col = "red")

####################### AGE ######################
sum(is.na(imput$Age)) # 0
hist(imput$Age)

####################### SEXE ######################
sum(is.na(imput$Sexe)) # 0
str(imput$Sexe)

####################### NF ######################
sum(is.na(ReAC$NF)) # 0

########## ATCD ###########
imput$ATCD.CV <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.CV), 0, imput$ATCD.CV))
table(imput$ATCD.CV, useNA = "ifany")
imput$ATCD.Respi <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.Respi), 0, imput$ATCD.Respi))
imput$ATCD.Diabete <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.Diabete), 0, imput$ATCD.Diabete))
imput$ATCD.FDV <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.FDV), 0, imput$ATCD.FDV))
imput$ATCD.Aucun <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.Aucun), 0, imput$ATCD.Aucun))
imput$ATCD.Autre <- as.factor(ifelse(!is.na(imput$ATCD)&is.na(imput$ATCD.Autre), 0, imput$ATCD.Autre))
imput$ATCD<- as.factor(imput$ATCD)

########## WITNESSED ###########
sum(is.na(imput$Temoin)) #0
str(imput$Temoin)
imput$Temoin<- factor(imput$Temoin, levels = c(0, 1), labels = c("absent", "present"))
imput$Type.temoin<- as.factor(imput$Type.temoin)

########## RCP IMMEDIATE ###########
sum(is.na(imput$Temoin)) #0
imput$Temoin.RCP.immediate<- factor(imput$Temoin.RCP.immediate, levels = c(0, 1), labels = c("absent", "present"))

########## DEA AVANT SMUR ###########
table(imput$DEA.avant.SMUR, useNA = "ifany") #0
imput$DEA.avant.SMUR<- factor(imput$DEA.avant.SMUR, levels = c(0, 1), labels = c("absent", "present"))

########## CHOC AVANT SMUR ###########
table(imput$Choc.avant.SMUR, useNA = "ifany") # 13549 soit 40%
imput$Choc.avant.SMUR<- factor(imput$Choc.avant.SMUR, levels = c(0, 1), labels = c("absent", "present"))

########## CAUSE AC ###########
table(imput$Cause, useNA = "ifany")

########## LIEU DE L'AC ###########
table(imput$Type.lieu, useNA = "ifany") # 1233
str(imput$Type.lieu)
imput$Type.lieu <- factor(imput$Type.lieu)

########## TYPE AC ###########
table(imput$Type.AC, useNA = "ifany")

########## Rythme SMUR ###########
table(imput$Rythme.SMUR, useNA = "ifany") #1495
imput$Rythme.SMUR <- factor(imput$Rythme.SMUR)
sum(is.na(imput$Rythme.SMUR))

########## Ville SMUR ###########
sum(is.na(imput$Ville.SMUR)) #0

########## Ville inter ###########
sum(is.na(imput$Ville.inter)) #0


####################################################################################################
###################################### EXPLORATION DES NA #################################
####################################################################################################

md.pattern(imput[, c("ID.fiche", "Ville.SMUR","Ville.inter", "Sexe", "Age", "Type.AC", "Appelant",
                     "Num.comp", "Temoin","Temoin.RCP.immediate", "Type.temoin", 
                     "DEA.avant.SMUR", "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé",
                     "Type.lieu", "ATCD.CV", "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre",
                     "DC.Admission", "CPC.dich", "Cause", "NF", "delais", "RASC")])

89847/(35436*27) #9%

prop.table(table(complete.cases(imput[, c("ID.fiche", "Ville.SMUR","Ville.inter", "Sexe", "Age", "Type.AC", "Appelant",
                                          "Num.comp", "Temoin","Temoin.RCP.immediate", "Type.temoin", 
                                          "DEA.avant.SMUR", "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé",
                                          "Type.lieu", "ATCD.CV", "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre",
                                          "DC.Admission", "CPC.dich", "Cause", "NF", "delais", "RASC")])))
aggr(imput[, c("ID.fiche", "Ville.SMUR","Ville.inter", "Sexe", "Age", "Type.AC", "Appelant",
               "Num.comp", "Temoin","Temoin.RCP.immediate", "Type.temoin", 
               "DEA.avant.SMUR", "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé",
               "Type.lieu", "ATCD.CV", "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre",
               "DC.Admission", "CPC.dich", "Cause", "NF","delais", "RASC")], cex.lab = 1, cex.axis = 0.3, ylab= c("Histogram of missing data", "Combinaisons"), numbers = F,
     sortVars=T, only.miss=T, gap= 3)


####################################################################################################
###################################### IMPUTATION #########################################
####################################################################################################
require(mice)
tmp<- mice(imput[,c("ID.fiche", "Sexe", "Age", "Type.AC", "Appelant",
             "Num.comp", "Temoin","Temoin.RCP.immediate", "Type.temoin", 
             "DEA.avant.SMUR", "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé",
             "Type.lieu", "ATCD.CV", "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre",
             "ATCD", "DC.Admission", "CPC.dich", "Cause", "NF", "delais", "RASC")], maxit = 0)
tmp$method
tmp$predictorMatrix[,"ID.fiche"]<- 0
tmp$predictorMatrix
library(doParallel)
detectCores()


install.packages("Z:/Audigier_Donnees_Manquantes/Hopital_saint_louis/R/MI_multilevel/package_micemd/micemd_1.0.0.tar_130417.gz", repos = NULL, type = "source")

require(digest)

imp2 <- mice(imput[, c("ID.fiche","Sexe", "Age", "Type.AC", "Appelant",
                      "Num.comp", "Temoin","Temoin.RCP.immediate", "Type.temoin", 
                      "DEA.avant.SMUR", "Choc.avant.SMUR", "Rythme.SMUR", "RCP.non.spé",
                      "Type.lieu", "ATCD.CV", "ATCD.Respi", "ATCD.Diabete", "ATCD.FDV", "ATCD.Aucun", "ATCD.Autre",
                      "ATCD", "DC.Admission", "CPC.dich", "Cause", "NF", "delais", "RASC")], 
            predictorMatrix = tmp$predictorMatrix, m=9, maxit=50, seed=1704)


imp2 <- mice.par(imput[, c("ID.fiche","Sexe", "Age", "Type.AC", "Appelant",
                           "Num.comp", "Temoin","Temoin.RCP.immediate")])

plot(imp)
comp1<- complete(imp, 1)
comp2<- complete(imp, 2)
comp3<- complete(imp, 3)
comp4<- complete(imp, 4)
comp5<- complete(imp, 5)
comp6<- complete(imp, 6)
comp7<- complete(imp, 7)
comp8<- complete(imp, 8)
comp9<- complete(imp, 9)


table(comp1$Choc.avant.SMUR)
table(comp2$Choc.avant.SMUR)
table(comp3$Choc.avant.SMUR)
table(comp4$Choc.avant.SMUR)
table(comp5$Choc.avant.SMUR)

setwd("/Users/Temp/Documents/M2/Memoire/Base2/Imputation")
saveRDS(comp1, file = "comp1.RDS")
saveRDS(comp2, file = "comp2.RDS")
saveRDS(comp3, file = "comp3.RDS")
saveRDS(comp4, file = "comp4.RDS")
saveRDS(comp5, file = "comp5.RDS")
saveRDS(comp6, file = "comp6.RDS")
saveRDS(comp7, file = "comp7.RDS")
saveRDS(comp8, file = "comp8.RDS")
saveRDS(comp9, file = "comp9.RDS")
saveRDS(imp, file = "imp.RDS")

