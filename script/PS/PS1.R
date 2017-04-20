names(imput)
require(cobalt)
require(ggplot2)
source('~/Documents/M2/Memoire/Base/love.R', encoding = 'UTF-8')
comp1<- readRDS("~/Documents/M2/Memoire/Base2/Imputation/comp1.RDS")
names(imput)
names(ReAC2)
imput$Groupe<- ReAC$G

##########################################################################################
######################################## PS 1 ############################################
##########################################################################################

merge1<- merge(imput[, c("Groupe", "Ville.SMUR", "lat.SMUR", "lon.SMUR", 
                         "Ville.inter", "lat.inter","lon.inter", "ID.fiche")], 
               comp1, by.x = "ID.fiche", by.y = "ID.fiche")
############################### PREPARATION BASE ##################################
###################################################################################
                                   # Y = CPC #
str(merge1$CPC.dich)
merge1$CPC.dich<- as.integer(merge1$CPC.dich)
####################################################################################
                                # Tr = TREATMENT #
str(merge1$Groupe)
merge1$Groupe <- as.integer(merge1$Groupe)
####################################################################################
                                # X = COVARIATE #
########## AGE ###########
str(merge1$Age)
boxplot(merge1$Age~merge1$Groupe)
########## SEXE ###########
str(merge1$Sexe)
barplot(prop.table(table(merge1$Sexe, merge1$Groupe),2), legend.text = T)
########## NF ###########
str(merge1$NF)
boxplot(merge1$NF~merge1$Groupe)
########## ATCD ###########
str(merge1$ATCD.CV)
str(merge1$ATCD.Respi)
str(merge1$ATCD.Diabete)
str(merge1$ATCD.FDV)
str(merge1$ATCD.NC)
str(merge1$ATCD.Aucun)
str(merge1$ATCD.Autre)
########## WITNESSED ###########
str(merge1$Temoin)
str(merge1$Temoin.RCP.immediate)
str(merge1$Type.temoin)
########## RCP NON SPE #############
str(merge1$RCP.non.spé)
merge1$RCP.non.spé<- as.factor(merge1$RCP.non.spé)
########## DEA AVANT SMUR ###########
str(merge1$DEA.avant.SMUR)
########## CHOC AVANT SMUR ###########
str(merge1$Choc.avant.SMUR)
########## CAUSE AC ###########
str(merge1$Cause)
merge1$Cause<- as.factor(merge1$Cause)
########## LIEU DE L'AC ###########
str(merge1$Type.lieu)
########## RYTHME ###########
str(merge1$Rythme.SMUR)


#########################################################################################
########################## ANALYSE MULTIVARIE CLASSIQUE #####################################
####################################################################################################
table(merge1$CPC.dich, merge1$Groupe)
wilcox.test(merge1$CPC.dich, merge1$Groupe)

#########################################################################################
################################### MATCHING ############################################
####################################################################################################
require(Matching)
covariates <- subset(merge1, select=c("Age", "Sexe", "NF", "Temoin", "Temoin.RCP.immediate", "DEA.avant.SMUR", 
                                     "Choc.avant.SMUR", "Cause", "Type.lieu", "Rythme.SMUR", "ATCD.CV", "ATCD.Respi", 
                                     "ATCD.FDV", "ATCD.Diabete", "ATCD.Aucun", "ATCD.Autre", "Temoin.RCP.immediate", 
                                     "RCP.non.spé"
))

glm1<- glm(Groupe~Age + Sexe + NF + Temoin + DEA.avant.SMUR + Choc.avant.SMUR + 
            ATCD.CV + ATCD.Respi + ATCD.FDV + ATCD.Diabete + ATCD.Aucun + ATCD.Autre +
            Cause + Type.lieu + Rythme.SMUR + Temoin.RCP.immediate + RCP.non.spé, data= merge1, family = "binomial")
merge1$ps1 <- predict(glm1, type="response")
summary(merge1$ps1)
match1<- Match(Y=NULL, Tr=merge1$Groupe, X=merge1$ps1, M=1, ties = F, replace=T,caliper=0.20)
saveRDS(match1, file = "match1.RDS")
love(bal.tab(match1, treat = merge1$Groupe, covs = covariates, arg = "matching"), stat = "mean.diffs", threshold = .1,
     var.order = "unmatched")

#########################################################################################
################################### MATCHIT ############################################
####################################################################################################
require(MatchIt)
names(m.out10)
summary(m.out10$model$weights)
boxplot(m.out10$model$weights~m.out10$model$data$Groupe)
boxplot(merge1$ps1~merge1$Groupe)
plot(m.out10)
?m.out10 <- matchit(Groupe~Age + Sexe + NF + Temoin + DEA.avant.SMUR + Choc.avant.SMUR + 
                    ATCD.CV + ATCD.Respi + ATCD.FDV + ATCD.Diabete + ATCD.Aucun + ATCD.Autre +
                    Cause + Type.lieu + Rythme.SMUR + Temoin.RCP.immediate + RCP.non.spé,
                  ratio=1, method = "nearest", caliper=.2,
                  data = merge1)
saveRDS(m.out1, file = "m_out1.RDS")
love(bal.tab(m.out10, arg = "matching"), stat = "mean.diffs", threshold = .1,
     var.order = "unmatched")
plot(m.out10, type = "hist")

####################################################################################################
################################################ TWANG ##############################################
####################################################################################################
require(twang)
ps.ReAC10<- ps(Groupe~Age + Sexe + NF + Temoin + DEA.avant.SMUR + Choc.avant.SMUR + 
               ATCD.CV + ATCD.Respi + ATCD.FDV + ATCD.Diabete + ATCD.Aucun + ATCD.Autre +
               Cause + Type.lieu + Rythme.SMUR + Temoin.RCP.immediate + RCP.non.spé,
             data = merge1,
             n.trees = 50000,
             interaction.depth = 2,
             shrinkage = 0.01,
             perm.test.iters = 0,
             stop.method = c("es.mean","ks.max"),
             estimand = "ATE",
             verbose = F)
merge1$w <- get.weights(ps.ReAC1)
plot(ps.ReAC1, plots=6)
summary(merge1$w)
saveRDS(ps.ReAC1, file = "ps_ReAC1.RDS")
boxplot(ps.ReAC1$w$es.mean.ATT~ps.ReAC1$data$Groupe)
love(bal.tab(ps.ReAC1, arg = "weighting"), stat = "mean.diffs", threshold = .1,
     var.order = "unmatched")


####################################################################################################
################################################ IPW ##############################################
####################################################################################################
require(ipw)
temp1<- ipwpoint(exposure = Groupe, family = "binomial", link = "logit", 
                numerator = ~ 1, 
                denominator = ~Age + Sexe + NF + Temoin + DEA.avant.SMUR + Choc.avant.SMUR + 
                  ATCD.CV + ATCD.Respi + ATCD.FDV + ATCD.Diabete + ATCD.Aucun + ATCD.Autre +
                  Cause + Type.lieu + Rythme.SMUR + Temoin.RCP.immediate + RCP.non.spé, 
                data= merge1)


saveRDS(temp1, file = "temp1.RDS")

summary(temp1$num.mod)
temp1$num.mod$weights
boxplot(temp1$den.mod$weights~temp1$den.mod$data$Groupe)

love(bal.tab(temp1$num.mod$data[,c("Age", "Sexe", "NF", "Temoin", "Temoin.RCP.immediate", "DEA.avant.SMUR", 
                                   "Choc.avant.SMUR", "Cause", "Type.lieu", "Rythme.SMUR", "ATCD.CV", "ATCD.Respi", 
                                   "ATCD.FDV", "ATCD.Diabete", "ATCD.Aucun", "ATCD.Autre", "Temoin.RCP.immediate", 
                                   "RCP.non.spé")], , estimand = "ATT",
             treat = temp1$num.mod$data$Groupe, weights = temp1$ipw.weights, arg= "weighting"),
     stat = "mean.diffs", threshold = .1,
     var.order = "unmatched")



