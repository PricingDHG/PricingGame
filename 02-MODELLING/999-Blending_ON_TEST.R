###################################################################################################
######################################## PTF MANIPULATION #########################################
###################################################################################################

# Last Update: 10/07/16
setwd("G:/pricing/")

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

require(dplyr)
require(tidyr)
require(magrittr)
require(FactoMineR)
require(ggplot2)
require(hglm)
require(data.table)
require(stringr)
require(caret)
require(hts)
require(Metrics)
require(ineq)
require(tweedie)
require (statmod)
require(xgboost)
require(randomForest)
library(extraTrees)
setJavaMemory(4000)
library(mlr)
library(glmnet)

###################################################################################################
## STEP 01 : DATA                                                                            ##
###################################################################################################

source("00-DATA_MANIPULATION.R")

###################################################################################################
## STEP 02 : GLM                                                                            ##
###################################################################################################

source("01-GLM_MODELING.R")
charge_pred  = predict(glm_claims,newdata  = ptf_test,type="response") + min_charge -1 
nb_pred      = predict(glm_freq, newdata   = ptf_test,type="response")
PP_pred_glm  = charge_pred* nb_pred

###################################################################################################
## STEP 02 : XGB                                                                            ##
###################################################################################################

source("04-XGBOOST_MODELING.R")
PP_pred_xgb = predict(train.gdbt,sparse_matrix_ptf_test)

###################################################################################################
## STEP 03 : RF                                                                            ##
###################################################################################################

source("05-RF_MODELING.R")
PP_pred_rf = predict(fit_rf,ptf_test)

###################################################################################################
## STEP 04 : XT                                                                            ##
###################################################################################################

source("06-XT_MODELING.R")
PP_pred_xt = predict(fit_xt,sparse_matrix_ptf_test)

###################################################################################################
## STEP 05 : BLENDING                                                                            ##
###################################################################################################

source("99-BLENDING.R")
PP_pred_blend<-coef_blend[1]*PP_pred_glm + 
               coef_blend[2]*PP_pred_xgb + 
               coef_blend[3]*PP_pred_rf + 
               coef_blend[4]*PP_pred_xt

###################################################################################################
## STEP 06 : SAVE                                                                            ##
###################################################################################################

relevel_pred<-2350000/sum(PP_pred_blend)
PP_pred_blend<-PP_pred_blend*relevel_pred

save(PP_pred_blend,file="01-OUTPUTS/PREDICTIONS/PREDICTIONS_BLEND_ON_TEST.Rdata")

###################################################################################################
## STEP 07 : ANALYSIS                                                                            ##
###################################################################################################

PRED_DGH<-data.frame(fread("01-OUTPUTS/ENVOIE_15_09/PRED_DGH.csv", dec=".", sep=","))
base4<-data.frame(fread("99-CONCURRENTS/Base_4.csv", dec=",", sep=";"))
base6<-data.frame(fread("99-CONCURRENTS/Base_6.csv", dec=".", sep=";"))
base6<-base6[1:15553,c(1,3)]
PRED_DGH<-PRED_DGH[1:15553,]

total<-data.frame(num_contrat   = c(1:15553), 
                  pred_dgh      = PRED_DGH[,2], 
                  pred_c1       = base4[,2],
                  pred_c2       = base6[,2],
                  PP_pred_blend = PP_pred_blend[1:15553])


relevel_c1<-sum(total$PP_pred_blend)/sum(total$pred_c1)
relevel_c2<-sum(total$PP_pred_blend)/sum(total$pred_c2)
relevel_dgh<-sum(total$PP_pred_blend)/sum(total$pred_dgh)

total %<>%
  mutate(pred_c1  = pred_c1*relevel_c1,
         pred_c2  = pred_c2*relevel_c2,
         pred_dgh = pred_dgh*relevel_dgh)

summary(total)
#Graphics
total %>% 
  ggplot() +
  geom_point(aes(x=pred_dgh,y=pred_c1)) + 
  geom_abline() +
  ggtitle("Comparaison des predictions")

total %>% 
  ggplot() +
  geom_point(aes(x=pred_dgh,y=pred_c2)) + 
  geom_abline() +
  ggtitle("Comparaison des predictions")

total %>% 
  ggplot() +
  geom_point(aes(x=pred_dgh,y=PP_pred_blend)) + 
  geom_abline() +
  ggtitle("Comparaison des predictions")
