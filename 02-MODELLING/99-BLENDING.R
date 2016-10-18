###################################################################################################
######################################## BLENDING #################################################
###################################################################################################

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################


###################################################################################################
## STEP 01 : IMPORT                                                                          ##
###################################################################################################

load("01-OUTPUTS/PREDICTIONS/PP_PRED_GLM.RData")
load("01-OUTPUTS/PREDICTIONS/PP_PRED_TWEEDIE.RData")
load("01-OUTPUTS/PREDICTIONS/PP_PRED_XGB.RData")
load("01-OUTPUTS/PREDICTIONS/PP_PRED_RF.RData")
load("01-OUTPUTS/PREDICTIONS/PP_PRED_XT.RData")

###################################################################################################
## STEP 02 : CREATE DATA                                                                          ##
###################################################################################################

PRED_FINAL <- data.frame(REAL    = base_test$CHARGE_SINISTRE,
                         GLM     = PP_pred_test_glm,
                         TWEEDIE = PP_pred_test_tweedie,
                         XGB     = PP_pred_test_xgb,
                         RF      = PP_pred_test_rf,
                         XT      = PP_pred_test_xt)


###################################################################################################
## STEP 03 : MODEL BLEND                                                                         ##
###################################################################################################

cor(PRED_FINAL)
#Model
glm_blend<- glm(REAL ~ 
                 GLM + 
                 XGB +
                 RF + 
                 XT ,
               family =gaussian,
               data=PRED_FINAL)

coef_blend<-glm_blend$coefficients[-1]/sum(glm_blend$coefficients[-1])

###################################################################################################
## STEP 04 : BLEND                                                                         ##
###################################################################################################

PRED_FINAL %<>% 
  mutate(FINAL = coef_blend[1]*GLM + 
                 coef_blend[2]*XGB + 
                 coef_blend[3]*RF + 
                 coef_blend[4]*XT)

###################################################################################################
## STEP 05 : ERROR MEASURES                                                                         ##
###################################################################################################

#Notre mod?le
ineq(PRED_FINAL$FINAL)
rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$FINAL)
our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$FINAL)

summary(PRED_FINAL$FINAL)

#PRED_FINAL %>% ggplot() + geom_point(aes(x=GLM,y=RF))

#Mod?le na?f
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))


#All
result<-data.frame(NAIF = c(ineq(mean(base_test$CHARGE_SINISTRE)),
                            rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE)),
                            our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))),
                   
                   GLM  = c(ineq(PRED_FINAL$GLM),
                            rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$GLM),
                            our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$GLM)),
                   
                   TWEE = c(ineq(PRED_FINAL$TWEEDIE),
                            rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$TWEEDIE),
                            our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$TWEEDIE)),
                   
                   XGB  = c(ineq(PRED_FINAL$XGB),
                            rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$XGB),
                            our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$XGB)),
                   
                   RF  = c(ineq(PRED_FINAL$RF),
                            rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$RF),
                            our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$RF)),
                   
                   XT  = c(ineq(PRED_FINAL$XT),
                           rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$XT),
                           our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$XT)),
                   
                   FINAL  = c(ineq(PRED_FINAL$FINAL),
                           rmse(base_test$CHARGE_SINISTRE,PRED_FINAL$FINAL),
                           our_mape(base_test$CHARGE_SINISTRE,PRED_FINAL$FINAL)))


###################################################################################################
## STEP 05 : Compare mean                                                                         ##
###################################################################################################

base_test %<>%
  mutate(PRED_FINAL_= PRED_FINAL$FINAL,
         PRED_GLM   = PRED_FINAL$GLM,
         PRED_XGB   = PRED_FINAL$XGB,
         PRED_RF    = PRED_FINAL$RF,
         PRED_XT    = PRED_FINAL$XT)

plot_mean(var = "Classe_Age_Situ_Cont")
plot_mean(var = "Mode_gestion")
plot_mean(var = "Zone")
plot_mean(var = "Segment")
plot_mean(var = "Fractionnement")
plot_mean(var = "Age_du_vehicule")
plot_mean(var = "FORMULE")
plot_mean(var = "Activite")
plot_mean(var = "Type_Apporteur")
plot_mean(var = "ValeurPuissance")



