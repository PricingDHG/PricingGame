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

#Classe_Age_Situ_Cont
base_test %>%
  dplyr::select(Classe_Age_Situ_Cont,
                PRED_FINAL_,
                PRED_GLM,
                PRED_XGB,
                PRED_RF,
                PRED_XT,
                CHARGE_SINISTRE) %>%
  group_by(Classe_Age_Situ_Cont) %>%
  summarize_each(funs(mean)) %>%
    
  ggplot() +
  geom_bar(aes(x=Classe_Age_Situ_Cont,y=CHARGE_SINISTRE), stat="identity", fill="cornflowerblue") +
  
  geom_point(aes(x=Classe_Age_Situ_Cont,y=PRED_FINAL_ , color="PRED_FINAL_")) +
  geom_line(aes(x=Classe_Age_Situ_Cont,y=PRED_FINAL_, group=1 , color="PRED_FINAL_")) +
  
  geom_point(aes(x=Classe_Age_Situ_Cont,y=PRED_GLM, color="PRED_GLM")) +
  geom_line(aes(x=Classe_Age_Situ_Cont,y=PRED_GLM, group=1, color="PRED_GLM")) +
  
  geom_point(aes(x=Classe_Age_Situ_Cont,y=PRED_XGB, color="PRED_XGB")) +
  geom_line(aes(x=Classe_Age_Situ_Cont,y=PRED_XGB, group=1, color="PRED_XGB")) +
  
  geom_point(aes(x=Classe_Age_Situ_Cont,y=PRED_RF, color="PRED_RF")) +
  geom_line(aes(x=Classe_Age_Situ_Cont,y=PRED_RF, group=1, colour="PRED_RF")) +
  
  geom_point(aes(x=Classe_Age_Situ_Cont,y=PRED_XT, color="PRED_XT")) +
  geom_line(aes(x=Classe_Age_Situ_Cont,y=PRED_XT, group=1, color="PRED_XT")) +

  ggtitle("Comparaison des moyennes par groupe")



#Creation_Entr
base_test %>%
  dplyr::select(Creation_Entr,
                PRED_FINAL_,
                PRED_GLM,
                PRED_XGB,
                PRED_RF,
                PRED_XT,
                CHARGE_SINISTRE) %>%
  group_by(Creation_Entr) %>%
  summarize_each(funs(mean)) %>%
  
  ggplot() +
  geom_bar(aes(x=Creation_Entr,y=CHARGE_SINISTRE), stat="identity", fill="cornflowerblue") +
  
  geom_point(aes(x=Creation_Entr,y=PRED_FINAL_ , color="PRED_FINAL_")) +
  geom_line(aes(x=Creation_Entr,y=PRED_FINAL_, group=1 , color="PRED_FINAL_")) +
  
  geom_point(aes(x=Creation_Entr,y=PRED_GLM, color="PRED_GLM")) +
  geom_line(aes(x=Creation_Entr,y=PRED_GLM, group=1, color="PRED_GLM")) +
  
  geom_point(aes(x=Creation_Entr,y=PRED_XGB, color="PRED_XGB")) +
  geom_line(aes(x=Creation_Entr,y=PRED_XGB, group=1, color="PRED_XGB")) +
  
  geom_point(aes(x=Creation_Entr,y=PRED_RF, color="PRED_RF")) +
  geom_line(aes(x=Creation_Entr,y=PRED_RF, group=1, colour="PRED_RF")) +
  
  geom_point(aes(x=Creation_Entr,y=PRED_XT, color="PRED_XT")) +
  geom_line(aes(x=Creation_Entr,y=PRED_XT, group=1, color="PRED_XT")) +
  
  ggtitle("Comparaison des moyennes par groupe")



