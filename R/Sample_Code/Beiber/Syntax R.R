############################################################################################################################
#Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure for Beginners Using SPSS, Stata, and R#
############################################################################################################################


#######################
#VARIABLE DESCRIPTIONS#
#######################
load("Dataset - Keep Calm and Learn Multilevel Logistic Modeling (R).rdata")

attr(d$pupils, "labels") <-  "level-1 identifier"
attr(d$classes, "labels") <-  "level-2 identifier"
attr(d$gpa, "labels")    <-  "level-1 predictor variable"
attr(d$teacher_fan, "labels") <-"level-2 predictor variable"
attr(d$bieber, "labels") <-  "binary outcome variable"

##########################################################################
#PRELIMINARY PHASE: Centering Variables (corresponding to Sub-Appendix A)#
##########################################################################
  
#Below are the commands to grand-mean center "gpa" (i.e., subtracting the general mean of the predictor variable).
#The new variable is "gpa_gmc".
grand_mean_gpa <- mean(d$gpa, na.rm=T)
d$gpa_gmc <- d$gpa - grand_mean_gpa


#Below are the commands to cluster-mean center "gpa" (i.e., subtracting the class-specific mean of the predictor variable). 
#The new variable is "gpa_cmc".
cluster_mean_gpa <- data.frame(tapply(d$gpa, d$classes, mean))
cluster_mean_gpa$classes <- rownames(cluster_mean_gpa)
names(cluster_mean_gpa)<- c("cluster_mean_gpa","classes")
d <- merge(d, cluster_mean_gpa, by="classes")
d$gpa_cmc <- d$gpa - d$cluster_mean_gpa


#Below is the command to center "teacher_fan"
d$teacher_fan_c <- NA
d$teacher_fan_c[d$teacher_fan==0] <- -0.5
d$teacher_fan_c[d$teacher_fan==1] <- 0.5

#####################################################################
#STEP #1: Building the Empty Model (corresponding to Sub-Appendix B)#
#####################################################################

#Import the library for multilevel logistic modeling
library(lme4)

#Below are the commands to run an empty model, that is, a model containing no predictors, 
# and calculate the intraclass correlation coefficient (ICC; the degree of homogeneity of the outcome within clusters).


M0 <- glmer(bieber ~ ( 1 | classes), data=d, family = "binomial")
summary(M0)

icc <- M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3))
icc

#########
#RESULTS#
#########
#N = 2000 pupils are nested in K = 100 classes (n = 20 pupils per classes);
#The fixed intercept is in the "Fixed effects" part of the output: (Intercept) 0.1028; 
#The random intercept variance (i.e., the level-2 residual) is in the "Random effects" part of the output: classes (Intercept) 1.254;
#The intraclass correlation coefficient is ICC = .28; 
#This indicates that 28% of the chance of owning an album is explained by between-classroom differences 

#############################################################################
#STEP #2: Building the Intermediate Models (corresponding to Sub-Appendix C)#
#############################################################################
  
#If you focus on the between-observation effect of the (level-1) variable, you can use the grand-mean centered variable ("gpa_gmc"). 
#If you focus on the within-cluster effect, use the cluster-mean centered variable ("gpa_cmc"). 
#We use the cluster-mean centered variable herein.

# Below are the commands to run the constrained intermediate model (CIM); 
# the model contains all level-1 variables, all level-2 variables well as all intra-level interactions).

CIM <- glmer(bieber ~ gpa_cmc + teacher_fan_c + (1 | classes), data = d, family = "binomial")
summary(CIM)
paste("FYI: The deviance of the CIM is:", CIM@devcomp$cmp[[8]])

# Below are the commands to run the augmented intermediate model (AIM); 
# the model similar to the constrained intermediate model with the exception that it includes the random slope term of "gpa_cmc").


AIM <- glmer(bieber ~ gpa_cmc + teacher_fan_c + (1 + gpa_cmc || classes), data = d, family = "binomial")
summary(AIM)
paste("FYI: The deviance of the AIM is:", AIM@devcomp$cmp[[8]])
#The random slope component is "classes.1 gpa_cmc 0.7720"  (corresponding to the variation of the effect of "gpa_cmc" from one class to another);

# Sometimes the glmer function prompts a warning message of non convergence. To understand if it represents a problem to your results 
# you should run the analysis with different optimizers (e.g., AIM <- glmer(bieber ~ gpa_cmc + teacher_fan_c + (1 + gpa_cmc || classes), data = d, family = "binomial",control=glmerControl(optimizer="bobyqa"))) and compare the results. 
# In most cases, the difference is very small (at the fourth decimal place) and may not compromise the interpretation of the results. 
# see lme4 convergence warnings: troubleshooting https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

#Below is the command to determine whether including the random slope of "gpa_cmc" improves the model. 
#The software performs a likelihood-ratio test LR X(1)²,  comparing the deviance of the CIM with the deviance of the AIM.
anova(CIM, AIM)

#########
#RESULTS#
#########
#The likelihood-ratio test is given by the anova function, and referred by the parameter Chisq = 26.896, Chi Df = 1, p < .001.
#Thus, allowing the effect of "gpa_cmc" to vary between classes improves the fit and it is better to take the random slope into account. 


#####################################################################
#STEP #3: Building the Final Model (corresponding to Sub-Appendix D)#
#####################################################################
  
#Below is the command to run the final model (adding the cross-level interaction(s)). 
FM <- glmer(bieber ~ gpa_cmc + teacher_fan_c + (1 + gpa_cmc || classes) + gpa_cmc:teacher_fan_c, data = d, family = "binomial")
summary(FM)
#Note:
# we have decided here to keep the random slope component of "gpa_cmc"

#Below is the command to calculate the first simple slope
#namely the effect of "teacher_fan_c" when "gpa_cmc" is low (-1 SD).

sd_gpa_cmc <- sd(d$gpa_cmc)
d$gpa_m1SD <- d$gpa_cmc + sd_gpa_cmc
FM_m1SD <- glmer(bieber ~ gpa_m1SD + teacher_fan_c + (1 + gpa_cmc || classes) + gpa_m1SD:teacher_fan_c, data = d, family = "binomial")
summary(FM_m1SD)

#Below is the command to calculate the second simple slope
#namely the effect of "teacher_fan_c" when "gpa_cmc" is high (+1 SD).
d$gpa_p1SD <- d$gpa_cmc - sd_gpa_cmc
FM_p1SD <- glmer(bieber ~ gpa_p1SD + teacher_fan_c + (1 + gpa_cmc || classes) + gpa_p1SD:teacher_fan_c, data = d, family = "binomial")
summary(FM_p1SD)

#Below are the command to compare the coefficient estimates obtained in the final model,
#with (glmer) or without (glm) the use of multilevel modelling.
GLMER <- glmer(bieber ~ gpa_cmc + teacher_fan_c + (1 + gpa_cmc || classes) + gpa_cmc:teacher_fan_c, data = d, family = "binomial")
GLM <-  glm(bieber ~ gpa_cmc + teacher_fan_c + gpa_cmc:teacher_fan_c, data = d, family = "binomial")

summary(GLMER)
summary(GLM)

#Calculate Odds-Ratios

OR <- exp(fixef(FM))
CI <- exp(confint(FM,parm="beta_")) # it can be slow (~ a few minutes). As alternative, the much faster but less precise Wald's method can be used: CI <- exp(confint(FM,parm="beta_",method="Wald")) 

OR_m1SD <- exp(fixef(FM_m1SD))
CI_m1SD <- exp(confint(FM_m1SD,parm="beta_")) 

OR_p1SD <- exp(fixef(FM_p1SD))
CI_p1SD <- exp(confint(FM_p1SD,parm="beta_")) 

OR.CI<-rbind(cbind(OR,CI), cbind(OR_m1SD,CI_m1SD)[3,], cbind(OR_p1SD,CI_p1SD)[3,])
rownames(OR.CI)<-c(rownames(cbind(OR,CI)), "teacher_fan_c_m1SD", "teacher_fan_c_p1SD")
OR.CI

#########
#RESULTS#
#########
#Regarding your main effect hypothesis, the effect of "teacher_fan_c" is significant, teacher_fan_c OR = 7.47, 95% CI [5.03, 11.33]
#pupils whose teacher is a belieber have 7.50 times more chance of owning Justin's album than pupils whose classroom teacher is not a fan a Justin Bieber 
#Regarding your interaction hypothesis, the interaction between "teacher_fan_c" and "gpa_cmc" is significant, gpa_cmc:teacher_fan_c OR = 2.99, 95% CI [1.89, 4.90]
#simple slope analysis show that for the highest achievers of their classroom (-1 SD), 
#having a teacher who is a belieber results in 3.50 times more chance of owning Justin's last album, teacher_fan_c_m1SD OR = 3.47. 95% CI [2.15, 5.65]
#...and for the lowest achievers of their classroom (-1 SD), 
#having a teacher who is a belieber results in 16 times more chance of owning Justin's last album, gpa_p1SD:tteacher_fan_c_p1SD OR = 16.05. 95% CI [9.35, 28.67]





