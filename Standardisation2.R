###Standardisation-----------
library(PHEindicatormethods)
library(tidyverse)
library(readxl)
library(MASS)
set.seed(123)
##Loading datasets-------------
0.07*13
national = read_xlsx("Standardisation Data.xlsx", sheet = 1)
trust = filter(read_xlsx("Standardisation Data.xlsx", sheet = 2), tretspef=='Cardiology' )
##Indirect Standardisation-------------
National_rates = national %>%
  group_by(startage, sex, imd_decile, suscorehrg)%>%
  mutate(rates = deaths/spells)

Expected = merge(National_rates, trust,
                 by.x = c("tretspef", "startage", "PoD", "sex", "imd_decile", "suscorehrg"), 
                 by.y = c("tretspef", "age", "PoD", "sex", "IMD_Decile", "suscorehrg"),
                 all.y = TRUE )

Expected_national =  National_rates %>%
  group_by(PoD, tretspef)%>%
  mutate(E = rates * spells, O = deaths) %>%
  summarise(total_E = sum(E, na.rm = TRUE),
            total_O = sum(O, na.rm = TRUE)) %>%
  mutate(Trust = "National", Ratio = (total_O/ total_E) *100) %>%
  select(Trust, PoD, tretspef, total_E, total_O, Ratio)

Expected_final = Expected %>%
  mutate(E = rates * spells.y, O = deaths.y) %>%
  group_by(Trust, PoD, tretspef) %>%
  summarise(total_E = sum(E, na.rm = TRUE),
            total_O = sum(O, na.rm = TRUE)) %>%
  mutate(Ratio = (total_O/ total_E) *100) %>%
  bind_rows(Expected_national)

ggplot(filter(Expected_final, Trust != "MCHFT" & Trust != "National" & tretspef == 'Cardiology'), aes(Trust, Ratio))+
  geom_bar(stat = 'identity')

output = bind_cols(Expected_final, pois.byar(Expected_final$total_O))
output$Ratio_lower = (output$lower/ output$total_E) *100
output$Ratio_upper = (output$upper/ output$total_E) *100
output =filter(output,  Trust != "MCHFT" & Trust != "National" & tretspef == 'Cardiology')
write.csv(output, "forKatie.csv")

### Regression standardisation------------------

national <- read_xlsx('Standardisation Data.xlsx', sheet = 1) %>%
  filter(tretspef == '300') %>%
  mutate(alive = spells- deaths) #%>%
  select(-spells)

alive <- national %>%
 # select(-deaths) %>%
  uncount(alive) %>%
  mutate(state = 0)

dead <- national %>%
  #select(-alive) %>%
  uncount(deaths) %>%
  mutate(state = 1)

national_uc <- bind_rows(alive,dead)# %>%
  select(-tretspef)
###
national1 = national %>%
  mutate(spellsnodeaths = spells - deaths)

national_uc_snd = uncount(filter(national1, tretspef =='Cardiology'), weights = spellsnodeaths)
national_uc_d= uncount(filter(national1, tretspef =='Cardiology'), weights = deaths)

national_uc_snd$state = 0
national_uc_d$state = 1

national_uc = bind_rows(national_uc_snd, national_uc_d) %>%
  select(-tretspef, - spells, - deaths, - spellsnodeaths)


trust1 = trust %>%
  mutate(spellsnodeaths = spells - deaths)

trust_uc_snd = uncount(filter(trust1, tretspef =='Cardiology'), weights = spellsnodeaths)
trust_uc_d= uncount(filter(trust1, tretspef =='Cardiology'), weights = deaths)

trust_uc_snd$state = 0
trust_uc_d$state = 1
set.seed(123)
trust_uc = bind_rows(trust_uc_snd, trust_uc_d)%>%
  select(-tretspef, - spells, - deaths, - spellsnodeaths)
#trust_uc = uncount(filter(trust, tretspef =='Cardiology'), weights = spells)
t = sample_n(national_uc, 10000)
t$suscorehrg = as.factor(t$suscorehrg)
t$PoD = as.factor(t$PoD)
t$imd_decile = as.factor(t$imd_decile)
t$startage = as.factor(t$startage)

fullmodel = glm(state ~., data = t, family = 'binomial') 

#estimate is the log-odd of the impact of this feature on likelihood of death in comparison to base category
summary(fullmodel)#sets a full model with all values as predictive variable
nullmodel = glm(state ~ 1, data = t, family = 'binomial') # sets a null model with no predictive variables
#selection of most predictive model (tries to avoid overfitting)
stepAIC(nullmodel, scope = list(lower = nullmodel, upper = fullmodel), direction = 'both')
#outcome of the stepAIC
final_model = glm(formula = state ~ PoD + startage, family = "binomial", 
                  data = t)
colnames(trust)[3] = "startage"
trust$expected = predict(final_model, newdata = trust, interval = 'prediction', type = 'response')
## Standardisation code
##Their are different types of regression model based on the type of data. 
#If influenced by a simple yes/no as with readmissions, then this is binomial regression


#load the data
NatPaedsReadM <- read.csv('National_readm_wave1.csv')

#Prepare the data for analysis - data cleaning
#na.strings = C("") <- ensure that missing values are coded as NA, making then easier to remove
#use sapply to look for the number of unique values. 
# sapply applies the function to each column of the dataframe
#No errors in the test data

library(dplyr)

NatPaedsRadM %>% filter (Treatment.specialty != 'ENT')

#fit the data to the binomial regression model

mod_fit <- glm(readmission ~
                 PoD + 
                 sex+
                 core.HRG+
                 Treatment.specialty +
                 Childrens,
               data = NatPaedsReadM , family="binomial")

modfitprint <- summary(mod_fit)$coefficients


#call as co-efficients to get odds-ratio
exp(coef(mod_fit))



#Predict for GM data
GMPeads_Data <- read.csv ('GM_readm_wave1.csv')
Predicted_GMPeads <- predict (mod_fit, newdata=GMPeads_Data,
                              type ="response")

GMPeads_Data$Expected_Readmissions <- Predicted_GMPeads

write.csv(GMPeads_Data,"S:/SERVTRAN/FINANCE AND ANALYTICS/Trainee Healthcare Analysts/SAC - Paediatrics/Peadiatrics update Q3 2017/RR Readmission/PeadsReadM.csv")
