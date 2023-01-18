library(lubridate)
library(readr)
library(dplyr)

setwd("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data")
#A) Import internal data
data <- read_csv("ACTL31425110AssignmentData2022.csv")

data$accident_month <- as.Date(data$accident_month)
data$term_start_date <- as.Date(data$term_start_date)
data$term_expiry_date <- as.Date(data$term_expiry_date)



#B) Import external data
mv_sales1 <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/New_motor_vehicle_sales_total_vehicles_-_long_term.csv")
mv_sales2 <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/New_motor_vehicle_sales_total_vehicles_-_short_term.csv")


#1) Foreign Exchange Rate
AUD_CNY <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/AUDCNY=X.csv") %>%
  select(Date,Close)
AUD_EUR <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/AUDEUR=X.csv") %>%
  select(Date,Close)
AUD_USD <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/AUDUSD=X.csv") %>%
  select(Date,Close)
AUD_JPY <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/AUDJPY=X.csv") %>%
  select(Date,Close)


AUD_CNY$Date <- as.Date(AUD_CNY$Date) 
AUD_EUR$Date <- as.Date(AUD_EUR$Date)
AUD_USD$Date <- as.Date(AUD_USD$Date)
AUD_JPY$Date <- as.Date(AUD_JPY$Date)

colnames(AUD_CNY) <- c("Date","AUD_CNY")
colnames(AUD_EUR) <- c("Date","AUD_EUR")
colnames(AUD_USD) <- c("Date","AUD_USD")
colnames(AUD_JPY) <- c("Date","AUD_JPY")



#2) Weather statistics (Temperature & Precipitation)
weather <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/Weather_Statistics.csv")

weather$Date <- as.Date(weather$Date,format="%d/%m/%Y")


#3) Interest rate
interest <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/Interest Rate.csv")

interest$Date <- as.Date(interest$Date,format="%d/%m/%Y")
colnames(interest) <- c("Date","Interest")

#4) CPI / Inflation
CPI <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/CPI.csv")

CPI$Date <- as.Date(CPI$Date,format="%d/%m/%Y")

#5) GDP
GDP <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/GDP.csv")

GDP$Date <- as.Date(GDP$Date,format="%d/%m/%Y")
colnames(GDP) <- c("Date","GDP")

#6) COVID-19 impact
covid <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/covid_impact.csv") %>%
  select(!`Date Key`)

covid$Date <- as.Date(covid$Date,format="%d/%m/%Y")


#7) Fuel price index
fuel <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/Fuel Price Index.csv")

fuel$Date <- as.Date(fuel$Date,format="%d/%m/%Y")

#8) Road statistics
road <- read_csv("C:/Users/iamwi/Documents/UNSW/ACTL3142/Assignment/Data/External Data/Road statistics.csv")

road$Date <- as.Date(road$Date,format="%d/%m/%Y")


#C) Create a master table
master <- data %>% 
  mutate(accident_month_1 = ymd(year(accident_month)*10000+month(accident_month)*100+1)) %>%
  left_join(weather,by = c("accident_month_1"="Date", "risk_state_name"="State")) %>%
  left_join(AUD_CNY,by = c("accident_month_1"="Date")) %>%
  left_join(AUD_EUR,by = c("accident_month_1"="Date")) %>%
  left_join(AUD_USD,by = c("accident_month_1"="Date")) %>%
  left_join(AUD_JPY,by = c("accident_month_1"="Date")) %>%
  left_join(interest,by = c("accident_month_1"="Date")) %>%
  left_join(CPI,by = c("accident_month_1"="Date")) %>%
  left_join(GDP,by = c("accident_month_1"="Date")) %>%
  left_join(covid,by = c("accident_month_1"="Date")) %>%
  left_join(fuel,by = c("accident_month_1"="Date", "risk_state_name"="State")) %>%
  left_join(road,by = c("accident_month_1"="Date", "risk_state_name"="State")) %>%
  mutate(vehicle_age = year(accident_month)-as.numeric(year_of_manufacture)) %>%
  mutate(vehicle_class_high = case_when(vehicle_class == "Class 11" ~ 1,
                                      TRUE ~ 0)) %>% 
  # mutate(vehicle_class_cs = case_when(vehicle_class == "Class 11" ~ "very high",
  #                                    vehicle_class == "Class 12" | vehicle_class == "Class 13" ~ "high",
  #                                    vehicle_class == "Class 14" | vehicle_class == "Class 3" | vehicle_class == "Class 1" | vehicle_class == "Class 10" ~ "medium",
  #                                    vehicle_class == "Class 4" | vehicle_class == "Class 15" ~ "very low",
  #                                    TRUE ~ "low")) %>%
  mutate(vehicle_class_cs = case_when(vehicle_class == "Class 10" | vehicle_class == "Class 11" ~ "very high",
                                      vehicle_class == "Class 7" | vehicle_class == "Class 8" ~ "high",
                                      vehicle_class == "Class 5" | vehicle_class == "Class 13" | vehicle_class == "Class 9" | vehicle_class == "Class 12" | vehicle_class == "Class 1" ~ "medium",
                                      vehicle_class == "Class 15" ~ "very low",
                                      TRUE ~ "low")) %>%
  mutate(risk_state_name_cs = case_when(risk_state_name == "NSW" ~ "very high",
                                        risk_state_name == "QLD" | risk_state_name == "VIC" ~ "high",
                                        risk_state_name == "SA" | risk_state_name == "WA" ~ "medium",
                                        risk_state_name == "NT" ~ "very low",
                                        TRUE ~ "medium"))



#D) Claim Severity modelling 
master_cs <- master %>%
  filter(!(is.na(total_claims_cost)|total_claims_cost==0))


cor_cs <- cor(master_cs %>% select(total_claims_cost,
                         policy_tenure,
                         sum_insured,
                         vehicle_age,
                         Precipitation
                         ,   Temperature
                         ,   Number_of_registered_vehicles
                         ,   Road_length
                         ,   Compact_level
                         ,   Interest
                         ,   CPI
                         ,   GDP
                         ,   `Fuel Price Index`
                         ,   c1_school_closing
                         ,   c2_workplace_closing
                         ,   c3_cancel_public_events
                         ,   c7_movementrestrictions
                         ,   AUD_CNY
                         ,   AUD_EUR
                         ,   AUD_JPY
                         ,   AUD_USD))

write.csv(cor_cs,"cor_cs.csv")

glm_cs <- glm(formula = total_claims_cost
              ~ policy_tenure
              + risk_state_name_cs
              + vehicle_class_cs
              + sum_insured
              + vehicle_age
              + Precipitation
              + Temperature
              + Number_of_registered_vehicles
              + Road_length
              + Compact_level
              + Interest
              + CPI
              + GDP
              + `Fuel Price Index`
              + c1_school_closing
              + c2_workplace_closing
              + c3_cancel_public_events
              + c7_movementrestrictions
              + AUD_CNY
              + AUD_EUR
              + AUD_JPY
              + AUD_USD,
              data = master_cs,
              family = Gamma(log))


summary(glm_cs)


library(MASS)
stepAIC(glm_cs, direction="backward")
detach(package:MASS,unload=TRUE)


glm_cs_v1 <- glm(formula = total_claims_cost 
              ~ policy_tenure
              + risk_state_name_cs
              + vehicle_class_cs
              + sum_insured 
              # + vehicle_age 
              + Precipitation
              # + Temperature 
              # + Number_of_registered_vehicles
              + Road_length
              + Interest
              # + CPI
              # + `Fuel Price Index`
              # + c1_school_closing
              # + c2_workplace_closing
              # + c3_cancel_public_events
              # + c7_movementrestrictions
              + AUD_CNY
              # + AUD_EUR
              + AUD_JPY,
              # + AUD_USD,
              data = master_cs,
              family = Gamma(log))


summary(glm_cs_v1)

###############################Test##################################
glm_cs2 <- glm(formula = total_claims_cost
               ~ policy_tenure
               + risk_state_name_cs
               + vehicle_class_cs
               + sum_insured
               + vehicle_age
               + exp(-vehicle_age)
               + exp(-vehicle_age):sum_insured
               + Precipitation
               + Temperature
               + Number_of_registered_vehicles
               + Road_length
               + Compact_level
               + Interest
               + Interest:sum_insured
               + CPI
               + CPI:sum_insured
               + GDP
               + GDP:sum_insured
               + `Fuel Price Index`
               + c1_school_closing
               + c2_workplace_closing
               + c3_cancel_public_events
               + c7_movementrestrictions
               + AUD_CNY
               + AUD_CNY:sum_insured
               + AUD_EUR
               + AUD_EUR:sum_insured
               + AUD_JPY
               + AUD_JPY:sum_insured
               + AUD_USD
               + AUD_USD:sum_insured,
               data = master_cs,
               family = Gamma(log))


summary(glm_cs2)


#Step-wise Subset Selection
library(leaps)
step_cs_back <- regsubsets(total_claims_cost
                           ~ policy_tenure
                           + risk_state_name_cs
                           + vehicle_class_cs
                           + sum_insured
                           + vehicle_age
                           + exp(-vehicle_age)
                           + exp(-vehicle_age):sum_insured
                           + Precipitation
                           + Temperature
                           + Number_of_registered_vehicles
                           + Road_length
                           + Compact_level
                           + Interest
                           + Interest:sum_insured
                           + CPI
                           + CPI:sum_insured
                           + GDP
                           + GDP:sum_insured
                           + `Fuel Price Index`
                           + c1_school_closing
                           + c2_workplace_closing
                           + c3_cancel_public_events
                           + c7_movementrestrictions
                           + AUD_CNY
                           + AUD_CNY:sum_insured
                           + AUD_EUR
                           + AUD_EUR:sum_insured
                           + AUD_JPY
                           + AUD_JPY:sum_insured
                           + AUD_USD
                           + AUD_USD:sum_insured,
                           data=master_cs, nvmax=NULL, method="backward")



step_cs_back_summary <- summary(step_cs_back)
step_cs_back_summary_table <- data.frame(`Number of parameters` = 1:36,
                                         `Cp` = step_cs_back_summary[['cp']],
                                         `BIC` = step_cs_back_summary[['bic']])


#Visualisation
par(mfrow=c(2,2))
par(mfrow=c(1,1))

plot(step_cs_back, scale="r2", cex=0.5)
plot(step_cs_back, scale="adjr2", cex=0.5)
plot(step_cs_back, scale="Cp")
plot(step_cs_back, scale="bic")


par(mfrow = c(2,2))
plot(step_cs_back_summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(step_cs_back_summary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
adjr2_max1 <- which.max(step_cs_back_summary$adjr2)
points(adjr2_max1,step_cs_back_summary$adjr2[adjr2_max1],col="red",cex=2,pch=20)
plot(step_cs_back_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
cp_min1 <- which.min(step_cs_back_summary$cp)
points(cp_min1,step_cs_back_summary$cp[cp_min1],col="red",cex=2,pch=20)
plot(step_cs_back_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
bic_min1 <- which.min(step_cs_back_summary$bic)
points(bic_min1,step_cs_back_summary$bic[bic_min1],col="red",cex=2,pch=20)




step_cs_full <- regsubsets(total_claims_cost
                           ~ policy_tenure
                           + risk_state_name_cs
                           + vehicle_class_cs
                           + sum_insured
                           + vehicle_age
                           + exp(-vehicle_age)
                           + exp(-vehicle_age):sum_insured
                           + Precipitation
                           + Temperature
                           + Number_of_registered_vehicles
                           + Road_length
                           + Compact_level
                           + Interest
                           + Interest:sum_insured
                           + CPI
                           + CPI:sum_insured
                           + GDP
                           + GDP:sum_insured
                           + `Fuel Price Index`
                           + c1_school_closing
                           + c2_workplace_closing
                           + c3_cancel_public_events
                           + c7_movementrestrictions
                           + AUD_CNY
                           + AUD_CNY:sum_insured
                           + AUD_EUR
                           + AUD_EUR:sum_insured
                           + AUD_JPY
                           + AUD_JPY:sum_insured
                           + AUD_USD
                           + AUD_USD:sum_insured,
                           data=master_cs, nvmax=NULL, method="exhaustive")



step_cs_full_summary <- summary(step_cs_full)
step_cs_full_summary_table <- data.frame(`Number of parameters` = 1:36,
                                         `Cp` = step_cs_full_summary[['cp']],
                                         `BIC` = step_cs_full_summary[['bic']])



par(mfrow=c(2,2))
par(mfrow=c(1,1))

plot(step_cs_full, scale="r2", cex=0.5)
plot(step_cs_full, scale="adjr2", cex=0.5)
plot(step_cs_full, scale="Cp")
plot(step_cs_full, scale="bic")


par(mfrow = c(2,2))
plot(step_cs_full_summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(step_cs_full_summary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="l")
adjr2_max2 <- which.max(step_cs_full_summary$adjr2)
points(adjr2_max2,step_cs_full_summary$adjr2[adjr2_max2],col="red",cex=2,pch=20)
plot(step_cs_full_summary$cp, xlab="Number of Variables", ylab="Cp", type="l")
cp_min2 <- which.min(step_cs_full_summary$cp)
points(cp_min2,step_cs_full_summary$cp[cp_min2],col="red",cex=2,pch=20)
plot(step_cs_full_summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
bic_min2 <- which.min(step_cs_full_summary$bic)
points(bic_min2,step_cs_full_summary$bic[bic_min2],col="red",cex=2,pch=20)

coef(step_cs_full,25)
coef(step_cs_full,11)

cs_null <- glm(total_claims_cost ~ 1, data=master_cs)
cs_full <- glm(total_claims_cost ~ policy_tenure
               + risk_state_name_cs
               + vehicle_class_cs
               + sum_insured
               + vehicle_age
               + exp(-vehicle_age)
               + exp(-vehicle_age):sum_insured
               + Precipitation
               + Temperature
               + Number_of_registered_vehicles
               + Road_length
               + Compact_level
               + Interest
               + Interest:sum_insured
               + CPI
               + CPI:sum_insured
               + GDP
               + GDP:sum_insured
               + `Fuel Price Index`
               + c1_school_closing
               + c2_workplace_closing
               + c3_cancel_public_events
               + c7_movementrestrictions
               + AUD_CNY
               + AUD_CNY:sum_insured
               + AUD_EUR
               + AUD_EUR:sum_insured
               + AUD_JPY
               + AUD_JPY:sum_insured
               + AUD_USD
               + AUD_USD:sum_insured, data=master_cs)

model.step <- step(cs_null,scope=list(lower=cs_null,upper=cs_full), direction="both")
model.forward <- step(cs_null,scope=list(lower=cs_null,upper=cs_full), direction="forward")
model.backward <- step(cs_full,direction="backward")


library(ggplot2)
library(hrbrthemes)

# coeff1 <- mean(step_cs_back_summary$cp)-mean(step_cs_back_summary$bic)
# 
# Cp_colour <- "#B20000"
# BIC_colour <- "#3895D3"
# 
# ggplot(step_cs_back_summary_table, aes(x=Number.of.parameters)) +
#   geom_line(aes(y=Cp), size=1, color=Cp_colour) +
#   # geom_point(y=Cp) +
#   geom_line(aes(y=BIC+coeff1), size=1, color=BIC_colour) +
#   # geom_point(y=BIC+coeff1) +
#   scale_y_continuous(
#            name = "Cp",
#            sec.axis = sec_axis(~.-coeff1,name="BIC")) +
#   theme_ipsum() +
#   theme(
#     axis.title.y=element_text(color=Cp_colour,size=10),
#     axis.title.y.right=element_text(color=BIC_colour,size=10),
#     axis.title.x=element_text(size=10)
#   ) +
#   labs(title="Claim Severity GLM Model Backward Selection Results", x="Number of parameters")



       

library(MASS)
stepAIC(glm_cs2, direction="backward")
detach(package:MASS,unload=TRUE)


glm_cs_v2 <- glm(formula = total_claims_cost
               ~ policy_tenure
               + risk_state_name_cs
               + vehicle_class_cs
               + sum_insured
               + vehicle_age
               + exp(-vehicle_age)
               + Precipitation
               # + Temperature
               # + Number_of_registered_vehicles
               + Road_length
               + GDP
               + sum_insured:GDP
               + Interest
               + sum_insured:Interest
               # + CPI
               # + CPI:sum_insured
               # + `Fuel Price Index`
               # + c1_school_closing
               # + c2_workplace_closing
               # + c3_cancel_public_events
               # + c7_movementrestrictions
               + AUD_CNY
               # + AUD_CNY:sum_insured
               # + AUD_EUR
               # + AUD_EUR:sum_insured
               + AUD_JPY,
               # + AUD_JPY:sum_insured
               # + AUD_USD
               # + AUD_USD:sum_insured,
               data = master_cs,
               family = Gamma(log))
summary(glm_cs_v2)


glm_cs3 <- glm(formula = total_claims_cost
               ~ policy_tenure
               + risk_state_name_cs
               + vehicle_class_cs
               + sum_insured
               + vehicle_age
               + exp(-vehicle_age)
               + exp(-vehicle_age):sum_insured
               + Precipitation
               + Temperature
               # + Number_of_registered_vehicles
               # + Road_length
               + Compact_level
               + Interest
               + Interest:sum_insured
               + CPI
               + CPI:sum_insured
               + GDP
               + GDP:sum_insured
               + `Fuel Price Index`
               + c1_school_closing
               + c2_workplace_closing
               + c3_cancel_public_events
               + c7_movementrestrictions
               + AUD_CNY
               + AUD_CNY:sum_insured
               + AUD_EUR
               + AUD_EUR:sum_insured
               + AUD_JPY
               + AUD_JPY:sum_insured
               + AUD_USD
               + AUD_USD:sum_insured,
               data = master_cs,
               family = Gamma(log))


summary(glm_cs3)

library(MASS)
stepAIC(glm_cs3, direction="backward")
detach(package:MASS,unload=TRUE)

glm_cs_v3 <- glm(formula = total_claims_cost
               ~ policy_tenure
               + risk_state_name_cs
               + vehicle_class_cs
               + sum_insured
               + vehicle_age
               + exp(-vehicle_age)
               # + exp(-vehicle_age):sum_insured
               + Precipitation
               # + Temperature
               # + Number_of_registered_vehicles
               # + Road_length
               + Compact_level
               + Interest
               + Interest:sum_insured
               # + CPI
               # + CPI:sum_insured
               + GDP
               + GDP:sum_insured
               # + `Fuel Price Index`
               # + c1_school_closing
               # + c2_workplace_closing
               # + c3_cancel_public_events
               # + c7_movementrestrictions
               + AUD_CNY
               # + AUD_CNY:sum_insured
               # + AUD_EUR
               # + AUD_EUR:sum_insured
               + AUD_JPY,
               # + AUD_JPY:sum_insured
               # + AUD_USD
               # + AUD_USD:sum_insured,
               data = master_cs,
               family = Gamma(log))
summary(glm_cs_v3)


###############################Test##################################




# par(mfrow=c(2,2))
# plot(glm_cs)
# 
# sim_cs <- master_cs %>%
#   select(policy_tenure,
#          sum_insured,
#          vehicle_age,
#          Precipitation,
#          Temperature,
#          AUD_CNY,
#          AUD_EUR,
#          AUD_JPY,
#          AUD_USD)
# 
# fit_cs <- predict(glm_cs, newdata=sim_cs, type="response")



#E) Claim Frequency modelling 
master_cf <- master %>% 
  mutate(claim_ind = case_when(
    is.na(total_claims_cost)|total_claims_cost==0 ~ 0,
    TRUE ~ 1
  )) %>%
  group_by(across(c(-exposure,-claim_ind,-total_claims_cost))) %>%
  summarize(claim_freq=sum(claim_ind), exposure=sum(exposure)) 

glm_cf <- glm(formula = claim_freq 
              ~ policy_tenure
              # + risk_state_name
              # + vehicle_class
              + sum_insured 
              + exposure
              + vehicle_age 
              + Precipitation 
              + Temperature 
              + Number_of_registered_vehicles
              + Road_length
              + Compact_level
              + Interest
              + CPI
              + GDP
              + `Fuel Price Index`
              + c1_school_closing
              + c2_workplace_closing
              + c3_cancel_public_events
              + c7_movementrestrictions
              + AUD_CNY
              + AUD_EUR
              + AUD_JPY
              + AUD_USD,
              data = master_cf,
              family = poisson)

summary(glm_cf)


MASS::stepAIC(glm_cf, direction="backward")



glm_cf_v1 <- glm(formula = claim_freq 
              ~ policy_tenure
              # + risk_state_name
              # + vehicle_class
              + sum_insured 
              + exposure
              + vehicle_age 
              + Precipitation 
              + Temperature 
              + Number_of_registered_vehicles
              + Road_length
              # + Compact_level
              # + Interest
              # + CPI
              # + GDP
              # + `Fuel Price Index`
              # + c1_school_closing
              + c2_workplace_closing
              # + c3_cancel_public_events
              # + c7_movementrestrictions
              # + AUD_CNY
              + AUD_EUR,
              # + AUD_JPY
              # + AUD_USD,
              data = master_cf,
              family = poisson)

summary(glm_cf_v1)






# par(mfrow=c(2,2))
# plot(glm_cf)




glm_cf2 <- glm(formula = claim_freq 
              ~ policy_tenure
              # + risk_state_name
              # + vehicle_class
              + sum_insured
              + vehicle_age
              + exposure
              + Precipitation 
              + Temperature
              + Compact_level
              + Precipitation:exposure
              + Temperature:exposure
              + Compact_level:exposure
              + Interest
              + CPI
              + GDP
              + `Fuel Price Index`:exposure
              + c1_school_closing
              + c2_workplace_closing
              + c3_cancel_public_events
              + c7_movementrestrictions
              + c1_school_closing:exposure
              + c2_workplace_closing:exposure
              + c3_cancel_public_events:exposure
              + c7_movementrestrictions:exposure,
              data = master_cf,
              family = poisson)

summary(glm_cf2)


MASS::stepAIC(glm_cf2, direction="backward")

glm_cf_v2 <- glm(formula = claim_freq 
               ~ policy_tenure
               # + risk_state_name
               # + vehicle_class
               + sum_insured
               + vehicle_age
               + exposure
               + Precipitation 
               + Temperature
               + Compact_level
               # + Precipitation:exposure
               + Temperature:exposure
               # + Compact_level:exposure
               # + Interest
               + CPI
               + GDP
               + `Fuel Price Index`:exposure,
               # + c1_school_closing
               # + c2_workplace_closing
               # + c3_cancel_public_events
               # + c7_movementrestrictions
               # + c1_school_closing:exposure
               # + c2_workplace_closing:exposure
               # + c3_cancel_public_events:exposure
               # + c7_movementrestrictions:exposure,
               data = master_cf,
               family = poisson)

summary(glm_cf_v2)




cf_null <- glm(formula = claim_freq 
                 ~ 1,
                 data = master_cf,
                 family = poisson)


















#Cross-validation
set.seed(20220721)

library(boot)
library(glmnet)
detach("package:MASS", unload=TRUE)


kfcv_cs_v2 <- cv.glm(master_cs,glm_cs_v2,K=5)
summary(kfcv_cs_v2)

kfcv_cs_v2 <- cv.glm(master_cs,glm_cs_v2,K=5)
summary(kfcv_cs_v2)


kfcv_cs4 <- cv.glm(master_cs,glm_cs4,K=10)
summary(kfcv_cs4)

x_cs <- data.matrix(master_cs %>% select(policy_tenure,
                             risk_state_name_cs,
                             vehicle_class_cs,
                             sum_insured,
                             vehicle_age,
                             Temperature,
                             Interest,
                             AUD_CNY,
                             AUD_JPY))

y_cs <- data.matrix(master_cs %>% select(total_claims_cost))

csfit <- cv.glmnet(x=x_cs,y=y_cs,nfolds=10)

coef(csfit, s = "lambda.min")


# master_check1 <- master %>%
#   filter(!is.na(total_claims_cost)) %>%
#   group_by(accident_month,policy_id) %>%
#   summarize(n=n()) %>%
#   filter(n>1)
# 
# master_check2 <- master %>%
#   filter(accident_month==as.Date("2016-07-31")&policy_id==32766)



# glm1 <- glm(formula = average_claim ~ time_ind, data = data_1)
# sim1 <- data.frame(time_ind=seq(1,60,1))
# fit1 <- predict(glm1, newdata=sim1, type="response")
# plot(data_1$time_ind,data_1$average_claim,xlab="Time",ylab="Average Claim Size")
# lines(fit1~time_ind,data=sim1,type="l")

# 
# 
# #data_check1
# data_check1 <- data %>% 
#   mutate(date_check = accident_month>term_start_date & (accident_month<term_expiry_date | (month(accident_month)==month(term_expiry_date) & year(accident_month)==year(term_expiry_date)))) %>%
#   filter(!date_check)
# 
# data_check_1 <- data %>% 
#   mutate(number_of_days_covered = exposure*365) %>%
#   mutate(number_of_days_month = as.numeric(days_in_month(accident_month))) %>%
#   mutate(number_of_days_start = as.numeric(difftime(accident_month,term_start_date,units = "days"))) %>%
#   mutate(number_of_days_expiry = as.numeric(difftime(term_expiry_date,accident_month,units = "days"))) %>%
#   mutate(number_of_days = pmin(number_of_days_expiry,number_of_days_start,number_of_days_month)) %>%
#   mutate(exposure_rate = number_of_days_covered/number_of_days) %>%
#   filter(!(exposure_rate < 1.1 & exposure_rate > 0.9))
# 
# data_check_2 <- data %>% 
#   group_by(policy_id,risk_state_name,risk_postcode) %>%
#   summarise(n1=n()) %>%
#   group_by(policy_id) %>%
#   summarise(n2=n()) %>% 
#   filter(n2!=1)
# 
# data_check_3 <- data %>% filter(policy_id==32422)
# 
# data_check_4 <- data %>% filter(!is.na(total_claims_cost))
# 
# #Summarise Claims by Accident Month 
# data_1 <- data %>% 
#   mutate(claim_indicator = !(is.na(total_claims_cost))) %>%
#   group_by(accident_month) %>%
#   summarise(claim_total = sum(total_claims_cost, na.rm =TRUE), 
#             inforce_count = n_distinct(policy_id), 
#             exposure_total = sum(exposure, na.rm=TRUE),
#             claim_count = sum(claim_indicator)) %>%
#   #Claim Frequency = Claim Count / Sum of Exposure
#   mutate(claim_frequency = claim_count/exposure_total) %>%
#   #Claim Rate = Claim Count / Inforce Count
#   mutate(claim_rate = claim_count/inforce_count) %>%
#   #Average Claim = Total Claim / Claim Count
#   mutate(average_claim = claim_total/claim_count) %>%
#   mutate(time_ind = row_number())
# 
# glm1 <- glm(formula = average_claim ~ time_ind, data = data_1)
# sim1 <- data.frame(time_ind=seq(1,60,1))
# fit1 <- predict(glm1, newdata=sim1, type="response")
# plot(data_1$time_ind,data_1$average_claim,xlab="Time",ylab="Average Claim Size")
# lines(fit1~time_ind,data=sim1,type="l")
# 
# step(glm1,test="LRT")
# 
# 
# par(mfrow=c(2,2))
# plot(glm1)
# 
# 
# 

if_2021_summary <- master %>% 
  filter(accident_month == as.Date("2021-06-30")) %>%
  filter(!(is.na(total_claims_cost)|total_claims_cost==0)) %>%
  summarise(claim.frequency =n(),claim.severity= mean(total_claims_cost),total.claim=sum(total_claims_cost))

if_2021 <- master %>% 
  filter(accident_month == as.Date("2021-06-30"))%>% 
  mutate(claim_ind = case_when(
    is.na(total_claims_cost)|total_claims_cost==0 ~ 0,
    TRUE ~ 1
  )) %>%
  group_by(across(c(-exposure,-claim_ind,-total_claims_cost))) %>%
  summarize(claim_freq=sum(claim_ind), exposure=sum(exposure)) 


predict_2021_cs <- predict(glm_cs_v2, if_2021, type="response")
predict_2021_cf <- predict(glm_cf_v1, if_2021, type="response")

if_2021$predict_cs_2021 <- predict_2021_cs
if_2021$predict_cf_2021 <- predict_2021_cf

if_2021 <- if_2021 %>% mutate(predict_total_claim_2021 = predict_cs_2021 * predict_cf_2021)

sum(if_2021$predict_cf_2021)
sum(if_2021$predict_cs_2021)
sum(if_2021$predict_total_claim_2021)


if_2022 <- if_2021

if_2022$Interest <- 10
if_2022$AUD_USD <- 0.61
if_2022$AUD_EUR <- 0.66
if_2022$AUD_JPY <- 93
if_2022$AUD_CNY <- 4.5
if_2022$GDP <- 514.8*1.025
if_2022$CPI <- 6681.966*1.06
if_2022$c1_school_closing <- 0
if_2022$c2_workplace_closing <- 0
if_2022$c3_cancel_public_events <- 0
if_2022$c7_movementrestrictions <- 0

predict_2022_cs <- predict(glm_cs_v2, if_2022, type="response")
predict_2022_cf <- predict(glm_cf_v1, if_2022, type="response")

if_2022$predict_cs_2022 <- predict_2022_cs
if_2022$predict_cf_2022 <- predict_2022_cf

if_2022 <- if_2022 %>% mutate(predict_total_claim_2022 = predict_cs_2022 * predict_cf_2022)

sum(if_2022$predict_cf_2022)
sum(if_2022$predict_total_claim_2022)/sum(if_2022$predict_cf_2022)
sum(if_2022$predict_total_claim_2022)
