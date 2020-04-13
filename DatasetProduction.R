library(tidyr)
library(dplyr)

# Load in Data ----
AllIndicators <-
  read.csv(
    "~/GitKraken/Final-Project-David-Adam-/Original Datasets/WorldBank_Data.csv",
    fileEncoding = 'UTF-8-BOM'
  )

# Country Reported Income Level ####
CountryTable <-
  read.csv(
    "~/GitKraken/Final-Project-David-Adam-/Original Datasets/WDICountry.csv",
    fileEncoding = 'UTF-8-BOM'
  )
WealthLevel <-
  CountryTable %>% select(Short.Name, Income.Group) %>% drop_na(Income.Group)
names(WealthLevel)[names(WealthLevel) == "Short.Name"] <-
  "Country.Name"
names(AllIndicators)[names(AllIndicators) == "Series.Name"] <-
  "Indicator.Name"
WealthLevel <- WealthLevel[!(WealthLevel$Income.Group)=="",]
WealthLevel %>% mutate(Income.Group=recode(Income.Group, 
                                           "Low income"=0,
                                           "Lower middle income"=1,
                                           "Upper middle income"=2,
                                           "High income"=3)) -> WealthLevel


AllIndicators2000 <-
  AllIndicators %>% select(Country.Name, Indicator.Name, X2000) %>% drop_na(X2000)
AllIndicators2012 <-
  AllIndicators %>% select(Country.Name, Indicator.Name, X2012) %>% drop_na(X2012)
AllIndicators2014 <-
  AllIndicators %>% select(Country.Name, Indicator.Name, X2014) %>% drop_na(X2014)
AllIndicators2015 <-
  AllIndicators %>% select(Country.Name, Indicator.Name, X2015) %>% drop_na(X2015)

# Life Expectancy Dataset Production ----
# Water ====
Water <- filter(
  AllIndicators,
  Indicator.Name == "People using at least basic drinking water services (% of population)"
)
Water2014 <- filter(
  AllIndicators2014,
  Indicator.Name == "People using at least basic drinking water services (% of population)"
) %>% select(Country.Name, X2014)
names(Water2014)[names(Water2014) == "X2014"] <-
  "Water2014"
Water2014 <- Water2014 %>% drop_na(Water2014)

#Education ====
Education <-
  filter(AllIndicators,
         Indicator.Name == "Compulsory education duration (years)")
Education2014 <- 
  filter(AllIndicators,
         Indicator.Name == "Compulsory education duration (years)") %>% select(Country.Name, X2014)
names(Education2014)[names(Education2014) == "X2014"] <-
  "Education2014"

#Greenhouse Gases ====
Greenhouse <-
  filter(AllIndicators,
         Indicator.Name == "Total greenhouse gas emissions (kt of CO2 equivalent)")
Greenhouse2012 <-
  filter(AllIndicators2012,
         Indicator.Name == "Total greenhouse gas emissions (kt of CO2 equivalent)") %>% select(Country.Name, X2012)
names(Greenhouse2012)[names(Greenhouse2012) == "X2012"] <-"Greenhouse2012"

#CO2 ====
CO2 <- filter(AllIndicators, Indicator.Name == "CO2 emissions (kt)")
CO22014 <- filter(AllIndicators2014, Indicator.Name == "CO2 emissions (kt)") %>% select(Country.Name, X2014)
names(CO22014)[names(CO22014) == "X2014"] <-"CO22014"

# Sanitation ====
Sanitation <- filter(AllIndicators, Indicator.Name == "People using at least basic sanitation services (% of population)")
Sanitation2014 <- filter(AllIndicators2014, Indicator.Name == "People using at least basic sanitation services (% of population)") %>% select(Country.Name, X2014)
names(Sanitation2014)[names(Sanitation2014) == "X2014"] <-"Sanitation2014"

# Rural Pop ====
RuralPOP <- filter(AllIndicators, Indicator.Name == "Rural population (% of total population)")
RuralPOP2014 <- filter(AllIndicators2014, Indicator.Name == "Rural population (% of total population)") %>% select(Country.Name, X2014)
names(RuralPOP2014)[names(RuralPOP2014) == "X2014"] <-"RuralPop2014"

# PopTotal ====
PopTotal <- filter(AllIndicators, Indicator.Name == "Population total")
PopTotal2014 <- filter(AllIndicators2014, Indicator.Name == "Population total") %>% select(Country.Name, X2014)
names(PopTotal2014)[names(PopTotal2014) == "X2014"] <-"PopTotal2014"

# PopMale ====
PopMale <- filter(AllIndicators, Indicator.Name == "Population male")
PopMale2014 <- filter(AllIndicators2014, Indicator.Name == "Population male") %>% select(Country.Name, X2014)
names(PopMale2014)[names(PopMale2014) == "X2014"] <-"PopMale2014"

# PopFemale ====
PopFemale <- filter(AllIndicators, Indicator.Name == "Population female")
PopFemale2014 <- filter(AllIndicators2014, Indicator.Name == "Population female") %>% select(Country.Name, X2014)
names(PopFemale2014)[names(PopFemale2014) == "X2014"] <-"PopFemale2014"

# HealthGDP ====
HealthGDP <- filter(AllIndicators, Indicator.Name == "Current health expenditure (% of GDP)")
HealthGDP2014 <- filter(AllIndicators2014, Indicator.Name == "Current health expenditure (% of GDP)") %>% select(Country.Name, X2014)
names(HealthGDP2014)[names(HealthGDP2014) == "X2014"] <-"HealthGDP2014"

# HealthPerCapita ==== 
HealthPerCapita <- filter(AllIndicators, Indicator.Name == "Current health expenditure per capita (current US$)")
HealthPerCapita2014 <- filter(AllIndicators2014, Indicator.Name == "Current health expenditure per capita (current US$)") %>% select(Country.Name, X2014)
names(HealthPerCapita2014)[names(HealthPerCapita2014) == "X2014"] <-"HealthPerCapita2014"

# LifeTotal ====
LifeTotal <- filter(AllIndicators, Indicator.Name == "Life expectancy at birth total (years)")
LifeTotal2014 <- filter(AllIndicators2014, Indicator.Name == "Life expectancy at birth total (years)") %>% select(Country.Name, X2014)
names(LifeTotal2014)[names(LifeTotal2014) == "X2014"] <-"LifeTotal2014"

#Life Male ====
LifeMale <- filter(AllIndicators, Indicator.Name == "Life expectancy at birth male (years)")
LifeMale2014 <- filter(AllIndicators2014, Indicator.Name == "Life expectancy at birth male (years)") %>% select(Country.Name, X2014)
names(LifeMale2014)[names(LifeMale2014) == "X2014"] <-"LifeMale2014"

# Life Female ====
LifeFemale <- filter(AllIndicators, Indicator.Name == "Life expectancy at birth female (years)")
LifeFemale2014 <- filter(AllIndicators2014, Indicator.Name == "Life expectancy at birth female (years)") %>% select(Country.Name, X2014)
names(LifeFemale2014)[names(LifeFemale2014) == "X2014"] <-"LifeFemale2014"

# Survive to 65 Male ====
Surviveto65Male  <- filter(AllIndicators, Indicator.Name == "Survival to age 65 male (% of cohort)")
Surviveto65Male2014  <- filter(AllIndicators2014, Indicator.Name == "Survival to age 65 male (% of cohort)") %>% select(Country.Name, X2014)
names(Surviveto65Male2014)[names(Surviveto65Male2014) == "X2014"] <-"Survivorto65Male2014"

# Survive to 65 Female ====
Surviveto65Female  <- filter(AllIndicators, Indicator.Name == "Survival to age 65 female (% of cohort)")
Surviveto65Female2014  <- filter(AllIndicators2014, Indicator.Name == "Survival to age 65 female (% of cohort)") %>% select(Country.Name, X2014)
names(Surviveto65Female2014)[names(Surviveto65Female2014) == "X2014"] <-"Surviveto65Female2014"

# Land Investigation Dataset Production ----
# Arable Land ====
ArableLand <- filter(AllIndicators, Indicator.Name == "Arable land (% of land area)")
ArableLand2014 <- filter(AllIndicators2014, Indicator.Name == "Arable land (% of land area)") %>% select(Country.Name, X2014)
names(ArableLand2014)[names(ArableLand2014) == "X2014"] <-"ArableLand2014"

#Average Rain ====
AverageRain <- filter(AllIndicators, Indicator.Name == "Average precipitation in depth (mm per year)")
AverageRain2014 <- filter(AllIndicators2014, Indicator.Name == "Average precipitation in depth (mm per year)") %>% select(Country.Name, X2014)
names(AverageRain2014)[names(AverageRain2014) == "X2014"] <-"AverageRain2014"

# Forest Land ====
ForestLand <- filter(AllIndicators, Indicator.Name == "Forest area (sq. km)")
ForestLand2014 <- filter(AllIndicators2014, Indicator.Name == "Forest area (sq. km)") %>% select(Country.Name, X2014)
names(ForestLand2014)[names(ForestLand2014) == "X2014"] <-"ForestLand2014"

# Agriculture Land ====
AgriculturalLand <- filter(AllIndicators, Indicator.Name == "Agricultural land (% of land area)")
AgriculturalLand2014 <- filter(AllIndicators2014, Indicator.Name == "Agricultural land (% of land area)") %>% select(Country.Name, X2014)
names(AgriculturalLand2014)[names(AgriculturalLand2014) == "X2014"] <-"AgriculturalLand2014"

# Rural Land ====
RuralLand <- filter(AllIndicators, Indicator.Name == "Rural land area (sq. km)")
RuralLand2000 <- filter(AllIndicators2000, Indicator.Name == "Rural land area (sq. km)") %>% select(Country.Name, X2000)
names(RuralLand2000)[names(RuralLand2000) == "X2000"] <-"RuralLand2000"

# Urban Land ====
UrbanLand <- filter(AllIndicators, Indicator.Name == "Urban land area (sq. km)")
UrbanLand2000 <- filter(AllIndicators2000, Indicator.Name == "Urban land area (sq. km)") %>% select(Country.Name, X2000)
names(UrbanLand2000)[names(UrbanLand2000) == "X2000"] <-"UrbanLand2000"

# Permanent Crop Land ====
PermCropLand <- filter(AllIndicators, Indicator.Name == "Permanent cropland (% of land area)")
PermCropLand2014 <- filter(AllIndicators2014, Indicator.Name == "Permanent cropland (% of land area)") %>% select(Country.Name, X2014)
names(PermCropLand2014)[names(PermCropLand2014) == "X2014"] <-"PermCropLand2014"

# Total Land ====
TotalLand <- filter(AllIndicators, Indicator.Name == "Land area (sq. km)")
TotalLand2014 <- filter(AllIndicators2014, Indicator.Name == "Land area (sq. km)") %>% select(Country.Name, X2014)
names(TotalLand2014)[names(TotalLand2014) == "X2014"] <-"TotalLand2014"

# Population Density ====
PopDensity <- filter(AllIndicators, Indicator.Name == "Population density (people per sq. km of land area)")
PopDensity2014 <- filter(AllIndicators2014, Indicator.Name == "Population density (people per sq. km of land area)") %>% select(Country.Name, X2014)
names(PopDensity2014)[names(PopDensity2014) == "X2014"] <-"PopDensity2014"

# Health Education Dataset Production ----
# Compulsory Education ====
CompulsoryEdu <- filter(AllIndicators, Indicator.Name == "Compulsory education duration (years)")
CompulsoryEdu2015 <- filter(AllIndicators2015, Indicator.Name == "Compulsory education duration (years)") %>% select(Country.Name, X2015)
names(CompulsoryEdu2015)[names(CompulsoryEdu2015) == "X2015"] <-"CompulsoryEdu2015"

#Smoking Male ====
SmokingMale <- filter(AllIndicators, Indicator.Name == "Smoking prevalence males (% of adults)")
SmokingMale2015 <- filter(AllIndicators2015, Indicator.Name == "Smoking prevalence males (% of adults)") %>% select(Country.Name, X2015)
names(SmokingMale2015)[names(SmokingMale2015) == "X2015"] <-"SmokingMale2015"

# Smoking Female ====
SmokingFemale <- filter(AllIndicators, Indicator.Name == "Smoking prevalence females (% of adults)")
SmokingFemale2015 <- filter(AllIndicators2015, Indicator.Name == "Smoking prevalence females (% of adults)") %>% select(Country.Name, X2015)
names(SmokingFemale2015)[names(SmokingFemale2015) == "X2015"] <-"SmokingFemale2015"

# Smoking Total ====
SmokingTotal <- filter(AllIndicators, Indicator.Name == "Smoking prevalence total (ages 15+)")
SmokingTotal2015 <- filter(AllIndicators, Indicator.Name == "Smoking prevalence total (ages 15+)") %>% select(Country.Name, X2015)
names(SmokingTotal2015)[names(SmokingTotal2015) == "X2015"] <-"SmokingTotal2015"

# Death by Communicative Disease ====
DeathCom <- filter(AllIndicators, Indicator.Name == "Cause of death by communicable diseases and maternal prenatal and nutrition conditions (% of total)")
DeathCom2015 <- filter(AllIndicators2015, Indicator.Name == "Cause of death by communicable diseases and maternal prenatal and nutrition conditions (% of total)") %>% select(Country.Name, X2015)
names(DeathCom2015)[names(DeathCom2015) == "X2015"] <-"DeathCom2015"

# Death by Non Comm Disease
DeathNonCom <- filter(AllIndicators, Indicator.Name == "Cause of death by non-communicable diseases (% of total)")
DeathNonCom2015 <- filter(AllIndicators2015, Indicator.Name == "Cause of death by non-communicable diseases (% of total)") %>% select(Country.Name, X2015)
names(DeathNonCom2015)[names(DeathNonCom2015) == "X2015"] <-"DeathNonCom2015"

# LifeTotal ====
LifeTotal2015 <- filter(AllIndicators2015, Indicator.Name == "Life expectancy at birth total (years)") %>% select(Country.Name, X2015)
names(LifeTotal2015)[names(LifeTotal2015) == "X2015"] <-"LifeTotal2015"

# PopTotal ====
PopTotal <- filter(AllIndicators, Indicator.Name == "Population total")
PopTotal2015 <- filter(AllIndicators2015, Indicator.Name == "Population total") %>% select(Country.Name, X2015)
names(PopTotal2015)[names(PopTotal2015) == "X2015"] <-"PopTotal2015"

# Total Land ====
TotalLand <- filter(AllIndicators, Indicator.Name == "Land area (sq. km)")
TotalLand2015 <- filter(AllIndicators2015, Indicator.Name == "Land area (sq. km)") %>% select(Country.Name, X2015)
names(TotalLand2015)[names(TotalLand2015) == "X2015"] <-"TotalLand2015"

# HealthPerCapita ==== 
HealthPC <- filter(AllIndicators, Indicator.Name == "Current health expenditure per capita (current US$)")
HealthPC2015 <- filter(AllIndicators2015, Indicator.Name == "Current health expenditure per capita (current US$)") %>% select(Country.Name, X2015)
names(HealthPC2015)[names(HealthPC2015) == "X2015"] <-"HealthPC2015"

# CSV Creation ----
# Life Expectancy ====
#LifeExpectancyPrediction2 <- LifeExpectancyPrediction[-c(1:47, 51,52,57,69,75,84,87,98,103,109,113,117,123,125,127,134,142,151,153,161,162,165,169,172,176,178,180,185,188,194,198,206,212,220,224,226,229,231,237,248,249,257,260,261,70,73,79,81,114,182,184,186,191,196,200,223),1:20]
LifeExpectancyPrediction <- Surviveto65Female[,1:2]
LifeExpectancyPrediction$Surviveto65Female2014 <- Surviveto65Female$X2014
LifeExpectancyPrediction$Surviveto65Male2014 <- Surviveto65Male$X2014
LifeExpectancyPrediction$LifeFemale2014 <- LifeFemale$X2014
LifeExpectancyPrediction$LifeMale2014 <- LifeMale$X2014
LifeExpectancyPrediction$LifeTotal2014 <- LifeTotal$X2014
LifeExpectancyPrediction$HealthPerCapita2014 <- HealthPerCapita$X2014
LifeExpectancyPrediction$HealthGDP2014 <- HealthGDP$X2014
LifeExpectancyPrediction$PopFemale2014 <- PopFemale$X2014
LifeExpectancyPrediction$PopMale2014 <- PopMale$X2014
LifeExpectancyPrediction$PopTotal2014 <- PopTotal$X2014
LifeExpectancyPrediction$RuralPOP2014 <- RuralPOP$X2014
LifeExpectancyPrediction$Sanitation2014 <- Sanitation$X2014
LifeExpectancyPrediction$CO22014 <- CO2$X2014
LifeExpectancyPrediction$TotalGreenhouse2012 <- Greenhouse$X2012
LifeExpectancyPrediction$Education2014 <- Education$X2014
LifeExpectancyPrediction$Water2014 <- Water$X2014

write.csv(LifeExpectancyPrediction, "~/GitKraken/Final-Project-David-Adam-/Working Datasets/LifeExpectancyPrediction.csv")
#, sep = ",", row.names = FALSE)
LifeExpectancyPrediction2014 <-
  Surviveto65Female2014 %>% inner_join(Surviveto65Male2014) %>% inner_join(LifeFemale2014) %>% inner_join(LifeMale2014) %>% inner_join(LifeTotal2014) %>% inner_join(HealthPerCapita2014) %>% inner_join(HealthGDP2014) %>% inner_join(PopFemale2014) %>% inner_join(PopMale2014) %>% inner_join(RuralPOP2014) %>% inner_join(Sanitation2014) %>% inner_join(CO22014) %>% inner_join(Greenhouse2012) %>% inner_join(Education2014) %>% inner_join(Water2014)
write.csv(LifeExpectancyPrediction2014, "~/GitKraken/Final-Project-David-Adam-/Working Datasets/LifeExpectancyPrediction2014.csv")

# Land Investigation ====
LandInvestigation <- AverageRain[,1:2]
LandInvestigation$AverageRain2014 <- AverageRain$X2014
LandInvestigation$ForestLand2014 <- ForestLand$X2014
LandInvestigation$AgriculturalLand2014 <- AgriculturalLand$X2014
LandInvestigation$RuralLand2010 <- RuralLand$X2014
LandInvestigation$UrbanLand2010 <- UrbanLand$X2014
LandInvestigation$PermCropLand2014 <- PermCropLand$X2014
LandInvestigation$TotalLand2014 <- TotalLand$X2014
LandInvestigation$PopDensity2014 <- PopDensity$X2014
LandInvestigation$LifeTotal2014 <- LifeTotal$X2014

write.csv(LandInvestigation, "~/GitKraken/Final-Project-David-Adam-/Working Datasets/LandInvestigation.csv")

LandInvestigation2014 <-
  AverageRain2014 %>% inner_join(ForestLand2014) %>% inner_join(AgriculturalLand2014) %>% inner_join(RuralLand2000) %>% inner_join(UrbanLand2000) %>% inner_join(PermCropLand2014) %>% inner_join(TotalLand2014) %>% inner_join(PopDensity2014) %>% inner_join(LifeTotal2014)

write.csv(LandInvestigation2014, "~/GitKraken/Final-Project-David-Adam-/Working Datasets/LandInvestigation2014.csv")

# Health Investigation ====
HealthInvestigation <- CompulsoryEdu[,1:2]
HealthInvestigation$CompulsoryEdu2015 <- CompulsoryEdu$X2015
HealthInvestigation$SmokingMale2015 <- SmokingMale$X2015
HealthInvestigation$SmokingFemale2015 <- SmokingFemale$X2015
HealthInvestigation$SmokingTotal2015 <- SmokingTotal$X2015
HealthInvestigation$DeathCom2015 <- DeathCom$X2015
HealthInvestigation$DeathNonCom2015 <- DeathNonCom$X2015
HealthInvestigation$LifeTotal2015 <- LifeTotal$X2015
HealthInvestigation$PopTotal2015 <- PopTotal$X2015

HealthInvestigation2015 <- 
  CompulsoryEdu2015 %>% inner_join(SmokingMale2015) %>% inner_join(SmokingFemale2015) %>% inner_join(SmokingTotal2015) %>% inner_join(DeathCom2015) %>% inner_join(DeathNonCom2015) %>% inner_join(PopTotal2015) %>% inner_join(TotalLand2015) %>% inner_join(HealthPC2015) %>% inner_join(WealthLevel) %>% inner_join(LifeTotal2015) 

write.csv(HealthInvestigation2015, "~/GitKraken/Final-Project-David-Adam-/Working Datasets/HealthInvestigation2015.csv")
