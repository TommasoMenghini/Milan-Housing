rm(list = ls())

library(ggplot2)
library(MASS)
library(dplyr)

source("FunzioniUtili.R")

data <- read.csv("test.csv", header = T)
str(data)


skimr::skim(data)


# Other Features ----------------------------------------------------------

data$other_features[1]
data$other_features[2]

out <- c()

for (i in 1:nrow(data)) {
  s <- data$other_features[i]
  split_righe <- strsplit(s, "\\|")[[1]]
  parole <- trimws(split_righe)
  out <- c(out, parole)
}

parole_uniche <- unique(out)

tab <- table(out, useNA = "always")
tab 


# Binary Variables -------------------------------------------------------

binaryExtr("fireplace") 
binaryExtr("hydromassage")
binaryExtr("video entryphone")
binaryExtr("terrace")
binaryExtr("security door")
binaryExtr("tavern")
binaryExtr("optic fiber")
binaryExtr("electric gate")
binaryExtr("pool")
binaryExtr("tennis court")
binaryExtr("alarm system")
binaryExtr("attic")
binaryExtr("balcony")
binaryExtr("cellar")
binaryExtr("closet")
binaryExtr("reception")

# Multicategorical Variables ---------------------------------------------------

# Concierge ---------------------------------------------------------------

indici1 <- grep("half-day concierge", data$other_features, ignore.case = TRUE)
indici2 <- grep("full day concierge", data$other_features, ignore.case = T)

any(indici1 %in% indici2) 

data$concierge[1:4800] <- NA 
data$concierge[indici1] <- "half-day concierge"
data$concierge[indici2] <- "full day concierge"
data$concierge[-c(indici1, indici2)] <- "no concierge"

table(data$concierge)

concierge_map <- c(
  "no concierge" = 0,
  "half-day concierge"  = 0.5,
  "full day concierge" = 1)

data$concierge <- concierge_map[as.character(data$concierge)]
mode(data$concierge)

# Furniture ---------------------------------------------------------------

indici123 <- grep("furnished", data$other_features, ignore.case = TRUE)
indici2 <- grep("partially furnished", data$other_features, ignore.case = T)
indici3 <- grep("only kitchen furnished", data$other_features, ignore.case = T)


length(indici123) - length(indici2) - length(indici3) 

indici1 <- setdiff(indici123, union(indici2, indici3))
length(indici1)

any(indici1 %in% indici2) # F
any(indici1 %in% indici3) # F
any(indici2 %in% indici3) # F 

data$furnished[1:4800] <- NA 
data$furnished[indici1] <- "totally"
data$furnished[c(indici2, indici3)] <- "partially"
data$furnished[-c(indici1, indici2, indici3)] <- "not furnished"

table(data$furnished)

furnished_map <- c(
  "not furnished" = 0,
  "partially"  = 0.5,
  "totally" = 1)

data$furnished <- furnished_map[as.character(data$furnished)]
mode(data$furnished)


# Garden ------------------------------------------------------------------

indici1 <- grep("private and shared garden", data$other_features, ignore.case = T)
indici2 <- grep("private garden", data$other_features, ignore.case = T)
indici31 <- grep("shared garden", data$other_features, ignore.case = T)

indici3 <- setdiff(indici31, indici1)
length(indici3) # torna
tab # torna

any(indici1 %in% indici2) # F
any(indici1 %in% indici3) # F
any(indici2 %in% indici3) # F 

data$garden[1:4800] <- NA
data$garden[indici1] <- "private and shared"
data$garden[indici2] <- "private"
data$garden[indici3] <- "shared"
data$garden[-c(indici1, indici2, indici3)] <- "no garden"

table(data$garden) 

data$garden[indici1] <- "private"

data$garden <- as.factor(data$garden)
table(data$garden)


# Tv System ---------------------------------------------------------------

indici1 <- grep("centralized tv system", data$other_features, ignore.case = T)
indici2 <- grep("single tv system", data$other_features, ignore.case = T)

any(indici2 %in% indici1) # F

data$tv_system[1:4800] <- NA
data$tv_system[indici1] <- "centralized"
data$tv_system[indici2] <- "single"
data$tv_system[-c(indici1, indici2)] <- NA


# Exposure -------------------------------------------------------------

tab

indici1 <- grep("double exposure", data$other_features, ignore.case = T)
length(indici3) # 3629
indici2 <- grep("internal exposure", data$other_features, ignore.case = T)
indici3 <- grep("external exposure", data$other_features, ignore.case = T)

data$exposure <- c()
data$exposure[1:4800] <- NA
data$exposure[indici1] <- "double"
data$exposure[c(indici2, indici3)] <- "single"
data$exposure[which(is.na(data$exposure))] <- "other"

table(data$exposure)
data$exposure <- as.factor(data$exposure)


# Window Frames ------------------------------------------------------------------

indici123 <- grep("glass", data$other_features, ignore.case = T)
length(indici123) 

indici2 <- grep("double glass", data$other_features, ignore.case = T)
length(indici2) #5145

indici3 <- grep("triple glass", data$other_features, ignore.case = T)
length(indici3) #452

any(indici123 %in% indici3) # T
any(indici123 %in% indici2) # T
any(indici2 %in% indici3) # F

indici1 <- setdiff(indici123, c(indici2, indici3))

any(indici1 %in% indici3) # F
any(indici1 %in% indici2) # F

length(indici1) #1221


data$window_glass[1:4800] <- NA
data$window_glass[-indici3] <- "other"
data$window_glass[indici3] <- "triple"

data$window_glass <- as.factor(data$window_glass)
table(data$window_glass)



# Window Material ---------------------------------------------------------

indici1 <- grep("metal", data$other_features, ignore.case = T)
length(indici1) 

indici2 <- grep("pvc", data$other_features, ignore.case = T)
length(indici2) 

indici3 <- grep("wood", data$other_features, ignore.case = T)
length(indici3) 

length(indici1) + length(indici2) + length(indici3)

data$window_material[1:4800] <- NA
data$window_material[-indici3] <- "other"
data$window_material[indici3] <- "wood"

data$window_material <- as.factor(data$window_material)
table(data$window_material)


# Pre-Processing: Fase 2 --------------------------------------------------

data <- data %>%
  select(-other_features)

skimr::skim(data)


# Missing: Part I ------------------------------------------------------

library(Hmisc)
library(naniar)

na.patters <- naclus(data)
naplot(na.patters, "na per var")

n_miss(data) # total number of NAs
n_var_miss(data) # number of covariates with NAs
miss_var_which(data) # the variables with missing data
print(miss_var_summary(data), n = 39) # for each variable percentage of missing

gg_miss_var(data) # missingness by variable

n_case_miss(data) # how many rows have at least a missing value
n_case_complete(data) # how many rows do not have a single missing value

miss_case_table(data)
# Mostra quante sono le righe con 0 NA e la loro percentuale sul totale.
# Mostra quante sono le righe con 1 NA e la loro percentuale sul totale.
# Mostra quante sono le righe con 2 NA e la loro percentuale sul totale.
# Mostra quante sono le righe con 3 NA e la loro percentuale sul totale.

miss_case_summary(data)
# mostra gli indici delle righe e il rispettivo conteggio delle NA


# Variable: zone ------------------------------------------------------------------------------------------------

tab_zone <- table(data$zone, useNA = "always")
tab_zone
names(tab_zone)

p <- boxplot(tab_zone)
p$n 
p$stats 
p$out 

data$zona_new <- data$zone

data$zona_new[data$zona_new == "brera"] <- "duomo"
data$zona_new[data$zona_new == "cascina gobba"] <- "crescenzago"
data$zona_new[data$zona_new == "figino"] <- "quarto cagnino"
data$zona_new[data$zona_new == "lanza"] <- "duomo"
data$zona_new[data$zona_new == "parco lambro"] <- "cimiano"
data$zona_new[data$zona_new == "qt8"] <- "portello - parco vittoria"
data$zona_new[data$zona_new == "quadrilatero della moda"] <- "duomo"
data$zona_new[data$zona_new == "rogoredo"] <- "corvetto"
data$zona_new[data$zona_new == "san babila"] <- "duomo"
data$zona_new[data$zona_new == "sant'ambrogio"] <- "san vittore"
data$zona_new[data$zona_new == "scala - manzoni"] <- "duomo"
data$zona_new[data$zona_new == "via calizzano"] <- "comasina"
data$zona_new[data$zona_new == "via canelli"] <- "cimiano"
data$zona_new[data$zona_new == "via fra' cristoforo"] <- "famagosta"
data$zona_new[data$zona_new == "via marignano, 3"] <- "corvetto"
data$zona_new[data$zona_new == "largo caioroli 2"] <- "cadorna - castello"
data$zona_new[data$zona_new == "corso magenta"] <- "cadorna - castello"

p <- boxplot(table(data$zona_new))
p$n 
p$stats 
p$out 


data$zona_new <- as.factor(data$zona_new)

# Variable: rooms_number -------------------------------------------------

data$rooms_number <- as.factor(data$rooms_number)
summary(data$rooms_number)

# Furthermore, variables that assign numbers to categories that are ordered
# but for which the gaps between neighboring categories are not always equivalent
# are often good candidates for discretization.


# Variable: square meters ------------------------------------------------

summary(data$square_meters)
anyNA(data$square_meters) 

indici <- which(data$square_meters < 20)
length(indici)

(mq_wrong <- data$square_meters[indici])

data$square_meters[indici] <- NA

hist(data$square_meters)
hist(log(data$square_meters))


# Variable: Year --------------------------------------------------------------

summary(data$year_of_construction)
boxplot(data$year_of_construction)
hist(data$year_of_construction)
hist(sqrt(data$year_of_construction))

# Variable: Year --------------------------------------------------------------

summary(data$year_of_construction)
boxplot(data$year_of_construction)
hist(data$year_of_construction)
hist(sqrt(data$year_of_construction))

data[which(data$year_of_construction == 1111), ]
data[which(data$year_of_construction == 1300), ]
data[which(data$year_of_construction == 1400), ]

data$year[1:4800] <- NA

indici <- which(data$year_of_construction <= 1945)
data$year[indici] <- "Very Old"

indici <- which(data$year_of_construction >= 1946 & data$year_of_construction <= 1960)
data$year[indici] <- "Old"

indici <- which(data$year_of_construction >= 1961 & data$year_of_construction <= 2000)
data$year[indici] <- "Modern"

indici <- which(data$year_of_construction >= 2001 & data$year_of_construction <= 2027)
data$year[indici] <- "Recent/New"

data$year <- as.factor(data$year)
data$year <- factor(data$year, levels = c("Recent/New", "Modern",
                                          "Old", "Very Old"), ordered = T)

table(data$year, useNA = "always")



# Variable: Bathrooms_number ---------------------------------------------

data$bathrooms_number
table(data$bathrooms_number, useNA = "always")

prop.table(table(data$bathrooms_number))
data$bathrooms_number <- as.factor(data$bathrooms_number)
data$bathrooms_number <- factor(data$bathrooms_number, levels = c("1", "2", "3", "3+"), ordered = T)


# Variable: total_floors  -----------------------------------------

data$total_floors_in_building
table(data$total_floors_in_building, useNA = "always")

data$total_floors_in_building[data$total_floors_in_building == "1 floor"] <- "1"
data$total_floors <- as.numeric(data$total_floors_in_building)
summary(data$total_floors)

# Variable: floor -------------------------------------------------------------

data$floor
table(data$floor, useNA = "always") 

data$floor <- as.factor(data$floor)
data$floor <- factor(data$floor, levels = c("semi-basement", "ground floor",
                                            "mezzanine", "1", "2", "3", 
                                            "4", "5", "6", "7", "8", "9"), ordered = T)

# Inconsistencies Floor vs Total_Floors  ------------------------------------

tab <- table(data$floor, data$total_floors, useNA = "always")
tab 

indici <- which(as.numeric(data$floor) - 3 > as.numeric(data$total_floors_in_building))
data$total_floors[indici] <- NA


# Variable: lift ---------------------------------------------------------

data$lift <- as.factor(data$lift)
table(data$lift, useNA = "always")

# Single Imputation lift ---------------------------------------------

indici <- which(is.na(data$lift))

table(data$total_floors[indici], data$lift[indici], useNA = "always")

data$lift[which(data$total_floors > 3 & is.na(data$lift))] <- "yes"
data$lift[which(as.numeric(data$floor) > 6 & is.na(data$lift))] <- "yes"

data$floor[which(is.na(data$total_floors) & is.na(data$lift))]


indici <- which(is.na(data$lift) & !is.na(data$total_floors))
df <- data.frame(t_floors = data$total_floors[-indici], lift = data$lift[-indici])
for(i in indici){
  
  f <- data$total_floors[i]
  sub_df_temp <- df %>% filter(t_floors == f)
  freq.cond <- table(sub_df_temp$lift)
  freq.ass <- table(data$lift)[names(freq.cond)]
  freq.rel <- freq.cond / freq.ass
  
  which.max(freq.rel)
  
  data$lift[i] <- levels(data$lift)[which.max(freq.rel)]
  
}

table(data$lift, useNA = "always")

data$lift[which(is.na(data$total_floors) & is.na(data$lift))] <- "yes"
table(data$lift, useNA = "always")


# Variable: tv_system ---------------------------------------------------------------

data <- data %>% select(-tv_system)

# Variable: Availability -------------------------------------------------

data$availability <- as.factor(data$availability)
levels(data$availability)

indici2023 <- grep("2023", data$availability, ignore.case = TRUE)
indici2024 <- grep("2024", data$availability, ignore.case = TRUE)
indici2025 <- grep("2025", data$availability, ignore.case = TRUE)
indici2026 <- grep("2026", data$availability, ignore.case = TRUE)
indici2027 <- grep("2027", data$availability, ignore.case = TRUE)
indici2028 <- grep("2028", data$availability, ignore.case = TRUE)
indici2029 <- grep("2029", data$availability, ignore.case = TRUE)
indici_NA <- which(is.na(data$availability))
indici_now <- setdiff(c(1:dim(data)[1]), c(indici2023, indici2024, indici2025, 
                                           indici2026, indici2027, indici2028,
                                           indici2029, indici_NA))


data$availability <- as.character(data$availability)

data$availability[indici2023] <- "from 2023"
data$availability[indici2024] <- "from 2024"
data$availability[indici2025] <- "from 2025"
data$availability[indici2026] <- "from 2026"
data$availability[indici2027] <- "from 2027"
data$availability[indici2028] <- "from 2028"
data$availability[indici2029] <- "from 2029"
data$availability[indici_now] <- "from now"

table(data$availability, useNA = "always")

options(max.print = 1e6)  
table(data$availability, data$conditions, useNA = "always")
options(max.print = 500) 

# Variable: car_parking --------------------------------------------------

data$car_parking <- as.factor(data$car_parking)
table(data$car_parking, useNA = "always") 

levels(data$car_parking)

# Categories:
# No
# garage/box
# garage + shared
# shared only

car_map <- c(
  "no" = "none",
  "1 in garage/box"  = "garage/box",
  "2 in garage/box"  = "garage/box",
  "1 in garage/box, 2 in shared parking" = "garage/box and shared",
  "1 in garage/box, 5 in shared parking" = "garage/box and shared",
  "2 in garage/box, 2 in shared parking" = "garage/box and shared",
  "2 in garage/box, 6 in shared parking" = "garage/box and shared",
  "1 in garage/box, 1 in shared parking" = "garage/box and shared",
  "1 in garage/box, 3 in shared parking" = "garage/box and shared",
  "2 in garage/box, 1 in shared parking" = "garage/box and shared",
  "2 in garage/box, 3 in shared parking" = "garage/box and shared",
  "2 in garage/box, 7 in shared parking" = "garage/box and shared",
  "7 in garage/box, 3 in shared parking" = "garage/box and shared",
  "2 in shared parking" = "shared",
  "3 in shared parking" = "shared",
  "5 in shared parking" = "shared",
  "9 in shared parking" = "shared",
  "1 in shared parking" = "shared",
  "20 in shared parking" = "shared",
  "4 in shared parking" = "shared"
)

data$car_parking <- car_map[as.character(data$car_parking)]
data$car_parking <- as.factor(data$car_parking)

summary(data$car_parking)
indici <- which(is.na(data$car_parking))
data$car_parking[indici] <- "none"

# Variable: Conditions ---------------------------------------------------

data$conditions <- as.factor(data$conditions)
table(data$conditions, useNA = "always")

levels(data$conditions) <- c(levels(data$conditions), "other")
data$conditions[which(is.na(data$conditions))] <- "other"

table(data$conditions, useNA = "always")

# Variable: heating centralized -------------------------------------------------------------

data$heating_centralized <- as.factor(data$heating_centralized)
table(data$heating_centralized, useNA = "always")

# Variable: energy efficiency --------------------------------------------

table(data$energy_efficiency_class, useNA = "always")

data$energy_efficiency_class[which(data$energy_efficiency_class == ",")] <- "g"

data$energy_efficiency_class <- as.factor(data$energy_efficiency_class)
table(data$energy_efficiency_class, useNA = "always")

data$house_efficiency[1:4800] <- NA

data$house_efficiency[which(data$energy_efficiency_class %in% c("f", "g"))] <- "Low"
data$house_efficiency[which(data$energy_efficiency_class %in% c("c", "d", "e"))] <- "Med"
data$house_efficiency[which(data$energy_efficiency_class %in% c("a", "b"))] <- "High"

data$house_efficiency <- factor(data$house_efficiency,
                                levels = c("Low", "Med", "High"), ordered = T)

table(data$house_efficiency, useNA = "always")


# Variable: condominium fees ---------------------------------------------

data$condominium_fees
levels(as.factor(data$condominium_fees)) 

indici <- which(data$condominium_fees == "No condominium fees") 
data$condominium_fees[indici] <- "0"

data$condominium_fees <- as.numeric(data$condominium_fees)

summary(data$condominium_fees)
bp <- boxplot(data$condominium_fees)
bp$stats

length(which(data$condominium_fees > 2500)) 

data$condominium_fees[which(data$condominium_fees > 2500)] <- NA

summary(data$condominium_fees)


# Variable: ID -----------------------------------------------------------

data <- data %>% select(-c(availability, zone, energy_efficiency_class, total_floors_in_building))

skimr::skim(data)


# Iterative Regression Imputation -----------------------------------------

set.seed(123)
data.imp <- data
data.imp$square_meters.imp <- random.imp(data$square_meters)
data.imp$condominium_fees.imp <- random.imp(data$condominium_fees)
data.imp$bathrooms_number.imp <- random.imp(data$bathrooms_number)
data.imp$heating_centralized.imp <- random.imp(data$heating_centralized)
data.imp$house_efficiency.imp <- random.imp(data$house_efficiency)
data.imp$year.imp <- random.imp(data$year)
data.imp$total_floors.imp <- random.imp(data$total_floors)


n.sims <- 7
train.mae.sqm <- c()
train.acc.baths <- c()
train.acc.heat <- c()
train.acc.house <- c()
train.acc.year <- c()
train.mae.condo <- c()
train.mae.tfloor <- c()


for(s in 1:n.sims){
  
  # Square Meters
  
  lm.1 <- lm(log(square_meters) ~ bathrooms_number.imp + rooms_number,
             data = data.imp)
  pred.1 <- floor(exp(predict(lm.1, newdata = data.imp)))
  train.mae.sqm[s] <- sum(abs(data.imp$square_meters.imp - pred.1))
  square_meters.imp <- impute(data$square_meters, pred.1) 
  
  # Bathrooms Number
  
  plr.1 <- polr(bathrooms_number ~ rooms_number + square_meters.imp, data = data.imp)
  pred.plr.1 <- predict(plr.1, newdata = data.imp)
  train.acc.baths[s] <- sum(as.numeric(pred.plr.1) == as.numeric(data.imp$bathrooms_number.imp))/nrow(data) 
  
  data.imp$bathrooms_number.imp <- impute(data$bathrooms_number, pred.plr.1)
  
  # Heating Centralized
  
  glm.heating <- glm(heating_centralized ~ conditions + condominium_fees.imp + 
                       house_efficiency.imp + fireplace, data = data.imp,
                     family = binomial(link = logit))
  pred.heating <- ifelse(predict(glm.heating, newdata = data.imp, type = "response") >= 0.5, "independent", "central")
  train.acc.heat[s] <- sum(as.numeric(as.factor(pred.heating)) == as.numeric(data.imp$heating_centralized.imp))/nrow(data) 
  data.imp$heating_centralized.imp <- impute(data$heating_centralized, pred.heating)
  
  # House Efficiency
  
  plr.2 <- polr(house_efficiency ~ conditions + heating_centralized.imp + year.imp +
                  pool + fireplace + pool + hydromassage, data = data.imp)
  pred.plr.2 <- predict(plr.2, newdata = data.imp)
  train.acc.house[s] <- sum(as.numeric(pred.plr.2) == as.numeric(data.imp$house_efficiency.imp))/nrow(data) 
  data.imp$house_efficiency.imp <- impute(data$house_efficiency, pred.plr.2)
  
  # Year
  
  plr.3 <- polr(year ~ conditions + house_efficiency.imp + zona_new + fireplace + optic_fiber, data = data.imp)
  pred.plr.3 <- predict(plr.3, newdata = data.imp)
  train.acc.year[s] <- sum(as.numeric(pred.plr.3) == as.numeric(data.imp$year.imp))/nrow(data)
  data.imp$year.imp <- impute(data$year, pred.plr.3)
  
  
  # Condominium Fees
  
  glm.sign <- glm(I(condominium_fees > 0) ~ lift + heating_centralized.imp +
                    total_floors.imp + car_parking + reception + pool + hydromassage +
                    conditions + house_efficiency.imp,
                  data = data.imp, family = binomial(link = logit))
  lm.ifpos.sqrt <- lm(I(sqrt(condominium_fees)) ~ lift + heating_centralized.imp +
                        total_floors.imp + car_parking + reception + pool + hydromassage +
                        conditions + house_efficiency.imp,
                      data = data.imp, subset = condominium_fees > 0)
  pred.sign <- ifelse(predict(glm.sign, newdata = data.imp, type = "response") >= 0.5, 1, 0)
  pred.pos.sqrt <- predict(lm.ifpos.sqrt, newdata = data.imp)
  pred.pos <- pred.pos.sqrt^2
  train.mae.condo[s] <- sum(abs(data.imp$condominium_fees.imp - pred.pos))
  condominium_fees.imp <- impute(data$condominium_fees, pred.sign*pred.pos)
  
  # Total Floors
  
  lm.2 <- lm(log(total_floors) ~ condominium_fees.imp + floor, data = data.imp)
  pred.2 <- floor(exp(predict(lm.2, newdata = data.imp)))
  train.mae.tfloor[s] <- sum(abs(data.imp$total_floors.imp - pred.2))
  total_floors.imp <- impute(data$total_floors, pred.2) 
  
}


## Checks 

# Square Meters
summary(data.imp$square_meters.imp)
summary(data$square_meters)
summary(data.imp$square_meters.imp[which(is.na(data$square_meters))])

par(mfrow = c(1, 2))
hist(data$square_meters)
hist(data.imp$square_meters.imp)


plot(train.mae.sqm, type = "l", col = "red")

# Bathrooms_Number
summary(data.imp$bathrooms_number.imp)
summary(data$bathrooms_number)

prop.table(table(data$bathrooms_number))
prop.table(table(data.imp$bathrooms_number.imp[which(is.na(data$bathrooms_number))]))

plot(train.acc.baths, type = "l", col = "red")


# Heating Centralized
table(data.imp$heating_centralized.imp)
table(data$heating_centralized)

prop.table(table(data$heating_centralized))
prop.table(table(data.imp$heating_centralized.imp[which(is.na(data$heating_centralized))]))

plot(train.acc.heat, type = "l", col = "red")


# House Efficiency
summary(data.imp$house_efficiency.imp)
summary(data$house_efficiency)

prop.table(table(data.imp$house_efficiency.imp[which(is.na(data$house_efficiency))]))
prop.table(table(data$house_efficiency))

plot(train.acc.house, type = "l", col = "red")

## Years
summary(data.imp$year.imp)
summary(data$year)

prop.table(table(pred.plr.3[which(is.na(data$year))]))
prop.table(table(data$year))

plot(train.acc.year, type = "l", col = "red")


# Condominium Fees
summary(data.imp$condominium_fees.imp)
summary(data$condominium_fees)
summary(data.imp$condominium_fees.imp[which(is.na(data$condominium_fees))]) 

par(mfrow = c(1, 3))
hist(data$condominium_fees)
hist(data.imp$condominium_fees.imp)
hist(data.imp$condominium_fees.imp[which(is.na(data$condominium_fees))])
par(mfrow = c(1, 1))


plot(train.mae.condo, type = "l", col = "red")


# Total Floors
summary(data.imp$total_floors.imp)
summary(data$total_floors)
summary(data.imp$total_floors.imp[which(is.na(data$total_floors))])

par(mfrow = c(1, 3))
hist(data$total_floors)
hist(data.imp$total_floors.imp)
hist(data.imp$total_floors.imp[which(is.na(data$total_floors))])
par(mfrow = c(1, 1))


# Near-Constant Variables -------------------------------------------------

# Caret package need to be installed
# guarda quelle variabili che hanno circa varianza pari a 0
colnames(data)[caret::nearZeroVar(data)]


# Results ---------------------------------------------------------------

data <- data %>% select(-c(year_of_construction, year))

data$square_meters <- data.imp$square_meters.imp
data$condominium_fees <- data.imp$condominium_fees.imp
data$bathrooms_number <- data.imp$bathrooms_number.imp
data$heating_centralized <- data.imp$heating_centralized.imp
data$house_efficiency <- data.imp$house_efficiency.imp
data$year <- data.imp$year.imp
data$total_floors <- data.imp$total_floors.imp


skimr::skim(data)


write.csv(data.frame(data), "cleaned_test.csv", row.names = FALSE)
