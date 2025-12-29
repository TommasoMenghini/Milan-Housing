rm(list = ls())

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(broom)


# Loading the data --------------------------------------------------------

data <- read.csv("cleaned_training.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
test <- read.csv("cleaned_test.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

skimr::skim(data)
skimr::skim(test)

# Training, Validation  ---------------------------------------------------

set.seed(123)

id_train <- sort(sample(1:nrow(data), size = floor(0.66 * nrow(data)), replace = FALSE))
id_validation <- setdiff(1:nrow(data), id_train)


# Train e Validation Split
train <- data[id_train, ]
dim(train)
validation <- data[id_validation, ]
dim(validation)

skimr::skim(train)


# Some Graphs --------------------------------------------------------------

# Square Meters vs Selling Price
plot(train$square_meters, train$selling_price)
plot(train$square_meters, log(train$selling_price), ylim = c(10, 16),
     xlab = "Square Meters", ylab = "Logarithm of Selling Price")

data[data$square_meters > 500, ]

p1 <- ggplot(data = train, aes(x = square_meters, y = log(selling_price))) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, span = 0.4, col = "limegreen", linetype = "dashed") +
  theme_minimal() +
  xlab("Square Meters") +
  ylab("Logarithm of Selling Price")

ggsave("log_price_square_meters.png", plot = p1, width = 8, height = 6, dpi = 300)

# Condominium Fees vs Selling Price
plot(train$condominium_fees, train$selling_price)
plot(train$condominium_fees, log(train$selling_price), ylim = c(10, 16),
     xlab = "Condominium Fees", ylab = "Logarithm of Selling Price")

p2 <- ggplot(data = train, aes(x = condominium_fees, y = log(selling_price))) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, span = 0.3, col = "blue", linetype = "dashed") +
  theme_minimal() +
  xlab("Condominium Fees") +
  ylab("Logarithm of Selling Price")

ggsave("log_price_condominium_fees.png", plot = p2, width = 8, height = 6, dpi = 300)


# Total Floors vs Selling Price
plot(train$total_floors, train$selling_price)
plot(train$total_floors, log(train$selling_price), ylim = c(10, 16),
     xlab = "Total_Floors", ylab = "Logarithm of Selling Price")

ggplot(data = train, aes(x = total_floors, y = log(selling_price))) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE, span = 0.3, col = "purple", linetype = "dashed") +
  theme_minimal() +
  xlab("Total Floors") +
  ylab("Logarithm of Selling Price")



# Floor vs Selling Price
boxplot(log(selling_price) ~ floor, data = train)

# Bathrooms_Number vs Selling Price
boxplot(log(selling_price) ~ bathrooms_number, data = train)

# Rooms_Number vs Selling Price
boxplot(log(selling_price) ~ rooms_number, data = train)

# Energy_efficiency_class vs Selling Price
boxplot(log(selling_price) ~ house_efficiency, data = train)

# Zona vs Selling Price
train %>%
  mutate(zona_new = reorder(zona_new, selling_price, FUN = median)) %>% 
  ggplot(aes(x = zona_new, y = log(selling_price))) +
  geom_boxplot() +
  labs(y = "Logarithm Selling Price", 
       x = "Zona") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Conditions vs Selling Price
boxplot(log(selling_price) ~ conditions, data = train)

# Exposure vs Selling Price
boxplot(log(selling_price) ~ exposure, data = train)


# Some Other Graphs -----------------------------------------------------------

boxplot(log(selling_price) ~ car_parking, data = train)
boxplot(log(selling_price) ~ concierge, data = train)
plot(train$concierge, log(train$selling_price))
boxplot(log(selling_price) ~ furnished, data = train)
plot(train$furnished, log(train$selling_price))
boxplot(log(selling_price) ~ tavern, data = train)
boxplot(log(selling_price) ~ cellar, data = train)
boxplot(log(selling_price) ~ attic, data = train)
boxplot(log(selling_price) ~ lift, data = train)
boxplot(log(selling_price) ~ hydromassage, data = train)
boxplot(log(selling_price) ~ pool, data = train)
boxplot(log(selling_price) ~ electric_gate, data = train)
boxplot(log(selling_price) ~ garden, data = train)
boxplot(log(selling_price) ~ window_glass, data = train)
boxplot(log(selling_price) ~ window_material, data = train)

# Setting a benchmark ---------------------------------------------------------------------

MAE <- function(y, y_fit) {
  mean(abs(y - y_fit))
}


y_hat_median <- rep(median(train$selling_price), nrow(validation)) 
round(MAE(validation$selling_price, y_hat_median), 4)


# A Simple Model --------------------------------------------------------------------------

m_simple <- lm(selling_price ~ square_meters + zona_new + condominium_fees +  conditions, data = train)
summary(m_simple)

y_hat_simple <- predict(m_simple, newdata = validation)
summary(y_hat_simple) # we have negative predictions

# We take the log of the response

m_simple <- lm(log(selling_price) ~ square_meters + zona_new + condominium_fees +  conditions, data = train)
summary(m_simple)

# We transform back the predictions to the original scale, however we have to consider the Jensen Inequality

y_hat_simple <- exp(predict(m_simple, newdata = validation))
summary(y_hat_simple)

round(MAE(validation$selling_price, y_hat_simple), 4)


# GAM ---------------------------------------------------------------------

library(mgcv)

# Non linear variables = variables that are modelled as non linear terms
# Linear variables = covariates that are modelled as linear terms

# Non linear variables
spline_vars <- c("square_meters", "condominium_fees", "total_floors")

# All the variables
all_vars <- setdiff(names(train), "selling_price")  

# Linear variables
linear_vars <- setdiff(all_vars, spline_vars)


spline_terms <- paste0("s(", spline_vars, ", bs = 'tp')")

# bs = "tp" stays for thin plate splines. Default choice.

linear_terms <- linear_vars

# Final formula
formula_string <- paste("log(selling_price) ~", paste(c(spline_terms, linear_terms), collapse = " + "))
formula_finale <- as.formula(formula_string)

# Generalized Additive Model
m_gam <- gam(formula_finale, data = train, method = "REML")

m_gam$sp       # smoothing parameters
m_gam$gcv.ubre # minimized smoothing parameter selection score


knitr::kable(tidy(m_gam, parametric = TRUE), digits = 3)
knitr::kable(tidy(m_gam, parametric = FALSE), digits = 3)


library(gratia)

data_plot <- smooth_estimates(m_gam, smooth = "s(square_meters)")

p3 <- ggplot(data = data_plot, aes(x = square_meters, y = .estimate)) +
  geom_point(data = add_partial_residuals(m_gam, data = train), aes(x = square_meters, y = `s(square_meters)`), size = 0.7) +
  geom_line(linewidth = 1, col = "limegreen") +
  theme_light() +
  xlab("Square Meters") +
  ylab("Partial Effect")

ggsave("partial_sqm.png", plot = p3, width = 8, height = 6, dpi = 300)


data_plot <- smooth_estimates(m_gam, smooth = "s(condominium_fees)")

p4 <- ggplot(data = data_plot, aes(x = condominium_fees, y = .estimate)) +
  geom_point(data = add_partial_residuals(m_gam, data = train), aes(x = condominium_fees, y = `s(condominium_fees)`), size = 0.7) +
  geom_line(linewidth = 1, col = "#1170aa") +
  theme_light() +
  xlab("Condominium Fees") +
  ylab("Partial Effect")

ggsave("partial_cfees.png", plot = p4, width = 8, height = 6, dpi = 300)


data_plot <- smooth_estimates(m_gam, smooth = "s(total_floors)")

p5 <- ggplot(data = data_plot, aes(x = total_floors, y = .estimate)) +
  geom_point(data = add_partial_residuals(m_gam, data = train), aes(x = total_floors, y = `s(total_floors)`), size = 0.7) +
  geom_line(linewidth = 1, col = "purple") +
  theme_light() +
  xlab("Total Floors") +
  ylab("Partial Effect")

ggsave("partial_totfloor.png", plot = p5, width = 8, height = 6, dpi = 300)


y_hat_gam <- exp(predict(m_gam, newdata = validation))
summary(y_hat_gam)

MAE(validation$selling_price, y_hat_gam) #  78937.59


## Appendix ###########
# Model with Shrinkage ---------------------------------------------------

m_gam_shrinked <- gam(formula_finale, data = train, method = "REML", select = T)

sum_shrinked <- summary(m_gam_shrinked)
gam.check(m_gam_shrinked, rep = 500) 

y_hat_gam <- exp(predict(m_gam_shrinked, newdata = validation))
summary(y_hat_gam)

MAE(validation$selling_price, y_hat_gam) 
