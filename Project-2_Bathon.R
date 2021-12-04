library(tidyverse)
library(ggfortify)
library(olsrr)

# Functions
  
  fixMake <-  function(make) {
    make <- tolower(make)
    fixed <- c()
    for ( val in make) {
      switch(val,
             "alfa-romero" = { fixed <- c(fixed,"alfa-romeo") },
             "maxda" = { fixed <- c(fixed,"mazda") },
             "porcshce" = { fixed <- c(fixed,"porsche") },
             "toyouta" = { fixed <- c(fixed,"toyota") },
             "vokswagen" = { fixed <- c(fixed,"volkswagen") },
             "vw" = { fixed <- c(fixed,"volkswagen") },
             { fixed <- c(fixed,val) }
      )
    }
    return(fixed)
  }
  
  buildIntervals <-  function(  modeldata,) {
    
    confidence <- as.data.frame(predict.lm(model, newdata = data, interval = "confidence")) %>% 
      rename(confLwr = lwr, confUpr = upr)
    
    prediction <- as.data.frame(predict.lm(model, newdata = data, interval = "prediction")) %>%
      rename(predictLwr = lwr, predictUpr = upr) %>%
      select(predictLwr, predictUpr)
    
    intervalData <- cbind(data,confidence,prediction)
    
    return(intervalData)
  }
  
# Task 2

cars <- read.csv("CarPriceData.csv")

## Cleaning

cars2 <- cars %>% 
  filter(!is.na(price)) %>%
  select(
    -X,
    -wheelbase,
    -enginetype,
    -fuelsystem,
    -symboling,
    -stroke,
    -peakrpm,
    -boreratio,
    -doornumber,
    -cylindernumber
    )

## Data Engineering

### Engineering
cars3 <- cars2 %>% 
  separate(CarName, into=c("make","model"), sep=" ") %>% 
  mutate(
    make = fixMake(make),
    projectedVolume = carwidth*carlength*carheight,
    avgMPG = (citympg + highwaympg)/2,
    logHP = log10(horsepower),
    logPrice = log10(price)
    )

### Cleaning
cars4 <- cars3 %>% 
  select(
    -model,
    -citympg,
    -highwaympg,
    -carwidth,
    -carlength,
    -carheight,
    -horsepower
  )

## Visualization

### Figure 1: Size of Car vs logPrice

model1 <- lm(logPrice ~ enginesize, data = cars4)

df1 <- buildIntervals(cars4, model1)

ggpolt

ggplot(df1, aes(x = enginesize)) +
  geom_point(aes(y = logPrice), alpha = .75) +
  geom_smooth(aes(y = logPrice), method = lm, fill = "yellow") +
  geom_line(aes(y = confLwr), linetype = "dashed") +
  geom_line(aes(y = confUpr), linetype = "dashed") +
  geom_line(aes(y = predictLwr), linetype = "dashed", color = "red") +
  geom_line(aes(y = predictUpr), linetype = "dashed", color = "red") +
  labs(
    title = "Figure 1: Size of Car vs logPrice",
    x = "Size of Car",
    y = "logPrice",
  )

### Figure 2: Size of Car vs Car Weight

model2 <- lm(logPrice ~ curbweight, data = cars4)

df2 <- buildIntervals(cars4, model2)

ggplot(df2, aes(x = curbweight)) +
  geom_point(aes(y = logPrice), alpha = .75) +
  geom_smooth(aes(y = logPrice), method = lm, fill = "yellow") +
  geom_line(aes(y = confLwr), linetype = "dashed") +
  geom_line(aes(y = confUpr), linetype = "dashed") +
  geom_line(aes(y = predictLwr), linetype = "dashed", color = "red") +
  geom_line(aes(y = predictUpr), linetype = "dashed", color = "red") +
  labs(
    title = "Figure 1: Car Weight vs log10(Price)",
    x = "Car Weigh",
    y = "log10(price)",
  )

### Figure 3: Engine Size vs Weight of Car

model2 <- lm(curbweight ~ enginesize, data = cars4)

df2 <- buildIntervals(cars4, model2)

ggplot(df2, aes(x = enginesize)) +
  geom_point(aes(y = curbweight), alpha = .75) +
  geom_smooth(aes(y = curbweight), method = lm, fill = "yellow") +
  geom_line(aes(y = confLwr), linetype = "dashed") +
  geom_line(aes(y = confUpr), linetype = "dashed") +
  geom_line(aes(y = predictLwr), linetype = "dashed", color = "red") +
  geom_line(aes(y = predictUpr), linetype = "dashed", color = "red") +
  labs(
    title = "Figure 3: Engine Size vs Weight of Car",
    x = "Weight of Car (lb)",
    y = "Engine Size (cc)",
  )

### Figure 4: logHorsepower vs logPrice

model3 <- lm(logPrice ~ logHP, data = cars4)

df3 <- buildIntervals(cars4, model3)

ggplot(df3, aes(x = logHP)) +
  geom_point(aes(y = logPrice), alpha = .75) +
  geom_smooth(aes(y = logPrice), method = lm, fill = "yellow") +
  geom_line(aes(y = confLwr), linetype = "dashed") +
  geom_line(aes(y = confUpr), linetype = "dashed") +
  geom_line(aes(y = predictLwr), linetype = "dashed", color = "red") +
  geom_line(aes(y = predictUpr), linetype = "dashed", color = "red") +
  labs(
    title = "Figure 4: logHorsepower vs logPrice",
    x = "logHorsepower",
    y = "logPrice",
  )

### Figures 5: Car Make vs Average logPrice\

df5 <-  df4 %>%
  arrange(price)

ggplot(df5, aes(x = make, y=price)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    title = "Figure 5: Car Make vs Average Price",
    x = "Car Make",
    y = "Average price",
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Figures 6: Car Body vs Average logPrice

ggplot(df4, aes(x = carbody, y=logPrice)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    title = "Figure 6: Car Body vs Average logPrice",
    x = "Car Body",
    y = "Average logPrice",
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Task 4

cars5 <- cars4 %>% 
  select(-enginesize)

testModel <- lm(price ~ . - car_ID -logPrice, data=cars5)
(bestSelect <-  ols_step_best_subset(testModel))

finalModel <- lm(logPrice ~ curbweight + logHP + make + carbody + fueltype , data=cars5)
finalSummary <- summary(finalModel)

coefficients <- round(finalSummary$coefficients[,1],4)

numCoe <-  length(coefficients)
coefficients[4:numCoe] <- coefficients[4:numCoe]*10


cars6 <- cars %>%
  filter(is.na(price)) %>%
  select(
    -X,
    -wheelbase,
    -enginetype,
    -fuelsystem,
    -symboling,
    -stroke,
    -peakrpm,
    -boreratio,
    -doornumber,
    -cylindernumber,
    -enginelocation
  ) %>%
  separate(CarName, into = c("make", "model"), sep = " ") %>%
  mutate(
    make = fixMake(make),
    projectedVolume = carwidth * carlength * carheight,
    avgMPG = (citympg + highwaympg) / 2,
    logHP = log10(horsepower),
    logPrice = log10(price)
  ) %>%
  select(
    -model,
    -citympg,
    -highwaympg,
    -carwidth,
    -carlength,
    -carheight,
    -horsepower
  )

cars7 <- buildIntervals(cars6, finalModel) %>%
  select(car_ID, fit, predictLwr, predictUpr) %>%
  mutate(fit = 10^fit, predictLwr = 10^predictLwr, predictUpr = 10^predictUpr) %>%
  rename(price = fit, lower = predictLwr, upper = predictUpr)

write.csv(cars7, "Jaden_Bathon.csv")

10^(coefficients[3]*111+coefficients[2]*2250)

coefficients[2]*2250+coefficients[3]*111

10^50.4111

10^(coefficients[1])*10^(coefficients[2]*2500)

