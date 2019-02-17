## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Model 1: energy ~ metro
## ────────────────────────────────────────

## load the data into a dataframe
  states.data <- readRDS("dataSets/states.rds") 
  data1 <- select(states.data,state,energy,metro)

## review data
  glimpse(data1)
  summary(data1)
  filter(data1,is.na(energy | metro))

## remove missing data from dataframe
  data1 <- na.omit(data1)
  summary(data1)

## plot the data
  ggplot(data1,aes(metro,energy)) +
    geom_point() +
    labs(x = "% pop in Metro", y = "energy per capita")

## fit a model to the data
  mod1 <- lm(energy ~ metro,data1)
  summary(mod1)    

## check model residuals
  plot(mod1, which = c(1, 2))

## Model 2: energy ~ metro + region + area
## ────────────────────────────────────────

  data2 <- select(states.data,state,energy,metro,region,area)

## review data
  glimpse(data2)
  summary(data2)
  filter(data2,is.na(energy))

## remove missing data from dataframe
  data2 <- na.omit(data2)
  summary(data2)

## plot the data
  ggplot(data2,aes(metro,energy)) +
    geom_point()
  
  ggplot(data2,aes(region,energy)) +
    geom_point()
  
  ggplot(data2,aes(area,energy)) +
    geom_point()

## fit a model to the data
  mod2 <- lm(energy ~ metro + region + area,data2)
  summary(mod2)    

## check model residuals
  plot(mod2, which = c(1, 2))

# compare using the anova() function
  anova(mod1,mod2)

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


## Exercise 2(1): interactions
## ────────────────────────────────────────
  data3 <- select(states.data,state,energy,metro,area)
  
  mod3 <- lm(energy ~ metro*area,data3)
  summary(mod3)    
  coef(summary(mod3))

  
## Exercise 2(2): factors
## ────────────────────────────────────────
  data4 <- select(states.data,state,energy,region)
  
  ggplot(data2,aes(region,energy)) +
    geom_point()
  
  mod4 <- lm(energy ~ region,data4)
  summary(mod4)    
  coef(summary(mod4))
  anova(mod4)
  
  