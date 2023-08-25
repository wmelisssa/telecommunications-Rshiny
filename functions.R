#functions


## Round Numerics
round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

## Percentage Table
percentage.table <- function(x, digits = 1){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}


## Create a user-friendly table of coefficients from the output of a linear regression model.
linear.regression.summary <- function(lm.mod, digits = 3){
  require(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  alpha = 0.05
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}

## Create a user-friendly table of coefficients from the output of a logistic regression model.
logistic.regression.summary <- function(glm.mod, digits = 3){
  require(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  alpha = 0.05
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  return(glm.coefs[])
}


## Create a user-friendly table of coefficients from the output of a linear / logistic regression model.

# dat:  a data.frame or data.table object
# the.formula:  a formula specification for the model.
# model.type:  "logistic" or "linear"
# digits:  How many digits to round the results to.

fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = T)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}


#Q1 Function
respondent <- function(data, var){
  res <- percentage.table(x = data[, .SD[1], by = id.name][, get(var)])
  data.table(res)
}


#Q2 Function

engagement <- function(data, engage, age, gender, income, region, persona, k){
  tab <- data[get(age.group.name) %in% age & get(gender.name) %in% gender & 
                get(income.group.name) %in% income & get(region.name) %in% region & 
                get(persona.name) %in% persona][, .(Mean = round.numerics(100*mean(get(engage), na.rm=TRUE), 2)), by = product.name]
  setorderv(x = tab, cols = "Mean", order = -1)
  topk.engage <- tab[1:k]
  return(topk.engage)
}


#Q3 Function

brand.percept <- function(data, age, gender, income, region, persona, k){
  brand <- data[get(age.group.name) %in% age & get(gender.name) %in% gender & 
                  get(income.group.name) %in% income & get(region.name) %in% region & 
                  get(persona.name) %in% persona][, .(ave.user.friendly = mean(get(user.friendly.name), na.rm=TRUE),
                                                      ave.fast = mean(get(fast.name), na.rm=TRUE),
                                                      ave.battery.life = mean(get(battery.life.name), na.rm=TRUE),
                                                      ave.camera = mean(get(camera.name), na.rm=TRUE),
                                                      ave.sleek = mean(get(sleek.name), na.rm=TRUE),
                                                      ave.stylish = mean(get(stylish.name), na.rm=TRUE),
                                                      ave.status = mean(get(status.symbol.name), na.rm=TRUE),
                                                      ave.screen = mean(get(good.screen.size.name), na.rm=TRUE),
                                                      ave.boring = 10 - mean(get(boring.name), na.rm=TRUE),
                                                      ave.bulky = 10 - mean(get(bulky.name), na.rm=TRUE),
                                                      ave.fragile = 10 - mean(get(fragile.name), na.rm=TRUE),
                                                      ave.expensive = 10 - mean(get(expensive.name), na.rm=TRUE)), by=product.name]
  brand.overall <- brand[,.(overall.ave.perception = round.numerics(rowMeans(.SD), 2)), by=product.name]
  setorderv(x = brand.overall, cols = "overall.ave.perception", order = -1)
  top.bp <- brand.overall[1:k]
  data.table(top.bp)
}


#Q4 Function

outcome.gap <- function(data, outcome1, outcome2, k, digits){
  ## outcome 1
  if(outcome1 == satisfaction.name){
    ave1 <- data[, .(ave.1 = mean(get(outcome1), na.rm=TRUE)), by = product.name]
  }
  if(outcome1 != satisfaction.name){
    ave1 <- data[!is.na(get(outcome1)), .(ave.1 = sum(get(outcome1) == 1) /.N ), by = product.name]
  }
  ## outcome 2
  if(outcome2 == satisfaction.name){
    ave2 <- data[, .(ave.2 = mean(get(outcome2), na.rm=TRUE)), by = product.name]
  }
  if(outcome2 != satisfaction.name){
    ave2 <- data[!is.na(get(outcome2)), .(ave.2 = sum(get(outcome2) == 1) /.N ), by = product.name]
  }
  ## merge
  engage.diff <- merge(ave1, ave2, by= product.name)[, .(Difference = round.numerics(100*(ave.1 - ave.2), digits)), by=product.name]
  setorderv(x = engage.diff, cols = "Difference", order = -1)
  topk.outcome.gap <- engage.diff[1:k]
  return(topk.outcome.gap)
}



#Q5 Function

ag.engage <- function(data, engage, product, age, gender, income, region, persona){
  subdat <- data[!get(product.name) %in% product & get(age.group.name) %in% age & get(gender.name) %in% gender & 
                   get(income.group.name) %in% income & get(region.name) %in% region & 
                   get(persona.name) %in% persona]
  ag.data <- subdat[, .(age.group = get(age.group.name), gender = get(gender.name), income = get(income.group.name), region = get(region.name), 
                        persona = get(persona.name), aggregated.engage = mean(get(engage), na.rm=TRUE)),by=id.name]
  outcome.dat <- data[get(product.name) %in% product, .(id = get(id.name), outcome = get(awareness.name))]
  ag.eng <- merge(ag.data, outcome.dat, by = 'id')
  ## condition of linear or logistic
  if(engage == satisfaction.name){
    model.type <- "linear"
  }
  if(engage != satisfaction.name){
    model.type <- "logistic"
  }
  ## fit model
  model.input <- c("age.group", "gender", "income", "region", "persona", "aggregated.engage")
  res <- fit.model(dt = ag.eng, outcome.name = "outcome", input.names = model.input, model.type = model.type)
  
  datatable(data = res)
}







