load(url("https://github.com/LucyMcGowan/dr-example-code/raw/master/df_continuous.rda"))

## Fit the propensity score ----
df_continuous$p_1 <- predict(glm(z ~ x_1 + x_2, data = df_continuous, family = binomial("probit")),
                       type = "response")

## Create weights ----

## Calculate the probability of receiving control
df_continuous$p_0 <- 1 - df_continuous$p_1
  
### Calculate the probability of being assigned the treatment you received
df_continuous$p_assign <- ifelse(df_continuous$z == 1,
                             df_continuous$p_1,
                             df_continuous$p_0)

### ATE
df_continuous$w_ate <- 1 / df_continuous$p_assign

### ATM
df_continuous$w_atm <- pmin(df_continuous$p_1, df_continuous$p_0) / df_continuous$p_assign

### ATO 
df_continuous$w_ato <- 1 - df_continuous$p_assign

## Fit outcome models ----

df_continuous$y_1 <- predict(glm(y ~ x_1, data = df_continuous[df_continuous$z == 1, ]),
                         newdata = df_continuous)

df_continuous$y_0 <- predict(glm(y ~ x_1, data = df_continuous[df_continuous$z == 0, ]),
                         newdata = df_continuous)

## Calculate DR estimators

dr <- function(data, weight, y = "y", y_1 = "y_1", y_0 = "y_0", z = "z") {
  y <- data[[y]]
  y_1 <- data[[y_1]]
  y_0 <- data[[y_0]]
  weight <- data[[weight]]
  z <- data[[z]]
       (sum(weight * (y_1 - y_0)) / sum(weight)) + 
         (sum(weight * z * (y - y_1)) / sum(weight * z)) -
         (sum(weight * (1 - z) * (y - y_0)) / sum(weight * (1 - z)))
}

dr(df_continuous, "w_ate")
dr(df_continuous, "w_atm")
dr(df_continuous, "w_ato")
