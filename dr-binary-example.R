load(url("https://github.com/LucyMcGowan/dr-example-code/raw/master/df_binary.rda"))

## Fit the propensity score ----
df_binary$p_1 <- predict(glm(z ~ x_1 + x_2, data = df_binary, family = binomial),
                       type = "response")

## Create weights ----

## Calculate the probability of receiving control
df_binary$p_0 <- 1 - df_binary$p_1
  
### Calculate the probability of being assigned the treatment you received
df_binary$p_assign <- ifelse(df_binary$z == 1,
                             df_binary$p_1,
                             df_binary$p_0)

### ATE
df_binary$w_ate <- 1 / df_binary$p_assign

### ATM
df_binary$w_atm <- pmin(df_binary$p_1, df_binary$p_0) / df_binary$p_assign

### ATO 
df_binary$w_ato <- 1 - df_binary$p_assign

## Fit outcome models ----

df_binary$y_1 <- predict(glm(y ~ x_1, data = df_binary[df_binary$z == 1, ], family = binomial),
                         newdata = df_binary,
                         type = "response")

df_binary$y_0 <- predict(glm(y ~ x_1, data = df_binary[df_binary$z == 0, ], family = binomial),
                         newdata = df_binary,
                         type = "response")

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

dr(df_binary, "w_ate")
dr(df_binary, "w_atm")
dr(df_binary, "w_ato")
