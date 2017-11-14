## Generate a single dataset using Freedman & Berk (2008) simulation setting
## n = 1000

## Set initial parameters ----
set.seed(525)
n <- 1000
mu_x_1 <- 0.5
mu_x_2 <- 1
cov_z <- 1
var_x_1 <- 2
var_x_2 <- 1
a <- 1
b <- 1
c_1 <- 1
c_2 <- 2
d <- 1
e <- 0.5
f_1 <- 0.25
f_2 <- 0.75

X <- mvtnorm::rmvnorm(n,
                      mean = c(mu_x_1, mu_x_2),
                      sigma = matrix(c(var_x_1, cov_z, cov_z, var_x_2), ncol = 2)
)
x_1 <- X[, 1]
x_2 <- X[, 2]

## Continuous outcome ----
U <- rnorm(n, 0, 1)
V <- rnorm(n, 0, 1)
z <- as.numeric(e + f_1 * x_1 + f_2 * x_2 + V > 0)
y <- a + b * z + c_1 * x_1 + c_2 * x_2 + d * U

df_continuous <- data.frame(
  y = y,
  z = z,
  x_1 = x_1,
  x_2 = x_2
)
save(df_continuous, file = "df_continuous.rda")

## Binary outcome ----
U <- rlogis(n, 0, 1)
V <- rlogis(n, 0, 1)
z <- as.numeric(e + f_1 * x_1 + f_2 * x_2 + V > 0)
y <- (a + b * z + c_1 * x_1 + c_2 * x_2 + d * U > 0)

df_binary <- data.frame(
  y = y,
  z = z,
  x_1 = x_1,
  x_2 = x_2
)

save(df_binary, file = "df_binary.rda")