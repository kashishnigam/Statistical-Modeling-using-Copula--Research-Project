# Load the necessary libraries
if(!require(readxl)) install.packages("readxl")
if(!require(copula)) install.packages("copula")

library(readxl)
library(copula)

# Define the file path
file_path <- "C:/Users/KASHISH NIGAM/Desktop/Last Sem Project/Final Datasets/India/OPEC_India/uniform_distributions-Pre_Covid_India.xlsx"

# Load the data from the Excel file
pre_covid <- read_excel(file_path)

# Convert the data to a matrix (if not already in matrix form)
u_data <- as.matrix(pre_covid)



copulas <- list(
  normal = normalCopula(dim = ncol(u_data)),
  t = tCopula(dim = ncol(u_data)),
  clayton = claytonCopula(dim = ncol(u_data)),
  rot_clayton = rotCopula(claytonCopula(dim = ncol(u_data)), flip = TRUE),
  frank = frankCopula(dim = ncol(u_data)),
  plackett = plackettCopula(),
  gumbel = gumbelCopula(dim = ncol(u_data)),
  rot_gumbel = rotCopula(gumbelCopula(dim = ncol(u_data)), flip = TRUE),
  sjc = joeCopula(dim = ncol(u_data))
)

# Initialize a list to store fit results
fit_results <- list()

# Fit each copula and store the results
for (name in names(copulas)) {
  copula <- copulas[[name]]
  fit <- fitCopula(copula, data = u_data, method = "mpl")
  fit_results[[name]] <- fit
}

# Extract AIC and BIC for each fitted copula
aic_bic <- sapply(fit_results, function(fit) {
  c(AIC = AIC(fit), BIC = BIC(fit))
})


# Convert the results to a data frame for easier viewing
aic_bic_df <- as.data.frame(t(aic_bic))


# Print AIC and BIC values
print(aic_bic_df)


# Determine the best copula based on AIC
best_copula_name <- rownames(aic_bic_df)[which.min(aic_bic_df$AIC)]
best_copula_fit <- fit_results[[best_copula_name]]


# Print the best copula
cat("Best copula based on AIC:", best_copula_name, "\n")
print(summary(best_copula_fit))


# Compute Pearson's correlation coefficient
pearson_rho <- cor(pre_covid, method = "pearson")

# Compute Kendall's tau
kendall_tau <- cor(pre_covid, method = "kendall")

# Print correlation matrices
print("Pearson's correlation coefficient:")
print(pearson_rho)

print("\nKendall's tau:")
print(kendall_tau)


# Define the Plackett copula density function
plackett_copula <- function(u1, u2, alpha) {
  term1 <- (alpha - 1) / (alpha + 1)
  term2 <- (2 / pi) * asin(sqrt(alpha / (alpha + 1)) * sin(pi * u1) * sin(pi * u2))
  density <- term1 + term2
  return(density)
}


# Define the upper tail dependence function
upper_tail_dependence <- function(alpha, n = 10000) {
  u <- runif(n)
  v <- runif(n)
  copula_density <- plackett_copula(u, v, alpha)
  upper_tail <- sum(u > 0.95 & v > 0.95) / sum(u > 0.95)
  return(upper_tail)
}

# Define the lower tail dependence function
lower_tail_dependence <- function(alpha, n = 10000) {
  u <- runif(n)
  v <- runif(n)
  copula_density <- plackett_copula(u, v, alpha)
  lower_tail <- sum(u < 0.05 & v < 0.05) / sum(u < 0.05)
  return(lower_tail)
}

# Estimate the upper and lower tail dependence coefficients
alpha <- 1.296  # Replace with your Plackett copula parameter estimate
lambda_U <- upper_tail_dependence(alpha)
lambda_L <- lower_tail_dependence(alpha)

# Print the results
cat("Upper Tail Dependence (lambda_U):", lambda_U, "\n")
cat("Lower Tail Dependence (lambda_L):", lambda_L, "\n")



