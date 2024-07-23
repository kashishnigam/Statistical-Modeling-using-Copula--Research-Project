# Load the necessary libraries
if(!require(readxl)) install.packages("readxl")
if(!require(copula)) install.packages("copula")

library(readxl)
library(copula)

# Define the file path
file_path <- "C:/Users/KASHISH NIGAM/Desktop/Last Sem Project/Final Datasets/India/OPEC_India/uniform_distributions-During_Covid_India.xlsx"

# Load the data from the Excel file
During_covid <- read_excel(file_path)

# Convert the data to a matrix (if not already in matrix form)
u_data <- as.matrix(During_covid)



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
pearson_rho <- cor(During_covid, method = "pearson")

# Compute Kendall's tau
kendall_tau <- cor(During_covid, method = "kendall")

# Print correlation matrices
print("Pearson's correlation coefficient:")
print(pearson_rho)

print("\nKendall's tau:")
print(kendall_tau)



