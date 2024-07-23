library(copula)

# Load the data from the Excel file
during_covid <- read_excel("C:/Users/KASHISH NIGAM/Desktop/Last Sem Project/Final Datasets/India/uniform_distributions-During_Covid_India.xlsx")

# Convert the data to a matrix (if not already in matrix form)
u_data <- as.matrix(during_covid)

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

# Initialize a list to store fit results and test results
fit_results <- list()
test_results <- list()

# Fit each copula and store the results
for (name in names(copulas)) {
  copula <- copulas[[name]]
  fit <- fitCopula(copula, data = u_data, method = "mpl")
  fit_results[[name]] <- fit
  
  # Perform goodness-of-fit testing
  test <- gofCopula(copula, u_data)
  test_results[[name]] <- test
}

# Print AIC and BIC values
aic_bic <- sapply(fit_results, function(fit) {
  c(AIC = AIC(fit), BIC = BIC(fit))
})
aic_bic_df <- as.data.frame(t(aic_bic))
print(aic_bic_df)

# Print goodness-of-fit test results
for (name in names(test_results)) {
  cat("Goodness-of-fit test results for", name, "\n")
  print(test_results[[name]])
  cat("\n")
}
