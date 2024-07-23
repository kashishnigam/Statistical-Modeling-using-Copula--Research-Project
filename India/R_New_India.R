if (!require(readxl)) install.packages("readxl")
if (!require(copula)) install.packages("copula")

library(readxl)
library(copula)

# Define the file path
file_path <- "C:/Users/KASHISH NIGAM/Desktop/Last Sem Project/Final Datasets/India/uniform_distributions-Post_Covid_India.xlsx"

# Load the data from the Excel file
post_covid <- read_excel(file_path)

# Convert the data to a matrix (if not already in matrix form)
u_data <- as.matrix(post_covid)

# Define copulas
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

# Extract LLF, AIC and BIC for each fitted copula
metrics <- sapply(fit_results, function(fit) {
  c(LLF = logLik(fit), AIC = AIC(fit), BIC = BIC(fit))
})

# Convert the results to a data frame for easier viewing
metrics_df <- as.data.frame(t(metrics))

# Print LLF, AIC and BIC values
print(metrics_df)

# Determine the best copula based on LLF and AIC
best_copula_name <- rownames(metrics_df)[which.min(metrics_df$AIC)]
best_copula_fit <- fit_results[[best_copula_name]]

# Print the best copula
cat("Best copula based on AIC:", best_copula_name, "\n")
print(summary(best_copula_fit))
