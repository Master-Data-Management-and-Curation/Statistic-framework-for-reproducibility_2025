# ------------------------------
# p-hacking / garden of forking paths demo
# ------------------------------

set.seed(124)

# 1. Generate fake data
n <- 100
df <- data.frame(
  y  = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  x5 = rnorm(n),
  x6 = rnorm(n),
  x7 = rnorm(n),
  x8 = rnorm(n),
  x9 = rnorm(n),
  x10 = rnorm(n)
)

# 2. Try many different models (forking paths)
results <- data.frame(
  model = character(),
  p_value = numeric()
)

y <- df$y

for (var in names(df)[-1]) {
  fit <- summary(lm(y ~ df[[var]]))
  pval <- fit$coefficients[2,4]
  results <- rbind(results, data.frame(model = var, p_value = pval))
}

# 3. Which ones look "significant"?
results <- results[order(results$p_value), ]
print(results)

# 4. Visualize
library(ggplot2)
ggplot(results, aes(x = reorder(model, p_value), y = p_value)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "P-hacking: The Garden of Forking Paths",
    x = "Model (predictor variable)",
    y = "p-value"
  ) +
  theme_minimal()

# 5. Optional: highlight "success"
signif_models <- subset(results, p_value < 0.05)
if (nrow(signif_models) > 0) {
  cat("\nAha! We found something 'significant':", signif_models$model[1],
      "p =", round(signif_models$p_value[1], 3), "\n")
  cat("...except it’s completely random.\n")
} else {
  cat("\nNo significant results this time — unlucky p-hacker!\n")
}
