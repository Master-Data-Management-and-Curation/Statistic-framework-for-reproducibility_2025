# R script: Beta-Binomial posterior and VE posterior (Pfizer-style)
# Requires: ggplot2
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# ---- INPUT: replace these with your observed counts ----
# Example (illustrative only): vaccine group 8 cases, placebo 162 cases
Y_v <- 8      # infected in vaccine group
n_v <- 18198  # total in vaccine group

Y_p <- 162    # infected in placebo group
n_p <- 18211  # total in placebo group
# -------------------------------------------------------

# ---- Prior (Beta(alpha,beta)) ----
alpha0 <- 1
beta0  <- 1

# Posterior parameters
alpha_v_post <- alpha0 + Y_v
beta_v_post  <- beta0  + n_v - Y_v

alpha_p_post <- alpha0 + Y_p
beta_p_post  <- beta0  + n_p - Y_p

# Print posterior params
cat("Vaccine posterior: Beta(", alpha_v_post, ",", beta_v_post, ")\n")
cat("Placebo posterior: Beta(", alpha_p_post, ",", beta_p_post, ")\n\n")

# ---- Analytical posterior means and credible intervals for pi ----
mean_pi_v <- alpha_v_post / (alpha_v_post + beta_v_post)
mean_pi_p <- alpha_p_post / (alpha_p_post + beta_p_post)
cat(sprintf("Posterior mean pi_v = %.6f\n", mean_pi_v))
cat(sprintf("Posterior mean pi_p = %.6f\n\n", mean_pi_p))

# ---- Draw posterior samples and compute VE samples ----
S <- 200000  # number of posterior samples
set.seed(12345)
pi_v_s <- rbeta(S, alpha_v_post, beta_v_post)
pi_p_s <- rbeta(S, alpha_p_post, beta_p_post)

# Avoid division by zero / extreme small pi_p: filter or stable computation
# compute VE = 1 - pi_v/pi_p
ve_s <- 1 - (pi_v_s / pi_p_s)

# Trim extreme values (optional): VE can be < -Inf if pi_p_s ~ 0, but with real data it's fine.
# We'll compute credible interval using quantiles (and median)
ve_mean <- mean(ve_s)
ve_med  <- median(ve_s)
ve_ci   <- quantile(ve_s, c(0.025, 0.975))

cat(sprintf("VE posterior mean = %.3f\n", ve_mean))
cat(sprintf("VE posterior median = %.3f\n", ve_med))
cat(sprintf("VE 95%% CrI = [%.3f, %.3f]\n\n", ve_ci[1], ve_ci[2]))

# ---- Plot 1: posterior Beta densities for pi_v and pi_p ----
# create grid for analytic densities
x <- seq(0, max(c(pi_v_s, pi_p_s))*1.3, length.out = 1000)
dens_v <- dbeta(x, alpha_v_post, beta_v_post)
dens_p <- dbeta(x, alpha_p_post, beta_p_post)
df <- data.frame(x = rep(x, 2),
                 density = c(dens_v, dens_p),
                 group = rep(c("Vaccine", "Placebo"), each = length(x)))

p1 <- ggplot(df, aes(x = x, y = density, color = group, linetype = group)) +
  geom_line(size = 1) +
  labs(x = expression(pi),
       y = "Density",
       title = "Posterior Beta densities: Vaccine vs Placebo",
       subtitle = paste0("Priors Beta(",alpha0,",",beta0,")  |  Data: Y_v=", Y_v,", n_v=", n_v,
                         "  |  Y_p=", Y_p,", n_p=", n_p)) +
  theme_minimal() +
  theme(legend.position = "top")
print(p1)

# ---- Plot 2: posterior distribution of VE ----
df_ve <- data.frame(VE = ve_s)
p2 <- ggplot(df_ve, aes(x = VE)) +
  geom_histogram(bins = 120, aes(y = ..density..), fill = "grey90", color = "black") +
  geom_vline(xintercept = ve_mean, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = ve_ci, color = "red", linetype = "dotted", size = 1) +
  labs(x = "Vaccine Efficacy (VE = 1 - pi_v/pi_p)",
       y = "Density",
       title = "Posterior distribution of VE",
       subtitle = sprintf("Mean = %.3f  |  95%% CrI = [%.3f, %.3f]", ve_mean, ve_ci[1], ve_ci[2])) +
  theme_minimal()
print(p2)

# ---- Optionally save plots to files ----
ggsave("posterior_beta_pi.png", p1, width = 7, height = 4, dpi = 300)
ggsave("posterior_ve.png", p2, width = 7, height = 4, dpi = 300)

# ---- End ----
