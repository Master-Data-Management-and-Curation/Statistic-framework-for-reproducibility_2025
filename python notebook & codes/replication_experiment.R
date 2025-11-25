# Librerie
library(tidyverse)
library(broom)
library(rstanarm)   # per analisi bayesiana simile a PyMC
library(bayesplot)  # per plotting posterior
library(ggplot2)

set.seed(1234)

# --- Preregistration (simulated) ---
prereg <- "
Preregistration (simulated):

Hypothesis:
  H1: Treatment increases average score compared to control.
  H0: No difference.

Primary outcome:
  Mean difference in continuous outcome Y.

Analysis plan:
  - Frequentist t-test
  - Linear regression Y ~ group
  - Bayesian posterior and 95% credible interval
  - Replication success defined as:
        sign(original effect) == sign(replication effect)
        AND
        credible intervals overlap substantially.

Exclusion criteria: none planned.
Sample sizes:
  Original: 60 participants
  Replication: 100 participants
"
cat(prereg, "\n\n")

# 1. Generate original experiment
n_original <- 60
true_effect <- 0.35

group_original <- rbinom(n_original, 1, 0.5)
y_original <- 2.0 + true_effect * group_original + rnorm(n_original, 0, 1)

df_original <- tibble(
  group = group_original,
  y = y_original
)

head(df_original)

# 2. Analyze original study (frequentist)
model_orig <- lm(y ~ group, data = df_original)
summary(model_orig)
orig_effect <- coef(model_orig)["group"]

# 3. Bayesian analysis of original
model_bayes_orig <- stan_glm(
  y ~ group,
  data = df_original,
  prior = normal(0, 1, autoscale = TRUE),
  prior_intercept = normal(0, 5),
  chains = 4, iter = 4000, seed = 1234
)

posterior <- as.matrix(model_bayes_orig)
mcmc_areas(posterior, pars = "group")
mcmc_intervals(posterior, pars = "group")


# 4. Generate replication study
n_replication <- 100
group_rep <- rbinom(n_replication, 1, 0.5)
y_rep <- 2.0 + 0.35 * group_rep + rnorm(n_replication, 0, 1)

df_rep <- tibble(group = group_rep, y = y_rep)

# 5. Analyze replication (frequentist)
model_rep <- lm(y ~ group, data = df_rep)
summary(model_rep)
rep_effect <- coef(model_rep)["group"]

# 6. Bayesian hierarchical synthesis
df_combined <- bind_rows(
  df_original %>% mutate(study = "original"),
  df_rep %>% mutate(study = "replication")
)

model_hier <- stan_glmer(
  y ~ group + (1|study),
  data = df_combined,
  prior = normal(0, 1),
  prior_intercept = normal(0, 5),
  chains = 4, iter = 4000, seed = 1234
)

posterior_hier <- as.matrix(model_hier)
mcmc_intervals(posterior_hier, pars = "group")

# 7. Replication Success Criteria
crit1 <- sign(orig_effect) == sign(rep_effect)
crit2 <- mean(posterior[, "group"]) * mean(posterior_hier[, "group"]) > 0
crit3 <- abs(orig_effect - rep_effect) < 0.25  # arbitrary tolerance

cat("Criteria met? ", crit1 & crit2 & crit3, "\n")
                       