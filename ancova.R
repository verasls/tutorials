# In the line below, change the text between the quotation marks by the path to
# your working directory
setwd("your/working/directory")

# Install and load packages -----------------------------------------------

install.packages("tidyverse")
install.packages("car")
install.packages("emmeans")

library(tidyverse)
library(car)
library(emmeans)

# Read and prepare data ---------------------------------------------------

data("anxiety", package = "datarium")

# Select and rename variables
anxiety <- anxiety %>% 
  as_tibble() %>% 
  select(id, group, pre_test = t1, post_test = t3)

# Recode group factors
anxiety$group <- recode_factor(
  anxiety$group,
  "grp1" = "Control",
  "grp2" = "Moderate",
  "grp3" = "High"
)

# Reshape data
anxiety_long <- anxiety %>% 
  pivot_longer(
    c(pre_test, post_test),
    names_to = "time",
    values_to = "score"
  )

# Recode time into a factor
anxiety_long$time <- as_factor(anxiety_long$time)
anxiety_long$time <- recode_factor(
  anxiety_long$time, 
  "pre_test" = "Pre-test",
  "post_test" = "Post-test"
)

# Explore data ------------------------------------------------------------

# Boxplot
ggplot(data = anxiety_long, mapping = aes(x = group, y = score)) +
  geom_boxplot() +
  geom_dotplot(
    binaxis = "y",
    stackdir = "center",
    dotsize = 0.7,
    binwidth = 0.3,
    fill = "white"
  ) +
  facet_wrap(~time) +
  labs(x = "Group", y = "Anxiety score")

# Histogram
# Define function to select best bin width
bin_width <- function(variable) {
  bw <- 2 * IQR(variable) / length(variable)^(1/3)
  return(bw)
}
# Pre-test
ggplot(data = anxiety, mapping = aes(pre_test)) +
  geom_histogram(binwidth = bin_width(anxiety$pre_test)) +
  facet_wrap(~group) +
  labs(x = "Pre-test", y = "")
# Post-test
ggplot(data = anxiety, mapping = aes(post_test)) +
  geom_histogram(binwidth = bin_width(anxiety$post_test)) +
  scale_x_continuous(breaks = seq(11, 21, 2)) +
  facet_wrap(~group) +
  labs(x = "Post-test", y = "")

# Normality tests
# Separate the groups into 3 different data frames
anxiety_c <- filter(anxiety, group == "Control")
anxiety_m <- filter(anxiety, group == "Moderate")
anxiety_h <- filter(anxiety, group == "High")
# Run normality tests
shapiro.test(anxiety_c$pre_test)
shapiro.test(anxiety_m$pre_test)
shapiro.test(anxiety_h$pre_test)

shapiro.test(anxiety_c$post_test)
shapiro.test(anxiety_m$post_test)
shapiro.test(anxiety_h$post_test)

# Levene's test
leveneTest(anxiety$post_test, anxiety$group, center = median)

# Descriptives
descriptives <- anxiety_long %>% 
  group_by(group, time) %>% 
  summarise(
    n = n(),
    mean = mean(score),
    sd = sd(score)
  )
descriptives %>% as.data.frame()

# Check assumptions -------------------------------------------------------

# ** Independence of the covariate and treatment effect -------------------

aov(formula = pre_test ~ group, data = anxiety) %>% summary()

# ** Linearity between the covariate and the outcome variable -------------

# Build linear regression models using data from each group separately
lm(formula = post_test ~ pre_test, data = anxiety_c) %>% summary()

lm(formula = post_test ~ pre_test, data = anxiety_m) %>% summary()

lm(formula = post_test ~ pre_test, data = anxiety_h) %>% summary()

# Plot
ggplot(
  data = anxiety, 
  mapping = aes(x = pre_test, y = post_test, colour = group)
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  guides(color = guide_legend("group")) +
  labs(
    x = "Pre-test",
    y = "Post-test"
  )
  
# ** Homegeneity of regression slopes -------------------------------------

aov(formula = post_test ~ pre_test * group, data = anxiety) %>% summary()

# ANCOVA ------------------------------------------------------------------

# Run ANCOVA
ancova <- aov(formula = post_test ~ pre_test + group, data = anxiety)

# Get type III sum of squares
Anova(ancova, type = "III")

# Estimated marginal means
emmeans(ancova, ~ group)

# Get model coefficients
summary.lm(ancova)

# Set orthogonal contrasts and re run the model
contrasts(anxiety$group) <- cbind(c(2, -1, -1), c(0, 1, -1))

# Run ANCOVA
ancova_2 <- aov(formula = post_test ~ pre_test + group, data = anxiety)

# Get type III sum of squares
Anova(ancova_2, type = "III")

# Estimated marginal means
emmeans(ancova_2, ~ group)

# Get model coefficients
summary.lm(ancova_2)

# Post hoc tests ----------------------------------------------------------

pairs(emmeans(ancova_2, ~ group), adjust = "Bonferroni")
pairs(emmeans(ancova_2, ~ group), adjust = "Holm")
pairs(emmeans(ancova_2, ~ group), adjust = "Tukey")

# Model diagnostic plots --------------------------------------------------

plot(ancova_2, 1) # Homogeneity of variance
plot(ancova_2, 2) # Q-Q plot

# Plot post-test scores by group ------------------------------------------

# ** Without adjusting for baseline ---------------------------------------

# Build plot data frame
# Get descriptives and compute 95% confidence interval
no_adj_plot_df <- descriptives %>% 
  filter(time == "Post-test") %>% 
  mutate(
    lower_CI = mean - ((sd / sqrt(n)) * qt(0.975, df = n - 1)),
    upper_CI = mean + ((sd / sqrt(n)) * qt(0.975, df = n - 1))
  )

# Plot
ggplot(data = no_adj_plot_df) +
  geom_point(mapping = aes(x = group, y = mean)) +
  geom_errorbar(
    aes(x = group, ymin = lower_CI, ymax = upper_CI),
    width = 0.3
  )

# Run an ANOVA to confirm plot
aov(formula = post_test ~ group, data = anxiety) %>% summary()
pairwise.t.test(
  anxiety$post_test, anxiety$group, 
  p.adjust.method = "bonferroni"
)

# ** Adjusting for baseline -----------------------------------------------

# Build plot dataframe
# Put estimated marginal means for post-test into a data frame
emmeans <- emmeans(ancova_2, ~ group) %>% as.data.frame()

# Plot
ggplot(data = emmeans) +
  geom_point(mapping = aes(x = group, y = emmean)) +
  geom_errorbar(
    aes(x = group, ymin = lower.CL, ymax = upper.CL), 
    width = 0.3
  )

# Effect sizes ------------------------------------------------------------

# Partial eta squared
partial_eta_squared <- function(SS_effect, SS_residual) {
  es <- SS_effect / (SS_effect + SS_residual)
  return(es)
}
# Put the sum of squares is a data frame
SS <- as.data.frame(Anova(ancova_2, type = "III"))[1]
# For the treatment effect
partial_eta_squared(SS[3, 1], SS[4, 1])
# For the covariate
partial_eta_squared(SS[2, 1], SS[4, 1])

# Publication plot --------------------------------------------------------

emmeans$group <- recode_factor(
  emmeans$group,
  "Control" = "Control Group",
  "Moderate" = "Moderate Exercise Group",
  "High" = "High Exercise Group"
)

plot <- ggplot(data = emmeans) +
  geom_point(
    mapping = aes(x = group, y = emmean),
    size = 2.5
  ) +
  geom_errorbar(
    aes(x = group, ymin = lower.CL, ymax = upper.CL),
    width = 0.1, size = 1
  ) +
  scale_y_continuous(
    limits = c(13, 17),
    breaks = seq(13, 17, 1),
    expand = c(0, 0)
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  ylab("Anxiety Score")

plot

ggsave("plot.pdf", plot, width = 8, height = 6, dpi = 100)