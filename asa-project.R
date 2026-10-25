#  ASA PROJECT - Predictive Statistical Analysis of a Real-World
#                Data set Using R
#  Dataset : Concrete Compressive Strength (I-Cheng Yeh, 1998)
#            UCI Machine Learning Repository / Kaggle


#  CONTENTS
#   SETUP        Packages and working folder
#   OBJECTIVE 1  Clean and organize the data set
#   OBJECTIVE 2  Descriptive analysis and visualization
#   OBJECTIVE 3  Probability, correlation/regression, hypothesis tests
#   OBJECTIVE 4  Use of R and AI-assisted tools

#  All figures and tables are saved in an "outputs" folder inside the
#  project folder, ready to paste into the report.


#  SETUP
# Packages: installs one only if it is missing, then loads it
packages <- c("tidyverse")
to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)   # readr (import), dplyr (cleaning), tidyr, ggplot2 (graphs)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dir.create("outputs", showWarnings = FALSE)

# Colours used in the graphs
col_main <- "#2a78d6"                                      # main blue
col_dark <- "#0d366b"                                      # dark blue for lines
col_age  <- c("#9ec5f4", "#5598e7", "#256abf", "#0d366b")  # light -> dark = young -> old
col_sp   <- c("No" = "#8f8e88", "Yes" = "#2a78d6")         # grey = not used, blue = used



#  OBJECTIVE 1: CLEAN AND ORGANIZE THE DATASET

# 1.1 Import the raw data

raw_file <- "concrete_data.csv"
if (!file.exists(raw_file)) raw_file <- file.choose()

raw <- read_csv(raw_file, show_col_types = FALSE)

dim(raw)
glimpse(raw)
head(raw)


# 1.2 Rename columns to short, consistent names

# Every copy of this data set (UCI, Kaggle) has the same 9 columns in
# the same order, but the original names are long and differ between
# copies. So we rename by position and keep the units in a dictionary.
if (ncol(raw) != 9) {
  stop("Expected 9 columns but found ", ncol(raw),
       ". If the first column is a row number, remove it: raw <- raw[, -1]")
}

new_names <- c("cement", "slag", "fly_ash", "water", "superplasticizer",
               "coarse_agg", "fine_agg", "age_days", "strength_mpa")

rename_table <- tibble(original_name = names(raw), new_name = new_names)
print(rename_table)   # the first row is cement, the last is cement strength

names(raw) <- new_names

data_dictionary <- tibble(
  variable    = new_names,
  description = c("Cement", "Blast furnace slag", "Fly ash", "Water",
                  "Superplasticizer (admixture)", "Coarse aggregate",
                  "Fine aggregate", "Age of specimen at testing",
                  "Concrete compressive strength"),
  unit        = c(rep("kg/m3 of mix", 7), "days", "MPa"),
  role        = c(rep("Input - mix ingredient", 7),
                  "Input - curing time", "Output - target variable")
)
print(data_dictionary)

# Working copy of the data, and a log of how many rows remain after
# each cleaning step.
concrete <- raw
cleaning_log <- tibble(step = "1. Raw data imported", rows = nrow(concrete))


# 1.3 A table showing missing values, if any

missing_summary <- tibble(variable  = names(concrete),
                          n_missing = colSums(is.na(concrete)))
print(missing_summary)

concrete <- concrete %>% drop_na()
cleaning_log <- add_row(cleaning_log, step = "2. Rows with missing values removed",
                        rows = nrow(concrete))

# Zeros in slag, fly ash and superplasticizer are NOT missing data.
# They mean that ingredient was not used in that mix.


# 1.4 Duplicate rows

# Identical rows are the same mix and test result entered twice. Keeping
# them would count those mixes twice and make results look more certain
# than they are, so we keep only one copy of each.
n_duplicates <- sum(duplicated(concrete))
cat("Exact duplicate rows found:", n_duplicates, "\n")

concrete <- concrete %>% distinct()
cleaning_log <- add_row(cleaning_log, step = "3. Duplicate rows removed",
                        rows = nrow(concrete))


# 1.5 Outliers (IQR rule)

# A value is flagged if it lies below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.
outlier_summary <- concrete %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(Q1          = quantile(value, 0.25),
            Q3          = quantile(value, 0.75),
            IQR         = Q3 - Q1,
            lower_fence = Q1 - 1.5 * IQR,
            upper_fence = Q3 + 1.5 * IQR,
            n_outliers  = sum(value < lower_fence | value > upper_fence),
            pct_outliers = round(100 * n_outliers / n(), 1)) %>%
  mutate(variable = factor(variable, levels = new_names)) %>%
  arrange(variable)
print(outlier_summary, width = Inf)

# Boxplots of all variables (each with its own scale)
fig1 <- concrete %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = new_names)) %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(fill = "#9ecae1", outlier.colour = "#d62728", outlier.size = 1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(title = "Figure 1: Outlier check for all variables (IQR rule)",
       subtitle = "Red points lie beyond 1.5 x IQR from the box",
       x = NULL, y = "Value (kg/m3, days or MPa)") +
  theme_bw()
print(fig1)
ggsave("outputs/fig1_outlier_boxplots.png", fig1, width = 9, height = 7, dpi = 300)

# outliers are KEPT. They are real laboratory results within
# physically possible ranges (long curing ages of 90-365 days, high
# superplasticizer doses in high-performance mixes, and strengths of
# well-cured high-strength concrete). Deleting them would remove exactly
# the mixes that show how age and admixtures raise strength.


# 1.6 Create new variables

concrete <- concrete %>%
  mutate(
    # Water-cement ratio (Abrams' law: lower = stronger)
    wc_ratio = water / cement,
    
    # Curing age groups
    age_group = cut(age_days,
                    breaks = c(0, 7, 28, 90, 365),
                    labels = c("Early (<=7 d)", "Standard (8-28 d)",
                               "Medium (29-90 d)", "Long-term (>90 d)")),
    
    # Strength bands (based on individual test results, so they are
    # bands, not IS 456 grades)
    strength_class = cut(strength_mpa,
                         breaks = c(-Inf, 20, 40, Inf), right = FALSE,
                         labels = c("Low (<20 MPa)", "Medium (20-40 MPa)",
                                    "High (>=40 MPa)")),
    
    # Superplasticizer used or not (for the t-test and chi-square test
    # in Objective 3)
    sp_used = factor(if_else(superplasticizer > 0, "Yes", "No"), levels = c("No", "Yes"))
  )

# Check the new groups have enough samples
count(concrete, age_group)
count(concrete, strength_class)
count(concrete, sp_used)
summary(concrete$wc_ratio)


# 1.7 Cleaning summary and save

cleaning_log <- cleaning_log %>%
  mutate(rows_removed = lag(rows, default = first(rows)) - rows)
print(cleaning_log)

cat("\nRaw data   :", nrow(raw), "rows x", ncol(raw), "columns",
    "\nClean data :", nrow(concrete), "rows x", ncol(concrete), "columns\n")

write_csv(concrete,        "outputs/concrete_clean.csv")
saveRDS(concrete,          "outputs/concrete_clean.rds")  # R format, keeps factor order
write_csv(data_dictionary, "outputs/table_data_dictionary.csv")
write_csv(rename_table,    "outputs/table_column_renaming.csv")
write_csv(missing_summary, "outputs/table_missing_values.csv")
write_csv(outlier_summary %>% mutate(across(where(is.numeric), ~ round(.x, 2))),
          "outputs/table_outlier_summary.csv")
write_csv(cleaning_log,    "outputs/table_cleaning_log.csv")



#  OBJECTIVE 2: DESCRIPTIVE ANALYSIS AND VISUALIZATION

# Part A: summary tables - saved as outputs/table_*.csv
# Part B: graphs - saved as outputs/fig*.png

# 2.1 Load the cleaned data.

if (!file.exists("outputs/concrete_clean.rds")) {
  stop("Cleaned data not found. Run all of Objective 1 first ",
       "(its last step saves outputs/concrete_clean.rds).")
}
concrete <- readRDS("outputs/concrete_clean.rds")
names(concrete)   


#  PART A: SUMMARY TABLES

# 2.2 Summary statistics of numeric variables

# For each variable: centre (mean, median), spread (SD, CV, range)
# and shape (skewness).
num_vars <- c("cement", "slag", "fly_ash", "water", "superplasticizer",
              "coarse_agg", "fine_agg", "wc_ratio", "age_days", "strength_mpa")

desc_stats <- concrete %>%
  select(all_of(num_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "value") %>%
  mutate(Variable = factor(Variable, levels = num_vars)) %>%
  group_by(Variable) %>%
  summarise(N        = n(),
            Mean     = mean(value),
            SD       = sd(value),
            Min      = min(value),
            Q1       = quantile(value, 0.25),
            Median   = median(value),
            Q3       = quantile(value, 0.75),
            Max      = max(value),
            CV_pct   = 100 * SD / Mean,                     # spread relative to the mean
            Skewness = mean((value - Mean)^3) / SD^3) %>%   # 0 = symmetric, + = long right tail
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(desc_stats, width = Inf)
write_csv(desc_stats, "outputs/table_descriptive_stats.csv")

# Explanation -
# - Mean close to Median and Skewness near 0 -> roughly symmetric
#   (water, coarse and fine aggregate, strength).
# - Mean well above Median and Skewness above 1 -> a few large values pull
#   the mean up (age_days: most tests are at 28 days or less, a few at 365).
# - CV_pct shows which variables vary most from mix to mix: slag, fly ash
#   and superplasticizer vary a lot because many mixes do not use them.


# 2.3 Frequency tables of the categorical variables

freq_age      <- concrete %>% count(age_group)      %>% mutate(Percent = round(100 * n / sum(n), 1))
freq_strength <- concrete %>% count(strength_class) %>% mutate(Percent = round(100 * n / sum(n), 1))
freq_sp       <- concrete %>% count(sp_used)        %>% mutate(Percent = round(100 * n / sum(n), 1))

freq_age
freq_strength
freq_sp

# One combined table
freq_tables <- bind_rows(
  freq_age      %>% transmute(Variable = "Age group",             Category = as.character(age_group),      n, Percent),
  freq_strength %>% transmute(Variable = "Strength class",        Category = as.character(strength_class), n, Percent),
  freq_sp       %>% transmute(Variable = "Superplasticizer used", Category = as.character(sp_used),        n, Percent)
)
write_csv(freq_tables, "outputs/table_frequency_tables.csv")


# 2.4 Strength by group

# Average strength for each age group and with / without superplasticizer
strength_by_age <- concrete %>%
  group_by(age_group) %>%
  summarise(N = n(), Mean = mean(strength_mpa), SD = sd(strength_mpa),
            Median = median(strength_mpa), Min = min(strength_mpa),
            Max = max(strength_mpa)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
strength_by_age

strength_by_sp <- concrete %>%
  group_by(sp_used) %>%
  summarise(N = n(), Mean = mean(strength_mpa), SD = sd(strength_mpa),
            Median = median(strength_mpa), Min = min(strength_mpa),
            Max = max(strength_mpa)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))
strength_by_sp

write_csv(strength_by_age, "outputs/table_strength_by_age.csv")
write_csv(strength_by_sp,  "outputs/table_strength_by_superplasticizer.csv")


#  PART B: GRAPHS


# 2.5 Histogram: distribution of strength
# What it shows: strength ranges from about 2 to 83 MPa. The shape is
# roughly bell-shaped with a slightly longer right tail (mean a little
# above median)

mean_str   <- mean(concrete$strength_mpa)
median_str <- median(concrete$strength_mpa)

fig2 <- ggplot(concrete, aes(x = strength_mpa)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = col_main, colour = "white") +
  geom_vline(xintercept = mean_str,   linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = median_str, linetype = "dotted", linewidth = 0.7) +
  annotate("text", x = mean_str + 1, y = Inf, vjust = 1.5, hjust = 0, size = 3.5,
           label = paste0("Mean = ", round(mean_str, 1), " MPa")) +
  annotate("text", x = median_str - 1, y = Inf, vjust = 1.5, hjust = 1, size = 3.5,
           label = paste0("Median = ", round(median_str, 1), " MPa")) +
  labs(title = "Figure 2: Distribution of concrete compressive strength",
       x = "Compressive strength (MPa)", y = "Number of mixes") +
  theme_bw()
print(fig2)
ggsave("outputs/fig2_strength_histogram.png", fig2, width = 8, height = 5, dpi = 300)




# 2.6 Boxplot: strength by age group
# What it shows: strength rises with curing age, from about 22 MPa
# (up to 7 days) to about 35 MPa (8-28 days) and about 47-49 MPa after
# 28 days. Most of the gain happens in the first 28 days; after 90 days
# it levels off.

fig3 <- ggplot(concrete, aes(x = age_group, y = strength_mpa, fill = age_group)) +
  geom_boxplot(width = 0.6, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white") +                         # white diamond = mean
  scale_fill_manual(values = col_age) +
  labs(title = "Figure 3: Compressive strength by curing age",
       subtitle = "Box = middle 50% of mixes, line = median, diamond = mean",
       x = "Age group", y = "Compressive strength (MPa)") +
  theme_bw() +
  theme(legend.position = "none")
print(fig3)
ggsave("outputs/fig3_strength_by_age.png", fig3, width = 8, height = 5, dpi = 300)




# 2.7 Scatter plot: strength vs water-cement ratio
# What it shows: strength falls as the water-cement ratio rises (Abrams'
# law). The drop is steep up to a ratio of about 0.75 and flatter after
# that, so the relationship is curved, not a straight line.

fig4 <- ggplot(concrete, aes(x = wc_ratio, y = strength_mpa)) +
  geom_point(colour = col_main, alpha = 0.5, size = 1.8) +
  geom_smooth(method = "loess", formula = y ~ x, colour = col_dark,
              fill = "grey70", linewidth = 1) +
  labs(title = "Figure 4: Compressive strength vs water-cement ratio",
       subtitle = "Each point is one mix; the line shows the average trend",
       x = "Water-cement ratio", y = "Compressive strength (MPa)") +
  theme_bw()
print(fig4)
ggsave("outputs/fig4_strength_vs_wc_ratio.png", fig4, width = 8, height = 5, dpi = 300)




# 2.8 Boxplot: strength with and without superplasticizer
# What it shows: mixes with superplasticizer are stronger on average
# (about 39 MPa vs 30 MPa). Whether this difference is statistically
# significant is tested ahead.

fig5 <- ggplot(concrete, aes(x = sp_used, y = strength_mpa, fill = sp_used)) +
  geom_boxplot(width = 0.5, outlier.size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white") +
  scale_fill_manual(values = col_sp) +
  labs(title = "Figure 5: Compressive strength with and without superplasticizer",
       subtitle = "Diamond = mean",
       x = "Superplasticizer used", y = "Compressive strength (MPa)") +
  theme_bw() +
  theme(legend.position = "none")
print(fig5)
ggsave("outputs/fig5_strength_by_superplasticizer.png", fig5, width = 7, height = 5, dpi = 300)


# 2.9 Correlation heatmap

# Correlation (r) runs from -1 to +1:
#   +1 = both rise together, -1 = one rises as the other falls, 0 = no link
cor_matrix <- cor(concrete[, num_vars])
round(cor_matrix, 2)

# Correlation of each variable with strength, strongest first
sort(round(cor_matrix[, "strength_mpa"], 2), decreasing = TRUE)

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(var1 = Var1, var2 = Var2, r = Freq)

fig6 <- ggplot(cor_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = "#e34948", mid = "#f0efec", high = col_main,
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(title = "Figure 6: Correlation matrix of all numeric variables",
       subtitle = "Blue = positive, red = negative, pale = weak",
       x = NULL, y = NULL) +
  scale_y_discrete(limits = rev(num_vars)) +   # first variable at the top
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(fig6)
ggsave("outputs/fig6_correlation_heatmap.png", fig6, width = 8, height = 7, dpi = 300)

# What it shows:
# - Strength is most linked to cement (r = +0.49) and water-cement ratio
#   (r = -0.49), then superplasticizer and age (about +0.34 each).
# - Water and superplasticizer are strongly negative (r = -0.65):
#   superplasticizer lets a mix use less water.
# - Cement and water-cement ratio are very strongly related (r = -0.88)
#   because one is calculated from the other.



#  OBJECTIVE 3: PROBABILITY, CORRELATION/REGRESSION AND HYPOTHESIS TESTING

# Part A: probability
# Part B: correlation and regression
# Part C: hypothesis tests
# Every test uses a 5% significance level:
#   p-value < 0.05  -> reject H0 (the result is statistically significant)
#   p-value >= 0.05 -> do not reject H0


# 3.1 Load the cleaned data

if (!file.exists("outputs/concrete_clean.rds")) {
  stop("Cleaned data not found. Run all of Objective 1 first ",
       "(its last step saves outputs/concrete_clean.rds).")
}
concrete <- readRDS("outputs/concrete_clean.rds")


#  PART A: PROBABILITY

# 3.2 Normal distribution of 28-day strength

# 28 days is the standard age for testing concrete, so we use only the
# 28-day results and treat their strength as a normal distribution.
strength_28 <- concrete$strength_mpa[concrete$age_days == 28]

n_28    <- length(strength_28)
mean_28 <- mean(strength_28)
sd_28   <- sd(strength_28)
cat("28-day results:", n_28, " Mean:", round(mean_28, 2), "MPa  SD:", round(sd_28, 2), "MPa\n")

# Probability that a 28-day result is at least 25 MPa (example target)
p_normal <- 1 - pnorm(25, mean = mean_28, sd = sd_28)   # from the normal model
p_actual <- mean(strength_28 >= 25)                     # actual share in the data
cat("P(strength >= 25 MPa): normal model =", round(p_normal, 3),
    " actual data =", round(p_actual, 3), "\n")

# Characteristic strength (IS 456 approach): the value below which only
# 5% of results are expected to fall = mean - 1.65 x SD
fck <- mean_28 - 1.65 * sd_28
cat("Characteristic strength =", round(fck, 2), "MPa\n")

fig7 <- ggplot(data.frame(strength = strength_28), aes(x = strength)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, boundary = 0,
                 fill = "#9ec5f4", colour = "white") +
  stat_function(fun = dnorm, args = list(mean = mean_28, sd = sd_28),
                colour = col_dark, linewidth = 1) +
  geom_vline(xintercept = fck, linetype = "dashed") +
  annotate("text", x = fck + 1, y = Inf, vjust = 1.5, hjust = 0, size = 3.5,
           label = paste0("Characteristic strength = ", round(fck, 1), " MPa")) +
  labs(title = "Figure 7: 28-day strength with fitted normal curve",
       subtitle = paste0("n = ", n_28, ", mean = ", round(mean_28, 1),
                         " MPa, SD = ", round(sd_28, 1), " MPa"),
       x = "Compressive strength at 28 days (MPa)", y = "Density") +
  theme_bw()
print(fig7)
ggsave("outputs/fig7_normal_28day.png", fig7, width = 8, height = 5, dpi = 300)

# What it shows: the normal curve fits the 28-day results well. It
# gives P(strength >= 25 MPa) = 0.786, and the actual share in the data is
# 0.788. The characteristic strength is about 12.7 MPa. It is low because
# the data combines many different mix designs, so the SD (about 14 MPa)
# is much larger than it would be for one mix on a real site.


# 3.3 Conditional probability: strength class given superplasticizer use

sp_strength_table <- table(concrete$sp_used, concrete$strength_class)
sp_strength_table

# Each row adds up to 1, e.g. P(High / superplasticizer used)
sp_strength_prob <- round(prop.table(sp_strength_table, margin = 1), 3)
sp_strength_prob
write.csv(sp_strength_prob, "outputs/table_conditional_probability.csv")

# What it shows: 43% of mixes with superplasticizer reach 40 MPa
# or more, compared with 24% of mixes without it. Only 13% of mixes with
# superplasticizer are below 20 MPa, compared with 31% without it.


#  PART B: CORRELATION AND REGRESSION

# 3.4 Correlation test: strength vs water-cement ratio

# H0: there is no correlation between w/c ratio and strength (r = 0)
# H1: there is a correlation (r is not 0)
cor_test <- cor.test(concrete$wc_ratio, concrete$strength_mpa)
cor_test

# What it shows: r = -0.49 and p < 0.001, so we reject H0. There is a
# significant, moderate negative correlation: the higher the w/c ratio,
# the lower the strength.


# 3.5 Simple linear regression: strength = a + b x (w/c ratio)

model_simple <- lm(strength_mpa ~ wc_ratio, data = concrete)
summary(model_simple)

# What it shows: strength = 54.5 - 25.4 x (w/c ratio). Each 0.1
# increase in w/c ratio lowers strength by about 2.5 MPa (p < 0.001).
# R-squared = 0.24, so w/c ratio alone explains about 24% of the
# variation in strength; other factors, especially age, also matter.


# 3.6 Multiple linear regression to predict strength

# Inputs: all 7 ingredients + log(age). Age is used as log(age) because
# strength rises fast in the first days and then levels off (Figure 3).
# w/c ratio is left out because it is calculated from cement and water,
# which are already in the model.

# Split the data: 80% to build (train) the model, 20% to test it
set.seed(123)   # makes the random split the same every time
train_rows <- sample(nrow(concrete), size = round(0.8 * nrow(concrete)))
train <- concrete[train_rows, ]
test  <- concrete[-train_rows, ]

model_multi <- lm(strength_mpa ~ cement + slag + fly_ash + water + superplasticizer +
                    coarse_agg + fine_agg + log(age_days), data = train)
summary(model_multi)

# Each coefficient = change in strength (MPa) for 1 kg/m3 more of that
# ingredient, keeping the others the same. Pr(>|t|) < 0.05 = significant.

# Predict strength for the 20% test mixes and check accuracy
test$predicted <- predict(model_multi, newdata = test)

rmse    <- sqrt(mean((test$strength_mpa - test$predicted)^2))
r2_test <- 1 - sum((test$strength_mpa - test$predicted)^2) /
  sum((test$strength_mpa - mean(test$strength_mpa))^2)
cat("Test set: R-squared =", round(r2_test, 3), "  RMSE =", round(rmse, 2), "MPa\n")

fig8 <- ggplot(test, aes(x = strength_mpa, y = predicted)) +
  geom_point(colour = col_main, alpha = 0.6, size = 1.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Figure 8: Predicted vs actual strength (test mixes)",
       subtitle = paste0("Dashed line = perfect prediction.  R-squared = ",
                         round(r2_test, 2), ", RMSE = ", round(rmse, 1), " MPa"),
       x = "Actual strength (MPa)", y = "Predicted strength (MPa)") +
  coord_equal() +
  theme_bw()
print(fig8)
ggsave("outputs/fig8_predicted_vs_actual.png", fig8, width = 6, height = 6, dpi = 300)

# What it shows: the model explains about 81% of the variation in
# strength (adjusted R-squared = 0.81) and all 8 inputs are significant
# (p < 0.05). Strength rises with cement, slag, fly ash, superplasticizer
# and age, and falls with water. On the 20% test mixes, which the model
# never saw, R-squared = 0.80 and the typical prediction error (RMSE) is
# about 7.8 MPa, so the model predicts strength well.


#  PART C: HYPOTHESIS TESTS

# 3.7 t-test: does superplasticizer change mean strength?

# H0: mean strength is the same with and without superplasticizer
# H1: mean strength is different
t_test <- t.test(strength_mpa ~ sp_used, data = concrete)
t_test
# What it shows: mean strength is 38.7 MPa with superplasticizer and
# 29.5 MPa without it (t = -9.18, p < 0.001), so we reject H0. Mixes with
# superplasticizer are significantly stronger, by about 7 to 11 MPa
# (95% confidence interval).


# 3.8 One-way ANOVA: does mean strength differ between age groups?

# H0: all 4 age groups have the same mean strength
# H1: at least one age group has a different mean strength
anova_model <- aov(strength_mpa ~ age_group, data = concrete)
anova_table <- summary(anova_model)[[1]]
anova_table
# What it shows: F = 168.3 and p < 0.001, so we reject H0. Mean
# strength differs significantly between age groups, which confirms
# Figure 3: concrete keeps gaining strength as it cures.


# 3.9 Chi-square test: is strength class linked to superplasticizer use?

# H0: strength class and superplasticizer use are independent (not linked)
# H1: they are linked
chi_test <- chisq.test(sp_strength_table)   # table from step 3.3
chi_test

# What it shows: Chi-square = 64.5 (df = 2) and p < 0.001, so we reject
# H0. Strength class is linked to superplasticizer use: mixes with
# superplasticizer are more often in the High class (see step 3.3).


# 3.10 Summary of all tests

test_results <- tibble(
  Test      = c("Correlation: strength vs w/c ratio",
                "t-test: strength with vs without superplasticizer",
                "ANOVA: strength across age groups",
                "Chi-square: strength class vs superplasticizer use"),
  Statistic = c(paste("r =", round(cor_test$estimate, 3)),
                paste("t =", round(t_test$statistic, 2)),
                paste("F =", round(anova_table$`F value`[1], 2)),
                paste("Chi-sq =", round(chi_test$statistic, 2))),
  p_value   = c(cor_test$p.value, t_test$p.value,
                anova_table$`Pr(>F)`[1], chi_test$p.value)
) %>%
  mutate(Decision = if_else(p_value < 0.05, "Reject H0 (significant)", "Do not reject H0"),
         p_value  = format.pval(p_value, digits = 3, eps = 0.001))
print(test_results, width = Inf)
write_csv(test_results, "outputs/table_hypothesis_tests.csv")



#  OBJECTIVE 4: USE OF R AND AI-ASSISTED TOOLS

# 4.1 R software and functions used

R.version.string
packageVersion("tidyverse")

r_functions <- tibble(
  Objective = c("1. Cleaning", "2. Descriptive analysis", "3. Probability",
                "3. Correlation and regression", "3. Hypothesis tests"),
  R_functions = c("read_csv, drop_na, duplicated, distinct, quantile, mutate, cut",
                  "summarise, group_by, count, ggplot, cor",
                  "pnorm, table, prop.table",
                  "cor.test, lm, predict",
                  "t.test, aov, chisq.test")
)
print(r_functions, width = Inf)
write_csv(r_functions, "outputs/table_r_functions.csv")


# 4.2 Log of AI-assisted tools

# Edit this table so it matches exactly how you used AI in your project.
ai_log <- tibble(
  Stage = c("Planning", "Objective 1", "Objective 2", "Debugging", "Objective 3"),
  AI_tool = "Claude (Anthropic)",
  What_AI_was_asked = c(
    "Suggest a construction dataset and an analysis plan for the 4 objectives",
    "Write R code to clean the data (rename, missing values, duplicates, outliers, new variables)",
    "Write R code for summary tables and graphs",
    "Explain the error 'wc_ratio doesn't exist' when running Objective 2 alone",
    "Write R code for probability, regression and hypothesis tests, and help interpret the output"),
  How_it_was_checked = c(
    "Matched the plan to the project objectives; downloaded the dataset from Kaggle",
    "Ran it in RStudio; confirmed 1,030 rows became 1,005 after removing 25 duplicates; removed optional steps",
    "Checked each graph against the summary tables (e.g. mean strength about 35 MPa)",
    "Applied the fix (load the saved cleaned data) and re-ran Objective 2 successfully",
    "Checked every p-value and coefficient in the R output; wrote the final interpretation in own words")
)
print(ai_log, width = Inf)
write_csv(ai_log, "outputs/table_ai_usage_log.csv")

# AI output was treated as a draft: all code was run and all numbers were
# checked in R before being used in the report.
