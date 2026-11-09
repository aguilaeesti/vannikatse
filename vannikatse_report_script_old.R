#..............................................................................
##### _________________ Vannikatse DISTRIBUTIONS  _______________________ #####
#..............................................................................

getwd()

# Histograms/bar-charts of all the variables
# Repetitive, but good for getting to know the data and catching outliers 

library(tidyverse)
library(ggplot2)
library(patchwork)

df1 <- read_csv2("data_vannikatse.csv")
# change
# look into seeds
df_26 <- df1 %>%
  filter(year=="2026")

sum(df_26$nr_seeds, na.rm=T) # 10 013

# data cleaning
df1$flowerunequalgenh <- xor(is.na(df1$flower_len), is.na(df1$genh))
df1[df1$flowerunequalgenh == TRUE, ]


df1$h1unequalvegh <- xor(is.na(df1$h1), is.na(df1$vegh))
df1[df1$h1unequalvegh == TRUE, ]


df1$leaf1 <- xor(is.na(df1$h1),is.na(df$w1)) | xor(is.na(df1$h1),is.na(df$l1))
print(head(df1[df1$leaf1 == TRUE, ],20)) # 2025 C7c7, C15a8, C17c9, C29a7, C29b12; 2026 A11b9

df1$leaf2 <- xor(is.na(df1$h2),is.na(df$w2)) | xor(is.na(df1$h2),is.na(df$l2))
print(head(df1[df1$leaf2 == TRUE, ],20)) # 2026 A11b9

df1$leaf3 <- xor(is.na(df1$h3),is.na(df$w3)) | xor(is.na(df1$h3),is.na(df$l3))
print(head(df1[df1$leaf3 == TRUE, ],20)) # 2025 A4 (näritud, that's why it's missing)

df1$leaf4 <- xor(is.na(df1$h4),is.na(df$w4)) | xor(is.na(df1$h4),is.na(df$l4))
print(head(df1[df1$leaf4 == TRUE, ],20)) # NULL

df1$leaf5 <- xor(is.na(df1$h5),is.na(df$w5)) | xor(is.na(df1$h5),is.na(df$l5))
print(head(df1[df1$leaf5 == TRUE, ],20)) # NULL

df1$flower <- xor(is.na(df1$flower_nr), is.na(df1$genh))
df1[df1$flower == TRUE, ] # 2024 B30 missing Genh


summary(df1$w5) # widths look all fine, between 1 and 29mm (extremely thick 3rd leaf!)
summary(df1$l1)
ifelse(df1$h2<100,df1$ID,NA) # fixed A11b9 - l2 was actually w1.


# Continuous variables 

p1 <- ggplot(df1, aes(x = vegh)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Vegetative height [mm]", y = "Sagedus")

p2 <- ggplot(df1, aes(x = genh)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Generative height [mm]", y = "Sagedus")

p3 <- ggplot(df1, aes(x = h1)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Height of 1st leaf [mm]", y = "Sagedus")

p4 <- ggplot(df1, aes(x = l1)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Length of 1st leaf [mm]", y = "Sagedus")

p5 <- ggplot(df1, aes(x = w1)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 1, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Width/breadth of 1st leaf [mm]", y = "Sagedus")

p6 <- ggplot(df1, aes(x = flower_len)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Flower length [mm]", y = "Sagedus")

p7 <- ggplot(df1, aes(x = `Teise õisiku pikkus`)) +
  geom_histogram(fill = "lightskyblue2", binwidth = 10, na.rm = TRUE) +
  theme_bw() +
  labs(x = "Second flower length [mm]", y = "Sagedus")


# Discrete (count) variables 

p8 <- ggplot(df1, aes(x = flower_nr)) +
  geom_bar(fill = "lightskyblue2", na.rm = TRUE) +
  theme_bw() +
  labs(x = "Number of flowers", y = "Sagedus")

p9 <- ggplot(df1, aes(x = `sidebulb_nr`)) +
  geom_bar(fill = "lightskyblue2", na.rm = TRUE) +
  theme_bw() +
  labs(x = "Number of new sidebulbs", y = "Sagedus")

p10 <- ggplot(df1, aes(x = `Õite arv`)) +
  geom_bar(fill = "lightskyblue2", na.rm = TRUE) +
  theme_bw() +
  labs(x = "Number of flowers in the second inflorescence", y = "Sagedus")


# Displaying them on one plot for the data overview
# Since there were only 3 recordings of data in p7 and p10, I left them out

(p1|p2|p3) / (p4|p5|p6) / (p8|p9) 


#..............................................................................
##### _________________ Vannikatse TRAY MAPS  _______________________ #####
#..............................................................................


library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)

df_raw <- read_csv2("data_vannikatse.csv")

# load the data
df <- df_raw %>%
  mutate(
    tray   = str_extract(ID, "^[A-Za-z]"),   # extract tray column from the ID column 
    ID = str_extract(ID, "(?<=^[A-Za-z]).+") # extract the ID (numbers 1-30 in the first year, letters a-b... for side bulbs belonging to the motherplant)
  ) %>%
  mutate(year=as.factor(year),
         ID=as.factor(ID))


##### TRAY-map for PRESENCE AND ABSCENCE ####

# make a presence/absence column that I'll call "pa"
df$pa <- ifelse(is.na(df$h1),0,1) # 0 means, that plant is absence; 1, that it is present

# 3 maps using ggplot and facet_grid(year~tray)
ggplot(df,aes(x = x, y = y, colour = factor(pa)))+
  geom_point()+
  geom_text(aes(label=as.character(ID)),hjust=-0.35, vjust=0.35)+
  xlim(0,500)+ ylim(0,700)+
  facet_grid(year~tray)+
  theme_bw()

##### TRAY-map for LIFE-STAGES ####


# Extract combined tray name and build complete multi-year state matrix
df_spatial_combined <- df_raw %>%
  mutate(
    year_num = as.numeric(year),
    raw_tray = str_extract(ID, "^[A-Za-z]+"),
    combined_tray = str_remove(raw_tray, "X$"), # Merges AX -> A, BX -> B, CX -> C
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh) | (!is.na(flower_nr) & flower_nr > 0))
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "Flowering",
      leaf_count >= 1 ~ "Vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "Dormant",
      leaf_count == 0 & leaves_after ~ "Dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "Dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "Unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "Dead"),
    state = if_else(dead_from_here, "Dead", state)
  ) %>%
  # Fill coordinates across missing years so spatial positions remain visible
  fill(x, y, .direction = "downup") %>%
  ungroup() %>%
  filter(!is.na(x), !is.na(y)) %>%
  mutate(
    state = factor(state, levels = c("Vegetative", "Flowering", "Dormant", "Dead", "Unknown")),
    combined_tray  = paste("Combined Tray", combined_tray)
  )

# Multi-year spatial plot across combined trays
ggplot(df_spatial_combined, aes(x = x, y = y, color = state, shape = state)) +
  geom_point(aes(size = state), alpha = 0.85) +
  scale_color_manual(values = c(
    "Vegetative" = "forestgreen",
    "Flowering"  = "magenta3",
    "Dormant"    = "orange2",
    "Dead"       = "red4",
    "Unknown"    = "black"
  )) +
  scale_shape_manual(values = c(
    "Vegetative" = 16,  
    "Flowering"  = 8,   
    "Dormant"    = 18,  
    "Dead"       = 15,
    "Unknown"    = 17
  )) +
  scale_size_manual(values = c(
    "Vegetative" = 2.2,
    "Flowering"  = 3.0,
    "Dormant"    = 2.5,
    "Dead"       = 2.5,
    "Unknown"    = 2.5
  )) +
  facet_grid(year ~ combined_tray, scales = "free") +
  labs(
    title = "Spatial Tray Maps (2022–2026)",
    color = "Plant Status",
    shape = "Plant Status",
    size  = "Plant Status"
  ) +
  theme_bw() +
  theme(
    plot.title     = element_text(face = "bold", size = 14),
    strip.text     = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )



##### TRAY-map for SIDEBULB PRODUCTION ####

# 0 means no sidebulbs, 1 means at least one sidebulb produced
df$sidebulb_pa <- ifelse(is.na(df$`sidebulb_nr`) | df$`sidebulb_nr` == 0, 0, 1)

ggplot(df, aes(x = x, y=y, colour = factor(sidebulb_pa))) +
  geom_point() + 
  geom_text(aes(label = ID), hjust = -0.35, vjust = 0.35) +
  xlim(0, 500) + ylim(0, 700) +
  facet_grid(year~tray) +
  scale_color_manual(values = c("grey60", "darkorange"), 
                     labels = c("No sidebulbs", "Produced sidebulbs"),
                     name = "")  +
  theme_bw() +
  theme(
    plot.title     = element_text(face = "bold", size = 14),
    strip.text     = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )




#..............................................................................
##### _________________ Vannikatse TRAY DIFFERENCES  _______________________ #####
#..............................................................................

##### MODELS OF DIFFERENCES BETWEEN TRAYS ####

library(tidyverse)
library(lme4)
library(lmerTest) 
library(dplyr)
library(ggplot2)
library(stringr)

# Load and clean data
df2_states <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh) | (!is.na(flower_nr) & flower_nr > 0)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()


# Let's create a binary dormancy variable
# We'll keep the year that a certain plant was first declared dead
# but not the following years
df2 <-  df2_states %>%
  mutate(
    is_dormant = ifelse(state == "dormant", 1, 0)
  ) %>%
  group_by(ID)%>%
  mutate(
    death_yr = if(any(state == "dead")) {
      min(year_num[state == "dead"]) }
    else {
      Inf
    }
  ) %>%
  ungroup() %>%
  filter(year_num <= death_yr)


# ......................................................................
# TRAY COMPARISONS (All Years vs. 2022-2024) ------------------------
# ......................................................................

# Filter for the first three years (2022, 2023, 2024)
df_first3 <- df2 %>% filter(year_num %in% c(2022, 2023, 2024))

### A. Flowering Comparison ---------

# Do the trays differ in flowering throughout the years?

# Flowering is a binary variable; we have repeated measurings; we need to account for the ID of plants
# -> Let's try binomial GLMM (generalized linear mixed model) with ID as a random intercept

# All years, excluding year 2022, which we had to exclude, 
# Because there were no flowering plants this year, which does not work well in the GLMM
# (Which is actually quite interesting biologically, because regardless of the beginning state, none of the plants flowered in the 1st year)

# So maybe the more appropriate question or hypothesis could be:
# Did the plants in different trays differ in flowering after initial planting?


df_flowering <- df2 %>% filter(year != "2022")
df_first3_clean <- df_first3 %>% filter(year != "2022")


glmm_flower_all <- glmer(flowering ~ tray + year + (1|ID), 
  data = df_flowering, family = binomial)
summary(glmm_flower_all)



# Binomial GLMM assumptions:
library(DHARMa)
res_flower <- simulateResiduals(glmm_flower_all)


# 1. Response variable is binary
# 2. Check model convergence / singularity (If outcomeis FALSE, then assumption is OK)
isSingular(glmm_flower_all)

# 3. Check residual uniformity (If p>0.05, assumption OK)
testUniformity(res_flower)

# 4. Check overdispersion (If p>0.05, assumption OK)
testDispersion(res_flower)

# 5. Check zero-inflation (If p>0.05, assumption OK)
testZeroInflation(res_flower)

# 6. Visual of residuals
plot(res_flower)

# Assumptions are OK

# Model interpretation:
# .......................................................
# TRAY DIFFERENCES (Testing whether Tray A != Tray B != Tray C)

# • Tray B vs Tray A:
#   - Estimate = 0.3826 (SE = 0.3339, z = 1.146, p = 0.252)
#   - Odds Ratio (OR) = exp(0.3826) ≈ 1.47
#   - INTERPRETATION: Not statistically significant (p > 0.05).
#     Plants in Tray B had ~47% higher odds of flowering than plants in
#     Tray A, but this difference is consistent with random chance.
#
# • Tray C vs Tray A:
#   - Estimate = 0.5797 (SE = 0.3159, z = 1.835, p = 0.067)
#   - Odds Ratio (OR) = exp(0.5797) ≈ 1.79
#   - INTERPRETATION: There is a positive trend, but it is NOT statistically
#     significant at the 0.05 level. Plants in Tray C had ~79% higher odds
#     of flowering than plants in Tray A.
#
#
# OVERALL TRAY CONCLUSION:
# There is NO statistically significant difference in flowering performance
# between the three trays across years, although Tray C shows a tendency
# toward higher flowering odds compared with Tray A.
#
# .......................................................
# YEAR EFFECTS (Relative to Baseline Year 2023)

# • Year 2024 vs 2023:
#   - Estimate = -1.0073 (SE = 0.2963, z = -3.399, p = 0.000676) HIGHLY SIGNIFICANT
#   - Odds Ratio (OR) = exp(-1.0073) ≈ 0.365
#   - INTERPRETATION: Flowering odds in 2024 were ~63.5% lower than in 2023.
#     This difference is statistically significant.
#
# • Year 2025 vs 2023:
#   - Estimate = -0.9074 (SE = 0.2942, z = -3.084, p = 0.002039) SIGNIFICANT
#   - Odds Ratio (OR) = exp(-0.9074) ≈ 0.404
#   - INTERPRETATION: Flowering odds in 2025 were ~59.6% lower than in 2023.
#     This difference is statistically significant.
#
# • Year 2026 vs 2023:
#   - Estimate = -0.1016 (SE = 0.2753, z = -0.369, p = 0.712)
#   - Odds Ratio (OR) = exp(-0.1016) ≈ 0.903
#   - INTERPRETATION: Flowering odds in 2026 were ~9.7% lower than in 2023,
#     but this difference is NOT statistically significant.
#
# OVERALL YEAR CONCLUSION:
# Flowering odds were significantly lower in 2024 and 2025 compared with
# 2023. By 2026, flowering odds were not significantly different from 2023.
#
# .......................................................
# RANDOM EFFECT (ID)
#
# • ID random-effect variance = 1.154 (SD = 1.074)
#   - INTERPRETATION: There is substantial variation in flowering tendency
#     among individual plants/IDs, supporting the use of a random intercept
#     for ID to account for repeated measurements of the same plants.

# First 3 years
# In the GLMM the random-effect variance of ID is 0
# So we used GLM instead


glm_flower_3yr <- glm(
  flowering ~ tray + year, 
  data = df_first3_clean, 
  family = binomial
)
summary(glm_flower_3yr)


# Binomial GLM assumptions:
library(DHARMa)
library(performance)
res_flower_3yr <- simulateResiduals(glm_flower_3yr)

# 1. Response variable is binary
# 2. Check model residual uniformity (If p>0.05, assumption is OK)
testUniformity(res_flower_3yr)

# 3. Check overdispersion (If p>0.05, assumption is OK)
testDispersion(res_flower_3yr)

# 4. Check outliers (If p>0.05, assumption is OK)
testOutliers(res_flower_3yr)

# 5. Check overdispersion using performance package (If p>0.05, assumption is OK)
check_overdispersion(glm_flower_3yr)

# 6. Check multicollinearity
# (VIF close to 1 indicates no problematic multicollinearity)
check_collinearity(glm_flower_3yr)

# 7. Visual of residuals
plot(res_flower_3yr)

# Assumptions are OK
# No significant problems with residual uniformity, dispersion,
# outliers, or multicollinearity were detected.



# INTERPRETATION:
# .......................................................
# TRAY DIFFERENCES

# • Tray B vs Tray A (Baseline):
#   - Estimate = 0.4261 (SE = 0.3209, z = 1.328, p = 0.184)
#   - Odds Ratio (OR) = exp(0.4261) ≈ 1.53
#   - INTERPRETATION: Not statistically significant (p = 0.184). Plants in Tray B
#     had approximately 53% higher odds of flowering than plants in Tray A, but
#     this difference is consistent with random variation.

# • Tray C vs Tray A (Baseline):
#   - Estimate = 0.0691 (SE = 0.3160, z = 0.219, p = 0.827)
#   - Odds Ratio (OR) = exp(0.0691) ≈ 1.07
#   - INTERPRETATION: Not statistically significant (p = 0.827). Plants in Tray C
#     had approximately 7% higher odds of flowering than plants in Tray A,
#     indicating essentially no detectable difference between the two trays.

# OVERALL TRAY CONCLUSION:
# No statistically significant differences in flowering probability
# among the three tray treatments during the first 3 years (2023–2024).

# .......................................................
# YEAR EFFECT (Relative to Baseline Year 2023)

# • Year 2024 vs 2023:
#   - Estimate = -0.8749 (SE = 0.2645, z = -3.307, p = 0.000942) HIGHLY SIGNIFICANT
#   - Odds Ratio (OR) = exp(-0.8749) ≈ 0.417
#   - INTERPRETATION: The odds of a plant flowering in 2024 were approximately
#     58.3% lower than in 2023. This difference was highly statistically
#     significant (p < 0.001).


### B. Dormancy Comparison ---------

# For that we should take out the last year, because we don't have any information on dormancy
# we don't know if the plants will reappear or not
# We'll also take out the unknown, because they might be dormant
# and the dead, because it doesn't help to explain the dormancy among living plants 
df2_dorm <- df2 %>%
  filter(year_num != 2026, state != "unknown", state != "dead")

# Let's take a look at how many dormant plants we have each year
df2_dorm %>%
  group_by(year) %>%
  summarise(dormant = sum(is_dormant),
            total = n(),
            proportion = mean(is_dormant)
  )

# All years
glmm_dorm_all <- glmer(is_dormant ~ tray + year + (1|ID), data = df2_dorm, family = binomial)
summary(glmm_dorm_all)


# Model assumptions:
# (Response variable is binary)
res_dorm <- simulateResiduals(glmm_dorm_all)
# 1. Check whether the model is singular (If outcome is FALSE, then assumption is OK)
isSingular(glmm_dorm_all)
# 2. Residual uniformity (If p>0.05, assumption is OK)
testUniformity(res_dorm)
# 3. Over/underdispersion (If p>0.05, assumption is OK)
testDispersion(res_dorm)
# 4. Excess zeros (If p>0.05, assumption is OK)
testZeroInflation(res_dorm)
# 5. Multicollinearity
check_collinearity(glmm_dorm_all)
# Assumptions are OK




# INTERPRETATION:
# .......................................................
# TRAY DIFFERENCES (Testing whether Tray A != Tray B != Tray C)

# • Tray B vs Tray A (Baseline):
#   - Estimate = -0.8828 (SE = 1.1356, z = -0.777, p = 0.4369)
#   - Odds Ratio (OR) = exp(-0.8828) ≈ 0.41
#   - INTERPRETATION: Not statistically significant (p = 0.437). Plants in Tray B
#     had approximately 59% lower odds of dormancy than plants in Tray A, but this
#     difference is consistent with random variation.

# • Tray C vs Tray A (Baseline):
#   - Estimate = -0.3853 (SE = 0.9642, z = -0.400, p = 0.6894)
#   - Odds Ratio (OR) = exp(-0.3853) ≈ 0.68
#   - INTERPRETATION: Not statistically significant (p = 0.689). Plants in Tray C
#     had approximately 32% lower odds of dormancy than plants in Tray A, but this
#     difference is consistent with random variation.

# OVERALL TRAY CONCLUSION:
# There were no statistically significant differences in dormancy probability
# among the three tray treatments.
#
# This suggests that the initial tray treatment did not have a detectable effect
# on whether plants entered dormancy.


# .......................................................
# YEAR EFFECTS (Relative to Baseline Year 2022)

# • Year 2023 vs 2022:
#   - Estimate = 1.9139 (SE = 1.2774, z = 1.498, p = 0.1341)
#   - Odds Ratio (OR) = exp(1.9139) ≈ 6.78
#   - INTERPRETATION: Not statistically significant (p = 0.134). The odds of
#     dormancy in 2023 were approximately 6.8 times higher than in 2022,
#     but this increase is not statistically significant.

# • Year 2024 vs 2022:
#   - Estimate = 3.6214 (SE = 1.3068, z = 2.771, p = 0.00559)
#   - Odds Ratio (OR) = exp(3.6214) ≈ 37.4
#   - INTERPRETATION: Statistically significant (p = 0.006). The odds of
#     dormancy in 2024 were approximately 37 times higher than in 2022.

# • Year 2025 vs 2022:
#   - Estimate = 3.9298 (SE = 1.3237, z = 2.969, p = 0.00299)
#   - Odds Ratio (OR) = exp(3.9298) ≈ 50.9
#   - INTERPRETATION: Statistically significant (p = 0.003). The odds of
#     dormancy in 2025 were approximately 51 times higher than in 2022.

# OVERALL YEAR CONCLUSION:
# Dormancy probability increased over time. Compared with 2022, plants had
# significantly higher odds of entering dormancy in 2024 and 2025, whereas
# the increase observed in 2023 was not statistically significant.
#
# The observed proportion of dormant plants increased from approximately 1.0%
# in 2022 to 3.9% in 2023, 9.6% in 2024, and 11.8% in 2025, supporting the
# pattern observed in the model.



# First 3 years
df2_dorm3 <- df_first3 %>%
  filter(state != "unknown", state != "dead")

glmm_dorm_3yr <- glmer(is_dormant ~ tray + year + (1|ID), data = df2_dorm3, family = binomial)
summary(glmm_dorm_3yr)


# Model assumptions:
# (Response variable is binary)
res_dorm <- simulateResiduals(glmm_dorm_3yr)
# 1. Check whether the model is singular
isSingular(glmm_dorm_3yr)
# 2. Residual uniformity (If p>0.05, assumption is OK)
testUniformity(res_dorm)
# 3. Over/underdispersion (If p>0.05, assumption is OK)
testDispersion(res_dorm)
# 4. Excess zeros (If p>0.05, assumption is OK)
testZeroInflation(res_dorm)
# 5. Multicollinearity
check_collinearity(glmm_dorm_3yr)
# OK


# INTERPRETATION:
# .......................................................
# TRAY DIFFERENCES (Testing whether Tray A != Tray B != Tray C)

# • Tray B vs Tray A (Baseline):
#   - Estimate = -0.9807 (SE = 2.1930, z = -0.447, p = 0.6547)
#   - Odds Ratio (OR) = exp(-0.9807) ≈ 0.375
#   - INTERPRETATION: Not statistically significant (p = 0.655). Plants in Tray B
#     had approximately 62.5% lower odds of dormancy compared to Tray A, but this
#     difference is consistent with random variation.

# • Tray C vs Tray A (Baseline):
#   - Estimate = 0.0302 (SE = 1.5624, z = 0.019, p = 0.9846)
#   - Odds Ratio (OR) = exp(0.0302) ≈ 1.031
#   - INTERPRETATION: Not statistically significant (p = 0.985). Plants in Tray C
#     had approximately the same odds of dormancy as plants in Tray A.

# OVERALL TRAY CONCLUSION:
# No statistically significant differences in dormancy probability were detected
# among the three trays during the initial period (2022–2024).

# .......................................................
# YEAR EFFECTS (Relative to Baseline Year 2022)

# • Year 2023 vs 2022:
#   - Estimate = 2.6939 (SE = 1.7646, z = 1.527, p = 0.1269)
#   - Odds Ratio (OR) = exp(2.6939) ≈ 14.79
#   - INTERPRETATION: Not statistically significant (p = 0.127). The odds of
#     dormancy in 2023 were approximately 15 times higher than in 2022, but
#     this increase was not statistically significant.

# • Year 2024 vs 2022:
#   - Estimate = 6.1547 (SE = 2.1213, z = 2.901, p = 0.00371)
#   - Odds Ratio (OR) = exp(6.1547) ≈ 471.0
#   - INTERPRETATION: Statistically significant (p = 0.004). The odds of
#     dormancy in 2024 were approximately 471 times higher than in 2022.

# NOTE:
# The very large odds ratio for 2024 does NOT mean that there were 471 times more dormant plants in 2024.
# Dormancy was extremely rare in 2022, with only one dormant
# individual, so the baseline odds of dormancy were extremely small so that change made a huge ratio.

# OVERALL YEAR CONCLUSION:
# Dormancy increased over the initial three years of the experiment.
# Compared with the planting year (2022), plants had significantly higher odds
# of being dormant in 2024, whereas the increase in 2023 was not statistically
# significant.


### C. Sidebulb Production ---------

# Years 2022-2026 (Poisson GLMM)

glmm_side_all <- glmer(sidebulbs ~ tray + year + (1|ID), data = df2, family = poisson)
summary(glmm_side_all)

# Model assumptions:
# 1. Singularity 
isSingular(glmm_side_all)
# 2. Residuals uniformity (If p>0.05, assumption is OK)
res_sidebulb <- simulateResiduals(glmm_side_all)
testUniformity(res_sidebulb)
# 3. overdispersion (If p>0.05, assumption is OK)
testDispersion(res_sidebulb)
# THIS ASSUMPTION IS NOT SATISFIED p=0.016, we have overdispersion
# Which means we should use negative binomial model

library(glmmTMB)
library(performance)

glmm_side_nb <- glmmTMB(sidebulbs ~ tray + year + (1|ID),
  data = df2, family = nbinom2)

summary(glmm_side_nb)


# Model assumptions check
# (Response variable is count data; negative-binomial GLMM)
res_side_nb <- simulateResiduals(glmm_side_nb)
plot(res_side_nb)
# 1. Check whether the model is singular
# Let's check from summary
# ID dispersion = 1.781, which is good, shows there are differences between individuals with different ID's 
# -> the model is not singular, because the variance is clearly bigger than 0
# which is exactly what we want
# 2. Residual uniformity (If p>0.05, assumption is OK)
testUniformity(res_side_nb)
# 3. Over/underdispersion (If p>0.05, assumption is OK)
testDispersion(res_side_nb)
# 4. Excess zeros (If p>0.05, assumption is OK)
testZeroInflation(res_side_nb)
# 5. Outliers (If p>0.05, assumption is OK)
testOutliers(res_side_nb)
# 6. Multicollinearity
check_collinearity(glmm_side_nb)

#OK


# INTERPRETATION:
# .......................................................
# TRAY DIFFERENCES (Testing whether Tray A != Tray B != Tray C)

# • Tray B vs Tray A (Baseline):
#   - Estimate = -0.4233 (SE = 0.4784, z = -0.885, p = 0.376)
#   - Rate Ratio (RR) = exp(-0.4233) ≈ 0.65
#   - INTERPRETATION: Not statistically significant (p = 0.376).
#     Plants in Tray B had approximately 35% lower expected sidebulb
#     counts compared to Tray A, but there is insufficient evidence
#     that this difference represents a real tray effect.

# • Tray C vs Tray A (Baseline):
#   - Estimate = -0.2701 (SE = 0.4501, z = -0.600, p = 0.548)
#   - Rate Ratio (RR) = exp(-0.2701) ≈ 0.76
#   - INTERPRETATION: Not statistically significant (p = 0.548), but
#     Plants in Tray C had approximately 24% lower expected sidebulb
#     counts compared to Tray A.

# OVERALL TRAY CONCLUSION:
# No statistically significant differences in sidebulb production between trays 

# .......................................................
# YEAR EFFECTS (Relative to Baseline Year 2022)

# • Year 2023 vs 2022:
#   - Estimate = 0.6879 (SE = 0.4194, z = 1.640, p = 0.101)
#   - Rate Ratio (RR) = exp(0.6879) ≈ 1.99
#   - INTERPRETATION: Not statistically significant (p = 0.101).
#     Plants produced approximately 1.99 times as many expected
#     sidebulbs in 2023 as in 2022, but there is insufficient evidence
#     that this difference represents a real year effect.

# • Year 2024 vs 2022:
#   - Estimate = 0.0760 (SE = 0.4387, z = 0.173, p = 0.863)
#   - Rate Ratio (RR) = exp(0.0760) ≈ 1.08
#   - INTERPRETATION: Not statistically significant (p = 0.863).
#     Plants had approximately 8% higher expected sidebulb counts
#     in 2024 than in 2022, but there is no evidence of a meaningful
#     difference between these years.

# • Year 2025 vs 2022:
#   - Estimate = -0.5401 (SE = 0.4936, z = -1.094, p = 0.274)
#   - Rate Ratio (RR) = exp(-0.5401) ≈ 0.58
#   - INTERPRETATION: Not statistically significant (p = 0.274).
#     Plants had approximately 42% lower expected sidebulb counts
#     in 2025 than in 2022, but there is insufficient evidence that
#     this difference represents a real year effect.

# • Year 2026 vs 2022:
#   - Estimate = -0.8321 (SE = 0.5298, z = -1.571, p = 0.116)
#   - Rate Ratio (RR) = exp(-0.8321) ≈ 0.44
#   - INTERPRETATION: Not statistically significant (p = 0.116).
#     Plants had approximately 56% lower expected sidebulb counts
#     in 2026 than in 2022, but this difference was not statistically
#     significant.

# OVERALL YEAR CONCLUSION:
# No statistically significant differences in sidebulb production were
# detected among years 2022–2026. Although estimated sidebulb production
# was higher in 2023 and lower in 2025–2026 relative to 2022, none of
# these differences were statistically significant.

# .......................................................
# BASELINE EXPECTED SIDE BULB PRODUCTION

# • Baseline Year 2022, Tray A:
#   - Intercept = -2.8631 (p < 0.001)
#   - Expected sidebulb count = exp(-2.8631) ≈ 0.057 sidebulbs per plant
#   - INTERPRETATION: Under the baseline conditions (Tray A in 2022),
#     the model estimated an expected sidebulb count of approximately
#     0.057 sidebulbs per plant.

# .......................................................
# RANDOM EFFECTS (Individual Variation)

# • Individual Random Intercept (ID):
#   - Variance = 1.781
#   - Std. Dev. = 1.334
#   - INTERPRETATION: There was substantial variation among individual
#     plants in sidebulb production. Even after accounting for tray
#     and year, individual plants differed in their expected sidebulb
#     production.


# First 3 years
glmm_side_3yr <- glmer(sidebulbs ~ tray + year + (1|ID), data = df_first3, family = poisson)
summary(glmm_side_3yr)


# Model assumptions check
# (Response variable is count data; negative-binomial GLMM)
res_side_3yr <- simulateResiduals(glmm_side_3yr)
# 1. Singularity
isSingular(glmm_side_3yr)
# 2. Residual uniformity (If p>0.05, assumption is OK)
testUniformity(res_side_3yr)
# 3. Overdispersion (If p>0.05, assumption is OK)
testDispersion(res_side_3yr)
# 4. Excess zeros (If p>0.05, assumption is OK)
testZeroInflation(res_side_3yr)
# 5. Outliers (If p>0.05, assumption is OK)
testOutliers(res_side_3yr)
# 6. Multicollinearitu
check_collinearity(glmm_side_3yr)
# Assumptions OK

# INTERPRETATION:
# .......................................................
# TRAY DIFFERENCES (Testing whether Tray A != Tray B != Tray C)

# • Tray B vs Tray A (Baseline):
#   - Estimate = -0.5328 (SE = 0.5512, z = -0.967, p = 0.334)
#   - Rate Ratio (RR) = exp(-0.5328) ≈ 0.59
#   - INTERPRETATION: Not statistically significant (p = 0.334).
#     Plants in Tray B had approximately 41% lower expected sidebulb
#     counts compared to Tray A, but this difference is consistent
#     with random variation.

# • Tray C vs Tray A (Baseline):
#   - Estimate = -0.4459 (SE = 0.5258, z = -0.848, p = 0.396)
#   - Rate Ratio (RR) = exp(-0.4459) ≈ 0.64
#   - INTERPRETATION: Not statistically significant (p = 0.396).
#     Plants in Tray C had approximately 36% lower expected sidebulb
#     counts compared to Tray A, but this difference is consistent
#     with random variation.

# OVERALL TRAY CONCLUSION:
# During 2022–2024, no statistically significant differences in sidebulb production among the three trays.

# .......................................................
# YEAR EFFECTS (Relative to Baseline Year 2022)

# • Baseline Year 2022:
#   - Intercept = -3.0996 (p < 0.001)
#   - Expected sidebulb count = exp(-3.0996) ≈ 0.045 sidebulbs per plant
#   - INTERPRETATION: Under the baseline conditions (Tray A in 2022),
#     the expected sidebulb count was approximately 0.045 sidebulbs
#     per plant, indicating that sidebulb production was very rare
#     during the first growing season.

# • Year 2023 vs 2022:
#   - Estimate = 0.6321 (SE = 0.3387, z = 1.866, p = 0.062)
#   - Rate Ratio (RR) = exp(0.6321) ≈ 1.88
#   - INTERPRETATION: Not statistically significant (p = 0.062).
#     Plants produced approximately 1.88 times as many expected
#     sidebulbs in 2023 as in 2022, but there is insufficient evidence
#     to conclude that sidebulb production genuinely differed between
#     these years.

# • Year 2024 vs 2022:
#   - Estimate = 0.1253 (SE = 0.3688, z = 0.340, p = 0.734)
#   - Rate Ratio (RR) = exp(0.1253) ≈ 1.13
#   - INTERPRETATION: Not statistically significant (p = 0.734).
#     Plants produced approximately 1.13 times as many expected
#     sidebulbs in 2024 as in 2022, but there is insufficient evidence
#     of a real difference between these years.

# OVERALL YEAR CONCLUSION:
# During 2022–2024, no statistically significant differences in
# sidebulb production were detected between years. Although production
# was estimated to be higher in 2023 than in 2022, this difference did
# not reach statistical significance.

# .......................................................
# RANDOM EFFECTS (Individual Variation)

# • Individual Random Intercept (ID):
#   - Variance = 2.689 (Std. Dev. = 1.640)
#   - INTERPRETATION: There was considerable variation among individual
#     plants in their ability to produce sidebulbs. Even after accounting
#     for tray and year effects, some plants consistently produced more
#     sidebulbs than others.


##### LINE PLOT OF MEAN HEIGHT OF FIRST LEAF ####

# Line plot connecting the means of [the variable of interest] in all the trays throughout the years
# For example the first leaf
ggplot(df2_states, aes(x = factor(year), y = h1, color = tray, group = tray)) +
  # plotting the mean line 
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  # error bars (standard error of the mean)
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, size = 1) +
  labs(x = "Year", y = "Mean height of first leaf by trays [mm] (± SE)", color = "Tray") +
  theme_bw()


##### FLOWERING AND SIDEBULB PRODUCTION RATE PLOTS ####

library(tidyverse)

# Calculate rates by tray and year
rates <- df2 %>%
  group_by(tray, year) %>%
  summarise(
    flowering_rate = mean(state == "flowering", na.rm = TRUE),
    sidebulb_rate  = mean(grew_sidebulbs > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(flowering_rate, sidebulb_rate),
    names_to = "trait",
    values_to = "rate"
  ) %>%
  mutate(
    trait = recode(
      trait,
      flowering_rate = "Flowering",
      sidebulb_rate = "Sidebulb production"
    )
  )

ggplot(rates, aes(x = year, y = rate, colour = tray, linetype = trait,
    group = interaction(tray, trait))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)) +
  scale_linetype_manual(values = c(
      "Flowering" = "solid",
      "Sidebulb production" = "dashed")) +
  labs(x = "Year", y = "Proportion of plants", 
       colour = "Tray", linetype = "Trait") +
  theme_bw() +
  theme(legend.position = "bottom")



##### STATES WITHIN TRAYS ####

library(patchwork)

# To set the order of columns
df2_states <- df2_states %>%
  mutate(state = factor(state, levels = c("flowering", "vegetative", "1-leaf", "dormant", "unknown", "dead")))

# Plots that show actual number of observations
# Good to show actual population size and it's changes
p1 <- ggplot(data = df2_states, aes(x=year_num)) + 
  geom_bar(aes(fill = state)) +
  labs(x ="Year", y = "Count", fill = "State") + theme_bw()

p2 <- ggplot(data = df2_states, aes(x=year_num)) + 
  geom_bar(aes(fill = state)) +
  labs(x ="Year", y = "Count", fill = "State") + theme_bw() + facet_wrap(~tray)

p1 + p2 + 
  plot_layout(guides = "collect") & theme(legend.position = "right")


# Plots that show the proportions of plants in each state
# Good to compare years
p3 <- ggplot(data = df2_states, aes(x=year_num)) + 
  geom_bar(position = "fill", aes(fill = state)) +
  labs(x ="Year", y = "Percent", fill = "State") +
  scale_y_continuous(labels = scales::percent) + # So that the percentage scale would be 0-100% not 0.00-1.00
  theme_bw() 

p4 <- ggplot(data = df2_states, aes(x=year_num)) + 
  geom_bar(position = "fill", aes(fill = state)) +
  labs(x ="Year", y = "Percent", fill = "State") +
  scale_y_continuous(labels = scales::percent) + # So that the percentage scale would be 0-100% not 0.00-1.00
  theme_bw() + facet_wrap(~tray)

p3 + p4 + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# All together

(p1 + p2) / (p3 + p4) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom") 




#..............................................................................
##### _________________ Vannikatse FAMILY TREES _______________________ #####
#..............................................................................



library(tidyverse)
library(igraph)
library(ggraph)

df <- read_csv2("data_vannikatse.csv")

# Extract combined tray (merges "AX" into "A", "BX" into "B", "CX" into "C")
get_combined_tray <- function(id) {
  raw_tray <- str_extract(id, "^[A-Za-z]+")
  str_remove(raw_tray, "X$")
}

get_parent_combined <- function(id) {
  combined_tray <- get_combined_tray(id)
  raw_tray <- str_extract(id, "^[A-Za-z]+")
  rest <- str_remove(id, paste0("^", raw_tray, "\\d+"))
  
  if (rest == "") {
    return(paste0("ROOT_", combined_tray))
  }
  last_piece <- str_extract(rest, "(>\\d+|[a-zA-Z]+\\d*)$")
  substr(id, 1, nchar(id) - nchar(last_piece))
}

all_ids <- unique(na.omit(df$ID))

edges <- data.frame(
  to   = all_ids,
  from = map_chr(all_ids, get_parent_combined)
) %>%
  select(from, to) %>%
  distinct()

##### FAMILY TREE WITH STATE DORMANT ####


# Calculate complete yearly state timeline per plant with 3-year death rule
all_years <- sort(unique(df$year))
full_grid <- expand_grid(ID = all_ids, year = all_years)

status_by_year <- full_grid %>%
  left_join(df, by = c("ID", "year")) %>%
  mutate(
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering  = !is.na(genh)
  ) %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Count consecutive missing years
    missing_streak = cumsum(leaf_count == 0) - cummax((leaf_count > 0) * cumsum(leaf_count == 0)),
    leaves_after   = rev(cumany(rev(leaf_count > 0))),
    
    state = case_when(
      flowering ~ "flowering",
      leaf_count >= 1 ~ "vegetative",
      missing_streak >= 3 ~ "dead",
      leaf_count == 0 & (leaves_after | missing_streak < 3) ~ "dormant",
      TRUE ~ "dead"
    ),
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

# Extract most recent status for lineage nodes
last_status <- status_by_year %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  select(ID, status = state)

# Build vertex data frame
nodes <- data.frame(name = unique(c(edges$from, edges$to))) %>%
  mutate(
    is_root = str_starts(name, "ROOT_"),
    tray    = ifelse(is_root, str_remove(name, "ROOT_"), get_combined_tray(name))
  ) %>%
  left_join(last_status, by = c("name" = "ID")) %>%
  mutate(status = ifelse(is_root, "tray", replace_na(status, "dormant"))) %>%
  mutate(status = factor(status, levels = c("flowering", "vegetative", "dormant", "dead", "tray")))

# Generate combined treemaps for Trays A, B, and C
combined_trays <- c("A", "B", "C")

for (t in combined_trays) {
  nodes_t <- nodes %>% filter(tray == t)
  edges_t <- edges %>% filter(from %in% nodes_t$name, to %in% nodes_t$name)
  
  gs <- graph_from_data_frame(edges_t, vertices = nodes_t, directed = TRUE)
  
  p <- ggraph(gs, layout = "tree") +
    geom_edge_diagonal(colour = "grey65", linewidth = 0.4) +
    geom_node_point(aes(colour = status, shape = status), size = 3) +
    geom_node_text(
      aes(label = ifelse(is_root, "", name)),
      hjust = -0.3,
      size = 2.6,
      family = "mono"
    ) +
    scale_colour_manual(
      values = c(
        flowering  = "#b641b8",  
        vegetative = "#829645",  
        dormant    = "grey70",   
        dead       = "red4",     
        tray       = "black"
      ), 
      breaks = c("flowering", "vegetative", "dormant", "dead")
    ) +
    scale_shape_manual(
      values = c(
        flowering  = 16, # Solid circle
        vegetative = 16, 
        dormant    = 16, 
        dead       = 16, 
        tray       = 16
      ),
      breaks = c("flowering", "vegetative", "dormant", "dead")
    ) +
    coord_flip() +
    theme_void() +
    labs(
      title = paste("Tray", t),
      subtitle = "Most recent known status (3-year dormancy threshold for death)",
      colour = "Plant Status",
      shape  = "Plant Status"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  
  print(p)
}



##### FAMILY TREE WITH STATE UNKNOWN ####

# Calculate complete yearly state timeline per plant with 3-year death rule
all_years <- sort(unique(df$year))
full_grid <- expand_grid(ID = all_ids, year = all_years)

status_by_year <- full_grid %>%
  left_join(df, by = c("ID", "year")) %>%
  mutate(
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering  = !is.na(genh)
  ) %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Count consecutive missing years
    missing_streak = cumsum(leaf_count == 0) - cummax((leaf_count > 0) * cumsum(leaf_count == 0)),
    leaves_after   = rev(cumany(rev(leaf_count > 0))),
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count >= 1 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown" 
    ),
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

# Extract most recent status for lineage nodes
last_status <- status_by_year %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  slice_tail(n = 1) %>%
  select(ID, status = state)

# Build vertex data frame
nodes <- data.frame(name = unique(c(edges$from, edges$to))) %>%
  mutate(
    is_root = str_starts(name, "ROOT_"),
    tray    = ifelse(is_root, str_remove(name, "ROOT_"), get_combined_tray(name))
  ) %>%
  left_join(last_status, by = c("name" = "ID")) %>%
  mutate(status = ifelse(is_root, "tray", replace_na(status, "dormant"))) %>%
  mutate(status = factor(status, levels = c("flowering", "vegetative", "dormant", "unknown", "dead", "tray")))

# Generate combined treemaps for Trays A, B, and C
combined_trays <- c("A", "B", "C")

for (t in combined_trays) {
  nodes_t <- nodes %>% filter(tray == t)
  edges_t <- edges %>% filter(from %in% nodes_t$name, to %in% nodes_t$name)
  
  gs <- graph_from_data_frame(edges_t, vertices = nodes_t, directed = TRUE)
  
  p <- ggraph(gs, layout = "tree") +
    geom_edge_diagonal(colour = "grey65", linewidth = 0.4) +
    geom_node_point(aes(colour = status, shape = status), size = 3) +
    geom_node_text(
      aes(label = ifelse(is_root, "", name)),
      hjust = -0.3,
      size = 2.6,
      family = "mono"
    ) +
    scale_colour_manual(
      values = c(
        flowering  = "#b641b8",  
        vegetative = "#829645",  
        dormant = "orange2",
        unknown = "gray40",
        dead = "red4",     
        tray = "black"
      ), 
      breaks = c("flowering", "vegetative", "dormant", "unknown", "dead")
    ) +
    scale_shape_manual(
      values = c(
        flowering = 16, # Solid circle
        vegetative = 16, 
        dormant = 16, 
        unknown = 16,
        dead = 16, 
        tray = 16
      ),
      breaks = c("flowering", "vegetative", "dormant", "unknown", "dead")
    ) +
    coord_flip() +
    theme_void() +
    labs(
      title = paste("Tray", t),
      subtitle = "Most recent known status (3-year dormancy threshold for death)",
      colour = "Plant Status",
      shape  = "Plant Status"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
  
  print(p)
}


#..............................................................................
##### _________________ INDIVIDUAL PROFILE FUNCTION  _______________________ #####
#..............................................................................


library(tidyverse)
library(scales)

df_clean <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    year_num   = as.numeric(year),
    ID         = as.character(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering  = as.integer(!is.na(genh)),
    h1_cm      = h1 / 10
  )

# global limits from the entire dataset to prevent axis distortion
MAX_LEAF_COUNT <- max(df_clean$leaf_count, na.rm = TRUE) 
MAX_HEIGHT_CM  <- max(df_clean$h1_cm, na.rm = TRUE)      
SCALE_FACTOR    <- MAX_HEIGHT_CM / MAX_LEAF_COUNT         

plot_individual_profile <- function(plant_id, data = df_clean) {
  
  plant_data <- data %>% filter(ID == plant_id)
  
  if (nrow(plant_data) == 0) {
    message(paste("Warning: Plant ID", plant_id, "not found in dataset"))
    return(NULL)
  }
  
  # ensuring all years (2022-2026) appear on the X axis, even if a sidebulb was absent in early years
  all_years <- data.frame(year_num = seq(min(data$year_num), max(data$year_num), by = 1))
  plant_data <- full_join(plant_data, all_years, by = "year_num") %>% 
    arrange(year_num)
  
  # Non-NA height points for clean rendering
  line_data <- plant_data %>% filter(!is.na(h1_cm))
  
  ggplot(plant_data, aes(x = year_num)) +
    # Leaf count bar chart
    geom_col(aes(y = leaf_count), fill = "seagreen3", alpha = 0.6, width = 0.4) +
    # Height line and points (scaled using the global scale factor)
    geom_line(data = line_data, aes(y = h1_cm / SCALE_FACTOR), color = "darkblue", linewidth = 1.2) +
    geom_point(data = line_data, aes(y = h1_cm / SCALE_FACTOR), color = "darkblue", size = 3) +
    # Width as comparison
    geom_line(data = line_data, aes(y = w1/10), color = "blue", linewidth = 1, linetype = 2) + 
    geom_point(data = line_data, aes(y = w1/10), color = "blue", size = 2) + 
    
    # Flowering markers
    geom_point(
      data = filter(plant_data, flowering == 1), 
      aes(y = leaf_count + 0.3), 
      color = "magenta3", 
      shape = 8, 
      size = 5
    ) +    
    # sidebulb point
    geom_point(
      data = filter(plant_data, is.na(`sidebulb_nr`) == FALSE),
      aes(y = leaf_count + 0.5), 
      color = "brown", 
      shape = 9, 
      size = 5
    ) +
    # Standardized axes across ALL individual graphs
    scale_y_continuous(
      name = "Leaf Count (Green Bars)",
      limits = c(0, MAX_LEAF_COUNT + 1),
      breaks = 0:MAX_LEAF_COUNT,
      sec.axis = sec_axis(~ . * SCALE_FACTOR, name = "1st Leaf Height and width [cm] (Blue Line)")
    ) +
    scale_x_continuous(breaks = seq(min(data$year_num), max(data$year_num), by = 1)) +
    labs(
      title = paste("Individual Profile:", plant_id),
      subtitle = "Asterisks (*) denote flowering years; blank years denote missing/dormant states",
      x = "Year"
    ) +
    theme_bw() +
    theme(
      plot.title        = element_text(face = "bold", size = 13),
      axis.title.y.right = element_text(color = "darkblue", face = "bold"),
      axis.title.y.left  = element_text(color = "seagreen4", face = "bold")
    )
}

# Example usage for a single graph:
 plot_individual_profile("A11")


# generating graphs for every individual plant & sidebulb

#all_ids <- unique(na.omit(df_clean$ID))

#saving all individual graphs to a PDF
# pdf("All_Individual_Plant_Profiles.pdf", width = 8, height = 5)
# for (id in all_ids) {
#  p <- plot_individual_profile(id, data = df_clean)
#  if (!is.null(p)) print(p)
# }
# dev.off()




 # does a width-height ratio make sense? Or should it rather be a width-length ratio?
 df2_states$ratio_lw <- df2_states$l1 / (df2_states$w1)
 
 plot(df2_states$leaf_count, df2_states$ratio_lw)
 
 plot(df2_states$ratio_hw)
 
 plot(df2_states$w1,df2_states$l1)
 # not really, as it is much harder to interpret
 # questions here: which leaves to compare? lowest leaf? or highest leaf, which is always the first leaf... 
 # Compare their height or their length? The ratio definitely changes based on that. And length makes more sense.
 # But it is also somewhat strange because the length is sometiems very small. Maybe, I cannot compare this for anything else than 1-leaved individuals?





#..............................................................................
##### _________________ Vannikatse PATH ANALYSIS  _______________________ #####
#..............................................................................

 
 library(tidyverse)
 library(lme4)
 library(lmerTest) 
 library(dplyr)
 
 
 # Load and clean data
 df3 <- read_csv2("data_vannikatse.csv") %>%
   mutate(
     tray = str_extract(ID, "^[A-Za-z]"),
     tray = factor(tray),
     year_num = as.numeric(year),
     year = factor(year),
     ID = factor(ID),
     leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
     flowering = as.integer(!is.na(genh)),
     sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
     grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
   ) %>%
   arrange(ID, year_num) %>%
   group_by(ID) %>%
   mutate(
     leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
     leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
     
     state = case_when(
       flowering == 1 ~ "flowering",
       leaf_count == 1 ~ "1-leaf",
       leaf_count >= 2 ~ "vegetative",
       
       # Dormancy conditions
       leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
       leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
       
       # Death contiton
       leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
       
       # The rest will be unknown
       leaf_count == 0 ~ "unknown"
     )
   ) %>%
     # Let's mark the rest of the years of death as "dead" too
     mutate(
       dead_from_here = cumany(state == "dead"),
       state = if_else(dead_from_here, "dead", state)
     ) %>%
     ungroup()
 
 # Path analysis should test and showcase direct and indirect effects
 # It has basically the same assumptions as linear regression:
 # 1. Linearity
 # 2. Normal distribution of residuals
 # 3. Homoscedasticity: Variance of residuals is consistent
 # 4. OBSERVATIONS MUST BE INDEPENDENT
 # 5. Low multicollinearity
 # Plus a few other assumptions
 
 
 # The main concern here is that path analysis assumes that the 
 # measurements are independent, which in our case isn't true due to the repeated mesurings of the same plants
 
 
 # So we have some other options:
 # 1. CLPM - cross lagged panel model - two-way effects between different groups over time
 # 2. LGCM - latent growth curve - focuses on the trajectory of change (questions about growth, aging, development)
 # 3. Multilevel SEM - when data has nested/clustered structure - multiple levels
 # 4. Piecewise SEM - can use linear mixed-effect models for each individual path
 
 
 # It feels like piecewise SEM produces the most similar outcome to what we want to achieve
 
 library(piecewiseSEM)
 library(lme4)
 
 
 # After plenty of modelling (which isn't included here)
 # I've come to a conclusion that for it to work we need to change the NAs of dormancy flowering or vegh to 0
 # and remove the NAs of dead and unknown plants
 
 df_new <- df3 %>%
   filter(state != "dead", state != "unknown")
 
 df_new$flower_nr[is.na(df_new$flower_nr)] <- 0
 df_new$flower_len[is.na(df_new$flower_len)] <- 0
 df_new$vegh[is.na(df_new$vegh)] <- 0
 
 
 # Let's try an easier model
 # It has limitations though:
 # 1. Dormancy is represented by vegh = 0
 # 2. Describes only active plant development, because dead and unknown plants are excluded
 df_new <- subset(df_new, !is.na(vegh) & !is.na(year_num) & !is.na(flower_nr))
 df_model_data <- df_new[, c("ID", "tray", "vegh", "year_num", "flower_nr", "flowering")]
 
 # Rescaling the variables:
 # 2022 becomes 0, 2023 becomes 1 so a unit of change essentially represents 1 year of growth 
 df_model_data$year_scaled <- df_model_data$year_num - min(df_model_data$year_num) 
 # Converting to a smaller scale (2.95dm instead of 295mm)
 df_model_data$vegh_scaled <- df_model_data$vegh / 100
 
 # Renaming the variables for the plot to have prettier titles
 df_model_data <- df_model_data %>%
   rename(Year = year_scaled,
          Vegetative_Height = vegh_scaled,
          Flower_Number = flower_nr,
          Flowering = flowering)
 
 model <- psem(
   lmer(Vegetative_Height ~ Year + (1 | ID), data = df_model_data),
   glm(Flowering ~ Vegetative_Height + Year, family = binomial, data = df_model_data) # Because the glmm was singular
 )
 
 summary(model)
 # INTERPERTATION:
 # Year -> Vegetative height
 # std effect 0.370 and p < 0.05 so plants become taller throughout the years
 # For every 1SD increase in time, vegetative height increases by 0.37SD
 # Vegetative height -> Flowering
 # std effect 0.851 and p < 0.05 so taller plants are more likely to flower
 # For every 1SD increase in vegetative height, the log-odds of flowering increase by 0.85 SD
 # Year -> Flowering
 # No direct effect of year after accounting for height (p=0.526)
 
 #Biologically:
 # With time plants get taller -> and are more likely to flower 
 # and year itself doesn't have effect on flowering, it only does, because plants have grown
 
 # To make the results make sense biologically, we look at the Estimate value given in the model
 # Since we standardised our year (0,1,2,3,4) and vegh (vegh/100), vegh estimate = 0.5974
 # Each additional year increases expected vegh by: 0.5974*100 = 59.74mm 
 
 # For every additional 100 mm of vegetative height, the odds of flowering are about exp(1.321) = 3.75 times greater
 
 # marginal R**2 = 0.13 means that year alone explains about 14% of the variation in vegetative height
 # (which makes sense, because there are many other factors that influence it)
 # conditional R**2 = 0.42 when we take into account the differences among individual plants then 42% of the variation is explained
 # Flowering Nagelkirke R**2 = 0.64 for flowering, which means that year and vegh explain 64% of the variance in flowering probability  
 
 plot(model, show = "std", digits = 2,
      node_attrs = list(shape = "rectangle", fillcolor = "thistle",
                        fontname = "Times",fontsize = 6))
 
 
 
 
 
 
 
 # ...............................................................
 # PIECEWISE SEM OF GLADIOLUS PERFORMANCE ----------------------
 # ...............................................................
 
 
 library(dplyr)
 library(lme4)
 library(piecewiseSEM)
 library(DHARMa)
 library(performance)
 
 df_new <- df3 %>%
   filter(state != "dead", state != "unknown")
 df_new$flower_nr[is.na(df_new$flower_nr)] <- 0
 df_new$flower_len[is.na(df_new$flower_len)] <- 0
 df_new$vegh[is.na(df_new$vegh)] <- 0
 df_new <- subset(df_new, !is.na(vegh) & !is.na(year_num) & !is.na(flower_nr))

 df_sem <- df_new %>%
   mutate(
     year_sc = as.numeric(scale(year_num)),
     vegh_sc = as.numeric(scale(vegh))) %>%
   select(ID, tray, vegh_sc, year_sc, flowering, sidebulbs)
 
 df_sem$ID <- factor(df_sem$ID)
 df_sem$tray <- factor(df_sem$tray)
 
 df_sem_sidebulbless <- df_sem %>%
   filter(str_detect(ID, "^[A-Za-z](X)?[0-9]+$"))
 

 sem_model <- psem(
   lmer(vegh_sc ~ tray + year_sc + (1|ID), data = df_sem_sidebulbless), 
   glm(flowering ~ vegh_sc + tray + year_sc, family = binomial, data = df_sem_sidebulbless), 
   glmer(sidebulbs ~ flowering + vegh_sc + tray + year_sc + (1|ID), family = poisson, data = df_sem_sidebulbless))
 
 summary(sem_model, conserve = TRUE)
 
 # Standardized coefficients
 coefs(sem_model, standardize = "scale")
 
 # R² values
 rsquared(sem_model)
 
 library(DiagrammeR)
 
 grViz("digraph SEM {
  graph [layout = dot, rankdir = LR, splash = false]

  node [shape = rectangle,
        style = filled,
        fillcolor = thistle,
        fontname = Times,
        fontsize = 12]

  edge [fontname = Helvetica, fontsize = 10]

  Year   [label = 'Year']
  Tray   [label = 'Tray']
  Height [label = 'Vegetative height']
  Flower [label = 'Flowering']
  Side   [label = 'Sidebulbs']

  # Paths to Vegetative Height
  Year -> Height [label = '0.35***', penwidth = 2.0]
  Tray -> Height [label = 'p = 0.0038**', style = solid]

  # Paths to Flowering
  Height -> Flower [label = '2.65***', penwidth = 2.5]
  Year -> Flower   [label = 'p = 0.157', style = dashed, color = gray50]
  Tray -> Flower   [label = 'p = 0.400', style = dashed, color = gray50]

  # Paths to Sidebulbs
  Flower -> Side [label = '0.98**', penwidth = 1.8]
  Year -> Side   [label = '-0.41**', penwidth = 1.5]
  Height -> Side [label = 'p = 0.946', style = dashed, color = gray50]
  Tray -> Side   [label = 'p = 0.229', style = dashed, color = gray50]
}")
 
 
 
 
 # INTERPRETATION:
 
 # Year -> Vegetative height
 # Vegetative height increases throughout the years (p < 0.005)
 
 # Tray -> Vegetative height
 # Tray had a significant effect (p = 0.0038) on vegetative height.
 # Plants in tray C had significantly higher vegetative height than the average (estimate = 0.514, p < 0.05),
 # while trays A and B did not differ significantly.
 
 # Vegetative height -> Flowering
 # Taller plants were significantly more likely to flower (Estimate = 2.652, p < 0.001).
 
 # Year -> Flowering
 # No direct effect of year on flowering after accounting for plant height (p = 0.533).
 
 # Tray -> Flowering
 # Trays did not have overall effect on flowering, however
 # tray C flowered significantly less than average (Estimate = -0.882, p = 0.0006)
 
 # Flowering -> Sidebulbs
 # Flowering plants produced significantly more sidebulbs (Estimate = 0.98, p < 0.001).
 
 # Vegetative height -> Sidebulbs
 # After accounting for flowering,vegh had no direct effect on sidebulb production
 # (p = 0.016).
 
 # Year -> Sidebulbs
 # Sidebulb production decreased significantly over the years (Estimate = -0.41, p < 0.05).
 
 # Tray -> Sidebulbs
 # Tray had no overall significant effect on sidebulb production (p = 0.777).
 
 
 
 # BIOLOGICALLY:
 
 # Plants became taller as the experiment progressed.
 # Taller plants were more likely to flower, and flowering plants produced more sidebulbs.
 
 # Once vegetative height was taken into account, year itself no longer affected flowering,
 # suggesting that the effect of time on flowering is mainly indirect through plant growth.
 # The same conclusion we had from the last SEM
 
 # Sidebulb production was positively associated with flowering, but showed no direct relationship
 # with vegetative height after flowering was accounted for. 

 # Year had a direct negative effect on sidebulb production, suggesting potential corm aging, resource 
 # depletion, or shifted allocation strategies as individual plants age.
 
 # Overall, vegetative height was moderately explained by year, tray location, and individual random effects 
 # (marginal R² = 0.17, conditional R² = 0.27). Flowering probability was very strongly predicted by the model 
 # (Nagelkerke R² = 0.61). Sidebulb production exhibited low explained variance (marginal R² = 0.009, 
 # conditional R² = 0.037), indicating that while biological pathways are real, sidebulb output remains heavily 
 # driven by unmeasured micro-environmental or physiological stochasticity.
 











#..............................................................................
##### _________________ Vannikatse LIFE-STAGE DEVELOPMENT  _______________________ #####
#..............................................................................

# incl. matrices

# Does the assumption of 1-leaf -> 2-leaf -> flowering apply?

library(tidyverse)
library(lme4)
library(lmerTest) 
library(dplyr)


# Load and clean data
df <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

##### TRANSITION MATRIX T -> T1####


# Transition pairs t-> t1
transitions_1 <- df %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,
    state_t1 = lead(state)
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  # Let's also exclude the dead and unknown states
  filter(!is.na(state_t), !is.na(state_t1),
         !state_t %in% c("unknown", "dead"),
         !state_t1 %in% c("unknown", "dead")) %>%
  ungroup()


# Calculating transition probabilities
transition_matrix <- transitions_1 %>%
  count(state_t, state_t1) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()

# We can view it as a table...
library(tidyr)
transition_matrix %>%
  select(state_t, state_t1, probability) %>%
  pivot_wider(names_from = state_t1, values_from = probability, values_fill = 0)

# ..or use ggplot to view it as a transition matrix
library(ggplot2)
library(scales)

# So they would be in logical order in the matrix
state_order <- c(
  "unknown",
  "dead",
  "dormant",
  "flowering",
  "vegetative",
  "1-leaf"
)

transition_matrix <- transition_matrix %>%
  mutate(
    state_t = factor(state_t, levels = state_order),
    state_t1 = factor(state_t1, levels = state_order)
  )


ggplot(transition_matrix, aes(x = state_t1, y = state_t, fill = probability)) +
  geom_tile(color = "white", size = 0.5) +
  # adding the probabilities inside the squares
  geom_text(aes(label = sprintf("%.1f", probability*100)), # sprintf("%.1f", probability) rounds to exactly two decimals
            color = "black", size = 5) +
  # hex color from the picture again :)
  scale_fill_gradient(low = "white", high = "#b641b8", limit = c(0,1), name = "Transition\nProbability",
                      labels = percent_format(accuracy = 1)) +
  labs(
    title = "Plant Life-Stage Transition Matrix (t to t+1)",
    x = "State in Year t + 1",
    y = "State in Year t"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10)
  )

df %>%
  group_by(state) %>%
  summarise(kokku = n())

df %>%
  filter(state == "vegetative", leaf_count > 2) %>%
  select(ID, leaf_count)



##### TRANSITION MATRIX T -> T2####


# Transition pairs
transitions <- df %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,
    state_t2 = lead(state, 2)
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  filter(!is.na(state_t), !is.na(state_t2),
         !state_t %in% c("unknown", "dead"),
         !state_t2 %in% c("unknown", "dead")) %>%
  ungroup()

# Calculating transition probabilities
transition_matrix <- transitions %>%
  count(state_t, state_t2) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()

# We can view it as a table...
library(tidyr)

transition_matrix %>%
  select(state_t, state_t2, probability) %>%
  pivot_wider(names_from = state_t, values_from = probability, values_fill = 0)

# ..or use ggplot to view it as a transition matrix
library(ggplot2)
library(scales)

# So they would be in logical order in the matrix
state_order <- c(
  "unknown",
  "dead",
  "dormant",
  "flowering",
  "vegetative",
  "1-leaf"
)

transition_matrix <- transition_matrix %>%
  mutate(
    state_t = factor(state_t, levels = state_order),
    state_t2 = factor(state_t2, levels = state_order)
  )


ggplot(transition_matrix, aes(x = state_t2, y = state_t, fill = probability)) +
  geom_tile(color = "white", size = 0.5) +
  # adding the probabilities inside the squares
  geom_text(aes(label = sprintf("%.1f", probability*100)), # sprintf("%.1f", probability) rounds to exactly two decimals
            color = "black", size = 5) +
  # hex color from the picture again :)
  scale_fill_gradient(low = "white", high = "#b641b8", limit = c(0,1), name = "Transition\nProbability",
                      labels = percent_format(accuracy = 1)) +
  labs(
    title = "Plant Life-Stage Transition Matrix (t to t+2)",
    x = "State in Year t + 2",
    y = "State in Year t"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10)
  )


##### STATE TRANSITION DIAGRAM ####


library(tidyverse)
library(lme4)
library(lmerTest) 
library(dplyr)


# Load and clean data
df3 <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

# Transition pairs
transitions <- df3 %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,
    state_t1 = lead(state)
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  filter(!is.na(state_t), !is.na(state_t1)) %>%
  ungroup()


# Calculating transition probabilities
transition_matrix <- transitions %>%
  count(state_t, state_t1) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()


#install.packages("igraph")
library(igraph)


edges <- transition_matrix %>%
  transmute(
    from = state_t,
    to = state_t1,
    probability = probability,
    percent = probability * 100
  )

g <- graph_from_data_frame(edges)

E(g)$weight <- edges$percent

# The easiest way to plot the transition diagram
plot(g)


# The prettier way
plot(
  g,
  main = "State transition diagram",
  
  # vertices (tips of the graph)
  vertex.size = 40,
  vertex.color = c("palegreen", "#636363", "brown",
                   "#b641b8", "#DADADA", "palegreen4"),
  vertex.frame.color = "grey30",
  vertex.label.cex = 1.5,
  vertex.label.color = "black",
  
  # edges and arrows
  edge.width = E(g)$percent/6 + 1,
  edge.color = "grey70",
  edge.arrow.size = 0.2,
  edge.curved = 0.2,
  
  # labels
  edge.label = paste0(round(E(g)$percent,1), "%"),
  edge.label.cex = 0.9,
  edge.label.color = "grey20",
  
  # layout
  layout =  layout_in_circle(g),
  xlim = c(-1.2, 1.2), #Limits are necessary or some of the loops will be cut off
  ylim = c(-1.2, 1.2),
)


##### STATE TRANSITION DIAGRAM WITHOUT UNKNOWN ####


# Transition pairs
transitions <- df3 %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,
    state_t1 = lead(state)
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  filter(!is.na(state_t), !is.na(state_t1),
         !state_t %in% c("unknown"),
         !state_t1 %in% c("unknown")) %>%
  ungroup()


# Calculating transition probabilities
transition_matrix <- transitions %>%
  count(state_t, state_t1) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()


#install.packages("igraph")
library(igraph)


edges <- transition_matrix %>%
  transmute(
    from = state_t,
    to = state_t1,
    probability = probability,
    percent = probability * 100
  )

g <- graph_from_data_frame(edges)

E(g)$weight <- edges$percent


# The prettier way
plot(
  g,
  main = "State transition diagram",
  
  # vertices (tips of the graph)
  vertex.size = 40,
  vertex.color = c("palegreen", "#636363", "brown",
                   "#b641b8", "palegreen4"),
  vertex.frame.color = "grey30",
  vertex.label.cex = 1.5,
  vertex.label.color = "black",
  
  # edges and arrows
  edge.width = E(g)$percent/6 + 1,
  edge.color = "grey70",
  edge.arrow.size = 0.2,
  edge.curved = 0.2,
  
  # labels
  edge.label = paste0(round(E(g)$percent,1), "%"),
  edge.label.cex = 0.9,
  edge.label.color = "grey20",
  
  # layout
  layout =  layout_in_circle(g),
  xlim = c(-1.2, 1.2), #Limits are necessary or some of the loops will be cut off
  ylim = c(-1.2, 1.2),
)



##### STATE TRANSITION DIAGRAM WITHOUT UNKNOWN AND DEAD ####

# Transition pairs
transitions <- df3 %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  mutate(
    # Adding current and following state
    state_t = state,
    state_t1 = lead(state)
  ) %>%
  # We can only keep the pairs where we know both the start and end state 
  filter(!is.na(state_t), !is.na(state_t1),
         !state_t %in% c("unknown", "dead"),
         !state_t1 %in% c("unknown", "dead")) %>%
  ungroup()


# Calculating transition probabilities
transition_matrix <- transitions %>%
  count(state_t, state_t1) %>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup()


#install.packages("igraph")
library(igraph)


edges <- transition_matrix %>%
  transmute(
    from = state_t,
    to = state_t1,
    probability = probability,
    percent = probability * 100
  )

g <- graph_from_data_frame(edges)

E(g)$weight <- edges$percent

# The easiest way to plot the transition diagram
plot(g)


# The prettier way
plot(
  g,
  main = "State transition diagram",
  
  # vertices (tips of the graph)
  vertex.size = 40,
  vertex.color = c("palegreen", "brown",
                   "#b641b8", "palegreen4", "#636363"),
  vertex.frame.color = "grey30",
  vertex.label.cex = 1.5,
  vertex.label.color = "black",
  
  # edges and arrows
  edge.width = E(g)$percent/7 + 1,
  edge.color = "grey70",
  edge.arrow.size = 0.2,
  edge.curved = 0.2,
  
  # labels
  edge.label = paste0(round(E(g)$percent,1), "%"),
  edge.label.cex = 0.9,
  edge.label.color = "grey20",
  
  # layout
  layout =  layout_in_circle(g),
  xlim = c(-1.3, 1.3), #Limits are necessary or some of the loops will be cut off
  ylim = c(-1.3, 1.3),
)






##### ALLUVIAL PLOT ####

library(tidyr)
library(ggalluvial)
library(tidyverse)
library(lme4)
library(lmerTest) 
library(dplyr)
library(ggplot2)
library(stringr)

# Load and clean data
df_alluvial <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh) | (!is.na(flower_nr) & flower_nr > 0)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

state_levels <- c("1-leaf", "vegetative", "flowering",
                  "dormant", "unknown", "dead")


df2_alluvial <- df_alluvial %>%
  select(ID, year_num, state) %>%
  pivot_wider(names_from = year_num,
              values_from = state)


# Let's follow the life-cycles of the original plants 
# that were present from the beginning to the end

# Fixating the state levels on the graph (ordering everything other than ID by that)
# and filtering out only the "original" plants
df2_alluvial <- df2_alluvial %>%
  mutate(across(-ID, ~factor(.x, levels = state_levels))) %>%
  filter(!is.na(`2022`),
         str_detect(ID, "^[A-Z]\\d+$"))

ggplot(data = df2_alluvial, aes(
  axis1 = `2022`, axis2 = `2023`, axis3 = `2024`,
  axis4 = `2025`, axis5 = `2026`)) +
  geom_alluvium(aes(fill = `2026`), curve_type = "linear") + 
  geom_stratum(width = 0.4) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("2022", "2023", "2024", "2025", "2026")) +
  theme_minimal() + labs(x="Year", y = "Number of plants", title = "State Transitions of the Original Plants")



##### HEIGHT OF 1-LEAF JUVENILES AND 1-LEAF ADULTS ####

library(tidyverse)
library(lme4)
library(lmerTest) 

# Load and clean data
df2_states <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()

# Let's create a binary dormancy variable
# We'll keep the year that a certain plant was first declared dead
# but not the following years
df2 <-  df2_states %>%
  mutate(
    is_dormant = ifelse(state == "dormant", 1, 0)
  ) %>%
  group_by(ID)%>%
  mutate(
    death_yr = if(any(state == "dead")) {
      min(year_num[state == "dead"]) }
    else {
      Inf
    }
  ) %>%
  ungroup() %>%
  filter(year_num <= death_yr)

# 1-leaved plants
df_1leaf <- df2 %>% filter(leaf_count == 1)

summary(df_1leaf$h1)

# Categorize 1-leaf plants into <20 cm (<200 mm) vs >=20 cm (>=200 mm)
df2 <- df2 %>%
  mutate(
    leaf1_cat = case_when(
      leaf_count == 1 & h1 < 200 ~ "1leaf_<20cm",
      leaf_count == 1 & h1 >= 200 ~ "1leaf_>=20cm",
      leaf_count == 0 ~ "dormant",
      leaf_count > 1 ~ "multi_leaf",
      TRUE ~ "other"
    )
  )

# transition from 1-leaf categories in year t to t+1
df_leaf_t <- df2 %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaf1_cat_t = leaf1_cat,
    flowering_t1 = lead(flowering),
    leaf_count_t1 = lead(leaf_count)
  ) %>%
  ungroup() %>%
  filter(!is.na(leaf1_cat_t), !is.na(flowering_t1))

# 1-leaf height impact on  flowering ----

# Firth’s penalized logistic regression (logistf) to fix the statistical error 
# caused by complete separation (zero flowering events in the under-20 cm group)

library(logistf)
firth_1leaf <- logistf(
  flowering_t1 ~ leaf1_cat_t + year, 
  data = df_leaf_t %>% filter(leaf1_cat_t %in% c("1leaf_<20cm", "1leaf_>=20cm"))
)
summary(firth_1leaf)

# INTERPRETATION:

# .......................................................
# OVERALL MODEL FIT

# • Likelihood Ratio Test: Chisq = 85.73, df = 4, p < 0.0001
# • INTERPRETATION: The overall model is highly statistically significant,
#   indicating that 1-leaf height category and transition year are important
#   predictors of flowering in the following year.


# .......................................................
# LEAF HEIGHT CATEGORY EFFECT (>= 20 cm vs < 20 cm)

# • 1leaf_>=20cm vs 1leaf_<20cm (Baseline):
#   - Estimate = 4.7917 (SE = 1.3944, Chisq = 53.55, p = 2.52e-13)
#   - Odds Ratio (OR) = exp(4.7917) ≈ 120.4
#   - 95% CI for OR ≈ exp(2.7974) – exp(9.6404) ≈ 16.4 – 15,340
#   - INTERPRETATION: Highly statistically significant (p < 0.001). Holding
#     transition year constant, 1-leaf plants with a leaf height of at least
#     20 cm had approximately 120 times higher odds of flowering in the
#     following year than plants with leaves shorter than 20 cm.


# .......................................................
# TRANSITION YEAR EFFECTS (2022 -> 2023)

# • Year 2023 (Transition 2023 -> 2024):
#   - Estimate = -1.7163 (SE = 0.7420, Chisq = 6.41, p = 0.0113)
#   - Odds Ratio (OR) = exp(-1.7163) ≈ 0.180
#   - 95% CI for OR ≈ exp(-3.4303) – exp(-0.3658) ≈ 0.032 – 0.693
#   - INTERPRETATION: Statistically significant (p = 0.011). The odds of a
#     1-leaf plant flowering in the following year were approximately 82.0%
#     lower during the 2023→2024 transition than during the baseline
#     2022→2023 transition.


# • Year 2024 (Transition 2024 -> 2025):
#   - Estimate = -0.7034 (SE = 0.4593, Chisq = 2.34, p = 0.1261)
#   - Odds Ratio (OR) = exp(-0.7034) ≈ 0.495
#   - 95% CI for OR ≈ exp(-1.6335) – exp(0.1967) ≈ 0.195 – 1.217
#   - INTERPRETATION: Not statistically significant (p = 0.126). There was
#     no evidence that the probability of flowering differed between the
#     2024→2025 transition and the baseline 2022→2023 transition.


# • Year 2025 (Transition 2025 -> 2026):
#   - Estimate = -1.0707 (SE = 0.4322, Chisq = 6.38, p = 0.0115)
#   - Odds Ratio (OR) = exp(-1.0707) ≈ 0.343
#   - 95% CI for OR ≈ exp(-1.9444) – exp(-0.2369) ≈ 0.143 – 0.789
#   - INTERPRETATION: Statistically significant (p = 0.012). The odds of a
#     1-leaf plant flowering in the following year were approximately 65.7%
#     lower during the 2025→2026 transition than during the baseline
#     2022→2023 transition.

# plants that go BACK to 1-leaf from flowering/multi-leaf states ----
df_reversion <- df2 %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    prev_leaf_count = lag(leaf_count),
    prev_flowering = lag(flowering)
  ) %>%
  ungroup() %>%
  filter(leaf_count == 1 & prev_leaf_count > 1)

# Summary of h1 (mm) for plants reverting BACK to 1-leaf from multi-leaf/flowering
summary(df_reversion$h1)


# Histogram of 1-leaf heights with threshold
ggplot(df_1leaf, aes(
  x = h1 / 10, 
  fill = ifelse(h1 < 200, "Babies/Offspring (< 20 cm)", "Adult Reversions (≥ 20 cm)")
)) +
  geom_histogram(binwidth = 2, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = c("Babies/Offspring (< 20 cm)" = "palegreen3", "Adult Reversions (≥ 20 cm)" = "darkgreen")) +
  labs(
    title = "1-Leaf Height Distribution (20 cm Threshold)",
    x = "First Leaf Height (cm)",
    y = "Count",
    fill = "Category"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )


# Let's see if the odds could be due to there being much less 1-leafs
df_leaf_t %>%
  filter(leaf1_cat_t %in% c("1leaf_<20cm", "1leaf_>=20cm")) %>%
  group_by(year) %>%
  summarise(
    n_1leaf = n(),
    flowering_next = sum(flowering_t1),
    flowering_prop = mean(flowering_t1)
  )

df_leaf_t %>%
  filter(leaf1_cat_t %in% c("1leaf_<20cm", "1leaf_>=20cm")) %>%
  count(year, leaf1_cat_t) %>%
  group_by(year) %>%
  mutate(prop = n / sum(n))


#..............................................................................
##### _________________ Vannikatse FLOWERING  _______________________ #####
#..............................................................................

library(tidyverse)
library(ggplot2)
library(stringr)
library(dplyr)

df_raw <- read_csv2("data_vannikatse.csv")

# load the data
df <- df_raw %>%
  mutate(
    tray   = str_extract(ID, "^[A-Za-z]"),   # extract tray column from the ID column 
    ID1 = str_extract(ID, "(?<=^[A-Za-z]).+") # extract the ID (numbers 1-30 in the first year, letters a-b... for side bulbs belonging to the motherplant)
  ) %>%
  mutate(year=as.factor(year),
         ID=as.factor(ID)) %>%
  mutate(leaf_count = rowSums(!is.na(across(c(h1, h2, h3, h4, h5)))),
         flower = as.integer(!is.na(genh)),
         sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)))


# --- (Year t to Year t+1) dataset ---

# years in chronological order
years <- sort(unique(df$year))
transitions <- list()

# pairing each plant's data in year t with its data in year t+1
for (i in 1:(length(years) - 1)) {
  y_curr <- years[i]
  y_next <- years[i+1]
  
  df_curr <- df %>% filter(year == y_curr)
  df_next <- df %>% filter(year == y_next)
  
  # Join current year and next year by plant ID
  merged <- inner_join(df_curr, df_next, by = "ID", suffix = c("_t", "_t1"))
  merged$year_t <- y_curr
  merged$year_t1 <- y_next
  
  transitions[[i]] <- merged
}

# year-to-year transitions in one data frame
df_trans <- bind_rows(transitions)

# binary indicator for whether the plant produced sidebulbs in year t
df_trans$has_sidebulbs_t <- ifelse(df_trans$sidebulbs_t > 0, 1, 0)

library(lme4)
df_trans$year_t <- as.factor(df_trans$year_t)

# filtering out 2022
df_trans_filtered <- df_trans %>% 
  filter(year_t != 2022)

# centering the leaf count around the mean of the new filtered dataset
df_trans_filtered <- df_trans_filtered %>% 
  mutate(leaf_count_t_centered = leaf_count_t - mean(leaf_count_t, na.rm = TRUE))

# year to factor (making 2023 the baseline)
df_trans_filtered$year_t <- as.factor(df_trans_filtered$year_t)


# .......................................................................
# ORIGINAL MODEL #####
# .......................................................................

# How do leaf count, flowering, and sidebulbs in year t
# affect the probability of flowering in year t+1?

# mixed model (GLMM) with a random intercept for 'ID' because the data
# contains repeated measurements on the same plants over multiple years. 
# Year effect included as a factor

library(lme4)

glmm_model_fixed <- glmer(
  flower_t1 ~ leaf_count_t_centered + flower_t + sidebulbs_t + year_t + (1 | ID), 
  data = df_trans_filtered, 
  family = binomial)
summary(glmm_model_fixed)


# Since the variance of the ID random effect is 0, we use standard logistic regression (GLM)
glm_model_fixed <- glm(
  flower_t1 ~ leaf_count_t_centered + flower_t + sidebulbs_t + year_t, 
  data = df_trans_filtered, 
  family = binomial)

summary(glm_model_fixed)
exp(coef(glm_model_fixed)) # Odds Ratios


# Model assumptions check
# (Response variable is binary data; binomial GLM)
res_flower_next <- simulateResiduals(glm_model_fixed)
# 1. Residual uniformity (If p > 0.05, assumption is OK)
testUniformity(res_flower_next)
# 2. Overdispersion (If p > 0.05, assumption is OK)
testDispersion(res_flower_next)
# 3. Excess zeros (If p > 0.05, assumption is OK)
testZeroInflation(res_flower_next)
# 4. Outliers (If p > 0.05, assumption is OK)
testOutliers(res_flower_next)
# 5. Multicollinearity (Low VIF values indicate no problematic multicollinearity)
check_collinearity(glm_model_fixed)
# 6. Random effect
# A GLMM with ID as a random intercept was initially fitted, but the ID
# random-effect variance was effectively zero, resulting in a singular fit.
# Therefore, a standard binomial GLM was used instead.

# Assumptions OK


# --- INTERPRETATION (baseline = 2023) ---

# leaf_count_t_centered (Estimate = 1.4190, p = 2.55e-12) -> Highly Significant (p < 0.001)
#    - Direction: Strongly Positive.
#    - Odds Ratio (OR): exp(1.4190) ≈ 4.13
#    - Meaning: Every additional leaf in year t is associated with approximately
#      4.1 times higher odds of flowering in year t+1, holding the other predictors constant.

# flower_t (Estimate = -2.5257, p = 6.42e-07) -> Highly Significant (p < 0.001)
#    - Direction: Strongly Negative.
#    - Odds Ratio (OR): exp(-2.5257) ≈ 0.080 (a 92.0% decrease in odds)
#    - Meaning: Plants that flowered in year t had approximately 92.0% lower odds
#      of flowering again in year t+1, holding the other predictors constant.

# sidebulbs_t (Estimate = 0.2847, p = 0.2270) -> Not Statistically Significant (p > 0.05)
#    - Direction: Positive, but not statistically significant.
#    - Odds Ratio (OR): exp(0.2847) ≈ 1.33 (a 32.9% increase in odds)
#    - Meaning: Each additional sidebulb produced in year t is associated with
#      approximately 33% higher odds of flowering in year t+1, but this effect
#      is not statistically significant (p = 0.227).

# by year (Baseline reference: 2023)

# year_t2024 (Estimate = 0.0966, p = 0.7491) -> Not Significant (p > 0.05)
#    - Meaning: Compared to the baseline year 2023, there was no statistically
#      significant difference in the odds of flowering in year t+1 for transitions
#      beginning in 2024, holding the other predictors constant.

# year_t2025 (Estimate = 0.9754, p = 0.000854) -> Highly Significant (p < 0.001)
#    - Direction: Strongly Positive.
#    - Odds Ratio (OR): exp(0.9754) ≈ 2.65 (a 165% increase in odds)
#    - Meaning: Compared to the baseline year 2023, the odds of flowering in year t+1
#      were approximately 2.7 times higher for transitions beginning in 2025,
#      holding the other predictors constant.




#### NEW MODELS ####

# Let's only keep original bulbs and bulbs with X
df_without_sides <- df_trans_filtered %>%
  filter(str_detect(ID, "^[A-Za-z](X)?[0-9]+$"))

# Let's evaluate the correlation between centered leaf count and flowering in t
cor.test(df_without_sides$leaf_count_t_centered,
         df_without_sides$flower_t,
         method = "pearson") 
# r = 0.854, p < 0.05 so we evaluated the effects in separate models to avoid overlapping


# Base model; all variables; new data (no sidebulbs)
glmm_model_fixed <- glmer(
  flower_t1 ~ leaf_count_t_centered + flower_t + sidebulbs_t + year_t + (1|ID), 
  data = df_without_sides , 
  family = binomial)

summary(glmm_model_fixed)
exp(fixef(glmm_model_fixed)) # Odds ratios


#### MODEL 1: WITHOUT FLOWERING_T ####

glmm_model_fixed <- glmer(
  flower_t1 ~ leaf_count_t_centered + sidebulbs_t + year_t + (1|ID), 
  data = df_without_sides , 
  family = binomial)
summary(glmm_model_fixed)

# Singular -> let's make a glm instead
glm_model_fixed <- glm(
  flower_t1 ~ leaf_count_t_centered + sidebulbs_t + year_t, 
  data = df_without_sides , 
  family = binomial)
summary(glm_model_fixed)

# INTERPRETATION:

# Centered leaf count in t had a statistically significant (p=0.03<0.05) positive effect on flowering in t+1 
# each new leaf is associated with exp(0.207) = 1.23 times higher odds of flowering in t+1

# Sidebulb production is not significant p=0.51.
# OR = exp(0.1481) = 1.160

# Compared with the base year 2023, flowering probability did not differ
# significantly (p=0.42) in 2024
# OR = exp(0.2506) = 1.285

# Compared with the base year 2023, flowering probability was 
# significantly (p=0.001) higher in 2025.
# Flowering odds were exp(1.0074) = 2.74 times higher



# Model assumptions check
# (Response variable is binary data; binomial GLM)
res_flower_next <- simulateResiduals(glm_model_fixed)
# 1. Residual uniformity (If p > 0.05, assumption is OK)
testUniformity(res_flower_next)
# 2. Overdispersion (If p > 0.05, assumption is OK)
testDispersion(res_flower_next)
# 3. Excess zeros (If p > 0.05, assumption is OK)
testZeroInflation(res_flower_next)
# 4. Outliers (If p > 0.05, assumption is OK)
testOutliers(res_flower_next)
# 5. Multicollinearity (Low VIF values indicate no problematic multicollinearity)
check_collinearity(glm_model_fixed)
# Assumptions OK


#### MODEL 2: WITHOUT LEAF_COUNT_T_CENTERED ####

glmm_model_fixed <- glmer(
  flower_t1 ~ flower_t + sidebulbs_t + year_t + (1|ID), 
  data = df_without_sides , 
  family = binomial)

summary(glmm_model_fixed)
exp(fixef(glmm_model_fixed)) # Odds ratios

# INTERPRETATION

# Flowering in year t had a significant (p=0.04 < 0.05) effect on flowering in t+1.
# Plants flowering in t had 1-exp(-0.954) = 1-0.385 = 0.615 62% lower odds of flowring in t+1

# Sidebulb production had a non-significant (p=0.176) positive effect of exp(0.374) = 1.454 times higher flowering odds

# Flowering odds did not differ significantly in 2024 (p=0.957)

# Flowering odds were significantly (p=0.006) higher for transitions beginning in 2025.
# OR = exp(0.96694) = 2.63


# ASSUMPTIONS CHECK 
# (Response variable is binary data; binomial GLMM)
res_flower_next <- simulateResiduals(glmm_model_fixed)
# 1. Residual uniformity (If p > 0.05, assumption is OK)
testUniformity(res_flower_next)
# 2. Overdispersion (If p > 0.05, assumption is OK)
testDispersion(res_flower_next)
# 3. Excess zeros (If p > 0.05, assumption is OK)
testZeroInflation(res_flower_next)
# 4. Outliers (If p > 0.05, assumption is OK)
testOutliers(res_flower_next)
# 5. Multicollinearity (Low VIF values indicate no problematic multicollinearity)
check_collinearity(glmm_model_fixed)
# OK





#### MODEL WITH SIDEBULBS; ACTIVE PLANTS WITH l1*w1 ####

df_active <- df_trans_filtered %>%
  select(flower_t1, flower_t, l1_t, w1_t, leaf_count_t, leaf_count_t_centered, ID, year_t) %>%
  filter(leaf_count_t > 0) %>%
  mutate(area = l1_t*w1_t,
         log_area_scaled = as.numeric(scale(log(area))))

# Let's see if it is skewed
hist(df_active$area,
     breaks = 30,
     main = "Distribution of leaf area",
     xlab = "Leaf area (mm²)")
# It is VERY right-skewed (has a long tail to the right side)
# So we should use scaled log(area)

# Let's check that the leaf "area" is not highly correlated with count or flowering
cor.test(df_active$flower_t, df_active$log_area_scaled, method = "pearson")
# r = -0.597, p<0.05 
cor.test(df_active$leaf_count_t_centered, df_active$log_area_scaled, method = "pearson")
# r = -0.497, p<0.05 
cor.test(df_active$leaf_count_t_centered, df_active$flower_t)
# r = 0.905, extremely problematic correlation

library(lme4)

# So let's make a model only about the effect of leaf "area"
# And since area is skewed, lets use the log scaled area value
glmm_model_active <- glmer(
  flower_t1 ~ log_area_scaled + year_t + (1|ID), 
  data = df_active, 
  family = binomial)

summary(glmm_model_active)
# Odds Ratios
exp(fixef(glmm_model_active))


# Model assumptions
# Response variable is binary
res <- simulateResiduals(glmm_model_active)
# 1. Residual uniformity
testUniformity(res)
# 2. Overdispersion
testDispersion(res)
# 3. Zero-inflation
testZeroInflation(res)
# 4. Outliers
testOutliers(res)
# 5. Multicollinearity
check_collinearity(glmm_model_active)
# 6. Random effect / model fit
isSingular(glmm_model_active)
# Assumptions OK


# INTERPRETATION; veryunintuitive due to the log(area)

# Log-transformed leaf area had a statistically significant positive effect
# on flowering in year t+1 (p < 0.001).
# A one-standard-deviation increase in log-transformed leaf area was associated
# with exp(1.343) = 3.83 times higher odds of flowering in year t+1

# Compared with the baseline year 2023, flowering odds did not differ
# significantly in 2024 (p = 0.540; OR = 1.28).
#
# Compared with 2023, flowering odds also did not differ significantly
# in 2025 (p = 0.113; OR = 1.96).


# Hypothetical predictions data frame
newdata <- data.frame(
  area = c(100, 500, 1000, 1500, 2000),
  year_t = factor("2023", levels = levels(df_active$year_t))
)

# Standardizing the log(area) values with the mean and sd of log(area)
newdata$log_area_scaled <- 
  (log(newdata$area) - mean(log(df_active$area), na.rm = TRUE)) /
  sd(log(df_active$area), na.rm = TRUE)

newdata$predicted_prob <- predict(
  glmm_model_active,
  newdata = newdata,
  type = "response", # for getting probabilities instead of log-odds
  re.form = NA
)

# As a table
newdata

# As a plot
ggplot(newdata, aes(x = area, y = predicted_prob)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Leaf area (mm²)",
    y = "Predicted probability of flowering in year t+1",
    title = "Predicted flowering probability by leaf area"
  ) +
  theme_bw()



#..............................................................................
##### _________________ Vannikatse DORMANCY  _______________________ #####
#..............................................................................



library(tidyverse)
library(lme4)
library(lmerTest) 

# Load and clean data
df <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray = str_extract(ID, "^[A-Za-z]"),
    tray = factor(tray),
    year_num = as.numeric(year),
    year = factor(year),
    ID = factor(ID),
    leaf_count = rowSums(!is.na(select(., h1, h2, h3, h4, h5))),
    flowering = as.integer(!is.na(genh)),
    sidebulbs = ifelse(is.na(`sidebulb_nr`), 0, as.numeric(`sidebulb_nr`)),
    grew_sidebulbs = ifelse(sidebulbs > 0, 1, 0),
  ) %>%
  arrange(ID, year_num) %>%
  group_by(ID) %>%
  mutate(
    leaves_before = lag(cumany(leaf_count > 0), default = FALSE),
    leaves_after  = rev(lag(cumany(rev(leaf_count > 0)), default = FALSE)),
    
    state = case_when(
      flowering == 1 ~ "flowering",
      leaf_count == 1 ~ "1-leaf",
      leaf_count >= 2 ~ "vegetative",
      
      # Dormancy conditions
      leaf_count == 0 & leaves_before & leaves_after ~ "dormant",
      leaf_count == 0 & leaves_after ~ "dormant", # That's for A24 which didn't appear the 1st year, counted it as dormant, hoping it's not a sidebulb that appeared
      
      # Death contiton
      leaf_count == 0 & lead(leaf_count, 1)== 0 & lead(leaf_count, 2) == 0 & !leaves_after~ "dead", 
      
      # The rest will be unknown
      leaf_count == 0 ~ "unknown"
    )
  ) %>%
  # Let's mark the rest of the years of death as "dead" too
  mutate(
    dead_from_here = cumany(state == "dead"),
    state = if_else(dead_from_here, "dead", state)
  ) %>%
  ungroup()


dormant_plants <- df %>%
  filter(state == "dormant") %>%
  count(ID, tray, name = "dormant years")

count(dormant_plants)
sum(dormant_plants$`dormant years`)
# there were 33 dormancy events from 25 different plants observed

dormant_plants %>%
  group_by(tray) %>%
  summarise(dormant_individuals = n())


#### PRESENCE-ABSCENSE TRAY MAP ####

library(stringr,dplyr)

# load the data
df <- read_csv2("data_vannikatse.csv") %>%
  mutate(
    tray   = str_extract(ID, "^[A-Za-z]"),   # extract tray column from the ID column 
    ID = str_extract(ID, "(?<=^[A-Za-z]).+") # extract the ID (numbers 1-30 in the first year, letters a-b... for side bulbs belonging to the motherplant)
  ) %>%
  mutate(year=as.factor(year),
         ID=as.factor(ID))

# the following is just to access the individual trays more easily
df_tray <- lapply(c("A", "B", "C"), function(x) {df[df$tray == x, ]})
names(df_tray) <- c("A","B","C")
df_tray$A

# make a presence/absence column that I'll call "pa"
df$pa <- ifelse(is.na(df$h1),0,1) # 0 means, that plant is absence; 1, that it is present

# 3 maps using ggplot and facet_grid(year~tray)
ggplot(df,aes(x = x, y = y, colour = factor(pa)))+
  geom_point()+
  geom_text(aes(label=as.character(ID)),hjust=-0.35, vjust=0.35)+
  xlim(0,500)+ ylim(0,700)+
  facet_grid(year~tray)+
  theme_bw() + theme(legend.position = "bottom")
