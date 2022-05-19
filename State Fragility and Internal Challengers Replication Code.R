# Replication Code for "State Fragility and Internal Challengers"
# by M. Karstens and D. Lemke

# Required packages for replication - Please install if needed =================

library(broom)
library(survival)
library(plm)
library(tidyverse)
library(viridis)
library(ggplot2)
library(stargazer)
require(dplyr)

# Settings =====================================================================

options(scipen = 50) # bias against scientific notation for convenience

# Loading data =================================================================

data <- read.csv("State Fragility and Internal Challengers Data.csv")
pitf_hs <- read.csv("pitf-hs-comparison.csv")
hs_dat <- read.csv("Hanson-Sigman-Capacity-Data.csv")


names(data)

# Subset of Data with at Least 8 Cases Before and After Interruptions ==========

sub_dat <- filter(data,
  ccode != 90,  # Guatemala
  ccode != 100, # Colombia
  ccode != 372, # Georgia
  ccode != 432, # Mali
  ccode != 451, # Sierra Leone
  ccode != 475, # Nigeria
  ccode != 483, # Chad
  ccode != 490, # DRC
  ccode != 500, # Uganda
  ccode != 501, # Kenya
  ccode != 520, # Somalia
  ccode != 530, # Ethiopia
  ccode != 560, # S. Africa
  ccode != 565, # Namibia
  ccode != 645, # Iraq
  ccode != 660, # Lebanon
  ccode != 750, # India
  ccode != 770, # Pakistan
  ccode != 775, # Burma
  ccode != 811, # Cambodia
  ccode != 817, # S. Vietnam
  ccode != 840) # Philippines

target <-
  c(
    90,
    100,
    372,
    432,
    451,
    475,
    483,
    490,
    500,
    501,
    520,
    530,
    560,
    565,
    645,
    660,
    750,
    770,
    775,
    811,
    817,
    840
  )

sub_dat2 <- filter(data, ccode %in% target)

# Tables & Figures =============================================================

# Table 1 - Main Analysis ----------------------------------------------------

main <- plm(
  fragility ~ num_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

main.sub <- plm(
  fragility ~ num_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

summary(main)
summary(main.sub)

stargazer(
  main,
  main.sub,
  type = "html",
  title = "Model Results with Two-Way Fixed Effects",
  model.numbers = F,
  column.labels = c("All States", "Subset"),
  dep.var.labels = c("Fragility"),
  covariate.labels = c(
    "Number of TCs",
    "Liberal Democracy Index",
    "Civil War",
    "Interstate War",
    "Fragility (t-1)"
  ),
  keep.stat = c("n"),
  out = "mainmod.htm"
)

# Table 2 - Survival Model ---------------------------------------------------

temp <- filter(data, !is.na(fail))
surv_elapse <- Surv(temp$alt_start, temp$alt_stop, temp$fail)

surv_test <- coxph(
  surv_elapse ~ fragility + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility +
    cluster(ccode),
  data = temp,
  method = "efron"
)

summary(surv_test)

stargazer(
  surv_test,
  type = "html",
  title = "Effect of Fragility on Risk of State Failure",
  column.labels = "State Failure",
  covariate.labels = c(
    "Fragility",
    "Democracy (VDem)",
    "Civil War",
    "Interstate War",
    "Fragility (t-1)"
  ),
  single.row = T,
  out = "conclusion.htm"
)


# Figure 1 - Heat Map created manually on mapchart.net -----------------------
# Figure 2 - Fragility by UN Sub-Region --------------------------------------

mean_dat <- group_by(data, un_region, year) %>%
  summarise(fragility = mean(fragility, na.rm = TRUE))

ggplot(na.omit(mean_dat), aes(x = year, y = fragility, colour = un_region)) +
  geom_point() +
  geom_line() +
  scale_color_viridis(discrete = T, option = "H") +
  labs(x = "Year", y = "Fragility", color = "Region") +
  theme_minimal() +
  theme(text = element_text(size = 12, family = "serif"))

# Figure 3 - Coefficient Plot of Main Model ----------------------------------

coefs_main <- tidy(main, conf.int = TRUE, exponentiate = F)

coefs_main$variable <- c(
  "Number of TCs",
  "Liberal Democracy Index",
  "Civil War",
  "Interstate War",
  "Fragility (t-1)"
)

coef_sub <- tidy(main.sub, conf.int = TRUE, exponentiate = F)
coef_sub$variable <- c(
  "Number of TCs",
  "Liberal Democracy Index",
  "Civil War",
  "Interstate War",
  "Fragility (t-1)"
)


all_plot <- data.frame(
  variable = coefs_main$variable,
  beta = coefs_main$estimate,
  se = summary(main)$coef[, 2],
  tc = "All Cases"
)

all_plot$variable <- factor(all_plot$variable,
  levels = unique(as.character(all_plot$variable))
)

sub_plot <- data.frame(
  variable = coef_sub$variable,
  beta = coef_sub$estimate,
  se = summary(main.sub)$coef[, 2],
  tc = "Subset"
)


allmod <- data.frame(rbind(all_plot, sub_plot))

allmod$variable <- factor(allmod$variable,
  levels = unique(as.character(allmod$variable))
)

submod <- filter(allmod, variable != "Fragility (t-1)")

submod$variable <- factor(submod$variable,
  levels = unique(as.character(submod$variable))
)

interval1 <- -qnorm((1 - 0.90) / 2) # 90 % multiplier
interval2 <- -qnorm((1 - 0.95) / 2) # 95 % multiplier


submod$tc <- factor(submod$tc,
  levels = c("All Cases", "Subset")
)

sub_coef <- ggplot(submod, aes(colour = tc))
sub_coef <-
  sub_coef + scale_color_viridis_d(
    option = "D",
    begin = 0.2,
    end = .8,
    breaks = c("All Cases", "Subset"),
    name = ""
  )
sub_coef <-
  sub_coef + geom_hline(
    yintercept = 0,
    colour = gray(1 / 2),
    lty = 2
  )
sub_coef <- sub_coef + geom_linerange(
  aes(
    x = variable,
    ymin = beta - se * interval1,
    ymax = beta + se * interval1
  ),
  lwd = 1.5,
  position = position_dodge(width = 1 / 2)
)
sub_coef <- sub_coef + geom_pointrange(
  aes(
    x = variable,
    y = beta,
    ymin = beta - se * interval2,
    ymax = beta + se * interval2,
    size = 2
  ),
  lwd = 1,
  shape = 18,
  position = position_dodge(width = 1 / 2)
)

sub_coef <- sub_coef + xlim(rev(levels(submod$variable))) +
  coord_flip() + theme_minimal()
sub_coef <- sub_coef + labs(y = "Coefficient Estimate")
sub_coef <- sub_coef + theme(
  axis.title.y = element_blank(),
  axis.title.x = element_text(family = "serif", size = 14),
  axis.text.y = element_text(family = "serif", size = 14),
  legend.position = c(.3, .3),
  legend.title = element_text(family = "serif", size = 16),
  legend.text = element_text(family = "serif", size = 14)
)

print(sub_coef)




# Non-Figure/Table Analyses in Main Text =======================================

# Correlation of PITF fragility score and Hanson and Sigman - page 12 =======

cor(pitf_hs$hs_fragility,
  pitf_hs$pitf_sf,
  use = "complete.obs",
  method = "pearson")

cor(pitf_hs$hs_fragility,
  pitf_hs$pitf_effect,
  use = "complete.obs",
  method = "pearson")

cor(pitf_hs$hs_fragility,
  pitf_hs$pitf_legit,
  use = "complete.obs",
  method = "pearson")

# Comparing fragility in developed vs. developing world - footnote 9 ========

glob_avg <- hs_dat %>%
  group_by(year) %>%
  summarize(glob_avg = mean(fragility, na.rm = T))

# UN developed economies

dev <- c(
  305,
  211,
  390,
  375,
  220,
  255,
  350,
  205,
  325,
  212,
  210,
  235,
  230,
  380,
  200,
  355,
  344,
  352,
  316,
  366,
  310,
  367,
  368,
  338,
  290,
  360,
  317,
  349,
  395,
  385,
  225,
  900,
  20,
  740,
  920,
  2
)

developed <- filter(hs_dat, ccode %in% dev)

# UN Economies in Transition

tran <- c(
  339,
  346,
  341,
  345,
  371,
  373,
  370,
  372,
  705,
  703,
  359,
  365,
  702,
  701,
  369,
  704
)

transition <- filter(hs_dat, ccode %in% tran)

# UN Developing Economies

developing <- filter(hs_dat, !ccode %in% tran)
developing <- filter(developing, !ccode %in% dev)

# Compare Averages

DevelAvg <- developed %>%
  group_by(year) %>%
  summarize(devel_avg = mean(fragility, na.rm = T))

UndevAvg <- developing %>%
  group_by(year) %>%
  summarize(undev_avg = mean(fragility, na.rm = T))

TransAvg <- transition %>%
  group_by(year) %>%
  summarize(trans_avg = mean(fragility, na.rm = T))

Avs <- left_join(glob_avg, DevelAvg)
Avs <- left_join(Avs, UndevAvg)
Avs <- left_join(Avs, TransAvg)

Avs <- Avs %>% filter(year < 2011)

mean(Avs$glob_avg)
mean(Avs$devel_avg)
mean(Avs$undev_avg)
mean(Avs$trans_avg)

# Average Fragility of full sample versus subset - page 18 -------------------

summary(data$fragility) # 0.146
summary(sub_dat$fragility) # -0.036
summary(sub_dat2$fragility) # 0.464



# Appendices ===================================================================

# Appendix A - Alternative Specifications ======================================
# A.1 Unit Fixed Effects Only ----

no.tfe <- plm(
  fragility ~ num_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "individual",
  data = data
)

summary(no.tfe)


no.tfe.sub <- plm(
  fragility ~ num_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "individual",
  data = sub_dat
)

summary(no.tfe.sub)

stargazer(
  no.tfe,
  no.tfe.sub,
  type = "latex",
  title = "Model Results with Unit Fixed Effects Only",
  model.numbers = F,
  column.labels = c(
    "All States",
    "Subset"
  ),
  dep.var.labels = c("Fragility"),
  covariate.labels = c(
    "Number of TCs",
    "Liberal Democracy Index",
    "Civil War",
    "Interstate War",
    "Fragility (t-1)"
  ),
  keep.stat = c("n")
)

# A.2 Running Counter of Total TCs - Permanent Effect ----

running <- plm(
  fragility ~ tc_tally + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

summary(running)



running.sub <- plm(
  fragility ~ tc_tally + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

summary(running.sub)

stargazer(
  running,
  running.sub,
  type = "latex",
  title = "Model Results with Running TC Count",
  model.numbers = F,
  column.labels = c(
    "All States",
    "Subset"
  ),
  dep.var.labels = c("Fragility"),
  covariate.labels = c(
    "Number of TCs Since 1960",
    "Liberal Democracy Index",
    "Civil War",
    "Interstate War",
    "Fragility (t-1)"
  ),
  keep.stat = c("n")
)

# A.3 Initial Year of Emergence Only - Shock Effect ----

shock <- plm(
  fragility ~ new_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = data
)

summary(shock)



shock.sub <- plm(
  fragility ~ new_tc + lib_dem_vdem
    + cowintraongoing + cow_inter + lag_fragility,
  index = c("state_name", "year"),
  model = "within",
  effect = "twoways",
  data = sub_dat
)

summary(shock.sub)

stargazer(
  shock,
  shock.sub,
  type = "latex",
  title = "Model Results with Only TC Emergence",
  model.numbers = F,
  column.labels = c(
    "All States",
    "Subset"
  ),
  dep.var.labels = c("Fragility"),
  covariate.labels = c(
    "TC Emerge",
    "Liberal Democracy Index",
    "Civil War",
    "Interstate War",
    "Fragility (t-1)"
  ),
  keep.stat = c("n")
)






# Appendix B - Information on Subset ===========================================
# B.1 - Subset Summary Statistics ----

table(data$state_name)
table(sub_dat$state_name)
table(sub_dat2$state_name)

summary(data$fragility)
summary(sub_dat$fragility)
summary(sub_dat2$fragility)

summary(data$lib_dem_vdem)
summary(sub_dat$lib_dem_vdem)
summary(sub_dat2$lib_dem_vdem)

table(data$cowintraongoing)
table(sub_dat$cowintraongoing)
table(sub_dat2$cowintraongoing)

table(data$cow_inter)
table(sub_dat$cow_inter)
table(sub_dat2$cow_inter)



# B.2 - List of states included in subset ------------------------------------
table(sub_dat$state_name)
# B.3 - List of states excluded in subset ------------------------------------
table(sub_dat2$state_name)



# Appendix C - Fragility Data ==================================================
# C.1 Hanson and Sigman Components ----

# Correlation of Hanson and Sigman with V-Dem territorial control

plot(data$num_tc, exp(data$v2terr))
plot(data$num_tc, exp(data$v2terr))

cor(data$num_tc, data$v2terr, use = "complete.obs", method = "pearson")
cor(data$num_tc,
  exp(data$v2terr),
  use = "complete.obs",
  method = "pearson"
)

# C.2 Comparing Fragility of developed and undeveloped states ----
# NOTE: Please run main analysis section prior to running this section
plot_dat <- Avs %>%
  dplyr::select(year, glob_avg, devel_avg, undev_avg, trans_avg) %>%
  gather(key = "Countries", value = "Fragility", -year)

ggplot(plot_dat, aes(x = year, y = Fragility)) +
  geom_line(size = .9, aes(color = Countries, linetype = Countries)) +
  scale_color_manual(
    values = c("darkred", "steelblue", "orange", "green"),
    labels = c(
      "Developed Countries",
      "All Countries",
      "Transitioning Countries",
      "Developing Countries"
    )
  ) +
  scale_linetype_discrete(
    labels = c(
      "Developed Countries",
      "All Countries",
      "Transitioning Countries",
      "Developing Countries"
    )
  ) +
  theme_minimal() +
  labs(x = "\nYear", y = "Average Fragility\n", title = "") +
  theme(
    text = element_text(size = 12, family = "serif"),
    legend.position = c(.8, .4),
    legend.title = element_blank()
  )



# C.3 Fragility by General Region ----

mean_dat <- group_by(data, region2, year) %>%
  summarise(fragility = mean(fragility, na.rm = TRUE))

ggplot(na.omit(mean_dat), aes(x = year, y = fragility, colour = region2)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Fragility", color = "Region") +
  theme_minimal() +
  theme(
    text = element_text(size = 12, family = "serif"),
    legend.position = c(.8, .85),
  )

