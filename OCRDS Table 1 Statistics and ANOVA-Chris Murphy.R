sessionInfo()

install.packages("lme4")
install.packages("lmerTest")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("tidyverse")
install.packages("effsize")
install.packages("boot")
install.packages("rcompanion")

library(effsize)
library(lme4)
library(lmerTest)
library(reshape2)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(readxl)
library(dplyr)


# Set feature to run

feat <- "depression_score"

setwd("C:/Users/owner/OCRDS")

# read file
short_df <- read_excel("OCRDS query output.xlsx", sheet = "depression")
short_df

long_df <- short_df[short_df$episodic_week %in% c(1, 6, 15),c("member_user_id","episodic_week",feat)]
long_df

# Remove missing values
long_df <- na.omit(long_df)
long_df

# Fit linear mixed model
equ <- paste0(feat," ~ week + (1 | user_id)")

## Type III anova table with p-values for F-tests based on Satterthwaite's
## method:
long_df$episodic_week <- factor(long_df$week)
## Fit linear mixed model
m <- lmer(eval(equ), data=long_df)

## Summary including coefficient table with p-values for t-statistics using
## Satterthwaite's method for denominator degrees of freedom:
summary(m)

## Type III anova table with p-values for F-tests based on Satterthwaite's ********************
## method:
anova(m,ddf="lme4")
m.test <- as(m,"lmerModLmerTest")
print(summary(m.test,ddf="Satterthwaite"),correlation=FALSE)

# ------------ plot
ggplot(long_df, aes_string("week",feat , fill="week")) +
  scale_x_discrete(labels=c(paste0(feat,"_start"), paste0(feat,"_mid"), paste0(feat,"_end")))+
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=1, width=0) +
  geom_smooth(method = "lm", se=TRUE, fullrange=TRUE, color="black", aes(group=1))
theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle(feat) +
  xlab("week")

############
# Calculate Hedge's G for the feature
no_mid <- long_df[long_df$week != 6, ]
no_mid$label <- ifelse(no_mid$week == 1, "start", ifelse(no_mid$week == 15, "end", NA))

df_filtered <- no_mid %>%
  group_by(member_user_id) %>%
  filter(any(label == "start") & any(label == "end"))

df_filtered <- df_filtered %>%
  ungroup()

df_filtered

cohen.d(df_filtered[[feat]], df_filtered$label, paired=TRUE, hedges.correction=TRUE)

#############
# Calculate mean (and std) number of therapy sessions completed in therapy

mean_sd_max_visits <- short_df %>%
  group_by(member_user_id) %>%
  summarize(max_visits = max(episode_visits)) %>%
  summarize(mean_max_visits = mean(max_visits),
            sd_max_visits = sd(max_visits))

mean_sd_max_visits

#############
# Calculate mean (and std) number of weeks in therapy

mean_sd_max_weeks <- short_df %>%
  group_by(member_user_id) %>%
  summarize(max_weeks = max(week)) %>%
  summarize(mean_max_weeks = mean(max_weeks),
            sd_max_weeks = sd(max_weeks))

mean_sd_max_weeks

#############
# Calculate mean change

df_calculated <- df_filtered %>%
  group_by(member_user_id) %>%
  summarize(week1_value = dass_depression[episodic_week == 1],
            week15_value = dass_depression[episodic_week == 15]) %>%
  mutate(average_change = week15_value - week1_value)

mean_value <- mean(df_calculated$average_change)
mean_value
mean_week1 <- mean(df_calculated$week1_value)
mean_end_value <- mean_week1 + mean_value
mean_end_value
mean_change <- (mean_value/mean_week1)*100
print(paste("Mean change:", mean_change, "%"))

# Counts for table 1
library(boot)

week1_df <- long_df[long_df$week == 1, ]
week1_df
week1_mean <- mean(week1_df[[feat]])
week1_mean
week1_sd <- sd(week1_df[[feat]])
week1_sd
column_data <- (week1_df[[feat]])
confidence_interval <- t.test(column_data)$conf.int
print(confidence_interval)

week6_df <- long_df[long_df$week == 6, ]
week6_df
week6_mean <- mean(week6_df[[feat]])
week6_mean
week6_sd <- sd(week6_df[[feat]])
week6_sd
column_data <- (week6_df[[feat]])
confidence_interval <- t.test(column_data)$conf.int
print(confidence_interval)


week15_df <- long_df[long_df$week == 15, ]
week15_df
week15_mean <- mean(week15_df[[feat]])
week15_mean
week15_sd <- sd(week15_df[[feat]])
week15_sd
column_data <- (week15_df[[feat]])
confidence_interval <- t.test(column_data)$conf.int
print(confidence_interval)

# Median confidence interval bootstrap
library(boot)

Mboot = boot(week1_df[[feat]],
             function(x,i) median(x[i]),
             R=10000)

boot.ci(Mboot,
        conf = 0.95,
        type = c("norm", "basic" ,"perc")
)

week1median <- median(week1_df[[feat]])

# Week 6
Mboot = boot(week6_df[[feat]],
             function(x,i) median(x[i]),
             R=1000)

boot.ci(Mboot,
        conf = 0.95,
        type = c("norm", "basic" ,"perc")
)

week6median <- median(week6_df[[feat]])

#Week 15
Mboot = boot(week15_df[[feat]],
             function(x,i) median(x[i]),
             R=10000)

boot.ci(Mboot,
        conf = 0.95,
        type = c("norm", "basic" ,"perc")
)

week15median <- median(week15_df[[feat]])

print(week1median)
print(week6median)
print(week15median)