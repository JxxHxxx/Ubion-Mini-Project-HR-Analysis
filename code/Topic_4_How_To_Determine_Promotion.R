install.packages('dplyr')
library(readxl)
library(dplyr)
library(ggplot2)
hr <- read.csv('HR_comma_sep.csv')
# 승진의 기준에 대한 고찰
View(hr_df)
hr_df <- hr

# 승진여부로 구분된 집단의 인사고과 평균 비교
hr_df_eva <- hr_df %>%
  group_by(promotion_last_5years) %>% 
  summarise(mean_eva = mean(last_evaluation))
hr_df_eva
ggplot(data = hr_df_eva, aes(x = promotion_last_5years, y=mean_eva))+geom_col()

# 승진여부로 구분된 집단의 프로젝트 수의 평균 비교
hr_df_proj <- hr_df %>% 
  group_by(promotion_last_5years) %>% 
  summarise(mean_proj = mean(number_project))
hr_df_proj
ggplot(data = hr_df_proj, aes(x=promotion_last_5years, y = mean_proj))+geom_col()

# 승진여부로 구분된 집단의 월 평균 근무 시간
hr_df_hours <- hr_df %>% 
  group_by(promotion_last_5years) %>% 
  summarise(mean_hours = mean(average_montly_hours))
hr_df_hours
ggplot(data = hr_df_hours, aes(x = promotion_last_5years, y = mean_hours))+geom_col()
hr_df_eva;hr_df_proj;hr_df_hours

#근속연수별 승진 비율
hr_count_promo <- hr_df %>% 
  filter(promotion_last_5years == 1) %>% 
  group_by(time_spend_company) %>% 
  summarise(count_promo = n())
hr_count_promo
ggplot(data = hr_count_promo, aes(x = time_spend_company, y = count_promo))+geom_col()
hr_count_total <- hr_df %>%
  select(time_spend_company, promotion_last_5years) %>% 
  group_by(time_spend_company) %>% 
  summarise(count_total = n())
hr_count_total
View(hr_count_total)
hr_ratio_promo[1] <- hr_count_promo[1]
hr_ratio_promo[2] <- hr_count_promo[2]/hr_count_total[2]*100
names(hr_ratio_promo)[2] <- "ratio_promo"
hr_ratio_promo
ggplot(data=hr_ratio_promo, aes(x=time_spend_company, y=ratio_promo))+geom_col()

