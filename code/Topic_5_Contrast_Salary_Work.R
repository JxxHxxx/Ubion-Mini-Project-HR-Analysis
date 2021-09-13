install.packages('dplyr')
library(readxl)
library(dplyr)
library(ggplot2)
hr <- read.csv('HR_comma_sep.csv')
hr_df <- hr

# 데이터 파악
summary(hr_df$average_montly_hours)

# 월 평균 근로 시간에 따른 분류
hr_df <-  hr_df %>%
  mutate(working_hours = ifelse(hr_df$average_montly_hours <= 156, "1st Qu",
                                ifelse(average_montly_hours <=200, '2nd Qu',
                                       ifelse(average_montly_hours <=245, '3rd Qu', '4th Qu'))))
hr_df %>% select(working_hours)  

# 1사분위수와 4사분위수에 해당하는 사원의 평균 프로젝트 수 비교 => 근무 시간이 길수록 진행한 프로젝트 수가 많다
hf_workh_prjt <- hr_df %>% 
  filter(working_hours == '1st Qu' | working_hours == '4th Qu') %>% 
  group_by(working_hours) %>% 
  summarise(mean_prjt = mean(number_project))
ggplot(data = hf_workh_prjt, aes(x = working_hours, y= mean_prjt))+geom_col()

# 진행 프로젝트에 따른 근무시간
# hr_prjt_hours <- hr_df %>% 
#   group_by(number_project) %>% 
#   summarise(mean_workh = mean(average_montly_hours))
# hr_prjt_hours
# ggplot(data = hr_prjt_hours, aes(x = number_project, y = mean_workh))+geom_col()

# 1사분위수와 4사분위수에 해당하는 사원의 평균 프로젝트 수 비교 
hr_workh_eval <- hr_df %>% 
  filter(working_hours == '1st Qu' | working_hours == '4th Qu') %>% 
  group_by(working_hours) %>%
  summarise(mean_eva = mean(last_evaluation))
hr_workh_eval
ggplot(data = hr_workh_eval, aes(x = working_hours, y = mean_eva))+geom_col()
