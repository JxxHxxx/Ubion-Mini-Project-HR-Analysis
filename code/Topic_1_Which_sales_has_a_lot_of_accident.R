# 코드 실행 전 dplyr, ggplot2 라이브러리를 import 해주세요.

raw_HR = read.csv('C:/Users/JH/Desktop/Rstudy/HR_comma_sep.csv')
HR = raw_HR

sales_accident = HR %>% # df : 부서별 사고(Sum)
  group_by(sales) %>%
  summarise(accident = sum(Work_accident))

sales_pop = HR %>% # df : 부서별 직원 수(sum)
  group_by(sales) %>%
  summarise(n = n())

# ------------------------ 부서별 사고율 시각화를 위해 Df 를 만들겠습니다. ------------------------
sales_accident[,1] # 부서 Name
sales_accident[,2]/sales_pop[,2]  # 부서별 사고율

accident_pop_df = data.frame(col1 = sales_accident[,1], col2=sales_accident[,2]/sales_pop[,2])
accident_pop_df # 부서별 사고율 DF


sales_accident_chart = ggplot(data = accident_pop_df, 
                      aes(x = reorder(sales, -(accident - accident_ratio)), y = accident - accident_ratio, 
                      fill = accident - accident_ratio)) + geom_col() +
                      labs(x = '부서', y = '부서별 사고율 - 사내 사고율')  # (부서별 사고율 - 사내 전체 사고율) 차트

sales_accident_chart
# ----------------------------------     부서별 근무 시간     ------------------------------------

sales_hours_chart = ggplot(HR, aes(x = reorder(sales, - average_montly_hours), y = average_montly_hours)) +
 geom_col() # 부서별 근무시간 차트

sales_hours_chart


