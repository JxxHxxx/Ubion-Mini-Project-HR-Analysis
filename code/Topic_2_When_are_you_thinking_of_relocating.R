left_df = HR %>%
  group_by(left) %>%
  summarise(n = n(), 
            years = mean(time_spend_company), 
            eval = mean(last_evaluation), 
            time = mean(average_montly_hours),
            accid = mean(Work_accident),
            satis = mean(satisfaction_level),
            promo = mean(promotion_last_5years)
            ) # 이직과 다른 피처들이 어떤 관계를 가지는지 살펴보기 위해 만든 DF


salary_df = HR %>% # 급여 수준과 이직률 DF
  group_by(salary) %>%
  summarise(left = sum(left),
            n =n()
            )


years_df = HR %>% # 연차별 이직률 DF
  group_by(time_spend_company) %>%
  summarise(n = n(),
            eval = mean(satisfaction_level), 
            accid = mean(Work_accident), 
            left = mean(left),
            worktime = mean(average_montly_hour)
            )


years_salary_df = HR %>% # 연차별 급여 수준 분포를 보기 위해 만든 DF
  group_by(time_spend_company, salary) %>%
  summarise(n = n()) %>%
  mutate(rati = sum(n)) %>%
  mutate(pct = round(n/rati*100,2)
         )


salary_df
years_df
years_salary_df

#------------------------------------------  차트 ---------------------------------------------------

salary_left_chart = ggplot(salary_df, aes(reorder(x = salary, -left/n),y = left/n), fill = salary) + 
  geom_col()  # 급여와 잔류 관계 차트

salary_left_chart 

years_left_chart = ggplot(years_df, aes(x = time_spend_company, y = left)) + 
  geom_col() #연차별 이직률 차트

years_left_chart


years_salary_chart = ggplot(years_salary_df, aes(x = time_spend_company, y = pct, fill = salary)) + 
  geom_col() +
  coord_flip() # 연차별 급여 수준 차트

years_salary_chart


