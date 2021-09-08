sales_pop
par(mfrow=c(1,2))
p1 = ggplot(data = sales_pop, aes(x = reorder(sales, -n), y = n)) + geom_col()
p1


# grid.arrange(p1, sales_accident_chart, ncol = 1)

nbp = abs_HR$number_project

avgmh = abs_HR$average_montly_hours

df_nbp_avgmh = data.frame(col1 = nbp, col2 = avgmh)

p2 = ggplot(df_nbp_avgmh, aes(x = col1, y = col2)) + geom_boxplot()
p2

abs_HR = read.csv('C:/Users/JH/Desktop/Rstudy/HRcomma.csv')

np = HR$number_project
mh = HR$average_montly_hours

p3 = ggplot(HR, aes(x = HR$sales, y = mh/np)) + geom_boxplot()
p3

c('technical'='#131230',
  'IT'='#002955',
  'management'='#074ca1',
  'accounting'='#7c0022',
  'sales'='#ff6600',
  'RandD'='#c70125',
  'support'='#000000',
  'product_mng'='#c30452',
  'marketing'='#315288',
  'hr'='#ff0000') -> sales_name



p4 = ggplot(HR, aes(x = reorder(sales, -average_montly_hours), y = average_montly_hours),fill = average_montly_hours) + geom_bar(stat='identity') +
  scale_fill_manual(values = c("orange", "skyblue", "royalblue", "blue", "navy","orange", "skyblue", "royalblue", "blue", "navy"))
p4

p5 = ggplot(left_pop,aes(x =sales, y = left/n)) + geom_col(stat='identity')
p5




left_pop = HR %>%
  group_by(sales) %>%
  summarise(left = sum(left), n = n())

time_sales = HR %>%
  group_by(sales) %>%
  summarise(years = sum(time_spend_company),  npj = sum(number_project),n = n())

p6 = ggplot(time_sales, aes(x=sales, y=years/n)) + geom_col()
p6

p7 = ggplot(time_sales, aes(x=sales, y = npj/n)) + geom_col()
p7

p8 = ggplot(time_sales, aes(x=sales, y = npj)) + geom_col()
p8

left_df = HR %>%
  group_by(left) %>%
  summarise(n = n(), 
            years = mean(time_spend_company), 
            eval = mean(last_evaluation), 
            time = mean(average_montly_hours))

salary_df = HR %>%
  group_by(salary) %>%
  summarise(left = sum(left), 
            n = n())

years_df = HR %>%
  group_by(time_spend_company) %>%
  summarise(eval = mean(satisfaction_level), 
            accid = mean(Work_accident), 
            left = mean(left),
            worktime = mean(average_montly_hours),
            n = n())

years_df
salary_df
left_df

s1 = ggplot(salary_df, aes(reorder(x = salary, -left/n),y = left/n), fill = salary) + geom_col()
s1  # 연봉과 잔류의 관계

s2 = ggplot(left_df, aes(x = left, y = years)) + geom_col()
s2 # 연차 이직률

s3 = ggplot(left_df, aes(x = left, y = time)) + geom_col()
s3

s4 = ggplot(years_df, aes(x = time_spend_company, y = left)) + geom_col()
s4

s5 
s5
