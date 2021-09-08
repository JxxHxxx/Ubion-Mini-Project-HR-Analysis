raw_HR = read.csv('C:/Users/JH/Desktop/Rstudy/HR_comma_sep.csv')
HR = raw_HR
plot1 = raw_HR$satisfaction_level 

acc = table(raw_HR$Work_accident)

p = ggplot(data = raw_HR, aes(raw_HR$Work_accident)) + geom_bar()
p #  전체 직원 사고 유무

class(acc)

sales_accident = HR %>%
  group_by(sales) %>%
  summarise(accident = sum(Work_accident))

sales_pop = HR %>%
  group_by(sales) %>%
  summarise(n = n())

sales_accident[,1]

sales_accident[,2]/sales_pop[,2] 

accident_pop_df = data.frame(col1 = sales_accident[,1], col2=sales_accident[,2]/sales_pop[,2])
accident_pop_df = sort(accident_pop_df, decreasing = T)



sales_accident_chart = ggplot(data = accident_pop_df, 
                      aes(x = reorder(sales, -(accident - accident_ratio)), y = accident - accident_ratio, 
                          fill = accident - accident_ratio)) + geom_col() + theme(plot.title=element_text(size=30))+
                                          labs(title = '어느 부서가 사고가 더 잘 발생할까?',
                                                x = '부서',
                                                y = '부서별 사고율 - 사내 사고율')# 부서별 사고율

sales_accident_chart 


table(HR$sales)


sales_accident

table(HR$sales)


t(table(HR$sales))

B = as.data.frame(A)
