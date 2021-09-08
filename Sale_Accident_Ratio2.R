sales_pop
par(mfrow=c(1,2))
p1 = ggplot(data = sales_pop, aes(x = reorder(sales, -n), y = n)) + geom_col()
p1

p2 = ggplot(data = HR, aes(x = number_project, y = average_montly_hours)) + geom_boxplot()
p2


grid.arrange(p1, sales_accident_chart, ncol = 1)

HR$number_project
