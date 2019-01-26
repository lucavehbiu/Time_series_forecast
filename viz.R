#Vizualize data
##Import the cleaned and modified data
load("cleanData.RDS")
load("hourly.RDS")
load("cost.included.RDS")

hourly <- plyr:: rename(hourly, c("Sub_metering_1" = "Kitchen", 
                 "Sub_metering_2" = "Laundry_room", 
                 "Sub_metering_3" = "Heaters"))


#hourly collapse
hourly$`hour(DateTime)` <- as.character(hourly$`hour(DateTime)`)
hourly <- hourly %>% dplyr:: select(-c(Global_intensity, Voltage))
hourly <- hourly %>%  fill(Global_active_power, Global_reactive_power, Kitchen, Laundry_room, S4, Heaters)
  
#daily grouping
daily <-  hourly %>% group_by(`date(DateTime)`, day(`date(DateTime)`), wday(`date(DateTime)`)) %>% 
  summarise_if(is.numeric, sum)

#weekly grouping
weekly <- hourly %>% group_by(year(`date(DateTime)`), week(`date(DateTime)`)) %>% 
  summarise_if(is.numeric, sum)

#monthly grouping
monthly <- hourly %>% group_by(year(`date(DateTime)`), month(`date(DateTime)`)) %>% 
  summarise_if(is.numeric, sum)

#quarterly grouping
quarterly <- daily %>% group_by(year(DateTime), quarter(DateTime)) %>% 
  summarise_if(is.numeric, sum)

################################################################################################
#divide by months of each year
january <- monthly %>%  filter(`month(date(DateTime))` == 1) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("January") + ylim(0, 1500)
february <- monthly %>% filter(`month(DateTime)` == 2)%>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("Febbruary")  + ylim(0, 1500)
march <- monthly %>% filter(`month(DateTime)` == 3) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("March")  + ylim(0, 1500)
april <- monthly %>% filter(`month(DateTime)` == 4) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("April")  + ylim(0, 1500)
may <- monthly %>% filter(`month(DateTime)` == 5) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("May")  + ylim(0, 1500)
june <- monthly %>% filter(`month(DateTime)` == 6) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("June")  + ylim(0, 1500)
july <- monthly %>% filter(`month(DateTime)` == 7) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("July")  + ylim(0, 1500)
august <- monthly %>% filter(`month(DateTime)` == 8) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("August")  + ylim(0, 1500)
september <- monthly %>% filter(`month(DateTime)` == 9) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("September")  + ylim(0, 1500)
october <- monthly %>% filter(`month(DateTime)` == 10) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("October")  + ylim(0, 1500)
november <- monthly %>% filter(`month(DateTime)` == 11) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("November")  + ylim(0, 1500)
december <- monthly %>% filter(`month(DateTime)` == 12) %>% 
  ggplot(aes(`year(DateTime)`, Global)) +
  geom_line() + xlab("December")  + ylim(0, 1500)

grid.arrange(january, february, march, april, may, june, july, august, september, october, november, december)

#losses by month
january <- monthly %>%  filter(`month(DateTime)` == 1) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("January") + ylim(0, 150)
february <- monthly %>% filter(`month(DateTime)` == 2)%>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("Febbruary")  + ylim(0, 150)
march <- monthly %>% filter(`month(DateTime)` == 3) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("March")  + ylim(0, 150)
april <- monthly %>% filter(`month(DateTime)` == 4) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("April")  + ylim(0, 150)
may <- monthly %>% filter(`month(DateTime)` == 5) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("May")  + ylim(0, 150)
june <- monthly %>% filter(`month(DateTime)` == 6) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("June")  + ylim(0, 150)
july <- monthly %>% filter(`month(DateTime)` == 7) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("July")  + ylim(0, 150)
august <- monthly %>% filter(`month(DateTime)` == 8) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("August")  + ylim(0, 150)
september <- monthly %>% filter(`month(DateTime)` == 9) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("September")  + ylim(0, 150)
october <- monthly %>% filter(`month(DateTime)` == 10) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("October")  + ylim(0, 150)
november <- monthly %>% filter(`month(DateTime)` == 11) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("November")  + ylim(0, 150)
december <- monthly %>% filter(`month(DateTime)` == 12) %>% 
  ggplot(aes(`year(DateTime)`, Loss)) +
  geom_line(color = "red") + xlab("December")  + ylim(0, 150)

grid.arrange(january, february, march, april, may, june, july, august, september, october, november, december)

#week global consumption
sunday <- daily %>%  filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Global))  +
  geom_line(color = "red") + xlab("Sunday") + ylim(0, 100)
saturday <- daily %>% filter(`wday(DateTime)` == 6)%>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "blue") + xlab("Saturday") + ylim(0, 100) 
friday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "green") + xlab("Friday") + ylim(0, 100)
thursday <- daily %>% filter(`wday(DateTime)` == 4) %>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "purple") + xlab("Thursday") + ylim(0, 100)
wednesday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "brown") + xlab("Wednesday") + ylim(0, 100)
tuesday <- daily %>% filter(`wday(DateTime)` == 6) %>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "yellow") + xlab("Tuesday") + ylim(0, 100)
monday <- daily %>% filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Global)) +
  geom_line(color = "gray") + xlab("Monday") + ylim(0, 100) 


grid.arrange(monday, tuesday, wednesday, thursday, friday, saturday, sunday)

#loss per weekdays
#week global consumption
daily <- daily[722:1442,]

sunday <- daily %>%  filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Loss))  +
  geom_area(color = "darkred") + xlab("Sunday") + ylim(0, 10) 
saturday <- daily %>% filter(`wday(DateTime)` == 6)%>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "darkblue") + xlab("Saturday") + ylim(0, 10) 
friday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "black") + xlab("Friday") + ylim(0, 10)
thursday <- daily %>% filter(`wday(DateTime)` == 4) %>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "black") + xlab("Thursday") + ylim(0, 10)
wednesday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "black") + xlab("Wednesday") + ylim(0, 10)
tuesday <- daily %>% filter(`wday(DateTime)` == 6) %>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "black") + xlab("Tuesday") + ylim(0, 10)
monday <- daily %>% filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Loss)) +
  geom_area(color = "black") + xlab("Monday") + ylim(0, 10) 


#check at different submeters

sunday <- daily %>%  filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1))  +
  geom_area(color = "darkred") + xlab("Sunday")  + ylim(0, 0.4)
saturday <- daily %>% filter(`wday(DateTime)` == 6)%>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "darkblue") + xlab("Saturday") + ylim(0, 0.4) 
friday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "black") + xlab("Friday") + ylim(0, 0.4)
thursday <- daily %>% filter(`wday(DateTime)` == 4) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "black") + xlab("Thursday") + ylim(0, 0.4)
wednesday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "black") + xlab("Wednesday") + ylim(0, 0.4)
tuesday <- daily %>% filter(`wday(DateTime)` == 6) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "black") + xlab("Tuesday") + ylim(0, 0.4)
monday <- daily %>% filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_1)) +
  geom_area(color = "black") + xlab("Monday")  + ylim(0, 0.4)

#submeter 2

sunday <- daily %>%  filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2))  +
  geom_area(fill = "blue") + xlab("Sunday")  + ylim(0, 0.4)
saturday <- daily %>% filter(`wday(DateTime)` == 6)%>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "darkblue") + xlab("Saturday") + ylim(0, 0.4) 
friday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "blue") + xlab("Friday") + ylim(0, 0.4)
thursday <- daily %>% filter(`wday(DateTime)` == 4) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "blue") + xlab("Thursday") + ylim(0, 0.4)
wednesday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "blue") + xlab("Wednesday") + ylim(0, 0.4)
tuesday <- daily %>% filter(`wday(DateTime)` == 6) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "blue") + xlab("Tuesday") + ylim(0, 0.4)
monday <- daily %>% filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_2)) +
  geom_area(fill = "blue") + xlab("Monday") + ylim(0, 0.4)


#heaters
sunday <- daily %>%  filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3))  +
  geom_area(fill = "orange") + xlab("Sunday")  + ylim(0, 0.4)

saturday <- daily %>% filter(`wday(DateTime)` == 6)%>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "darkorange") + xlab("Saturday") + ylim(0, 0.4) 

friday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "orange") + xlab("Friday") + ylim(0, 0.4)

thursday <- daily %>% filter(`wday(DateTime)` == 4) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "orange") + xlab("Thursday") + ylim(0, 0.4)

wednesday <- daily %>% filter(`wday(DateTime)` == 5) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "orange") + xlab("Wednesday") + ylim(0, 0.4) 
tuesday <- daily %>% filter(`wday(DateTime)` == 6) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "orange") + xlab("Tuesday") + ylim(0, 0.4)
monday <- daily %>% filter(`wday(DateTime)` == 7) %>% 
  ggplot(aes(`date(DateTime)`, Sub_metering_3)) +
  geom_area(fill = "orange") + xlab("Monday") + ylim(0, 0.4)







###################################################################################################

#gather function
test <- daily %>% gather('Kitchen', 'Laundry_room', 'Heaters', 'S4',
                                  key = "submeters", value = "consumption")

test %>% ggplot(aes(`date(DateTime)`), fill = factor(submeters)) + geom_density(alpha = 0.8)

#scatterplot - for relationship between variables

panel.plot <- function(x, y) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  ct <- cor.test(x,y)
  sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "**", "*", ".", " "))
  r <- ct$estimate
  rt <- format(r, digits=2)[1]
  cex <- 0.5/strwidth(rt)
  
  text(.5, .5, rt, cex=cex * abs(r), col = "gray")
  text(.8, .8, sig, cex=cex, col='blue')
}
panel.smooth <- function (x, y) {
  points(x, y)
  abline(lm(y~x), col="red")
  lines(stats::lowess(y~x, delta = 1), col="blue")
}
pairs(daily[5:10], lower.panel= panel.smooth, upper.panel= panel.plot)


#viz for disrrbution

#distrubtion of energy consumption
test %>% ggplot(aes(Global, color = submeters)) +
  geom_histogram(bins = 24) 
#distribution of energy loss 
test %>% ggplot(aes(Loss, color = submeters )) +
  geom_histogram(bins = 24) 
#######  more is consumed and lost for average consumption (30kvh - 2.3 kvh loss) per day
##distrubtion throughout the years
test %>% ggplot(aes(Global_reactive_power, Global_active_power, color = submeters)) +
  geom_area(bins = 24) + facet_wrap(~ wday(`date(DateTime)`))

test %>% ggplot(aes(Global, color = submeters, size = Global_reactive_power)) +
  geom_histogram(bins = 24) + facet_wrap(~ year(`date(DateTime)`))


