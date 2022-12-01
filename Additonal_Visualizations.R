fires <- fires <- read_csv("forestfires.csv")

fires <- na.omit(fires)

ires <- fires %>% mutate_if(is.character, as.factor)

fires <- fires %>% mutate(area_log = log(area+1))

fires <- fires %>% mutate(RH_Level = cut(fires$RH, breaks = c(0, 10, 20, 30, 40, 50, 60, 70,80,90,100), labels = c("0-10%", "10%-20%", "20%-30%", "30%-40%", "40%-50", "50%-60%", "60%-70%", "70%-80%", "80%-90%", "90%-100%")))

fires <- fires %>% mutate(Fire_size = cut(area, breaks = c(-1,1,10,100,1100), labels = c("<1 ha", "1-10 ha", "10-100 ha", "100-1100 ha")))

fires <- fires %>% mutate(if_rain = (ifelse(rain==0, "No Rain", "Rain")))

##Visualizations

fires %>% ggplot(aes(x=Fire_size, y = RH, fill = Fire_size)) + geom_bar(stat = "summary", fun = mean) +
  xlab("Fire Size") + ylab("Humidity") + ggtitle("Humidity's Effect on Fire Size") + theme_bw()

fires %>% ggplot(aes(x=area)) + geom_histogram()

fires %>% ggplot(aes(x=log(1+area))) + geom_histogram()

fires %>% group_by(RH_Level) %>% summarize(count = n()) %>% ggplot(aes(x=RH_Level, y = count)) + geom_bar(stat = "identity") + geom_text(aes(label = count), vjust =0)