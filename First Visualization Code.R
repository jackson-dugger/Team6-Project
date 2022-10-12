require(readr)

fires <- read_csv("forestfires.csv")
head(fires)

fires$month <- as.factor(fires$month)
fires$month <- ordered(fires$month, levels=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"))
fires %>% ggplot(aes(x=month)) + geom_bar() + ggtitle("Forest Fires by Month") + xlab("Month") + ylab("# of Fires") + theme_bw()
