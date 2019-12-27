library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(plotly)


#各車站各月出站人數bar
out_107 <- read_excel("107_out.xlsx")
station<-c("南港","臺北","板橋","桃園","新竹","苗栗","臺中","彰化","雲林","嘉義","臺南","左營")
out_long <- gather(out_107, station, n, station, factor_key=TRUE)
names(out_long)[1] <- "time"
out_long <- out_long %>%
  group_by(time) %>%
  filter(time != "107") %>%
  ungroup(time)
pic_out <- ggplot(out_long)+
  geom_bar(aes(x = station, y = n, fill = station), stat = "identity", position = "dodge")+
  theme(text=element_text(family="BiauKai", size=14))
pic_out <- ggplotly(pic_out)
pic_out

#各車站各月進站人數bar
in_107 <- read_excel("107_in.xlsx")
station<-c("南港","臺北","板橋","桃園","新竹","苗栗","臺中","彰化","雲林","嘉義","臺南","左營")
in_long <- gather(in_107, station, n, station, factor_key=TRUE)
names(in_long)[1] <- "time"
in_long <- in_long %>%
  group_by(time) %>%
  filter(time != "107") %>%
  ungroup(time)
pic_in <- ggplot(in_long)+
  geom_bar(aes(x = station, y = n, fill = station), stat = "identity", position = "dodge")+
  facet_wrap(vars(time))+
  theme(text=element_text(family="BiauKai", size=14))
pic_in <- ggplotly(pic_in)
pic_in

#南港每月加總折線
df_107 <- tibble(time = in_long$time, station = in_long$station, n_in = in_long$n, n_out = out_long$n)
df_107 <- df_107 %>%
  mutate(n_total = n_in + n_out) %>%
  filter(station == "臺北")
pic_total_per_month <- ggplot(df_107) +
  geom_line(aes(x = time, y = n_total), stat = "identity") +
  scale_x_discrete(limits=c(1:12))+
  theme(text=element_text(family="BiauKai", size=14))
pic_total_per_month <- ggplotly(pic_total_per_month)
pic_total_per_month

#歷年佔比
year_in <- read_excel("year_in.xlsx")
year_out <- read_excel("year_out.xlsx")
names(year_in)[1] <- "time"
names(year_out)[1] <- "time"
year_in_long <- gather(year_in, station, n, station, factor_key=TRUE)
year_out_long <- gather(year_out, station, n, station, factor_key=TRUE)
names(year_in_long)[2] <- "total"
names(year_out_long)[2] <- "total"
names(year_in_long)[4] <- "n_in"
names(year_out_long)[4] <- "n_out"
b4_prop <- cbind(year_in_long, n_out = year_out_long$n_out)
prop <- b4_prop %>%
  mutate(n_each = n_in+n_out, 
         proportion = (n_each/total)*100) %>%
  select(time, station, proportion)
  # a clear table for prop. each station
tidyprop <- spread(prop, station, proportion)
tidyprop
  # to draw a pic for prop. each station
pic_prop <- ggplot(prop) +
  geom_line(aes(x = time, y = proportion, color = station)) +
  scale_x_discrete(limits=c(96:107)) +
  theme(text=element_text(family="BiauKai", size=14))

pic_prop

#歷年成長率
b4_gr <- b4_prop %>%
  filter(station == "南港") %>%
  mutate(n_each = n_in+n_out) %>%
  select(time, station, n_each)

gr_1 <- b4_gr %>%
  mutate(grow_rate_na = c(0, (b4_gr$n_each[-1]-b4_gr$n_each)/(b4_gr$n_each[-1]))[1:12], 
         grow_rate = replace_na(grow_rate_na, 0)) %>%
  select(time, station, n_each, grow_rate)


#arrange the total data
in_long <- gather(in_107, station, n, station, factor_key=TRUE)
out_long <- gather(out_107, station, n, station, factor_key=TRUE)
names(in_long)[1] <- "time"
in_long2 <- in_long %>%
  group_by(time) %>%
  filter(time == "107") %>%
  ungroup(time)
out_long2 <- rbind(out_long[1, ],out_long[14, ],out_long[27, ],out_long[40, ],out_long[53, ],out_long[66, ],out_long[79, ],out_long[92, ],out_long[105, ],out_long[118, ],out_long[131, ],out_long[144, ])
df_107 <- tibble(time = "107", station = station, n_in = in_long2$n, n_out = out_long2$n)
df_107 <- df_107 %>%
  mutate(n_total = n_in + n_out) %>%
  select(station,n_total) %>%
  arrange(-n_total)
  
df_107 
ggplot(df_107) +
  geom_bar(aes(x = station, y = n_total), stat = "identity")+
  theme(text=element_text(family="BiauKai", size=14))


