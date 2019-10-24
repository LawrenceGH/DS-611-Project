library(sqldf)
library(ggplot2) 
library(plyr)
library(tidyverse)

setwd ("C:/DS/09 Visualization/week8/data")

olympic <- read.csv("athlete_events.csv", header = TRUE)

### below command used to produce charts in PPT
##slide 1-1 .  all male and female under 19 player in same line chart, both Summer and Winter Olympic

sm<- sqldf("SELECT year,COUNT(*) male FROM olympic WHERE Sex='M' and Age < 19 and 
year in ('2000','2004','2008','2012','2016') group by year,sex order by year")

sf<- sqldf("SELECT year,COUNT(*) female FROM olympic WHERE Sex='F' and Age < 19 and 
year in ('2000','2004','2008','2012','2016') group by year,sex order by year")

wm<- sqldf("SELECT year,COUNT(*) male FROM olympic WHERE Sex='M' and Age < 19 and 
year in ('2002','2006','2010','2014') group by year,sex order by year")

wf<- sqldf("SELECT year,COUNT(*) female FROM olympic WHERE Sex='F' and Age < 19 and 
year in ('2002','2006','2010','2014') group by year,sex order by year")


tot1 <- data.frame(sm, sf[match(sm[,"Year"], sf[,"Year"]),])
tot2 <- data.frame(wm, wf[match(wm[,"Year"], wf[,"Year"]),])


palls = ggplot() + 
  geom_line(data = tot1, aes(x = tot1$Year, y = tot1$male, color = "blue")) +
  geom_line(data = tot1, aes(x = tot1$Year, y = tot1$female, color = "red")) +
  geom_line(data = tot2, aes(x = tot2$Year, y = tot2$male, color = "green")) +
  geom_line(data = tot2, aes(x = tot2$Year, y = tot2$female, color = "black")) +
  xlab('Dates') +   ylab('percent.change') +  labs(title = "Total players under 19 years old",
  x = "From 2000 to 2016",
  y = "Player number",
  fill = "male") + 
  scale_color_discrete(name = "Sex", labels = c( "Female Winter", "Male Summer","Male Winter","Female Summer")) +
  scale_x_continuous("Year", breaks=seq(2000, 2016, 4))


palls



-----------------
##slide 1-2 .  all male and female under 19 player won medals in same line chart, both Summer and Winter Olympic

smm<- sqldf("SELECT year,COUNT(*) male FROM olympic WHERE Sex='M' and Age < 19 and medal <>'NA' and
year in ('2000','2004','2008','2012','2016') group by year,sex order by year")

sfm<- sqldf("SELECT year,COUNT(*) female FROM olympic WHERE Sex='F' and Age < 19 and medal <>'NA' and
year in ('2000','2004','2008','2012','2016') group by year,sex order by year")

wmm<- sqldf("SELECT year,COUNT(*) male FROM olympic WHERE Sex='M' and Age < 19  and medal <>'NA' and 
year in ('2002','2006','2010','2014') group by year,sex order by year")

wfm<- sqldf("SELECT year,COUNT(*) female FROM olympic WHERE Sex='F' and Age < 19  and medal <>'NA' and 
year in ('2002','2006','2010','2014') group by year,sex order by year")


totm1 <- data.frame(smm, sf[match(smm[,"Year"], sfm[,"Year"]),])
totm2 <- data.frame(wmm, wf[match(wmm[,"Year"], wfm[,"Year"]),])


pallm = ggplot() + 
  geom_line(data = totm1, aes(x = totm1$Year, y = totm1$male, color = "blue")) +
  geom_line(data = totm1, aes(x = totm1$Year, y = totm1$female, color = "red")) +
  geom_line(data = totm2, aes(x = totm2$Year, y = totm2$male, color = "green")) +
  geom_line(data = totm2, aes(x = totm2$Year, y = totm2$female, color = "black")) +
  xlab('Dates') +   ylab('percent.change') +  labs(title = "Total players under 19 years old - medal winning",
  x = "From 2000 to 2016",
  y = "Player number",
  fill = "male") + 
  scale_color_discrete(name = "Sex", labels = c( "Female Winter", "Male Summer","Male Winter","Female Summer")) +
  scale_x_continuous("Year", breaks=seq(2000, 2016, 4))

pallm


##==============================
##Slide 2, barhart, Medal winning tier countries, player under 19 (summer Olympic and winter Olympic)

tops<- sqldf("SELECT year,'Top10' Tier, COUNT(*) Player FROM olympic WHERE noc in 
('USA','GBR','CHN','RUS','GER','JPN','FRA','KOR','ITA','AUS')
and Sex='M' and Age < 19 and year in ('2000','2004','2008','2012','2016') group by year,sex order by year")

ntops<- sqldf("SELECT year,'Non-Top10' Tier ,COUNT(*) Player FROM olympic WHERE noc not in 
('USA','GBR','CHN','RUS','GER','JPN','FRA','KOR','ITA','AUS')
and Sex='F' and Age < 19 and year in ('2000','2004','2008','2012','2016') group by year,sex order by year")


topw<- sqldf("SELECT Year,'Top10' Tier , COUNT(*) Player FROM olympic WHERE noc in 
('RUS','NOR','CAN','USA','NED','GER','SWI','BEL','AUS','FRA')
and Sex='M' and Age < 19 and year in ('2002','2006','2010','2014') group by year,sex order by year") 


ntopw<- sqldf("SELECT year,'Non-Top10' Tier,COUNT(*) Player FROM olympic WHERE noc not in 
('RUS','NOR','CAN','USA','NED','GER','SWI','BEL','AUS','FRA')
and Sex='F' and Age < 19 and year in ('2002','2006','2010','2014') group by year,sex order by year")


ts <- rbind(tops,ntops)
tw <- rbind(topw,ntopw)

tsp <- ddply(ts, c("Year", "Tier", "Player"))
twp <- ddply(tw, c("Year", "Tier", "Player"))

# Slide 2-1, summer Olympic
tspl <-ggplot(tsp, aes(Year, Player))
tspl + geom_bar(stat = "identity", aes(fill = Tier),width=1) +theme_minimal() +
  xlab('Year') +
  ylab('Player')+ labs(title = "Player under 19 years old\n- Medal winning countries(Summer Olympic)",
  x = "From 2002 to 2014",
  y = "Player number",
  fill = "Player from\nmedal winning\ncountry") + scale_x_continuous("Year", breaks=seq(2000, 2016, 4)) +
  scale_fill_manual("Tier", values = c("Non-Top10" = "#F57A7A", "Top10" = "#008080"))

# Slide 2-2, winter Olympic

twpl <-ggplot(twp, aes(Year, Player))
twpl + geom_bar(stat = "identity", aes(fill = Tier),width=1) +theme_minimal() +
  xlab('Year') +
  ylab('Player')+ labs(title = "Player under 19 years old\n- Medal winning countries(Winter Olympic)",
  x = "From 2002 to 2014",
  y = "Player number",
  fill = "Player from\nmedal winning\ncountry") + scale_x_continuous("Year", breaks=seq(2002, 2014, 4)) +
  scale_fill_manual("Tier", values = c("Non-Top10" = "#F57A7A", "Top10" = "#008080"))



##==============================

###Slide 3-1 barhart, male/female medal winning tier countries, player under 19 (summer Olympic and winter Olympic)

ss<- sqldf("SELECT year,Sex, COUNT(*) Player FROM olympic WHERE Age < 19 and year in ('2000','2004','2008','2012','2016') group by year,sex order by year")
ws<- sqldf("SELECT year,Sex ,COUNT(*) Player FROM olympic WHERE  Age < 19 and year in ('2002','2006','2010','2014') group by year,sex order by year")

tsp <- ddply(ss, c("Year", "Sex", "Player"))
twp <- ddply(ws, c("Year", "Sex", "Player"))

# Slide 3-1, summer Olympic
tspl <-ggplot(tsp, aes(Year, Player))
tspl + geom_bar(stat = "identity", aes(fill = Sex),width=1) +theme_minimal() +
  xlab('Year') +
  ylab('Player')+ labs(title = "Player under 19 years old \nfrom medal winning countries(Summer Olympic)",
  x = "From 2002 to 2014",
  y = "Player number",
  fill = "Player from\nmedal winning\ncountry") + scale_x_continuous("Year", breaks=seq(2000, 2016, 4))


# Slide 3-2, winter Olympic

twpl <-ggplot(twp, aes(Year, Player))
twpl + geom_bar(stat = "identity", aes(fill = Sex),width=1) +theme_minimal() +
  xlab('Year') +
  ylab('Player')+ labs(title = "Player under 19 years old \nfrom medal winning countries(Winter Olympic)",
  x = "From 2002 to 2014",
  y = "Player number",
  fill = "Player from\nmedal winning\ncountry") + scale_x_continuous("Year", breaks=seq(2002, 2014, 4)) 





##==============================
#Slide 4, USA male/female players under 19 years old

usa_tot <- sqldf("SELECT year, sex, COUNT(*) Player FROM olympic WHERE noc ='USA' and Age < 19 and year >1999 group by year order by year")

usas <- sqldf("SELECT year,sex, COUNT(*) Player FROM olympic WHERE noc ='USA'
and Age < 19 and year in ('2000','2004','2008','2012','2016') group by year,sex order by year");

usaw <- sqldf("SELECT year,sex, COUNT(*) Player FROM olympic WHERE noc ='USA'
and Age < 19 and year in ('2002','2006','2010','2014','2018') group by year,sex order by year");

usps <-ggplot(usas, aes(Year, Player))
usps +geom_bar(stat = "identity", aes(fill = Sex),width=1) +
 labs(title = "USA player under 19 years old(Summer Olympic)",
  x = "From 2000 to 2016",
  y = "Player number") + scale_x_continuous("Year", breaks=seq(2000, 2016, 4)) 



uspw <-ggplot(usaw, aes(Year, Player))
uspw +geom_bar(stat = "identity", aes(fill = Sex),width=1) +
 labs(title = "USA player under 19 years old(Winter Olympic)",
  x = "From 2002 to 2014",
  y = "Player number") + scale_x_continuous("Year", breaks=seq(2002, 2014, 4)) 


