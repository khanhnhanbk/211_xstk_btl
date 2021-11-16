options(warn=-1)

library(readr)
library(psych)

day <- read_csv("day.csv")
attach(day)


# Kiem dinh bang Chi-Square.

# Temp = (t - t_min) / (t_max - t_min) = (t + 8) / (39+8)
tempKenvin = (temp * 47 - 8) + 273
temptMa = matrix(c(cnt, tempKenvin), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ tempKenvin, main = "CNT phu thuoc va nhiet do", xlab = "Temperature (K)", ylab = "CNT", ylim = c(0,10000), xlim = c(260, 310), pch = 16, col = "blue")


# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
tempKenvin = (temp * 66 - 16) + 273
temptMa = matrix(c(cnt, tempKenvin), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ tempKenvin, main = "cnt ~ Feeling Temperature", xlab = "Feeling Temperature (K)", ylab = "CNT", ylim = c(0,9500), xlim = c(260, 315), pch = 16, col = "blue")


# hum: (do am) Normalized humidity. The values are divided to 100 (max)

humid = hum * 100
temptMa = matrix(c(cnt, humid), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ humid, main = "cnt ~ Normalized humidity", xlab = "Normalized humidity", ylab = "CNT", ylim = c(0,9000), xlim = c(0, 100), pch = 16, col = "blue")



# windspeed: Normalized wind speed. The values are divided to 67 (max)
windspeedT = windspeed*67
temptMa = matrix(c(cnt, windspeedT), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ windspeedT, main = "cnt ~ Normalized wind speed", xlab = "Normalized wind speed", ylab = "CNT", ylim = c(0,9000), xlim = c(0, 67), pch = 16, col = "blue")

###
# Anova
# season
av = aov(cnt ~ as.factor(season))
print(summary(av))
print(describe.by(cnt, as.factor(season), skew=F))
boxplot(cnt ~ as.factor(season), col="blue", main = "Th???ng kê CNT theo mùa", xlab = "Mùa (Dông - Xuân - H??? - Thu)", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T,col = "blue")
# qua nay thi thay season 1 (winter) la it nhat...
# year
av = aov(cnt ~ as.factor(yr))
print(summary(av))
print(describe.by(cnt, as.factor(yr), skew=F))
boxplot(cnt ~ as.factor(yr), col="blue", , main = "Th???ng kê CNT theo nam", xlab = "Nam (2011 - 2012)", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, col="blue")
# tam thoi de do vi phan tich theo yr ko co y nghia thuc tien
# month
av = aov(cnt ~ as.factor(mnth))
print(summary(av))
print(describe.by(cnt, as.factor(mnth), skew=F))
boxplot(cnt ~ as.factor(mnth), col="blue", main = "Th???ng kê CNT theo tháng", xlab = "Tháng", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, col="blue")
# Tháng thì có tháng cao di???m có tháng th???p. Ph???n b??? ???nh hu???ng th???t s???.
# holiday : weather day is holiday or not 
av = aov(cnt ~ as.factor(holiday))
print(summary(av))
print(describe.by(cnt, as.factor(holiday), skew=F))
boxplot(cnt ~ as.factor(holiday), col="blue", , main = "Th???ng kê CNT theo ngày ngh???", xlab = "0: di làm, 1: ngh???.", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, , col="blue")
# Không có th??? k???t lu???n du???c, tuy nhiên ngày l??? th???y s??? trung bình th???p hon.
# week day
av = aov(cnt ~ as.factor(weekday))
print(summary(av))
print(describe.by(cnt, as.factor(weekday), skew=F))
boxplot(cnt ~ as.factor(weekday), col="blue", , main = "Th???ng kê CNT theo ngày trong tu???n", xlab = "Ngày trong tu???n (0:Ch??? nh???t)", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, , col="blue")
# Có th??? th???y week day không ???nh hu???ng d??? cnt
# working day
av = aov(cnt ~ as.factor(workingday))
print(summary(av))
print(describe.by(cnt, as.factor(workingday), skew=F))
boxplot(cnt ~ as.factor(workingday), col="blue",  , main = "Th???ng kê CNT theo ngày trong tu???n và ngày cu???i tu???n", xlab = "1: ngày cu???i tu???n", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, , col="blue")
# Không th??? k???t lu???n workday ít hon ngày ngh???.
# weather
#- 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#- 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#- 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#- 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

av = aov(cnt ~ as.factor(weathersit))
print(summary(av))
print(describe.by(cnt, as.factor(weathersit), skew=F))
boxplot(cnt ~ as.factor(weathersit), col="blue", main = "Th???ng kê CNT theo th???i ti???t", xlab = "Th???i ti???t", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T,col="blue")
# k???t lu???n có ???nh hu???ng n???ng
