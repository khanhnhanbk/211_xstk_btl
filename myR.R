options(warn=-1)

library(readr)
library(psych)
library(coin)

day <- read_csv("day.csv")
attach(day)


# Kiem dinh bang Chi-Square.

# Temp = (t - t_min) / (t_max - t_min) = (t + 8) / (39+8)
    tempKenvin = (round(temp * 47) - 8) + 273
    temptMa = matrix(c(cnt, tempKenvin), ncol = 2)
    print(chisq.test(temptMa))
    plot(cnt ~ tempKenvin, main = "CNT phu thuoc va nhiet do", xlab = "Nhiet do (K)", ylab = "CNT", ylim = c(0,10000), xlim = c(260, 310), pch = 16, col = "blue")


# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
    tempKenvin = (round(temp * 66) - 16) + 273
    temptMa = matrix(c(cnt, tempKenvin), ncol = 2)
    print(chisq.test(temptMa))
    plot(cnt ~ tempKenvin, main = "CNT phu thuoc va cam giac nhiet", xlab = "Cam giac nhiet (K)", ylab = "CNT", ylim = c(0,9500), xlim = c(260, 315), pch = 16, col = "blue")


# hum: (do am) Normalized humidity. The values are divided to 100 (max)

humid = round(hum * 100)
temptMa = matrix(c(cnt, humid), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ humid, main = "CNT phu thuoc vao do am", xlab = "Do am", ylab = "CNT", ylim = c(0,9000), xlim = c(0, 100), pch = 16, col = "blue")



# windspeed: Normalized wind speed. The values are divided to 67 (max)
windspeedT = round(windspeed*67)
temptMa = matrix(c(cnt, windspeedT), ncol = 2)
print(chisq.test(temptMa))
plot(cnt ~ windspeedT, main = "CNT phu thuoc vao toc do gio", xlab = "Toc do giao", ylab = "CNT", ylim = c(0,9000), xlim = c(0, 67), pch = 16, col = "blue")

###
# Anova
# season
av = aov(cnt ~ as.factor(season))
print(summary(av))
print(describe.by(cnt, as.factor(season), skew=F))
boxplot(cnt ~ as.factor(season), col="blue", main = "Thong ke CNT theo mua", xlab = "Mua (Dong - Xuan - Ha - Thu", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T,col = "blue")
# qua nay thi thay season 1 (winter) la it nhat...
# year
#av = aov(cnt ~ as.factor(yr))
#print(summary(av))
#print(describe.by(cnt, as.factor(yr), skew=F))
#boxplot(cnt ~ as.factor(yr), col="blue", , main = "Th???ng k� CNT theo nam", xlab = "Nam (2011 - 2012)", ylab = "CNT")
#print(TukeyHSD(av))
#plot(TukeyHSD(av), ordered = T, col="blue")
# tam thoi de do vi phan tich theo yr ko co y nghia thuc tien
# month
av = aov(cnt ~ as.factor(mnth))
print(summary(av))
print(describe.by(cnt, as.factor(mnth), skew=F))
boxplot(cnt ~ as.factor(mnth), col="blue", main = "Thong ke CNT theo thang", xlab = "Thang", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, col="blue")
# Th�ng th� c� th�ng cao di???m c� th�ng th???p. Ph???n b??? ???nh hu???ng th???t s???.
# Kh�ng c� th??? k???t lu???n du???c, tuy nhi�n ng�y l??? th???y s??? trung b�nh th???p hon.
# week day
av = aov(cnt ~ as.factor(weekday))
print(summary(av))
print(describe.by(cnt, as.factor(weekday), skew=F))
boxplot(cnt ~ as.factor(weekday), col="blue", , main = "Thong ke CNT theo ngay trong tuan", xlab = "Ngay trong tuan (0:Chu nha)", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T, , col="blue")
# C� th??? th???y week day kh�ng ???nh hu???ng d??? cnt
# weather
#- 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#- 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#- 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#- 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

av = aov(cnt ~ as.factor(weathersit))
print(summary(av))
print(describe.by(cnt, as.factor(weathersit), skew=F))
boxplot(cnt ~ as.factor(weathersit), col="blue", main = "Thong ke CNT theo thoi tiet", xlab = "Thoi tiet", ylab = "CNT")
print(TukeyHSD(av))
plot(TukeyHSD(av), ordered = T,col="blue")
# k???t lu???n c� ???nh hu???ng n???ng

# kiem dinh hoan vi

# holiday : weather day is holiday or not 

oneway_test(cnt ~ as.factor(holiday), distribution=approximate(B=10000))
print(describe.by(cnt, as.factor(holiday), skew=F))
boxplot(cnt ~ as.factor(holiday), col="blue",  main = "Thong ke CNT theo ngay nghi", xlab = "1: ngay nghi.", ylab = "CNT")

# working day

oneway_test(cnt ~ as.factor(workingday), distribution=approximate(B=10000))
print(describe.by(cnt, as.factor(workingday), skew=F))
boxplot(cnt ~ as.factor(workingday), col="blue",   main = "Thong ke CNT theo lam viec", xlab = "0: ngay lam viec", ylab = "CNT")
