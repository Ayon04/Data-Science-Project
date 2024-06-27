projectdata<-read.csv("D:/Data Science Project/Dataset_Project 1.csv", header=TRUE, sep=",")
options(max.print = 2000)
projectdata

install.packages("tibble")
library("tibble")

projectdata$gender<- factor(projectdata$gender, levels=c("male","female"), labels= c(1,2))
projectdata
projectdata$class<- factor(projectdata$class, levels=c("positive","negative"), labels= c(1,0))
projectdata

which(is.na(projectdata$age))
which(is.na(projectdata$gender))
which(is.na(projectdata$impulse))
which(is.na(projectdata$pressurehight))
which(is.na(projectdata$pressurelow))
which(is.na(projectdata$glucose))
which(is.na(projectdata$class))
sum(is.na(projectdata))
is.na(projectdata)
colSums(is.na(projectdata))

summary(projectdata)


remove<- na.omit(projectdata)
remove
projectdata


mean_value <- mean(projectdata$age, na.rm = TRUE)
projectdata$age[is.na(projectdata$age)] <-mean_value
projectdata$age <- ceiling(projectdata$age)
projectdata


median_value <- median(projectdata$age, na.rm = TRUE)
projectdata$age[is.na(projectdata$age)] <- median_value
projectdata


install.packages("modeest")
library(modeest)
mode_val<- mlv(projectdata$age, method = "mfv")
mode_val
projectdata$age[is.na(projectdata$age)] <-mode_val
projectdata


md<- mlv(projectdata$gender, method = "mfv")
md
projectdata$gender[is.na(projectdata$gender)] <- md
projectdata
install.packages("zoo")
library(zoo)
tp<-projectdata$gender <- na.locf(projectdata$gender)
tp
projectdata$gender[is.na(projectdata$gender)] <-tp
projectdata


mean_val <- mean(projectdata$pressurehight, na.rm = TRUE)
projectdata$pressurehight <- ceiling(projectdata$pressurehight)
projectdata$pressurehight[is.na(projectdata$pressurehight)]<- mean_val
projectdata

median_value <- median(projectdata$pressurehight, na.rm = TRUE)
ceiled_median <- ceiling(median_value)
projectdata$pressurehight[is.na(projectdata$pressurehight)]<- ceiled_median
projectdata

install.packages("modeest")
library(modeest)
mode_v<- mlv(projectdata$pressurehight, method = "mfv")
mode_v
projectdata$pressurehight[is.na(projectdata$pressurehight)] <-mode_v
projectdata



a<- boxplot.stats(projectdata$age)$out
a
boxplot(projectdata$age,
        ylab = "age",
        main = "Boxplot for age"
)
mtext(paste("Outliers: ", paste(a, collapse = ", ")))
projectdata$age[projectdata$age %in% boxplot(projectdata)$out] <- median_value
projectdata


median_impluse <- median(projectdata$impluse, na.rm = TRUE)
projectdata$impluse[is.na(projectdata$impluse)] <- median_impluse
projectdata

b<- boxplot.stats(projectdata$impluse)$out
b
boxplot(projectdata$impluse,
        ylab = "impluse",
        main = "Boxplot for impluse"
)
mtext(paste("Outliers: ", paste(b, collapse = ", ")))
projectdata$impluse[projectdata$impluse %in% boxplot(projectdata)$out] <- median_impluse
projectdata



p<- boxplot.stats(projectdata$pressurehight)$out
p
boxplot(projectdata$pressurehight,
        ylab = "Pressurehight",
        main = "Boxplot for pressurehight"
)
mtext(paste("Outliers: ", paste(p, collapse = ", ")))
projectdata$pressurehight[projectdata$pressurehight %in% boxplot(projectdata)$out] <- ceiled_median
projectdata



median_low <- median(projectdata$pressurelow, na.rm = TRUE)
ceiled_median_low <- ceiling(median_low)
projectdata$pressurelow[is.na(projectdata$pressurelow)]<- ceiled_median_low
projectdata

plow<- boxplot.stats(projectdata$pressurelow)$out
plow
boxplot(projectdata$pressurelow,
        ylab = "Pressurelow",
        main = "Boxplot for pressurelow"
)
mtext(paste("Outliers: ", paste(plow, collapse = ", ")))
projectdata$pressurelow[projectdata$pressurelow %in% boxplot(projectdata)$out] <- ceiled_median_low
projectdata



median_glucose <- median(projectdata$glucose, na.rm = TRUE)
ceiled_median_glucose <- ceiling(median_glucose)
projectdata$glucose[is.na(projectdata$glucose)]<- ceiled_median_glucose
projectdata

gl<- boxplot.stats(projectdata$glucose)$out
gl
boxplot(projectdata$glucose,
        ylab = "glucose",
        main = "Boxplot for Glucose"
)
mtext(paste("Outliers: ", paste(gl, collapse = ", ")))
projectdata$glucose[projectdata$glucose %in% boxplot(projectdata)$out] <- ceiled_median_glucose
projectdata



s<- projectdata$age
sd(s)
im<- projectdata$impluse
sd(im)
hi<- projectdata$pressurehight
sd(hi)
lo<- projectdata$pressurelow
sd(lo)
glu<- projectdata$glucose
sd(glu)


ag <- table(projectdata$age)
barplot(ag, main="Age", xlab="Age")


bar <- table(projectdata$gender)
barplot(bar, main="Gender", xlab="Male = 1 & Female = 2")


imp<- table(projectdata$impluse)
barplot(imp, main="Impluse", xlab="Impluse")


high <- table(projectdata$pressurehight)
barplot(high, main="Pressurehight", xlab="Pressurehight")


lo <- table(projectdata$pressurelow)
barplot(lo, main="Pressure Low", xlab="Pressure Low")


glu <- table(projectdata$glucose)
barplot(glu, main="Glucose", xlab="Glucose")


cls <- table(projectdata$class)
barplot(cls, main="Class", xlab="Positive = 1 & Negative = 0")


hist(projectdata$age, main="Age", xlab="Age")
hist(projectdata$impluse, main="Impluse", xlab="Impluse")
hist(projectdata$pressurehight, main="Pressurehight", xlab="Pressurehight")
hist(projectdata$pressurelow, main="Pressure low", xlab="Pressure low")
hist(projectdata$glucose, main="Glucose", xlab="Glucose")

