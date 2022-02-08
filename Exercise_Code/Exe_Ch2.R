college.data <- read.csv("Data/College.csv",header=T,stringsAsFactors=T)

View(college.data)

rownames(college.data) <- college.data[,1]
college.data <- college.data[,-1]
View(college.data)

summary(college.data)

pairs(college.data[,1:10])

attach(college.data)
plot(Private,Outstate, xlab = "Private", ylab="Outstate")

Elite <- rep("No",nrow(college.data))
Elite[college.data$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college.data <- data.frame(college.data, Elite)
plot(Elite, Outstate, xlab = "Elite", ylab="Outstate")


# par(mfrow = c(2, 2))
hist(Apps,breaks=25)
hist(Outstate,breaks=15)
# dev.off()

plot(S.F.Ratio,Grad.Rate,xlab = "Student to Faculty Ratio", ylab = "Graduation Rate",
     main = "Plot of Grad.Rate vs S/F Ratio")
abline(lm(Grad.Rate~S.F.Ratio), col="red")
# Local regression line with smoothing of 25%.
# a variable bandwidth smoother, no monotonic assumption made, using NN to smooth
# weighted fit based on observation within windows, and adjusted by distance between observations and predictions 
loessMod = loess(Grad.Rate ~ S.F.Ratio, data=college.data, span=0.25)
j = order(S.F.Ratio)
lines(S.F.Ratio[j],loessMod$fitted[j], col="blue")


auto.data <- read.csv("Data/Auto.csv",header=T,stringsAsFactors = T)
View(auto.data)
auto.data$horsepower <- as.numeric(auto.data$horsepower)
sapply(auto.data[,1:7],range)
sapply(auto.data[,1:7],mean)
sapply(auto.data[,1:7],sd)
auto.data.new <- auto.data[-c(10:84),]
sapply(auto.data.new[,1:7],range)
sapply(auto.data.new[,1:7],mean)
sapply(auto.data.new[,1:7],sd)






