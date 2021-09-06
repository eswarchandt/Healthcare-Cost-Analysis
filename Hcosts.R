library(readxl)
Hcosts <- read_excel("C:/Users/Eswar/Desktop/Simpli_Learn/Data Science With R/Hcosts.xlsx")
summary(Hcosts)
hist(Hcosts$AGE)
age<-as.factor(Hcosts$AGE)
summary(age)
max(Hcosts$TOTCHG)
tapply(Hcosts$TOTCHG,Hcosts$AGE,sum)


model1<-as.factor(Hcosts$APRDRG)
summary(model1)
tapply(Hcosts$TOTCHG,model1,sum)
max(tapply(Hcosts$TOTCHG,model1,sum))
which.max(tapply(Hcosts$TOTCHG,model1,sum))
tab<-table(Hcosts$TOTCHG,Hcosts$LOS)
chisq.test(tab)

Hcosts1<-na.omit(Hcosts)
summary(Hcosts1)
model2<-lm(RACE~TOTCHG,data=Hcosts1)
summary(model2)
cor(Hcosts1$TOTCHG,Hcosts1$RACE)


model3<-lm(TOTCHG~AGE+FEMALE,data=Hcosts)
summary(model3)

model4<-lm(LOS~AGE+FEMALE+RACE,Hcosts)
summary(model4)

model5<-lm(TOTCHG~.,data=Hcosts)
summary(model5)