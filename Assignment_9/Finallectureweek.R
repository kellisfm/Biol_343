#Final week stats 343

library(dplyr)
library(ggplot2)
library(ggfortify)

isodat <- read.csv("./Classcsvs/isolation.csv")

summary(isodat)

ggplot(isodat, aes(x = isolation, y = ï..incidence)) +
  geom_point(size = 5, alpha = 0.4)

#Seems to be catagorical in nature, adding a linear model gives 
#extreme residuals, as it expects the data to be continuous

autoplot(mod <- lm(ï..incidence~isolation,data = isodat), c(1,2,3,6))


#one option is to factor -> boxplot, but this is weird and scary
ggplot(isodat, aes(y = isolation, x = factor(ï..incidence))) +
  geom_boxplot()

#lets review odds

(P= 7/10)
oddsH = 7/3
oddsT = 3/7

#logit transformation used to correct these issues when combined with the glm
#function. GLM works basically like lm, but is more flexible


summary(gmod <- glm(ï..incidence~isolation, family = binomial(link = "logit"), data = isodat))

#to determine if isolation impacts incidence we can add an null model
summary(gnull <- glm(ï..incidence~1, family = binomial(link = "logit"), data = isodat))


#can then test with chisq (needed for catagorical)
anova(gnull,gmod, test = "Chisq")

confint(gmod)

gmod$coefficients["isolation"]

#this shows that for a 1km decrease there is a 74% decrease in the odds of incidence
#closer to one means less change
exp(gmod$coefficients["isolation"])

# can use predictions to generate a sigmoidal response curve
isodat$predi <- predict(gmod,type = "response")


ggplot(isodat, aes(x = isolation, y = ï..incidence))+
  geom_point(size = 5, alpha = 0.4, colour = "blue")+
  geom_line(aes(y = predi),lwd = 1, colour = "blue")


predict(gmod, list(isolation = 2.5), type = "response")
#this predicts that there is a 99.4% chance for the bird to appear w/2.5km isol
predict(gmod, list(isolation = 6), type = "response")

predict(gmod, list(isolation = 9), type = "response")





#Time to use these lessons to analyze the impact of island area.
ggplot(isodat, aes(x = area, y = ï..incidence))+
  geom_point(size = 5, alpha = 0.4, colour = "blue")
 
#seems to be a general pattern, with fairly high overlap
#but lets test it

summary(gmod <- glm(ï..incidence~area, family = binomial(link = "logit"), data = isodat))

summary(gnull <- glm(ï..incidence~1, family = binomial(link = "logit"), data = isodat))

anova(gnull,gmod, test = "Chisq")
#significant result, deviance drops by over 17


exp(gmod$coefficients["area"])
#since its greater than 1, we see an increase. Specifically 87% increase/square km

# can use predictions to generate a sigmoidal response curve
isodat$predi <- predict(gmod,type = "response")

ggplot(isodat, aes(x = area, y = ï..incidence))+
  geom_point(size = 5, alpha = 0.4, colour = "blue")+
  geom_line(aes(y = predi),lwd = 1, colour = "blue")

######lecture block 2, bonus GLMS!
######catagorical response and predictors
library(emmeans)
library(MuMIn)

options(na.action = "na.fail")

cach <- read.csv("./Classcsvs/CachPollinations.csv")

summary(cach)

sumdat <- cach %>% group_by(flw_size,poll_trt) %>% 
  summarise(n = n(),sumfs = sum(frtset),
            fruit_set = sumfs/n,
            se_fs = sqrt(fruit_set*(1-fruit_set)/n))
pd = position_dodge(width = 0.5)

ggplot(sumdat, aes(x = flw_size, y = fruit_set, colour = poll_trt))+
  geom_point(size = 5, position = pd)+
  geom_errorbar(aes(ymin = fruit_set-se_fs*1.96, ymax = fruit_set+se_fs*1.96),width = 0.2,  position = pd)


summary(full <- glm(frtset~poll_trt * flw_size, family = binomial(link = "logit"), data = cach))

summary(nox <- glm(frtset~poll_trt + flw_size, family = binomial(link = "logit"), data = cach))


anova(full,nox, test = "Chisq")

dredge(full)

exp(full$coefficients["(Intercept)"])
#how to interpret the models
exp(full$coefficients[1]+full$coefficients[2])
exp(full$coefficients[1]+full$coefficients[2]+full$coefficients[3]+full$coefficients[4])

#can once again use the predict function
predict(full, list(poll_trt = "X", flw_size = "LF"), type = "response")
#can make total predictions with this function
(em.d <- summary(emmeans(full, c("poll_trt","flw_size"),transform = "response")))

ggplot(em.d, aes(x = flw_size, y = prob, colour = poll_trt))+
  geom_point(size = 5, position = pd)+
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.2,  position = pd)

##### using GLM for proportional data
sr <- read.csv("./Classcsvs/sexratio.csv")

summary(sr)

sr <- sr %>% mutate(pmale = males/(males+females))

ggplot(sr, aes(x =log10(ï..density), y = pmale, colour = "magenta"))+
  geom_point(size = 5, position = pd)

sr <- sr %>% mutate(logdens = log10(ï..density))

#create a binomal variable column. It works with glm!
sr$resp  <- cbind(sr$males, sr$females)

summary(gsr <- glm(resp~logdens, family = binomial(link = "logit"), data = sr))

summary(gnull <- glm(resp~1, family = binomial(link = "logit"), data = sr))

anova(gnull,gsr, test = "Chisq")
#for every increase in resp theere is a 3.94x increase in density
(exp(gsr$coefficients[2])-1)*100
    
sr.pred <- predict(gsr, type = "response")
ggplot(sr, aes(x =log10(ï..density), y = pmale, colour = "magenta"))+
  geom_point(size = 5, position = pd)+
 geom_line(aes(y = sr.pred),lwd = 1, colour = "magenta")

#good start, but line ends up being a bit jank thanks to the low density
#we can fix this by creating a new data set and applying pred to it

pred.data <- data.frame(logdens  <-  seq(0,2.65, 0.01))

pred.data$predv <- predict(gsr, newdata = pred.data, type = "response")

#there we go, much better

ggplot(sr, aes(x =log10(ï..density), y = pmale, colour = "magenta"))+
  geom_point(size = 5, position = pd)+
  geom_line(data = pred.data,aes(x = logdens, y = predv),lwd = 1, colour = "magenta")
