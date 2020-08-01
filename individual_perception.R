library(reshape2)
library(ggplot2)
library(lme4)
library(Rmisc)
library(Hmisc)
library(reghelper)


setwd('/Users/ziqichen/Desktop/perception/')
data <- read.table('identification_all.txt', header = TRUE)

# prepare the data
data$tone <- as.factor(data$tone)
data$cf0 <- as.factor(data$cf0)
data$sex <- as.factor(data$sex)
data$order <- as.factor(data$order)
data$response <- as.factor(data$response)
data$subject <- as.factor(data$subject)


# # #

guoxuan <- subset(data, data$subject == "guoxuan")
hanzonghui <- subset(data, data$subject == "hanzonghui")
hejiayu <- subset(data, data$subject == "hejiayu")
hongyitian <- subset(data, data$subject == "hongyitian")
huangweitao <- subset(data, data$subject == "huangweitao")
huxiaoyu <- subset(data, data$subject == "huxiaoyu")
yangyujing <- subset(data, data$subject == "yangyujing")
zengjun <- subset(data, data$subject == "zengjun")
zengyao <- subset(data, data$subject == "zengyao")
zhanghuicao <- subset(data, data$subject == "zhanghuicao")
zhanglu <- subset(data, data$subject == "zhanglu")
zhangweiwei <- subset(data, data$subject == "zhangweiwei")
zhaomingwei <- subset(data, data$subject == "zhaomingwei")
zhongshulin <- subset(data, data$subject == "zhongshulin")
zhuchenlin <- subset(data, data$subject == "zhuchenlin")


# # #

print("guoxuan")
glm.guoxuan <- glm(response ~ cf0 * vot, data = guoxuan, family="binomial")
beta(glm.guoxuan)

print("hanzonghui")
glm.hanzonghui <- glm(response ~ cf0 * vot, data = hanzonghui, family="binomial")
beta(glm.hanzonghui)

print("hejiayu")
glm.hejiayu <- glm(response ~ cf0 * vot, data = hejiayu, family="binomial")
beta(glm.hejiayu)

print("hongyitian")
glm.hongyitian <- glm(response ~ cf0 * vot, data = hongyitian, family="binomial")
beta(glm.hongyitian)

print("huangweitao")
glm.huangweitao <- glm(response ~ cf0 * vot, data = huangweitao, family="binomial")
beta(glm.huangweitao)

print("huxiaoyu")
glm.huxiaoyu <- glm(response ~ cf0 * vot, data = huxiaoyu, family="binomial")
beta(glm.huxiaoyu)

print("yangyujing")
glm.yangyujing <- glm(response ~ cf0 * vot, data = yangyujing, family="binomial")
beta(glm.yangyujing)

print("zengjun")
glm.zengjun <- glm(response ~ cf0 * vot, data = zengjun, family="binomial")
beta(glm.zengjun)

print("zengyao")
glm.zengyao <- glm(response ~ cf0 * vot, data = zengyao, family="binomial")
beta(glm.zengyao)

print("zhanghuicao")
glm.zhanghuicao <- glm(response ~ cf0 * vot, data = zhanghuicao, family="binomial")
beta(glm.zhanghuicao)

print("zhanglu")
glm.zhanglu <- glm(response ~ cf0 * vot, data = zhanglu, family="binomial")
beta(glm.zhanglu)

print("zhangweiwei")
glm.zhangweiwei <- glm(response ~ cf0 * vot, data = zhangweiwei, family="binomial")
beta(glm.zhangweiwei)

print("zhaomingwei")
glm.zhaomingwei <- glm(response ~ cf0 * vot, data = zhaomingwei, family="binomial")
beta(glm.zhaomingwei)

print("zhongshulin")
glm.zhongshulin <- glm(response ~ cf0 * vot, data = zhongshulin, family="binomial")
beta(glm.zhongshulin)

print("zhuchenlin")
glm.zhuchenlin <- glm(response ~ cf0 * vot, data = zhuchenlin, family="binomial")
beta(glm.zhuchenlin)

# # # Save output into a file
# con <- file("test.log")
# sink(con, append=TRUE)
# sink(con, append=TRUE, type="message")



# #


glm.zhongshulin <- glm(response ~ cf0*(vot+I(vot^2)), data = zhongshulin, family="binomial")
beta(glm.zhongshulin)
summary(glm.zhongshulin)

data$predicted.probs <- glm.zhongshulin$fitted.values
p.probs <- subset(data, data$response == "p")

## Lastly, we can plot the predicted probabilities
ggplot(data=p.probs, aes(x=vot, y=predicted.probs, group=cf0, color=cf0)) +
  stat_summary(geom="errorbar", fun.data=mean_cl_normal,
               width=0.02, conf.int=0.95)+
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  scale_color_manual(breaks = c("H", "L"),
                     labels = c("High", "Low"),
                     values = c("orange", "blue")) +
  labs(title = "Model predictions of /p h/-responses",
       x = "VOT Step", y = "Predicted probability",
       color = "CF0") +
  theme(text = element_text(size=13))+
  ylim(0,1)

# #

high <- subset(huangweitao, huangweitao$cf0=="H")
highT <- table(high$response, high$vot)
highdf <- as.data.frame(highT)
highdf$prob <- highdf$Freq/12
highdf$cf0 <- "H"

low <- subset(huangweitao, huangweitao$cf0=="L")
lowT <- table(low$response, low$vot)
lowdf <- as.data.frame(lowT)
lowdf$prob <- lowdf$Freq/12
lowdf$cf0 <- "L"

newdata <- rbind(lowdf,highdf)
col_headings <- c('response','vot', 'freq','prob','cf0')
names(newdata) <- col_headings
pResponse <- subset(newdata, newdata$response=='p')

ggplot(pResponse, aes(x=vot, y=prob)) +
  geom_point(aes(color=cf0), alpha=10, shape=1,size=5) +
  scale_color_manual(labels = c("High", "Low"),
                     values = c("orangered", "darkblue")) +
  labs(title = "Subject 2 (M): Percentage of /p h/-response",
       x = "Voice onset time (VOT)", y = "Percentage",
       color = "CF0") +
  theme(text = element_text(size=13))
ggsave("identification_huangweitao_descrip.jpeg")

