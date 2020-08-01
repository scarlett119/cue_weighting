library(tidyverse)
library(readr)
library(Rmisc)
library(ggplot2)
library(dplyr)

theme_set(theme_minimal())

setwd('/Users/ziqichen/Desktop/production/')

# Read in multiple .csv files in working directory and add original filename to each record
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

files <-list.files(pattern = ".+_dft.csv", 
                   full.names = T) %>% 
  map_df(~read_plus(.))

files$filename <- gsub("./", "", files$filename)
files$filename <- gsub("_dft.csv", "", files$filename)
files <- files[,c("filename","id","label","tone","time","f0","vot","startVot","endVot")]


###Preparing the dataframes for merger
files1 <- files %>% separate(label, c("ID", "syll"), sep="_")
files0 <- files1 %>% separate(syll, c("onset", "rime"), sep=1)
##adding the frame sentence as a variable. 
files <- files0 %>% separate(ID, c("frame", "num"), sep=1)
files$time <- as.factor(files$time)


###adding useful predictors
Manner <- c("son", "plain", "son", "asp", "asp", "plain", "son")
onset <- c("l","b","m","p","t","d","n")
cons <- data.frame(onset, Manner)

df <- merge(files, cons, by=c("onset"))

df$filename <- as.factor(df$filename)
df$Manner <- as.factor(df$Manner)
df$tone <- as.factor(df$tone)
# df$onset <- as.factor(df$onset)

test <- subset(df, df$onset==c("p","b"))
test <- subset(test, test$time=="2")

# # re-code response as 0 or 1
onset <- c("p","b")
binaryResp <- c(1, 0)
hey <- data.frame(onset, binaryResp)
test <- merge(test, hey, by="onset")

# test.m <- lm(binaryResp ~ f0 * vot, data=test)
# summary(test.m)
# test$predicted.probs <- test.m$fitted.values
# summary(test)

# foo <- table(test$Manner, test$vot)
# foodt <- as.data.frame(foo)
# foodt <- subset(foodt,foodt$Var1==c("asp","plain"))

# ## Lastly, we can plot the predicted probabilities
# ggplot(data=test, aes(x=vot, y=predicted.probs)) +
#   # stat_smooth(method="lm", se=FALSE)+
#   # stat_summary(geom="errorbar", fun.data=mean_cl_normal,
#   #              width=0.02, conf.int=0.95)+
#   stat_summary(fun.y=mean, geom="point") +
#   # labs(title = "Model predictions of /p h/-responses",
#   #      x = "VOT Step", y = "Predicted probability",
#   #      color = "CF0") 


# # # # Perform fit multiple models, each on the data of a subject
# test <-as_tibble(test)
# lms_production <- test %>%
#   nest(data = -filename) %>% 
#   mutate(
#     fit = map(data, ~ lm(f0 ~ vot, data = .x)),
#     tidied = map(fit, tidy)
#   ) 
# lms_production %>% 
#   unnest(tidied)

# levels(test$filename)

# # Linear regressions: production

guoxuan1 <- subset(test, test$filename == "guoxuan1")
hanzonghui1 <- subset(test, test$filename == "hanzonghui1")
hejiayu1 <- subset(test, test$filename == "hejiayu1")
hongyitian1 <- subset(test, test$filename == "hongyitian1")
huangweitao1 <- subset(test, test$filename == "huangweitao1")
huxiaoyu1 <- subset(test, test$filename == "huxiaoyu1")
yangyujing1 <- subset(test, test$filename == "yangyujing1")
zengjun1 <- subset(test, test$filename == "zengjun1")
zengyao1 <- subset(test, test$filename == "zengyao1")
zhanghuicao1 <- subset(test, test$filename == "zhanghuicao1")
zhanglu1 <- subset(test, test$filename == "zhanglu1")
zhangweiwei1 <- subset(test, test$filename == "zhangweiwei1")
zhaomingwei1 <- subset(test, test$filename == "zhaomingwei1")
zhongshulin1 <- subset(test, test$filename == "zhongshulin1")
zhuchenlin1 <- subset(test, test$filename == "zhuchenlin1")

source("Beta_coeff_calc.R")


# # #
lm.guoxuan1 <- lm(binaryResp ~ f0 * vot, data = guoxuan1)
# summary(lm.guoxuan1)
beta.coef(lm.guoxuan1)

lm.hanzonghui1 <- lm(binaryResp ~ f0 * vot, data = hanzonghui1)
# summary(lm.hanzonghui1)
beta.coef(lm.hanzonghui1)

lm.hejiayu1 <- lm(binaryResp ~ f0 * vot, data = hejiayu1)
beta.coef(lm.hejiayu1)

lm.hongyitian1 <- lm(binaryResp ~ f0 * vot, data = hongyitian1)
beta.coef(lm.hongyitian1)

lm.huangweitao1 <- lm(binaryResp ~ f0 * vot, data = huangweitao1)
summary(lm.huangweitao1)
beta.coef(lm.huangweitao1)

lm.huxiaoyu1 <- lm(binaryResp ~ f0 * vot, data = huxiaoyu1)
beta.coef(lm.huxiaoyu1)

lm.yangyujing1 <- lm(binaryResp ~ f0 * vot, data = yangyujing1)
beta.coef(lm.yangyujing1)

lm.zengjun1 <- lm(binaryResp ~ f0 * vot, data = zengjun1)
beta.coef(lm.zengjun1)

lm.zengyao1 <- lm(binaryResp ~ f0 * vot, data = zengyao1)
beta.coef(lm.zengyao1)

lm.zhanghuicao1 <- lm(binaryResp ~ f0 * vot, data = zhanghuicao1)
beta.coef(lm.zhanghuicao1)

lm.zhanglu1 <- lm(binaryResp ~ f0 * vot, data = zhanglu1)
beta.coef(lm.zhanglu1)

lm.zhangweiwei1 <- lm(binaryResp ~ f0 * vot, data = zhangweiwei1)
beta.coef(lm.zhangweiwei1)

lm.zhaomingwei1 <- lm(binaryResp ~ f0 * vot, data = zhaomingwei1)
beta.coef(lm.zhaomingwei1)

lm.zhongshulin1 <- lm(binaryResp ~ f0 * vot, data = zhongshulin1)
beta.coef(lm.zhongshulin1)

lm.zhuchenlin1 <- lm(binaryResp ~ f0 * vot, data = zhuchenlin1)
beta.coef(lm.zhuchenlin1)

# # # Save output into a file
# con <- file("test.log")
# sink(con, append=TRUE)
# sink(con, append=TRUE, type="message")

