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


###### cf0 and vot

m3 <- glm(response ~ cf0 * (vot + I(vot^2)) + order + subject, 
          data = data, family="binomial")
summary(m3)
beta(m3)
sink("m3_summary.txt")
print(summary(m3))
sink()  # returns output to the console

data$predicted.probs <- m3$fitted.values
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
  theme(text = element_text(size=13))

ggsave("identification_group.jpeg")

## output a table of model summary
