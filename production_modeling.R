library(tidyverse)
library(dplyr)
library(readr)
library(Rmisc)
library(ggplot2)

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

##Checking to see what the VOT distributions look like based on onset type.
ggplot((aes(x = vot,  group=onset, color=onset)), data=files) +
  theme_bw()+geom_density() +
  labs(title = "VOT distributions of onset consonants", 
       x = "VOT", y = "Density", 
       color = "Onset") +
  theme(text = element_text(size=13)) +
  scale_color_manual(values=c("#FFDB6D", "#D16103", "#4E84C4", "#293352"),
                    breaks=c("b", "d", "p","t"), 
                    labels=c("/p/", "/t/", "/p h/", "/t h/"))
# ggsave("vot_distr.jpeg")



##

### ANOVA tests on vot values 
vot.aov <- aov(vot ~ onset + tone + (onset*tone), data=files)
summary(vot.aov)

aspVot <- subset(files, files$onset==c("p","t"))
plainVot <- subset(files, files$onset==c("b","d"))
asp.aov <- aov(vot ~ onset + tone + (onset*tone), data=aspVot)
plain.aov <- aov(vot ~ onset + tone + (onset*tone), data=plainVot)
summary(asp.aov)
summary(plain.aov)


###adding useful predictors
Manner <- c("son", "plain", "son", "asp", "asp", "plain", "son")
onset <- c("l","b","m","p","t","d","n")
cons <- data.frame(onset, Manner)

df <- merge(files, cons, by=c("onset"))


### normalize f0 values across subjects 
###
f0subj <- ddply(df, .(filename), summarize, 
                meanF0 = mean(f0),
                sdF0 = sd(f0))
df <- merge(df, f0subj, by="filename")
df$zf0 <- (df$f0 - df$meanF0)/df$sdF0
df$tone <- factor(df$tone)

ZF0 <- subset(df, df$Manner==c("asp","plain"))
zf0_onset <- subset(ZF0, ZF0$time == "2")

ggplot(aes(x = tone, y = zf0, fill = Manner), data = zf0_onset) +
  geom_boxplot() +
  labs(y = "Normalized F0", x = "Tone categories", fill = "Onset") +
  theme(legend.box="horizontal") +
  theme(legend.text=element_text(size=10)) +
  scale_fill_manual(values = c("orange", "skyblue" ))+
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size = 12),
        axis.text.x  = element_text(size=12))
#ggsave("boxplot_zF0.jpeg",width = 8, height = 5, units = "in")


### normalize vot values by aspiration types and across subjects 
###

ZVOT <- subset(df, df$Manner==c("asp","plain"))
zvotsubj <- ddply(ZVOT, .(filename), summarize,
                  meanVot = mean(vot),
                  sdVot <- sd(vot))
ZVOT <- merge(ZVOT, zvotsubj, by="filename")
ZVOT$zvot <- (ZVOT$vot - ZVOT$meanVot)/ZVOT$sdVot
ZVOT$tone <- factor(ZVOT$tone)

ggplot(aes(x = tone, y = zvot, fill = Manner), data = ZVOT) +
  geom_boxplot() +
  labs(y = "Normalized VOT", x = "Tone categories", fill = "Onset") +
  theme(legend.box="horizontal") +
  theme(legend.text=element_text(size=10)) +
  scale_fill_manual(values = c("orange", "skyblue" ))+
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size = 12),
        axis.text.x  = element_text(size=12)) 
#ggsave("boxplot_zVOT.jpeg",width = 8, height = 5, units = "in")

# # # Figure illustrating the effect of onset manner and voicing on f0 across time
# set desired dodge width
pd <- position_dodge(width = 0.4)

# # # Figure illustrating the effect of onset manner and voicing on f0 across time
# # Across tones
df1 <- summarySE(df, measurevar="zf0", groupvars=c("time", "Manner"))

ggplot((aes(x = time, y=zf0,  group=Manner, color=Manner)), data=df1) +
  theme_bw() + geom_point(size=2) + geom_line(size=1)+
  labs(y = "zF0", x = "Normalized Time", color = "Onset", 
       title = "CF0-effect across tones")+
  theme(legend.box="horizontal")+ 
  geom_errorbar(aes(ymin=zf0 -ci, ymax=zf0 +ci), 
                width=.3,  linetype="solid", position = pd)+ 
  theme(legend.text=element_text(size=12)) +
  theme(plot.title = element_text(lineheight=5, face="bold", size=14), 
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size = 12),
        axis.text.x  = element_text(size=12)) 
#ggsave("cf0_effect_across_tone.jpeg",width = 6, height = 6, units = "in")




# # By-tone
# subj_tone <- summarySE(df, measurevar="zf0", groupvars=c("time", "Manner","tone","filename"))
# tone <- summarySE(df, measurevar="zf0", groupvars=c("time", "Manner","tone"))
subj <- summarySE(df, measurevar="zf0", groupvars=c("time", "Manner","filename"))


curves <- ggplot((aes(x = time, y=zf0,  group=Manner, color=Manner)), 
                 data=subj) +
  theme_bw()+geom_point(size=1)+ geom_line(size=.5)+
  labs(y = "zF0", x = "Normalized Time", color = "Onset")+
  geom_errorbar(aes(ymin=zf0 -ci, ymax=zf0 +ci), 
                width=.3,  linetype="solid", position = pd)+ 
  theme(plot.title = element_text(lineheight=5, face="bold", size=10)) +
  theme(legend.text=element_text(size=10)) 

# # The data can be split up by one or two variables that vary on the horizontal and/or vertical direction.
# # for subset: subj_tone
# curves + facet_grid(tone~filename)
# # for subset: subj
curves + facet_wrap(~filename, ncol = 4)
# for subset: tone
# curves + facet_wrap(~tone, ncol = 2,
                    labeller = labeller(tone = c('1' = 'T1(55)',
                                                 '2' = 'T2 (35)',
                                                 '3' = 'T3(214)',
                                                 '4' = 'T4(51)'))) +
  labs(title = "CF0-effect by tone") 
ggsave("cf0_by_subject.jpeg", height = 12,width = 12, units = "in")


# # # Figures illustrating the relation between VOT and Cf0 at Time 2
# # Subset of data for plotting
aspiration <- subset(df, df$Manner == c("asp","plain"))
T1 <- subset(aspiration, aspiration$tone == "1")
T2 <- subset(aspiration, aspiration$tone == "2")
T3 <- subset(aspiration, aspiration$tone == "3")
T4 <- subset(aspiration, aspiration$tone == "4")

df1 <- summarySE(aspiration[aspiration$time=="2",], measurevar="zf0", groupvars=c("vot", "Manner"))
ggplot((aes(x = vot, y=zf0,  group=Manner, color=Manner)), 
                  data=df1) + theme_bw()+
  geom_point(size=1)+
  geom_smooth(method = "lm",size = .8) +
  labs(y = "zF0", x = "VOT", color = "Manner", 
       title = "Overall") +
  theme(legend.box="horizontal") +
  theme(plot.title = element_text(lineheight=3, face="bold", size=12)) + 
  theme(legend.text=element_text(size=10)) +
  scale_color_manual(values = c("orangered", "blue" ))

#ggsave("scatter_overall.jpeg", width = 8.31, height = 3.77, units = "in")

# # # Linear regression for each tone

#aspiration <- subset(df, df$Manner == "plain")
aspiration <- subset(ZVOT, ZVOT$Manner == "plain")
cf0_aspiration <- subset(aspiration, aspiration$time == "2")
T1 <- subset(cf0_aspiration, cf0_aspiration$tone == "1")
T2 <- subset(cf0_aspiration, cf0_aspiration$tone == "2")
T3 <- subset(cf0_aspiration, cf0_aspiration$tone == "3")
T4 <- subset(cf0_aspiration, cf0_aspiration$tone == "4")

t1.m <- lm(zf0 ~ zvot, data = T1)
summary(t1.m)
beta.coef(t1.m)

t2.m <- lm(zf0 ~ zvot, data = T2)
summary(t2.m)

t3.m <- lm(zf0 ~ zvot, data = T3)
summary(t3.m)

t4.m <- lm(zf0 ~ zvot, data = T4)
summary(t4.m)

### ANOVA tests on cf0 values 
aspiration <- subset(df, df$Manner == c("asp","plain"))
cf0_aspiration <- subset(aspiration, aspiration$time == "2")

cf0.aov <- aov(zf0 ~ onset + tone + (onset*tone), data=cf0_aspiration)
summary(cf0.aov)


