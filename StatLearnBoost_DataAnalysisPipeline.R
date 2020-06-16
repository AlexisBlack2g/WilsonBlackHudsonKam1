library(gdata)
library(tidyverse)
library(lme4) #for mixed models
library(xtable) #for apa tables

##Analyses, in order through paper

#Experiment 1 Load data
exp1 <- read.csv('Experiment1.csv')

##Sentence Test Results
means <-
  exp1 %>% 
  filter(Phrase == "NonPhrase" & Test == "test1") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###T-tests
t.test(means$ACC[means$Condition == "WithoutCue"], mu = .5)
t.test(means$ACC[means$Condition == "WithCue"], mu = .5)

###Cohen's d
(mean(means$ACC[means$Condition == "WithoutCue"]) - .5)/sd(means$ACC[means$Condition == "WithoutCue"])
(mean(means$ACC[means$Condition == "WithCue"]) - .5)/sd(means$ACC[means$Condition == "WithCue"])

###Mixed Models
model_SentTest_Exp1 <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(exp1, Phrase == "NonPhrase" & Test == "test1"), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

##Phrase 1 Test Results
phrase1means <-
  exp1 %>% 
  filter(Phrase == "Phrase" & Test == "test1") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###t-tests
t.test(phrase1means$ACC[phrase1means$Condition == "WithoutCue"], mu = .5)
t.test(phrase1means$ACC[phrase1means$Condition == "WithCue"], mu = .5)

###cohen's d
(mean(phrase1means$ACC[phrase1means$Condition == "WithoutCue"]) - .5)/sd(phrase1means$ACC[phrase1means$Condition == "WithoutCue"])
(mean(phrase1means$ACC[phrase1means$Condition == "WithCue"]) - .5)/sd(phrase1means$ACC[phrase1means$Condition == "WithCue"])

###Mixed model ##does not converge; likely because the means are too high (too little variance)?
model_PhraseTestOld_Exp1 <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(exp1, Phrase == "Phrase" & Test == "test1"), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))


##Phrase 2 Test Results
phrase2means <-
  exp1 %>% 
  filter(!is.na(RESP)) %>% 
  #mutate(ACC = ifelse(is.na(RESP), 1, ACC)) %>% 
  filter(Phrase == "Phrase" & Test == "test2") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###t-tests
t.test(phrase2means$ACC[phrase2means$Condition == "WithoutCue"], mu = .5)
t.test(phrase2means$ACC[phrase2means$Condition == "WithCue"], mu = .5)

###Cohen's d
(mean(phrase2means$ACC[phrase2means$Condition == "WithoutCue"]) - .5)/sd(phrase2means$ACC[phrase2means$Condition == "WithoutCue"])
(mean(phrase2means$ACC[phrase2means$Condition == "WithCue"]) - .5)/sd(phrase2means$ACC[phrase2means$Condition == "WithCue"])

###Mixed effects
model_PhraseTestNew_Exp1 <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(exp1, Phrase == "Phrase" & Test == "test2"), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))



#Experiment 2 Load Data
exp2 <- read.csv('Experiment2.csv')

##Predictive sets Sentence Tests
means2 <-
  exp2 %>% 
  filter(Phrase == "NonPhrase" & TestType == "Predictive" & Test == "test1") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###T-tests
t.test(means2$ACC[means2$Condition == "EightyOld"], mu = .5)
t.test(means2$ACC[means2$Condition == "SixtyOld"], mu = .5)
t.test(means2$ACC[means2$Condition == "EightyNew"], mu = .5)

###Cohen's d
(mean(means2$ACC[means2$Condition == "EightyOld"]) - .5)/sd(means2$ACC[means2$Condition == "EightyOld"])
(mean(means2$ACC[means2$Condition == "SixtyOld"]) - .5)/sd(means2$ACC[means2$Condition == "SixtyOld"])
(mean(means2$ACC[means2$Condition == "EightyNew"]) - .5)/sd(means2$ACC[means2$Condition == "EightyNew"])

#for Mixed Effects
exp1$TestType <- NA
all3 <- rbind(exp1, exp2)
all3 <- all3 %>% mutate(Condition = factor(Condition, levels = c("EightyOld", "SixtyOld",
                                                                   "EightyNew", "WithoutCue",
                                                                   "WithCue")))

###WithoutCue
model_SentTestWOcue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithoutCue","EightyOld") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

model_SentTestWOcue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithoutCue","SixtyOld") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

model_SentTestWOcue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithoutCue","EightyNew") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

###With Cue
model_SentTestWCue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithCue","EightyOld") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

model_SentTestWCue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithCue","SixtyOld") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

model_SentTestWCue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                data = subset(all3, Phrase == "NonPhrase" & 
                                Test == "test1" & 
                                Condition %in% c("WithCue","EightyNew") &
                                TestType %in% c("Predictive", NA)), family = binomial, 
                control = glmerControl(optimizer = "bobyqa", 
                                       optCtrl=list(maxfun=2e6)))

##Experiment 2 Phrase Test 1
phrase1means_pred <-
  all2 %>% 
  #  filter(!is.na(RESP)) %>% 
  filter(Phrase == "Phrase" & Test == "test1") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###T-tests
t.test(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyOld"], mu = .5)
t.test(phrase1means_pred$ACC[phrase1means_pred$Condition == "SixtyOld"], mu = .5)
t.test(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyNew"], mu = .5)

###Cohen's d
(mean(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyOld"]) - .5)/sd(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyOld"])
(mean(phrase1means_pred$ACC[phrase1means_pred$Condition == "SixtyOld"]) - .5)/sd(phrase1means_pred$ACC[phrase1means_pred$Condition == "SixtyOld"])
(mean(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyNew"]) - .5)/sd(phrase1means_pred$ACC[phrase1means_pred$Condition == "EightyNew"])

###Mixed Models: Without Cue
model_PhraseTestWOCue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithoutCue","EightyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTestWOCue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithoutCue","SixtyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTestWOCue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithoutCue","EightyNew") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

###Mixed Models With Cue
model_PhraseTestWCue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithCue","EightyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTestWCue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithCue","SixtyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTestWCue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test1" & 
                                 Condition %in% c("WithCue","EightyNew") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))


##Exp 2 Phrase Test 2 
phrase2means_pred <-
  exp2 %>% 
  #  filter(!is.na(RESP)) %>% 
  filter(Phrase == "Phrase" & Test == "test2") %>% 
  group_by(SubjectNumber, Condition) %>% 
  summarise(ACC = mean(ACC))

###T-tests
t.test(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyOld"], mu = .5)
t.test(phrase2means_pred$ACC[phrase2means_pred$Condition == "SixtyOld"], mu = .5)
t.test(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyNew"], mu = .5)

###Cohen's d
(mean(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyOld"]) - .5)/sd(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyOld"])
(mean(phrase2means_pred$ACC[phrase2means_pred$Condition == "SixtyOld"]) - .5)/sd(phrase2means_pred$ACC[phrase2means_pred$Condition == "SixtyOld"])
(mean(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyNew"]) - .5)/sd(phrase2means_pred$ACC[phrase2means_pred$Condition == "EightyNew"])

###Mixed Models: Without Cue
model_PhraseTest2WOCue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithoutCue","EightyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTest2WOCue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithoutCue","SixtyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTest2WOCue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithoutCue","EightyNew") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

###With Cue
model_Phrase2TestWCue_Exp2_EightyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithCue","EightyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTest2WCue_Exp2_SixtyOld <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithCue","SixtyOld") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))

model_PhraseTest2WCue_Exp2_EightyNew <- glmer(ACC ~ Condition + (1 | SubjectNumber), 
                 data = subset(all3, Phrase == "Phrase" & 
                                 Test == "test2" & 
                                 Condition %in% c("WithCue","EightyNew") &
                                 TestType %in% c("Predictive", NA)), family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", 
                                        optCtrl=list(maxfun=2e6)))
