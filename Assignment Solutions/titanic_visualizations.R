options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train

titanic %>% ggplot(aes(x=Age,group=Sex,color=Sex)) + geom_density(alpha = 0.2)
titanic %>% filter(Age>40) %>% ggplot(aes(x=Age,group=Sex,color=Sex)) + geom_density()
titanic %>% filter(Age>=18 & Age<=35) %>% ggplot(aes(x=Age,group=Sex,color=Sex)) + geom_density()
titanic %>% filter(Age<17) %>% ggplot(aes(x=Age,group=Sex,color=Sex)) + geom_density()

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params) + geom_abline()

titanic %>% ggplot(aes(Survived)) + geom_bar(position = position_dodge()) 
titanic %>% ggplot(aes(Sex)) + geom_bar(position = position_dodge()) 
titanic %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge()) 
titanic %>% ggplot(aes(Sex,fill=Survived,color=Survived)) + geom_bar(position = position_dodge()) 

titanic %>% ggplot(aes(x=Age,fill=Survived)) + geom_density(alpha = 0.2)

titanic %>% filter(Fare!=0) %>% ggplot(aes(Survived,Fare)) +geom_boxplot()+scale_y_continuous(trans = "log2")
titanic %>% filter(Fare!=0) %>% ggplot(aes(Survived,Fare)) +geom_boxplot()+scale_y_continuous(trans = "log2") + geom_jitter()

titanic %>% ggplot(aes(Survived,fill=Pclass)) + geom_bar()
titanic %>% ggplot(aes(Survived,fill=Pclass)) +geom_bar(position = position_fill()) 
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar(position = position_fill()) 
titanic %>% ggplot(aes(Pclass)) +geom_bar() 

titanic %>% ggplot(aes(x=Age,fill=Survived)) + geom_density() + facet_grid(Sex~Pclass)
