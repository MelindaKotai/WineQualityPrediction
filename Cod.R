library(rpart)
library(rpart.plot)
library(rsample)
library(tidyverse)
library(modelr)
library(tree)
library(caret)
library(corrplot)
library(randomForest)
library(e1071)


#se incarca setul de date
wine <- read.csv('C:\\Users\\Meli\\Desktop\\a3s2\\big data\\winequality-red.csv')

#graficul de distributie a variabilelor
wine %>%
  select_if(is.numeric)%>%
  gather(metric, value) %>%
  ggplot(aes(value, fill = metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

#grafic de distributie a calitatii
ggplot(data = wine, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('red'))

#grafic de corelatie
wine %>%
  select_if(is.numeric)%>%
  cor()%>%
  corrplot::corrplot(method = "number")

#transformam calitatea din intreg in factor
wine$quality <- factor(wine$quality, ordered = T)

#cream variabila rating
wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))
wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))
#grafic de distributie a ratingului
ggplot(data = wine, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('blue'))

#candle plots pentru fiecare variabila vs quality
ggplot(wine)+
  geom_boxplot(aes(quality, y = alcohol, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = fixed.acidity, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(x = quality, y = volatile.acidity, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = citric.acid, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = residual.sugar, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = chlorides, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = free.sulfur.dioxide, fill = quality))+
  theme(text = element_text(size = 20))
ggplot(wine)+
  geom_boxplot(aes(quality, y = density, fill = quality))+
  theme(text = element_text(size = 20))


#--------------------------------------------------------------------------#
#----------------------REGRESIA LINIARA------------------------------------#
#--------------------------------------------------------------------------#


wine<- wine[,-13]

#primul model cu toate variabilele
linear_model_all <- lm(as.numeric(quality) ~., data = wine)
summary(linear_model_all)

#modelul doar cu variabilele semnificative
linear_model_sv <- lm(as.numeric(quality) ~ volatile.acidity + chlorides + free.sulfur.dioxide +
                        total.sulfur.dioxide + pH + sulphates + alcohol, data = wine)
summary(linear_model_sv)

#model cu toate combinatiile variabilelor semnificative
linear_model_interactions<- lm(as.numeric(quality)~ volatile.acidity + chlorides + free.sulfur.dioxide +
                                 total.sulfur.dioxide + pH + sulphates + alcohol + volatile.acidity*chlorides +
                                 volatile.acidity*free.sulfur.dioxide + volatile.acidity*total.sulfur.dioxide + volatile.acidity*pH +
                                 volatile.acidity*sulphates + volatile.acidity*alcohol + chlorides*free.sulfur.dioxide +
                                 chlorides*total.sulfur.dioxide + chlorides*pH + chlorides*sulphates + chlorides*alcohol +
                                 free.sulfur.dioxide*total.sulfur.dioxide + free.sulfur.dioxide*pH + free.sulfur.dioxide*sulphates +
                                 free.sulfur.dioxide*alcohol + total.sulfur.dioxide*pH + total.sulfur.dioxide*sulphates +
                                 total.sulfur.dioxide*alcohol + pH *sulphates + pH *alcohol + sulphates *alcohol, data = wine)
summary(linear_model_interactions)

#model cu variabilele semnificative si iteractiunile semnificative
linear_model_interactions2<- lm(as.numeric(quality) ~ volatile.acidity + chlorides + free.sulfur.dioxide
                                + total.sulfur.dioxide + pH + sulphates + alcohol + volatile.acidity:free.sulfur.dioxide +
                                  total.sulfur.dioxide:sulphates + sulphates:alcohol , data = wine)
summary(linear_model_interactions2)



#-----------------------------------------------------------------------------------#
#----------------------REGRESIA LOGISTICĂ---------------------------------#
#-----------------------------------------------------------------------------------#

mod_qualityAlcohol <- glm(data = wine, quality ~alcohol, family = binomial)
summary(mod_qualityAlcohol)

#da-mi o suta de valori in aria alcolului intre[min balata, max balanta]
#type =response : prezice probabilitati, cum creste probabilitatea in fucntie de alcohol
wine <-read.csv('C:\\Users\\Meli\\Desktop\\a3s2\\big data\\winequality-red.csv')
wine <- wine %>%
  mutate(quality = ifelse(quality <= 5, 0, 1))
Data <- Data %>% mutate(quality = factor(quality))
table(Data$quality)
set.seed(123)

#impartim 70%-30%
split <- initial_split(wine, prop = 0.7, strata = "quality")
train <- training(split)
test <- testing(split)

#atoate atributele
mod_all <- glm (data = train, quality ~., family = binomial)
summary(mod_all)
pred_test <-predict(mod_all, newdata = test, type = "response")
table(pred_test> 0.4, test$quality)

#toate atributele si dependetele relevante
mod_all_dependencies<- glm(data = train, quality ~. + total.sulfur.dioxide:sulphates +
                             sulphates:alcohol, family = binomial)
summary(mod_all_dependencies)
pred_test <-predict(mod_all_dependencies, newdata = test, type = "response")
table(pred_test> 0.3, test$quality)

#atributele relevante si dependetele
mod_attributes_dependencies <- glm(data = train, quality ~ volatile.acidity + chlorides +
                                     free.sulfur.dioxide + total.sulfur.dioxide+ pH + sulphates + alcohol + volatile.acidity:free.sulfur.dioxide
                                   + total.sulfur.dioxide:sulphates + sulphates:alcohol, family = binomial)
summary(mod_attributes_dependencies)
pred_test <-predict(mod_attributes_dependencies, newdata = test, type = "response")
table(pred_test> 0.3, test$quality)

#atributele relevante
mod_attributes <- glm(data = train, quality ~ volatile.acidity + chlorides + free.sulfur.dioxide +
                        total.sulfur.dioxide+ pH + sulphates + alcohol, family = binomial)
summary(mod_attributes)
pred_test <-predict(mod_attributes, newdata = test, type = "response")
table(pred_test> 0.3, test$quality)



#--------------------------------------------------------------------------#
#----------------------RANDOM FOREST---------------------------------------#
#--------------------------------------------------------------------------#


#un singur tree
#SE FOLOSESTE CARET
#impartim setul de date in doua tipuri de vin de calitate joasa si de calitate inalta
levels(wine$quality) <- c("low", "low", "low", "high", "high", "high")

#IMPARTIM SETUL DE DATE IN DATE DE TEST SI DE ANTRENAMENT
set.seed(123)
wine_split <- initial_split(wine, prop = 0.7, strata = "quality")
wine_train <- training(wine_split)
wine_test <- testing(wine_split)
table(wine_train$quality)
table(wine_test$quality)

#se contruieste primul model folosind caret
set.seed(123)
m1 = rpart(
  formula = quality ~. ,
  data = wine_train,
  method = "class"
)
m1
summary(m1)
rpart.plot(m1)

#se fac predictii pe primul model folosind setul de date de test
pred_m1 <- predict(m1, newdata = wine_test, target ="class")
pred_m1 <- as_tibble(pred_m1) %>% mutate(class = ifelse(low >= high, "low", "high") )
table(pred_m1$class, wine_test$quality)
confusionMatrix(factor(pred_m1$class), wine_test$quality)

#CU ENRTROPE SI GINI
#se face un model folosind entropia
set.seed(123)
m1_tree <- tree(quality ~., data = wine_train)
m1_tree
summary(m1_tree)

#se fac predictiile
pred_m1_tree <- predict(m1_tree, newdata = wine_test, target = "class")
pred_m1_tree <- as_tibble(pred_m1_tree) %>% mutate(class = ifelse(low >= high, "low", "high"))
confusionMatrix(factor(pred_m1_tree$class), factor(wine_test$quality))

#se construieste un model folosind gini
set.seed(123)
m1_tree_gini <- tree(quality ~., data = wine_train, split="gini")
m1_tree_gini
summary(m1_tree_gini)

#se fac predictiile
pred_m1_tree_gini <- predict(m1_tree_gini, newdata = wine_test, target = "class")
pred_m1_tree_gini <- as_tibble(pred_m1_tree_gini) %>% mutate(class=ifelse(low >= high, "low",
                                                                          "high"))
confusionMatrix(factor(pred_m1_tree_gini$class), factor(wine_test$quality))

#random forests
set.seed(123)
m1_rf <- randomForest(
  formula = quality ~ .,
  data = wine_train
)
m1_rf
plot(m1_rf)

#eroarea obb
m1_rf$err.rate
which.min(m1_rf$err.rate[,1])
m1_rf$err.rate[which.min(m1_rf$err.rate[,1])]

#se fac predictii pe model
pred_rf<-predict(m1_rf,newdata = wine_test,taget="class")
confusionMatrix(factor(pred_rf), factor(wine_test$quality))