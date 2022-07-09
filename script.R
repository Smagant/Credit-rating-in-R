# PROJET R

#Installation des librairies
install.packages("readxl")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("dplyr")
install.packages("gmodels")
install.packages("scales")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("lsr")
install.packages("crosstable")
install.packages('caret')
install.packages("pROC")
install.packages("rpart.plot")

#Activation des librairies
library(readxl)
library(ggplot2)
library(plotrix)
library(dplyr)
library(gmodels)
library(scales)
library(corrplot)
library(ggcorrplot)
library(Hmisc)
library(tidyverse)
library(lsr)
library(crosstable)
library(caret)
library(pROC)
library(rpart.plot)


#Suppression de toutes les variables dans l'environnement
rm(list = ls())

#Ouverture des bases de données
#dict <- read_excel("data/data_dico.xls")
df <- read.csv("data/data_train.csv", sep=",", header = TRUE)

# Etude préliminaire de la base de données
summary(df)

# Nettoyage de la base de données
#====================================================================================

# Suppression de la colonne correspondant à l'indice de la base de données.
df$ï.. <- NULL

# Nettoyage "SeriousDlqin2yrs"
df <- filter(df, df$SeriousDlqin2yrs == 0 | df$SeriousDlqin2yrs == 1)

# Nettoyage "RevolvingUtilizationOfUnsecuredLines"
df <- filter(df, df$RevolvingUtilizationOfUnsecuredLines >= 0 & df$RevolvingUtilizationOfUnsecuredLines <= 1)

# Nettoyage de la variable "age"
df <- filter(df, df$age >= 18 & df$age <= 100)

# Nettoyage de la variable "NumberOfTime30.59DaysPastDueNotWorse"
df <- filter(df, df$NumberOfTime30.59DaysPastDueNotWorse <= 12*max(df$NumberOfOpenCreditLinesAndLoans))

# Nettoyage de la variable "NumberOfOpenCreditLinesAndLoans"
df <- filter(df, df$NumberOfOpenCreditLinesAndLoans >= 0)

# Nettoyage de la variable "Debtratio". Hypothèse retenue : le taux d'endettement ne peut pas excéder 100%
df <- filter(df, df$DebtRatio >= 0 & df$DebtRatio <= 3)

# Nettoyage de la variable "MonthlyIncome". Ne peut pas être négatif mais aucune limite haute n'est intégrée
df <- filter(df, df$MonthlyIncome >= 0)

# Nettoyage de la variable "NumberOfTimes90DaysLate". Ne peut pas être négatif mais aucune limite haute n'est intégrée
df <- filter(df, df$NumberOfTimes90DaysLate >= 0)

# Nettoyage de la variable "NumberRealEstateLoansOrLines". Ne peut pas être négatif mais aucune limite haute n'est intégrée
df <- filter(df, df$NumberRealEstateLoansOrLines >= 0)

# Nettoyage de la variable "NumberOfTime60.89DaysPastDueNotWorse"
df <- filter(df, df$NumberOfTime60.89DaysPastDueNotWorse <= 8*max(df$NumberOfOpenCreditLinesAndLoans))

# Nettoyage de la variable "NumberOfDependents"
df <- filter(df, df$NumberOfDependents <= 10)

# On enlève les valeurs manquantes.
df <- na.omit(df)

#====================================================================================
# Construction d'une base de données avec des variables qualitatives
#====================================================================================

#Intégration de la variable "SeriousDlqin2yrs"
#------------------------------------------------------------------------------------
df_quali <- data.frame(df$SeriousDlqin2yrs)
names(df_quali)[1] <- "SeriousDlqin2yrs"
#CrossTable(df$SeriousDlqin2yrs)

#Intégration de la variable "RevolvingUtilizationOfUnsecuredLines"
#------------------------------------------------------------------------------------

#Passage des valeurs en pourcentages
#df$Percent_revol <- round(df$RevolvingUtilizationOfUnsecuredLines, digits = 4)
#df$Percent_revol <- df$RevolvingUtilizationOfUnsecuredLines*100 

#Création d'une variable vide
df_quali$qual_Revol <- rep(NA, length(df$RevolvingUtilizationOfUnsecuredLines))

#Création d'une variable contenant les quantiles
q_Revol <- quantile(df$RevolvingUtilizationOfUnsecuredLines)

#Boucle pour créer la variable quali
for (i in 1:length(q_Revol)) {
  if (is.na(q_Revol[i+1])) {
    break
  }
  df_quali$qual_Revol[
    which(df$RevolvingUtilizationOfUnsecuredLines >= q_Revol[i] & df$RevolvingUtilizationOfUnsecuredLines < q_Revol[i+1])
  ] <- paste("[",
             paste(as.character(round(q_Revol[i]*100, digits = 0)), "%", sep=""),
             ";",
             paste(as.character(round(q_Revol[i+1]*100, digits = 0)), "%", sep = ""),
             "[")
}

#Statistiques descriptives
#CrossTable(df_quali$qual_Revol)

#Vérification du nombre de NA à la suite de la classification
table(is.na(df_quali$qual_Revol))

#Suppression des NA
#df_quali <- na.omit(df_quali)

#Suppression des variables temporaires
rm(q_Revol, i)


#Intégration de la variable "Age"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_age <- rep(NA, length(df$age))

#Création d'une variable contenant les quantiles
q_age <- quantile(df$age)

#Boucle pour créer la variable quali
for (i in 1:length(q_age)) {
  if (is.na(q_age[i+1])) {
    break
  }
  df_quali$qual_age[
    which(df$age > q_age[i] & df$age <= q_age[i+1])
  ] <- paste("[",as.character(q_age[i]),";", as.character(q_age[i+1]), "[")
}

#CrossTable(df_quali$qual_age)
rm(q_age, i)


#Intégration de la variable "NumberOfTime30-59DaysPastDueNotWorse"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_3059 <- rep(NA, length(df$NumberOfTime30.59DaysPastDueNotWorse))

#Création d'une variable contenant les valeurs séparatrices
q_3059 <- c(0,1,100)

#Boucle pour créer la variable quali
for (i in 1:length(q_3059)) {
  if (is.na(q_3059[i+1])) {
    break
  }
  df_quali$qual_3059[
    which(df$NumberOfTime30.59DaysPastDueNotWorse >= q_3059[i] & df$NumberOfTime30.59DaysPastDueNotWorse < q_3059[i+1])
  ] <- paste("[", as.character(q_3059[i]),";", as.character(q_3059[i+1]), "[")
}

#CrossTable(df_quali$qual_3059)
rm(q_3059, i)



#Intégration de la variable "NumberOfTime60-89DaysPastDueNotWorse"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_6089 <- rep(NA, length(df$NumberOfTime60.89DaysPastDueNotWorse))

#Création d'une variable contenant les quantiles
q_6089 <- c(0,1,100)

#Boucle pour créer la variable quali
for (i in 1:length(q_6089)) {
  if (is.na(q_6089[i+1])) {
    break
  }
  df_quali$qual_6089[
    which(df$NumberOfTime60.89DaysPastDueNotWorse >= q_6089[i] & df$NumberOfTime60.89DaysPastDueNotWorse < q_6089[i+1])
  ] <- paste("[", as.character(q_6089[i]),";", as.character(q_6089[i+1]), "[")
}

#CrossTable(df_quali$qual_6089)
rm(q_6089, i)

#Intégration de la variable "NumberOfTimes90DaysLate"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_90 <- rep(NA, length(df$NumberOfTimes90DaysLate))

#Création d'une variable contenant les quantiles
q_90 <- c(0,1,100)

#Boucle pour créer la variable quali
for (i in 1:length(q_90)) {
  if (is.na(q_90[i+1])) {
    break
  }
  df_quali$qual_90[
    which(df$NumberOfTimes90DaysLate >= q_90[i] & df$NumberOfTimes90DaysLate < q_90[i+1])
  ] <- paste("[", as.character(q_90[i]),";", as.character(q_90[i+1]), "[")
}

#CrossTable(df_quali$qual_90)
rm(q_90, i)

#Intégration de la variable "DebtRatio"
#------------------------------------------------------------------------------------

#Passage de la valeur en pourcentage
#df_quali$Percent_DebtRatio <- df$DebtRatio*100

#Création d'une variable vide
df_quali$qual_DebtRatio <- rep(NA, length(df$DebtRatio))

#Création d'une variable contenant les quantiles
q_DebtRatio <- quantile(df$DebtRatio)

#Boucle pour créer la variable quali
for (i in 1:length(q_DebtRatio)) {
  if (is.na(q_DebtRatio[i+1])) {
    break
  }
  df_quali$qual_DebtRatio[
    which(df$DebtRatio >= q_DebtRatio[i] & df$DebtRatio < q_DebtRatio[i+1])
  ] <- paste("[",
             paste(as.character(round(q_DebtRatio[i]*100, digits = 0)), "%", sep = ""),
             ";",
             paste(as.character(round(q_DebtRatio[i+1]*100, digits = 0)), "%", sep = ""),
             "[")
}

#CrossTable(df_quali$qual_DebtRatio)
rm(q_DebtRatio, i)

#Intégration de la variable "MonthlyIncome"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_MonthlyIncome <- rep(NA, length(df$MonthlyIncome))

#Création d'une variable contenant les quantiles
q_MonthlyIncome <- quantile(df$MonthlyIncome, probs = seq(0, 1, by = .2))

#Boucle pour créer la variable quali
for (i in 1:length(q_MonthlyIncome)) {
  if (is.na(q_MonthlyIncome[i+1])) {
    break
  }
  df_quali$qual_MonthlyIncome[
    which(df$MonthlyIncome >= q_MonthlyIncome[i] & df$MonthlyIncome < q_MonthlyIncome[i+1])
  ] <- paste("[", as.character(q_MonthlyIncome[i]),";", as.character(q_MonthlyIncome[i+1]), "[")
}

#CrossTable(df_quali$qual_MonthlyIncome)
rm(q_MonthlyIncome, i)

#Intégration de la variable "NumberOfOpenCreditLinesAndLoans"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_OpenCredit <- rep(NA, length(df$NumberOfOpenCreditLinesAndLoans))

#Création d'une variable contenant les quantiles
q_OpenCredit <- quantile(df$NumberOfOpenCreditLinesAndLoans)

#Boucle pour créer la variable quali
for (i in 1:length(q_OpenCredit)) {
  if (is.na(q_OpenCredit[i+1])) {
    break
  }
  df_quali$qual_OpenCredit[
    which(df$NumberOfOpenCreditLinesAndLoans >= q_OpenCredit[i] & df$NumberOfOpenCreditLinesAndLoans < q_OpenCredit[i+1])
  ] <- paste("[", as.character(q_OpenCredit[i]),";", as.character(q_OpenCredit[i+1]), "[")
}

#CrossTable(df_quali$qual_OpenCredit)
rm(q_OpenCredit, i)


#Intégration de la variable "NumberRealEstateLoansOrLines"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_REloans <- rep(NA, length(df$NumberRealEstateLoansOrLines))

#Création d'une variable contenant les quantiles
q_REloans <- c(0,1,2,3,max(df$NumberRealEstateLoansOrLines))

#Boucle pour créer la variable quali
for (i in 1:length(q_REloans)) {
  if (is.na(q_REloans[i+1])) {
    break
  }
  df_quali$qual_REloans[
    which(df$NumberRealEstateLoansOrLines >= q_REloans[i] & df$NumberRealEstateLoansOrLines < q_REloans[i+1])
  ] <- paste("[", as.character(q_REloans[i]),";", as.character(q_REloans[i+1]), "[")
}

#CrossTable(df_quali$qual_REloans)
rm(q_REloans, i)

#Intégration de la variable "NumberOfDependents"
#------------------------------------------------------------------------------------

#Création d'une variable vide
df_quali$qual_Dep <- rep(NA, length(df$NumberOfDependents))

#Création d'une variable contenant les quantiles
q_Dep <- c(0,1,2,3,max(df$NumberOfDependents))

#Boucle pour créer la variable quali
for (i in 1:length(q_Dep)) {
  if (is.na(q_Dep[i+1])) {
    break
  }
  df_quali$qual_Dep[
    which(df$NumberOfDependents >= q_Dep[i] & df$NumberOfDependents < q_Dep[i+1])
  ] <- paste("[", as.character(q_Dep[i]),";", as.character(q_Dep[i+1]), "[")
}

#CrossTable(df_quali$qual_Dep)
rm(q_Dep, i)

#Etudier les valeurs manquantes
table(is.na(df_quali))

# On enlève les valeurs manquantes.
df_quali <- na.omit(df_quali)

#====================================================================================
# Data visualisation
#====================================================================================


#CrossTable sur chaque variable de la base de données
col <- colnames(df_quali)
for (i in 1:length(col)) {
  print(col[i])
  CrossTable(df_quali[,i])
}
rm(i)

#CrossTable de chaque variable explicative avec la variable à expliquer

for (i in 1:length(col)) {
  print(paste(col[1],' en fonction de ', col[i]))
  CrossTable(df_quali[,1],
             df_quali[,i],
             prop.r = TRUE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE
             )
}
rm(i, col)

CrossTable(df_quali$qual_age, df_quali$qual_MonthlyIncome,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


#SeriousDlqin2yrs AND qual_Revol
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_Revol,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_age
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_age,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_3059
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_3059,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_6089
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_6089,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_90
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_90,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_DebtRatio
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_DebtRatio,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_MonthlyIncome
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_MonthlyIncome,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_OpenCredit
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_OpenCredit,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_REloans
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_REloans,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#SeriousDlqin2yrs AND qual_Dep
#CrossTable(df_quali$df.SeriousDlqin2yrs, df_quali$qual_Dep,prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


#==========================Visualisation "SeriousDlqin2yrs"==========================

#count <- table(df$SeriousDlqin2yrs)
# Create test data.
#data <- data.frame(
#  category=c("Normal", "Impayés"),
#  count=c(count[1], count[2])
#)
# Compute percentages
#data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
#data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
#data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
#data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
#ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
#  geom_rect() +
#  geom_text(x=1, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
#  scale_fill_brewer(palette=1) +
#  scale_color_brewer(palette=1) +
#  coord_polar(theta="y") +
#  xlim(c(-1, 4)) +
#  theme_void() +
#  theme(legend.position = "none")

#rm(count)

#==========================Visualisation "RevolvingUtilizationOfUnsecuredLines"======================

#hist(df$RevolvingUtilizationOfUnsecuredLines, breaks = 25, col = "orange", main = "RevolvingUtilizationOfUnsecuredLines", xlab = "RevolvingUtilizationOfUnsecuredLines")

#==========================Visualisation "Age"======================


#==========================Visualisation "NumberOfTime30-59DaysPastDueNotWorse"======================
#df1 <- filter(df, df$NumberOfTime30.59DaysPastDueNotWorse <= 10)

#ggplot(data = df,
#       mapping = aes(x = exper, y = wage)) +
#  geom_point()
#hist(df1$NumberOfTime30.59DaysPastDueNotWorse, breaks = 25, col = "orange", main = "Histogram of age", xlab = "age")

#==========================Visualisation NumberOfTime60-89DaysPastDueNotWorse======================


#==========================Visualisation NumberOfTimes90DaysLate======================


#==========================Visualisation DebtRatio======================

#==========================Visualisation MonthlyIncome======================
#dfc <- filter(df, df$MonthlyIncome >= 0 & df$MonthlyIncome <= 30000)
#hist(dfc$MonthlyIncome, breaks = 100, col = "orange", main = "Histogram of MonthlyIncome", xlab = "$")

#==========================Visualisation NumberOfOpenCreditLinesAndLoans======================

#==========================Visualisation NumberOfTimes90DaysLate======================


#==========================Visualisation NumberRealEstateLoansOrLines======================


#==========================Visualisation NumberOfDependents======================

#df_new <- df
#names(df_new)[1] <- "Y"
#names(df_new)[2] <- "Revol"
#names(df_new)[3] <- "age"
#names(df_new)[4] <- "3059"
#names(df_new)[5] <- "DebtRatio"
#names(df_new)[6] <- "Income"
#names(df_new)[7] <- "OpenCredit"
#names(df_new)[8] <- "d90"
#names(df_new)[9] <- "RE_Loans"
#names(df_new)[10] <- "6089"
#names(df_new)[11] <- "Dep"

#===============================================================================
#CORRELATION ENTRE LES VARIABLES

#Nos variables explicatives sont corrélées significativement mais elles ne sont pas beaucoup corrélées.
#Par conséquent le problème de la multicolinéarité des variables n'est pas un problème.

corr <- rcorr(as.matrix(df), type = "pearson")
#On a trop de corrélation sur les var 3059 6089 90. J'enlève la variable 90 qui n'est pas trop intéressante pour étudier l'impact

#df2 <- subset(df, select = -NumberOfTimes90DaysLate)
#rcorr(as.matrix(df2), type = "pearson")
#rm(df2)

#On a bien une forte corrélation entre les variables 3059 et 6089. On fera une variable croisée entre les deux
#On a aussi une grande corrélation entre la var crédits ouverts et prêts immo. On fera une variable croisée


#corr = cor(df2)
#corr

#Test Corrélation pour des variables qualitatives (pas pertinent et difficile à lire la matrice de corrélation)

# function to get chi square p value and Cramers V
#f = function(x,y) {
#  tbl = df_quali %>% select(x,y) %>% table()
#  chisq_pval = round(chisq.test(tbl)$p.value, 4)
#  cramV = round(cramersV(tbl), 4) 
#  data.frame(x, y, chisq_pval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
#df_comb = data.frame(t(combn(sort(names(df_quali)), 2)), stringsAsFactors = F)

# apply function to each variable combination
#df_res = map2_df(df_comb$X1, df_comb$X2, f)

# plot results
#df_res %>%
#  ggplot(aes(x,y,fill=chisq_pval))+
#  geom_tile()+
#  geom_text(aes(x,y,label=cramV))+
#  scale_fill_gradient(low="red", high="yellow")+
#  theme_classic()

#étude d'un dataframe avec juste ces variables pour étudier la corrélation entre les deux.
#df_quali1 <- data.frame(
#  df_quali$qual_DebtRatio,
#  df_quali$qual_REloans)


# nouvelle matrice de corrélation avec code simplifié (de même pas pertinent)
#model.matrix(~0+., data=df_quali) %>% 
#  cor(use="pairwise.complete.obs") %>% 
#  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#Cross table entre qual_3059 et qual_6089
#ct1 = crosstable(df_quali, c(qual_3059, qual_6089), by=SeriousDlqin2yrs, total="both", 
#                 percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%
#  as_flextable()
#ct1

#===============================================================================
# CONSTRUCTION D'UNE BASE DE DONNEE TEST ET TRAINING

# Set seed of 567
set.seed(567)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(df_quali), 2 / 3 * nrow(df_quali))

# Create training set: training_set
training_set <- df_quali[index_train, ]

# Create test set: test_set
test_set <- df_quali[-index_train, ]

rm(index_train)

#===============================================================================
# CONSTRUCTION DES EQUATIONS

#equation1 <- SeriousDlqin2yrs ~
#  qual_Revol +
#  qual_age +
#  qual_3059 +
#  qual_6089 +
#  qual_90 +
#  qual_DebtRatio +
#  qual_MonthlyIncome +
#  qual_OpenCredit +
#  qual_REloans +
#  qual_Dep

#log_model_cat1 <- glm(formula = equation1, family = binomial(link = logit), data = training_set)

#summary(log_model_cat1)

# On enlève la variable "NumberOfTimes90DaysLate" car elle n'apporte rien à l'analyse
#equation11 <- SeriousDlqin2yrs ~
#  qual_Revol +
#  qual_age +
#  qual_3059 +
#  qual_6089 +
#  qual_DebtRatio +
#  qual_MonthlyIncome +
#  qual_OpenCredit +
#  qual_REloans +
#  qual_Dep

#log_model_cat11 <- glm(formula = equation11, family = binomial(link = logit), data = training_set)

# Print the parameter estimates 
#summary(log_model_cat11)


#equation12 <- SeriousDlqin2yrs ~
#  qual_Revol +
#  qual_age +
#  qual_DebtRatio +
#  qual_MonthlyIncome +
#  qual_OpenCredit +
#  qual_Dep +
#  qual_3059_6089 +
#  qual_OpenCredit_REloans

#log_model_cat12 <- glm(formula = equation12, family = binomial(link = logit), data = training_set)

#summary(log_model_cat12)

#On enlève tout ce qui n'est pas logique économiquement par rapport aux résultats de l'équation 1.
equation2 <- SeriousDlqin2yrs ~
  qual_Revol +
  qual_age +
  qual_3059 +
  qual_6089 +
  qual_DebtRatio +
  qual_MonthlyIncome
  
log_model_cat2 <- glm(formula = equation2, family = binomial(link = logit), data = training_set)
probit_model_cat2 <- glm(formula = equation2, family = binomial(link = probit), data = training_set)

summary(log_model_cat2)
summary(probit_model_cat2)

#Test en enlevant la variable debt ratio
#equation3 <- SeriousDlqin2yrs ~
#  qual_Revol +
#  qual_age +
#  qual_3059 +
#  qual_6089 +
#  qual_MonthlyIncome

#log_model_cat3 <- glm(formula = equation3, family = binomial(link = logit), data = training_set)
#probit_model_cat3 <- glm(formula = equation3, family = binomial(link = probit), data = training_set)

#summary(log_model_cat3)

#===============================================================================
# PREDICTIONS

# Les deux derniers modèles sont proches et logique économiquement. On run des prédictions sur les deux pour décider lequel est le plus intéressant.
predictions_eq2_logit <- predict(log_model_cat2, newdata = test_set, type = "response")
predictions_eq2_probit <- predict(probit_model_cat2, newdata = test_set, type = "response")


# Look at the predictions range
#range(predictions_multi)

test_set$predictions_logit=predictions_eq2_logit
test_set$predictions_probit=predictions_eq2_probit


#===============================================================================
# CREATING A CONFUSION MATRIX

#Trouver le cutoff qui maximise notre accuracy.
#cutoff = 0.01
#vect_co = c()
#vect_acc = c()

#while (cutoff < 1) {
#  logit_cutoff <- ifelse(predictions_eq3_logit > cutoff, 1, 0)
#  tab_class_logit <- table(test_set$SeriousDlqin2yrs, logit_cutoff)
#  acc <- sum(diag(tab_class_logit)) / nrow(test_set)
#  vect_co <- append(vect_co, cutoff)
#  vect_acc <- append(vect_acc, acc)
#  cutoff = cutoff + 0.01
#}

#Créer un dataframe pour représenter nos valeurs
#data_acc <- data.frame(
#  vect_co,
#  vect_acc
#)

#Selon notre modèle, le cutoff optimal est de 0,63. Voici ci-dessous la matrice de confusion
#tab_class_logit <- table(test_set$SeriousDlqin2yrs,
#      ifelse(predictions_eq2_logit > 0.64, 1, 0))

#On remarque que cela n'est pas la bonne méthode. On va tester avec une autre méthode
#On veut maximiser le true positive et true négative
#Après plusieurs tests, on trouve que le cutoff optimal est celui 0.05
#On utilise la fonction Confusion Matrix à présent de la librarie "caret" pour avoir plus de chiffres

expected_value <- factor(test_set$SeriousDlqin2yrs)
predicted_value_logit <- factor(ifelse(predictions_eq2_logit > 0.05, 1, 0))
predicted_value_probit <- factor(ifelse(predictions_eq2_probit > 0.05, 1, 0))

CM_logit <- confusionMatrix(data=predicted_value_logit, reference = expected_value)
CM_probit <- confusionMatrix(data=predicted_value_probit, reference = expected_value)

#Afficher les résultats. 
CM_logit
CM_probit

#===============================================================================
# ROC CURVE

# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$SeriousDlqin2yrs, predictions_eq2_logit)
ROC_probit <- roc(test_set$SeriousDlqin2yrs, predictions_eq2_probit)

# Draw all ROCs on one plot
plot(ROC_probit)
#lines(ROC_logit, col = "blue")

# Compute the AUCs
auc(ROC_logit)
auc(ROC_probit)

# Premier test construction de la variable grade

#condA <- (test_set$SeriousDlqin2yrs == 0) & (test_set$qual_3059 == "[ 0 ; 1 [") & (test_set$qual_6089 == "[ 0 ; 1 [")
#condB <- (test_set$SeriousDlqin2yrs == 0) & (test_set$qual_3059 == "[ 1 ; 100 [") & (test_set$qual_6089 == "[ 0 ; 1 [")
#condC <- (test_set$SeriousDlqin2yrs == 0) & (test_set$qual_3059 == "[ 0 ; 1 [") & (test_set$qual_6089 == "[ 1 ; 100 [")
#condD <- (test_set$SeriousDlqin2yrs == 0) & (test_set$qual_3059 == "[ 1 ; 100 [") & (test_set$qual_6089 == "[ 1 ; 100 [")
#condE <- (test_set$SeriousDlqin2yrs == 1) & (test_set$qual_3059 == "[ 0 ; 1 [") & (test_set$qual_6089 == "[ 0 ; 1 [")
#condF <- (test_set$SeriousDlqin2yrs == 1) & (test_set$qual_3059 == "[ 1 ; 100 [") & (test_set$qual_6089 == "[ 0 ; 1 [")
#condG <- (test_set$SeriousDlqin2yrs == 1) & (test_set$qual_3059 == "[ 0 ; 1 [") & (test_set$qual_6089 == "[ 1 ; 100 [")
#condH <- (test_set$SeriousDlqin2yrs == 1) & (test_set$qual_3059 == "[ 1 ; 100 [") & (test_set$qual_6089 == "[ 1 ; 100 [")

#Construction de la variable grade.
#test_set$grade <- ifelse(condA, "A",
#                         ifelse(condB, "B",
#                                ifelse(condC, "C",
#                                       ifelse(condD, 'D',
#                                              ifelse(condE, "E",
#                                                     ifelse(condF, "F",
#                                                            ifelse(condG, "G",
#                                                                   ifelse(condH, "H", "non classé")
#                                                                   )
#                                                            )
#                                                     )
#                                              )
#                                       )
#                                )
#                         )

#rm(condA, condB, condC, condD, condE, condF, condG, condH)

# Validation de la variable grade
#CrossTable(test_set$grade, test_set$SeriousDlqin2yrs,
#           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#CrossTable(df_quali$grade, df_quali$qual_3059,
#           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#CrossTable(df_quali$grade, df_quali$qual_6089,
#           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)



#Création d'une variable vide
#test_set$pred_cat <- rep(NA, length(test_set$predictions_logit))

#Création d'une variable contenant les quantiles
#q_grade <- quantile(test_set$predictions_logit,
#                    probs = seq(0, 1, by = .05))


#Boucle pour créer la variable quali
#for (i in 1:length(q_grade)) {
#  if (is.na(q_grade[i+1])) {
#    break
#  }
#  test_set$pred_cat[
#    which(test_set$predictions_logit >= q_grade[i] & test_set$predictions_logit < q_grade[i+1])
#  ] <- paste("[",
#             paste(as.character(round(q_grade[i]*100, digits = 0)), "%", sep = ""),
#             ";",
#             paste(as.character(round(q_grade[i+1]*100, digits = 0)), "%", sep = ""),
#             "[")
#}

#for (i in 1:length(q_grade)) {
#  if (is.na(q_grade[i+1])) {
#    break
#  }
#  test_set$pred_cat[
#    which(test_set$predictions_logit >= q_grade[i] & test_set$predictions_logit < q_grade[i+1])
#  ] <- round(q_grade[i]*100, digits = 0)
#}

#===============================================================================
#Deuxième test construction variable grade

#Création d'une variable contenant les quantiles
#q_grade <- quantile(test_set$predictions_logit,
#                    probs = seq(0, 1, by = .05))


#cond grade V1
#condAAA <- test_set$predictions_logit >= q_grade[1] & test_set$predictions_logit < q_grade[2]
#condAA_p <- test_set$predictions_logit >= q_grade[2] & test_set$predictions_logit < q_grade[3]
#condAA <- test_set$predictions_logit >= q_grade[3] & test_set$predictions_logit < q_grade[4]
#condAA_m <- test_set$predictions_logit >= q_grade[4] & test_set$predictions_logit < q_grade[5]
#condA_p <- test_set$predictions_logit >= q_grade[5] & test_set$predictions_logit < q_grade[6]
#condA <- test_set$predictions_logit >= q_grade[6] & test_set$predictions_logit < q_grade[7]
#condA_m <- test_set$predictions_logit >= q_grade[7] & test_set$predictions_logit < q_grade[8]
#condBBB_p <- test_set$predictions_logit >= q_grade[8] & test_set$predictions_logit < q_grade[9]
#condBBB <- test_set$predictions_logit >= q_grade[9] & test_set$predictions_logit < q_grade[10]
#condBBB_m <- test_set$predictions_logit >= q_grade[10] & test_set$predictions_logit < q_grade[11]
#condBB_p <- test_set$predictions_logit >= q_grade[11] & test_set$predictions_logit < q_grade[12]
#condBB <- test_set$predictions_logit >= q_grade[12] & test_set$predictions_logit < q_grade[13]
#condBB_m <- test_set$predictions_logit >= q_grade[13] & test_set$predictions_logit < q_grade[14]
#condB_p <- test_set$predictions_logit >= q_grade[14] & test_set$predictions_logit < q_grade[15]
#condB <- test_set$predictions_logit >= q_grade[15] & test_set$predictions_logit < q_grade[16]
#condB_m <- test_set$predictions_logit >= q_grade[16] & test_set$predictions_logit < q_grade[17]
#condCCC_p <- test_set$predictions_logit >= q_grade[17] & test_set$predictions_logit < q_grade[18]
#condCCC <- test_set$predictions_logit >= q_grade[18] & test_set$predictions_logit < q_grade[19]
#condCCC_m <- test_set$predictions_logit >= q_grade[19] & test_set$predictions_logit < q_grade[20]


#Construction de la variable grade V1.
#test_set$grade <- ifelse(condAAA, "AAA",
#                         ifelse(condAA_p, "AA+",
#                                ifelse(condAA, "AA",
#                                       ifelse(condAA_m, 'AA-',
#                                              ifelse(condA_p, "A+",
#                                                     ifelse(condA, "A",
#                                                            ifelse(condA_m, "A-",
#                                                                   ifelse(condBBB_p, "BBB+",
#                                                                          ifelse(condBBB, "BBB",
#                                                                                 ifelse(condBBB_m, "BBB-",
#                                                                                       ifelse(condBB_p, "BB+",
#                                                                                               ifelse(condBB, "BB",
#                                                                                                      ifelse(condBB_m, "BB-",
#                                                                                                             ifelse(condB_p, "B+",
#                                                                                                                    ifelse(condB, "B",
#                                                                                                                           ifelse(condB_m, "B-",
#                                                                                                                                  ifelse(condCCC_p, "CCC+",
#                                                                                                                                         ifelse(condCCC, "CCC",
#                                                                                                                                                ifelse(condCCC_m, "CCC-", "DDD")
#                                                                                                                                                )
#                                                                                                                                         )
#                                                                                                                                  )
#                                                                                                                           )
#                                                                                                                    )
#                                                                                                             )
#                                                                                                      )
#                                                                                               )
#                                                                                        )
#                                                                                 )
#                                                                          )
#                                                                 )
#                                                           )
#                                                    )
#                                             )
#                                     )
#                               )
#                         )

#CrossTable(test_set$grade, test_set$SeriousDlqin2yrs,
#          prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)


#Dernière version construction de la variable grade

#Création d'une variable contenant les quantiles
q_grade <- quantile(test_set$predictions_logit,
                    probs = seq(0, 1, by = .1))


#Construction des conditions
condAAA <- test_set$predictions_logit >= q_grade[1] & test_set$predictions_logit < q_grade[2]
condAA <- test_set$predictions_logit >= q_grade[2] & test_set$predictions_logit < q_grade[3]
condA <- test_set$predictions_logit >= q_grade[3] & test_set$predictions_logit < q_grade[4]
condBBB <- test_set$predictions_logit >= q_grade[4] & test_set$predictions_logit < q_grade[5]
condBB <- test_set$predictions_logit >= q_grade[5] & test_set$predictions_logit < q_grade[6]
condB <- test_set$predictions_logit >= q_grade[6] & test_set$predictions_logit < q_grade[7]
condCCC <- test_set$predictions_logit >= q_grade[7] & test_set$predictions_logit < q_grade[8]
condCC <- test_set$predictions_logit >= q_grade[8] & test_set$predictions_logit < q_grade[9]
condC <- test_set$predictions_logit >= q_grade[9] & test_set$predictions_logit < q_grade[10]


#Construction de la variable grade
test_set$grade <- ifelse(condAAA, "AAA",
                         ifelse(condAA, "AA",
                                ifelse(condA, "A",
                                       ifelse(condBBB, 'BBB',
                                              ifelse(condBB, "BB",
                                                     ifelse(condB, "B",
                                                            ifelse(condCCC, "CCC",
                                                                   ifelse(condCC, "CC",
                                                                          ifelse(condC, "C", "DDD")
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )

# Validation de la variable par un crosstable avec la variable dépendante
CrossTable(test_set$grade, test_set$SeriousDlqin2yrs,
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)



#===============================================================================
# Test Contruction variable croisée

#Construire une variable croisée avec qual_3059 et qual_6089
#df_quali <- transform(df_quali, qual_3059_6089=paste(qual_3059, " & ", qual_6089))

#Construire une variable croisée avec qual_OpenCredit et qual_REloans
#df_quali <- transform(df_quali, qual_OpenCredit_REloans=paste(qual_OpenCredit, " & ", qual_REloans))


