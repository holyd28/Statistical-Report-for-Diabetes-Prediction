# importing packages
library(mice)
library(caret)
library(class)
library(rpart)
library(rpart.plot)
library(ROCR)


# loading dataset
raw_data = read.csv("diabetes-dataset.csv")
# adjusting categorical variables to be factors
raw_data$hypertension = as.factor(raw_data$hypertension)
raw_data$heart_disease = as.factor(raw_data$heart_disease)
raw_data$diabetes = as.factor(raw_data$diabetes)


### ====== PART I: EDA ======

dim(raw_data)
head(raw_data)
tail(raw_data)
summary(raw_data)

## observing the distributions of each feature variable 
## & the response variable in the data set

# pie chart to observe distribution of diabetes status

par(mfrow=c(1,5))

par(mar = c(1 ,1 ,1 ,1))
freq_diabetes = table(raw_data$diabetes)
lab_diabetes = paste(names(freq_diabetes), "\n", 
                     round(100 * freq_diabetes / sum(freq_diabetes), 1), 
                     "%", sep = "")
col_diabetes = c("blue3","orange3")
density_diabetes = c(45, 60)            
angle_diabetes = c(45, 135)

pie(freq_diabetes,
    labels = lab_diabetes,
    col = col_diabetes,
    density = density_diabetes,
    angle = angle_diabetes,
    border = "black",
    main = "Distribution of Diabetes Status")


# pie chart to observe distribution of gender

par(mar = c(1 ,1 ,1 ,1))
freq_gender = table(raw_data$gender)
lab_gender = paste(names(freq_gender), "\n", round(100 * freq_gender / sum(freq_gender), 1), "%", sep="")
col_gender = c("skyblue3","orange4")
density_gender = c(45, 60)            
angle_gender = c(45, 135)             

pie(freq_gender,
    labels = lab_gender,
    col = col_gender,
    density = density_gender,
    angle = angle_gender,
    border = "black",
    main = "Distribution of Gender")

# pie chart to observe distribution of hypertension status

par(mar = c(1 ,1 ,1 ,1))
freq_hypertension = table(raw_data$hypertension)
lab_hypertension = paste(names(freq_hypertension), "\n", round(100 * freq_hypertension / sum(freq_hypertension), 1), "%", sep="")
col_hypertension = c("lightblue3","orange4")
density_hypertension = c(45, 60)            
angle_hypertension = c(45, 135)             

pie(freq_hypertension,
    labels = lab_hypertension,
    col = col_hypertension,
    density = density_hypertension,
    angle = angle_hypertension,
    border = "black",
    main = "Distribution of Hypertension Status")


# pie chart to observe distribution of heart disease status

par(mar = c(1 ,1 ,1 ,1))
freq_heart = table(raw_data$heart_disease)
lab_heart = paste(names(freq_heart), "\n", round(100 * freq_heart / sum(freq_heart), 1), "%", sep="")
col_heart = c("cyan4","orange")
density_heart = c(45, 60)            
angle_heart = c(45, 135)             

pie(freq_heart,
    labels = lab_heart,
    col = col_heart,
    density = density_heart,
    angle = angle_heart,
    border = "black",
    main = "Distribution of Heart Disease Status")


# pie chart to observe proportion of smoking history

par(mar = c(1 ,1 ,1 ,1))
freq_smoking = table(raw_data$smoking_history)
lab_smoking = paste(names(freq_smoking), "\n", round(100 * freq_smoking / sum(freq_smoking), 1), "%", sep="")
col_smoking = c("cyan","lightblue","skyblue2","royalblue","darkblue","blue")
density_smoking = c(30,45,60,75,90)            
angle_smoking = c(20,40,60,80,100)             

pie(freq_smoking,
    labels = lab_smoking,
    col = col_smoking,
    density = density_smoking,
    angle = angle_smoking,
    border = "black",
    main = "Distribution of Smoking History")

par(mfrow=c(1,1))

par(mfrow=c(1,4))
# histogram to observe distribution of age

hist(raw_data$age, freq=F, main="Age", xlab = "Age", ylab = "Density", col="skyblue4", border="black")
lines(density(raw_data$age), col="orange", lwd=2)

# histogram to observe the distribution of bmi

hist(raw_data$bmi, freq=F, main="BMI", xlab = "BMI", ylab = "Density", col="royalblue", border="black")
lines(density(raw_data$bmi), col="orange", lwd=2)
# histogram to observe the distributino of HbA1c level

hist(raw_data$HbA1c_level, freq=F, main="HbA1c Level", xlab = "HbA1c Level", ylab = "Density", col="darkblue", border="black", breaks=18, ylim=c(0.0,0.8))
lines(density(raw_data$HbA1c_level), col="orange", lwd=2)

# histogram to observe the distribution of blood glucose level

hist(raw_data$blood_glucose_level, freq=F, main="Blood Glucose Level", xlab = "Blood Glucose Level", ylab = "Density", col="blue", border="black")
lines(density(raw_data$blood_glucose_level), col="orange", lwd=2)

par(mfrow=c(1,1))
## investigating the association between each categorical feature variable & response variable "diabetes"

par(mfrow=c(1,4))
# gender
tab_gender = table(raw_data$gender, raw_data$diabetes)
prop_tab_gender = prop.table(tab_gender, 1)

barplot((prop_tab_gender[,2]), col=("lightblue"), border="black",
        main="Proportion of Diabetes Status by Gender", ylab="Proportion", xlab="Gender", ylim=c(0.00,0.1))


# hypertension status
tab_hypertension = table(raw_data$hypertension, raw_data$diabetes)
prop_tab_hypertension = prop.table(tab_hypertension, 1)

barplot((prop_tab_hypertension[,2]), col=("blue"), border="black",
                     main="Proportion of Diabetes Status by Hypertension Status", ylab="Proportion", xlab="Hypertension Status", ylim=c(0.00,0.3))

hypertension_o.r = (prop_tab_hypertension[2,2]/prop_tab_hypertension[2,1])/(prop_tab_hypertension[1,2]/prop_tab_hypertension[1,1]); hypertension_o.r # 5.195207
# odds of someone who has diabetes having hypertension is 5.195207 times another one who does not have hypertension


# heart disease status
tab_heart = table(raw_data$heart_disease, raw_data$diabetes)
prop_tab_heart = prop.table(tab_heart, 1)

barplot((prop_tab_heart[,2]), col=("royalblue"), border="black",
        main="Proportion of Diabetes Status by Heart Disease Status", ylab="Proportion", xlab="Heart Disease Status", ylim=c(0.00,0.35))

heart_o.r = (prop_tab_heart[2,2]/prop_tab_heart[2,1])/(prop_tab_heart[1,2]/prop_tab_heart[1,1]);heart_o.r # 5.816605
# odds of a person with diabetes having heart disease is 5.816605 times another one without heart disease


# smoking history
tab_smoking = table(raw_data$smoking_history, raw_data$diabetes)
prop_tab_smoking = prop.table(tab_smoking, 1)

barplot((prop_tab_smoking[,2]), col=("darkblue"), border="black",
        main="Proportion of Diabetes Status by Smoking History", ylab="Proportion", xlab="Smoking History", ylim=c(0.00,0.2))

par(mfrow=c(1,1))
## investigating the association between each quantitaive feature variable & response variable "diabetes"

par(mfrow=c(1,4))
# age
box_age_diabetes = boxplot(age~diabetes, raw_data, main="Boxplot of Diabetes Status with Age",
                           xlab="Diabetes Status", ylab="Age",col=c("green","red"))
box_age_diabetes$stats[3,]
# [1] 40 62
# median age of those with & without diabetes is 62 & 40 respectively

length(box_age_diabetes$out)
# [1] 118
# there are 118 outliers among those with diabetes

# the median age of those with diabetes is higher than that of those without diabetes, which suggests there may be an association between age & diabetes status
# the distribution of age for those without diabetes is unimodal & rather symmetrical about the median age which is 40, suggesting that the distribution might be normal
# the distribution of age for those with diabetes is unimodal & highly left skewed about the median age which is 62, suggesting that the distribution might not be normal
# furthermore, the distribution of age for those with diabetes contains 118 outliers

## investigating the outliers

# from the boxplot, we know that the all outliers have diabetes
age_outliers_index = which(raw_data$age %in% box_age_diabetes$out & raw_data$diabetes == 1)
age_outliers = raw_data[age_outliers_index,]
summary(age_outliers)


# bmi
box_bmi_diabetes = boxplot(bmi~diabetes, raw_data, main="Boxplot of Diabetes Status with BMI",
                           xlab="Diabetes Status", ylab="BMI",col=c("darkgreen","darkred"))
box_bmi_diabetes$stats[3,]
# [1] 27.32 29.97
# median BMI of those with & without diabetes is 29.97 & 27.32 respectively

length(box_bmi_diabetes$out)
# [1] 7065
# there are a total of 7065 outliers among those with & without diabetes

# the median BMI among those with diabetes is higher than that among those without diabetes, which may suggest an association between BMI & diabetes status
# the distribution of BMI for those with & without are both unimodal & highly right skewed, with both containing outliers, suggesting that both distributions may not be normal

## investigating the outliers

# since outliers are present in both outcomes for diabetes status, extract the information of outliers by their row indices

# function to find outlier row indices
get_outliers = function(x) {
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = Q3 - Q1
  return (which(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR)))
}

# get indices of outliers among those without diabetes
bmi_0_index = which(raw_data$diabetes == 0)
bmi_0_outliers_index = get_outliers(raw_data$bmi[bmi_0_index])
bmi_0_outliers_rows = bmi_0_index[bmi_0_outliers_index]

# get indices of outliers among those with diabetes
bmi_1_index = which(raw_data$diabetes == 1)
bmi_1_outliers_index = get_outliers(raw_data$bmi[bmi_1_index])
bmi_1_outliers_rows = bmi_1_index[bmi_1_outliers_index]

# extract rows of outliers from data
bmi_outliers = raw_data[c(bmi_0_outliers_rows, bmi_1_outliers_rows), ]
summary(bmi_outliers)


# HbA1c levels
box_Hb1Ac_diabetes = boxplot(HbA1c_level~diabetes, raw_data, main="Boxplot of Diabetes Status with HbA1c Level",
                             xlab="Diabetes Status", ylab="HbA1c Level",col=c("lightgreen","maroon"))
box_Hb1Ac_diabetes$stats[3,]
# [1] 5.8 6.6
# median HbA1c level is 6.6 & 5.8 among those with & without diabetes respectively

# the median HbA1c level among those with diabetes is greater than that of those without diabetes, which could suggest an association between HbA1c levels & diabetes status
# the distribution of HbA1c levels for those with diabetes is unimodal & left skewed while that for those without diabetes is unimodal & right skewed
# this suggests that both distributions may not be normal
# no outliers present in both distributions

# blood glucose levels
box_glucose_diabetes = boxplot(blood_glucose_level~diabetes, raw_data, main="Boxplot of Diabetes Status with Blood Glucose Level",
                               xlab="Diabetes Status", ylab="Blood Glucose Level",col=c("forestgreen","red2"))
box_glucose_diabetes$stats[3,]
# [1] 140 160
# the median blood glucose level is 160 & 140 among those with & without diabetes respectively

# the median blood glucose level among those with diabetes is greater than that of those without diabetes, which suggests there may be an association between blood glucose level & diabetes status
# the distribution of blood glucose levels for those with diabetes is unimodal & right skewed, which suggests that the distribution may not be normal
# the distribution of blood glucose levels for those without diabetes is unimodal & rather symmetrical about the median, suggesting that the distribution could be normal
# no outliers present in both distributions

## investigating the strength of association between blood glucose levels & diabetes status by comparing their effect size

par(mfrow=c(1,1))


## imputating rows with "no info" for smoking history

# determining randomness of "No Info"
chisq.test(table(raw_data$smoking_history, raw_data$diabetes))
# p-value < 2.2e-16

# the low p-value suggests that there 
 # is sufficient evidence to conclude that there is strong association between diabetes & smoking history. 
 # From this we can assume that the missingness mechanism of “No Info” should be considered as 
 # Missing at Random & not Missing Completely at Random. 
 # This justifies the before suggested approach to carry out multiple imputation.
 
 
raw_data$smoking_history[raw_data$smoking_history == "No Info"] = NA
raw_data$smoking_history = as.factor(raw_data$smoking_history)

method_vector = rep("", ncol(raw_data[,-9]))
method_vector[which(names(raw_data) == "smoking_history")] = "polyreg"

imp = mice(data = raw_data[,-9],
           method = method_vector,
           m = 50,       
           seed = 611)

data = complete(imp, 1)
imputated = cbind(data,diabetes=raw_data$diabetes)

compare = table(data$smoking_history)

# comparing proportions of each value for smoking history
prop.table(compare)
prop.table(freq_smoking)


base = glm(diabetes~., imputated, family=binomial)
summary(base)

# regroup smoking into "former", "never", "current"

for (x in seq(length(imputated$smoking_history))) {
  if (imputated$smoking_history[x] == "ever" | imputated$smoking_history[x] == "former") {
    imputated$smoking_history[x] = "former"
  } else if (imputated$smoking_history[x] == "not current" | imputated$smoking_history[x] == "never") {
    imputated$smoking_history[x] = "never"
  }
}
imputated$smoking_history = factor(imputated$smoking_history, 
       levels=c("never", "former", "current"), 
       ordered=T)
### ====== PART II: METHODS ======


set.seed(611)
n_folds = 5

og.cat.var = imputated[, c("diabetes", "gender", "hypertension", "heart_disease", "smoking_history")]
imputated$cat = interaction(og.cat.var, drop=TRUE)
og.folds = createFolds(y = imputated$cat, k = n_folds, list = TRUE, returnTrain = FALSE)
imputated$cat = NULL

length(og.folds$Fold1) # 20001
length(og.folds$Fold2) # 19999
length(og.folds$Fold3) # 19996
length(og.folds$Fold4) # 20004
length(og.folds$Fold5) # 20000


# size of each fold is different to preserve proportions between each categorical variable
# in unbalanced datasets where some classes
# occur much less frequently than others, equal sized folds could skew class 
# representation—leading to poor performance evaluation especially on minority classes

# use those in 5th fold of data set to build the models

indicies = og.folds$Fold5
build.imputated = imputated[indicies,]

build.cat.var = build.imputated[, c("diabetes", "gender", "hypertension", "heart_disease", "smoking_history")]
build.imputated$cat = interaction(build.cat.var, drop=TRUE)
build.folds = createFolds(y = build.imputated$cat, k = n_folds, list = TRUE, returnTrain = FALSE)
build.imputated$cat = NULL

build.imputated$smoking_history = as.character(build.imputated$smoking_history)

## KNN:
knn_imputated = build.imputated[,c(2,3,4,6,7,8,9)]
knn_imputated[,2] = ifelse(knn_imputated$hypertension == "0", 0, 1)
knn_imputated[,3] = ifelse(knn_imputated$heart_disease == "0", 0, 1)
knn_imputated[,-7] = scale(knn_imputated[,-7])
nrow(knn_imputated)^0.5 # 141.4214
k_val = seq(1,141,2)
knn.tpr = numeric()

for (j in k_val) {
  n_folds.tpr = numeric()
  for (i in 1:n_folds) {
    test.index = build.folds[[i]]
    test.set = knn_imputated[test.index,]
    train.set = knn_imputated[-test.index,]
    pred = knn(train.set[,-7],test.set[,-7],train.set[,7],k=j)
    c.m = table(test.set[,7],pred)
    tpr = c.m[2,2]/sum(c.m[2,])
    n_folds.tpr = append(n_folds.tpr,tpr)
  }
  knn.tpr = append(knn.tpr, mean(n_folds.tpr))
}

knn.frame = data.frame(K=k_val,TPR=knn.tpr)
knn.frame[order(knn.frame$TPR, decreasing = TRUE), ]
best.k_index = which.max(knn.frame$TPR)
best.k_val = knn.frame$K[best.k_index]; best.k_val # 1
best.k_tpr = knn.frame$TPR[best.k_index]; best.k_tpr # 0.6745021

plot(knn.frame$K, knn.frame$TPR, type = "l", col = "blue",
     main = "True Positive Rate against Values of K",
     xlab = "Value of K", 
     ylab = "True Positive Rate")
abline(v = best.k_val, col = "orange", lty = 2)
points(best.k_val, best.k_tpr, col = "orange", pch = 4)
text(best.k_val, best.k_tpr, labels = paste("K =", best.k_val), pos = 4, col = "orange")

## DECISION TREE:
minsplit_val = 1:50
maxdepth_val = 2:10
cp_val = 10^(-5:-1)
dt.frame = data.frame(CP=numeric(),MinSplit=numeric(),MaxDepth=numeric(),TPR=numeric())

for (g in cp_val) {
  for (h in minsplit_val) {
    for (j in maxdepth_val) {
      n_folds.tpr = numeric()
      for (i in 1:n_folds) {
        test.index = build.folds[[i]]
        test.set = build.imputated[test.index,]
        train.set = build.imputated[-test.index,]
        dt = rpart(diabetes~., data = train.set, method = "class", 
                   control = rpart.control(cp=g, minsplit=h, maxdepth=j), 
                   parms = list(split="information"))
        pred = predict(dt, test.set[,-9],type="class")
        c.m = table(test.set[,9],pred)
        tpr = c.m[2,2]/sum(c.m[2,])
        n_folds.tpr = append(n_folds.tpr,tpr)
      }
      new = data.frame(CP=g, MinSplit=h, MaxDepth=j, TPR=mean(n_folds.tpr))
      dt.frame = rbind(dt.frame, new)
    }
  }
}


dt.frame[order(dt.frame$TPR, decreasing = TRUE), ]
best.dt_index = which.max(dt.frame$TPR)
best.dt_cp_val = dt.frame$CP[best.dt_index]; best.dt_cp_val # 1e-05
best.dt_minsplit_val = dt.frame$MinSplit[best.dt_index]; best.dt_minsplit_val # 14
best.dt_maxdepth_val = dt.frame$MaxDepth[best.dt_index]; best.dt_maxdepth_val # 10
best.dt_tpr = dt.frame$TPR[best.dt_index]; best.dt_tpr # 0.6833159

plot(dt.frame$TPR~dt.frame$CP, col="blue", xlab="CP Values", ylab="True Positive Rate", main="True Positive Rate against CP Values")
plot(dt.frame$TPR~dt.frame$MinSplit, col="darkblue", xlab="MinSplit Values", ylab="True Positive Rate", main="True Positive Rate against MinSplit Values")
plot(dt.frame$TPR~dt.frame$MaxDepth, col="royalblue",xlab="MaxDepth Values", ylab="True Positive Rate", main="True Positive Rate against MaxDepth Values")




### ====== PART III EXTENDED: MODEL COMPARISON ======

# select folds 1 to 4 as train set & fold 5 as test set
imputated$smoking_history = as.character(imputated$smoking_history)
training_i = numeric()
for (x in 2:5) {
  training_i = append(training_i, unlist(og.folds[[x]]))
}
training = imputated[training_i, ]
testing = imputated[-training_i, ]

## KNN:
set.seed(611)

f.knn_imputated = imputated[,c(2,3,4,6,7,8,9)]
f.knn_imputated[,2] = ifelse(f.knn_imputated$hypertension == "0", 0, 1)
f.knn_imputated[,3] = ifelse(f.knn_imputated$heart_disease == "0", 0, 1)
f.knn_imputated[,-7] = scale(f.knn_imputated[,-7])
knn_training = f.knn_imputated[training_i, ]
knn_testing = f.knn_imputated[-training_i,]

knn_pred = knn(knn_training[,-7], knn_testing[,-7], knn_training[,7], k = best.k_val, prob = TRUE)

### TPR for KNN 
knn_c.m = table(knn_testing[,7], knn_pred)
knn_tpr = knn_c.m[2,2]/sum(knn_c.m[2,]); knn_tpr # 0.7168038

### Precision for KNN
knn_prec = knn_c.m[2,2]/sum(knn_c.m[,2]); knn_prec # 0.7393939

### AUC for KNN
knn_winning.prob= attr(knn_pred, "prob")
knn_prob = ifelse(knn_pred == 1, knn_winning.prob, 1 - knn_winning.prob) 
knn_rocr = prediction(knn_prob, knn_testing[,7])
knn_perf = performance(knn_rocr, "tpr", "fpr")
knn_auc = performance(knn_rocr, "auc")@y.values[[1]]; knn_auc # 0.8486042


## DECISION TREE:
set.seed(611)
best_dt = rpart(diabetes~., data = training, method = "class",
           control = rpart.control(cp = best.dt_cp_val, maxdepth = best.dt_maxdepth_val, minsplit = best.dt_minsplit_val),
           parms = list(split = "information"))
rpart.plot(best_dt, type=4, extra=2, varlen=0, faclen=0, clip.right.labs=FALSE) 
### TPR for Decision Tree
dt_pred.class = predict(best_dt, testing[,-9], type="class")
dt_c.m = table(testing[,9], dt_pred.class)
dt_tpr = dt_c.m[2,2]/sum(dt_c.m[2,]); dt_tpr # 0.6980024

### Precision for Decision Tree
dt_prec = dt_c.m[2,2]/sum(dt_c.m[,2]); dt_prec # 0.9690049

### AUC for Decision Tree
dt_pred.prob = predict(best_dt, testing[,-9], type="prob")
dt_pred.prob = dt_pred.prob[,2]
dt_rocr = prediction(dt_pred.prob, testing[,9])
dt_perf = performance(dt_rocr, "tpr", "fpr")
dt_auc = performance(dt_rocr, "auc")@y.values[[1]]; dt_auc # 0.9710294


## LOGISTIC REGRESSION:

logr = glm(diabetes~., data=training, family=binomial)
summary(logr)


logr_prob = predict(logr, newdata=testing[,-9], type="response")
logr_pred = ifelse(logr_prob > 0.5, "1", "0")
logr_c.m = table(testing[,9], logr_pred)

### TPR for Logistic Regression
logr_tpr = logr_c.m[2, 2]/sum(logr_c.m[2, ]); logr_tpr # 0.626322

### Precision for Logistic Regression
logr_prec = logr_c.m[2,2]/sum(logr_c.m[,2]); logr_prec # 0.8737705

### AUC for Logistic Regression
logr_rocr = prediction(logr_prob, testing[,9])
logr_perf = performance(logr_rocr, "tpr", "fpr")
logr_auc = performance(logr_rocr, "auc")@y.values[[1]]; logr_auc # 0.9623711



## COMPILING FINDINGS:
plot(knn_perf, col = "darkblue", main = "ROC for New Models", xlab = "False Positive Rate", ylab = "True Positive Rate", lwd=2)
plot(dt_perf, col = "darkgreen", add = TRUE, lwd=2)
plot(logr_perf, col = "maroon", add = TRUE, lwd=2)
abline(0, 1, lty = 2, col = "orange", lwd=2)

legend("bottomright", 
       legend = c(paste0("KNN (AUC=", round(knn_auc, 3), ")"),
                  paste0("Decision Tree (AUC=", round(dt_auc, 3), ")"),
                  paste0("Logistic Regression (AUC=", round(logr_auc, 3), ")")),
       col = c("darkblue", "darkgreen", "maroon"), lwd = 1, cex = 0.5)


res = data.frame(
  Model = c("KNN", "Decision Tree", "Logistic Regression"),
  TPR = c(knn_tpr, dt_tpr, logr_tpr),
  Precision = c(knn_prec, dt_prec, logr_prec),
  AUC = c(knn_auc, dt_auc, logr_auc)
)
print(res)

#                 Model       TPR Precision       AUC
# 1                 KNN 0.7168038 0.7393939 0.8486042
# 2       Decision Tree 0.6980024 0.9690049 0.9710294
# 3 Logistic Regression 0.6263220 0.8737705 0.9623711