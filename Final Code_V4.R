library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(boot)
library(caret)
library(randomForest)
library(MASS)

setwd("D:\\Trimester 1\\AN6003 Analytics strategy\\group project")
data <- fread('simulated HF mort data for GMPH (1K) final.csv', stringsAsFactors=T, na.strings = c("")) 

#check for NA values
sum(is.na(data))  
summary(data)

#10 0/blank values from quintile; 6 blank values alr detected as na; 43 NA values from ethnic group

## some values in quintile = 0, assuming no info
data[data$quintile==0] <- NA
table(data$quintile)

data3 <- na.omit(data)[] 
sum(is.na(data3))  
summary(data3)


#set that we are working with = 949


#labelling the variable and changing all to factor
data2 <- data3

data2$death <- factor(data2$death, levels = c(0,1),
                           labels = c('non-death', 'death'))
data2$gender <- factor(data2$gender, levels = c(1,2),
                      labels = c('male', 'female'))

data2$cancer <- factor(data2$cancer, levels = c(0,1),
                       labels = c('non-cancer', 'cancer'))
data2$cabg <- factor(data2$cabg, levels = c(0,1),
                       labels = c('non-bypass', 'bypass'))
data2$crt <- factor(data2$crt, levels = c(0,1),
                       labels = c('non-device', 'device'))
data2$defib <- factor(data2$defib, levels = c(0,1),
                       labels = c('non-defib', 'defib'))
data2$dementia <- factor(data2$dementia, levels = c(0,1),
                         labels = c('non-dementia', 'dementia'))
data2$diabetes <- factor(data2$diabetes, levels = c(0,1),
                       labels = c('non-diabetes', 'diabetes'))
data2$hypertension <- factor(data2$hypertension, levels = c(0,1),
                         labels = c('non-hypertension', 'hypertension'))
data2$ihd <- factor(data2$ihd, levels = c(0,1),
                         labels = c('non-ihd', 'ihd'))
data2$mental_health <- factor(data2$mental_health, levels = c(0,1),
                         labels = c('non-mental', 'mental'))
data2$arrhythmias <- factor(data2$arrhythmias, levels = c(0,1),
                         labels = c('non-arrhythmias', 'arrhythmias'))
data2$copd <- factor(data2$copd, levels = c(0,1),
                         labels = c('non-copd', 'copd'))
data2$obesity <- factor(data2$obesity, levels = c(0,1),
                         labels = c('non-obese', 'obese'))
data2$pvd <- factor(data2$pvd, levels = c(0,1),
                        labels = c('non_pvd', 'pdv'))
data2$renal_disease <- factor(data2$renal_disease, levels = c(0,1),
                        labels = c('non-renal_disease', 'renal_disease'))
data2$valvular_disease <- factor(data2$valvular_disease, levels = c(0,1),
                        labels = c('non-val_disease', 'val_disease'))
data2$metastatic_cancer <- factor(data2$metastatic_cancer, levels = c(0,1),
                        labels = c('non-meta_cancer', 'meta_cancer'))
data2$pacemaker <- factor(data2$pacemaker, levels = c(0,1),
                        labels = c('non-pacemaker', 'pacemaker'))
data2$pneumonia <- factor(data2$pneumonia, levels = c(0,1),
                        labels = c('non-pneumonia', 'pneumonia'))
data2$pci <- factor(data2$pci, levels = c(0,1),
                        labels = c('non-pci', 'pci'))
data2$stroke <- factor(data2$stroke, levels = c(0,1),
                        labels = c('non-stroke', 'stroke'))
data2$senile <- factor(data2$senile, levels = c(0,1),
                        labels = c('non-senile', 'senile'))
data2$quintile <- factor(data2$quintile, levels = c(1,2,3,4,5),
                        labels = c('most affluent', 'second affluent','third affluent','fourth affluent', 'poorest'))
data2$ethnicgroup <- factor(data2$ethnicgroup, levels = c(1,2,3,8,9),
                        labels = c('white', 'black','indian subcontinent','not known','other'))

summary(data2)


#================================================================#
#================================================================#

#exploration
library(ggplot2)


summary(data2)


#plot for number of deaths - overview
ggplot(data2, aes(x=death)) +
  geom_bar(position="identity", fill=c("cadetblue","orange")) + 
  scale_x_discrete(labels=c('non-death','death'))


#death vs quintile -- no clear pattern between death and quintile
ggplot(data2, aes(x=quintile, fill=death) ) +
  geom_bar(position="dodge") + 
  scale_x_discrete(labels=c('Most Affulent', 'Second Affluent', 'Third Affluent', 'Fourth Affluent', 'Poorest'))+
  scale_fill_manual(values=c("cadetblue",
                             "orange"), labels = c("non-death","death"))

#as age increase, the number of missed appt decreases
ggplot(data2, aes(x = age, y = prior_dnas)) +
  geom_point(colour="cadetblue")+
  labs(y="No. of missed appt")

cor(data2$age,data2$prior_dnas)
#negative correlation -0.16 btw age and appt missed

  

#age and death -- death happens at older age
y= boxplot(age~death,
        data=data2,
        xlab="condition",
        ylab="age",
        col=c("cadetblue","orange"),
        border="brown")
y
list(y)
#min age of non-death = 44, 1st quartile = 68, mean=77, 3rd quartile=85, max=101
#min age of death = 62, 1st quartile = 77, median =84, 3rd quartile = 88, max=102

#age by gender females in the dataset are of higher age
z = boxplot(age~gender,
           data=data2,
           xlab="gender",
           ylab="age",
           col=c("lightblue","pink"),
           border="brown")
z
list(z)




ggplot(data2, aes(x=age)) + 
  geom_histogram(binwidth = 10, color="black", fill="mediumorchid2")


#death and los
a = boxplot(los~death,
            data=data2,
            xlab="condition",
            ylab="length of stay",
            col=c("cadetblue","orange"),
            border="brown")
a
list(a)

#death and gender - relatively equal proportion
ggplot(data2, aes(x=death, fill=gender) ) +
geom_bar(position="dodge") + 
  scale_fill_manual(values=c("lightblue",
                             "pink"))

#no clear pattern/trend for length of stay and age
ggplot(data2, aes(x=age, y=los) ) +
  geom_line(color="mediumorchid2") +
  labs(y="Length of Stay")
cor(data2$los,data2$age)
#0.0834 as correlation




#================================================================#
#================================================================#

## Split Data
levels(data2$death)
set.seed(1)
train <- sample.split(Y=data2$death, SplitRatio = 0.85)
trainset <- subset(data2, train == T)
testset <- subset(data2, train == F)
trainset=trainset[,-1]
testset=testset[,-1]

#================================================================#
####################### Logistic Regression ######################
#================================================================#
lr_model <- glm(death ~ . , data = trainset, family = binomial(link = "logit"))


## test on the testset
pred_lr = ifelse(predict(lr_model, newdata = testset, type = "response") >= 0.5, 1, 0)
cm_lr = table(Actual = testset$death, predicted = pred_lr)

accuracy_lr = (cm_lr[1,1] + cm_lr[2,2])/sum(cm_lr)
accuracy_lr


step.model <- stepAIC(lr_model, direction = "both",
                      trace = FALSE)
coef(summary(step.model))

final_features <- c("death","los", "age", "gender", "cabg", "dementia", "ihd", "arrhythmias", "copd",
                    "pvd" ,"metastatic_cancer", "prior_dnas", "ethnicgroup", "pci")

trainset_new <- trainset[,..final_features]
testset_new <- testset[,..final_features,]

optimized_model <- glm(death ~ ., data = trainset_new, family = binomial(link = 'logit'))

pred_new = ifelse(predict(optimized_model, newdata = testset_new, type = "response") >= 0.5, 1, 0)
cm_new = table(Actual = testset_new$death, predicted = pred_new)

accuracy_new = (cm_new[1,1] + cm_new[2,2])/sum(cm_new)
accuracy_new

VarImportance <- varImp(optimized_model, scale = FALSE)
VarImportance <- cbind(Variable = rownames(VarImportance), VarImportance)
rownames(VarImportance) <- 1:nrow(VarImportance)

ggplot(data = VarImportance, aes(x = reorder(Variable, Overall), y = Overall, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Variables", y = "Importance") +
  guides(fill = "none") +
  coord_flip()


#================================================================#
########################## Decision Tree #########################
#================================================================#
tree = rpart(formula = death ~., data = trainset, method = 'class',  control = rpart.control(minsplit = 2, cp = 0))
tree$variable.importance
printcp(tree)
plotcp(tree)
cverror_salary.cap = tree$cptable[which.min(tree$cptable[,"xerror"]), "xerror"] +
  tree$cptable[which.min(tree$cptable[,"xerror"]), "xstd"]
a <- 1; b <- 4
while (tree$cptable[a,b] > cverror_salary.cap) {
  a <- a + 1
}
cp_salary.opt <- ifelse(a > 1, sqrt(tree$cptable[a, 1] * tree$cptable[a-1, 1]), 1)
treePruned <- prune(tree, cp = cp_salary.opt)
rpart.plot(treePruned, main = "Pruned Tree with cp = 0.0289")
pred <- predict(treePruned, newdata = testset,type = 'class')
cm <-table(testset$death,pred)
(cm[1,1]+cm[2,2])/sum(cm)
round(cp_salary.opt,4)

#================================================================#
########################## Random Forest #########################
#================================================================#

customRF <- list(type = "Classification",
                 library = "randomForest",
                 loop = NULL)

customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs) {
  randomForest(x, y,
               mtry = param$mtry,
               ntree=param$ntree)
}

#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

####
set.seed(1)
control <- trainControl(method="cv", 
                        number=5, 
                        allowParallel = TRUE)

tunegrid <- expand.grid(.mtry=c(1:15),.ntree=c(500,700,900))
metric = "Accuracy"
custom <- train(death~., data=trainset, 
                method=customRF, 
                metric=metric, 
                tuneGrid=tunegrid, 
                trControl=control)

summary(custom)
plot(custom)

custom$bestTune

custom$results

# Fit the best random forest
set.seed(1)
rf.fit<-randomForest(death~.,data=trainset,mtry=14,ntree=500)
varImpPlot(rf.fit)

# Accuracy rate on test data
yhat<-predict(rf.fit,testset,type="class")
cm=table(testset$death,yhat)
(cm[1,1]+cm[2,2])/sum(cm)



