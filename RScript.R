train = read.csv("criminal_train.csv")

table(is.na(train))
colSums(is.na(train))

dim(train)
str(train)

inputvariables = names (train[2:71])
inputvariables

formula.att = as.formula(paste("Criminal~", paste(inputvariables, collapse = " + ")))
formula.att

model = glm (formula.att, train[2:72], family = "binomial")

library(lmtest)
lrtest(model)

library(pscl)
pR2(model)

summary(model)

confint(model)
exp(coef(model))
exp(confint(model))

Prediction=fitted(model)
Prediction
cutoff=floor(Prediction+0.5)
cutoff
table(Actual=train$Criminal, Predicted=cutoff)
accuracy = (41824+1096) / (41824+1096+719+2079)
accuracy

test = read.csv("criminal_test.csv", header = TRUE)
table(is.na(test))
prediction=predict(model, newdata = test[2:71], type = "response")
test$Criminal = floor(prediction+0.50)

result =subset(test, select = c(PERID, Criminal))

write.csv(result, "submission.csv")

