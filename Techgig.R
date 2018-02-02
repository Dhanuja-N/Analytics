deoB = read.csv("Deodorant B.csv", header = TRUE)
deoF = read.csv("Deodorant F.csv", header = TRUE)
deoG = read.csv("Deodorant G.csv", header = TRUE)
deoH = read.csv("Deodorant H.csv", header = TRUE)
deoJ = read.csv("Deodorant J.csv", header = TRUE)

table(is.na(deoB))
table(is.na(deoF))
table(is.na(deoG))
table(is.na(deoH))
table(is.na(deoJ))

library(gtools)
dev = smartbind(deoB, deoF)
dev = smartbind(dev, deoG)
dev = smartbind(dev, deoH)
dev = smartbind(dev, deoJ)
dev[is.na(dev)] <- 0

dim(dev)
names(dev)
str(dev)
#library(mctest)
#imcdiag(x, dev$Instant.Liking)

deo.matrix <- model.matrix(~ Product - 1, data = dev)
dev.new <- data.frame(dev, deo.matrix)

inputVariables = names(dev.new[-(1:3)])
formula.dev <- as.formula(paste("Instant.Liking~", paste(inputVariables[!inputVariables %in% "Instant.Liking"], collapse = " + ")))
formula.dev

Model = glm(formula.dev, data=dev.new[4:77], family = binomial)

library(lmtest)
lrtest(Model)

library(pscl)
pR2(Model)

summary(Model)

confint(Model)
exp(coef(Model))
exp(confint(Model))

Prediction=fitted(Model)
Prediction
cutoff=floor(Prediction+0.5)
cutoff
table(Actual=dev.new$Instant.Liking, Predicted=cutoff)

library(Deducer)
rocplot(Model)

library(fmsb)
NagelkerkeR2(Model)

test = read.csv("test_data.csv", header = TRUE)
test[setdiff(names(dev), names(test))] <- 0
test = subset(test, select = -c(Instant.Liking))
deo.matrix <- model.matrix(~ Product - 1, data = test)
testdata <- data.frame(test, deo.matrix)

prediction=predict(Model, newdata = testdata[4:76], type = "response")
testdata$Instant.Liking = floor(prediction+0.50)
result =subset(testdata, select = c(Respondent.ID, Product, Instant.Liking))

write.csv(result, "submission.csv")

