train = read.csv("train.csv", header = TRUE)
test = read.csv("test.csv", header = TRUE)

table (is.na(train))
train$store_location <- as.numeric(factor(train$store_location , 
                                  levels=c("AT-WORK", "EDUCATIONAL", "EATING & DRINKING",
                                           "THIRD PARTY (NON-CONSUMER)",
                                           "GROCERY SHOPPING",
                                           "TRAVEL/TRANSPORTATION/HOSPITALITY",
                                           "OTHER SHOPPING & SERVICES",
                                           "ENTERTAINMENT/RECREATION/LEISURE")))

train$time_zone = as.numeric(factor(train$time_zone, 
                                    levels=c("EST", "PST", "MST", "CST")))

train$location_employee_code = as.numeric(factor(train$location_employee_code, 
                              levels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")))

train$credit_score = as.numeric(factor(train$credit_score, 
                            levels=c("A+", "A", "B+", "B", "C+", "C", "I", "P", "U")))


test$store_location <- as.numeric(factor(test$store_location , 
                                          levels=c("AT-WORK", "EDUCATIONAL", "EATING & DRINKING",
                                                   "THIRD PARTY (NON-CONSUMER)",
                                                   "GROCERY SHOPPING",
                                                   "TRAVEL/TRANSPORTATION/HOSPITALITY",
                                                   "OTHER SHOPPING & SERVICES",
                                                   "ENTERTAINMENT/RECREATION/LEISURE")))

test$time_zone = as.numeric(factor(test$time_zone, 
                                    levels=c("EST", "PST", "MST", "CST")))

test$location_employee_code = as.numeric(factor(test$location_employee_code, 
                                                 levels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")))

test$credit_score = as.numeric(factor(test$credit_score, 
                                       levels=c("A+", "A", "B+", "B", "C+", "C", "I", "P", "U")))


inputVariables = names(train[-(1:5)])
formula.dev <- as.formula(paste("total_sales~", paste(inputVariables[!inputVariables %in% "total_sales"], collapse = " + ")))
formula.dev

mlm = lm(formula.dev, data=train)
mlm
names(test)

test$total_sales_actual = predict(mlm, newdata = test)
result =subset(test, select = c(outlet_no, total_sales_actual))

write.csv(result, "submission.csv")

