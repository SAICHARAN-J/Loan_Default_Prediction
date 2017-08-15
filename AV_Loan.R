#Reading the training_set
dataset <- read.csv("train.csv")

#Factor Variables
dataset$Married <- factor(dataset$Married, levels = c("Yes","No"),labels = c(1,0))
dataset$Education <- factor(dataset$Education, levels = c("Graduate","Not Graduate"), labels = c(1,0))
dataset$Loan_Amount_Term <- as.factor(dataset$Loan_Amount_Term)
dataset$Credit_History <- as.factor(dataset$Credit_History)
dataset$Property_Area <- factor(dataset$Property_Area, levels = c("Urban","Rural","Semiurban"), labels = c(0,1,2))
dataset$Loan_Status <- factor(dataset$Loan_Status, levels = c("Y","N"), labels = c(1,0))
write.csv(dataset,"dataset_factored.csv")

#Analysis
colSums(is.na(dataset))
table(dataset$Gender, dataset$Loan_Status)
table(dataset$Education, dataset$Loan_Status)

#Mode parameter to Married column missing values.
married_table <- as.data.frame(table(dataset$Married))
dataset[is.na(dataset$Married),]$Married <- married_table[which.max(married_table$Freq),]$Var1

#Missing values in Credit_history
table(dataset$Credit_History,dataset$Loan_Status)
dataset[is.na(dataset$Credit_History),]$Credit_History  <- 1

#Checking Credibility
#Checking Co-Relation
cor.test((as.numeric(as.character(dataset$Credit_History))),as.numeric(as.character(dataset$Loan_Status)))
ggplot(dataset[!is.na(dataset$LoanAmount),], aes(x = sort(dataset[!is.na(dataset$LoanAmount),]$ApplicantIncome), y = sort(dataset[!is.na(dataset$LoanAmount),]$LoanAmount))) + geom_point()
cor.test(dataset$ApplicantIncome,dataset$LoanAmount)
#Polynomial Regression for Missing Values - Loan Amount

poly <- data.frame(LoanAmount = dataset$LoanAmount, Income = dataset$ApplicantIncome)

poly$appincome4 = poly$Income ^ 4
poly$appincome5 = poly$Income ^ 5
poly$appincome6 = poly$Income ^ 6

train <- poly[!is.na(poly$LoanAmount),]
test <- poly[is.na(poly$LoanAmount),]

regressor <- lm(LoanAmount ~ . , data = poly)
summary(regressor)

prediction <- predict(regressor, test[c(-1)])
prediction <- round(prediction)

test$LoanAmount <- prediction

#fitting missing loan amount values to the dataset
dataset[is.na(dataset$LoanAmount),]$LoanAmount <- prediction

Loan_Term_Table <- as.data.frame(table(dataset$Loan_Amount_Term))
dataset[is.na(dataset$Loan_Amount_Term),]$Loan_Amount_Term <- Loan_Term_Table[which.max(Loan_Term_Table$Freq),]$Var1

#------------------------------
data <- dataset
data$year <- (data$LoanAmount * 1000) / as.numeric(as.character(data$Loan_Amount_Term)) 
data$year <- data$year * 12
data$diff <- data$ApplicantIncome - data$year
data$diff_value <- ifelse(data$diff > 0 , 1 , 0)

data <- data[c(-1,-14,-15)]
data <- data[c(-3,-5,-9)]
data <- data[c(-1)]

classifier <- glm(Loan_Status ~ . , family = binomial, data)
summary(classifier)

#------------------------------
dataset <- read.csv("test.csv")
View(dataset)

#Factor Variables
dataset$Married <- factor(dataset$Married, levels = c("Yes","No"),labels = c(1,0))
dataset$Education <- factor(dataset$Education, levels = c("Graduate","Not Graduate"), labels = c(1,0))
dataset$Loan_Amount_Term <- as.factor(dataset$Loan_Amount_Term)
dataset$Credit_History <- as.factor(dataset$Credit_History)
dataset$Property_Area <- factor(dataset$Property_Area, levels = c("Urban","Rural","Semiurban"), labels = c(0,1,2))
write.csv(dataset,"dataset_factored_test.csv")

#Analysis
colSums(is.na(dataset))

dataset[is.na(dataset$Credit_History),]$Credit_History  <- 1

#Polynomial Regression for Missing Values - Loan Amount

poly <- data.frame(LoanAmount = dataset$LoanAmount, Income = dataset$ApplicantIncome)

poly$appincome4 = poly$Income ^ 4
poly$appincome5 = poly$Income ^ 5
poly$appincome6 = poly$Income ^ 6

train <- poly[!is.na(poly$LoanAmount),]
test <- poly[is.na(poly$LoanAmount),]

regressor <- lm(LoanAmount ~ . , data = poly)
summary(regressor)

prediction <- predict(regressor, test[c(-1)])
prediction <- round(prediction)

test$LoanAmount <- prediction

#fitting missing loan amount values to the dataset
dataset[is.na(dataset$LoanAmount),]$LoanAmount <- prediction

Loan_Term_Table <- as.data.frame(table(dataset$Loan_Amount_Term))
dataset[is.na(dataset$Loan_Amount_Term),]$Loan_Amount_Term <- Loan_Term_Table[which.max(Loan_Term_Table$Freq),]$Var1

#------------------------------
data <- dataset
data$year <- (data$LoanAmount * 1000) / as.numeric(as.character(data$Loan_Amount_Term)) 
data$year <- data$year * 12
data$diff <- data$ApplicantIncome - data$year
data$diff_value <- ifelse(data$diff > 0 , 1 , 0)

data <- data[c(-1,-13,-14)]
data <- data[c(-3,-5,-9)]
data <- data[c(-1)]

y_pred <- predict(classifier, type = "response", newdata = data)
y_pred <- ifelse(y_pred > 0.5, "N","Y")
table(y_pred)

av <- data.frame(Loan_ID = dataset$Loan_ID, Loan_Status = y_pred)

write.csv(av,"av_loan.csv",row.names = F)
