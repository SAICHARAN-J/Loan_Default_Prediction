log <- read.csv("av_loan.csv")
svm <- read.csv("av_svm.csv")
rf <- read.csv("av_rr.csv")
dt <- read.csv("av_dt.csv")
ann <- read.csv("av_ann.csv")

ensemble <- data.frame(Loan_ID = log$Loan_ID, log = log$Loan_Status,
                       svm = svm$Loan_Status, rf = rf$Loan_Status,
                       dt = dt$Loan_Status, ann = ann$Loan_Status)
View(ensemble)

ensemble$log <- as.numeric(as.character(factor(ensemble$log, levels = c("Y","N"), labels = c(1,0))))
ensemble$svm <- as.numeric(as.character(factor(ensemble$svm, levels = c("Y","N"), labels = c(1,0))))
ensemble$rf <- as.numeric(as.character(factor(ensemble$rf, levels = c("Y","N"), labels = c(1,0))))
ensemble$dt <- as.numeric(as.character(factor(ensemble$dt, levels = c("Y","N"), labels = c(1,0))))
ensemble$ann <- as.numeric(as.character(factor(ensemble$ann, levels = c("Y","N"), labels = c(1,0))))

ensemble <- mutate(ensemble, total = log+svm+rf+dt+ann)
View(ensemble)

ensemble <- data.frame(Loan_ID = ensemble$Loan_ID, Output = ensemble$total)

ensemble$Loan_Status <- ifelse(ensemble$Output >= 4,"Y","N")

ensemble <- data.frame(Loan_ID=ensemble$Loan_ID,Loan_Status = ensemble$Loan_Status)
write.csv(ensemble, "ensemble_1.csv",row.names = F)
table(ensemble$Loan_Status)
