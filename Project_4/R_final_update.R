install.packages("MASS")
install.packages("reshape2")
install.packages("reshape")
library(MASS)
library(reshape2)
library(reshape)
library(ggplot2)

#1
install.packages('dplyr')
library(dplyr)
a<-read.csv("LoanStats3c.csv",stringsAsFactors = FALSE)
df<-a[,c(9,10,23,6,3,14,15,21,81,7,17,104,107)]
View(df)
df<-df[complete.cases(df),]
str(df)
df$int_rate = as.numeric(gsub("\\%", "", df$int_rate))
boxplot(df$loan_amnt, xlab = "Loan amount")
boxplot(df$annual_inc, xlab = "annual_inc")
hist(log(df$annual_inc))
#####Winsorize###############
#annual_inc_w <- quantile(df$annual_inc, 0.95)
#df$annual_inc_w <- ifelse(df$annual_inc>annual_inc_w, annual_inc_w, df$annual_inc)

boxplot(df$int_rate, xlab = "int_rate")
#####Winsorize###############
#int_rate_w <- quantile(df$int_rate, 0.95)
#df$int_rate_w <- ifelse(df$int_rate>int_rate_w, int_rate_w, df$int_rate)
boxplot(df$int_rate_w, xlab = "int_rate")
hist(df$int_rate_w, xlab = "int_rate")
#####Winsorize###############

#pct_tl_nvr_dlq_w <- quantile(df$pct_tl_nvr_dlq, 0.05)
#df$pct_tl_nvr_dlq_w <- ifelse(df$pct_tl_nvr_dlq<pct_tl_nvr_dlq_w, pct_tl_nvr_dlq_w, df$pct_tl_nvr_dlq)


View(df)
##ii.	ZIP x Verification Status
df_1 <- df %>%select(zip_code,verification_status,loan_amnt,annual_inc,int_rate,pct_tl_nvr_dlq,tax_liens) %>% group_by(verification_status,zip_code) %>%summarize_all(mean)
View(df_1)

str(df_1)

#contvar1 <- df %>% select(zip_code, verification_status, loan_amnt, annual_inc, int_rate) %>%
# group_by(zip_code, verification_status) %>% summarize_all(mean)


## Factor variables
factorvar1 <- melt(subset(df, select = c(zip_code, verification_status,grade, sub_grade, term, purpose, loan_status ) ),
                   id.var = c('zip_code', 'verification_status'))
factorvar1 <- factorvar1 %>% group_by(zip_code, verification_status, variable, value) %>% summarise(frequency = n()) %>% mutate(fraction = frequency/sum(frequency))
#v<-cast(factorvar1,zip_code+verification_status~variable+value+frequency)
#v1<-cast(factorvar1,zip_code+verification_status~variable+value+fraction)

df_2 <- df %>%select(zip_code,verification_status,term,sub_grade,loan_amnt,annual_inc,int_rate,pct_tl_nvr_dlq,tax_liens) %>%group_by(verification_status,zip_code,term,sub_grade) %>% summarize_all(mean)
View(df_2)



####factor variables 2
factorvar2 <- melt(subset(df, select = c(zip_code, verification_status,grade, sub_grade, term, purpose, loan_status ) ),
                   id.var = c('zip_code', 'verification_status','sub_grade','term'))
factorvar2 <- factorvar2 %>% group_by(zip_code, verification_status,sub_grade,term,variable, value) %>% summarise(frequency = n()) %>% mutate(fraction = frequency/sum(frequency))
View(factorvar2)

#444444444
#tabular and graphical summary info on int_rate, loan_status, and loan_amnt
hist(df_2$loan_amnt)
hist(df_2$annual_inc)

View(df_2)
annual_inc_income_w <- quantile(df_2$annual_inc, 0.95)
df_2$annual_inc_income_w <- ifelse(df_2$annual_inc>annual_inc_income_w, annual_inc_income_w, df_2$annual_inc)
hist(df_2$annual_inc_income_w)
hist(df_2$int_rate)
hist(df_2$pct_tl_nvr_dlq)
demean_intrate <- mean(df_2$int_rate)
desd_intrate <- sd(df_2$int_rate)
demedian_intrate <- median(df_2$int_rate)

summary(df_2$loan_amnt)
demean_loanamnt <- mean(df_2$loan_amnt)
desd_loanamnt <- sd(df_2$loan_amnt)
demedian_loanamnt <- median(df_2$loan_amnt)


variables <- c("interest rate", "loan amount")
means <- c(demean_intrate,demean_loanamnt)
standard_deviations <- c(desd_intrate,desd_loanamnt)
medians <- c(demedian_intrate,demedian_loanamnt)
tabular.info <- data.frame(variables, means, standard_deviations, medians)
View(tabular.info)
#hist(df_2$int_rate)
#hist(df_2$loan_amnt)
barplot(table(df_2$verification_status))
#with(factorvar2,table(factorvar2$term,factorvar2$verification_status))
plot(df_2$annual_inc_income_w~df_2$int_rate)
plot(df_2$annual_inc_income_w~df_2$loan_amnt)
plot(df_2$annual_inc_income_w~as.factor(df_2$verification_status))
View(factorvar2)
sub_debt <- subset(factorvar2, variable =="purpose" & value=="debt_consolidation")
View(sub_debt)
merged_data_debt <- merge(df_2, sub_debt,by.x=c("zip_code","verification_status","sub_grade","term"), by.y=c("zip_code","verification_status","sub_grade","term"))
View(merged_data_debt)
plot(merged_data_debt$int_rate~merged_data_debt$fraction)
abline(lm(merged_data_debt$int_rate~merged_data_debt$fraction))
sub_del1 <- subset(factorvar2, variable =="loan_status" & value=="Late (16-30 days)")
View(sub_del1)
sub_del2 <- subset(factorvar2, variable =="loan_status" & value=="Late (31-120 days)")
View(sub_del2)
merged_data_del1 <- merge(df_2,sub_del1 ,by.x=c("zip_code","verification_status","sub_grade","term"), by.y=c("zip_code","verification_status","sub_grade","term"))
View(merged_data_del1)
plot(merged_data_del1$int_rate~merged_data_del1$fraction)
merged_data_del2 <- merge(df_2,sub_del2 ,by.x=c("zip_code","verification_status","sub_grade","term"), by.y=c("zip_code","verification_status","sub_grade","term"))
plot(merged_data_del2$int_rate~merged_data_del2$fraction)
abline(lm(merged_data_del2$int_rate~merged_data_del2$fraction))
summary(lm(merged_data_del2$int_rate~merged_data_del2$fraction))

#Int_rate against Pct delinquent
plot(df_2$pct_tl_nvr_dlq, df_2$int_rate)
abline(lm(df_2$int_rate~df_2$pct_tl_nvr_dlq))
summary(lm(df_2$int_rate~df_2$pct_tl_nvr_dlq))

#Int_Rate against tax liens
plot(df_2$tax_liens, df_2$int_rate)
abline(lm(df_2$int_rate~df_2$tax_liens))

tax_liens_w <- quantile(df_2$tax_liens, 0.95)
df_2$tax_liens_w <- ifelse(df_2$tax_liens>tax_liens_w, tax_liens_w, df_2$tax_liens)
hist(df_2$tax_liens)
hist(df_2$tax_liens_w)
View(df_2)
plot(df_2$tax_liens_w, df_2$int_rate)
abline(lm(df_2$int_rate~df_2$tax_liens))
summary(lm(df_2$int_rate~df_2$tax_liens))

#Loan Amount against PCT delinquent
plot(df_2$pct_tl_nvr_dlq, df_2$loan_amnt)
abline(lm(df_2$loan_amnt~df_2$pct_tl_nvr_dlq))

#Loan Amount against tax liens
plot(df_2$tax_liens_w, df_2$loan_amnt)
abline(df_2$loan_amnt~df_2$tax_liens_w)
summary(lm(df_2$loan_amnt~df_2$tax_liens_w))


###### Part 5#####
#5(i) sort of
with(factorvar2,table(factorvar2$term,factorvar2$verification_status))
with(factorvar2,chisq.test(factorvar2$term,factorvar2$verification_status))
plot(as.factor(factorvar2$term),as.factor(factorvar2$verification_status))
factorvar2$verif_bin <- NA
for (i in (1:nrow(factorvar2))){
  if (factorvar2$verification_status[i] == "Not Verified"){
    factorvar2$verif_bin[i] <- 0
  } else {
    factorvar2$verif_bin[i] <- 1
  }
}

with(factorvar2,table(factorvar2$term,factorvar2$verif_bin))
summary(lm(factorvar2$verif_bin~factorvar2$term))

#Real 5(i)

sub_status <- subset(factorvar2, variable =="loan_status")
View(sub_status)
sub_status$status_bin <- NA
for (i in (1:nrow(sub_status))){
  if (sub_status$value[i] == c("Charged Off", "Default", "Late (16-30 days)", "Late (31-120 days)")){
    sub_status$status_bin[i] <- 1
  } else {
    sub_status$status_bin[i] <- 0
  }
}

summary(lm(sub_status$status_bin~sub_status$term))
plot(as.factor(sub_status$term), as.factor(sub_status$status_bin))
with(sub_status, table(sub_status$term, sub_status$status_bin))

#5(ii)
summary(lm(df_2$loan_amnt~df_2$annual_inc_income_w))
plot(df_2$loan_amnt~df_2$annual_inc_income_w)
summary(lm(df_2$int_rate~df_2$annual_inc_income_w))
plot(df_2$int_rate~df_2$annual_inc_income_w)
abline(lm(df_2$int_rate~df_2$annual_inc_income_w),col=25)

##ScatterPlot of the full dataset, colorder by decade
ggplot(df_2, aes(x=df_2$annual_inc_income_w, y=df_2$loan_amnt, color=df_2$verification_status, lty=verification_status)) +
  xlab("annual_income") +
  ylab("loan_amt") +
  geom_point(size=0.5) +
  theme_classic()



summary(lm(df_2$loan_amnt~df_2$annual_inc_income_w*df_2$verification_status))
summary(lm(df_2$loan_amnt~df_2$annual_inc_income_w+df_2$verification_status))

ggplot(df_2, aes(x=df_2$annual_inc_income_w, y=df_2$int_rate, color=df_2$verification_status, lty=verification_status)) +
  xlab("annual_income") +
  ylab("int_rate") +
  geom_point(size=0.2) +
  theme_classic()
summary(lm(df_2$int_rate~df_2$annual_inc_income_w*df_2$verification_status))
summary(lm(df_2$int_rate~df_2$annual_inc_income_w+df_2$verification_status))

#5(c)

sub_status$charge_bin <- NA
for (i in (1:nrow(sub_status))){
  if (sub_status$value[i] == c("Charged Off", "Default")){
    sub_status$charge_bin[i] <- 1
  } else {
    sub_status$charge_bin[i] <- 0
  }
}
summary(lm(sub_status$charge_bin~sub_status$sub_grade))
plot(as.factor(sub_status$sub_grade), as.factor(sub_status$charge_bin))

#5(d)
merged_data_status <- merge(df_2, sub_status,by.x=c("zip_code","verification_status","sub_grade","term"), by.y=c("zip_code","verification_status","sub_grade","term"))
View(merged_data_status)

summary(lm(merged_data_status$charge_bin~merged_data_status$pct_tl_nvr_dlq+merged_data_status$sub_grade))

summary(lm(merged_data_status$charge_bin~merged_data_status$tax_liens_w+merged_data_status$sub_grade))

#5(e)
View(df_2)
summary(lm(df_2$int_rate~df_2$sub_grade+df_2$tax_liens))
summary(lm(df_2$loan_amnt~df_2$sub_grade+df_2$tax_liens))

summary(lm(df_2$int_rate~df_2$sub_grade+df_2$pct_tl_nvr_dlq))
summary(lm(df_2$loan_amnt~df_2$sub_grade+df_2$pct_tl_nvr_dlq))

#5(f)
df$loan_status_bin <- NA
for (i in (1:nrow(df))){
  if (df$loan_status[i] == c("Charged Off", "Default")){
    df$loan_status_bin[i] <- 1
  } else {
    df$loan_status_bin[i] <- 0
  }
}

summary(lm(df$loan_status_bin~df$purpose))
summary(as.factor(df$purpose))
##########PARTB
b<-read.csv("14zpallnoagi.csv",stringsAsFactors = FALSE)
tax<-b[,c(3,16,17)]
names(tax) <- c("zipcode", "total_tax_returns","total_income")
View(tax)
tax$avg_income<-tax$total_income/tax$total_tax_returns
summary(tax$avg_income)
View(tax_final)
tax_final <- tax %>%select(zipcode,avg_income) %>%group_by(zipcode) %>%summarize_all(mean)
boxplot(tax_final$avg_income)
boxplot(log(tax_final$avg_income))
hist(tax_final$avg_income)
hist(log(tax_final$avg_income))
summary(tax_final$avg_income)
View(df_1)
#df<-a[,c(9,10,23,6,3,14,15,21,81,7,17,104)]
new_zip1<-substr(tax_final$zipcode,1,3)
tax_final<-cbind(new_zip1,tax_final)
View(tax_final)
tax_final <- tax_final %>%select(new_zip1,avg_income) %>%group_by(new_zip1) %>%summarize_all(mean)
View(tax_final)
df_new <- df %>%select(zip_code,verification_status,loan_amnt,annual_inc) %>%group_by(zip_code,verification_status) %>%summarize_all(mean)
View(df_new)
str(df_new)
df_new$new_zip<-substr(df_new$zip_code,1,3)
View(df_new)
str(new_zip)
#df_new<-cbind(new_zip,df_new)
View(df_new)
merged.data <- merge(df_new, tax_final,by.x="new_zip", by.y="new_zip1")
View(merged.data)
merged.data$avg_income_thousand<-merged.data$avg_income*1000
####333
summary(lm(merged.data$avg_income~merged.data$annual_inc))
summary(lm(merged.data$avg_income_thousand~merged.data$annual_inc))
plot(merged.data$annual_inc,merged.data$avg_income_thousand)
#merged.data.verified<-subset(merged.data,merged.data$verification_status=="Verified")
#summary(lm(merged.data.verified$avg_income_thousand~merged.data.verified$annual_inc))
#plot(merged.data.verified$avg_income_thousand,merged.data.verified$annual_inc)
#merged.data.notverified<-subset(merged.data,merged.data$verification_status=="Not Verified")
#summary(lm(merged.data.notverified$avg_income_thousand~merged.data.notverified$annual_inc))
#plot(merged.data.notverified$avg_income_thousand,merged.data.notverified$annual_inc)
#merged.data.sourceverified<-subset(merged.data,merged.data$verification_status=="Source Verified")
#summary(lm(merged.data.sourceverified$avg_income_thousand~merged.data.sourceverified$annual_inc))
#plot(merged.data.sourceverified$avg_income_thousand,merged.data.sourceverified$annual_inc)

summary(lm(merged.data$avg_income_thousand~merged.data$annual_inc+merged.data$verification_status))
summary(lm(merged.data$avg_income_thousand~merged.data$annual_inc*merged.data$verification_status))

ggplot(merged.data, aes(x=merged.data$annual_inc, y=merged.data$avg_income_thousand, color=merged.data$verification_status, lty=merged.data$verification_status)) +
  xlab("annual_income") +
  ylab("avg_income_thousand") +
  geom_point(size=0.2) +
  theme_classic()

annual_inc_w <- quantile(merged.data$annual_inc, 0.95)
merged.data$annual_inc_w <- ifelse(merged.data$annual_inc>annual_inc_w, annual_inc_w, merged.data$annual_inc)

avg_inc_w <- quantile(merged.data$avg_income_thousand, 0.95)
merged.data$avg_inc_w <- ifelse(merged.data$avg_income_thousand>avg_inc_w, avg_inc_w, merged.data$avg_income_thousand)

summary(lm(merged.data$avg_inc_w~merged.data$annual_inc_w+merged.data$verification_status))
summary(lm(merged.data$avg_inc_w~merged.data$annual_inc_w*merged.data$verification_status))

ggplot(merged.data, aes(x=merged.data$annual_inc_w, y=merged.data$avg_inc_w, color=merged.data$verification_status, lty=merged.data$verification_status)) +
  xlab("annual_income_w") +
  ylab("avg_income_thousand_w") +
  geom_point(size=0.2) +
  theme_classic()

par(mfrow=c(1,2))
hist(merged.data$avg_income_thousand)
hist(merged.data$annual_inc)
t.test(merged.data.sourceverified$annual_inc,merged.data.sourceverified$avg_income_thousand)
t.test(merged.data$annual_inc,merged.data$avg_income_thousand)
##third parties are doing than the lending club
#(b)
summary(lm(merged.data$avg_income_thousand~merged.data$verification_status+merged.data$annual_inc))
summary(lm(merged.data$avg_income_thousand~merged.data$verification_status*merged.data$annual_inc))

#(c)
summary(merged.data$annual_inc)
merged.data$dataBinned <- cut( merged.data$avg_income_thousand,
                             
                             breaks=quantile( merged.data$avg_income_thousand, probs=seq(from=0,to=1,by=.25) ),
                             
                             include.lowest=TRUE )
summary(lm(merged.data$annual_inc~merged.data$dataBinned))
quantile((merged.data$avg_income_thousand), probs = seq(0, 1, 0.25))
plot(merged.data$dataBinned~merged.data$annual_inc)
plot(merged.data$annual_inc~merged.data$dataBinned)
View(merged.data)


####################
######Other ways to distinguish your project.  This part is a little less structured.
# b)
df_last<-a[,c(12,15)]
df_last<-df_last[complete.cases(df_last),]
with(df_last,table(df_last$verification_status,df_last$emp_length))
plot(table(df_last$emp_length,df_last$verification_status))
plot((as.factor(df_last$emp_length)~as.factor(df_last$verification_status)))
plot((as.factor(df_last$emp_length)~as.factor(df_last$verification_status=="Verified")))
plot((as.factor(df_last$emp_length)~as.factor(df_last$verification_status=="Not Verified")))
plot((as.factor(df_last$emp_length)~as.factor(df_last$verification_status=="Source Verified")))
less_than_5<-subset(df_last$emp_length,c(df_last$emp_length=="<1 year",df_last$emp_length=="1 year",df_last$emp_length=="2 years",df_last$emp_length=="3 years",df_last$emp_length=="4 years"))
#c)
df_emp <- df %>%select(zip_code,loan_amnt,annual_inc,int_rate,pct_tl_nvr_dlq,tax_liens,emp_length) %>% group_by(emp_length,zip_code) %>%summarize_all(mean)
View(df_emp)
df_emp$new_zip<-substr(df_emp$zip_code,1,3)
View(df_emp)
merged.data1 <- merge(df_emp, tax_final,by.x="new_zip", by.y="new_zip1")
merged.data1$avg_income_thousand<-merged.data1$avg_income*1000
summary(lm(merged.data1$avg_income_thousand~merged.data1$annual_inc+merged.data1$emp_length))
#verification status is better R^2 value 
d<-read.csv("14zpallagi.csv",stringsAsFactors = FALSE)
df_a<-d[complete.cases(d),]
tax_b<-df_a[,c(3,16,17)]
names(tax_b) <- c("zipcode", "total_tax_returns","total_income")
tax_b$avg_income<-tax$total_income/tax$total_tax_returns
tax_b <- tax_b %>%select(zipcode,avg_income) %>%group_by(zipcode) %>%summarize_all(mean)


new_zip2<-substr(tax_b$zipcode,1,3)


tax_final2<-cbind(new_zip2,tax_b)
View(tax_final2)
tax_final2 <- tax_final2 %>%select(new_zip2,avg_income) %>%group_by(new_zip2) %>%summarize_all(mean)
View(tax_final2)
merged.data.agi <- merge(df_new, tax_final2,by.x="new_zip", by.y="new_zip2")
View(merged.data.agi)
merged.data.agi$avg_income_thousand<-merged.data.agi$avg_income*1000
summary(lm(merged.data.agi$avg_income~merged.data.agi$annual_inc))
summary(merged.data$annual_inc)
merged.data.agi$dataBinned <- cut( merged.data.agi$avg_income_thousand,
                                   
                                   breaks=quantile( merged.data.agi$avg_income_thousand, probs=seq(from=0,to=1,by=.25) ),
                                   
                                   include.lowest=TRUE )
summary(lm(merged.data.agi$annual_inc~merged.data.agi$dataBinned))

