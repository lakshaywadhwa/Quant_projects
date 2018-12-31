require(gtools)
setwd("~/Desktop/Quant/QMProject3-Youtube/YouTube2013Attention/Revenues")
s<-NULL
count<-0
f<-0
temp1 <- list.files(pattern="*.csv")
temp1<-mixedsort(temp1)
setwd("~/Desktop/Quant/QMProject3-Youtube/YouTube2013Attention/Videos")
temp2<-list.files(pattern="*.csv")
temp2<-mixedsort(temp2)


for(i in 1:length(temp1))
{
  setwd("~/Desktop/Quant/QMProject3-Youtube/YouTube2013Attention/Revenues")
  a<-temp1[i]
  n<-gsub("\\.csv","",a)
  
  b<-read.csv(temp1[i])
  b$rev_name<-n
  setwd("~/Desktop/Quant/QMProject3-Youtube/YouTube2013Attention/Videos")
  c<-temp2[i]
  n1<-gsub("\\.csv","",c)
  
  d<-read.csv(temp2[i])
  d$video_name<-n1
  e<-merge(b,d,on='Date')
  s<-rbind(s,e)
}


names(s)<-c("Date","daily_earnings","AFV.earn","YT.earn","Transactions","rev_name","Views","Est_min_watched","Avg_View","Unique_cookies","video_name")
s$daily_earnings<-as.character(s$daily_earnings)
s$daily_earnings = as.numeric(gsub("\\$", "", s$daily_earnings))
s$earning_per_view<-s$daily_earnings/s$Views
s$Transactions <- as.character(s$Transactions)
s$Transactions = as.numeric(gsub("\\$", "", s$Transactions))
s$AFV.earn <- as.character(s$AFV.earn)
s$AFV.earn <- as.numeric(gsub("\\$", "", s$AFV.earn))
s$YT.earn <- as.character(s$YT.earn)
s$YT.earn <- as.numeric(gsub("\\$", "", s$AFV.earn))

class(s$Date)
s$Date<-as.character(s$Date)
s$Date <- as.Date(s$Date, format="%B %d,%Y")



s$Date<-as.character(s$Date)
s$earning_per_view<-gsub("\\NaN",0,s$earning_per_view)
View(s)


class(s$video_name)
s$rev_name<-gsub("\\Revenue","",s$rev_name)

s$video_name<-gsub("\\Video","",s$video_name)
View(s)

setwd("~/Desktop/Quant/QMProject3-Youtube/YouTube2013Attention")
instream<-read.csv("adinstream.csv")
s$ad_before<-rep(NA,nrow(s))
s$ad_after<-rep(NA,nrow(s))

for(i in 1:nrow(instream)){
  for(j in 1:nrow(s))
  {
  if(toupper(s$video_name[j])== toupper(instream$video.id[i]))
  {
    s$ad_before[j]<-instream$annoy_before[i] 
    s$ad_after[j]<-instream$annoy_after[i]
    }

  }
}

s$AFV.earn <- as.numeric(s$AFV.earn)
s$Transactions <- as.numeric(s$Transactions)
s$YT.earn <- as.numeric(s$YT.earn)
s$ad_before <- as.factor(s$ad_before)
s$ad_after <- as.factor(s$ad_after)

summary(s)

#####Winsorize###############
highTE <- quantile(s$daily_earnings, 0.95)
s$W_daily_earnings <- ifelse(s$daily_earnings>highTE, highTE, s$daily_earnings)
highAFV <- quantile(s$AFV.earn, 0.95)
s$W_AFV.earn <- ifelse(s$AFV.earn>highAFV, highAFV, s$AFV.earn)
highYT <- quantile(s$YT.earn, 0.95)
s$W_YT.earn <- ifelse(s$YT.earn>highYT, highYT, s$YT.earn)
highT <- quantile(s$Transactions, 0.95)
s$W_Transactions <- ifelse(s$Transactions>highT, highT, s$Transactions)
highV <- quantile(s$Views, 0.95)
s$W_Views <- ifelse(s$Views>highV, highV, s$Views)
highEMW <- quantile(s$Est_min_watched, 0.95)
s$W_Est_min_watched <- ifelse(s$Est_min_watched>highEMW, highEMW, s$Est_min_watched)
highAV <- quantile(s$Avg_View, 0.95)
s$W_Avg_View <- ifelse(s$Avg_View>highAV, highAV, s$Avg_View)
highUC <- quantile(s$Unique_cookies, 0.95)
s$W_Unique_cookies <- ifelse(s$Unique_cookies>highUC, highUC, s$Unique_cookies)
highEPV <- quantile(as.numeric(s$earning_per_view), 0.95)
s$W_earning_per_view <- ifelse(s$earning_per_view>highEPV, highEPV, s$earning_per_view)


#tabular and graphical summary info on daily_earnings, views, average_watch_time
daily_earnings_agg <- aggregate(s$W_daily_earnings, list(as.factor(s$Date)),sum)
summary(daily_earnings_agg)
demean <- mean(daily_earnings_agg$x)
desd <- sd(daily_earnings_agg$x)
demedian <- median(daily_earnings_agg$x)

daily_views <- aggregate(s$W_Views, list(as.factor(s$Date)),sum)
summary(daily_views)
dvmean <- mean(daily_views$x)
dvsd <- sd(daily_views$x)
dvmedian <- median(daily_views$x)

daily_watch <- aggregate(s$W_Avg_View, list(as.factor(s$Date)),mean)
summary(daily_watch)
dwmean <- mean(daily_watch$x)
dwsd <- sd(daily_watch$x)
dwmedian <- median(daily_watch$x)

variables <- c("daily earnings aggregated", "daily views", "daily average watch time")
means <- c(demean,dvmean,dwmean)
standard_deviations <- c(desd,dvsd,dwsd)
medians <- c(demedian,dvmedian,dwmedian)
tabular.info <- data.frame(variables, means, standard_deviations, medians)
View(tabular.info)

variables <- c("Daily Earnings", "Views", "Average Watch Time")
means <- c(mean(s$W_daily_earnings), mean(s$W_Views), mean(s$W_Avg_View))
standard_deviations <- c(sd(s$W_daily_earnings), sd(s$W_Views), sd(s$W_Avg_View))
medians <- c(median(s$W_daily_earnings), median(s$W_Views), median(s$W_Avg_View))
tabular.info <- data.frame(variables, means, standard_deviations, medians)
View(tabular.info)

test <- c(daily_earnings_agg, daily_views, daily_watch)
attach(test)
par(mfrow=c(3,1))
par(mfrow=c(1,1))
plot(daily_earnings_agg, xlab="Date", ylab = "Daily Earnings Aggregated")
plot(daily_views, xlab = "Date", ylab = "Daily Views")
plot(daily_watch, xlab = "Date", ylab = "Daily Avg Watch Time")
detach(test)


#boxplots for all variables
attach(s)
par(mfrow=c(2,2))
boxplot(W_daily_earnings, xlab = "Daily_Earnings per Video per Day")
boxplot(W_AFV.earn, xlab = "AFV_Earnings per Video per Day")
boxplot(W_YT.earn, xlab = "YouTube_Earnings per Video per Day")
boxplot(W_Transactions, xlab = "Transactions_Earnings per Video per Day")
detach(s)

attach(s)
par(mfrow=c(2,2))
boxplot(W_Views, xlab = "Views per Video per Day")
boxplot(W_Est_min_watched, xlab = "Estimated Minutes Watched per Video per Day")
boxplot(W_Avg_View, xlab = "Average Minutes Viewed per Video per Day")
boxplot(W_Unique_cookies, xlab = "Unique Cookies per Video per Day")
detach(s)

attach(s)
par(mfrow=c(1,3))
boxplot(daily_earnings_agg$x, xlab = "Daily_Earnings Aggregated for All Videos")
boxplot(daily_views$x, xlab = "Daily_Views for All Videos")
boxplot(daily_watch$x, xlab = "Average Daily Watch Time (minutes) for All Videos")
detach(s)

####Part 2
#pre-roll ads vs not
pre.period <- subset(s, Date < "2013-02-05")
pre.ad <- subset(pre.period, ad_before == 1)
non.pre.ad <- subset(pre.period, ad_before == 0)

t.test(pre.ad$W_Avg_View, non.pre.ad$W_Avg_View)
#t = -9.6179, df = 10817, p-value < 2.2e-16
t.test(pre.ad$W_Views, non.pre.ad$W_Views)
#t = -9.4844, df = 10562, p-value < 2.2e-16

#How does average watch	time relate to daily earnings on a video?
summary(lm(s$W_daily_earnings ~ s$W_Avg_View))

#How do views relate to daily earnings on a video?
summary(lm(s$W_daily_earnings ~ s$W_Views))

#How do views relate to average watch time on a video?
summary(lm(s$W_Avg_View ~ s$W_Views))

###Part 3
#How does the Post Period relate to Daily Earnings?
#date-level variable
post <- rep(NA, nrow(s))
for (i in 1:nrow(s)){
	if (s$Date[i] < "2013-02-05"){
		post[i] <- 0
	} else {
		post[i] <-1
	}
}
s$post <- post

summary(lm(s$W_daily_earnings ~ s$post))

###Part 4

#How do videos that switched from no pre-roll to pre-roll in the post period perform in relation 
#to Daily Earnings?

#video-level variable
s$annoy_later <- rep(9,nrow(s))
change <- as.numeric(s$ad_after) - as.numeric(s$ad_before)
for (i in 1:nrow(s)){
	if (change[i] > 0){
		s$annoy_later[i] <- 1
	} else {
		s$annoy_later[i] <- 0
	}
}

summary(lm(s$W_daily_earnings ~ s$annoy_later))

###Part 5
#multiple regression
summary(lm(s$W_daily_earnings~s$annoy_later * s$post))

###Part 6

#avg_watch_time

#Average Watch Time affected by videos in the post period
summary(lm(s$W_Avg_View~s$post))

#Average Watch Time predicted by videos that switched from no pre-roll to pre-roll
summary(lm(s$W_Avg_View~s$annoy_later))

#Average watch time against a MLR including our above variables
summary(lm(s$W_Avg_View~s$annoy_later * s$post))

