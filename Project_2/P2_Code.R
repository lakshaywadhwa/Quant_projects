output <- file("Project2_Team5_Ouput_LOG.txt", open = 'wt')
sink('Project2_Team5_Ouput_LOG.txt', type="output")

#Question 1
##Reading In and Formatting the Data

#Reading in the Data
gdp <- read.csv("~/Desktop/Quant/QMProject2/GDP.csv", stringsAsFactors=FALSE)
unemp <- read.csv("~/Desktop/Quant/QMProject2/UNRATE.csv", stringsAsFactors=FALSE)
gpurch <- read.csv("~/Desktop/Quant/QMProject2/GCE.csv", stringsAsFactors=FALSE)
gpurch$percent<-gpurch$GCE/gdp$GDP
pgpurch <- subset(gpurch, DATE >= '1990-01-01' & DATE <= '2010-10-01')
unemp_new <- subset(unemp, DATE >= '1990-01-01' & DATE <= '2010-10-01')
new_data<-merge(unemp_new,pgpurch,by="DATE")
full_data<-merge(gpurch,unemp,by="DATE")
colnames(new_data)[2] <- "Unemployment_Rate" #Rename column
colnames(new_data)[3] <- "GCE" #Rename column

#Question 2 
##Creating Taylor's Plot

#Formatting the Dates correctly
new_data$DATE <- as.Date(new_data$DATE, format = "%Y-%m-%d")
full_data$DATE <- as.factor(paste(substr(full_data$DATE,1,3),"0s",sep=""))

#Importing GGPlot for graphs
library ("ggplot2")

#Taylor's ScatterPlot
ggplot(new_data, aes(y=Unemployment_Rate, x=percent)) + 
  geom_point(color="blue") + 
  ylab("Unemployment rate") +
  xlab("Government purchases as a percent of GDP") +
  theme_classic()

#Question 3
##ScatterPlot of the full dataset, colorder by decade
ggplot(full_data, aes(x=full_data$percent, y=full_data$UNRATE, color=DATE, lty=DATE)) +
  xlab("Government purchases as a percent of GDP") +
  ylab("Unemployment rate") +
  geom_point() +
  theme_classic()

##ScatterPlot of the full dataset, regression lines for decade
ggplot(full_data, aes(x=full_data$percent, y=full_data$UNRATE, color=DATE, lty=DATE)) +
  xlab("Government purchases as a percent of GDP") +
  ylab("Unemployment rate") +
  geom_smooth(method = lm, se = F) +
  theme_classic()

##Full dataset with no decade differentiation
ggplot(full_data, aes(y=UNRATE, x=percent)) + 
  geom_point(color="blue") + 
  ylab("Unemployment rate") +
  xlab("Government purchases as a percent of GDP") +
  theme_classic()

#Question 4
##Analyzing Taylor's Correlation

#Looking at Taylor's correlation
TX <- new_data$percent
TY <- new_data$Unemployment_Rate
Tcor <- cor(TX,TY)

#Looking at Full Data Correlation
fullX <- full_data$percent
fullY <- full_data$UNRATE
fullcor <- cor(fullX, fullY)

#Looking at 50's correlation
fiftypgpurch <- subset(gpurch, DATE >= '1950-01-01' & DATE <= '1959-10-01')
fiftyunemp_new <- subset(unemp, DATE >= '1950-01-01' & DATE <= '1959-10-01')
fifty_data<-merge(fiftyunemp_new,fiftypgpurch,by="DATE")
fiftyX <- fifty_data$percent
fiftyY <- fifty_data$UNRATE
fiftycor <- cor(fiftyX, fiftyY)

#Looking at 60's correlation
sixtypgpurch <- subset(gpurch, DATE >= '1960-01-01' & DATE <= '1969-10-01')
sixtyunemp_new <- subset(unemp, DATE >= '1960-01-01' & DATE <= '1969-10-01')
sixty_data<-merge(sixtyunemp_new,sixtypgpurch,by="DATE")
sixtyX <- sixty_data$percent
sixtyY <- sixty_data$UNRATE
sixtycor <- cor(sixtyX, sixtyY)

#Looking at 70's correlation
seventypgpurch <- subset(gpurch, DATE >= '1970-01-01' & DATE <= '1979-10-01')
seventyunemp_new <- subset(unemp, DATE >= '1970-01-01' & DATE <= '1979-10-01')
seventy_data<-merge(seventyunemp_new,seventypgpurch,by="DATE")
seventyX <- seventy_data$percent
seventyY <- seventy_data$UNRATE
seventycor <- cor(seventyX, seventyY)

#Looking at 80's correlation
eightypgpurch <- subset(gpurch, DATE >= '1980-01-01' & DATE <= '1989-10-01')
eightyunemp_new <- subset(unemp, DATE >= '1980-01-01' & DATE <= '1989-10-01')
eighty_data<-merge(eightyunemp_new,eightypgpurch,by="DATE")
eightyX <- eighty_data$percent
eightyY <- eighty_data$UNRATE
eightycor <- cor(eightyX, eightyY)

#Looking at 90's correlation
nintypgpurch <- subset(gpurch, DATE >= '1990-01-01' & DATE <= '1999-10-01')
nintyunemp_new <- subset(unemp, DATE >= '1990-01-01' & DATE <= '1999-10-01')
ninty_data<-merge(nintyunemp_new,nintypgpurch,by="DATE")
nintyX <- ninty_data$percent
nintyY <- ninty_data$UNRATE
nintycor <- cor(nintyX, nintyY)

#Looking at 00's correlation
centpgpurch <- subset(gpurch, DATE >= '2000-01-01' & DATE <= '2009-10-01')
centunemp_new <- subset(unemp, DATE >= '2000-01-01' & DATE <= '2009-10-01')
cent_data<-merge(centunemp_new,centpgpurch,by="DATE")
centX <- cent_data$percent
centY <- cent_data$UNRATE
centcor <- cor(centX, centY)

#Looking at 10's correlation
tenpgpurch <- subset(gpurch, DATE >= '2010-01-01' & DATE <= '2018-04-01')
tenunemp_new <- subset(unemp, DATE >= '2010-01-01' & DATE <= '2018-04-01')
ten_data<-merge(tenunemp_new,tenpgpurch,by="DATE")
tenX <- ten_data$percent
tenY <- ten_data$UNRATE
tencor <- cor(tenX, tenY)

#printing all correlations
decades <- c('Taylor', 'Full', '50s', '60s', '70s', '80s', '90s', '00s', '10s')
correlations <- c(Tcor, fullcor, fiftycor, sixtycor, seventycor, eightycor, nintycor, centcor, tencor)
for (i in 1:9){
  print(c(decades[i], correlations[i]))
}

#Question 5
##Breaking the For Loop into parts

###Part 1: Storing 1000 samples of X Means
samp_size = 1000
x = rep(NA, 1000)

for(i in 1:1000){
  x_sample = rnorm(samp_size, mean = 5, sd = 10)
  x[i] = mean(x_sample)   
}


###Part 2: Storing 1000 samples of Y Means
samp_size = 1000
y = rep(NA, 1000)
set.seed(1)
for(i in 1:1000){
  x_sample = rnorm(samp_size, mean = 10, sd = 20)
  y[i] = mean(x_sample)   
}

cor(x,y) #Checking Correlation
plot(x,y) #Plotting for visual identification

###Part 3: Building our for loop to simulate 1000 trials of 256 quarter periods.
set.seed(1) #setting seed so we get same output (just for consistency in report)
cplist <- rep(NA,1000) #Initializing List
ncplist<-rep(NA,1000) #Initializing List
for(a in 1:1000){ #Starting 1000 overarching trials
  samp_size = 1000 #Picking a sample size, 1000 felt appropriate to not keep some variation
  x = rep(NA, 256) #Creating a vector of 256 NA
    for(i in 1:256){ #Same Code as part 1, just for 256 quarters 
      x_sample = rnorm(samp_size, mean = 5, sd = 10)
      x[i] = mean(x_sample)   
    }
  samp_size = 1000 #Picking a sample size, 1000 felt appropriate to not keep some variation
  y = rep(NA, 256) #Creating a vector of 256 NA
    for(i in 1:256){ #Same Code as part 2, just for 256 quarters
      x_sample = rnorm(samp_size, mean = 5, sd = 10)
      y[i] = mean(x_sample)   
      }
  x_new=rep(NA,196) #Initializing new lists to hold 196 correlations (256-60)
  y_new=rep(NA,196) 
  cor_new<-rep(NA,256) #Creating a vector of 256 NA
    for(i in 60:256){ #Iterrating through 60 quarters up to full time frame
      x_new<-x[(257-i):256] #Putting list of first 60 quarters, 61, etc. into x_new variable
      y_new<-y[(257-i):256] #Putting list of first 60 quarters, 61, etc. into y_new variable
      cor_new[i]<-cor(x_new,y_new) #Finding correlation between x and y lists for current quarter itteration
      }
  cor_new<-na.omit(cor_new) #Pulling out NA values
  cplist[a] <- max(cor_new) #Storing maximum positive correlation for current itteration
  ncplist[a] <- cor_new[197] #Storing correlation over whole 256 quarters for current trial itteration
}

X <- cplist #Saving cherry picked correlations to X variable
Y <- ncplist #Saving full dataset correlations to Y variable
Z <- cplist-ncplist #Getting difference between pairs
#Graphical Evidence of difference
hist(X) #Histogram of cherry picked correlations
hist(Y) #Histogram of whole data set correlations
#These histograms give us insight into the fact that these are two different populations. X seems to be centerd more around .8, while Y is certainly centered around 0. In addition, X has a larger spread overall accompanying a right skew that goes past .4. Y is almost perfectally normally distributed and stays within correlations of .2. 
#Numerical Evidence of difference
t.test(X,Y) #2-sample t-test
t.test(Z) #Paired-samples t-test
#Both these t.tests lend heavy evidence that the two populations of correlations are not the same. With t-values exceeded 20 and P-Values approaching 0, we can say that these two populations are statistically different at the .05 level. 
#Running t-test against Cherry Picked values
t.test(X, mu=Tcor)
max(X)
#While Taylor's value is Cherry Picked, It could not be generated from this totally random value set of cherry picked values. Our maximum Cherry Picked correlation was only able to reach .4166832 under the total randomness, this just wasn't enough variability to reach Taylor's over .8 positive correlation. 

sink()
unlink("Project2_Team5_Ouput_LOG.txt")