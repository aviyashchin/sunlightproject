library(dplyr)
library(googleVis)
library(data.table)
library(fasttime)
library(ggplot2)
library(dplyr)
library(leaps)
library(mgcv)
library(evtree)
library(randomForest)

setwd("e:/downloads/sunlightproject")
contrib = read.csv("data/tail.contributions.fec.csv")
lobbying = read.csv("data/tail.lobbying.csv")
cleancontrib = fread("data/interesting.contributions.csv")
cleancontrib.int = fread("data/contributions.fec.csv")

#sapply(contrib, class)
#sapply(lobbying, class)
#sapply(cleancontrib, class)

#class(cleancontrib$date)
dt<-data.table(cleancontrib.int)
cleancontrib.int$date <- fastPOSIXct(cleancontrib.int$date)

qplot(recipient_party, amount, data = cleancontrib, geom = "boxplot")
qplot(committee_party, amount, data = cleancontrib, geom = "boxplot")

qplot(recipient_party, data = cleancontrib)
qplot(contributor_state, data = cleancontrib)

qplot(contributor_state, data = cleancontrib, geom = "bar", fill = recipient_party)
qplot(seat_result, data = cleancontrib, geom = "bar", fill = recipient_party)

#average contribution size
qplot(amount, data = cleancontrib, binwidth = 1000)

qplot(amount, data = cleancontrib, binwidth = 100) + 
  zoom + facet_wrap(~ recipient_party)

qplot(amount, data = cleancontrib, geom = "density", 
      color = recipient_party)


#-----------------------------------------------------------
ccr <- cleancontrib %>% group_by(recipient_party, date) %>% summarise_each(funs(sum(amount, na.rm = T)))
myvars <- c("contributor_state", "amount","date","transaction_type","contributor_category","recipient_party","committee_party","contributor_zipcode")
newdata <- ccr[,myvars,with = FALSE]
newdata$date2 <- format(as.Date(newdata$date),'%Y')

#ccr <- newdata[,amount:=sum(amount),by="recipient_party,date2"]

#for converting to annual data
newdata$date2 <- as.Date(paste(newdata$date2,"-01-01",sep=""))
#sapply(newdata,class)

Motion.contrib=gvisMotionChart(newdata, 
                       idvar="recipient_party", 
                       timevar="date2",
                       options=list(state='{"showTrails":false};'))
plot(Motion.contrib)
#-----------------------------------------------------------
#ccr2 <- cleancontrib %>% group_by(recipient_party, date) %>% summarise_each(funs(sum(amount, na.rm = T)))
ccr2 <- dt[,sum(amount),by="recipient_party,date"]
names(ccr2)[3] <- "amount"


Cal <- gvisCalendar(ccr2, 
                    datevar="date", 
                    numvar="amount",
                    options=list(
                      title="Daily temperature in Cairo",
                      height=620,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}"))
plot(Cal)
#-----------------------------------------------------------
dems = subset(cleancontrib, recipient_party == "D")
reps = subset(cleancontrib, recipient_party == "R")
indys = subset(cleancontrib, recipient_party == "I")

ccr3 <- indys %>% group_by(recipient_party, contributor_state) %>% summarise_each(funs(sum(amount, na.rm = T)))
myvars <- c("contributor_state", "amount","recipient_party")
newdata <- ccr3[,myvars,with = FALSE]

GeoStates <- gvisGeoChart(newdata, "contributor_state", "amount",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

#-----------------------------------------------------------
#get non-blank rows
m1 <- cleancontrib[cleancontrib$seat_result != "",]
m1$seat_result <- as.factor(m1$seat_result)
m1$recipient_party <- as.factor(m1$recipient_party)
m1$contributor_state <- as.factor(m1$contributor_state)
m1$transaction_type <- as.factor(m1$transaction_type)
m1$contributor_category <- as.factor(m1$contributor_category)
m1$contributor_zipcode <- as.factor(m1$contributor_zipcode)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(m1))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(m1)), size = smp_size)

train <- m1[train_ind, ]
test <- m1[-train_ind, ]
#-----------------------------------------------------------
#LM
m3 <- lm(amount ~ seat_result + recipient_party + contributor_state + transaction_type + contributor_category, 
         data = train)
coef(m3)
summary(m3)
# ---------------------------------------
#GLM
gmod <- gam(seat_result ~  recipient_party + contributor_state + transaction_type + contributor_category + amount,
            data = train, family = binomial)
summary(gmod)

frmla = seat_result ~  recipient_party + contributor_state + transaction_type + contributor_category + amount
#tree
ev.raw = evtree(frmla, data=train)
plot(ev.raw)

# ---------------------------------------
