



library(survival)
library(survminer)


# preparation data
subj <-c(1,2,3,4,5,6)
time  <-c(18,23,27,32,57,64)
status <-c(0,1,0,1,0,1)
cov <-c(1,2,1,2,3,2)

data<-data.frame(subj,time,status,cov)

data

data$censored <- 1 - data$status

data$Tstart <- 0
times <- sort(unique(data$time))

data.long <- survSplit(data,cut = times,end = "time",start = "Tstart",event = "status",id = "id")

data.long <- data.long[order(data.long$id,data.long$time),]

#####

data.long.cens <- survSplit(data,cut = times,end = "time",start = "Tstart",event = "censored",id = "id")
data.long.cens <- data.long.cens[order(data.long.cens$id,data.long.cens$time),]
data.long$censored <- data.long.cens$censored
data.long


#Step 1: Fit Censoring Model
CZ <- coxph(Surv(Tstart,time,censored) ~ cov,data = data.long)

# Step 2: Estimate Probabilities of Remaining Uncensored
data.long$KZti <- NULL
for(i in 1:nrow(data.long))
{
  datai <- data.long[i,]
  sfiCZ <- survfit(CZ, newdata = datai, type = "kaplan-meier")
  ssfiCZ <- summary(sfiCZ,times = datai$Tstart)
  data.long$KZti[i] <- ssfiCZ$surv
}

IDs <- unique(data.long$id)
KZti <- NULL
for(i in IDs)
{
  datai <- subset(data.long, id == i)
  sfiCZ <- survfit(CZ, newdata = datai[1,])
  ssfiCZ <- summary(sfiCZ,times = datai$Tstart)
  KZti <- c(KZti, ssfiCZ$surv)
}
data.long$KZti <- KZti

# Step 3: Compute IPCW Weights

data.long$Weight <- 1/data.long$KZti

survNoIPCW <- survfit(Surv(Tstart,time,status) ~ 1,data=data.long)
survIPCW <- survfit(Surv(Tstart,time,status) ~ 1,data=data.long,weights=Weight)

par(mfrow=c(1,2))
ggsurvplot(survNoIPCW, data = data.long)
ggsurvplot(survIPCW, data = data.long) 

  