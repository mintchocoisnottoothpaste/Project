library(StMoMo)

read.kordemogdata <- function(file,popfile,type,label,max.mx=10,skip=2,popskip=skip,lambda, scale=1){
  if(missing(lambda))
  {
    if(type=="mortality")
      lambda <- 0
    else if(type=="fertility")
      lambda <- 0.4
    else
      lambda <- 1
  }
  
  mfile <- !missing(file)
  mpopfile <- !missing(popfile)
  
  obj <- list(type=type,label=label,lambda=lambda)
  
  if(mfile)
  {
    tmp1 <- file
    obj$year=sort(unique(tmp1[,1]))
    n <- length(obj$year)
    m <- length(unique(tmp1[,2]))
    obj$age <- tmp1[1:m,2]
    mnames <- names(tmp1)[-c(1,2)]
    n.mort <- length(mnames)
    obj$rate <- list()
    for(i in 1:n.mort)
    {
      obj$rate[[i]] = matrix(tmp1[,i+2], nrow=m, ncol=n)
      # Check bounds
      obj$rate[[i]][obj$rate[[i]] < 0] <- NA
      obj$rate[[i]][obj$rate[[i]] > max.mx] <- max.mx
      if(scale > 1)
        obj$rate[[i]] <- obj$rate[[i]] / scale
      dimnames(obj$rate[[i]]) <- list(obj$age,obj$year)
    }
    names(obj$rate) = tolower(mnames)
  }
  
  if(mpopfile)
  {
    tmp2 <- popfile
    obj$year=sort(unique(tmp2[,1]))
    n <- length(obj$year)
    m <- length(unique(tmp2[,2]))
    obj$age <- tmp2[1:m,2]
    pnames <- names(tmp2)[-c(1,2)]
    if(mfile)
    {
      if(sum(pnames==mnames) != length(pnames))
      {
        warning("Population names different from rates names")
        if(length(pnames) <- length(mnames))
          pnames <- mnames
      }
      if(n!=ncol(obj$rate[[1]]) | m != nrow(obj$rate[[1]]))
        warning("Population matrices different size from rates matrices")
    }
    p.mort <- length(pnames)
    obj$pop <- list()
    for(i in 1:p.mort)
    {
      obj$pop[[i]] = matrix(tmp2[,i+2], nrow=m, ncol=n)
      # Check bounds
      obj$pop[[i]][obj$pop[[i]] < 0] <- NA
      dimnames(obj$pop[[i]]) <- list(obj$age,obj$year)
    }
    names(obj$pop) = tolower(pnames)
  }
  
  junk <- options(warn=-1)
  obj$age <- as.numeric(as.character(obj$age))
  options(warn=junk$warn)
  if(is.na(obj$age[m]))
    obj$age[m] <- 2*obj$age[m-1] - obj$age[m-2]
  
  return(structure(obj,class="demogdata"))
}

###########################################################################3

# Lee-Carter model
LC <- lc(link = "logit")

# CBD model
CBD <- cbd()

# APC model, RH model and M7 model
APC <- apc(link = "logit")
RH <- rh(link = "logit",  cohortAgeFun = "1")
M7 <- m7()

# PLAT model
f2 <- function(x, ages) mean(ages) - x
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  phiReg<-lm(gc~1+c+I(c^2),na.action=na.omit)
  phi <- coef(phiReg)
  gc<-gc-phi[1]-phi[2]*c-phi[3]*c^2
  kt[2,]<-kt[2,]+2*phi[3]*t
  kt[1,]<-kt[1,]+phi[2]*t+phi[3]*(t^2-2*xbar*t)
  ax<-ax+phi[1]-phi[2]*x+phi[3]*x^2
  ci <- rowMeans(kt, na.rm = TRUE)
  ax<-ax+ci[1]+ci[2]*(xbar-x)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  list(ax=ax,bx=bx,kt=kt,b0x=b0x,gc=gc)
}

PLAT <- StMoMo(link = "logit", staticAgeFun = TRUE,
               periodAgeFun = c("1", f2), cohortAgeFun = "1", constFun = constPlat)  


###########################################################################
Whole_Cancer_Male<-subset(Whole_Cancer,Sex=="Male");Whole_Cancer_Male <- Whole_Cancer_Male[,-c(1,2)]
Whole_Cancer_Male<-melt(Whole_Cancer_Male, idvar=c("Age"))
Whole_Cancer_Male$AGE_GRP<-seq(1,18)
Whole_Cancer_Male<-Whole_Cancer_Male[,-1];Whole_Cancer_Male<-Whole_Cancer_Male[,c(1,3,2)]
colnames(Whole_Cancer_Male) <-c("Year","Age","Death")
Whole_Cancer_Male <-subset(Whole_Cancer_Male,Year!=2020)
Whole_Cancer_Male$Pop <-population_m_data$Population
Whole_Cancer_Male$Male <-Whole_Cancer_Male$Death/Whole_Cancer_Male$Pop*1000
Whole_Cancer_Male <-Whole_Cancer_Male[,-c(3,4)]
colnames(population_m_data) <-c("Year","Age","Male")
population_m_data<-arrange(population_m_data,Year,Age)

KORDATA<-read.kordemogdata(Whole_Cancer_Male,population_m_data,type="mortality",label="KOR")


###########################################################################
ages.fit <-seq(6,18)
wxt<-genWeightMat(ages = ages.fit, years = KORDATA$year,clip = 3)


LCfit <- fit(LC, data = KORDATA, ages.fit = ages.fit, wxt = wxt)
APCfit <- fit(APC, data = KORDATA, ages.fit = ages.fit, wxt = wxt)
CBDfit <- fit(CBD, data = KORDATA, ages.fit = ages.fit, wxt = wxt)
M7fit <- fit(M7, data = KORDATA, ages.fit = ages.fit, wxt = wxt)
PLATfit <- fit(PLAT, data = KORDATA, ages.fit = ages.fit, wxt = wxt)





