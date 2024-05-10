sink(file = "C:/Users/alexa877/Documents/UMN/Research/Covid Location project/RAship 2022/Graphics/model_comparisons.txt")
print("PA AVG Model Comparisons")
setwd("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets")

# load packages
library(data.table)
library(dplyr)
library(OpenMx)
# convert twin data into long-form family-level data

# positive affect
pa.mz = fread('pa.av.mz.5.23.23.csv',data.table = F)
pa.dz = fread('pa.av.dz.5.23.23.csv',data.table = F)

pa.fam = bind_rows(pa.mz,pa.dz)
pa.fam$intercept = 1
pa.fam$zero = 0

# list of variables to include in model
Vars = names(pa.fam)[grep('20[12][0912]_.._t1$',names(pa.fam))]
Vars = gsub('_t1','',Vars)

# subset to March 11 - April 11 2019, January 11 - February 11,  2020, March 11 - April 11 2021, 
# March 11 - April 11 2021, and March 11 - April 11 2022

Vars = Vars[c(grep('2019_1[12345]',Vars),
              grep('2020_0[34567]',Vars),
              grep('2020_1[12345]',Vars),
              grep('2021_1[01234]',Vars),
              grep('2022_1[01234]',Vars))]


useVars = c(paste0(Vars,'_t1'),paste0(Vars,'_t2'))
dat = pa.fam[,useVars]


# number of columns for each latent variable
v2019 = length(Vars[grep('2019',Vars)])
v12020 = length(Vars[grep('2020_0',Vars)])
v22020 = length(Vars[grep('2020_1',Vars)])
v2021 = length(Vars[grep('2021_1',Vars)])
v2022 = length(Vars[grep('2022_1',Vars)])
nv <- length(Vars)

# lambda weights
count = 0
tmp = c(v2019,v12020,v22020,v2021,v2022,
        v2019,v12020,v22020,v2021,v2022)

idmat = matrix(c(rep(1,times = nv*2), # fam id
                 rep(1,times = nv), rep(0, times = nv), # indiv id 1
                 rep(0, times = nv), rep(1,times = nv)), # indiv id 2
               ncol = 3)

for(i in 1:length(tmp)){
     col.tmp = c(rep(0,times = count),rep(1,times = tmp[i]),rep(0,times = 2*nv-count - tmp[i]))
     
     idmat = cbind(idmat, col.tmp)
     count = count + tmp[i]
}
     
     

growth.model<- function(dat, Vars){
     
     nv <- length(Vars)
     totvars = c(paste0(Vars,'_t1'),paste0(Vars,'_t2'))
     ntv = length(totvars)

     ############
     mtfsData   <- mxData( observed=dat[,totvars], type="raw" )
     
     nl <- 13 # number of latent factors
     # individual intercept t1, individual intercept t2, 
     # family intercept,                 Mar-Apr 2019 t1, Jan-Feb 2020 t1, Mar-Apr 2020 t1, 
     # Mar-Apr 2021 t1, Mar-Apr 2022 t1, Mar-Apr 2019 t2, Jan-Feb 2020 t2, Mar-Apr 2020 t2, 
     # Mar-Apr 2021 t2, Mar-Apr 2022 t2, 
     
     # covariance matrix of intercepts
     X <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.5),
                       labels= c(paste0('x1',1:13),paste0('x2',2:13),paste0('x3',3:13),
                                 paste0('x4',4:13),paste0('x5',5:13),paste0('x6',6:13),
                                 paste0('x7',7:13),paste0('x8',8:13),paste0('x9',9:13),
                                 paste0('x10',10:13),paste0('x10',11:13),
                                 paste0('x10',12:13),paste0('x1013')),
                       byrow = F, name="X")
     
     Sigma = mxAlgebra(X %*% t(X), name = 'Sigma')
     # correlation matrix of intercepts and slopes
     corS <- mxAlgebra(cov2cor(Sigma), name="corSigma")
     # residuals of the raw data
     Resids <-  mxMatrix(type="Diag", nrow=ntv, ncol=ntv,
                         labels = c(rep('rt1',times = nv),rep('rt2',times = nv)),
                         free=TRUE, values=.3, name="resid") 
     
     # contains growth paths(individual, family, and date-specific intercepts)
     lambda <- mxMatrix(type="Full", 
          values = idmat,
          nrow=ntv, ncol=nl,free=FALSE,
          name="lambda", byrow=F)
     # formula for the covariance matrix of the raw data
     Cov <- mxAlgebra( expression= lambda %*% Sigma %*% t(lambda) + resid %*% t(resid), name="Cov" ) 
     # formula for the means of the raw data
     # means of the latent intercepts and slopes
     FacMeans <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     'MA2019t1','JF2020t1','MA2020t1','MA2021t1','MA2022t1',
                                     'MA2019t2','JF2020t2','MA2020t2','MA2021t2','MA2022t2'),
                          name="FacMeans")
     means    <- mxAlgebra(t(lambda%*% FacMeans), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeans,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     mod = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('unconstrained means:')
     print(mod$FacMeans$values)
     #####################
     # model constraints #
     #####################
     
     # precovid 2019 = covid 2020
     FacMeanscon1920 <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     'MA2020t1','JF2020t1','MA2020t1','MA2021t1','MA2022t1',
                                     'MA2020t2','JF2020t2','MA2020t2','MA2021t2','MA2022t2'),
                          name="FacMeans1920")
     
     means    <- mxAlgebra(t(lambda%*% FacMeans1920), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeanscon1920,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     mod1920 = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('test mar-apr 2019 and mar-apr 2020')
     print(mxCompare(mod, mod1920))
     print(mod$FacMeans$values)
     
     # precovid 2020 = covid 2020
     FacMeanscon2020 <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     'MA2019t1','MA2020t1','MA2020t1','MA2021t1','MA2022t1',
                                     'MA2019t2','MA2020t2','MA2020t2','MA2021t2','MA2022t2'),
                          name="FacMeans2020")
     
     means    <- mxAlgebra(t(lambda%*% FacMeans2020), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeanscon2020,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     mod2020 = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('test jan-feb 2020 and mar-apr 2020')
     print(mxCompare(mod, mod2020))
     print(mod$FacMeans$values)
     
     # covid 2020 = post-covid 2021
     FacMeanscon2021 <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     'MA2019t1','JF2020t1','MA2020t1','MA2020t1','MA2022t1',
                                     'MA2019t2','JF2020t2','MA2020t2','MA2020t2','MA2022t2'),
                          name="FacMeans2021")
     
     means    <- mxAlgebra(t(lambda%*% FacMeans2021), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeanscon2021,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     mod2021 = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('test mar-apr 2020 and mar-apr 2021')
     print(mxCompare(mod, mod2021))
     print(mod$FacMeans$values)
     
     # covid 2020 = post-covid 2022
     FacMeanscon2022 <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     'MA2019t1','JF2020t1','MA2020t1','MA2021t1','MA2020t1',
                                     'MA2019t2','JF2020t2','MA2020t2','MA2021t2','MA2020t2'),
                          name="FacMeans2022")
     
     means    <- mxAlgebra(t(lambda%*% FacMeans2022), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeanscon2022,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     mod2022 = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('test mar-apr 2020 and mar-apr 2022')
     print(mxCompare(mod, mod2022))
     print(mod$FacMeans$values)
     
     # what if we hold everything equal except covid?
     # covid 2020 = post-covid 2022
     FacMeanscovid <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('fam_int','t1_int','t2_int',
                                     '20xxt1','20xxt1','MA2020t1','20xxt1','20xxt1',
                                     '20xxt2','20xxt1','MA2020t2','20xxt1','20xxt1'),
                          name="FacMeanscovid")
     
     means    <- mxAlgebra(t(lambda%*% FacMeanscovid), name="means")
     ### group parameters together
     pars <- list(X,Sigma,corS,FacMeanscovid,Resids,lambda,Cov,means) 
     funML        <- mxFitFunctionML()
     # expectation for raw data is covariance matrix 'Cov' with means 'means'
     exp = mxExpectationNormal(covariance = 'Cov',means = 'means',dimnames = totvars)
     mtfsModel <- mxModel(mtfsData, pars, exp, funML, name="model")
     mxOption(NULL,"Default optimizer","SLSQP")
     modcovid = mxRun(mtfsModel, suppressWarnings=TRUE,silent = T,intervals = F)
     
     print('test mar-apr 2020 vs. all others')
     print(mxCompare(mod, modcovid))
     print(mod$FacMeans$values)
     
     
     }


# run this model for positive and negative affect, pts, and distance


# positive affect
growth.model(dat,Vars)

##################
# negative affect#
##################

na.mz = fread('na.av.mz.5.23.23.csv',data.table = F)
na.dz = fread('na.av.dz.5.23.23.csv',data.table = F)

na.fam = bind_rows(na.mz,na.dz)
na.fam$intercept = 1
na.fam$zero = 0
dat = na.fam[,useVars]
growth.model(dat,Vars)


###################
# number of points#
###################

pts.mz = fread('numpts.mz.5.23.23.csv',data.table = F)
pts.dz = fread('numpts.dz.5.23.23.csv',data.table = F)

pts.fam = bind_rows(pts.mz,pts.dz)
pts.fam$intercept = 1
pts.fam$zero = 0
dat = pts.fam[,useVars]
growth.model(dat,Vars)



#####################
# distance travelled#
#####################

dist.mz = fread('dist.mz.5.23.23.csv',data.table = F)
dist.dz = fread('dist.dz.5.23.23.csv',data.table = F)

dist.fam = bind_rows(dist.mz,dist.dz)
dist.fam$intercept = 1
dist.fam$zero = 0
dat = dist.fam[,useVars]
growth.model(dat,Vars)


sink()








