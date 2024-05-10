setwd("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets")

# load packages
library(data.table)
library(dplyr)
library(OpenMx)
# convert twin data into long-form family-level data

# positive affect
pa.mz = fread('pa.av.mz.5.23.23.csv',data.table = F)
pa.mz$intercept = 1
pa.mz$zero = 0
pa.dz = fread('pa.av.dz.5.23.23.csv',data.table = F)
pa.dz$intercept = 1
pa.dz$zero = 0


# list of variables to include in model
Vars = names(pa.mz)[grep('20[12][0912]_.._t1$',names(pa.mz))]
Vars = gsub('_t1','',Vars)

# subset to March 11 - April 11 2019, January 11 - February 11,  2020, March 11 - April 11 2021, 
# March 11 - April 11 2021, and March 11 - April 11 2022

Vars = Vars[c(grep('2019_1[12345]',Vars),
              grep('2020_0[34567]',Vars),
              grep('2020_1[12345]',Vars),
              grep('2021_1[01234]',Vars),
              grep('2022_1[01234]',Vars))]


useVars = c(paste0(Vars,'_t1'),paste0(Vars,'_t2'),'intercept','zero')
MZdat = pa.mz[,useVars]
DZdat = pa.dz[useVars]


# number of columns for each latent variable
v2019 = length(Vars[grep('2019',Vars)])
v12020 = length(Vars[grep('2020_0',Vars)])
v22020 = length(Vars[grep('2020_1',Vars)])
v2021 = length(Vars[grep('2021_1',Vars)])
v2022 = length(Vars[grep('2022_1',Vars)])
nv <- length(Vars)

# lambda weights
count = 0
tmp = c(v2019,v12020,v22020,v2021,v2022)

idmat = matrix(c(rep('data.intercept',times = nv)), # indiv id 
                 ncol = 1)

for(i in 1:length(tmp)){
     col.tmp = c(rep('data.zero',times = count),
                 rep('data.intercept',times = tmp[i]),
                 rep('data.zero',times = nv-count - tmp[i]))
     
     idmat = cbind(idmat, col.tmp)
     count = count + tmp[i]
}
     
     

growth.model<- function(MZdat,DZdat, Vars){
     
     nv <- length(Vars)
     totvars = c(paste0(Vars,'_t1'),paste0(Vars,'_t2'),'intercept','zero')
     ntv = length(totvars)


     DataMZ   <- mxData( observed=MZdat[,totvars], type = 'raw')
     DataDZ   <- mxData( observed=DZdat[,totvars], type = 'raw')
     
     nl <- 6 # number of latent factors
     # individual intercept t1, individual intercept t2, 
     # family intercept,                 Mar-Apr 2019 t1, Jan-Feb 2020 t1, Mar-Apr 2020 t1, 
     # Mar-Apr 2021 t1, Mar-Apr 2022 t1, Mar-Apr 2019 t2, Jan-Feb 2020 t2, Mar-Apr 2020 t2, 
     # Mar-Apr 2021 t2, Mar-Apr 2022 t2, 
     
     # covariance matrix of intercepts
     xlabs = c()
     for(i in 1:nl){
          row = paste0('x',i,i:nl)
          xlabs = append(xlabs,row)
     }
     
     ylabs = gsub('x','y',xlabs)
     zlabs = gsub('x','z',xlabs)
     
     
     X <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= xlabs,
                       byrow = F, name="X")
     
     diag(X$values) = .5
     
     Y <- mxMatrix(type="Lower", nrow=nl, ncol=nl, 
                   free=T,
                   values=c(rep(.1, times = nl*(nl+1)/2)), 
                   labels=ylabs,
                   byrow = F, name="Y") 
          
     diag(Y$values) = .2
     
     Z <- mxMatrix(type="Lower", nrow=nl, ncol=nl, 
                   free=T,
                   values=c(rep(.2, times = nl*(nl+1)/2)), 
                   labels=zlabs,
                   byrow = F, name="Z") 
     
     diag(Z$values) = .4
     
     Al <- mxAlgebra(X %*% t(X), name="Al")
     Cl <- mxAlgebra(Y %*% t(Y), name="Cl")
     El <- mxAlgebra(Z %*% t(Z), name="El")
     Sigma <- mxAlgebra(expression = Al + Cl + El, name="Sigma")
     corS <- mxAlgebra(cov2cor(Sigma), name="corSigma")
     Astd <-mxAlgebra(Al/Sigma,name = 'Astd')
     Cstd <-mxAlgebra(Cl/Sigma,name = 'Cstd')
     Estd <-mxAlgebra(El/Sigma,name = 'Estd')
     Acor <-mxAlgebra(cov2cor(Al),name ='Acor')
     Ccor <-mxAlgebra(cov2cor(Cl),name ='Ccor')
     Ecor <-mxAlgebra(cov2cor(El),name ='Ecor')
     
     # correlation matrix of intercepts and slopes
     ### specific factors (eg residuals). Here residuals are freely estimated
     
     pathAs <- mxMatrix(type="Diag", nrow=nv, ncol=nv, 
                        free=TRUE, values=.3,
                        labels = 'a11',
                        name = 'as')
     
     pathCs <- mxMatrix(type="Diag", nrow=nv, ncol=nv, 
                        free=TRUE, values=.3,
                        labels = 'c11',
                        name = 'cs')
     
     pathEs <- mxMatrix(type="Diag", nrow=nv, ncol=nv, 
                        free=TRUE, values=.3,
                        labels = 'e11',
                        name = 'es')
     
     
     # contains growth paths(individual, family, and date-specific intercepts)
     lambda <- mxMatrix(type="Full", 
          labels = idmat,
          nrow=nv, ncol=nl,free=FALSE,
          name="lambda", byrow=F)
     # formula for the covariance matrix of the raw data
     covA <- mxAlgebra( expression=lambda %*% Al %*% t(lambda) + as %*% t(as), name="A" ) 
     covC <- mxAlgebra( expression=lambda %*% Cl %*% t(lambda) + cs %*% t(cs), name="C" ) 
     covE <- mxAlgebra( expression=lambda %*% El %*% t(lambda) + es %*% t(es), name="E" )
     
     
         FacMeans <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=0, 
                          labels = c('int','MA2019','JF2020','MA2020','MA2021','MA2022'),
                          name="FacMeans")
     means    <- mxAlgebra(t(lambda%*% FacMeans), name="means")
     expMean <- mxAlgebra(cbind(means, means), name="expMean")
     
     # specify MZ/DZ twin covariance structure with respect to A,C, and E
     
     covMZ <- mxAlgebra( expression= rbind( cbind(A+C+E, A+C),
                                             cbind(A+C,  A+C+E)), name="expCovMZ" )
     covDZ <- mxAlgebra( expression= rbind( cbind(A+C+E, 0.5%x%A+C),
                                             cbind(0.5%x%A+C, A+C+E)), name="expCovDZ" )
     
     ### Combine Groups
     objMZ <- mxExpectationNormal( covariance="expCovMZ", means="expMean",
                                   dimnames = c(paste0(Vars,'_t1'),
                                                paste0(Vars,'_t2')))
     
     objDZ <- mxExpectationNormal( covariance="expCovDZ", means="expMean",
                                   dimnames = c(paste0(Vars,'_t1'),
                                                paste0(Vars,'_t2')))
     
    ### List matrices and algebras to include in the model
     pars <- list( X, Y, Z, 
                   Al, Cl, El,
                   Astd,Cstd,Estd,
                   Acor,Ccor,Ecor, expMean, 
                   pathAs, pathCs, pathEs,
                   covA, covC, covE, Sigma, corS,
                   lambda, FacMeans, means)
     
     funML <- mxFitFunctionML()
     
     ModelMZ <- mxModel( pars, covMZ, DataMZ, objMZ, funML, name="MZ")
     ModelDZ <- mxModel( pars, covDZ, DataDZ, objDZ, funML, name="DZ" )
     
     #run model
     fitML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
     both_mod <- mxModel( "both_mod", ModelMZ, ModelDZ, fitML)
     mod <- mxRun(both_mod, intervals=F,suppressWarnings = T,silent = T)
     
     print('A')
     print(diag(mod$MZ$Al$result))
     
     print('C')
     print(diag(mod$MZ$El$result))
     
     print('E')
     print(diag(mod$MZ$Al$result))
     #####################
     # model constraints #
     #####################
     
     # all As are the same vs. all As free
     
     X <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= c('x11','x1cov', 'x1cov', 'x1cov', 'x1cov', 'x1cov',
                                       'xcon','xcon','xcon','xcon','xcon',
                                              'xcon','xcon','xcon','xcon',
                                                     'xcon','xcon','xcon',
                                                            'xcon','xcon',
                                                                   'xcon'),
                       byrow = F, name="X")
     

     pars <- list( X, Y, Z, 
                   Al, Cl, El,
                   Astd,Cstd,Estd,
                   Acor,Ccor,Ecor, expMean, 
                   pathAs, pathCs, pathEs,
                   covA, covC, covE, Sigma, corS,
                   lambda, FacMeans, means)
     
     # need to constrain X covariances and variances
     Acon1 = mxConstraint(MZ.Al[2,2] == MZ.Al[3,3],name = 'Acon1')
     Acon2 = mxConstraint(MZ.Al[2,2] == MZ.Al[4,4],name = 'Acon2')
     Acon3 = mxConstraint(MZ.Al[2,2] == MZ.Al[5,5],name = 'Acon3')
     Acon4 = mxConstraint(MZ.Al[2,2] == MZ.Al[6,6],name = 'Acon4')

     #run model
     funML <- mxFitFunctionML()
     ModelMZ <- mxModel( pars, covMZ, DataMZ, objMZ, funML, name="MZ")
     ModelDZ <- mxModel( pars, covDZ, DataDZ, objDZ, funML, name="DZ" )
     mod_Acon <- mxModel( "mod_Acon", ModelMZ, ModelDZ,
                          Acon1,Acon2,Acon3,Acon4,
                          fitML)
     mod_Acon <- mxRun(mod_Acon, intervals=F,suppressWarnings = T,silent = T) 
     
     print('test As constrained')
     print(mxCompare(mod, mod_Acon))
     
     # all Cs are the same vs. all Cs free
    X <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= xlabs,
                       byrow = F, name="X")
     
    Y <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= c('y11','y1cov', 'y1cov', 'y1cov', 'y1cov', 'y1cov',
                                       'ycon','ycon','ycon','ycon','ycon',
                                              'ycon','ycon','ycon','ycon',
                                                     'ycon','ycon','ycon',
                                                            'ycon','ycon',
                                                                   'ycon'),
                       byrow = F, name="Y")
     
     pars <- list( X, Y, Z, 
                   Al, Cl, El,
                   Astd,Cstd,Estd,
                   Acor,Ccor,Ecor, expMean, 
                   pathAs, pathCs, pathEs,
                   covA, covC, covE, Sigma, corS,
                   lambda, FacMeans, means)
     
     
     Ccon1 = mxConstraint(MZ.Cl[2,2] == MZ.Cl[3,3],name = 'Ccon1')
     Ccon2 = mxConstraint(MZ.Cl[2,2] == MZ.Cl[4,4],name = 'Ccon2')
     Ccon3 = mxConstraint(MZ.Cl[2,2] == MZ.Cl[5,5],name = 'Ccon3')
     Ccon4 = mxConstraint(MZ.Cl[2,2] == MZ.Cl[6,6],name = 'Ccon4')

     #run model
     
     funML <- mxFitFunctionML()
     ModelMZ <- mxModel( pars, covMZ, DataMZ, objMZ, funML, name="MZ")
     ModelDZ <- mxModel( pars, covDZ, DataDZ, objDZ, funML, name="DZ" )
     mod_Ccon <- mxModel( "mod_Ccon", ModelMZ, ModelDZ,Ccon1,Ccon2,Ccon3,Ccon4, fitML)
     mod_Ccon <- mxRun(mod_Ccon, intervals=F,suppressWarnings = T,silent = T) 
     
     print('test Cs constrained')
     print(mxCompare(mod, mod_Ccon))
     
     
     # all Es are the same vs. all Es free
    
     Y <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= ylabs,
                       byrow = F, name="Y")
     
    Z <- mxMatrix(type="Symm", nrow=nl, ncol=nl,
                       free=T,
                       values=c(.4),
                       labels= c('z11','z1cov', 'z1cov', 'z1cov', 'z1cov', 'z1cov',
                                       'zcon','zcon','zcon','zcon','zcon',
                                              'zcon','zcon','zcon','zcon',
                                                     'zcon','zcon','zcon',
                                                            'zcon','zcon',
                                                                   'zcon'),
                       byrow = F, name="Z")
     
     pars <- list( X, Y, Z, 
                   Al, Cl, El,
                   Astd,Cstd,Estd,
                   Acor,Ccor,Ecor, expMean, 
                   pathAs, pathCs, pathEs,
                   covA, covC, covE, Sigma, corS,
                   lambda, FacMeans, means)
     
     
     Econ1 = mxConstraint(MZ.El[2,2] == MZ.El[3,3],name = 'Econ1')
     Econ2 = mxConstraint(MZ.El[2,2] == MZ.El[4,4],name = 'Econ2')
     Econ3 = mxConstraint(MZ.El[2,2] == MZ.El[5,5],name = 'Econ3')
     Econ4 = mxConstraint(MZ.El[2,2] == MZ.El[6,6],name = 'Econ4')

     #run model
     
     funML <- mxFitFunctionML()
     ModelMZ <- mxModel( pars, covMZ, DataMZ, objMZ, funML, name="MZ")
     ModelDZ <- mxModel( pars, covDZ, DataDZ, objDZ, funML, name="DZ" )
     mod_Econ <- mxModel( "mod_Ccon", ModelMZ, ModelDZ,Econ1,Econ2,Econ3,Econ4, fitML)
     mod_Econ <- mxRun(mod_Ccon, intervals=F,suppressWarnings = T,silent = T) 
     
     print('test Es constrained')
     print(mxCompare(mod, mod_Econ))
          }


# run this model for positive and negative affect, pts, and distance


# positive affect
sink(file = "C:/Users/alexa877/Documents/UMN/Research/Covid Location project/RAship 2022/Graphics/model_comparisons_ACE.txt",append = T)
print("Positive Affect")
growth.model(MZdat,DZdat,Vars)

##################
# negative affect#
##################

na.mz = fread('na.av.mz.5.23.23.csv',data.table = F)
na.mz$intercept = 1
na.mz$zero = 0

na.dz = fread('na.av.dz.5.23.23.csv',data.table = F)
na.dz$intercept = 1
na.dz$zero = 0

MZdat = na.mz[,useVars]
DZdat = na.dz[,useVars]

print("Negative Affect")
growth.model(MZdat,DZdat,Vars)


###################
# number of points#
###################

pts.mz = fread('numpts.mz.5.23.23.csv',data.table = F)
pts.dz = fread('numpts.dz.5.23.23.csv',data.table = F)
pts.mz = fread('numpts.mz.5.23.23.csv',data.table = F)
pts.mz$intercept = 1
pts.mz$zero = 0

pts.dz = fread('numpts.dz.5.23.23.csv',data.table = F)
pts.dz$intercept = 1
pts.dz$zero = 0

MZdat = pts.mz[,useVars]
DZdat = pts.dz[,useVars]

print("Daily Locations Visited")
growth.model(MZdat,DZdat,Vars)

#####################
# distance travelled#
#####################

dist.mz = fread('dist.mz.5.23.23.csv',data.table = F)
dist.mz$intercept = 1
dist.mz$zero = 0

dist.dz = fread('dist.dz.5.23.23.csv',data.table = F)
dist.dz$intercept = 1
dist.dz$zero = 0

MZdat = dist.mz[,useVars]
DZdat = dist.dz[,useVars]

print("Distance Travelled")
growth.model(MZdat,DZdat,Vars)

sink(file = NULL)








