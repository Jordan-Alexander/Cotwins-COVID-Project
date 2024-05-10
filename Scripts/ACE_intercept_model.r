# write ACE longitudinal intercept model

# load packages ----
library(OpenMx)
library(data.table)
library(dplyr)
library(lubridate)


# load data ----
MZdata = fread("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets\\pa.av.mz.5.23.23.csv",data.table = F)
DZdata = fread("C:\\Users\\alexa877\\Documents\\UMN\\Research\\Covid Location project\\RAship 2022\\Datasets\\pa.av.dz.5.23.23.csv",data.table = F)

# declare variables ----
# pull column names of phenotype data for t1, then strip '_t1' from these
# so applicable to 't1' and 't2'

variable_index = grep('2019_[0-5][0-9]_t1',names(MZdata))
variables = names(MZdata)[variable_index]
variables = gsub('_t1','',variables)

# add intercept definition variable
MZdata$Intercept = 1
MZdata$Zero = 0

DZdata$Intercept = 1
DZdata$Zero = 0

# write intercept model ----

# intermediate functions ----

specify_CIs = function(nl){
     
     cisA = paste0('MZ.Astd[',1:nl,',',1:nl,']')
     cisC = paste0('MZ.Cstd[',1:nl,',',1:nl,']')
     cisE = paste0('MZ.Estd[',1:nl,',',1:nl,']')
     cis = c(cisA,cisC,cisE)
     return(cis)
     }
          
# maps weeks to months to create intercept matrix
map_weeks_to_months = function(variables){
     tmp = c()
     for(i in 1:length(variables)){
     wyr = variables[i]
     year = substr(wyr,1,4)
     week = substr(wyr, 6,7)
     month <- month(as.Date(paste(year, week, 1),
                             format = "%Y %U %u"),
                    label = T)
     
     tmp = append(tmp,month)
     }
     return(tmp)
     }


# use this to create the values for our i x j intercept lambda matrix, with
# cells i,j equal to 1 if week i is in month j and equal to 0 if not

get_intercept_path_values = function(months){
     values_intercept = c()
     index = 0
     
     for(i in unique(months)){
          num_weeks = sum(months == i)
          
          if(length(values_intercept) == 0){
               col = c(rep('data.Intercept',times = num_weeks),
                       rep('data.Zero', times = numvars - num_weeks))
               
               values_intercept = append(values_intercept,col)
               index = index + num_weeks
               
          } else{
               col = c(rep('data.Zero', times = index),
                       rep('data.Intercept',times = num_weeks),
                       rep('data.Zero', times = numvars - index - num_weeks))
               values_intercept = append(values_intercept, col)
               index = index + num_weeks}
          }
     
     return(values_intercept)
     }
               
# specify mx options, change optimizer and multithread
mxOption(NULL, "Default optimizer", "SLSQP")
#mxOption(NULL, 'Number of Threads', 24)

# number of random intercepts is number of months in data
nmonths = round(length(variables)/52.14*12,digits = 0)

# Main ACE model function ----

monthly.intercept.ACE.model = function(MZdata,DZdata, variables, 
                                       nl=nmonths, months = map_weeks_to_months(variables)){
     
     # format data ----
     
     # specify number of variables, number total variables, names of pheno
     # and definition variables
     numvars = length(variables)
     numtotalvars = numvars*2
     phenovars <- c(paste0(variables,'_t1'),paste0(variables,'_t2'))
     defvars<- c('Zero','Intercept')
     
     # strip MZ and DZ data to pheno and definition variables
     MZdat<- MZdata %>% select(all_of(phenovars), all_of(defvars)) %>% as.data.frame()
     DZdat<- DZdata %>% select(all_of(phenovars), all_of(defvars)) %>% as.data.frame()
     
     # some variables convert to logical when loaded, change back to numeric
     MZdat<-as.data.frame(apply(MZdat,2, as.numeric))
     DZdat<-as.data.frame(apply(DZdat,2, as.numeric))
     
     # convert to Mx data object
     DataMZ   <- mxData( observed=MZdat, type="raw" )
     DataDZ   <- mxData( observed=DZdat, type="raw" )
     
     # cholesky matrices ----
     
     # Matrices ac, cc, and ec to store a, c, and e path coefficients for latent phenotype(s) 
     
     # create x,y, and z labels
     xlabs = c()
     for(i in 1:nl){
          row = paste0('x',i,i:nl)
          xlabs = append(xlabs,row)
     }
     
     ylabs = gsub('x','y',xlabs)
     zlabs = gsub('x','z',xlabs)     
     
     # create a, c, and e matrices for cholesky decomposition
     
     X <- mxMatrix(type="Lower", nrow=nl, ncol=nl, 
                   free=rep(TRUE,times = nl*(nl+1)/2),
                   values=c(rep(.3, times = nl*(nl+1)/2)), 
                   labels=xlabs,
                   byrow = F, name="X") 
     
     diag(X$values) = .5
     
     Y <- mxMatrix(type="Lower", nrow=nl, ncol=nl, 
                   free=rep(TRUE,times = nl*(nl+1)/2),
                   values=c(rep(.1, times = nl*(nl+1)/2)), 
                   labels=ylabs,
                   byrow = F, name="Y") 
          
     diag(Y$values) = .2
     
     Z <- mxMatrix(type="Lower", nrow=nl, ncol=nl, 
                   free=rep(TRUE,times = nl*(nl+1)/2),
                   values=c(rep(.2, times = nl*(nl+1)/2)), 
                   labels=zlabs,
                   byrow = F, name="Z") 
     
     diag(Z$values) = .4
     
     
     # cholesky algebras ----
     
     Al <- mxAlgebra(X %*% t(X), name="Al")
     Cl <- mxAlgebra(Y %*% t(Y), name="Cl")
     El <- mxAlgebra(Z %*% t(Z), name="El")
     VarL <- mxAlgebra(expression = Al + Cl + El, name="VarL")
     corL <- mxAlgebra(cov2cor(VarL), name="corL")
     Astd <-mxAlgebra(Al/VarL,name = 'Astd')
     Cstd <-mxAlgebra(Cl/VarL,name = 'Cstd')
     Estd <-mxAlgebra(El/VarL,name = 'Estd')
     Acor <-mxAlgebra(cov2cor(Al),name ='Acor')
     Ccor <-mxAlgebra(cov2cor(Cl),name ='Ccor')
     Ecor <-mxAlgebra(cov2cor(El),name ='Ecor')
     
     # residual matrices ----
     ### Matrices as, cs, and es to store a, c, and e path coefficients for
     ### specific factors (eg residuals). Here residuals are freely estimated
     
     pathAs <- mxMatrix(type="Diag", nrow=numvars, ncol=numvars, 
                        free=TRUE, values=.3, name="as") 
     
     pathCs <- mxMatrix(type="Diag", nrow=numvars, ncol=numvars, 
                        free=TRUE, values=.3, name="cs")
     
     pathEs <- mxMatrix(type="Diag", nrow=numvars, ncol=numvars, 
                        free=TRUE, values=.3, name="es")
     
     # intercept matrix ----
     
     intercept_values = get_intercept_path_values(months)
                       
     pathintercept <- mxMatrix(type="Full", 
                               nrow=numvars, 
                               ncol=nl,
                               free=FALSE,
                               labels = intercept_values,
                               name="intercept_loadings", 
                               byrow=F)
     
     # model algebras ----
     
     # algebras for the covariance matrix of the random intercept model
     covA <- mxAlgebra( expression=intercept_loadings %*% Al %*% t(intercept_loadings) + as %*% t(as), name="A" ) 
     covC <- mxAlgebra( expression=intercept_loadings %*% Cl %*% t(intercept_loadings) + cs %*% t(cs), name="C" ) 
     covE <- mxAlgebra( expression=intercept_loadings %*% El %*% t(intercept_loadings) + es %*% t(es), name="E" ) 
     
     # algebras for the monthly and weekly means
     FacMeans <- mxMatrix("Full",nrow=nl,ncol=1, free=c(TRUE), values=.5, name="FacMeans")
     means    <- mxAlgebra(t(intercept_loadings %*% FacMeans), name="means")
     expMean <- mxAlgebra(cbind(means, means), name="expMean")
     
     # specify MZ/DZ twin covariance structure with respect to A,C, and E
     
     covMZ <- mxAlgebra( expression= rbind( cbind(A+C+E, A+C),
                                             cbind(A+C,  A+C+E)), name="expCovMZ" )
     covDZ <- mxAlgebra( expression= rbind( cbind(A+C+E, 0.5%x%A+C),
                                             cbind(0.5%x%A+C, A+C+E)), name="expCovDZ" )
     
     ### Combine Groups
     objMZ <- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=phenovars)
     objDZ <- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames= phenovars)
     
     ### List matrices and algebras to include in the model
     pars <- list( X, Y, Z, 
                   Al, Cl, El,
                   Astd,Cstd,Estd,
                   Acor,Ccor,Ecor, 
                   pathAs, pathCs, pathEs,
                   covA, covC, covE, expMean, VarL, corL,
                   pathintercept, FacMeans, means) 
     
     ###
     cis = specify_CIs(nl)
     CI = mxCI(cis)
     

     funML <- mxFitFunctionML()
     
     ModelMZ <- mxModel( pars, covMZ, DataMZ, objMZ, funML, name="MZ")
     ModelDZ <- mxModel( pars, covDZ, DataDZ, objDZ, funML, name="DZ" )
     
     #run model
     fitML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
     LGLinear <- mxModel( "LinearGrowthACE", ModelMZ, ModelDZ, fitML)
     LGLinear<-mxModel(LGLinear, CI)
     LGFitLinear <- mxRun(LGLinear, intervals=T)
     return(LGFitLinear)
     }
     
     }
     


