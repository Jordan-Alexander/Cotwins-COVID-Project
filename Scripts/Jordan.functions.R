###Repository of useful functions
library(Hmisc)

#creates a list of filenames with structure '<filename>.replicate<repnum>.rds'
get.replicate.filenames<- function(string, reps)
{
     filenames<-paste(string,'.replicate',c(1:reps),'.rds',sep = '')
     return(filenames)
}

#function to pull lower 95% CI for bootstrap
lower<- function(x)
{
     low<-round(quantile(x, 0.025), digits = 3)
     low<-as.numeric(low)
     return(low)
}
#function to pull upper 95% CI for bootstrap
upper<- function(x)
{
     up<-round(quantile(x, 0.975), digits = 3)
     up<-as.numeric(up)
     return(up)
}

#function to pull mean for bootstrap
mid<- function(x)
{
     middle<-round(mean(x), digits = 3)
     middle<-as.numeric(middle)
     return(middle)
}

#function to create a row including the varname, mean, lowerci, and upperci for a bootstrap
extract.param<- function(name,x)
{
     line<-data.frame(name = name, mean = mid(x), lower = lower(x), upper = upper(x))
     return(line)
}



#function to create twin data. takes a dataframe, an id variable, a family variable, and a zygosity variable and returns
# an MZ and DZ wideform dataset

gettwindata<- function(df, idvar, familyvar){
     #adding new id, family, and zygosity variables for ease of reference
     df$idtmp<-df[,idvar]
     df$familytmp<-df[,familyvar]
     #separating into twin 1 and twin 2 and combining
     df.t1<-df[!duplicated(df$familytmp),]
     df.t2<-df[duplicated(df$familytmp),]
     df.wide<-left_join(df.t1,df.t2, by = c('familytmp'))
     names(df.wide)<-gsub('\\.x','_t1', names(df.wide))
     names(df.wide)<-gsub('\\.y','_t2', names(df.wide))
     which(names(df.wide)=='familytmp')
     return(df.wide)
}

getmzdata<- function(df, idvar, familyvar, zygvar){
     
     #adding new id, family, and zygosity variables for ease of reference
     df <- df %>% mutate(idtmp = idvar,
                         familytmp = familyvar,
                         zygtmp = zygvar)
     #separating into twin 1 and twin 2 and combining
     df.t1<-df[!duplicated(df$familytmp),]
     df.t2<-df[duplicated(df$familytmp),]
     df.wide<-left_join(df.t1,df.t2, by = c('familytmp','zygtmp'))
     names(df.wide)<-gsub('\\.x','_t1', names(df.wide))
     names(df.wide)<-gsub('\\.y','_t2', names(df.wide))
     which(names(df.wide)=='familytmp')
     mzdata<-subset(df.wide, (df.wide$zygtmp == 1 | df.wide$zygtmp == 'MZ'))
     mzdata<-subset(mzdata, select = -c(idtmp_t1,familytmp,zygtmp,idtmp_t2))
     return(mzdata)
}

getdzdata<- function(df, idvar, familyvar, zygvar){
     
     #adding new id, family, and zygosity variables for ease of reference
     df <- df %>% mutate(idtmp = idvar,
                         familytmp = familyvar,
                         zygtmp = zygvar)
     #separating into twin 1 and twin 2 and combining
     df.t1<-df[!duplicated(df$familytmp),]
     df.t2<-df[duplicated(df$familytmp),]
     df.wide<-left_join(df.t1,df.t2, by = c('familytmp','zygtmp'))
     names(df.wide)<-gsub('\\.x','_t1', names(df.wide))
     names(df.wide)<-gsub('\\.y','_t2', names(df.wide))
     which(names(df.wide)=='familytmp')
     dzdata<-subset(df.wide, (df.wide$zygtmp == 1 | df.wide$zygtmp == 'DZ'))
     dzdata<-subset(dzdata, select = -c(idtmp_t1,familytmp,zygtmp,idtmp_t2))
     return(dzdata)
}



# z transform a variable

zscale<- function(vec){
     vecz<- (vec - mean(vec, na.rm = T))/
          sd(vec, na.rm = T)
     return(vecz)}
     



#########################################
##### ACE models I stole from david lol##
#########################################

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var and t2_var are the column names for the
# phenotypes of the first and second twin, respectively.
library(psych)
library(GPArotation)
library(lme4)
library(lmerTest)
library(ggplot2)
library(Hmisc)
library(tidyr)
library(lubridate)
library(readr)
library(dplyr)
library(reshape2)




get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

cor_graph <- function(cormat1){
  upper_tri <- get_upper_tri(cormat1)
  print(upper_tri)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  print(melted_cormat)
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  ggheatmap +   geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}


fit_ace_univariate <- function (mz_data, dz_data, t1_var, t2_var) {
  require(OpenMx)
  
  mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
  mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
  
  phenos <- c(t1_var, t2_var)
  nv <- 1 # number of observed variables
  
  # Matrices ac, cc, and ec to store a, c, and e path coefficients
  # for latent phenotype(s)
  
  X <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .25,
      labels = "x",
      name = "X"
    )
  Y <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .35,
      labels = "y",
      name = "Y"
    )
  Z <-
    mxMatrix(
      type = "Lower",
      nrow = nv,
      ncol = nv,
      free = TRUE,
      values = .45,
      labels = "z",
      name = "Z"
    )
  
  A <- mxAlgebra(X %*% t(X), name = "A")
  C <- mxAlgebra(Y %*% t(Y), name = "C")
  E <- mxAlgebra(Z %*% t(Z), name = "E")
  
  cov_P <- mxAlgebra(expression = A + C + E, name = "V")
  
  a2 <- mxAlgebra(expression = A / V, name = "a2")
  c2 <- mxAlgebra(expression = C / V, name = "c2")
  e2 <- mxAlgebra(expression = E / V, name = "e2")
  
  mat_I <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
  inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
  mean_G <-
    mxMatrix(
      type = "Full",
      nrow = 1,
      ncol = nv * 2,
      free = TRUE,
      values = 1,
      labels = c("m1", "m1"),
      name = "expMean"
    )
  
  cov_mz <-
    mxAlgebra(
      expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
      name = "expCovMZ"
    )
  cov_dz <-
    mxAlgebra(
      expression =
        rbind(cbind(A + C + E, 0.5 %x% A + C), cbind(0.5 %x% A + C, A + C + E)),
      name="expCovDZ")
  
  ### Combine Groups
  obj_mz <-
    mxExpectationNormal(covariance = "expCovMZ",
                        means = "expMean",
                        dimnames = phenos)
  obj_dz <-
    mxExpectationNormal(covariance = "expCovDZ",
                        means = "expMean",
                        dimnames = phenos)
  
  
  pars <- list(X, Y, Z, A, C, E, a2, c2, e2, cov_P, mat_I, inv_SD, mean_G)
  
  fun_ML <- mxFitFunctionML()
  
  model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
  model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
  
  fit_ML <- mxFitFunctionMultigroup(c("MZ.fitfunction","DZ.fitfunction"))
  Cf <-
    mxModel("ACE", pars, model_mz, model_dz, fit_ML, mxCI(c('a2', 'c2', 'e2')))
  
  mxRun(Cf, intervals = T) ### Run CholACE model
}

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var1 and t2_var1 are the column names for the
# first phenotype for the first and second twin and t1_var2 and t2_var2
# for the second phenotype
fit_ace_bivariate <-
  function (mz_data,
            dz_data,
            t1_var1,
            t2_var1,
            t1_var2,
            t2_var2) {
    require(OpenMx)
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 2 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t2_var1, t2_var2)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", "x21", "x22"),
        name = "X"
      )
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("y11", "y21", "y22"),
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", "z21", "z22"),
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = 1,
        labels = c("m1", "m2", "m1", "m2"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
      )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
        ),
        name = "expCovDZ"
      )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    rA12 <- mxAlgebra(expression = rA[1, 2], name = "rA12")
    rC12 <- mxAlgebra(expression = rC[1, 2], name = "rC12")
    rE12 <- mxAlgebra(expression = rE[1, 2], name = "rE12")
    
    pars <-
      list(X,
           Y,
           Z,
           A,
           C,
           E,
           a1_2,
           c1_2,
           e1_2,
           a2_2,
           c2_2,
           e2_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,
           corC,
           corE,
           rA12,
           rC12,
           rE12)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel("ACE",
              pars,
              model_mz,
              model_dz,
              fit_ML,
              est_VC,
              mxCI(
                c(
                  "a1_2",
                  "c1_2",
                  "e1_2",
                  "a2_2",
                  "c2_2",
                  "e2_2",
                  "rA12",
                  "rC12",
                  "rE12"
                )
              ))
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }


# same as above but for AE instead of ACE model
fit_ae_bivariate <-
  function (mz_data,
            dz_data,
            t1_var1,
            t2_var1,
            t1_var2,
            t2_var2) {
    require(OpenMx)
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 2 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t2_var1, t2_var2)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", "x21", "x22"),
        name = "X"
      )
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = FALSE,
        values = 0,
        labels = c("y11", "y21", "y22"),
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", "z21", "z22"),
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = 1,
        labels = c("m1", "m2", "m1", "m2"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
      )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
        ),
        name = "expCovDZ"
      )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    rA12 <- mxAlgebra(expression = rA[1, 2], name = "rA12")
    rC12 <- mxAlgebra(expression = rC[1, 2], name = "rC12")
    rE12 <- mxAlgebra(expression = rE[1, 2], name = "rE12")
    
    pars <-
      list(X,
           Y,
           Z,
           A,
           C,
           E,
           a1_2,
           c1_2,
           e1_2,
           a2_2,
           c2_2,
           e2_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,
           corC,
           corE,
           rA12,
           rC12,
           rE12)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel("ACE",
              pars,
              model_mz,
              model_dz,
              fit_ML,
              est_VC,
              mxCI(
                c(
                  "a1_2",
                  "c1_2",
                  "e1_2",
                  "a2_2",
                  "c2_2",
                  "e2_2",
                  "rA12",
                  "rC12",
                  "rE12"
                )
              ))
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }

# mz_data and dz_data are dataframes containing the phenotype, with one
# row per twin pair. t1_var1 and t2_var1 are the column names for the
# first phenotype for the first and second twin, t1_var2 and t2_var2
# for the second phenotype, and t1_var3 and t2_var3 for the third
fit_ace_generic <-
  function (mzData,
            dzData,
            selVars) {
    require(OpenMx)
     end<-length(selVars)/2
     vars<-gsub("T1","",selVars[1:end])
     ntv<-length(selVars)
     nv<-ntv/2
     # Select Data for Analysis
     
     
     # Set Starting Values
     svMe <- rep(0, times = nv) # start value for means
     svPa <- .3 # start value for path coefficient
     svPaD <- vech(diag(svPa,nv,nv)) # start values for diagonal of covariance matrix
     svPe <- .3 # start value for path coefficient for e
     svPeD <- vech(diag(svPe,nv,nv)) # start values for diagonal of covariance matrix
     # ----------------------------------------------------------------------------------------------------------------------
     # PREPARE MODEL
     # Create Algebra for expected Mean Matrices
     meanG <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=TRUE, values=svMe, labels=paste("mean",vars), name="meanG" )
     # Create Matrices for Path Coefficients
     pathA <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, label=labLower("a",nv), name="a" )
     pathC <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, label=labLower("c",nv), name="c" )
     pathE <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPeD, label=labLower("e",nv), name="e" )
     # Create Algebra for Variance Components
     covA <- mxAlgebra( expression=a %*% t(a), name="A" )
     covC <- mxAlgebra( expression=c %*% t(c), name="C" )
     covE <- mxAlgebra( expression=e %*% t(e), name="E" )
     # Create Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
     covP <- mxAlgebra( expression= A+C+E, name="V" )
     covMZ <- mxAlgebra( expression= A+C, name="cMZ" )
     covDZ <- mxAlgebra( expression= 0.5%x%A+ C, name="cDZ" )
     expCovMZ <- mxAlgebra( expression= rbind( cbind(V, cMZ), cbind(t(cMZ), V)), name="expCovMZ" )
     expCovDZ <- mxAlgebra( expression= rbind( cbind(V, cDZ), cbind(t(cDZ), V)), name="expCovDZ" )
     
     # Create Algebra for Standardization
     matI <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
     invSD <- mxAlgebra( expression=solve(sqrt(I*V)), name="iSD")
     # Calculate genetic and environmental correlations
     corA <- mxAlgebra( expression=solve(sqrt(I*A))%&%A, name ="rA" ) #cov2cor()
     corC <- mxAlgebra( expression=solve(sqrt(I*C))%&%C, name ="rC" )
     corE <- mxAlgebra( expression=solve(sqrt(I*E))%&%E, name ="rE" )
     
     
     
     # Create Data Objects for Multiple Groups
     dataMZ <- mxData( observed=mzData, type="raw" )
     dataDZ <- mxData( observed=dzData, type="raw" )
     # Create Expectation Objects for Multiple Groups
     expMZ <- mxExpectationNormal( covariance="expCovMZ", means="meanG", dimnames=selVars )
     expDZ <- mxExpectationNormal( covariance="expCovDZ", means="meanG", dimnames=selVars )
     funML <- mxFitFunctionML()
     # Create Model Objects for Multiple Groups
     pars <- list( meanG, matI, invSD, pathA, pathC, pathE, covA, covC, covE, covP, corA, corC, corE)
     modelMZ <- mxModel( pars, covMZ, expCovMZ, dataMZ, expMZ, funML, name="MZ" )
     modelDZ <- mxModel( pars, covDZ, expCovDZ, dataDZ, expDZ, funML, name="DZ" )
     multi <- mxFitFunctionMultigroup( c("MZ","DZ") )
     
     
     # Create Algebra for Variance Components
     colVC <- vars
     rowVC <- rep(c('A','C','E','SA','SC','SE'),each=nv)
     estVC <- mxAlgebra( expression=rbind(A,C,E,A/V,C/V,E/V), name="VC", dimnames=list(rowVC,colVC))
     
     # Build Model with Confidence Intervals
     modelACE <- mxModel( "mulACEc", pars, modelMZ, modelDZ, multi, estVC)
     # ----------------------------------------------------------------------------------------------------------------------
     # RUN MODEL
     # Run ACE Model
     fitACE <- mxRun( modelACE, intervals=T )
     return(fitACE)
  }

#Given a fit generic model, extract the variance component and correlation estimates




# Given a fit trivariate model, extract the variance component
# and correlation estimates
extract_trivariate_comps <- function (model) {
  require(tibble)
  df <- tibble(
    lbound = model$output$confidenceIntervals[, 1],
    estimate = model$output$confidenceIntervals[, 2],
    ubound = model$output$confidenceIntervals[, 3],
    component = c(rep(c("a2", "c2", "e2"), 3), rep(c("rA", "rC", "rE"), 3)),
    pheno = c(
      rep("Intercept", 3),
      rep("Slope", 3),
      rep("Quadratic", 3),
      rep("Intercept to Slope", 3),
      rep("Intercept to Quadratic", 3),
      rep("Slope to Quadratic", 3)
    )
  )
  df$lbound[is.na(df$lbound)] <- -1.0
  df$ubound[is.na(df$ubound)] <- 1.0
  df$pheno <- factor(df$pheno, levels = unique(df$pheno))
  
  df
}

# Given a fit bivariate model, extract the variance component
# and correlation estimates
extract_bivariate_comps <- function (model) {
  require(tibble)
  df <- tibble(
    lbound = model$output$confidenceIntervals[, 1],
    estimate = model$output$confidenceIntervals[, 2],
    ubound = model$output$confidenceIntervals[, 3],
    component = c(rep(c("a2", "c2", "e2"), 2), rep(c("rA", "rC", "rE"), 1)),
    pheno = c(
      rep("Intercept", 3),
      rep("Slope", 3),
      rep("Intercept to Slope", 3)
    )
  )
  
  df$lbound[is.na(df$lbound)] <- -1.0
  df$ubound[is.na(df$ubound)] <- 1.0
  df$pheno <- factor(df$pheno, levels = unique(df$pheno))
  
  df
}




fit_ace_trivariate <-
  function (mz_data,
            dz_data,
            t1_var1,
            t2_var1,
            t1_var2,
            t2_var2,
            t1_var3,
            t2_var3) {
    require(OpenMx)
    
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 3 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t1_var3, t2_var1, t2_var2, t2_var3)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", "x21", "x31", "x22", "x32", "x33"),
        name = "X"
      )
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("y11", "y21", "y31", "y22", "y32", "y33"),
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", "z21", "z31", "z22", "z32", "z33"),
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    a3_2 <- mxAlgebra(expression = A[3, 3] / V[3, 3], name = "a3_2")
    c3_2 <- mxAlgebra(expression = C[3, 3] / V[3, 3], name = "c3_2")
    e3_2 <- mxAlgebra(expression = E[3, 3] / V[3, 3], name = "e3_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = 1,
        labels = c("m1", "m2", "m3", "m1", "m2", "m3"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
      )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
        ),
        name = "expCovDZ"
      )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    rA12 <- mxAlgebra(expression = rA[1, 2], name = "rA12")
    rC12 <- mxAlgebra(expression = rC[1, 2], name = "rC12")
    rE12 <- mxAlgebra(expression = rE[1, 2], name = "rE12")
    
    rA13 <- mxAlgebra(expression = rA[1, 3], name = "rA13")
    rC13 <- mxAlgebra(expression = rC[1, 3], name = "rC13")
    rE13 <- mxAlgebra(expression = rE[1, 3], name = "rE13")
    
    rA23 <- mxAlgebra(expression = rA[2, 3], name = "rA23")
    rC23 <- mxAlgebra(expression = rC[2, 3], name = "rC23")
    rE23 <- mxAlgebra(expression = rE[2, 3], name = "rE23")
    
    pars <-
      list(X,
           Y,
           Z,
           A,
           C,
           E,
           a1_2,
           c1_2,
           e1_2,
           a2_2,
           c2_2,
           e2_2,
           a3_2,
           c3_2,
           e3_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,
           corC,
           corE,
           rA12,
           rC12,
           rE12,
           rA13,
           rC13,
           rE13,
           rA23,
           rC23,
           rE23)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2", "var3")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel("ACE",
              pars,
              model_mz,
              model_dz,
              fit_ML,
              est_VC,
              mxCI(
                c(
                  "a1_2",
                  "c1_2",
                  "e1_2",
                  "a2_2",
                  "c2_2",
                  "e2_2",
                  "a3_2",
                  "c3_2",
                  "e3_2",
                  "rA12",
                  "rC12",
                  "rE12",
                  "rA13",
                  "rC13",
                  "rE13",
                  "rA23",
                  "rC23",
                  "rE23"
                )
              ))
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }


fit_ace_6 <-
  function (mz_data,
            dz_data,
            t1_var1,t2_var1,
            t1_var2,t2_var2,
            t1_var3,t2_var3,
            t1_var4,t2_var4,
            t1_var5,t2_var5,
            t1_var6,t2_var6) {
    require(OpenMx)
    
    mx_mz_data <- mxData(observed = data.frame(mz_data), type="raw")
    mx_dz_data <- mxData(observed = data.frame(dz_data), type="raw")
    
    nv <- 6 # Number of observed variables
    phenos <- c(t1_var1, t1_var2, t1_var3, t1_var4, t1_var5, t1_var6,
                t2_var1, t2_var2, t2_var3, t2_var4, t2_var5, t2_var6)
    
    X <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("x11", 
                   "x21", "x22",
                   "x31", "x32", "x33",
                   "x41", "x42", "x43","x44",
                   "x51", "x52", "x53","x54","x55",
                   "x61", "x62", "x63","x64","x65","x6"),byrow = T,
        name = "X")
    Y <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("y11", 
                   "y21", "y22",
                   "y31", "y32", "y33",
                   "y41", "y42", "y43","y44",
                   "y51", "y52", "y53","y54","y55",
                   "y61", "y62", "y63","y64","y65","y6"),byrow = T,
        name = "Y"
      )
    Z <-
      mxMatrix(
        type = "Lower",
        nrow = nv,
        ncol = nv,
        free = TRUE,
        values = .6,
        labels = c("z11", 
                   "z21", "z22",
                   "z31", "z32", "z33",
                   "z41", "z42", "z43","z44",
                   "z51", "z52", "z53","z54","z55",
                   "z61", "z62", "z63","z64","z65","z6"), byrow = T,
        name = "Z"
      )
    
    A <- mxAlgebra(X %*% t(X), name = "A")
    C <- mxAlgebra(Y %*% t(Y), name = "C")
    E <- mxAlgebra(Z %*% t(Z), name = "E")
    
    cov_P <- mxAlgebra(expression = A + C + E, name = "V")
    
    a1_2 <- mxAlgebra(expression = A[1, 1] / V[1, 1], name = "a1_2")
    c1_2 <- mxAlgebra(expression = C[1, 1] / V[1, 1], name = "c1_2")
    e1_2 <- mxAlgebra(expression = E[1, 1] / V[1, 1], name = "e1_2")
    
    a2_2 <- mxAlgebra(expression = A[2, 2] / V[2, 2], name = "a2_2")
    c2_2 <- mxAlgebra(expression = C[2, 2] / V[2, 2], name = "c2_2")
    e2_2 <- mxAlgebra(expression = E[2, 2] / V[2, 2], name = "e2_2")
    
    a3_2 <- mxAlgebra(expression = A[3, 3] / V[3, 3], name = "a3_2")
    c3_2 <- mxAlgebra(expression = C[3, 3] / V[3, 3], name = "c3_2")
    e3_2 <- mxAlgebra(expression = E[3, 3] / V[3, 3], name = "e3_2")
    
    a4_2 <- mxAlgebra(expression = A[4, 4] / V[4, 4], name = "a4_2")
    c4_2 <- mxAlgebra(expression = C[4, 4] / V[4, 4], name = "c4_2")
    e4_2 <- mxAlgebra(expression = E[4, 4] / V[4, 4], name = "e4_2")
    
    a5_2 <- mxAlgebra(expression = A[5, 5] / V[5, 5], name = "a5_2")
    c5_2 <- mxAlgebra(expression = C[5, 5] / V[5, 5], name = "c5_2")
    e5_2 <- mxAlgebra(expression = E[5, 5] / V[5, 5], name = "e5_2")
    
    a6_2 <- mxAlgebra(expression = A[6, 6] / V[6, 6], name = "a6_2")
    c6_2 <- mxAlgebra(expression = C[6, 6] / V[6, 6], name = "c6_2")
    e6_2 <- mxAlgebra(expression = E[6, 6] / V[6, 6], name = "e6_2")
    
    mat_I <- mxMatrix(
      type = "Iden",
      nrow = nv,
      ncol = nv,
      name = "I"
    )
    inv_SD <- mxAlgebra(expression = solve(sqrt(I * V)), name = "iSD")
    
    mean_G <-
      mxMatrix(
        type = "Full",
        nrow = 1,
        ncol = nv * 2,
        free = TRUE,
        values = 1,
        labels = c("m1", "m2", "m3","m4","m5","m6", "m1", "m2", "m3","m4","m5","m6"),
        name = "expMean"
      )
    
    cov_mz <-
      mxAlgebra(
        expression = rbind(cbind(A + C + E, A + C), cbind(A + C,  A + C + E)),
        name = "expCovMZ"
      )
    cov_dz <-
      mxAlgebra(
        expression = rbind(
          cbind(A + C + E, 0.5 %x% A + C),
          cbind(0.5 %x% A + C, A + C + E)
        ),
        name = "expCovDZ"
      )
    
    obj_mz <-
      mxExpectationNormal(covariance = "expCovMZ",
                          means = "expMean",
                          dimnames = phenos)
    obj_dz <-
      mxExpectationNormal(covariance = "expCovDZ",
                          means = "expMean",
                          dimnames = phenos)
    
    # Calculate genetic and environmental correlations
    corA <-
      mxAlgebra(expression = solve(sqrt(I * A)) %&% A, name = "rA")
    corC <-
      mxAlgebra(expression = solve(sqrt(I * C)) %&% C, name = "rC")
    corE <-
      mxAlgebra(expression = solve(sqrt(I * E)) %&% E, name = "rE")
    
    rA12 <- mxAlgebra(expression = rA[1, 2], name = "rA12")
    rC12 <- mxAlgebra(expression = rC[1, 2], name = "rC12")
    rE12 <- mxAlgebra(expression = rE[1, 2], name = "rE12")
    
    rA13 <- mxAlgebra(expression = rA[1, 3], name = "rA13")
    rC13 <- mxAlgebra(expression = rC[1, 3], name = "rC13")
    rE13 <- mxAlgebra(expression = rE[1, 3], name = "rE13")
    
    rA14 <- mxAlgebra(expression = rA[1, 4], name = "rA14")
    rC14 <- mxAlgebra(expression = rC[1, 4], name = "rC14")
    rE14 <- mxAlgebra(expression = rE[1, 4], name = "rE14")
    
    rA15 <- mxAlgebra(expression = rA[1, 5], name = "rA15")
    rC15 <- mxAlgebra(expression = rC[1, 5], name = "rC15")
    rE15 <- mxAlgebra(expression = rE[1, 5], name = "rE15")
    
    rA16 <- mxAlgebra(expression = rA[1, 6], name = "rA16")
    rC16 <- mxAlgebra(expression = rC[1, 6], name = "rC16")
    rE16 <- mxAlgebra(expression = rE[1, 6], name = "rE16")
    
    rA23 <- mxAlgebra(expression = rA[2, 3], name = "rA23")
    rC23 <- mxAlgebra(expression = rC[2, 3], name = "rC23")
    rE23 <- mxAlgebra(expression = rE[2, 3], name = "rE23")
    
    rA24 <- mxAlgebra(expression = rA[2, 4], name = "rA24")
    rC24 <- mxAlgebra(expression = rC[2, 4], name = "rC24")
    rE24 <- mxAlgebra(expression = rE[2, 4], name = "rE24")
    
    rA25 <- mxAlgebra(expression = rA[2, 5], name = "rA25")
    rC25 <- mxAlgebra(expression = rC[2, 5], name = "rC25")
    rE25 <- mxAlgebra(expression = rE[2, 5], name = "rE25")
    
    rA26 <- mxAlgebra(expression = rA[2, 6], name = "rA26")
    rC26 <- mxAlgebra(expression = rC[2, 6], name = "rC26")
    rE26 <- mxAlgebra(expression = rE[2, 6], name = "rE26")
    
    rA34 <- mxAlgebra(expression = rA[3, 4], name = "rA34")
    rC34 <- mxAlgebra(expression = rC[3, 4], name = "rC34")
    rE34 <- mxAlgebra(expression = rE[3, 4], name = "rE34")
    
    rA35 <- mxAlgebra(expression = rA[3, 5], name = "rA35")
    rC35 <- mxAlgebra(expression = rC[3, 5], name = "rC35")
    rE35 <- mxAlgebra(expression = rE[3, 5], name = "rE35")
    
    rA36 <- mxAlgebra(expression = rA[3, 6], name = "rA36")
    rC36 <- mxAlgebra(expression = rC[3, 6], name = "rC36")
    rE36 <- mxAlgebra(expression = rE[3, 6], name = "rE36")
    
    rA45 <- mxAlgebra(expression = rA[4, 5], name = "rA45")
    rC45 <- mxAlgebra(expression = rC[4, 5], name = "rC45")
    rE45 <- mxAlgebra(expression = rE[4, 5], name = "rE45")
    
    rA46 <- mxAlgebra(expression = rA[4, 6], name = "rA46")
    rC46 <- mxAlgebra(expression = rC[4, 6], name = "rC46")
    rE46 <- mxAlgebra(expression = rE[4, 6], name = "rE46")
    
    rA56 <- mxAlgebra(expression = rA[5, 6], name = "rA56")
    rC56 <- mxAlgebra(expression = rC[5, 6], name = "rC56")
    rE56 <- mxAlgebra(expression = rE[5, 6], name = "rE56")
    
    pars <-
      list(X,Y,Z,
           A,C,E,
           a1_2,c1_2,e1_2,
           a2_2,c2_2,e2_2,
           a3_2,c3_2,e3_2,
           a4_2,c4_2,e4_2,
           a5_2,c5_2,e5_2,
           a6_2,c6_2,e6_2,
           cov_P,
           mat_I,
           inv_SD,
           mean_G,
           corA,corC,corE,
           rA12, rC12, rE12,
           rA13, rC13, rE13,
           rA14, rC14, rE14,
           rA15, rC15, rE15,
           rA16, rC16, rE16,
           rA23, rC23, rE23,
           rA24, rC24, rE24,
           rA25, rC25, rE25,
           rA26, rC26, rE26,
           rA34, rC34, rE34,
           rA35, rC35, rE35,
           rA36, rC36, rE36,
           rA45, rC45, rE45,
           rA46, rC46, rE46,
           rA56, rC56, rE56)
    
    fun_ML <- mxFitFunctionML()
    
    model_mz <- mxModel(pars, cov_mz, mx_mz_data, obj_mz, fun_ML, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_dz_data, obj_dz, fun_ML, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("var1", "var2", "var3","var4","var5","var6")
    row_VC <- rep(c('A', 'C', 'E', 'SA', 'SC', 'SE'), each = nv)
    est_VC <-
      mxAlgebra(
        expression = rbind(A, C, E, A / V, C / V, E / V),
        name = "VC",
        dimnames = list(row_VC, col_VC)
      )
    
    fit_ML <-
      mxFitFunctionMultigroup(c("MZ.fitfunction", "DZ.fitfunction"))
    Cf <-
      mxModel("ACE",
              pars,
              model_mz,
              model_dz,
              fit_ML,
              est_VC,
              mxCI(
                c("a1_2","c1_2","e1_2",
                  "a2_2","c2_2","e2_2",
                  "a3_2","c3_2","e3_2",
                  "a4_2","c4_2","e4_2",
                  "a5_2","c5_2","e5_2",
                  "a6_2","c6_2","e6_2",
                  "rA12","rC12","rE12",
                  "rA13","rC13","rE13",
                  "rA14","rC14","rE14",
                  "rA15","rC15","rE15",
                  "rA16","rC16","rE16",
                  "rA23","rC23","rE23",
                  "rA24","rC24","rE24",
                  "rA25","rC25","rE25",
                  "rA26","rC26","rE26",
                  "rA34","rC34","rE34",
                  "rA35","rC35","rE35",
                  "rA36","rC36","rE36",
                  "rA45","rC45","rE45",
                  "rA46","rC46","rE46",
                  "rA56","rC56","rE56")
                
              ))
    # Run the CholACE model
    mxRun(Cf, intervals = T)
  }


#' correlation_matrix (taken from https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/)
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}


