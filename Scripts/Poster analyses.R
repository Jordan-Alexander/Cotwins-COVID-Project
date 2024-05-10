## analyses for BGA 2023 abstract

# set directory and load packages

setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")
library(data.table)
library(lubridate)
library(dplyr)
library(reshape2)
library(readxl)
source('Scripts/Jordan.functions.R')
# load data
data = fread('Datasets/mobility.affect.covid.2.21.csv',data.table = F)
ids = read_xlsx('Datasets/id_mapping_comp.xlsx')

# generate twin data function

get.wideform.twin.data = function(data, var){
     
     vars = c('SVID','family','bestzygos','year',var)
     df = data[,vars] %>% na.omit()
     names(df)[5] = 'outcome'
     df_wide = reshape2::dcast(df, SVID + family + bestzygos ~ year, value.var = 'outcome',
                               fun.aggregate = mean)
     
     df_wide_t1 = df_wide[!duplicated(df_wide$family),]
     df_wide_t2 = df_wide[duplicated(df_wide$family),]
     names(df_wide_t1)[4:ncol(df_wide_t1)] = paste0('t1_',
                                                    names(df_wide_t1)[4:ncol(df_wide_t1)])
     
     names(df_wide_t1)[1] = 'SVID_t1'
     
     names(df_wide_t2)[4:ncol(df_wide_t2)] = paste0('t2_',
                                                    names(df_wide_t2)[4:ncol(df_wide_t2)])
     
     names(df_wide_t2)[1] = 'SVID_t2'
     
     
     df_wide_twin = left_join(df_wide_t1,df_wide_t2)
     
     MZdata = df_wide_twin[df_wide_twin$bestzygos == 'MZ',]
     DZdata = df_wide_twin[df_wide_twin$bestzygos != 'MZ',]
     
     out = list(MZdata,DZdata)
     return(out)
     }


# compute yearly averages for ACE models

data$year = year(data$date)

dat.yr = data %>% group_by(SVID,year) %>% 
     summarise(pa = mean(pa.av,na.rm = T),
               na = mean(na.av,na.rm = T),
               pts = mean(numpts,na.rm = T),
               dist = mean(distance, na.rm = T)) 

dat.yr = dat.yr[dat.yr$SVID != "",]
dat.yr = left_join(dat.yr, ids[,c('SVID','family','bestzygos'),])

# convert to wide twinform

pa_mz = get.wideform.twin.data(dat.yr, 'pa')[[1]]
pa_dz = get.wideform.twin.data(dat.yr, 'pa')[[2]]

na_mz = get.wideform.twin.data(dat.yr, 'na')[[1]]
na_dz = get.wideform.twin.data(dat.yr, 'na')[[2]]

pts_mz = get.wideform.twin.data(dat.yr, 'pts')[[1]]
pts_dz = get.wideform.twin.data(dat.yr, 'pts')[[2]]

dist_mz = get.wideform.twin.data(dat.yr, 'dist')[[1]]
dist_dz = get.wideform.twin.data(dat.yr, 'dist')[[2]]



## Run ACE models by year

mod.pa = fit_ace_trivariate(pa_mz,
                            pa_dz,
                            't1_2019','t2_2019',
                            't1_2020','t2_2020',
                            't1_2021','t2_2021')

mod.na = fit_ace_trivariate(na_mz,
                            na_dz,
                            't1_2019','t2_2019',
                            't1_2020','t2_2020',
                            't1_2021','t2_2021')


mod.pts = fit_ace_trivariate(pts_mz,
                             pts_dz,
                            't1_2019','t2_2019',
                            't1_2020','t2_2020',
                            't1_2021','t2_2021')

mod.dist = fit_ace_trivariate(dist_mz,
                              dist_dz,
                              't1_2019','t2_2019',
                              't1_2020','t2_2020',
                              't1_2021','t2_2021')





# test significance of pts model

    mx_pts_mz <- mxData(observed = data.frame(pts_mz), type="raw")
    mx_pts_dz <- mxData(observed = data.frame(pts_dz), type="raw")
    
    nv <- 3 # Number of observed variables
    phenos <- c('t1_2019', 't1_2020', 't1_2021', 't2_2019', 't2_2020', 't2_2021')
    
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
    
    con1 = mxConstraint(a1_2 == a2_2, name = "A_2019 eq A_2020")
    con2 = mxConstraint(a2_2 == a3_2, name = "A_2020 eq A_2021")
    
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
    
    model_mz <- mxModel(pars, cov_mz, mx_pts_mz, obj_mz, fun_ML, con1,con2, name = "MZ")
    model_dz <- mxModel(pars, cov_dz, mx_pts_dz, obj_dz, fun_ML, con1,con2, name = "DZ")
    
    # Create algebra for variance components
    col_VC <- c("2019", "2020", "2021")
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
    mod2 = mxRun(Cf, intervals = T)

mxCompare(mod.pts,mod2)
