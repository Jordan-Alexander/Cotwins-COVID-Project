# create effects plot for GAMM4 data
library(gratia)
library(ggeffects)
library(lubridate)
library(gamm4)
library(ggplot2)
library(data.table)
library(gridExtra)
library(sjPlot)

setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022")

#dat = fread('Datasets/mobility.affect.covid.2.21.csv')

# input model, name of smooth term, number of points to evaluate derivative of smooth function
# output plot of smooth and its first derivative
get.smooths.plot = function(model, 
                            smooth,
                            pheno,
                            color,
                            deriv_pts = 500,
                            date = TRUE){
          # input model, name of smooth term, number of points to evaluate derivative of smooth function
          # output plot of smooth and its first derivative
          
          model.pred = derivatives(model,
                                   term = smooth,
                                   n = deriv_pts)
     
     if(date == TRUE)
     {
     model.pred = model.pred[model.pred$data >= min(model$model$days.post.covid),]
     model.pred = model.pred[model.pred$data <= max(model$model$days.post.covid),]
     model.pred$date = as_date(model.pred$data, 
                             origin = ymd('2020-01-26'))
     
     
     model.main = ggpredict(model, terms = 'days.post.covid')
     
     model.main = model.main[model.main$x >= min(model$model$days.post.covid),]
     model.main = model.main[model.main$x <= max(model$model$days.post.covid),]
     model.main$date = as_date(model.main$x, 
                             origin = ymd('2020-01-26'))
     
     
     plot.deriv = 
          ggplot(model.pred, aes(x = date,y = derivative)) +
          geom_line(color = color)+
          geom_ribbon(aes(ymin = lower, ymax = upper),
                      fill = color, 
                      alpha = .2)+
          ylab('f\'(x)')+
          xlab('date')+
          scale_x_date(date_breaks = "3 months",
                       date_minor_breaks = "1 months")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     plot.model = 
          ggplot(model.main, aes(x = date,y = predicted)) +
          geom_line(color = color) +
          geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                      fill = color, 
                      alpha = .2) +
          ylab(pheno)+
          xlab('date')+
          scale_x_date(date_breaks = "3 months",
                       date_minor_breaks = "1 months")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     plot = grid.arrange(plot.model,plot.deriv,ncol = 1)
     }
          
     if(date == F)
     {
              model.main = ggpredict(model, terms = 'cases.weekly.per.100k')

          
          plot.deriv =
               ggplot(model.pred, aes(x = data, y = derivative)) +
               geom_line(color = color)+
               geom_ribbon(aes(ymin = lower, ymax = upper),
                           fill = color, 
                           alpha = .2)+
               ylab('f\'(x)')+
               xlab('Weekly cases per 100,000')+ 
               xlim(0,1000)+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
          
         
          
          plot.model = 
               ggplot(model.main, aes(x = x,y = predicted)) +
               geom_line(color = color) +
               geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                           fill = color, 
                           alpha = .2) +
               ylab(pheno)+
               xlab('Weekly cases per 100,000')+
               xlim(0,1000)+
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     plot = grid.arrange(plot.model,plot.deriv,ncol = 1)
          
          
          
     }
     
     return(plot)
}


# days plots --------------------------------------------------------------


# affect days plots -------------------------------------------------------


pa.mod = readRDS('Models/pa.avgamm4.days.3.21.23.rds')
pa.days.plot = get.smooths.plot(pa.mod$gam,
                                smooth = 's(days.post.covid)',
                                pheno = 'Positive Affect',
                                color = 'blue',
                                deriv_pts = 500)
rm(pa.mod)
ggsave('pa.days.plot.3.30.png',
       pa.days.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')

na.mod = readRDS('Models/na.avgamm4.days.3.21.23.rds')
na.days.plot = get.smooths.plot(na.mod$gam,
                                smooth = 's(days.post.covid)',
                                pheno = 'Negative Affect',
                                color = 'red',
                                deriv_pts = 500)


rm(na.mod)

ggsave('na.days.plot.3.30.png',
       na.days.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')


# mobility days plots -----------------------------------------------------


pts.mod = readRDS('Models/numptsgamm4.days.3.21.23.rds')
pts.days.plot = get.smooths.plot(pts.mod$gam,
                                smooth = 's(days.post.covid)',
                                pheno = 'Daily Locations Visited',
                                color = 'purple',
                                deriv_pts = 500)
rm(pts.mod)

dist.mod = readRDS('Models/distancegamm4.days.3.21.23.rds')

dist.days.plot = get.smooths.plot(dist.mod$gam,
                                smooth = 's(days.post.covid)',
                                pheno = 'Daily Distance Travelled',
                                color = 'green',
                                deriv_pts = 500)

rm(dist.mod)




# cases plots -------------------------------------------------------------


# affect cases plots ------------------------------------------------------


pa.mod = readRDS('Models/pa.avgamm4.cases.3.21.23.rds')
pa.cases.plot = get.smooths.plot(pa.mod$gam,
                                smooth = 's(cases.weekly.per.100k)',
                                pheno = 'Positive Affect',
                                color = 'blue',
                                deriv_pts = 500,
                                date = F)

ggsave('pa.cases.plot.3.30.png',
       pa.cases.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')

rm(pa.mod)

na.mod = readRDS('Models/na.avgamm4.cases.3.21.23.rds')
na.cases.plot = get.smooths.plot(na.mod$gam,
                                smooth = 's(cases.weekly.per.100k)',
                                pheno = 'Negative Affect',
                                color = 'blue',
                                deriv_pts = 500,
                                date = F)

ggsave('na.cases.plot.3.30.png',
       na.cases.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')

rm(na.mod)


# mobility cases plots ----------------------------------------------------



pts.mod = readRDS('Models/numptsgamm4.cases.3.21.23.rds')
pts.cases.plot = get.smooths.plot(pts.mod$gam,
                                smooth = 's(cases.weekly.per.100k)',
                                pheno = 'Daily Locations Visited',
                                color = 'purple',
                                deriv_pts = 500,
                                date = F)

ggsave('pts.cases.plot.3.30.png',
       pts.cases.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')

rm(pts.mod)


dist.mod = readRDS('Models/distancegamm4.cases.3.21.23.rds')
dist.cases.plot = get.smooths.plot(dist.mod$gam,
                                smooth = 's(cases.weekly.per.100k)',
                                pheno = 'Daily Distance Travelled',
                                color = 'green',
                                deriv_pts = 500,
                                date = F)

ggsave('dist.cases.plot.3.30.png',
       dist.cases.plot,
       device = 'png',
       height = 8, 
       width = 8, 
       units = 'in')

rm(dist.mod)