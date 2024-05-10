setwd("C:\\Users\\alexa877\\Documents\\UMN Courses\\Research\\Covid Location project\\RAship 2022\\Models")
library(dplyr)
library(gamm4)
library(mgcViz)
library(ggplot2)
library(plot3D)
library(htmlwidgets)

# plot 2d gamm interaction PA
pa = readRDS('pa.av.gamm4.10knot.tensor.2.14.23.rds')

pa_interaction <- getViz(pa$gam)

#plot method from mgcViz object helps in plotting 2D
pa.plot.smooth = plot(sm(pa_interaction,1)) + 
     l_fitRaster() + l_fitContour()

ggsave('pa.tensor.plot.png',pa.plot.smooth$ggObj, device = 'png',width = 7, height = 7, units = 'in')


# plot 2d gamm interaction NA
na = readRDS('na.av.gamm4.10knot.tensor.2.14.23.rds')

na_interaction <- getViz(na$gam)

na.plot.smooth = plot(sm(na_interaction,1)) + 
     l_fitRaster() + l_fitContour()
ggsave('na.tensor.plot.png',na.plot.smooth$ggObj, device = 'png',width = 7, height = 7, units = 'in')


# plot 2d gamm interaction Pts
numpts = readRDS('numpts.gamm4.10knot.tensor.2.14.23.rds')

numpts_interaction <- getViz(numpts$gam)

numpts.plot.smooth = plot(sm(numpts_interaction,1)) + 
     l_fitRaster() + l_fitContour()
ggsave('numpts.tensor.plot.png',numpts.plot.smooth$ggObj, device = 'png',width = 7, height = 7, units = 'in')



# plot 2d gamm interaction Pts
distance = readRDS('distance.gamm4.10knot.tensor.2.14.23.rds')

distance_interaction <- getViz(distance$gam)

distance.plot.smooth = plot(sm(distance_interaction,1)) + 
     l_fitRaster() + l_fitContour()
ggsave('distance.tensor.plot.png',distance.plot.smooth$ggObj, device = 'png',width = 7, height = 7, units = 'in')
