


# generate some data for testing the effect of weighting random effects


# simple model with random intercept and 2 groups of data 

testdata1<- data.frame(
  Year = rep(1:20, 2),
    group = rep(c("a", "b"), each = 20),
  Number = c(rpois(20, 10), rpois(20, 20))
)

library(lme4)
summary(glm(Number~group, data = testdata1, family = "poisson"))
  #group a: 2.22
 # group b: 2.93
summary(glmer(Number~1 + (1|group), data = testdata1, family = "poisson"))
# intercept = 2.585

library(INLA)
summary(inla(Number~group, data = testdata1, family = "poisson")) # works 
# Fixed effects:
#             mean    sd    0.025quant 0.5quant 0.975quant mode kld
# (Intercept) 2.225 0.074      2.077    2.226      2.366   NA   0
# groupb      0.717 0.090      0.543    0.717      0.895   NA   0
# sum = 2.942



# complete RE model: 
summary(inla(Number~1+
             f(group, model='iid'),
            data = testdata1, family = "poisson")) # works 
# intercept = 2.587 # almost same as glmer model

weights<- rep(c(1, 2), each = 20 )

# make predictor matrix 
A <- Diagonal(n = nrow(testdata1), x = weights)

summary(inla(Number~1+
             f(group, model='iid'),
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 
# now intercept = 1.836 = lower! so the higher group is weighted lower



weights<- rep(c(2, 1), each = 20 )

# make predictor matrix 
A <- Diagonal(n = nrow(testdata1), x = weights)

summary(inla(Number~1+
               f(group, model='iid'),
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 
# now intercept = 2.025 = lower! so the lower group is weighted lower



summary(inla(Number~group,
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 



# now check the effect of the weighting on groups on another predictor 
# create trend in yearly counts. This creates underdispersion though, because the sd doesnt grow with the mean
testdata1$Number2<- testdata1$Number + testdata1$Year
  
#glms
summary(glm(Number2~group+Year, data = testdata1, family = "poisson"))
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.558209   0.082950  30.840  < 2e-16 ***
#   groupb      0.399540   0.065034   6.144 8.07e-10 ***
#   Year        0.038175   0.005609   6.806 1.01e-11 ***

# as re model
summary(glmer(Number2 ~ Year + (1|group), data = testdata1, family = "poisson"))
# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 2.759018   0.155978  17.689  < 2e-16 ***
#   Year      0.038175   0.005607   6.809 9.84e-12 ***



summary(inla(Number2~group + Year,
             data = testdata1, family = "poisson")) # works 

#             mean    sd 0.025quant 0.5quant 0.975quant mode kld
# (Intercept) 2.558 0.083      2.393    2.558      2.719   NA   0
# groupb      0.400 0.065      0.272    0.399      0.528   NA   0
# Year        0.038 0.006      0.027    0.038      0.049   NA   0


summary(inla(Number2~group + Year,
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 
#Fixed effects:
#               mean    sd 0.025quant 0.5quant 0.975quant mode kld
# (Intercept) 1.201 0.053      1.097    1.202      1.303   NA   0
# groupb      1.902 0.048      1.807    1.903      1.997   NA   0
# Year        0.026 0.004      0.018    0.026      0.033   NA   0
# everything is lower again 

summary(inla(Number2~ Year+
               f(group, model='iid'),
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 
# exactly the same slope estimate as when group is fixed



# We need different slopes #####
# : one with slope and one without
# create trend in yearly counts. This creates underdispersion though, because the sd doesnt grow with the mean
testdata1$Number2<- testdata1$Number
testdata1$Number2[testdata1$group == "b"]<- testdata1$Number[testdata1$group == "b"] + testdata1$Year[testdata1$group == "b"]*2
# add random slope for group
testdata1$groupSlope<- testdata1$group

library(ggplot2)
basicPlot<- ggplot(testdata1, aes(y = Number2, x = Year, color = group))+
  scale_y_continuous(trans='log', limits = c(1,200)) + 
  geom_point()+
  stat_smooth(method = 'lm') # this should work
basicPlot
summary(glm(Number2~group*Year, data = testdata1, family = "poisson"))
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     2.284710   0.149133  15.320  < 2e-16 ***
#   groupb        0.609413   0.177213   3.439 0.000584 ***
#   Year          -0.002193   0.012516  -0.175 0.860910    
# groupb:Year     0.047148   0.014454   3.262 0.001106 ** 



# simple model without weighting and 
summary(inla(Number2~group * Year,
             data = testdata1, family = "poisson")) # works 
#               mean    sd 0.025quant 0.5quant 0.975quant mode kld
# (Intercept)  2.286 0.149      1.986    2.289      2.571   NA   0
# groupb       0.608 0.177      0.265    0.607      0.960   NA   0
# Year        -0.002 0.013     -0.027   -0.002      0.022   NA   0
# groupb:Year  0.047 0.014      0.019    0.047      0.076   NA   0

# random intercept and slope model
noweightmodel<- (inla(Number2~ Year+
               f(group, model='iid')+
               f(groupSlope, Year, model='iid'),
             data = testdata1, family = "poisson")) # works 
summary(noweightmodel)
#              mean    sd 0.025quant 0.5quant 0.975quant mode   kld
# (Intercept) 2.616 0.240      2.059    2.640      3.073   NA 0.005
# Year        0.020 0.037     -0.065    0.022      0.095   NA 0.016
# slope is a bit smaller than the mean of the two slopes
noweightmodel$summary.fixed

# not sure if this makes any sense: 
summary(inla(Number2~group * Year,
             control.predictor = list(A = A),
             data = testdata1, family = "poisson")) # works 


# test out different configurations of the predictor matrix
weights<- rep(c(2, 1), each = 20 )
A <- Diagonal(n = nrow(testdata1), x = weights)

weights<- rep(c(1, 2), each = 20 )
B <- Diagonal(n = nrow(testdata1), x = weights)

weights<- rep(c(4, 1), each = 20 )
C <- Diagonal(n = nrow(testdata1), x = weights)

weights<- rep(c(1, 4), each = 20 )
D <- Diagonal(n = nrow(testdata1), x = weights)


# half weight to flat slope (1/2)
steepweightedmodel<-inla(Number2~ Year +
               f(group, model='iid')+
               f(groupSlope, Year, model='iid'),
             control.predictor = list(A = A),
             data = testdata1, family = "poisson") # works 
#              mean    sd 0.025quant 0.5quant 0.975quant mode   kld
# (Intercept) 2.020 0.788      0.377    2.019      3.668   NA 0.004
# Year        0.022 0.020     -0.020    0.022      0.064   NA 0.003

basicPlot + 
  geom_abline(slope = (noweightmodel$summary.fixed$mean[2]), intercept = (noweightmodel$summary.fixed$mean[1]), linewidth = 2 )


# half weight to steep slope (1/2)
flatweightedmodel<-(inla(Number2~ Year+
               f(group, model='iid')+
               f(groupSlope, Year, model='iid'),
             control.predictor = list(A = B),
             data = testdata1, family = "poisson")) # works 
#               mean    sd 0.025quant 0.5quant 0.975quant mode   kld
# (Intercept) 1.773 0.278      1.219    1.762      2.374   NA 0.003
# Year        0.018 0.010     -0.004    0.018      0.036   NA 0.001

basicPlot + 
  geom_abline(slope = (noweightmodel$summary.fixed$mean[2]), intercept = (noweightmodel$summary.fixed$mean[1]), linewidth = 2 ) +
  geom_abline(slope = (flatweightedmodel$summary.fixed$mean[2]), intercept = (flatweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "red" )+
  geom_abline(slope = (steepweightedmodel$summary.fixed$mean[2]), intercept = (steepweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "green" )


# third weight to flat slope (1/2)
steepmoreweightedmodel<-(inla(Number2~ Year+
                            f(group, model='iid')+
                            f(groupSlope, Year, model='iid'),
                          control.predictor = list(A = C),
                          data = testdata1, family = "poisson")) # works 

flatmoreweightedmodel<-(inla(Number2~ Year+
                           f(group, model='iid')+
                           f(groupSlope, Year, model='iid'),
                         control.predictor = list(A = D),
                         data = testdata1, family = "poisson")) # works 
summary(flatmoreweightedmodel)
summary(flatweightedmodel)
steepweightedmodel$summary.fixed
steepmoreweightedmodel$summary.fixed
basicPlot + 
  geom_abline(slope = (noweightmodel$summary.fixed$mean[2]), intercept = (noweightmodel$summary.fixed$mean[1]), linewidth = 2 ) +
  geom_abline(slope = (flatweightedmodel$summary.fixed$mean[2]), intercept = (flatweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "red" )+
  geom_abline(slope = (steepweightedmodel$summary.fixed$mean[2]), intercept = (steepweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "green" )+
  geom_abline(slope = (flatmoreweightedmodel$summary.fixed$mean[2]), intercept = (flatmoreweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "darkred" )+
  geom_abline(slope = (steepmoreweightedmodel$summary.fixed$mean[2]), intercept = (steepmoreweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "darkgreen" )
# so this kind of works. except that there's almost no difference between weighting 2 and 4 times...


# this gets very weird:
weights<- rep(c(0.5, 1), each = 20 )
E <- Diagonal(n = nrow(testdata1), x = weights)

weights<- rep(c(1, 0.5), each = 20 )
G <- Diagonal(n = nrow(testdata1), x = weights)

flat0.5weightedmodel<-(inla(Number2~ Year+
                                f(group, model='iid')+
                                f(groupSlope, Year, model='iid'),
                              control.predictor = list(A = E),
                              data = testdata1, family = "poisson")) # works 

steep0.5weightedmodel<-(inla(Number2~ Year+
                               f(group, model='iid')+
                               f(groupSlope, Year, model='iid'),
                             control.predictor = list(A = G),
                             data = testdata1, family = "poisson")) # works 
noweightmodel$summary.fixed
flatweightedmodel$summary.fixed
flat0.5weightedmodel$summary.fixed
steepweightedmodel$summary.fixed
steep0.5weightedmodel$summary.fixed

basicPlot + 
  geom_abline(slope = (noweightmodel$summary.fixed$mean[2]), intercept = (noweightmodel$summary.fixed$mean[1]), linewidth = 2 ) +
  geom_abline(slope = (flatweightedmodel$summary.fixed$mean[2]), intercept = (flatweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "red" )+
  geom_abline(slope = (steepweightedmodel$summary.fixed$mean[2]), intercept = (steepweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "green" )+
  geom_abline(slope = (flatmoreweightedmodel$summary.fixed$mean[2]), intercept = (flatmoreweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "darkred" )+
  geom_abline(slope = (steepmoreweightedmodel$summary.fixed$mean[2]), intercept = (steepmoreweightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "darkgreen" )+
  geom_abline(slope = (flat0.5weightedmodel$summary.fixed$mean[2]), intercept = (flat0.5weightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "blue" )+
  geom_abline(slope = (steep0.5weightedmodel$summary.fixed$mean[2]), intercept = (steep0.5weightedmodel$summary.fixed$mean[1]), linewidth = 2, color = "orange" )



# try out almost perfectly straight lines

head(testdata1)
testdata1$Number3<- 5

testdata1$Number3[testdata1$group == "b"]<- 5 + round(1.2^testdata1$Year[testdata1$group == "b"], 0)

basicPlot2<- ggplot(testdata1, aes(y = Number3, x = Year, color = group))+
  scale_y_continuous(trans='log', limits = c(1,50)) + 
  geom_point()+
  stat_smooth(method = 'lm') # this should work
basicPlot2
summary(glm(Number2~group*Year, data = testdata1, family = "poisson"))


noweightmodel2<- (inla(Number3~ Year+
                        f(group, model='iid')+
                        f(groupSlope, Year, model='iid'),
                      data = testdata1, family = "poisson")) # works 
noweightmodel2$summary.fixed


steepweightedmodel2<-inla(Number3~ Year +
                           f(group, model='iid')+
                           f(groupSlope, Year, model='iid'),
                         control.predictor = list(A = A),
                         data = testdata1, family = "poisson") # works 

basicPlot2 + 
  geom_abline(slope = (noweightmodel2$summary.fixed$mean[2]), intercept = (noweightmodel2$summary.fixed$mean[1]), linewidth = 2 )


# half weight to steep slope (1/2)
flatweightedmodel2<-(inla(Number3~ Year+
                           f(group, model='iid')+
                           f(groupSlope, Year, model='iid'),
                         control.predictor = list(A = B),
                         data = testdata1, family = "poisson")) # works 
#               mean    sd 0.025quant 0.5quant 0.975quant mode   kld
# (Intercept) 1.773 0.278      1.219    1.762      2.374   NA 0.003
# Year        0.018 0.010     -0.004    0.018      0.036   NA 0.001

basicPlot2 + 
  geom_abline(slope = (noweightmodel2$summary.fixed$mean[2]), intercept = (noweightmodel2$summary.fixed$mean[1]), linewidth = 2 ) +
  geom_abline(slope = (flatweightedmodel2$summary.fixed$mean[2]), intercept = (flatweightedmodel2$summary.fixed$mean[1]), linewidth = 2, color = "red" )+
  geom_abline(slope = (steepweightedmodel2$summary.fixed$mean[2]), intercept = (steepweightedmodel2$summary.fixed$mean[1]), linewidth = 2, color = "green" )

# I think this does exactly what I want it to: 
# The line that is weighted most 







# What happens when not all groups are present with weights? 



testdata2<- data.frame(
  Year = rep(1:20, 4),
  group = rep(c("a", "b"), each = 40),
  place = rep(c("L1", "L2", "L1", "L2"), each = 20),
  Number = c(rpois(20, 10), rpois(20, 20))
)



weights<- rep(c(1, 2), each = 40 )

# make predictor matrix 
A <- Diagonal(n = nrow(testdata2), x = weights)

# make slopes
testdata2$Number2<- testdata2$Number
testdata2$Number2[testdata2$group == "b"]<- testdata2$Number[testdata2$group == "b"] + testdata2$Year[testdata2$group == "b"]*2
# add random slope for group
testdata2$groupSlope<- testdata2$group

steepweightedmodel3<-inla(Number2~ place + Year : place +
                            f(group, model='iid')+
                            f(groupSlope, Year, model='iid'),
                          control.predictor = list(A = A),
                          data = testdata2, family = "poisson") # works 

steepweightedmodel3$summary.fixed

steepweightedmodel3t<-inla(Number2~ place + Year : place +
                            f(group, model='iid')+
                            f(groupSlope, Year, model='iid'),
                          control.predictor = list(A = A),
                          data = subset(testdata2, place!= "L1" | group != "a"  ), 
                          family = "poisson") # works NOT. problem because group is missing

steepweightedmodel3$summary.fixed


testdata2t<- testdata2
testdata2t$Number2[testdata2t$place== "L1" &testdata2t$group == "a" ] <- NA

steepweightedmodel3t<-inla(Number2~ place + Year : place +
                             f(group, model='iid')+
                             f(groupSlope, Year, model='iid'),
                           control.predictor = list(A = A),
                           data = testdata2t, 
                           family = "poisson") # works somehow with these NA's...

steepweightedmodel3t$summary.fixed





basicPlot2 + 
  geom_abline(slope = (noweightmodel2$summary.fixed$mean[2]), intercept = (noweightmodel2$summary.fixed$mean[1]), linewidth = 2 )



