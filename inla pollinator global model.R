
library (INLA)
INLA:::inla.dynload.workaround()

args <- commandArgs(trailingOnly = T)
output <- args[1]
threads =  as.integer(Sys.getenv("NSLOTS", "1"))

completeDataPollinators<- readRDS("completeDataPollinators.RDS")







  threads = 2

# define prior for RE
sd.res <- 3 * sd(log(completeDataPollinators$NumberRounded[is.finite(log(completeDataPollinators$NumberRounded))]), na.rm = T) 

prior.prec <- list(prec = list(prior = "pc.prec", param = c(sd.res, 0.01))) #1% prob bigger than 1


inlaPolglobalModel1 <- inla(NumberRounded ~ cYear + 
                f(Period_4INLA,model='iid', hyper = prior.prec)+ # 
                
                f(Datasource_4INLA,                  model='iid', hyper = prior.prec)+
                f(DatasourceLoc_4INLA,               model='iid', hyper = prior.prec)+
                f(DatasourceLocPlot_4INLA,           model='iid', hyper = prior.prec)+
                f(Datasource_4INLAslope,       iYear,model='iid', hyper = prior.prec)+
                f(DatasourceLoc_4INLAslope,    iYear,model='iid', hyper = prior.prec)+
                f(DatasourceLocPlot_4INLAslope,iYear,model='iid', hyper = prior.prec)+
                
                f(iYear,model='ar1', replicate=as.numeric(Plot_ID)), # auto regression at plot level
              
              family = 'Poisson',
              control.compute = list( 
                #openmp.strategy="huge"
                config = TRUE, 
                dic=TRUE,
                waic=TRUE, 
                cpo = TRUE ), 
              control.inla = list( tolerance =  1e-08), 
              control.predictor = list(link = 1) , 
              verbose = T, 
              quantiles=c(0.001, 0.01, 0.025, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.95, 0.975, 0.99, 0.999)  ,    
              num.threads = threads, blas.num.threads = threads,
              data=completeDataPollinators) # 





fixed = inlaPolglobalModel1$summary.fixed

parameters$model_name[taskID]
print("done:")
Sys.time() - starttime
#assign(as.character(parameters$model_name[i]), model)

model_file <- file.path(output_dir, "inlaPolglobalModel1_TEST.rds")
fixed_file <- file.path(output_dir, "inlaPolglobalModel1SUMMARY.rds")



saveRDS (model, file =  model_file)
saveRDS (fixed, file = fixed_file)



# extract posterior marginals and sample posteriors
# m <-  data.frame(Metric = metric, 
# 		 Realm = "Freshwater",
# 		 inla.smarginal(model$marginals.fixed$`RealmFreshwater:cYear`, factor = 50))
n <-  data.frame(Realm = "Pollinators",
                 inla.smarginal(inlaPolglobalModel1$marginals.fixed$`cYear`, factor = 50))
marg<- n#rbind(m,n)

marg$y80<- marg$y
#marg$y80  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.1quant"] ]<- NA # allocate 0 to evrything below the 80% quantile 
#marg$y80  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.9quant"] ]<- NA
marg$y80  [  marg$x< fixed["cYear", "0.1quant"] ]<- NA # allocate 0 to everything below the 80% quantile 
marg$y80  [  marg$x> fixed["cYear", "0.9quant"] ]<- NA
marg$y90<- marg$y
#marg$y90  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.05quant"] ]<- NA # allocate 0 to evrything below the 90% quantile 
#marg$y90  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.95quant"] ]<- NA
marg$y90  [  marg$x< fixed["cYear", "0.05quant"] ]<- NA # allocate 0 to everything below the 90% quantile 
marg$y90  [  marg$x> fixed["cYear", "0.95quant"] ]<- NA
marg$y95<- marg$y
#marg$y95  [ marg$Realm == "Freshwater"  & marg$x< fixed["RealmFreshwater:cYear", "0.025quant"] ]<- NA # allocate 0 to evrything below the 95% quantile 
#marg$y95  [ marg$Realm == "Freshwater"  & marg$x> fixed["RealmFreshwater:cYear", "0.975quant"] ]<- NA
marg$y95  [  marg$x< fixed["cYear", "0.025quant"] ]<- NA # allocate 0 to everything below the 95% quantile 
marg$y95  [  marg$x> fixed["cYear", "0.975quant"] ]<- NA


marg_file <- file.path(output_dir, "inlaPolglobalModel1_Marginal.rds")

saveRDS (marg, file =  marg_file)


RandEfDataset <- 	unique(completeDataPollinators[,c("Datasource_ID", "Datasource_name", "Datasource_4INLA", "Datasource_4INLAs", "Realm")])
#pull out random intercepts and slopes:

#data source ID
intercepts     <- model$summary.random$Datasource_4INLA
slopes         <- model$summary.random$Datasource_4INLAslope
slopes_Location<-model$summary.random$DatasourceLoc_4INLAslope
slopes_plot    <-model$summary.random$DatasourceLocPlot_4INLAslope
names(intercepts)[2:ncol(intercepts)]      <- paste("Datasource_Intercept_", names(intercepts)[2:ncol(intercepts)]) # names for dataset intercepts
names(slopes)[2:ncol(intercepts)]          <- paste("Datasource_Slope_", names(slopes)[2:ncol(intercepts)])             # names for dataset slopes
names(slopes_Location)[2:ncol(intercepts)] <- paste("Loc_slp_", names(slopes_Location)[2:ncol(intercepts)]) # names for Location slopes
names(slopes_plot)[2:ncol(intercepts)]     <- paste("Plot_slp_", names(slopes_plot)[2:ncol(intercepts)])        # names for plot slopes

# datasource level slopes for Fig 1
RandEfDataset <- merge(RandEfDataset, intercepts, by.x="Datasource_4INLA", by.y="ID")
RandEfDataset <- merge(RandEfDataset, slopes, by.x="Datasource_4INLAslope", by.y="ID")

# add up fixed slope and random slopes

fx<-data.frame(Realm =  "Pollinators", #
               fixedSlp = inlaPolglobalModel1$summary.fixed$mean[2], 
               fixedIntercept = (inlaPolglobalModel1$summary.fixed$mean[1]  ) )
RandEfDataset <- merge(RandEfDataset, fx, by = "Realm" )
RandEfDataset$slope <- RandEfDataset$'Datasource_Slope_ mean'+ RandEfDataset$fixedSlp # sum of fixed and random slopes  

rand_file <- file.path(output_dir, "randomSlopes.rds")


saveRDS (RandEfDataset, file =  rand_file)

#+
#			 f(iYear, model='ou', replicate=as.numeric(TaxonPlot_4INLA), 
#			hyper = list(theta1 = list(prior='pc.prec')))

#+
#			 f(iYear, model='ou', 
#			hyper = list(theta1 = list(prior='pc.prec')))





# all random slopes 
allRandEf <- 	unique(inlaPolglobalModel1[,c("Datasource_ID", "Datasource_name", "Datasource_4INLA", 
                                            "Datasource_4INLAs", "Realm", 'DatasourceLocPlot_4INLAslope', "DatasourceLoc_4INLAslope" )])




slopes_study         <- inlaPolglobalModel1$summary.random$Datasource_4INLAs
names(slopes_study)[2:ncol(slopes_study)] <- paste0("Study_Slope_", names(slopes_study)[2:ncol(slopes_study)])             # names for dataset slopes
allRandEf<- merge(allRandEf, slopes_study, by.x="Datasource_4INLAs", by.y="ID", all = T )

slopes_Location<-inlaPolglobalModel1$summary.random$DatasourceLoc_4INLAslope
names(slopes_Location)[2:ncol(slopes_Location)] <- paste0("Loc_slp_", names(slopes_Location)[2:ncol(slopes_Location)]) # names for Location slopes
allRandEf<- merge(allRandEf, slopes_Location, by.x="Location_4INLAs", by.y="ID", all = T )

slopes_plot    <-inlaPolglobalModel1$summary.random$DatasourceLocPlot_4INLAslope
names(slopes_plot)[2:ncol(slopes_plot)]     <- paste0("Plot_slp_", names(slopes_plot)[2:ncol(slopes_plot)])        # names for plot slopes
allRandEf<- merge(allRandEf, slopes_plot, by.x="Plot_ID_4INLAs", by.y="ID", all = T )

# slopes_polGroup <- inlaPolglobalModel1$summary.random$TaxonPlot_4INLAs
# names(slopes_spec)[2:ncol(slopes_spec)] <- paste0("Spec_Slope_", names(slopes_spec)[2:ncol(slopes_spec)])             # names for dataset slopes
# allRandEf<- merge(allRandEf, slopes_spec, by.x="TaxonPlot_4INLAs", by.y="ID", all = T )
# 

All_rand_file <- file.path(output_dir, "allRandomSlopes.rds")


saveRDS (allRandEf, file =  All_rand_file)





save (inla1.2r, file = output)


