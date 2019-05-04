# Maxent

# set up ---------------------------------------------
pacman::p_load(dismo,raster,data.table, Soysambu, randomForest, ecospat)

# predictor raster
predictors=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")
myvars=c("dist_bound","dist_roads", "ASTER","interface_raw", "SAVI")
m=as.formula(paste("pb~",paste(myvars,collapse="+")))
predras=subset(predictors, myvars[myvars != "label"])

# train and test sets
sn=ppp_dt(snares)[cat=="NECK SNARE"]
group=kfold(sn,5)
pres_train=sn[group!=1, .(x,y)] %>% setDF
pres_test=sn[group==1, .(x,y)] %>% setDF

# background points
backg=randomPoints(predras,500)
group=kfold(backg, 5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]

# presence and background identifier
presvals=extract(predras, sn[, .(x,y)])
absvals=extract(predras, backg)
pb = c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata=data.table(cbind(pb, rbind(presvals,absvals)))

# sdmdata=envtrain
testpres=extract(predras,pres_test)
testbackg=extract(predras,backg_test)

# domain --------------------------------------------

pred_nf= dropLayer(predras, 'SAVI')
dm <- domain(pred_nf, pres_train)
ed <- evaluate(pres_test, backg_test, dm, pred_nf)
prd=predict(pred_nf,dm)
trd=threshold(ed,'spec_sens')
plot(prd>trd)

ecospat::ecospat.boyce(fit=prd, obs=backg_test)


  # maxent --------------------------------------------
# xm=maxent(predras,pres_train, factors="SAVI")
# plot(xm)
# response(xm)
#
# # evaluate
# em=evaluate(pres_test,backg_test, xm, predras)
# px=predict(predras,xm)
# trm=threshold(em,'spec_sens')
#
# # plot
# par(mfrow=c(1,2))
# plot(px)
# plot(px>trm)
# # plot(subset(predictors, "bush"))
# points(pres_train, pch="+", cex=0.5)
# par(mfrow=c(1,1))

# ecospat::ecospat.boyce(fit=px, obs=backg_test)

# pocplot=function(pred,back,linearize=TRUE,...) {
#   ispresence=c(rep(1,length(pred)), rep(0,length(back)))
#   predd=smoothdist(c(pred,back),ispresence)
#   c=mean(back)*length(back)/length(pred)
#   if(linearize) {
#     fun=function(x,y) c*y/(1-y)
#     predd$y=mapply(fun,predd$x,predd$y)
#     predd$se=mapply(fun, predd$x, press$se)
#     ideal=function(x) x
#     ylab="Relative probability of presence"
#   }
# }


# randomForest-----------------------------------
m=factor(pb) ~ dist_bound+dist_roads+ASTER+interface_raw+SAVI
rf=randomForest(m, data=sdmdata, na.action=na.omit)
erf=evaluate(testpres, testbackg, rf)
pr=predict(predras,rf)
tr=threshold(erf,'spec_sens')

# plot
# par(mfrow=c(1,2))
# plot(pr)
# plot(pr>tr)
# points(pres_train, pch="+", cex=0.5)
# par(mfrow=c(1,1))

plot(pr>tr, legend=F)


# JRip -------------------------------------------

# library(RWeka)
#
# dfx=cbind(pb,envtrain)[, pb := ifelse(pb==1,F,T)]
# jr=JRip(m, data=dfx)
# erj=evaluate(testpres,testbackg,jr)
# pr=predict(predras,jr)
# tr=threshold(erj,'spec_sens')
#
# # plot
# par(mfrow=c(1,2))
# plot(pr)
# plot(pr>tr)
# points(pres_train, pch="+",cex=0.5)
# par(mfrow=c(1,2))
