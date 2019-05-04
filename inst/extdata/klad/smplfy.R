# Machine learning for neck snares

pacman::p_load(raster,data.table,foreach,doParallel,
               Soysambu, ranger, mlr, spatstat, dismo,
               FFTrees, randomForest, viridis, raster,
               parallelMap, ggplot2)

# load and select data =================================

predictors=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")

# collect data ===============

# presence --------------------------------------------
necksnares=ppp_dt(snares)[cat=="NECK SNARE", .(x,y)]
presvals=raster::extract(predictors,necksnares)
presvals=cbind(necksnares, presvals) # include coordinates

# rounding
myvars1=c("slope", "interface_raw", "TWI", "TRI","interface_class")
presvals[, (myvars1):=lapply(.SD, function(x){round(x,2)}), .SDcols=myvars1]
myvars2=c("dist_bound","dist_roads","ASTER","dist_centroid","distances")
presvals[, (myvars2):=lapply(.SD, round), .SDcols=myvars2]


# background ----------------------------------------
mask=subset(predictors, "SAVI")
set.seed(1964)
# bg=dismo::randomPoints(mask, nrow(necksnares)*2)
bg=dismo::randomPoints(mask, 10000)
absvals=raster::extract(predictors, bg)
absvals=cbind(bg, absvals)  # include coordinates

# put together -------------------------------------
df=rbind(data.table(presvals), data.table(absvals))
df[, pb := c(rep(T, nrow(presvals)),
             rep(F, nrow(absvals)))]

# remove or impute missing values
df=na.omit(df)
# can also impute: https://mlr.mlr-org.com/articles/tutorial/impute.html

# coordinates: for spatial cross-validation
coords=df[, .(x,y)]

# data: remove coordinates and unncessary variables; leave target variable (class)
data=df[, .(ASTER, slope, TRI, TWI,interface_raw,dist_bound,dist_roads,SAVI,pb)]

# avoid mlr warnings
setDF(data)
setDF(coords)

# create task - can include costs
task=makeClassifTask(data=data,
                     target="pb",
                     positive="TRUE",
                     coordinates=coords)

# make learner
lrn=makeLearner(cl="classif.JRip",
                predict.type="prob",
                fix.factors.prediction=T)

n=getTaskSize(task)

n# performance measure
perf_level=makeResampleDesc(method="SpRepCV",folds=5,reps=100)

# resample
# sp_cv=mlr::resample(learner=lrn,task=task,resampling=perf_level,measures=list(ppv,tpr,tnr))

# ppv = positive predictive value = precision = TP/(TP+FP): how many predictive positives are truly positive?
# tpr = true positive rate = recall = sensitivity = TP/(TP+FN): how many truly positives are found back by the model?
# tnr = true negative rate = specificity = TN/(TN+FP): how many truly negatives are correctly identified by the model?
# far = false alarm rate = fp = 1-specificity : out of the truly negative values, how many are incorrectly classified as positive by the model?
# res=sp_cv$measures.test %>% setDT
# ggplot(res) + geom_point(aes(x=tpr,y=ppv)) + theme_bw()

# predict
mod=train(lrn, task, subset=seq(1,n,by=2))
pred=predict(mod,task,subset=seq(2,n,by=2))
performance(pred,measures=list(fpr,fnr,mmce, auc))

# performance
d=generateThreshVsPerfData(pred,measures=list(fpr,fnr,mmce))
plotThreshVsPerf(d) + theme_bw()

# ROC curves
pd=generateThreshVsPerfData(pred,measures=list(ppv,tpr,tnr))
plotROCCurves(pd, diagonal=F) + theme_bw()

# to use ROCR library (DMwR)
rocr.pred=asROCRPrediction(pred)
rocr.perf=ROCR::performance(rocr.pred,"prec","rec")
rocr.perf2=ROCR::performance(rocr.pred, "lift","rpp")

par(mfrow=c(1,2))
ROCR::plot(rocr.perf)
ROCR::plot(rocr.perf2)
par(mfrow=c(1,1))

preds=unlist(rocr.pred@predictions)
trues=rocr.pred@labels %>% unlist %>% as.logical
trues=ifelse(trues==T,0,1)

par(mfrow=c(1,2))
DMwR::PRcurve(preds,trues)
DMwR::CRchart(preds,trues)
par(mfrow=c(1,1))

# on raster
predr=raster::predict(predictors, mod$learner.model, type="prob")
par(mfrow=c(1,2))
plot(predr, col=viridis(255))
predrr=copy(predr)
predrr[predrr>0.5]=NA
plot(predrr)
plotstuff()
par(mfrow=c(1,1))

# performance
# https://mlr.mlr-org.com/articles/tutorial/measures.html


# directly with randomForest

# set.seed(1964)
# setDT(train)
# train=train[, pb := ifelse(pb==T,1,0)]
# # setDT(test)
# # test=test[, pb := ifelse(pb==T,1,0)]
# rf=randomForest(m,data=train)
# pr=predict(rf, test, type="response")
# preds=pr
# trues=test$pb
# myroc=pROC::roc(response=trues,predictor=preds)
# tr=pROC::coords(myroc, "best","threshold")[1]
#


# ml.R
preds=pred.test$data$prob.TRUE
trues=test$pb
myroc=pROC::roc(response=trues,predictor=preds)
plot(myroc)
tr=pROC::coords(myroc, "best","threshold")[1]
predclass=ifelse(preds>tr, 1,0)

