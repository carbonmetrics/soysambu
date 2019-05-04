# Machine learning for neck snares

pacman::p_load(raster,data.table,foreach,doParallel,Soysambu, ranger, mlr, spatstat, dismo, FFTrees, randomForest)

# load and select data =================================

type="necksnares"

predictors=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")

# invert open areas -> non open is considered to be "bush"
open_areas=subset(predictors, "open_areas")
bush=spatialEco::raster.invert(open_areas)

# select rasters, make formula and extract from raster
myvars=c("slope","dist_bound","dist_roads", "TWI","ASTER","interface_raw")
m=as.formula(paste("pb~",paste(myvars,collapse="+")))
predraster=subset(predictors, myvars[myvars != "label"])

# make training set and validation set ===============

# presence
necksnares=ppp_dt(snares)[cat=="NECK SNARE", .(x,y)]
footsnares=ppp_dt(snares)[cat=="GROUND SNARE", .(x,y)]

if(type=="necksnares") {
  presvals=extract(predraster, necksnares)
}else{
  presvals=extract(predraster, footsnares)
}

# background
mask=subset(predictors, "SAVI")
set.seed(1964)
bg=dismo::randomPoints(mask, nrow(necksnares))
absvals=raster::extract(predraster, bg)

# put together
df=rbind(data.table(presvals), data.table(absvals))
df[, pb := c(rep(1, nrow(presvals)),
             rep(0, nrow(absvals)))]
df[, `:=` (
  SAVI = factor(SAVI),
  pb=factor(pb)
)]

df=na.omit(df)

myvars=c("slope", "interface_raw", "TWI")
df[, (myvars):=lapply(.SD, function(x){round(x,2)}), .SDcols=myvars]
myvars=c("dist_bound","dist_roads","ASTER")
df[, (myvars):=lapply(.SD, round), .SDcols=myvars]

# training and test sets
sample=base::sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train=df[sample, ]
test=df[-sample, ]

# ====================================================

# randomforest model
rf=randomForest(pb~., data=df)
pr=predict(rf,newdata=train)
prr=predict(predictors, rf)
plot(prr,col=viridis(255, direction=-1))
plotstuff()


# with inTrees -------------------------------------------------------------------------------
pacman::p_load(RRF,inTrees)
dfx=copy(df)
dfx[, SAVI:=NULL]
setDF(dfx)
X=subset(dfx, select=-pb)
target=dfx$pb
rf=RRF(X, as.factor(target), ntree=100)
treeList = inTrees::RF2List(rf)
ruleExec=extractRules(treeList,dfx)
ruleExec=unique(ruleExec)
ruleMetric <- getRuleMetric(ruleExec,X,target)
ruleMetric <- pruneRule(ruleMetric,X,target, maxDecay=0.05, typeDecay=2)
learner <- buildLearner(ruleMetric,X,target, minFreq=0.05)
pred <- applyLearner(learner,X)
read <- presentRules(learner,colnames(X))

freqPattern=getFreqPattern(ruleMetric)
freqPatternMetric <- getRuleMetric(freqPattern,X,target)

library(xtable)
print(xtable(read), include.rownames=FALSE)
print(xtable(ruleMetric[1:2,]), include.rownames=FALSE)


# with pre
pacman::p_load(pre)
res=pre(m, data=dfx)



prednr=as.integer(pred)

# with sboost

mushroom_classifier <- sboost(mushrooms[-1], mushrooms[1], iterations = 5)
mushroom_classifier
mushroom_classifier$classifier
validate(mushrooms[-1], mushrooms[1], iterations = 5, k_fold = 3, positive = "p")

setDF(df)
features=subset(df, select=-pb)
outcomes= df$pb %>% as.data.frame
names(outcomes)="pb"
outcomes$pb=as.character(outcomes$pb)
sb=sboost(subset(df, select=-pb), outcomes, positive="1")
validate(features, outcomes, positive="1")

# with FFTrees
set.seed(1964)
dfx=copy(df)
# mycols=c("dist_bound","dist_roads","ASTER")
# dfx[, (mycols) := lapply(.SD, function(x) {round(x,2)}), .SDcols=mycols]
setDT(dfx)
dfx[, pb := ifelse(pb==1,T,F)]
fft=FFTrees(formula=m,
            data=dfx,
            train.p=0.75,
            main="necksnares",
            decision.labels=c("no snares", "snares")
            )

plot(fft, what="cues")
plot(fft, data="test")
plot(fft, tree=7)
plot(fft)
