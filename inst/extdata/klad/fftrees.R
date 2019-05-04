# FFTrees

pacman::p_load(data.table, raster, FFTrees, spatstat, Soysambu, dismo, viridis)

# load and select data =================================

type="necksnares"

predictors=brick("/home/henk/Documents/PhD/Soysambu/Soysambu/inst/extdata/spatial/brickfile.grd")

open_areas=subset(predictors, "open_areas")
bush=spatialEco::raster.invert(open_areas)

predictors=dropLayer(predictors, 7)
predictors=addLayer(predictors, bush)
i=which(names(predictors)=="open_areas")
names(predictors)[i]="bush"

# select rasters, make formula and extract from raster
myvars=c("slope","dist_bound","dist_roads", "TWI","ASTER","interface_raw", "bush")
m=as.formula(paste("pb~",paste(myvars,collapse="+")))
predraster=subset(predictors, myvars[myvars != "label"])

# presence
necksnares=ppp_dt(snares)[cat=="NECK SNARE", .(x,y)]
footsnares=ppp_dt(snares)[cat=="GROUND SNARE", .(x,y)]

if(type=="necksnares") {
  presvals=extract(predraster, necksnares)
}else{
  presvals=extract(predraster, footsnares)
}

# background ------------------------------
mask=subset(predictors, "SAVI")
set.seed(1964)
bg=dismo::randomPoints(mask, 5000)
absvals=raster::extract(predraster, bg)

# put together
df=rbind(data.table(presvals), data.table(absvals))
df[, pb := c(rep(1, nrow(presvals)),
             rep(0, nrow(absvals)))]

# clean up -----------------------------
# NA values (edge cases)
df=na.omit(df)

# round
myvars1=c("slope", "interface_raw", "TWI")
df[, (myvars1):=lapply(.SD, function(x){round(x,2)}), .SDcols=myvars1]
myvars2=c("dist_bound","dist_roads","ASTER")
df[, (myvars2):=lapply(.SD, round), .SDcols=myvars2]

# training and test sets
df[, pb := ifelse(pb==1,T,F)]
sample=base::sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train=df[sample, ]
test=df[-sample, ]

# modelling =================================================================


f=FFTrees(m,
          data=train, data.test=test,
          decision.labels=c("no snares", "snares"))

# plot
plot(f)

# plot raster
plot.fft(f,predictors, 1)

# work with ROC and PR curves to select different patrolling strategies
