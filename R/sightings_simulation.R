# resampling of poacher sightings

pacman::p_load(data.table,Soysambu,foreach)

data(freq_poachers)

x=freq_poachers[!is.na(Freqd), .(No.report,Freqf,Freqd)]
y=x[, .N, Freqf]
y[, freqyr:=c(52,12,12*2,4,0,2*52,1)]
y=y[Freqf !="Never"]

urn=1:365
n=1000
seedfun=1:n

l=foreach(j=1:length(seedfun)) %:%
  foreach(i=1:nrow(y),.combine=rbind) %do% {
    size=y[i,freqyr*N]
    set.seed=seedfun[j]
    res=sample(urn,size=size,replace=T)
    dt=data.table(
      idx=1:size,
      res=sort(res))
    dt[, label:=y[i,Freqf]]
  }

ll=rbindlist(l,idcol=T)

# unique sightings per year
a=unique(ll, by=c("res",".id"))

yearly=a[, .N, .id]
median(yearly$N)/12
quantile(yearly$N,probs=c(0.05,0.9))/12
nrow(a)/n/12

hist(yearly$N/12)
