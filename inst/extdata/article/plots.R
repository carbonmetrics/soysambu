#' figures for article Soysambu
#'

# hotspots ----

par(mfrow=c(1,2))
stienen(snares)
plot(Window(snares), main="Hotspots")
plot(soysambu_roads_psp, style="none", add=T, main="", show.window=F, lwd=1, col="lightgrey")
plotclusters(snares, add=T, distance=250)

