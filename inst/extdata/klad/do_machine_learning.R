# do machine learning

predvals=preds[, .(landcover, ASTER, slope, TRI, dist_roads, dist_bounds, dist_clust, label)]
predvals[, dist_b := pmin(dist_bounds, dist_roads)]
predvals[, c("dist_bounds", "dist_roads") := NULL]

bc=bioclim(
  predvals[label=="presence", .(ASTER, slope, TRI, dist_clust, dist_b)]
)

pairs(bc)
response(bc, range="p")
bc %>% predict(predictors) %>% plot

predvals[, label := ifelse(label=="presence", 1, 0)]
m1 <- glm(label ~ ASTER+slope+TRI+dist_clust+dist_b, data=predvals)
predict(m1, pd)
plot(predict(predictors,m1), col=viridis(255))
