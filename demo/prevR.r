par(ask = TRUE)

message("Creating a prevR object", domain = "R-prevR")
col <- c(
  id = "cluster",
  x = "x",
  y = "y",
  n = "n",
  pos = "pos",
  c.type = "residence",
  wn = "weighted.n",
  wpos = "weighted.pos"
)
dhs <- as.prevR(fdhs.clusters, col, fdhs.boundary)

str(dhs)
print(dhs)

plot(dhs, main = "Clusters position")
plot(dhs, type = "c.type", main = "Clusters by residence")
plot(dhs, type = "count", main = "Observations by cluster")
plot(dhs, type = "flower", main = "Positive cases by cluster")

message("Changing coordinates projection", domain = "R-prevR")

plot(dhs, axes = TRUE)
dhs <- changeproj(dhs, "+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs")
print(dhs)
plot(dhs, axes = TRUE)

message("Quick analysis", domain = "R-prevR")

quick.prevR(fdhs)

message(
  "Calculating rings of the same number of observations for different values of N", # nolint
  domain = "R-prevR"
)
dhs <- rings(dhs, N = c(100, 200, 300, 400, 500))
print(dhs)
summary(dhs)

message("Prevalence surface for N=300", domain = "R-prevR")
prev.N300 <- kde(dhs, N = 300, nb.cells = 200)
plot(
  prev.N300["k.wprev.N300.RInf"],
  pal = prevR.colors.red,
  lty = 0,
  main = "Regional trends of prevalence (N=300)"
)
library(ggplot2)
ggplot(prev.N300) +
  aes(fill = k.wprev.N300.RInf) +
  geom_sf(colour = "transparent") +
  scale_fill_gradientn(colours = prevR.colors.red()) +
  theme_prevR_light()

message("Surface of rings' radius", domain = "R-prevR")
radius.N300 <- krige("r.radius", dhs, N = 300, nb.cells = 200)

plot(
  radius.N300,
  pal = prevR.colors.blue,
  lty = 0,
  main = "Radius of circle (N=300)"
)

par(ask = FALSE)
