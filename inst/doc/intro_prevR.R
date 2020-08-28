## ----include=FALSE------------------------------------------------------------
Sys.setenv(LANG = "en")

## -----------------------------------------------------------------------------
library(prevR, quietly = TRUE)
col <- c(id = "cluster", x = "x", y = "y", n = "n", pos = "pos", c.type = "residence", wn = "weighted.n", wpos = "weighted.pos")
dhs <- as.prevR(fdhs.clusters, col, fdhs.boundary)
str(dhs)
print(dhs)

## ---- eval = FALSE------------------------------------------------------------
#  imported_data <- import.dhs("data.sav", "gps.dbf")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(dhs, main = "Clusters position")
plot(dhs, type = "c.type", main = "Clusters by residence")
plot(dhs, type = "count", main = "Observations by cluster")
plot(dhs, type = "flower", main = "Positive cases by cluster")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(dhs, axes = TRUE)
dhs <- changeproj(dhs, "+proj=utm +zone=30 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
print(dhs)
plot(dhs, axes = TRUE)

## ---- include=FALSE-----------------------------------------------------------
qa <- quick.prevR(fdhs, return.results = TRUE, return.plot = TRUE, plot.results = FALSE, progression = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  quick.prevR(fdhs)

## ---- echo=FALSE, fig.width=6, fig.height=6-----------------------------------
qa$plot

## ---- fig.width=8, fig.height=4-----------------------------------------------
res <- quick.prevR(fdhs, N = c(100, 200, 300), return.results = TRUE, return.plot = TRUE, plot.results = FALSE, progression = FALSE, nb.cells = 50)
res$plot

## ---- fig.width=6, fig.height=6-----------------------------------------------
# Calculating rings of the same number of observations for different values of N
dhs <- rings(fdhs, N = c(100, 200, 300, 400, 500), progression = FALSE)
print(dhs)
summary(dhs)

# Prevalence surface for N=300
prev.N300 <- kde(dhs, N = 300, nb.cells = 200, progression = FALSE)
library(sp)
spplot(prev.N300, "k.wprev.N300.RInf", cuts = 100, col.regions = prevR.colors.red(101), main = "Regional trends of prevalence (N=300)")

# Surface of rings' radius
radius.N300 <- krige("r.radius", dhs, N = 300, nb.cells = 200)
spplot(radius.N300, cuts = 100, col.regions = prevR.colors.blue(101), main = "Radius of circle (N=300)")

# ggplot2 graph
res <- as.data.frame(prev.N300)
res <- res[!is.na(res$k.wprev.N300.RInf), ]
library(ggplot2)
ggplot(data = res) +
  aes(x = x, y = y, fill = k.wprev.N300.RInf) +
  geom_raster() +
  scale_fill_gradientn(colours=prevR.colors.red(20)) +
  coord_fixed() +
  theme_prevR_light() +
  labs(fill = "Prevalence (%)")

