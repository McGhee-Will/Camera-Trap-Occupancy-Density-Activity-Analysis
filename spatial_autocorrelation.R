# Spatial Autocorrelation

library(sf)        
library(spdep) 
library(terra)

# open space sites
sites_sf <- vect("C:/Users/will5/OneDrive/Documents/GALLANT Technician/Camera Trap Analysis/Openspacesites/Openspacesites/Site_polygon/gdb_data_vectssnew.shp")

# 1. Get centroids of polygons
coords <- crds(centroids(sites_sf))

# 2. Variable of interest
x <- sites_sf$prop_urban

# 3. Subset to non-missing sites
coords_good <- coords[!is.na(x), ]
x_good <- x[!is.na(x)]

# 4. Build neighbors + weights
knn_nb_good <- knn2nb(knearneigh(coords_good, k = 5))
lw_good <- nb2listw(knn_nb_good, style = "W", zero.policy = TRUE)

# 5. Moran’s I
moran.test(x_good, lw_good, alternative = "greater")

# Permutation version (more robust)
mor_perm <- moran.mc(x_good, lw_good, nsim = 999)
print(mor_perm)

# 6. Local Moran’s I (LISA)
locmor <- localmoran(x_good, lw_good, alternative = "greater")
locmor

# If you want to attach back to sites_sf:
sites_sf$localI <- NA
sites_sf$localI[good] <- locmor[, "Ii"]

# Now you can plot local Moran values directly:
plot(sites_sf, "localI")


plot(knn_nb_good, coords_good)


sites_sf$cluster <- NA
sites_sf$cluster[good] <- attr(locmor, "quadr")[, "mean"]

# Replace non-significant with "Not significant"
sites_sf$cluster[good][locmor[, "Pr(z > E(Ii))"] > 0.05] <- "Not significant"

plot(sites_sf, "cluster")



