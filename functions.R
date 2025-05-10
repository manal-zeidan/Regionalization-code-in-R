# Montserrat Fuentes' program  (http://www4.stat.ncsu.edu/~fuentes/)
# to compute geodetic distance
rdistearth<- function(loc1, loc2, miles = FALSE ){
  if (miles) 
    R <- 3963.34
  else R <- 6378.388	
  if(missing(loc2))
    loc2 <- loc1
  R <- 6371
  lat <- loc1[, 2]
  lon <- loc1[, 1]
  coslat1 <- cos((lat * pi)/180)
  sinlat1 <- sin((lat * pi)/180)
  coslon1 <- cos((lon * pi)/180)
  sinlon1 <- sin((lon * pi)/180)
  lat <- loc2[, 2]
  lon <- loc2[, 1]
  coslat2 <- cos((lat * pi)/180)
  sinlat2 <- sin((lat * pi)/180)
  coslon2 <- cos((lon * pi)/180)
  sinlon2 <- sin((lon * pi)/180)
  PP1 <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1)
  PP2 <- cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2)
  pp <- (PP1 %*% t(PP2))
  R * acos(ifelse(pp > 1, 1, pp))
}
# transform the coordinates
# Montserrat Fuentes' projection on the plane Centered around the center of gravity
### lon.lat represent the matrix of the logitude and latitude of the data
lonlat.to.planar<-function(lon.lat, miles =FALSE){
  x <- lon.lat[, 1]
  y <- lon.lat[, 2]
  mx <- mean(x)
  my <- mean(y)
  temp <- cbind(rep(mx, 2), range(y))
  sy <- rdistearth(temp)[2, 1]
  temp <- cbind(range(x), rep(my, 2))
  sx <- rdistearth(temp ,miles = miles)[2, 1]
  temp <- list(x = sx/(max(x) - min(x)), y = sy/(max(y) - min(y)))
  COORD=cbind((x - mx) * temp$x, (y - my) * temp$y)
  return(COORD)
}

#############################
data2frechet<-function(data)
{
  n.site<-dim(data)[2]
  n.observation<-dim(data)[1]
  newdata<-matrix(NA,n.observation,n.site)
  
  for (i in 1: n.site) {
    newdata[,i]<-gev2frech(data[,i],emp=TRUE)	
  }
  return(newdata)
}	
###############################

extremal_concurrence_prob<-function(data)  ######### data after transformed to unit Frechet
{
  n.site<-dim(data)[2]
  dis.matrix <-data
  Co.data<-matrix(NA,n.site,n.site)
  for (i in 1:n.site){ 
    for(j in  1:n.site){
      P.count <- (dis.matrix[,i])
      Q.count <- (dis.matrix[,j])
      Co<-concprob(cbind(P.count,Q.count),COORD[i,],which ="kendall",plot = F)
      Co.data[i,j]<-Co[1,2]
    }}  
  return(Co.data) 
  
} 

###############################################
spectral_clustering<-function(Co.data,k)
{
  n.site<-dim(Co.data)[2]
  dv <-(rowSums(Co.data))
  D<-diag(dv)
  L<-D-Co.data
  dv1<- 1/sqrt(dv)
  D1<-diag(dv1)
  Lsys<-D1 %*% L %*% D1    
  decomp <- eigen(Lsys)
  lamda<-decomp$values
  lamdavector <- decomp$vectors
  lamda<-sort(lamda)
  lamdavector<-lamdavector[,seq(n.site,1,-1)]
  Q<-lamdavector[,1:k]
  Y <- Q/sqrt(rowSums(Q^2))
  n<-ClusterR::GMM(Y, k, verbose = FALSE, seed_mode = "random_spread")
  pr <- ClusterR::predict_GMM(Y, n$centroids, n$covariance_matrices,n$weights)
  cl<-pr$cluster_labels
  return(cl) 
  
}







