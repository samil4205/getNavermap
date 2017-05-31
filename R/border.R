getBorderLonLat <- function(center_lon, center_lat, zoom, xpix, ypix){
  zoom_res <- c(2048, 1024, 512, 256, 128, 64,  32,16,8,4,2,1,0.5,0.25)
  width_meter <- zoom_res[zoom] * xpix
  height_meter <- zoom_res[zoom] * ypix

  ur.lon <- destPoint(c(center_lon, center_lat), b=90, d=width_meter/2)[1]
  ur.lat <- destPoint(c(center_lon, center_lat), b=0,  d=height_meter/2)[2]
  ll.lon <- destPoint(c(center_lon, center_lat), b=270, d=width_meter/2)[1]
  ll.lat <- destPoint(c(center_lon, center_lat), b=180, d=height_meter/2)[2]

  c(ll.lon, ll.lat, ur.lon, ur.lat)
}
