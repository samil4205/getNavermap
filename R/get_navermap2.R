get_navermap2 <- function(
  center = c(lon = 126.9849208, lat = 37.5664519), zoom = 4,
  size = c(640,640), format = c("png", "jpeg", "jpg"),
  crs = c("EPSG:4326", "NHN:2048", "NHN:128", "EPSG:4258", "EPSG:4162", "EPSG:2096", "EPSG:2097", "EPSG:2098", "EPSG:900913"),
  baselayer = c("default", "satellite"), color = c("color","bw"),
  overlayers = c("anno_satellite", "bicycle", "roadview", "traffic"),
  markers, key, uri, filename = NULL, messaging = FALSE, urlonly = FALSE,
  force = FALSE, where = tempdir(), archiving = TRUE, ...
){

  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)
  crs <- match.arg(crs)

  if("center" %in% argsgiven){
    if(!(
      (is.numeric(center) && length(center) == 2) ||
      (is.character(center) && length(center) == 1)
    )){
      stop("center of map misspecified, see ?get_googlemap.", call. = F)
    }
    if(all(is.numeric(center))){
      lon <- center[1]; lat <- center[2]
      if(lon < -180 || lon > 180){
        stop("longitude of center must be between -180 and 180 degrees.",
             " note ggmap uses lon/lat, not lat/lon.", call. = F)
      }
      if(lat < -90 || lat > 90){
        stop("latitude of center must be between -90 and 90 degrees.",
             " note ggmap uses lon/lat, not lat/lon.", call. = F)
      }
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && zoom == round(zoom) && zoom > 0)){
      stop("zoom must be a whole number between 1 and 14", call. = F)
    }
  }

  if("size" %in% argsgiven){
    stopifnot(all(is.numeric(size)) && all(size == round(size)) && all(size > 0))
  }

  # format arg checked by match.arg
  format <- match.arg(format)

  # baselayer, overlayers arg checked by match.arg
  baselayer <- match.arg(baselayer)
  if(is.null(overlayers)){
    overlayers <- NULL
  } else {
    overlayers <- match.arg(overlayers, several.ok=TRUE)  
  }
  
  if("markers" %in% argsgiven){
    markers_stop <- TRUE
    if(is.data.frame(markers) && all(apply(markers[,1:2],2,is.numeric))) markers_stop <- FALSE
    if(
      class(markers) == "list" &&
      all(sapply(markers, function(elem){
        is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
      }))
    ) markers_stop <- FALSE
    if(is.character(markers) && length(markers) == 1) markers_stop <- FALSE

    if(markers_stop) stop("improper marker specification, see ?get_navermap.", call. = F)
  }

  if(is.null(filename)){
    destfile <- tempfile(fileext = paste(".", format, sep = ""))
  } else{
    filename_stop <- TRUE
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
    if(filename_stop) stop("improper filename specification, see ?get_navermap", call. = F)
    destfile <- paste(filename, format, sep = '.')
  }

  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))

  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))


  # argument checking (no checks for language, region, markers, path, visible, style)
  color <- match.arg(color)
  crs <- match.arg(crs)

  if(!missing(markers) && class(markers) == "list") markers <- list_to_dataframe(markers)



  # url segments
  base_url <- "https://openapi.naver.com/v1/map/staticmap.bin"
  center_url <- if(all(is.numeric(center))){ # lon/lat specification
    center <- round(center, digits = 6)
    lon <- center[1]; lat <- center[2]
    paste0("center=", paste(lon,lat,sep = ","))
  } else {
    stop("improper center specification, see ?get_navermap.", call. = F)
  }
  zoom_url <- paste0("level=",zoom)
  size_url <- paste0("w=", paste(size, collapse="&h="))
  format_url <- paste0("format=",format)

  baselayer_url <- paste0("baselayer=", baselayer)
  overlayers_url <- paste0("overlayers=", paste(overlayers, collapse=","))
  key_url <- paste0("clientId=", key)
  uri_url <- paste0("url=", uri)
  crs_url <- paste0("crs=", crs)
  color_url <- paste0("color=", color)

  markers_url <-
    if(!missing(markers)){
      if(is.data.frame(markers)){
        paste("markers=",
              paste(
                apply(markers, 1, function(v) paste(round(v,6), collapse = ",")),
                collapse = ","),
              sep = ""
        )
      } else {
        paste("markers=", markers, sep = "")
      }
    } else { "" }

  # format url proper
  post_url <- paste(key_url, uri_url, center_url,crs_url ,zoom_url, size_url, baselayer_url, overlayers_url,
                    format_url, markers_url, color_url, sep = "&")
  url <- paste(base_url, post_url, sep = "?")
  url <- gsub("[&]+","&",url) # removes missing arguments
  if(substr(url, nchar(url), nchar(url)) == "&"){ # if ends with &
    url <- substr(url, 1, nchar(url)-1)
  }
  url <- URLencode(url)
  if(urlonly) return(url)
  if(nchar(url) > 2048) stop("max url length is 2048 characters.", call. = FALSE)

  # finalize filename
  download.file(url, destfile, quiet = !messaging, mode = "wb")
  message(paste0("Map from URL : ", url))

  if(format == "png"){
    map <- readPNG(destfile)
  } else{
    map <- readJPEG(destfile)
  }

  # format file
  if(color == "color"){
    map <- apply(map, 2, rgb)
  } else if(color == "bw"){
    mapd <- dim(map)
    map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
    dim(map) <- mapd[1:2]
  }
  class(map) <- c("ggmap","raster")

  ll <- getBorderLonLat(center[1], center[2], zoom, size[1], size[2])

  attr(map, "bb") <- data.frame(
    ll.lat = ll[2], ll.lon = ll[1],
    ur.lat = ll[4], ur.lon = ll[3]
  )
  
  out <- t(map)
  
  # additional map meta-data
  attr(map, "source")  <- "naver"
  attr(map, "maptype") <- "naver"
  attr(map, "zoom")    <- zoom


  out
}
