require(reticulate)
require(rstudioapi)
require(tidyr)

pack_address <- function(x, mapping){
  for(i in 1:ncol(x)){
    x[,i] <- lapply(x[,i], FUN = as.character)
  }
  x[is.na(x)] <- ""

  x = as.matrix(x)
  x <- x[,mapping]
  address_list <- lapply(1:NROW(x), function(i) as.list(x[i,,drop=FALSE]))

  for(j in 1:length(mapping)){
    for(i in 1:length(address_list)){
      names(address_list[[i]])[j] <- names(mapping)[j]
    }
  }

  return(address_list)
}

batch_geocode <- function(addresses, geocoder, arcgis){

  counter <- seq_along(1:length(addresses))
  counter <- ceiling(counter/500)
  chunks <- split(x = addresses, f = counter)

  for(i in 1:length(chunks)){
    geo_out <- arcgis$geocoding$batch_geocode(addresses = chunks[[i]], geocoder = geocoder)
    if(i ==1){
      adjust = 1
    } else {
      adjust = ((i-1)*500)+1
    }
    geo_data <- as.data.frame(do.call(rbind, geo_out))
    geo_data <- tidyr::unnest_wider(geo_data,col = c(address,location,score,attributes),names_sep = ".")
    geo_data$ID <- as.numeric(geo_data$attributes.ResultID + adjust)
    if(i ==1){
      final_data <- geo_data
    } else {
      final_data <- rbind(final_data,geo_data)
    }
  }
  return(final_data[order(final_data$ID),])
}

load_arcgis <- function(){
  if(reticulate::py_module_available("arcgis")){
    arcgis <- reticulate::import("arcgis")
  } else {
    install <- try(reticulate::py_install("arcgis"))
    if(class(install)=="try-error"){
      cat(paste(install[1], "Now attempting pip install",sep = ""))
      pip_install <- system("pip install arcgis")
      if(pip_install==1){
        cat("Pip install failed. Now attempting conda install")
        conda_install <- system("conda install -c esri arcgis")
        if(conda_install==1){
          stop("Unable to import arcgis python package. Refer to https://developers.arcgis.com/python/guide/intro/")
        } else {
          arcgis <- reticulate::import("arcgis")
        }
      }
    }
  }
  return(arcgis)
}

login_arcgis <- function(arcgis = NULL, url = "", username = "", password = rstudioapi::askForPassword()){
  if(is.null(arcgis)){
    stop("must supply active arcgis module to use this function. Try gisDSTR::load_arcgis()")
  }
  if(url == ""){
    stop("url can not be empty")
  }
  if(username ==""){
    stop("username can not be empty")
  }
  if(password ==""){
    stop("password can not be empty")
  }
  arcgis_inst <- arcgis$gis$GIS(url = url, username = username, password = password)
  return(arcgis_inst)
}

get_geocoders <- function(arcgis = NULL, arcgis_instance = NULL){
  if(is.null(arcgis)){
    stop("must supply active arcgis module to use this function. Try gisDSTR::load_arcgis()")
  }
  if(is.null(arcgis_instance)){
    stop("Must supply active arcgis login instance to use this function. Try gisDSTR::login_arcgis()")
  }
  geocoders <- arcgis$geocoding$get_geocoders(gis = arcgis_instance)
  return(geocoders)
}


full_batch_geocode <- function(x,mapping, username = "", password = rstudioapi::askForPassword(), geocoder = "", url = ""){
  if(url == ""){
    stop("url can not be empty")
  }
  if(username ==""){
    stop("username can not be empty")
  }
  if(password ==""){
    stop("password can not be empty")
  }
  if(geocoder == ""){
    stop("Must supply a name for the geocoder that will be used")
  }
  if(is.null(mapping)){
    stop("Must supply mapping for geocoder input")
  }
  if(is.null(x)){
    stop("X must contain data for geocoding. Please supply a data.frame")
  }

  arcgis <- gisDSTR::load_arcgis()
  arcgis_inst <- gisDSTR::login_arcgis(url = url, arcgis = arcgis, username = username, password = password)
  arcgis_geocoder <- gisDSTR::get_geocoders(arcgis = arcgis, arcgis_instance = arcgis_inst)
  geocoder_ndx <- which(grepl(geocoder,arcgis_geocoder,ignore.case = T))
  if(length(geocoder_ndx)>1){
    stop(paste("Found multiple geocoders with that name. Try to be more specific. Available geocoders: ","\n", paste(sapply(arcgis_geocoder, paste, collapse = "."),collapse = "\n"), sep = ""))
  }
  address_list <- gisDSTR::pack_address(x = x, mapping = mapping)
  data <- gisDSTR::batch_geocode(addresses = address_list, geocoder = arcgis_geocoder[geocoder_ndx],arcgis = arcgis)
  return(data)
}
