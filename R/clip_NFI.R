#' clip_NFI() Function
#'
#' This function clip_NFI
#' @param NFI_point_dir : dir
#' @param NFI_DF : NFI_DF
#' @param plygn : plygn
#' @param district : district
#' @keywords clip_NFI
#' @export
#'
#'

clip_NFI <- function(NFI_point_dir, NFI_DF=NULL, plygn=NULL, district=NULL){
  
  if(!file.exists(NFI_point_dir)) {
    stop(paste('file ', NFI_point_dir, ' does not exist.'))}


    
  nfi_point <- rgdal::readOGR(NFI_point_dir)
  nfi_point <- sp::spTransform(nfi_point, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")
  
  
  
  if(!is.null(plygn)){
    
    if (first(class(plygn)) %in% c('sf', 'SpatialPolygons', 'SpatialPolygonsDataFrame') == FALSE){
      stop('mask must be spatial polygons object of class sp or sf. ')
    }
    
    if(first(class(plygn)) == c('sf')){
      plygn <- sf::as_Spatial(plygn)
      
    }
    
    
    
    plygn <- sp::spTransform(plygn, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")
    clip_point<- raster::intersect(nfi_point,plygn)
    
    }
  
  
  
  else if(!is.null(district)) {
    
    site_code <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
    
    
    ## epsg5186
    ## do, sgg, emd = 대한민국 행정구역 시도/시군구/읍면동 2020.05
    ## 출처 : http://www.gisdeveloper.co.kr/?p=2332
    ## do <- sp::spTransform(do, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")  
    region <- 
      if(nchar(site_code) == 2){
      do[do$CTPRVN_CD == site_code, ]
    }else if(nchar(site_code) == 5){
      sgg[sgg$SIG_CD == site_code, ]
    }else{
      emd[emd$EMD_CD == substr(site_code,1,8), ]}
    
    
    
    clip_point <- raster::intersect(nfi_point,region)
    
  }
  
  
  
  else if(!is.null(NFI_DF)) {
    
    sample_list <- unique(NFI_DF[,"표본점번호"])
    clip_point <- nfi_point[nfi_point@data$SP_ID %in% sample_list,]
    
  }
  
  
  
  else  {
    clip_point <- nfi_point}
  
  
  return(clip_point)
  
  }
