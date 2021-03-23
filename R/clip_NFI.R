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
  
  if(!dir.exists(NFI_point_dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))}
  
  
  
  nfi_point <- rgdal::readOGR(NFI_point_dir)
  nfi_point <- sp::spTransform(nfi_point, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")
  
  
  
  if(plygn){
    
    
    plygn <- sp::spTransform(plygn, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")
    clip_point<- raster::intersect(nfi_point,plygn)
    
    
    
  }
  
  
  else if(district) {
    
    code_loc <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
    
    ## epsg5186
    ##do <- sp::spTransform(do, "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +units=m +no_defs")  
    region <- if(code_loc< 99){
      do[do$CTPRVN_CD == code_loc, ]
    }else if(code_loc<99999 & code_loc>100){
      sgg[sgg$SIG_CD == code_loc, ]
    }else{
      emd[emd$EMD_CD == substr(code_loc,1,8), ]}
    
    
    
    clip_point<- raster::intersect(nfi_point,region)
    
    
    
    
  }
  
  else if(NFI_DF) {
    
    sample_list <- unique(NFI_DF[,"표본점번호"])
    clip_point <- nfi_point[sample_list %in% nfi_point$SP_ID,]
  }
  
  else  {
    stop("clip_NFI needs more parameters")}
  
  
  return(clip_point)
  
  
}
