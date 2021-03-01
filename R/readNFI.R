#' readNFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @param location : location
#' @return merge data nfi
#' @examples
#' readNFI("C:/SYP/y2021/y202101/rNFI/NFI/NFI6/")
#' @export




readNFI <- function(dir, location=NULL){
  
  if (stringr::str_sub(dir,-1) != '/') 
    dir <- paste(dir, '/', sep = "") 
  
  
  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))
  
  if(!dir.exists(dir)) {
    stop(paste('Directory', dir, 'does not exist.'))
  }
  
  
  for(i in 1:length(filenames)){
    
    ##일반정보
    General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "일반정보", 
                                        col_names = TRUE, col_types = "text")
    
    G_cname <- c("산림여부", "조사가능여부")
    General_info[ , G_cname ] <- lapply(lapply(General_info[ , G_cname ], as.numeric), as.logical)
   
    G_cname2 <- c("토지이용코드", "토지이용","임상코드","임상")
    General_info[ , G_cname2 ] <- lapply (General_info[ , G_cname2 ], as.factor)
    
    
    ##비산림면적
    Non_forest <-readxl:: read_excel(paste(dir, filenames[i], sep = ""), sheet = "비산림면적",
                                      col_names = TRUE, col_types = "text")
   
    N_cname <- c("기본조사원 비산림면적", "대경목조사원 비산림면적")
    Non_forest[ , N_cname ] <- lapply(Non_forest[ , N_cname ], as.numeric)
    
    
    ##"임분조사표"
    Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임분조사표",
                                      col_names = TRUE, col_types = "text")
    
    S_cname <- c("도로로부터의거리", "해발고","경사","방위","중심수관밀도",	"0도수관밀도",
                 "120도수관밀도",	"240도수관밀도", "수관밀도평균")
    Stand_inve[ , S_cname ] <- lapply(Stand_inve[ , S_cname ], as.numeric)
    
    Stand_inve$"조사일자" <- as.Date(Stand_inve$"조사일자", format = '%Y%m%d')
    
    
    
    ##"임목조사표"
    Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                     col_names = TRUE, col_types = "text")
    
    T_cname <- c("흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
                 "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적")
    Tree_inve[ , T_cname ] <- lapply(Tree_inve[ , T_cname ], as.numeric)
   
    Tree_inve[ , "대경목조사원내존재여부" ] <- 
      lapply(lapply(Tree_inve[ , "대경목조사원내존재여부" ], as.numeric), as.logical)
    
    
    ## merge
    data_merge <- merge(x=Tree_inve, y=General_info, 
                        by=c('집락번호', '표본점번호', '조사차기'), all.x=TRUE)
    
    data_merge <- merge(x=data_merge, y=Non_forest, 
                        by=c('집락번호', '표본점번호', '조사차기', '조사연도'), all.x=TRUE)
    
    data_merge <- merge(x=data_merge, y=Stand_inve, 
                        by=c('집락번호', '표본점번호', '조사차기', '조사연도', '임상코드', '임상'), all.x=TRUE)
   
    
    
    data[[i]] <- data_merge
    
    
    
    ##오타에러 메세지 띄워야함
    ##법정동 이름치면 법정동 코드로 인식, 읍면동까지, 여러개 인자 들어올 때 
    
    if(!is.null(location)){
      
      loc_vec <- unlist(strsplit(location, split=" "))
      
      if(length(loc_vec) == 3){
        data[[i]] <- data[[i]] %>% 
          dplyr::filter(data[[i]]$'광역시도' == loc_vec[1] & data[[i]]$'시군구' == loc_vec[2] & data[[i]]$'읍면동' == loc_vec[3])}
     
      else if (length(loc_vec) == 2){
        data[[i]] <- data[[i]] %>% dplyr::filter(data[[i]]$'광역시도' == loc_vec[1] & data[[i]]$'시군구' == loc_vec[2])}
      
      else if (length(loc_vec) == 1){
        data[[i]] <- data[[i]] %>% dplyr::filter(data[[i]]$'광역시도' == loc_vec[1])}
      
    }
   
    
  }
  
  
  NFI <- do.call(rbind, data)
  
  return(NFI)
  
}




##시간 오래걸려 필요없는 열 제외 옵션으로 all=t로 하면 모든 열 아니면 기본 열만 read하는 기능 

