#' readNFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @param district : district
#' @param col_all : all column read 
#' @return merge data nfi
#' @examples
#' readNFI("C:/SYP/y2021/y202101/rNFI/data/NFI/NFI6/", district = "\uc804\ub77c\ub0a8\ub3c4") 
#' @export


readNFI <- function(dir, district=NULL, col_all=FALSE){
   
  
  ## 경로에 있는 .xlsx 파일 리스트 불러오기--------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))
  
   
  
  ## error message--------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  
  
  if(!is.logical(col_all)) {
    stop("param 'col_all' must be 'logical'")
  }
 
  
  
  
  
  for(i in 1:length(filenames)){
    
    ## 모든 column 불러오기--------------------------------------------------------------
    if(col_all){ 
    
      ## 일반정보 sheet 불러오기--------------------------------------------------------------
      General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "일반정보", 
                                         col_names = TRUE, col_types = "text")
      
      ## 비산림면적 sheet 불러오기--------------------------------------------------------------
      Non_forest <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "비산림면적",
                                       col_names = TRUE, col_types = "text")

      ## 임분조사표 sheet 불러오기--------------------------------------------------------------
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임분조사표",
                                       col_names = TRUE, col_types = "text")
      
      ## 임목조사표 sheet 불러오기--------------------------------------------------------------
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                      col_names = TRUE, col_types = "text")
      
      
  
      ## 임목조사표, 임분조사표 merge-----------------------------------------------------------
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c('집락번호', '표본점번호', '조사차기'), all.x=TRUE)
      
    
      
      ## 지역별 filitering--------------------------------------------------------------
      if(!is.null(district)){
        
        if(!is.character(district)) {
          stop("param 'district' must be 'character'")
        }
        
        ## district_code = 법정동 코드 전체자료, 출처 : "행정표준코드관리시스템"
        ## 행정표준코드관리시스템에 없는 법정동명일 때 error----------------------------------------------
        if(any(district %in% district_code[,2] == FALSE )) {
          stop(paste( 'District ', district, ' does not exist.'))
        }
        
        
        site_code <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
        
        if(nchar(site_code) == 10){
          data_merge <- data_merge %>% 
            filter(data_merge$'읍면동코드' == substr(site_code,1,8))}
        
        else if(nchar(site_code) == 5){
          data_merge <- data_merge %>% filter(data_merge$'시군구코드' == site_code)}
        
        else{
          data_merge <- data_merge %>% filter(data_merge$'시도코드' == site_code)}
        
        
        ## NFI 자료가 없는 지역구 error-----------------------------------------------------------
        if(nrow(data_merge) == 0) {
          stop(paste('NFI data in ',district ,' does not exist.'))}
        }
    
      
     
      ## 일반정보, 비산림면적을 입목자료 기준으로 merge----------------------------------------------
      data_merge <- merge(x=data_merge, y=General_info, 
                          by=c('집락번호', '표본점번호', '조사차기',  '조사연도', '임상코드', '임상'), 
                          all.x=TRUE)
      
      data_merge <- merge(x=data_merge, y=Non_forest, 
                          by=c('집락번호', '표본점번호', '조사차기', '조사연도'), all.x=TRUE)
      
      ## .xlsx별(연도별) 데이터 data[[i]]에 기록------------------------------------------
      data[[i]] <- data_merge
      
    }
    
    
   
    ## 필요한 column만 불러오기, defult--------------------------------------------------------------
    else {
    
      ## 임분조사표 sheet, 임목조사표 sheet만 불러오기---------------------------------------------
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임분조사표",
                                       col_names = TRUE, col_types = "text")
      
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                      col_names = TRUE, col_types = "text")
      
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c('집락번호', '표본점번호', '조사차기'), all.x=TRUE)
      
      
      
      if(!is.null(district)){
        
        site_code <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
        
        if(nchar(site_code) == 10){
          data_merge <- data_merge %>% 
            filter(data_merge$'읍면동코드' == substr(site_code,1,8))}
        
        else if(nchar(site_code) == 5){
          data_merge <- data_merge %>% filter(data_merge$'시군구코드' == site_code)}
        
        else{
          data_merge <- data_merge %>% filter(data_merge$'시도코드' == site_code)}
        
      
        if(nrow(data_merge) == 0) {
          stop(paste('NFI data in ',district ,' does not exist.'))}
        }
    
      data[[i]] <- data_merge
      
      }
    }
  
  
  
  ## .xlsx별(연도별) 데이터 합치기--------------------------------------------------------------
  NFI <- do.call(rbind, data)
  
  
  
  log_col <- c("산림여부", "조사가능여부")
  NFI[ , colnames(NFI) %in% log_col ] <- lapply(lapply(NFI[ , colnames(NFI) %in% log_col ], as.numeric), as.logical)
  
  
  fac_col <- c("토지이용코드", "토지이용","임상코드","임상")
  NFI[ , colnames(NFI) %in% fac_col ] <- lapply(NFI[ , colnames(NFI) %in% fac_col ], as.factor)
  
  
  num_col <- c("기본조사원 비산림면적", "대경목조사원 비산림면적","도로로부터의거리", "해발고","경사",
               "방위","중심수관밀도",	"0도수관밀도","120도수관밀도",	"240도수관밀도", "수관밀도평균",
               "흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
               "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적")
  NFI[ , colnames(NFI) %in% num_col ] <- lapply(NFI[ , colnames(NFI) %in% num_col ], as.numeric)
  
  
  date_col <- c("조사일자")
  NFI[ , colnames(NFI) %in% date_col ] <- as.Date(NFI[ , colnames(NFI) %in% date_col ], format = '%Y%m%d')
  
  
  
  return(NFI) 
  
  
}


