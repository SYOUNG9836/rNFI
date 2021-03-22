#' readNFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @param district : district
#' @param col_all : all column read 
#' @return merge data nfi
#' @examples
#' readNFI("C:/SYP/y2021/y202101/rNFI/data/NFI/NFI6/", district = "\uc804\ub77c\ub0a8\ub3c4 \uc2e0\uc548\uad70") 
#' @export





readNFI <- function(dir, district=NULL, col_all=FALSE){
  
  
  ##--------------------------------------------------------------
  ## 경로에 있는 .xlsx 파일 리스트 불러오기 
  ## 불러오고 싶은 NFI 자료는 한 폴더 안에 모아두어야 함 
  ##--------------------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))
  
  
  ##--------------------------------------------------------------
  ## dir error message
  ## 경로를 잘못 입력 시 에러 메시지 나타남
  ##--------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  ##--------------------------------------------------------------
  ## district error message
  ## 행정표준코드관리시스템에서 우리나라 법정동명,코드 변화 파일 다운로드, 이와 비교 시 없는 법정동명일 때 에러 즉 오타
  ##--------------------------------------------------------------
  if(any(district %in% district_code[,2] == FALSE )) {
    
    stop(paste( 'District ', district, ' does not exist.'))
    
  }
  
  
  
  for(i in 1:length(filenames)){
    
    ##--------------------------------------------------------------
    ## 기본 defult는 FALSE
    ## 모든 column 불러오기, 작업 속도에 너무 많은 시간 소요하기 때문에 비추천 
    ##--------------------------------------------------------------
    if(col_all){ 
      
      
      ##--------------------------------------------------------------
      ## 특정 시트 없으면??
      ## 일반정보 sheet 불러오기 
      ## 기본 타입 text, 일부 logical("산림여부", "조사가능여부")과 factor("토지이용코드", "토지이용","임상코드","임상")
      ##--------------------------------------------------------------
      General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "일반정보", 
                                         col_names = TRUE, col_types = "text")
      
     
      
      ##--------------------------------------------------------------
      ## 비산림면적 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("기본조사원 비산림면적", "대경목조사원 비산림면적")
      ##--------------------------------------------------------------
      Non_forest <-readxl:: read_excel(paste(dir, filenames[i], sep = ""), sheet = "비산림면적",
                                       col_names = TRUE, col_types = "text")
      
    
      
      ##--------------------------------------------------------------
      ## 임분조사표 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("도로로부터의거리", "해발고","경사","방위","중심수관밀도",	"0도수관밀도",
      ## "120도수관밀도",	"240도수관밀도", "수관밀도평균")과 Date(조사일자)
      ##--------------------------------------------------------------
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임분조사표",
                                       col_names = TRUE, col_types = "text")
      
     
      
      
      ##--------------------------------------------------------------
      ## 임목조사표 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
      ## "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적")과 logical("대경목조사원내존재여부")
      ##--------------------------------------------------------------
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                      col_names = TRUE, col_types = "text")
      
    
      
      ##--------------------------------------------------------------
      ## merge
      ## merge 기준이 되는 '집락번호', '표본점번호', '조사차기' 중 하나라도 없으면? 
      ##--------------------------------------------------------------
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c('집락번호', '표본점번호', '조사차기'), all.x=TRUE)
      
      
      
      
      ##--------------------------------------------------------------
      ## 여러개 인자 들어올 때는? 
      ##--------------------------------------------------------------
      if(!is.null(district)){
        
        
        code_loc <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
        
        
        
        if(nchar(code_loc) == 10){
          data_merge <- data_merge %>% 
            dplyr::filter(data_merge$'읍면동코드' == substr(code_loc,1,8))}
        
        else if (nchar(code_loc) == 5){
          data_merge <- data_merge %>% dplyr::filter(data_merge$'시군구코드' == code_loc)}
        
        else if (nchar(code_loc) == 2){
          data_merge <- data_merge %>% dplyr::filter(data_merge$'시도코드' == code_loc)}
        
        
        ##--------------------------------------------------------------
        ## district error message
        ## NFI 자료가 없는 지역구가 많음
        ##--------------------------------------------------------------
        if(nrow(data_merge) == 0) {
          
          stop(paste('NFI data in ',district ,' does not exist.'))
          
        }
        
      }
      
      
      
      
      
      ##--------------------------------------------------------------
      ## 모든 시트를 입목자료 기준으로 merge
      ## 임목이 없는 임분 자료는 얻을 수 없음, 필요할 일이 있을까?
      ##--------------------------------------------------------------
      data_merge <- merge(x=data_merge, y=General_info, 
                          by=c('집락번호', '표본점번호', '조사차기',  '조사연도', '임상코드', '임상'), all.x=TRUE)
      
      data_merge <- merge(x=data_merge, y=Non_forest, 
                          by=c('집락번호', '표본점번호', '조사차기', '조사연도'), all.x=TRUE)
      
      data[[i]] <- data_merge
    }
    
    
    ##--------------------------------------------------------------
    ## defult = FALSE
    ## 필요한 column만 불러오기 #우선 임목자료와 임분자료에 있는 column은 모두 불러왔음
    ##--------------------------------------------------------------
    else {
      
      
      
      ##--------------------------------------------------------------
      ## 임분조사표 sheet 불러오기 
      ##--------------------------------------------------------------
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임분조사표",
                                       col_names = TRUE, col_types = "text")
      
 
      
      
      ##--------------------------------------------------------------
      ## 임목조사표 sheet 불러오기 
      ##--------------------------------------------------------------
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                      col_names = TRUE, col_types = "text")
      
 
      
      
      ##--------------------------------------------------------------
      ## 임목자료에 임분자료 merge
      ##--------------------------------------------------------------
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c('집락번호', '표본점번호', '조사차기'), all.x=TRUE)
      
      
      ##--------------------------------------------------------------
      ## 여러개 인자 들어올 때는? 
      ##--------------------------------------------------------------
      if(!is.null(district)){
        
        
        code_loc <- (gsub("-", "", district_code[district_code[,2] == district, 1]))
        
        
        
        if(nchar(code_loc) == 10){
          data_merge <- data_merge %>% 
            dplyr::filter(data_merge$'읍면동코드' == substr(code_loc,1,8))}
        
        else if (nchar(code_loc) == 5){
          data_merge <- data_merge %>% dplyr::filter(data_merge$'시군구코드' == code_loc)}
        
        else if (nchar(code_loc) == 2){
          data_merge <- data_merge %>% dplyr::filter(data_merge$'시도코드' == code_loc)}
        
        
        ##--------------------------------------------------------------
        ## district error message
        ## NFI 자료가 없는 지역구가 많음
        ##--------------------------------------------------------------
        if(nrow(data_merge) == 0) {
          
          stop(paste('NFI data in ',district ,' does not exist.'))
          
        }
        
      }
      
      
      
      ##--------------------------------------------------------------
      ## .xlsx별(연도별) 데이터 data[[i]]에 기록 
      ##--------------------------------------------------------------
      data[[i]] <- data_merge
      
      
      
    }}
  
  ##--------------------------------------------------------------
  ## .xlsx별(연도별) 데이터 합치기
  ##--------------------------------------------------------------
  NFI <- do.call(rbind, data)
  
  
  
  
  return(NFI) 
  
  
}
