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
  
  
  ## Read the NFI data(.xlsx) list in the local directory --------------------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))
  
  
  
  ## Warning message due to local directory input error --------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  
  ##--------------------------------------------------------------
  ## district error message
  ## 행정표준코드관리시스템에서 우리나라 법정동명,코드 변화 파일 다운로드, 이와 비교 시 없는 법정동명일 때 에러 즉 오타
  ##--------------------------------------------------------------
  if(any(district %in% district_code[,stringi::stri_unescape_unicode("\\ubc95\\uc815\\ub3d9\\uba85") == FALSE] )) {
    
    stop(paste( 'District ', district, ' does not exist.'))
    
  }
  
  
  
  for(i in 1:length(filenames)){
    
    ##--------------------------------------------------------------
    ## 기본 defult는 FALSE
    ## 모든 column 불러오기, 작업 속도에 너무 많은 시간 소요하기 때문에 비추천 
    ##--------------------------------------------------------------
    if(col_all){ 
      
      
      ##--------------------------------------------------------------
      ## 일반정보 sheet 불러오기 
      ## 기본 타입 text, 일부 logical("산림여부", "조사가능여부")과 factor("토지이용코드", "토지이용","임상코드","임상")
      ## 타입을 지정한 열이 불러온 자료에 없으면? 타입은 바꿔야겠고 흠.. 고민 그 사람이 알아서 그 열을 만들어서 넣게 해야할까? 
      ## 아님 코드로 필수열 임의로 삽입?
      ##--------------------------------------------------------------
      General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = stringi::stri_unescape_unicode("\\uc77c\\ubc18\\uc815\\ubcf4"), 
                                         col_names = TRUE, col_types = "text")
      
      G_cname <- c("\\uc0b0\\ub9bc\\uc5ec\\ubd80", "\\uc870\\uc0ac\\uac00\\ub2a5\\uc5ec\\ubd80")
      G_cname<-stringi::stri_unescape_unicode(G_cname)
      General_info[ , G_cname ] <- lapply(lapply(General_info[ , G_cname ], as.numeric), as.logical)
      
      G_cname2 <- c("\\ud1a0\\uc9c0\\uc774\\uc6a9\\ucf54\\ub4dc", "\\ud1a0\\uc9c0\\uc774\\uc6a9","\\uc784\\uc0c1\\ucf54\\ub4dc","\\uc784\\uc0c1")
      General_info[ , G_cname2 ] <- lapply (General_info[ , G_cname2 ], as.factor)
      
      
      
      ##--------------------------------------------------------------
      ## 비산림면적 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("기본조사원 비산림면적", "대경목조사원 비산림면적")
      ##--------------------------------------------------------------
      Non_forest <-readxl:: read_excel(paste(dir, filenames[i], sep = ""), sheet = stringi::stri_unescape_unicode("\\ube44\\uc0b0\\ub9bc\\uba74\\uc801"),
                                       col_names = TRUE, col_types = "text")
      
      N_cname <- c("\\uae30\\ubcf8\\uc870\\uc0ac\\uc6d0 \\ube44\\uc0b0\\ub9bc\\uba74\\uc801", "\\ub300\\uacbd\\ubaa9\\uc870\\uc0ac\\uc6d0 \\ube44\\uc0b0\\ub9bc\\uba74\\uc801")
      N_cname<-stringi::stri_unescape_unicode(N_cname)
      Non_forest[ , N_cname ] <- lapply(Non_forest[ , N_cname ], as.numeric)
      
      
      ##--------------------------------------------------------------
      ## 임분조사표 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("도로로부터의거리", "해발고","경사","방위","중심수관밀도",	"0도수관밀도",
      ## "120도수관밀도",	"240도수관밀도", "수관밀도평균")과 Date(조사일자)
      ##--------------------------------------------------------------
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = stringi::stri_unescape_unicode("\\uc784\\ubd84\\uc870\\uc0ac\\ud45c"),
                                       col_names = TRUE, col_types = "text")
      
      S_cname <- c("\\ub3c4\\ub85c\\ub85c\\ubd80\\ud130\\uc758\\uac70\\ub9ac", "\\ud574\\ubc1c\\uace0","\\uacbd\\uc0ac","\\ubc29\\uc704" ,"\\uc911\\uc2ec\\uc218\\uad00\\ubc00\\ub3c4",	"0\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4",
                   "120\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4", "240\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4", "\\uc218\\uad00\\ubc00\\ub3c4\\ud3c9\\uade0")
      
      S_cname<-stringi::stri_unescape_unicode(S_cname)
      Stand_inve[ , S_cname ] <- lapply(Stand_inve[ , S_cname ], as.numeric)
    
      suy_date <- stringi::stri_unescape_unicode("\\uc870\\uc0ac\\uc77c\\uc790")
      Stand_inve[, suy_date ] <- as.Date(suy_date, format = '%Y%m%d')
      
      
      ##--------------------------------------------------------------
      ## 임목조사표 sheet 불러오기 
      ## 기본 타입 text, 일부 numeric("흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
      ## "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적")과 logical("대경목조사원내존재여부")
      ##--------------------------------------------------------------
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = stringi::stri_unescape_unicode("\\uc784\\ubaa9\\uc870\\uc0ac\\ud45c"),
                                      col_names = TRUE, col_types = "text")
      
      T_cname <- c("\\ud749\\uace0\\uc9c1\\uacbd", "\\uc9c0\\ud558\\uace0", "\\uc218\\uace0","\\uac70\\ub9ac(m)", "\\ubc29\\uc704\\uac01(\\u00ba)",  "\\uc218\\ub839",	"\\uc218\\uae38\\uc774",	 "\\uc0dd\\uc7a5\\ub7c9",
                   "\\uc218\\ud53c",	"\\ube44\\uc728", "\\ud45c\\uc900\\ubaa9\\uac04\\uc7ac\\uc801",	"\\ucd94\\uc815\\uc218\\uace0",	"\\ucd94\\uc815\\uac04\\uc7ac\\uc801")
      T_cname<-stringi::stri_unescape_unicode(T_cname)
      Tree_inve[ , T_cname ] <- lapply(Tree_inve[ , T_cname ], as.numeric)

      Tree_inve[ , stringi::stri_unescape_unicode("\\ub300\\uacbd\\ubaa9\\uc870\\uc0ac\\uc6d0\\ub0b4\\uc874\\uc7ac\\uc5ec\\ubd80") ] <- 
        lapply(lapply(Tree_inve[ , stringi::stri_unescape_unicode("\\ub300\\uacbd\\ubaa9\\uc870\\uc0ac\\uc6d0\\ub0b4\\uc874\\uc7ac\\uc5ec\\ubd80") ], as.numeric), as.logical)
      
      
      ##--------------------------------------------------------------
      ## merge
      ##--------------------------------------------------------------
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c(stringi::stri_unescape_unicode("\\uc9d1\\ub77d\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\ud45c\\ubcf8\\uc810\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\uc870\\uc0ac\\ucc28\\uae30")), all.x=TRUE)
      
      
      
      
      ##--------------------------------------------------------------
      ## 여러개 인자 들어올 때는? 
      ##--------------------------------------------------------------
      if(!is.null(district)){
        
        district_name <- district_code %>% dplyr::filter(district_code[2] == district)
        code_loc <- (gsub("-", "", district_name[[1]]))
        
        
        
        if(nchar(code_loc) == 10){
          data_merge <- data_merge %>% 
            dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc74d\\uba74\\ub3d9\\ucf54\\ub4dc") == substr(code_loc,1,8)])}
        
        else if (nchar(code_loc) == 5){
          data_merge <- data_merge %>% dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc2dc\\uad70\\uad6c\\ucf54\\ub4dc") == code_loc])}
        
        else if (nchar(code_loc) == 2){
          data_merge <- data_merge %>% dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc2dc\\ub3c4\\ucf54\\ub4dc") == code_loc])}
        
        
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
      ## 임목이 없는 임분 자료는 얻을 수 없음, 필요할 일이 있을까요?
      ##--------------------------------------------------------------
      data_merge <- merge(x=data_merge, y=General_info, 
                          by=c(stringi::stri_unescape_unicode("\\uc9d1\\ub77d\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\ud45c\\ubcf8\\uc810\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\uc870\\uc0ac\\ucc28\\uae30"),  '조사연도', '임상코드', '임상'), all.x=TRUE)
      
      data_merge <- merge(x=data_merge, y=Non_forest, 
                          by=c(stringi::stri_unescape_unicode("\\uc9d1\\ub77d\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\ud45c\\ubcf8\\uc810\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\uc870\\uc0ac\\ucc28\\uae30"), '조사연도'), all.x=TRUE)
      
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
      Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = stringi::stri_unescape_unicode("\\uc784\\ubd84\\uc870\\uc0ac\\ud45c"),
                                       col_names = TRUE, col_types = "text")
      
      S_cname <- c("\\ub3c4\\ub85c\\ub85c\\ubd80\\ud130\\uc758\\uac70\\ub9ac", "\\ud574\\ubc1c\\uace0","\\uacbd\\uc0ac","\\ubc29\\uc704" ,"\\uc911\\uc2ec\\uc218\\uad00\\ubc00\\ub3c4",	"0\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4",
                   "120\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4", "240\\ub3c4\\uc218\\uad00\\ubc00\\ub3c4", "\\uc218\\uad00\\ubc00\\ub3c4\\ud3c9\\uade0")
      
      S_cname<-stringi::stri_unescape_unicode(S_cname)
      Stand_inve[ , S_cname ] <- lapply(Stand_inve[ , S_cname ], as.numeric)
      suy_date <- stringi::stri_unescape_unicode("\\uc870\\uc0ac\\uc77c\\uc790")
      Stand_inve[, suy_date ] <- as.Date(suy_date, format = '%Y%m%d')
      
      
      
      ##--------------------------------------------------------------
      ## 임목조사표 sheet 불러오기 
      ##--------------------------------------------------------------
      Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet =stringi::stri_unescape_unicode("\\uc784\\ubaa9\\uc870\\uc0ac\\ud45c"),
                                      col_names = TRUE, col_types = "text")
      
      T_cname <- c("\\ud749\\uace0\\uc9c1\\uacbd", "\\uc9c0\\ud558\\uace0", "\\uc218\\uace0","\\uac70\\ub9ac(m)", "\\ubc29\\uc704\\uac01(\\u00ba)",  "\\uc218\\ub839",	"\\uc218\\uae38\\uc774",	 "\\uc0dd\\uc7a5\\ub7c9",
                   "\\uc218\\ud53c",	"\\ube44\\uc728", "\\ud45c\\uc900\\ubaa9\\uac04\\uc7ac\\uc801",	"\\ucd94\\uc815\\uc218\\uace0",	"\\ucd94\\uc815\\uac04\\uc7ac\\uc801")
      T_cname<-stringi::stri_unescape_unicode(T_cname)
      Tree_inve[ , T_cname ] <- lapply(Tree_inve[ , T_cname ], as.numeric)
      
      Tree_inve[ , stringi::stri_unescape_unicode("\\ub300\\uacbd\\ubaa9\\uc870\\uc0ac\\uc6d0\\ub0b4\\uc874\\uc7ac\\uc5ec\\ubd80") ] <- 
        lapply(lapply(Tree_inve[ ,  stringi::stri_unescape_unicode("\\ub300\\uacbd\\ubaa9\\uc870\\uc0ac\\uc6d0\\ub0b4\\uc874\\uc7ac\\uc5ec\\ubd80") ], as.numeric), as.logical)
      
      
      
      ##--------------------------------------------------------------
      ## 임목자료에 임분자료 merge
      ##--------------------------------------------------------------
      data_merge <- merge(x=Tree_inve, y=Stand_inve, 
                          by=c(stringi::stri_unescape_unicode("\\uc9d1\\ub77d\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\ud45c\\ubcf8\\uc810\\ubc88\\ud638"), stringi::stri_unescape_unicode("\\uc870\\uc0ac\\ucc28\\uae30")), all.x=TRUE)
      
      
      ##--------------------------------------------------------------
      ## 여러개 인자 들어올 때는? 
      ##--------------------------------------------------------------
      if(!is.null(district)){
        
        
        district_name <- district_code %>% dplyr::filter(district_code[2] == district)
        code_loc <- (gsub("-", "", district_name[[1]]))
        
        
        if(nchar(code_loc) == 10){
          data_merge <- data_merge %>% 
            dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc74d\\uba74\\ub3d9\\ucf54\\ub4dc") == substr(code_loc,1,8)])}
        
        else if (nchar(code_loc) == 5){
          data_merge <- data_merge %>% dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc2dc\\uad70\\uad6c\\ucf54\\ub4dc") == code_loc])}
        
        else if (nchar(code_loc) == 2){
          data_merge <- data_merge %>% dplyr::filter(data_merge[,stringi::stri_unescape_unicode("\\uc2dc\\ub3c4\\ucf54\\ub4dc") == code_loc])}
        
        
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


