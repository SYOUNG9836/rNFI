#' readNFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @param district : district 
#' @return merge data nfi
#' @examples
#' readNFI("D:/y2021/y202101/rNFI/NFI/NFI7/", district = "\uc804\ub77c\ub0a8\ub3c4") 
#' @export


readNFI <- function(dir, district=NULL){
  
  
  ## 경로에 있는 .xlsx 파일 리스트 불러오기--------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))
  
  
  
  ## error message--------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  
 
  
  
  for(i in 1:length(filenames)){
    
   
      ## 일반정보 sheet 불러오기--------------------------------------------------------------
      General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "일반정보", 
                                         col_names = TRUE, col_types = "text")
      
      General_info <- General_info[(names(General_info) != c("조사일자"))]
      
      
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
      data_merge <- left_join(x=Tree_inve, y=Stand_inve, 
                          by=c('집락번호', '표본점번호', '조사차기'))
      
      ## point DB
      
      data_merge <- left_join(data_merge, NFI_plot_DB, by=c('표본점번호'))
      
     
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
            filter(data_merge$EMD_CD == substr(site_code,1,8))}
        
        else if(nchar(site_code) == 5){
          data_merge <- data_merge %>% filter(data_merge$SIG_CD == site_code)}
        
        else{
          data_merge <- data_merge %>% filter(data_merge$CTPRVN_CD == site_code)}
        
        
        ## NFI 자료가 없는 지역구 error-----------------------------------------------------------
        if(nrow(data_merge) == 0) {
          stop(paste('NFI data in ',district ,' does not exist.'))}
      }
      
      
      
      ## 일반정보, 비산림면적을 입목자료 기준으로 merge----------------------------------------------
      data_merge <- left_join(x=data_merge, y=General_info, 
                          by=c('집락번호', '표본점번호', '조사차기',  '조사연도', '임상코드', '임상'))
      
      data_merge <- left_join(x=data_merge, y=Non_forest, 
                          by=c('집락번호', '표본점번호', '조사차기', '조사연도'))
      
      ## .xlsx별(연도별) 데이터 data[[i]]에 기록------------------------------------------
      data[[i]] <- data_merge
      
    }
    
    
  ## .xlsx별(연도별) 데이터 합치기--------------------------------------------------------------
  #NFI <- do.call(rbind, data)
  NFI <- data.table::rbindlist(data, fill=TRUE, use.names=TRUE)
  NFI <- as.data.frame(NFI)
  
  log_col <- c("산림여부", "조사가능여부")
  NFI[ , colnames(NFI) %in% log_col ] <- lapply(lapply(NFI[ , colnames(NFI) %in% log_col ], as.numeric), as.logical)
  
  
  fac_col <- c("토지이용코드", "토지이용","임상코드","임상")
  NFI[ , colnames(NFI) %in% fac_col ] <- lapply(NFI[ , colnames(NFI) %in% fac_col ], as.factor)
  
  ##조사연도 숫자에서 날짜로 바꾸기
  num_col <- c("기본조사원 비산림면적", "대경목조사원 비산림면적","도로로부터의거리", "해발고","경사",
               "방위","중심수관밀도",	"0도수관밀도","120도수관밀도",	"240도수관밀도", "수관밀도평균",
               "흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
               "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적", "조사연도")
  NFI[ , colnames(NFI) %in% num_col ] <- lapply(NFI[ , colnames(NFI) %in% num_col ], as.numeric)
  
  
  char_col <- c("표본점번호", "조사일자", "CTPRVN_CD","SIG_CD", "EMD_CD")
  NFI[ , colnames(NFI) %in% char_col ] <- lapply(NFI[ , colnames(NFI) %in% char_col ], as.character)
  
 
  #date_col <- c("조사일자")
  #NFI$"조사일자" <- as.Date(NFI$"조사일자" ,format = '%Y%m%d')
  
  #NFI[ , colnames(NFI) %in% date_col ] <- lapply(NFI[ , colnames(NFI) %in% date_col ], function(x) as.Date( x ,format = '%Y%m%d'))
  
  
  # 수종별 침활구분 
  
  NFI$'침활구분'[is.na(NFI$'침활구분')] <- unlist(lapply(NFI$'수종명'[is.na(NFI$'침활구분')],
                                                 FUN=function(x){Species_DB$type[which(x==Species_DB$species)]}))
  
  
  # 흉고단면적 기준 임상구분
  
  NFI$basal_area <- (pi*(NFI$'흉고직경'/2)^2)/10000
  NFI$'추정간재적'[is.na(NFI$'추정간재적')] <- ifelse(NFI$'침활구분'[is.na(NFI$'추정간재적')] =="활엽수",
                                            (0.1673*(NFI$'흉고직경'^2.393))/1000, (0.086*(NFI$'흉고직경'^2.393))/1000)
  
  
  stand_temp <- NFI %>% 
    mutate(deciduous_ba = ifelse(NFI$"침활구분" == "활엽수",  basal_area, 0)) %>%
    group_by(NFI$'표본점번호',NFI$'조사연도') %>% 
    summarise(all_ba = sum(basal_area), 
              deciduous_ba = sum(deciduous_ba),
              .groups = 'drop')
  
  stand_temp$percent <- (stand_temp$deciduous_ba/stand_temp$all_ba) *100
  stand_temp$stand <- ifelse(stand_temp$percent>=75, "Deciduous", 
                             ifelse(stand_temp$percent>25, "Mixed", "Coniferous"))
  
  
  stand_temp <- stand_temp %>% rename("표본점번호"= "NFI$표본점번호", "조사연도"= "NFI$조사연도")
  
  condition <- (names(stand_temp) %in% c("표본점번호","조사연도", "stand"))
  NFI <- left_join(NFI, stand_temp[condition], by= c("표본점번호","조사연도"))
  
  
  
  # NFI[,c('집락번호', '표본점번호', '조사차기',  '조사연도', '흉고직경', "기본조사원 비산림면적", "대경목조사원 비산림면적"
  #         "stand",  "수종명" , "학명번호", "수목형태구분","침활구분", "흉고직경", ,"basal_area",'추정간재적', 
  #         "형질급")]
  
 
  NFI <- NFI[!(names(NFI) %in% c("시도코드", "시도", "시군구코드", "시군구", "읍면동코드", "읍면동",
                                 "도엽번호", "입력자", "조사N", "조사E", "조사자1", "조사자2", "조사자3", "조사자4"))]
    
  
  
  return(NFI) 
  
  
}


