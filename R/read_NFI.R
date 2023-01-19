#' read_NFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @param district : district 
#' @param cwd : dead tree 
#' @return merge data nfi
#' @examples
#' read_NFI("D:/y2021/y202101/rNFI/NFI/NFI7/", district = "\uc804\ub77c\ub0a8\ub3c4") 
#' @export


read_NFI <- function(dir, district=NULL, cwd=TRUE){
  
  
  ## 경로에 있는 .xlsx 파일 리스트 불러오기--------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  filenames <- list.files(path=dir, pattern="xlsx")
  plot_list <- vector("list", length = length(filenames))
  tree_list <- vector("list", length = length(filenames))
  cwd_list <- vector("list", length = length(filenames))
  
  
  
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
    
    ## NFI_plot_DB 행정구역 DB
    Stand_inve$"표본점번호" <- (gsub("-", "", Stand_inve$"표본점번호"))
    Stand_inve <- left_join(Stand_inve, NFI_plot_DB, by=c('표본점번호'))
    
  
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
      
      
      site_code <- (gsub("-", "", district_code[district_code[,2] == district, 1][1]))
      
      if(nchar(site_code) == 10){
        Stand_inve <- Stand_inve %>% 
          filter(Stand_inve$EMD_CD == substr(site_code,1,8))}
      
      else if(nchar(site_code) == 5){
        Stand_inve <- Stand_inve %>% filter(Stand_inve$SIG_CD == site_code)}
      
      else{
        Stand_inve <- Stand_inve %>% filter(Stand_inve$CTPRVN_CD == site_code)}
      
      
      ## NFI 자료가 없는 지역구 error-----------------------------------------------------------
      if(nrow(Stand_inve) == 0) {
        stop(paste('NFI data in ',district ,' does not exist.'))}
    }
    
    General_info$"표본점번호" <- (gsub("-", "", General_info$"표본점번호"))
    Non_forest$"표본점번호" <- (gsub("-", "", Non_forest$"표본점번호"))
    
    
    ## 일반정보, 비산림면적을 임분자료 기준으로 merge----------------------------------------------
    Stand_inve <- left_join(x=Stand_inve, y=General_info, 
                            by=c('집락번호', '표본점번호', '조사차기',  '조사연도', '임상코드', '임상'))
    
    Stand_inve <- left_join(x=Stand_inve, y=Non_forest, 
                            by=c('집락번호', '표본점번호', '조사차기', '조사연도'))
    
    
    ## .xlsx별(연도별) 데이터 plot_list[[i]]에 기록------------------------------------------
    plot_list[[i]] <- Stand_inve
    
    
    plot_all <- unique(plot_list[[i]]$"표본점번호")
    
    ## 임목조사표 sheet 불러오기--------------------------------------------------------------
    tree_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "임목조사표",
                                    col_names = TRUE, col_types = "text")
    tree_list[[i]]$"표본점번호" <- (gsub("-", "", tree_list[[i]]$"표본점번호"))
    tree_list[[i]] <- tree_list[[i]][tree_list[[i]]$"표본점번호" %in% plot_all,]
    
    if(cwd){
      
      ## 고사목조사표 sheet 불러오기--------------------------------------------------------------
      cwd_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = "고사목조사표", range = cellranger::cell_cols("A:M"),
                                          col_names = TRUE, col_types = "text")
      
      cwd_list[[i]]$"표본점번호" <- (gsub("-", "", cwd_list[[i]]$"표본점번호"))
      cwd_list[[i]] <- cwd_list[[i]][cwd_list[[i]]$"표본점번호" %in% plot_all,]
      
    }
    
    
  }
  
  
  ## .xlsx별(연도별) 데이터 합치기--------------------------------------------------------------
  plot_df <- data.table::rbindlist(plot_list, fill=TRUE, use.names=TRUE)
  plot_df <- as.data.frame(plot_df)
  plot_df$"집락번호" <- (gsub("-", "", plot_df$"집락번호"))
  
  
  tree_df <- data.table::rbindlist(tree_list, fill=TRUE, use.names=TRUE)
  tree_df <- as.data.frame(tree_df)
  tree_df$"집락번호" <- (gsub("-", "", tree_df$"집락번호"))
  
  
  if(cwd){
    cwd_df <- data.table::rbindlist(cwd_list, fill=TRUE, use.names=TRUE)
    cwd_df <- as.data.frame(cwd_df)
    cwd_df$"집락번호" <- (gsub("-", "", cwd_df$"집락번호"))
    NFI <- list(plot = plot_df, tree = tree_df, cwd = cwd_df)
    
  }else{
      
    NFI <- list(plot = plot_df, tree = tree_df)
    
    }
  
  NFI$tree <- left_join(NFI$tree, NFI$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도')])
  NFI$cwd <- left_join(NFI$cwd, NFI$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도')])
  
  
  ## column 속성 맞추기
  log_col <- c("산림여부", "조사가능여부")
  NFI$plot[ , colnames(NFI$plot) %in% log_col ] <- lapply(lapply(NFI$plot[ , colnames(NFI$plot) %in% log_col ], as.numeric), as.logical)
  
  
  fac_col <- c("토지이용코드", "토지이용","임상코드","임상", "부후도코드")
  NFI$plot[ , colnames(NFI$plot) %in% fac_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% fac_col ], as.factor)
  
  
  num_col <- c("기본조사원 비산림면적", "대경목조사원 비산림면적","도로로부터의거리", "해발고","경사",
               "방위","중심수관밀도",	"0도수관밀도","120도수관밀도",	"240도수관밀도", "수관밀도평균",
               "흉고직경", "지하고", "수고", "거리(m)", "방위각(º)", "수령",	"수길이",	"생장량",
               "수피",	"비율", "표준목간재적",	"추정수고",	"추정간재적", "조사연도", "재적", "길이", "직경")
  NFI$plot[ , colnames(NFI$plot) %in% num_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% num_col ], as.numeric)
  NFI$tree[ , colnames(NFI$tree) %in% num_col ] <- lapply(NFI$tree[ , colnames(NFI$tree) %in% num_col ], as.numeric)
  
  
  char_col <- c("표본점번호", "집락번호", "조사일자", "CTPRVN_CD","SIG_CD", "EMD_CD")
  NFI$plot[ , colnames(NFI$plot) %in% char_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% char_col ], as.character)
  NFI$tree[ , colnames(NFI$tree) %in% char_col ] <- lapply(NFI$tree[ , colnames(NFI$tree) %in% char_col ], as.character)
  
  
  if(cwd){
    NFI$cwd[ , colnames(NFI$cwd) %in% num_col ] <- lapply(NFI$cwd[ , colnames(NFI$cwd) %in% num_col ], as.numeric)
    NFI$cwd[ , colnames(NFI$cwd) %in% char_col ] <- lapply(NFI$cwd[ , colnames(NFI$cwd) %in% char_col ], as.character)
    NFI$cwd <- left_join(NFI$cwd, Species_DB, by= c("수종명" ="species") )
  }
  

  ##  수종 db 가져오기
  NFI$tree <- left_join(NFI$tree, Species_DB, by= c("수종명" ="species") )
  
  
  # 흉고단면적 기준 임상구분 부표본점 단위
  NFI$tree$basal_area <- (pi*(NFI$tree$'흉고직경'/2)^2)/10000
 
  stand_sub <- NFI$tree %>% 
    mutate(deciduous_ba = ifelse(NFI$tree$type_leaf == "활엽수",  basal_area, 0)) %>%
    group_by(NFI$tree$'표본점번호', NFI$tree$'조사차기') %>% 
    summarise(all_ba = sum(basal_area), 
              deciduous_ba = sum(deciduous_ba),
              .groups = 'drop')
  
  stand_sub$percent <- (stand_sub$deciduous_ba/stand_sub$all_ba) *100
  stand_sub$stand_subplot <- ifelse(stand_sub$percent>=75, "Deciduous", 
                             ifelse(stand_sub$percent>25, "Mixed", "Coniferous"))
  
  
  stand_sub <- stand_sub %>% rename("표본점번호"= "NFI$tree$표본점번호", "조사차기"= "NFI$tree$조사차기")
  
  condition <- (names(stand_sub) %in% c("표본점번호","조사차기", "stand_sub"))
  NFI$plot <- left_join(NFI$plot, stand_sub[condition], by= c("표본점번호","조사차기"))
  
  
  # 흉고단면적 기준 임상구분 부표본점 단위
  stand_clust <- NFI$tree %>% 
    mutate(deciduous_ba = ifelse(NFI$tree$type_leaf == "활엽수",  basal_area, 0)) %>%
    group_by(NFI$tree$'집락번호', NFI$tree$'조사차기') %>% 
    summarise(all_ba = sum(basal_area), 
              deciduous_ba = sum(deciduous_ba),
              .groups = 'drop')
  
  stand_clust$percent <- (stand_clust$deciduous_ba/stand_clust$all_ba) *100
  stand_clust$stand_clusterplot <- ifelse(stand_clust$percent>=75, "Deciduous", 
                                    ifelse(stand_clust$percent>25, "Mixed", "Coniferous"))
  
  
  stand_clust <- stand_clust %>% rename("집락번호"= "NFI$tree$집락번호", "조사차기"= "NFI$tree$조사차기")
  
  NFI$plot <- left_join(NFI$plot, stand_clust[condition], by= c("집락번호","조사차기"))
  
  
  
  # 불필요한 열 제거 
  NFI$plot <- NFI$plot[!(names(NFI$plot) %in% c("시도코드", "시도", "시군구코드", "시군구", "읍면동코드", "읍면동",
                                 "도엽번호", "입력자", "조사N", "조사E", "조사자1", "조사자2", "조사자3", "조사자4"))]
  
  
  
  return(NFI) 
  
  
}


