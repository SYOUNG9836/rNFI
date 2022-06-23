
## biomass 구하기--------------------------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)-----------------
T_bm <- function(data){
  
  output <- data  %>% mutate(T_biomass = case_when(
    
    ## bio_coeff = 국가고유배출계수, 
    ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
    ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
    
    ## 강원지방소나무--------------------------------------------------------------
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "강원지방소나무",6]),
    
    ## 수종별--------------------------------------------------------------
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "중부지방소나무",6]),
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4])*(1+bio_coeff[bio_coeff[,1] == "낙엽송",6]),
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4])*(1+bio_coeff[bio_coeff[,1] == "리기다소나무",6]),
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4])*(1+bio_coeff[bio_coeff[,1] == "곰솔",6]),
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4])*(1+bio_coeff[bio_coeff[,1] == "잣나무",6]),
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4])*(1+bio_coeff[bio_coeff[,1] == "삼나무",6]),
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4])*(1+bio_coeff[bio_coeff[,1] == "편백",6]),
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4])*(1+bio_coeff[bio_coeff[,1] == "굴참나무",6]),
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4])*(1+bio_coeff[bio_coeff[,1] == "신갈나무",6]),
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4])*(1+bio_coeff[bio_coeff[,1] == "상수리나무",6]),
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4])*(1+bio_coeff[bio_coeff[,1] == "졸참나무",6]),
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4])*(1+bio_coeff[bio_coeff[,1] == "붉가시나무",6]),
    
    ## 기타 활엽수 및 기타 침엽수--------------------------------------------------------------
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 활엽수",6]),
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 침엽수",6]),
    TRUE ~ NA_real_
    
  ))
  
  return(output)
  
}



## 지상부 biomass 구하기--------------------------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)-------------------------------
AG_bm <- function(data){
  
  output <- data  %>% mutate(AG_biomass = case_when(
    
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4]),
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4]),
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4]),
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4]),
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4]),
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4]),
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4]),
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4]),
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4]),
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4]),
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4]),
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4]),
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4]),
    
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4]),
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4]),
    TRUE ~ NA_real_
    
  ))
  
  return(output)
  
}



## 탄소흡수량 구하기-----------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))-----------------------
c_stck <- function(data){
  
  output <- data  %>% mutate(carbon_stock = case_when(
    
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "강원지방소나무",6])*0.51,
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "중부지방소나무",6])*0.51,
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4])*(1+bio_coeff[bio_coeff[,1] == "낙엽송",6])*0.51,
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4])*(1+bio_coeff[bio_coeff[,1] == "리기다소나무",6])*0.51,
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4])*(1+bio_coeff[bio_coeff[,1] == "곰솔",6])*0.51,
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4])*(1+bio_coeff[bio_coeff[,1] == "잣나무",6])*0.51,
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4])*(1+bio_coeff[bio_coeff[,1] == "삼나무",6])*0.51,
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4])*(1+bio_coeff[bio_coeff[,1] == "편백",6])*0.51*0.51,
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4])*(1+bio_coeff[bio_coeff[,1] == "굴참나무",6])*0.48,
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4])*(1+bio_coeff[bio_coeff[,1] == "신갈나무",6])*0.48,
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4])*(1+bio_coeff[bio_coeff[,1] == "상수리나무",6])*0.48,
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4])*(1+bio_coeff[bio_coeff[,1] == "졸참나무",6])*0.48,
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4])*(1+bio_coeff[bio_coeff[,1] == "붉가시나무",6])*0.48,
    
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 활엽수",6])*0.48,
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 침엽수",6])*0.51,
    TRUE ~ NA_real_
    
  ))
  
  
  
  
  return(output)
  
}



#' biomass2() Function
#'
#' This function 
#' @param data : data
#' @param byplot : byplot
#' @param grpby : grpby
#' @keywords biomass2
#' @export 


biomass2 <- function(data, byplot= TRUE, grpby=NULL){
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(data$'추정간재적')){
    data$'추정간재적' <- as.numeric(data$'추정간재적')} 
  
  
  data_temp <- T_bm(data)
  data_temp <- AG_bm(data_temp)
  data_temp <- c_stck(data_temp)
  ## 이산탄소흡수량 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))*(44/12)--------------------
  data_temp <- data_temp %>% mutate(co2_stock = data_temp$carbon_stock*(44/12))
  data_temp <- data_temp %>% filter(data_temp$'토지이용' == "임목지")
  data_temp$largetree <- ifelse(data_temp$'흉고직경'>=30, 1, 0)
  
  data_temp$largetree_area <- (100 - data_temp$'대경목조사원 비산림면적')/100
  data_temp$tree_area <-(100 - data_temp$'기본조사원 비산림면적')/100
  
  data <- data_temp
  
  
  
  ## 표본점 번호별 바이오매스 확인--------------------------------------------------------------
  if (byplot){
    
    ## byplot + group_by--------------------------------------------------------------------------
    ## ex) 수종명, 수관급, 형질급, 광역시도, 시군구, 읍면동, 지형, 임상, 경급, 영급------------------- 
    if (!is.null(grpby)){
      
        bm <- data %>% 
          group_by(data$'표본점번호', data$'조사연도', data[,grpby]) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename("plot_id"= "data$표본점번호","year"= "data$조사연도" ) #"grpby"= "data[, grpby]"
        
        
        bm_temp <- data %>% 
          group_by(data$'표본점번호', data$'조사연도', data[,grpby], largetree) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
        bm_temp[condition] <- lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, x/0.08 , x/0.04))
        bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도")  #"grpby"= "data[, grpby]"
        
        
        bm_temp <- bm_temp %>% 
          group_by(plot_id, grpby) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- merge(bm, bm_temp, key=c(plot_id, year) ,  all= TRUE) 
        
        
    }
    
    ## byplot !groupby-------------------------------------------------------------------------
    else{
      
        bm <- data %>% 
          group_by(data$'표본점번호', data$'조사연도') %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도")
        
        
        bm_temp <- data %>% 
          group_by(data$'표본점번호', data$'조사연도', largetree) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
        bm_temp[condition] <- lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, x/0.08 , x/0.04))
        bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도")
        
        
        bm_temp <- bm_temp %>% 
          group_by(plot_id) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- merge(bm, bm_temp, key=c(plot_id),  all= TRUE) 
        
        

      
    }
  }
  
  ## !byplot--------------------------------------------------------------
  else{
    
    if (!is.null(grpby)){
      
      
      # 대경목 조사원
      bm_temp <- data %>% 
        group_by(data$'표본점번호',data$'조사연도', data$'임상코드', data[,grpby], largetree, largetree_area, tree_area) %>% 
        summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                  biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                  AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                  carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                  co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
      
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(0.08*bm_temp$largetree_area),
                                                      x/(0.04*bm_temp$tree_area)))
      bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도", 
                                    "forest_stand"= "data$임상코드", "grpby"= "data[, grpby]")
      
      # 플롯별
      bm_temp <- bm_temp %>% 
        group_by(plot_id, year, forest_stand, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
      
      # 
      weight_grpby <- bm_temp %>% 
        group_by(grpby) %>% 
        summarise(plot_num_all = n(),.groups = 'drop')
      
      
      weight_year <- bm_temp %>% 
        group_by(year, grpby) %>% 
        summarise(plot_num_year = n(),.groups = 'drop')
      
      
      weight_stand <- bm_temp %>% 
        group_by(year, forest_stand, grpby) %>% 
        summarise(plot_num_stand = n(),.groups = 'drop')
      
      
      
      weight <- merge(weight_stand, weight_year, by =c("year", "grpby"))
      weight$weight <- weight$plot_num_stand/weight$plot_num_year
      
      #기본계획구(p)내 표본층(h)의 ha당 평균 임목자원량
      bm_temp2 <- bm_temp %>% 
        group_by(year, forest_stand, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      bm_temp2 <- merge(bm_temp2, weight, by =c("year", "forest_stand", "grpby"))
      
      
      condition <- (names(bm_temp2) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp2[condition] <- 
        lapply(bm_temp2[condition], function(x) (x/bm_temp2$plot_num_stand)*bm_temp2$weight)
      
      
      #
      bm_temp3 <- bm_temp2 %>% 
        group_by(year, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      
      weight <- merge(weight_year, weight_grpby, by =c("grpby"))
      weight$weight <- weight$plot_num_year/weight$plot_num_all
      
      
      bm_temp3 <- merge(bm_temp3, weight, by =c("year", "grpby"))
      
      
      condition <- (names(bm_temp3) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp3[condition] <- 
        lapply(bm_temp3[condition], function(x) x*bm_temp3$weight )
      
      
      bm_temp3 <- bm_temp3 %>% 
        group_by(grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      bm <- bm_temp3
      
        

    }
    
    else{
        
        plot_num <- n_distinct(data$'표본점번호')
        
        bm <- data %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        bm_temp <- data %>% 
          group_by(largetree)
        summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                  biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                  AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                  carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                  co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm_temp[,2:6] <- lapply(bm_temp[,2:6], function(x) ifelse(bm_temp$largetree == 1, x/(0.08*plot_num) , x/(0.04*plot_num)))
        
        
        bm_temp <- bm_temp %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- cbind(bm, bm_temp) 
        
      
    }
    
  } 
  
  return(bm)
  
}
