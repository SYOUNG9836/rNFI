
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
#' @param largetree : largetree 
#' @keywords biomass2
#' @export 


biomass2 <- function(data, byplot= TRUE, grpby=NULL,largetree=TRUE ){
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(data$'추정간재적')){
    data$'추정간재적' <- as.numeric(data$'추정간재적')} 
  
  
  data_temp <- T_bm(data)
  data_temp <- AG_bm(data_temp)
  data_temp <- c_stck(data_temp)
  ## 이산탄소흡수량 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))*(44/12)--------------------
  data_temp <- data_temp %>% mutate(co2_stock = data_temp$carbon_stock*(44/12))
  data <- data_temp
  
  
  
  ## 표본점 번호별 바이오매스 확인--------------------------------------------------------------
  if (byplot){
    
    ## byplot + group_by--------------------------------------------------------------------------
    ## ex) 수종명, 수관급, 형질급, 광역시도, 시군구, 읍면동, 지형, 임상, 경급, 영급------------------- 
    if (!is.null(grpby)){
      
      if(largetree){
        
        
        bm <- data %>% 
          group_by(data$'표본점번호', data[,grpby]) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename("plot_id"= "data$표본점번호","grpby"= "data[, grpby]")
        
        
        bm_temp <- data %>% 
          group_by(data$'표본점번호', data[,grpby], largetree) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm_temp[,4:8] <- lapply(bm_temp[,4:8], function(x) ifelse(bm_temp$largetree == 1, x/0.08 , x/0.04))
        bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호","grpby"= "data[, grpby]")
        
        
        bm_temp <- bm_temp %>% 
          group_by(plot_id, grpby) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomasS_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- merge(bm, bm_temp, key=c(plot_id, grpby) ,  all= TRUE) 
        
        
        
        
        
      }
      else{
        
        data <- data %>% filter(data$'대경목조사원내존재여부' == 0) 
        bm <- data %>% 
          group_by(data$'표본점번호', data[,grpby]) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),
                    volume_m3_ha = sum(get('추정간재적'), na.rm=TRUE)/0.04,
                    biomass_ton_ha = sum(.data$T_biomass, na.rm=TRUE)/0.04,
                    AG_biomass_ton_ha = sum(.data$AG_biomass, na.rm=TRUE)/0.04,
                    carbon_stock_tC_ha = sum(.data$carbon_stock, na.rm=TRUE)/0.04,
                    co2_stock_tCO2_ha = sum(.data$co2_stock, na.rm=TRUE)/0.04,.groups = 'drop')
        bm <- bm %>% rename("plot_id"= "data$표본점번호","grpby"= "data[, grpby]")
      }
      
      
      
      
    }
    
    ## byplot-------------------------------------------------------------------------
    else{
      
      if(largetree){
        
        bm <- data %>% 
          group_by(data$'표본점번호') %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename("plot_id"= "data$표본점번호")
        
        
        bm_temp <- data %>% 
          group_by(data$'표본점번호', largetree) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm_temp[,3:7] <- lapply(bm_temp[,3:7], function(x) ifelse(bm_temp$largetree == 1, x/0.08 , x/0.04))
        bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호")
        
        
        bm_temp <- bm_temp %>% 
          group_by(plot_id, grpby) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomasS_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- merge(bm, bm_temp, key=c(plot_id),  all= TRUE) 
        
        
      }
      else{
        
        data <- data %>% filter(data$'대경목조사원내존재여부' == 0) 
        bm <- data %>% 
          group_by(data$'표본점번호') %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),
                    volume_m3_ha = sum(get('추정간재적'), na.rm=TRUE)/0.04,
                    biomass_ton_ha = sum(.data$T_biomass, na.rm=TRUE)/0.04,
                    AG_biomass_ton_ha = sum(.data$AG_biomass, na.rm=TRUE)/0.04,
                    carbon_stock_tC_ha = sum(.data$carbon_stock, na.rm=TRUE)/0.04,
                    co2_stock_tCO2_ha = sum(.data$co2_stock, na.rm=TRUE)/0.04,.groups = 'drop')
        
        bm <- bm %>% rename("plot_id"= "data$표본점번호")
        
        
      }
    }
  }
  
  ## 개별 수목별 바이오매스--------------------------------------------------------------
  else{
    
    if (!is.null(grpby)){
      
      if(largetree){
        
        plot_num <- n_distinct(data$'표본점번호')
        
        bm <- data %>% 
          group_by(data[,grpby]) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename("grpby"= "data[, grpby]")
        
        
        bm_temp <- data %>% 
          group_by(data[,grpby], largetree) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm_temp[,3:7] <- lapply(bm_temp[,3:7], function(x) ifelse(bm_temp$largetree == 1, x/(0.08*plot_num) , x/(0.04*plot_num)))
        bm_temp <- bm_temp %>% rename("grpby"= "data[, grpby]")
        
        
        bm_temp <- bm_temp %>% 
          group_by(plot_id, grpby) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomasS_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- merge(bm, bm_temp, key=c(grpby) ,  all= TRUE) 
        
        
      }
      else{
        
        data <- data %>% filter(data$'대경목조사원내존재여부' == 0 )
        plot_num <- n_distinct(data$'표본점번호')
        bm <- data %>% 
          group_by(data[, grpby]) %>%  
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),
                    volume_m3_ha = sum(get('추정간재적'), na.rm=TRUE)/(0.04*plot_num),
                    biomass_ton_ha = sum(.data$T_biomass, na.rm=TRUE)/(0.04*plot_num),
                    AG_biomass_ton_ha = sum(.data$AG_biomass, na.rm=TRUE)/(0.04*plot_num),
                    carbon_stock_tC_ha = sum(.data$carbon_stock, na.rm=TRUE)/(0.04*plot_num),
                    co2_stock_tCO2_ha = sum(.data$co2_stock, na.rm=TRUE)/(0.04*plot_num),.groups = 'drop')
        
        bm <- bm %>% rename("grpby"= "data[, grpby]")
      }
      
    }
    
    else{
      
      if(largetree){
        
        plot_num <- n_distinct(data$'표본점번호')
        
        bm <- data %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        bm_temp <- data %>% 
          group_by(largetree)
        summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                  biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                  AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                  carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                  co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm_temp[,2:6] <- lapply(bm_temp[,2:6], function(x) ifelse(bm_temp$largetree == 1, x/(0.08*plot_num) , x/(0.04*plot_num)))
        
        
        bm_temp <- bm_temp %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomasS_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- cbind(bm, bm_temp) 
        
        
      }
      else{
        
        data <- data %>% filter(data$'대경목조사원내존재여부' == 0 )
        plot_num <- n_distinct(data$'표본점번호')
        bm <- data %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomasS_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),
                    volume_m3_ha = sum(get('추정간재적'), na.rm=TRUE)/(0.04*plot_num),
                    biomass_ton_ha = sum(.data$T_biomass, na.rm=TRUE)/(0.04*plot_num),
                    AG_biomass_ton_ha = sum(.data$AG_biomass, na.rm=TRUE)/(0.04*plot_num),
                    carbon_stock_tC_ha = sum(.data$carbon_stock, na.rm=TRUE)/(0.04*plot_num),
                    co2_stock_tCO2_ha = sum(.data$co2_stock, na.rm=TRUE)/(0.04*plot_num),.groups = 'drop')
        
      }
      
    }
    
  } 
  
  return(bm)
  
}
