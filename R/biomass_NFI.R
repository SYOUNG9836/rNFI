
## biomass 구하기--------------------------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)-----------------
bm_df <- function(data){
  
  
  output <- data  %>% mutate(species_bm = case_when(
    
    ## bio_coeff = 국가고유배출계수, 
    ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
    ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
    
    ## 강원지방소나무--------------------------------------------------------------
    (data$'수종명' =="소나무" && ( data$SIG_KOR_NM == "영주군" ||  data$SIG_KOR_NM == "봉화군" ||  data$SIG_KOR_NM == "울진군" || data$SIG_KOR_NM == "영양군" || data$SIG_KOR_NM == "강원도" )) 
    ~ "강원지방소나무" ,
    
    ## 수종별--------------------------------------------------------------
    data$'수종명' =="소나무" ~ "중부지방소나무",
    data$'수종명' =="낙엽송" ~ "낙엽송" ,
    data$'수종명' =="리기다소나무" ~ "리기다소나무",
    data$'수종명' =="곰솔" ~ "곰솔",
    data$'수종명' =="잣나무" ~ "잣나무",
    data$'수종명' =="삼나무" ~ "삼나무" ,
    data$'수종명' =="편백" ~ "편백" ,
    data$'수종명' =="굴참나무" ~ "굴참나무" ,
    data$'수종명' =="신갈나무" ~ "신갈나무" ,
    data$'수종명' =="상수리나무" ~ "상수리나무" ,
    data$'수종명' =="졸참나무" ~ "졸참나무" ,
    data$'수종명' =="붉가시나무" ~ "붉가시나무" ,
    data$'수종명' =="아까시나무" ~ "아까시나무" ,
    data$'수종명' =="자작나무" ~ "자작나무" ,
    data$'수종명' =="백합나무" ~ "백합나무" ,
    data$'수종명' =="현사시나무" ~ "현사시나무" ,
    data$'수종명' =="밤나무" ~ "밤나무" ,
    
    ## 기타 활엽수 및 기타 침엽수--------------------------------------------------------------
    (data$type_ever_g == 1) ~ "상록활엽수" ,
    (data$type_leaf =="활엽수") ~ "기타 활엽수" ,
    (data$type_leaf =="침엽수") ~ "기타 침엽수",
    TRUE ~ as.character(NA)
    
  ))
  
  ## bio_coeff = 국가고유배출계수, 
  ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
  ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
  output <- left_join(output, bio_coeff, by= c("species_bm" ="수종") )
  
  ## 지상부 biomass 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)-------------------------------
  output$AG_biomass <- (output$'추정간재적')*(output$"목재기본밀도_값")*(output$"바이오매스확장계수_값")
  ## biomass 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)-----------------
  output$T_biomass <- output$AG_biomass*(1+output$"뿌리함량비_값")
  ## 탄소흡수량 구하기-----------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))-----------------------
  output$CF <- ifelse(output$type_leaf =="활엽수", 0.48, 0.51 ) ##탄소전환계수
  output$carbon_stock <- output$T_biomass*output$CF
  ## 이산탄소흡수량 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))*(44/12)--------------------
  output$co2_stock = output$carbon_stock*(44/12)
  
  
  return(output)
  
}







#' biomass_NFI() Function
#'
#' This function 
#' @param data : data
#' @param byplot : byplot
#' @param grpby : grpby
#' @keywords biomass
#' @export 


biomass_NFI <- function(data, byplot= TRUE, grpby=NULL){
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(data$'추정간재적')){
    data$'추정간재적' <- as.numeric(data$'추정간재적')} 
  
  
  data_temp <- bm_df(data)
  data_temp <- data_temp %>% filter(data_temp$'토지이용' == "임목지")
  data_temp <- data_temp %>% filter(data_temp$'수목형태구분' == "교목")
  data_temp$largetree <- ifelse(data_temp$'흉고직경'>=30, 1, 0)
  
  # 단위 m2/10
  data_temp$largetree_area <- 0.08 - ((data_temp$'대경목조사원 비산림면적'*10)/10000)
  data_temp$tree_area <- 0.04 - ((data_temp$'기본조사원 비산림면적'*10)/10000)

  
  data <- data_temp
  
  
  
  ## 표본점 번호별 바이오매스 확인--------------------------------------------------------------
  if (byplot){
    
    ## byplot + group_by--------------------------------------------------------------------------
    ## ex) 수종명, 수관급, 형질급, 광역시도, 시군구, 읍면동, 지형, 임상, 경급, 영급------------------- 
    if (!is.null(grpby)){
      
        bm <- data %>% 
          group_by(data$'조사차기', data$'표본점번호', data$'조사연도', data[,grpby]) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename('order' = "data$조사차기","plot_id"= "data$표본점번호","year"= "data$조사연도" , "grpby"= "data[, grpby]") #
        
        
        bm_temp <- data %>% 
          group_by(data$'조사차기', data$'표본점번호', data$'조사연도', data[,grpby], largetree, largetree_area, tree_area) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
        bm_temp[condition] <- lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                                            x/(bm_temp$largetree_area),
                                                                            x/(bm_temp$tree_area)))
        bm_temp <- bm_temp %>% rename('order' = "data$조사차기", "plot_id"= "data$표본점번호", "year"= "data$조사연도", "grpby"= "data[, grpby]")  
        
        
        bm_temp <- bm_temp %>% 
          group_by(order, plot_id, year, grpby) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- full_join(bm, bm_temp, by=c('order', 'plot_id', 'year', 'grpby')) 
        
        
    }
    
    ## byplot !groupby-------------------------------------------------------------------------
    else{
      
        bm <- data %>% 
          group_by(data$'조사차기', data$'표본점번호', data$'조사연도') %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        bm <- bm %>% rename('order' = "data$조사차기", "plot_id"= "data$표본점번호", "year"= "data$조사연도")
        
        
        bm_temp <- data %>% 
          group_by(data$'조사차기', data$'표본점번호', data$'조사연도', largetree, largetree_area, tree_area) %>% 
          summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                    biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                    AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                    carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                    co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
        
        
        condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
        bm_temp[condition] <- lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1,
                                                                            x/(bm_temp$largetree_area),
                                                                            x/(bm_temp$tree_area)))
        bm_temp <- bm_temp %>% rename('order' = "data$조사차기", "plot_id"= "data$표본점번호", "year"= "data$조사연도")
        
        
        bm_temp <- bm_temp %>% 
          group_by(order, plot_id, year) %>% 
          summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                    biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                    AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                    carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                    co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
        
        bm <- full_join(bm, bm_temp, by=c('order', 'plot_id', "year")) 
        
        

      
    }
  }
  
  ## !byplot grpby--------------------------------------------------------------
  else{
    
    if (!is.null(grpby)){
      
      
      # 대경목 조사원 (0.08ha + 0.04ha)
      bm_temp <- data %>% 
        group_by(data$'표본점번호',data$'조사연도', stand, data[,grpby], largetree, largetree_area, tree_area) %>% 
        summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                  biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                  AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                  carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                  co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
      
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도", 
                                    "grpby"= "data[, grpby]")
      
      # 플롯별
      bm_temp <- bm_temp %>% 
        group_by(plot_id, year, stand, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
      

      weight_grpby <- bm_temp %>% 
        group_by(grpby) %>% 
        summarise(plot_num_all = n(),.groups = 'drop')
      
      
      weight_year <- bm_temp %>% 
        group_by(year, grpby) %>% 
        summarise(plot_num_year = n(),.groups = 'drop')
      
      
      weight_stand <- bm_temp %>% 
        group_by(year, stand, grpby) %>% 
        summarise(plot_num_stand = n(),.groups = 'drop')
      
      
      # Double sampling for post-stratification(forest stand)
      weight_DSS <- full_join(weight_stand, weight_year, by =c("year", "grpby"))
      weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
      
      
      # 기본계획구(p)내 표본층(h)의 ha당 평균 임목자원량
      bm_temp_DSS <- bm_temp %>% 
        group_by(year, stand, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("year", "stand", "grpby"))
      
      
      condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp_DSS[condition] <- 
        lapply(bm_temp_DSS[condition], function(x) (x/bm_temp_DSS$plot_num_stand)*bm_temp_DSS$weight_DSS)
      
      
  
      bm_temp_WMA <- bm_temp_DSS %>% 
        group_by(year, grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      # Weighted Moving Average(to combine annual inventory field data)
      weight_WMA <- full_join(weight_year, weight_grpby, by =c("grpby"))
      weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
      
      
      bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("year", "grpby"))
      
      
      condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp_WMA[condition] <- 
        lapply(bm_temp_WMA[condition], function(x) x*bm_temp_WMA$weight_WMA )
      
      
      bm <- bm_temp_WMA %>% 
        group_by(grpby) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')

        

    }
    
    else{
        
      
      # 대경목 조사원 (0.08ha + 0.04ha)
      bm_temp <- data %>% 
        group_by(data$'표본점번호',data$'조사연도', stand, largetree, largetree_area, tree_area) %>% 
        summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                  biomass_ton = sum(.data$T_biomass, na.rm=TRUE),
                  AG_biomass_ton = sum(.data$AG_biomass, na.rm=TRUE),
                  carbon_stock_tC = sum(.data$carbon_stock, na.rm=TRUE),
                  co2_stock_tCO2 = sum(.data$co2_stock, na.rm=TRUE),.groups = 'drop')
      
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomasS_ton","carbon_stock_tC","co2_stock_tCO2"))
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      bm_temp <- bm_temp %>% rename("plot_id"= "data$표본점번호", "year"= "data$조사연도")
      
      # 플롯별
      bm_temp <- bm_temp %>% 
        group_by(plot_id, year, stand) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')

      
      weight_year <- bm_temp %>% 
        group_by(year) %>% 
        summarise(plot_num_year = n(),.groups = 'drop')
      
      
      weight_stand <- bm_temp %>% 
        group_by(year, stand) %>% 
        summarise(plot_num_stand = n(),.groups = 'drop')
      
      
      # Double sampling for post-stratification(forest stand)
      weight_DSS <- full_join(weight_stand, weight_year, by =c("year"))
      weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
      
      
      # 기본계획구(p)내 표본층(h)의 ha당 평균 임목자원량
      bm_temp_DSS <- bm_temp %>% 
        group_by(year, stand) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("year", "stand"))
      
      
      condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp_DSS[condition] <- 
        lapply(bm_temp_DSS[condition], function(x) (x/bm_temp_DSS$plot_num_stand)*bm_temp_DSS$weight_DSS)
      
      
      
      bm_temp_WMA <- bm_temp_DSS %>% 
        group_by(year) %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      # Weighted Moving Average(to combine annual inventory field data)
      weight_WMA <- weight_year
      plot_num_all <- sum(weight_WMA$plot_num_year)
      weight_WMA$weight_WMA <- weight_WMA$plot_num_year/plot_num_all
      
      
      bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("year"))
      
      
      condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      bm_temp_WMA[condition] <- 
        lapply(bm_temp_WMA[condition], function(x) x*bm_temp_WMA$weight_WMA )
      
      
      bm <- bm_temp_WMA %>% 
        summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
      
      
      
      
    }
    
  } 
  
  return(bm)
  
}

