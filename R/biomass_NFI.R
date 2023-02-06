
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
    data$'수종명' =="일본잎갈나무" ~ "일본잎갈나무" ,
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
#' @param strat : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param clusterplot : byplot TRUE 집락 
#' @keywords biomass
#' @return biomass
#' @export 


biomass_NFI <- function(data, byplot= FALSE, grpby=NULL, strat="stand_subplot", clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  
  if (!is.null(grpby)){
    if(grpby==strat){
      stop("param 'grpby' is the same as param 'strat'")
    }
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }
    #if(byplot){
    #  warning("param 'grpby' has priority over param 'byplot'")
    #}
    
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
    if(byplot){
      warning("param 'byplot' has priority over param 'strat'")
    }
    
  }
  
  
  df <- left_join(data$tree[,c('집락번호', '표본점번호',"조사차기", '수목형태구분','수종명', 
                                'type_leaf', 'type_ever_g', '흉고직경', '추정간재적',  '대경목조사원내존재여부')], 
                  data$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도', '토지이용',
                               '기본조사원 비산림면적', '대경목조사원 비산림면적', strat, grpby)],
                  by = c("집락번호", "표본점번호", "조사차기"))
  
  
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(df$'추정간재적')){
    df$'추정간재적' <- as.numeric(df$'추정간재적')
  } 
  
  
  if (Stockedland){
    df <- df %>% filter(df$'토지이용' == "임목지")
  }
  
  if(talltree){
    df <- df %>% filter(df$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    df <- df %>% filter(df$'대경목조사원내존재여부' == 0)
  }else{
    df$largetree <- ifelse(df$'흉고직경'>=30, 1, 0)
    df$largetree_area <- 0.08 - ((df$'대경목조사원 비산림면적'*10)/10000) # 단위 m2/10
  }
  
  
  
  
  df$tree_area <- 0.04 - ((df$'기본조사원 비산림면적'*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- '집락번호'
  }else{
    plot_id <- '표본점번호'
  }
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  strat<- rlang::sym(strat)
  
  if(!largetreearea){
    largetree <- NULL
  }
  
  if(byplot){
    strat <- NULL
  }
  
  
  
  if(clusterplot){
    
    plot_area <- df[-which(duplicated(df[c('표본점번호', '조사차기')])),c('조사차기', '조사연도', '집락번호', '표본점번호', 'largetree_area', 'tree_area')]
    
    # plot_area_temp <- df %>% 
    #   group_by(df$'조사차기', df$'표본점번호', df$'조사연도') %>% 
    #   summarise(largetree  = sum(largetree, na.rm=TRUE),.groups = 'drop')
    # 
    # plot_area_temp <- plot_area_temp %>% rename('조사차기' = "df$조사차기", '표본점번호' = "df$표본점번호", "조사연도"= "df$조사연도") 
    # 
    # 
    # plot_area <- left_join(plot_area, plot_area_temp, by = c("조사차기", "조사연도", "표본점번호"))
    # 
    # plot_area[plot_area$largetree == 0, 'largetree_area'] <- 0
    
    plot_area <- plot_area %>%
      group_by(plot_area$'조사차기', !!plot_id,  plot_area$'조사연도') %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    
    plot_area <- plot_area %>% rename('order' = "plot_area$조사차기",  "year"= "plot_area$조사연도") 
    
    
    
    # 대경목 조사원 + 집락
    bm_temp <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', largetree, !!!grpby, !!strat) %>% 
      summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    
    bm_temp <- bm_temp %>% rename('order' = "df$조사차기",  "year"= "df$조사연도")
    
    bm_temp <- full_join(bm_temp, plot_area, by=c('order', 'year', quo_name(plot_id)))
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    
    if(!largetreearea){
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetreearea <- NULL
      
      
    }else{
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{
    
    
    # 대경목 조사원 (0.08ha + 0.04ha)
    bm_temp <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', !!strat, largetree, !!!grpby, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    bm_temp <- bm_temp %>% rename('order' = "df$조사차기", "year"= "df$조사연도")
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    if(!largetreearea){
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetreearea <- NULL
      
      
      
    }else{
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  if(!byplot){
    
    weight_grpby <- bm_temp %>% 
      group_by(order, !!!grpby) %>% 
      summarise(plot_num_all = n(),.groups = 'drop')
    
    
    weight_year <- bm_temp %>% 
      group_by(order, year, !!!grpby) %>% 
      summarise(plot_num_year = n(),.groups = 'drop')
    
    
    weight_stand <- bm_temp %>% 
      group_by(order, year, !!strat, !!!grpby) %>% 
      summarise(plot_num_stand = n(),.groups = 'drop')
    
    
    # Double sampling for post-strat(forest stand)
    weight_DSS <- full_join(weight_stand, weight_year, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))
    weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
    
    
    # 기본계획구(p)내 표본층(h)의 ha당 평균 임목자원량
    bm_temp_DSS <- bm_temp %>% 
      group_by(order, year, !!strat, !!!grpby) %>% 
      summarise(var_volume_m3_ha =  var(volume_m3_ha, na.rm=TRUE),
                volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                var_biomass_ton_ha =  var(biomass_ton_ha, na.rm=TRUE),
                biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha =  var(AG_biomass_ton_ha, na.rm=TRUE),
                AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha =  var(carbon_stock_tC_ha, na.rm=TRUE),
                carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha =  var(co2_stock_tCO2_ha, na.rm=TRUE),
                co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
    
    
    bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("order", "year", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
    
    
    condition_DSS <- c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha")
    bm_temp_DSS[condition_DSS] <-  NA
    bm_temp_DSS <- as.data.frame(bm_temp_DSS)
    
    condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
    
    bm_temp_DSS[condition] <- 
      lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
    
    condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha"))
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_biomass_ton_ha","var_AG_biomass_ton_ha","var_carbon_stock_tC_ha","var_co2_stock_tCO2_ha"))
    bm_temp_DSS[condition_var] <- 
      lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(order, year, !!!grpby) %>% 
      summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
                w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
                w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
                w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
    
    bm_temp_DSS[condition_DSS] <-  NULL
    bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))
    
    
    
    ## na <- 0 ????
    
    bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
    
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(order, year, !!!grpby) %>% 
      summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
                biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
                var_biomass_ton_ha = sum(var_biomass_ton_ha, na.rm=TRUE),
                AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha = sum(var_AG_biomass_ton_ha, na.rm=TRUE),
                carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha = sum(var_carbon_stock_tC_ha, na.rm=TRUE),
                co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha = sum(var_co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
    
    
    # var2_volume_m3_ha = sum(weight_DSS*(volume_m3_ha-w_volume_m3_ha)^2/plot_num_year),
    
    
    # Weighted Moving Average(to combine annual inventory field data)
    weight_WMA <- full_join(weight_year, weight_grpby, by =c("order", as.character(unlist(lapply(grpby, quo_name)))))
    weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
    
    
    bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("order","year", as.character(unlist(lapply(grpby, quo_name)))))
    
    condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
    
    
    bm_temp_WMA[condition] <- 
      lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
    
    
    bm <- bm_temp_WMA %>% 
      group_by(order, !!!grpby) %>% 
      summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha = sqrt(var_volume_m3_ha),
                rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
                biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
                var_biomass_ton_ha = sum(weight_WMA^2*var_biomass_ton_ha, na.rm=TRUE),
                se_biomass_ton_ha = sqrt(var_biomass_ton_ha),
                rse_biomass_ton_ha = se_biomass_ton_ha/biomass_ton_ha*100,
                AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
                var_AG_biomass_ton_ha = sum(weight_WMA^2*var_AG_biomass_ton_ha, na.rm=TRUE),
                se_AG_biomass_ton_ha = sqrt(var_AG_biomass_ton_ha),
                rse_AG_biomass_ton_ha = se_AG_biomass_ton_ha/AG_biomass_ton_ha*100,
                carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
                var_carbon_stock_tC_ha = sum(weight_WMA^2*var_carbon_stock_tC_ha, na.rm=TRUE),
                se_carbon_stock_tC_ha = sqrt(var_carbon_stock_tC_ha),
                rse_carbon_stock_tC_ha = se_carbon_stock_tC_ha/carbon_stock_tC_ha*100,
                co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),
                var_co2_stock_tCO2_ha = sum(weight_WMA^2*var_co2_stock_tCO2_ha, na.rm=TRUE),
                se_co2_stock_tCO2_ha = sqrt(var_co2_stock_tCO2_ha),
                rse_co2_stock_tCO2_ha = se_co2_stock_tCO2_ha/co2_stock_tCO2_ha*100,.groups = 'drop')
    
    
    
    
    
  }else{
    
    bm <- bm_temp
    
  }
  
  
  
  
  
  
  
  return(bm)
  
}



#' biomass_tsvis() Function
#'
#' This function 
#' @param data : data
#' @param grpby : grpby
#' @param strat : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param clusterplot : byplot TRUE 집락 
#' @keywords biomass


biomass_tsvis <- function(data, grpby=NULL, strat="stand_subplot", clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  
  if (!is.null(grpby)){
    if(grpby==strat){
      stop("param 'grpby' is the same as param 'strat'")
    }
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }
    
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
    
  }
  
  
  df <- left_join(data$tree[,c('집락번호', '표본점번호',"조사차기", '수목형태구분','수종명', 
                               'type_leaf', 'type_ever_g', '흉고직경', '추정간재적',  '대경목조사원내존재여부')], 
                  data$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도', '토지이용',
                               '기본조사원 비산림면적', '대경목조사원 비산림면적', strat, grpby)],
                  by = c("집락번호", "표본점번호", "조사차기"))
  
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(df$'추정간재적')){
    df$'추정간재적' <- as.numeric(df$'추정간재적')
  } 
  
  
  if (Stockedland){
    df <- df %>% filter(df$'토지이용' == "임목지")
  }
  
  if(talltree){
    df <- df %>% filter(df$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    df <- df %>% filter(df$'대경목조사원내존재여부' == 0)
  }else{
    df$largetree <- ifelse(df$'흉고직경'>=30, 1, 0)
    df$largetree_area <- 0.08 - ((df$'대경목조사원 비산림면적'*10)/10000) # 단위 m2/10
  }
  
  
  
  
  df$tree_area <- 0.04 - ((df$'기본조사원 비산림면적'*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- '집락번호'
  }else{
    plot_id <- '표본점번호'
  }
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  strat<- rlang::sym(strat)
  
  if(!largetreearea){
    largetree <- NULL
  }
  
  
  if(clusterplot){
    
    plot_area <- df[-which(duplicated(df[c('표본점번호', '조사차기')])),c('조사차기', '조사연도', '집락번호', '표본점번호', 'largetree_area', 'tree_area')]
    
    plot_area <- plot_area %>%
      group_by(plot_area$'조사차기', !!plot_id,  plot_area$'조사연도') %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    
    plot_area <- plot_area %>% rename('order' = "plot_area$조사차기",  "year"= "plot_area$조사연도") 
    
    
    
    # 대경목 조사원 + 집락
    bm_temp <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', largetree, !!!grpby, !!strat) %>% 
      summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    
    bm_temp <- bm_temp %>% rename('order' = "df$조사차기",  "year"= "df$조사연도")
    
    bm_temp <- full_join(bm_temp, plot_area, by=c('order', 'year', quo_name(plot_id)))
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    
    if(!largetreearea){
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetreearea <- NULL
      
      
    }else{
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{
    
    
    # 대경목 조사원 (0.08ha + 0.04ha)
    bm_temp <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', !!strat, largetree, !!!grpby, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(get('추정간재적'), na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    bm_temp <- bm_temp %>% rename('order' = "df$조사차기", "year"= "df$조사연도")
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    if(!largetreearea){
      
      
      condition_ha <- c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha")
      bm_temp[condition_ha] <-  NA
      bm_temp <- as.data.frame(bm_temp)
      
      condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
      
      bm_temp[condition_ha] <- 
        lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
      
      
      condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
      
      
      bm_temp[condition] <- NULL
      bm_temp$tree_area <- NULL
      bm_temp$largetreearea <- NULL
      
      
    }else{
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  
  weight_grpby <- bm_temp %>% 
    group_by(!!!grpby) %>% 
    summarise(plot_num_all = n(),.groups = 'drop')
  
  
  weight_year <- bm_temp %>% 
    group_by(order, year, !!!grpby) %>% 
    summarise(plot_num_year = n(),.groups = 'drop')
  
  
  weight_stand <- bm_temp %>% 
    group_by(order, year, !!strat, !!!grpby) %>% 
    summarise(plot_num_stand = n(),.groups = 'drop')
  
  
  # Double sampling for post-strat(forest stand)
  weight_DSS <- full_join(weight_stand, weight_year, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))
  weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
  
  
  # 기본계획구(p)내 표본층(h)의 ha당 평균 임목자원량
  bm_temp_DSS <- bm_temp %>% 
    group_by(order, year, !!strat, !!!grpby) %>% 
    summarise(var_volume_m3_ha =  var(volume_m3_ha, na.rm=TRUE),
              volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
              var_biomass_ton_ha =  var(biomass_ton_ha, na.rm=TRUE),
              biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha =  var(AG_biomass_ton_ha, na.rm=TRUE),
              AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha =  var(carbon_stock_tC_ha, na.rm=TRUE),
              carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha =  var(co2_stock_tCO2_ha, na.rm=TRUE),
              co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
  
  
  bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("order", "year", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
  
  
  condition_DSS <- c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha")
  bm_temp_DSS[condition_DSS] <-  NA
  bm_temp_DSS <- as.data.frame(bm_temp_DSS)
  
  condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
  
  bm_temp_DSS[condition] <- 
    lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
  
  condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_biomass_ton_ha","w_AG_biomass_ton_ha","w_carbon_stock_tC_ha","w_co2_stock_tCO2_ha"))
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_biomass_ton_ha","var_AG_biomass_ton_ha","var_carbon_stock_tC_ha","var_co2_stock_tCO2_ha"))
  bm_temp_DSS[condition_var] <- 
    lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(order, year, !!!grpby) %>% 
    summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
              w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
              w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
              w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
  
  bm_temp_DSS[condition_DSS] <-  NULL
  bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))
  
  
  
  ## na <- 0 ????
  
  bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
  
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(order, year, !!!grpby) %>% 
    summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
              biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
              var_biomass_ton_ha = sum(var_biomass_ton_ha, na.rm=TRUE),
              AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha = sum(var_AG_biomass_ton_ha, na.rm=TRUE),
              carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha = sum(var_carbon_stock_tC_ha, na.rm=TRUE),
              co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha = sum(var_co2_stock_tCO2_ha, na.rm=TRUE),.groups = 'drop')
  
  
  # var2_volume_m3_ha = sum(weight_DSS*(volume_m3_ha-w_volume_m3_ha)^2/plot_num_year),
  
  
  # Weighted Moving Average(to combine annual inventory field data)
  weight_WMA <- full_join(weight_year, weight_grpby, by =c(as.character(unlist(lapply(grpby, quo_name)))))
  weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
  
  
  bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("order","year", as.character(unlist(lapply(grpby, quo_name)))))
  
  condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
  
  
  bm_temp_WMA[condition] <- 
    lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
  
  
  bm <- bm_temp_WMA %>% 
    group_by(!!!grpby) %>% 
    summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
              se_volume_m3_ha = sqrt(var_volume_m3_ha),
              rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
              biomass_ton_ha = sum(biomass_ton_ha, na.rm=TRUE),
              var_biomass_ton_ha = sum(weight_WMA^2*var_biomass_ton_ha, na.rm=TRUE),
              se_biomass_ton_ha = sqrt(var_biomass_ton_ha),
              rse_biomass_ton_ha = se_biomass_ton_ha/biomass_ton_ha*100,
              AG_biomass_ton_ha = sum(AG_biomass_ton_ha, na.rm=TRUE),
              var_AG_biomass_ton_ha = sum(weight_WMA^2*var_AG_biomass_ton_ha, na.rm=TRUE),
              se_AG_biomass_ton_ha = sqrt(var_AG_biomass_ton_ha),
              rse_AG_biomass_ton_ha = se_AG_biomass_ton_ha/AG_biomass_ton_ha*100,
              carbon_stock_tC_ha = sum(carbon_stock_tC_ha, na.rm=TRUE),
              var_carbon_stock_tC_ha = sum(weight_WMA^2*var_carbon_stock_tC_ha, na.rm=TRUE),
              se_carbon_stock_tC_ha = sqrt(var_carbon_stock_tC_ha),
              rse_carbon_stock_tC_ha = se_carbon_stock_tC_ha/carbon_stock_tC_ha*100,
              co2_stock_tCO2_ha = sum(co2_stock_tCO2_ha, na.rm=TRUE),
              var_co2_stock_tCO2_ha = sum(weight_WMA^2*var_co2_stock_tCO2_ha, na.rm=TRUE),
              se_co2_stock_tCO2_ha = sqrt(var_co2_stock_tCO2_ha),
              rse_co2_stock_tCO2_ha = se_co2_stock_tCO2_ha/co2_stock_tCO2_ha*100,.groups = 'drop')
  
  
  
  
  return(bm)
  
}


