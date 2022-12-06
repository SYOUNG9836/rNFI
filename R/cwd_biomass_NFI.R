
## biomass 구하기--------------------------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)-----------------
cwd_bm_df <- function(data){
  
  
  output <- data  %>% mutate(species_bm = case_when(
    
    ## bio_coeff = 국가고유배출계수, 
    ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
    ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
    
    ## 강원지방소나무--------------------------------------------------------------
    data$'수종명' =="소나무" ~ "소나무",
    data$'수종명' =="일본잎갈나무" ~ "일본잎갈나무" ,
    data$'수종명' =="리기다소나무" ~ "리기다소나무",
    data$'수종명' =="잣나무" ~ "잣나무",
    data$'수종명' =="굴참나무" ~ "굴참나무" ,
    data$'수종명' =="신갈나무" ~ "신갈나무" ,
    data$'수종명' =="상수리나무" ~ "상수리나무" ,
    
    ## 기타 활엽수 및 기타 침엽수--------------------------------------------------------------
    (data$type_leaf =="활엽수") ~ "기타 활엽수" ,
    (data$type_leaf =="침엽수") ~ "기타 침엽수",
    TRUE ~ as.character(NA)
    
  ))
  
  ## bio_coeff = 국가고유배출계수, 
  ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
  ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
  output <- left_join(output, cwd_bio_coeff, by= c("species_bm" ="수종", "부후도코드" = "부후등급"))
  
  ## 지상부 biomass 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)-------------------------------
  output$carbon_stock <- (output$'재적')*(output$"목재기본밀도_값")*(output$"탄소전환계수")
  
  return(output)
  
}







#' cwd_biomass_NFI() Function
#'
#' This function 
#' @param data : data
#' @param byplot : byplot
#' @param grpby : grpby
#' @param strat : 흉고단면적/개체수
#' @param Stockedland : 임목지
#' @keywords cwd tree biomass
#' @return cwd tree biomass
#' @export 


cwd_biomass_NFI <- function(data, byplot= FALSE, grpby=NULL, strat="stand", Stockedland=TRUE){
  
  
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
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(data$'재적')){
    data$'재적' <- as.numeric(data$'재적')
  } 
  
  
  if (Stockedland){
    data <- data %>% filter(data$'토지이용' == "임목지")
  }

  
  data$tree_area <- 0.04 - ((data$'기본조사원 비산림면적'*10)/10000)
  
  data <- cwd_bm_df(data)
  
  plot_id <- '표본점번호'
  

  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  strat<- rlang::sym(strat)
  

  if(byplot){
    strat <- NULL
  }
  
  
  bm_temp <- data %>% 
    group_by(data$'조사차기', !!plot_id, data$'조사연도', !!strat, !!!grpby, tree_area) %>% 
    summarise(volume_m3 = sum(get('재적'), na.rm=TRUE),
              cwd_carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),.groups = 'drop')
  
  bm_temp <- bm_temp %>% rename('order' = "data$조사차기", "year"= "data$조사연도")
  
  condition <- (names(bm_temp) %in% c("volume_m3","cwd_carbon_stock_tC"))
  
  condition_ha <- c("volume_m3_ha","cwd_carbon_stock_tC_ha")
  bm_temp[condition_ha] <-  NA
  bm_temp <- as.data.frame(bm_temp)
  
  condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  bm_temp[condition_ha] <- 
    lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
  
  
  condition <- (names(bm_temp) %in% c("volume_m3","cwd_carbon_stock_tC"))
  
  bm_temp[condition] <- NULL
  bm_temp$tree_area <- NULL
  bm_temp$largetreearea <- NULL
  
  
 
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
                var_cwd_carbon_stock_tC_ha =  var(cwd_carbon_stock_tC_ha, na.rm=TRUE),
                cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    
    
    bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("order", "year", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
    
    
    condition_DSS <- c("w_volume_m3_ha","w_cwd_carbon_stock_tC_ha")
    bm_temp_DSS[condition_DSS] <-  NA
    bm_temp_DSS <- as.data.frame(bm_temp_DSS)
    
    condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
    
    bm_temp_DSS[condition] <- 
      lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
    
    condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_cwd_carbon_stock_tC_ha"))
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_cwd_carbon_stock_tC_ha"))
    bm_temp_DSS[condition_var] <- 
      lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(order, year, !!!grpby) %>% 
      summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                w_cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    
    bm_temp_DSS[condition_DSS] <-  NULL
    bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))
    
    
    
    ## na <- 0 ????
    
    bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_cwd_carbon_stock_tC_ha <- bm_temp_DSS$var_cwd_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_carbon_stock_tC_ha-bm_temp_DSS$w_cwd_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
   
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(order, year, !!!grpby) %>% 
      summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
                cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE),
                var_cwd_carbon_stock_tC_ha = sum(var_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    

    # Weighted Moving Average(to combine annual inventory field data)
    weight_WMA <- full_join(weight_year, weight_grpby, by =c("order", as.character(unlist(lapply(grpby, quo_name)))))
    weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
    
    
    bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("order","year", as.character(unlist(lapply(grpby, quo_name)))))
    
    condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
    
    
    bm_temp_WMA[condition] <- 
      lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
    
    
    bm <- bm_temp_WMA %>% 
      group_by(order, !!!grpby) %>% 
      summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
                var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha = sqrt(var_volume_m3_ha),
                rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
                cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE),
                var_cwd_carbon_stock_tC_ha = sum(weight_WMA^2*var_cwd_carbon_stock_tC_ha, na.rm=TRUE),
                se_cwd_carbon_stock_tC_ha = sqrt(var_cwd_carbon_stock_tC_ha),
                rse_cwd_carbon_stock_tC_ha = se_cwd_carbon_stock_tC_ha/cwd_carbon_stock_tC_ha*100,.groups = 'drop')
    
    
    
    
    
  }else{
    
    bm <- bm_temp
    
  }
  
  
  
  
  
  
  
  return(bm)
  
}



#' cwd_biomass_evaluate() Function
#'
#' This function 
#' @param data : data
#' @param grpby : grpby
#' @param strat : 흉고단면적/개체수
#' @param Stockedland : 임목지
#' @keywords biomass


cwd_biomass_evaluate <- function(data, grpby=NULL, strat="stand", Stockedland=TRUE){
  
  
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
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(data$'재적')){
    data$'재적' <- as.numeric(data$'재적')
  } 
  
  
  if (Stockedland){
    data <- data %>% filter(data$'토지이용' == "임목지")
  }
  
  
  data$tree_area <- 0.04 - ((data$'기본조사원 비산림면적'*10)/10000)
  
  data <- cwd_bm_df(data)
  
  plot_id <- '표본점번호'
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  strat<- rlang::sym(strat)
  
  
  
  bm_temp <- data %>% 
    group_by(data$'조사차기', !!plot_id, data$'조사연도', !!strat, !!!grpby, tree_area) %>% 
    summarise(volume_m3 = sum(get('재적'), na.rm=TRUE),
              cwd_carbon_stock_tC = sum(carbon_stock, na.rm=TRUE), .groups = 'drop')
  
  bm_temp <- bm_temp %>% rename('order' = "data$조사차기", "year"= "data$조사연도")
  
  condition <- (names(bm_temp) %in% c("volume_m3","cwd_carbon_stock_tC"))
  
  condition_ha <- c("volume_m3_ha","cwd_carbon_stock_tC_ha")
  bm_temp[condition_ha] <-  NA
  bm_temp <- as.data.frame(bm_temp)
  
  condition_ha <- (names(bm_temp) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  bm_temp[condition_ha] <- 
    lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
  
  condition <- (names(bm_temp) %in% c("volume_m3","cwd_carbon_stock_tC"))
  
  bm_temp[condition] <- NULL
  bm_temp$tree_area <- NULL
  bm_temp$largetreearea <- NULL
  
  
  
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
              var_cwd_carbon_stock_tC_ha =  var(cwd_carbon_stock_tC_ha, na.rm=TRUE),
              cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  
  bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("order", "year", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
  
  
  condition_DSS <- c("w_volume_m3_ha","w_cwd_carbon_stock_tC_ha")
  bm_temp_DSS[condition_DSS] <-  NA
  bm_temp_DSS <- as.data.frame(bm_temp_DSS)
  
  condition <- (names(bm_temp_DSS) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  bm_temp_DSS[condition] <- 
    lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
  
  condition_DSS <- (names(bm_temp_DSS) %in% c("w_volume_m3_ha","w_cwd_carbon_stock_tC_ha"))
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  condition_var <- (names(bm_temp_DSS) %in% c("var_volume_m3_ha","var_cwd_carbon_stock_tC_ha"))
  bm_temp_DSS[condition_var] <- 
    lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(order, year, !!!grpby) %>% 
    summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              w_cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  bm_temp_DSS[condition_DSS] <-  NULL
  bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("order", "year", as.character(unlist(lapply(grpby, quo_name)))))

  
  bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_cwd_carbon_stock_tC_ha <- bm_temp_DSS$var_cwd_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_carbon_stock_tC_ha-bm_temp_DSS$w_cwd_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
  
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(order, year, !!!grpby) %>% 
    summarise(volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(var_volume_m3_ha, na.rm=TRUE),
              cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE),
              var_cwd_carbon_stock_tC_ha = sum(var_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  
  # Weighted Moving Average(to combine annual inventory field data)
  weight_WMA <- full_join(weight_year, weight_grpby, by =c(as.character(unlist(lapply(grpby, quo_name)))))
  weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
  
  
  bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("order","year", as.character(unlist(lapply(grpby, quo_name)))))
  
  condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  
  bm_temp_WMA[condition] <- 
    lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
  
  
  bm <- bm_temp_WMA %>% 
    group_by(!!!grpby) %>% 
    summarise(volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),
              var_volume_m3_ha = sum(weight_WMA^2*var_volume_m3_ha, na.rm=TRUE),
              se_volume_m3_ha = sqrt(var_volume_m3_ha),
              rse_volume_m3_ha = se_volume_m3_ha/volume_m3_ha*100,
              cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE),
              var_cwd_carbon_stock_tC_ha = sum(weight_WMA^2*var_cwd_carbon_stock_tC_ha, na.rm=TRUE),
              se_cwd_carbon_stock_tC_ha = sqrt(var_cwd_carbon_stock_tC_ha),
              rse_cwd_carbon_stock_tC_ha = se_cwd_carbon_stock_tC_ha/cwd_carbon_stock_tC_ha*100,.groups = 'drop')
  
  
  

  
  
  
  
  
  return(bm)
  
}


