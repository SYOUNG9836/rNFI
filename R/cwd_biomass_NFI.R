#' @description
#' cwd_bm_df() is a function that
#' 
#' @param data : data

## biomass 구하기--------------------------------------------------------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)-----------------
cwd_bm_df <- function(data){
  
  
  output <- data  %>% mutate(species_bm = case_when(
    
    ## bio_coeff = 국가고유배출계수, 
    ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
    ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
    

    data$SPCD =="14994" ~ "14994", #소나무
    data$SPCD =="14964" ~ "14964" , #일본잎갈나무
    data$SPCD =="14987" ~ "14987", #리기다나무
    data$SPCD =="14973" ~ "14973", #잣나무
    data$SPCD =="6617" ~ "6617" , #굴참나무
    data$SPCD =="6556" ~ "6556" , #신갈나무
    data$SPCD =="6512" ~ "6512" , #상수리나무
    
    (data$CONDEC_CLASS_CD ==1) ~ "OTHER_DEC" , # 기타활엽수
    (data$CONDEC_CLASS_CD ==0) ~ "OTHER_CON",  #기타 침엽수
    TRUE ~ as.character(NA)
    
  ))
  
  ## bio_coeff = 국가고유배출계수, 
  ## 출처 : "탄소배출계수를 활용한 국가 온실가스 통계 작성", 
  ## "NIFoS 산림정책이슈 제129호 : 주요 산림수종의 표준 탄소흡수량(ver.1.2)"
  output <- left_join(output, cwd_bio_coeff, by= c("species_bm" ="SPCD", "DECAYCD" = "decay_class"))
  
  ## 지상부 biomass 구하기--------------------------------------------------------------
  ## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)-------------------------------
  output$carbon_stock <- (output$VOL)*(output$wood_density)*(output$cf)
  
  return(output)
  
}







#' @description
#' cwd_biomass_NFI() is a function that
#' 
#' @details
#'
#'
#' @param data : A `list` produced by \code{\link{read_NFI}} that contains 'plot' and 'cwd' data frames.
#' @param byplot : byplot
#' @param grpby : grpby
#' @param grpby2 : grpby2
#' @param strat : 흉고단면적/개체수
#' @param Stockedland : A logical value indicating whether to include only stocked land or also include other types of land.

#' @return cwd tree biomass
#' @export 


cwd_biomass_NFI <- function(data, byplot= FALSE, grpby=NULL, grpby2=NULL, strat="FORTYP_SUB", Stockedland=TRUE){
  
  
  #경고
  required_names <- c("plot", "cwd")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  if (!is.null(grpby)){
    if(grpby==strat){
      stop("param 'grpby' is the same as param 'strat'")
    }
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }
    # if(byplot){
    #  warning("param 'grpby' has priority over param 'byplot'")
    # }
  }
  
  if (!is.null(grpby2)){
    if(!is.character(grpby2)) {
      stop("param 'grpby2' must be 'character'")
    }
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
    if(byplot){
      warning("param 'byplot' has priority over param 'strat'")
    }
    
  }
  
  
  # 전처리
  if (Stockedland){ #임목지
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }

  df <- left_join(data$cwd[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'SP', 'SPCD','DECAYCD', 
                                'CONDEC_CLASS_CD', 'VOL', grpby2)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', 'LAND_USECD',
                               'NONFR_INCL_AREA_SUBP', strat, grpby)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  
  

  
  
  
  ## 추정간재적 type이 num이 아닌 경우 as.numeric--------------------------------------------------------------
  if (!is.numeric(df$VOL)){
    df$VOL <- as.numeric(df$VOL)
  } 

  
  
  plot_id <- 'SUB_PLOT'
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  df <- cwd_bm_df(df)
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  grpby2  <- rlang::syms(grpby2)
  strat<- rlang::sym(strat)
  
  
  if(byplot){
    strat <- NULL
  }
  
  
  # 표본점별 고사량 계산
  bm_temp <- df %>% 
    group_by(CYCLE, !!plot_id, INVYR, !!strat, !!!grpby, !!!grpby2, tree_area) %>% 
    summarise(cwd_volume_m3 = sum(VOL, na.rm=TRUE),
              cwd_carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),.groups = 'drop')
  

  condition_ha <- c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha")
  bm_temp[condition_ha] <-  NA
  bm_temp <- as.data.frame(bm_temp)
  
  condition_ha <- (names(bm_temp) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
  condition <- (names(bm_temp) %in% c("cwd_volume_m3","cwd_carbon_stock_tC"))
  
  bm_temp[condition_ha] <- 
    lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
  
  
  condition <- (names(bm_temp) %in% c("cwd_volume_m3","cwd_carbon_stock_tC"))
  
  bm_temp[condition] <- NULL
  bm_temp$tree_area <- NULL
  
  
  
  
  if(!byplot){ # 사후층화이중추출법 및 가중이동평균 고사량 계산
    
    # Double sampling for post-strat(forest stand)
    weight_grpby <- data$plot %>% 
      group_by(CYCLE, !!!grpby) %>% 
      summarise(plot_num_all = n(),.groups = 'drop')
    
    
    weight_year <- data$plot %>% 
      group_by(CYCLE, INVYR, !!!grpby) %>% 
      summarise(plot_num_year = n(),.groups = 'drop')
    
    
    weight_stand <- data$plot %>% 
      group_by(CYCLE, INVYR, !!strat, !!!grpby) %>% 
      summarise(plot_num_stand = n(),.groups = 'drop')
    
    
    
    weight_DSS <- full_join(weight_stand, weight_year, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name)))))
    weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
    
    
    # plot to stand 고사량 계산
    bm_temp_DSS <- bm_temp %>% 
      group_by(CYCLE, INVYR, !!strat, !!!grpby, !!!grpby2) %>% 
      summarise(var_cwd_volume_m3_ha =  var(cwd_volume_m3_ha, na.rm=TRUE),
                cwd_volume_m3_ha = sum(cwd_volume_m3_ha, na.rm=TRUE),
                var_cwd_carbon_stock_tC_ha =  var(cwd_carbon_stock_tC_ha, na.rm=TRUE),
                cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    
    
    bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
    
    
    condition_DSS <- c("w_cwd_volume_m3_ha","w_cwd_carbon_stock_tC_ha")
    bm_temp_DSS[condition_DSS] <-  NA
    bm_temp_DSS <- as.data.frame(bm_temp_DSS)
    
    condition <- (names(bm_temp_DSS) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
    
    bm_temp_DSS[condition] <- 
      lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
    
    condition_DSS <- (names(bm_temp_DSS) %in% c("w_cwd_volume_m3_ha","w_cwd_carbon_stock_tC_ha"))
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    condition_var <- (names(bm_temp_DSS) %in% c("var_cwd_volume_m3_ha","var_cwd_carbon_stock_tC_ha"))
    bm_temp_DSS[condition_var] <- 
      lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
      summarise(w_cwd_volume_m3_ha = sum(w_cwd_volume_m3_ha, na.rm=TRUE),
                w_cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    
    
    # stand to study area 생물량 계산
    bm_temp_DSS[condition_DSS] <-  NULL
    bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name))),
                                                             as.character(unlist(lapply(grpby2, quo_name)))))

    bm_temp_DSS$var_cwd_volume_m3_ha <- bm_temp_DSS$var_cwd_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_volume_m3_ha-bm_temp_DSS$w_cwd_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_cwd_carbon_stock_tC_ha <- bm_temp_DSS$var_cwd_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_carbon_stock_tC_ha-bm_temp_DSS$w_cwd_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
    
    condition <- (names(bm_temp_DSS) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
    condition_DSS <- (names(bm_temp_DSS) %in% c("w_cwd_volume_m3_ha","w_cwd_carbon_stock_tC_ha"))
    
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
      summarise(cwd_volume_m3_ha = sum(w_cwd_volume_m3_ha, na.rm=TRUE),
                var_cwd_volume_m3_ha = sum(var_cwd_volume_m3_ha, na.rm=TRUE),
                cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE),
                var_cwd_carbon_stock_tC_ha = sum(var_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
    
    
    # Weighted Moving Average(to combine annual inventory field data)
    weight_WMA <- full_join(weight_year, weight_grpby, by =c("CYCLE", as.character(unlist(lapply(grpby, quo_name))))) 
    weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
    
    
    bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(grpby, quo_name)))))
    
    condition <- (names(bm_temp_WMA) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
    
    
    bm_temp_WMA[condition] <- 
      lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
    
    
    bm <- bm_temp_WMA %>% 
      group_by(CYCLE, !!!grpby, !!!grpby2) %>% 
      summarise(cwd_volume_m3_ha = sum(cwd_volume_m3_ha, na.rm=TRUE),
                var_cwd_volume_m3_ha = sum(weight_WMA^2*var_cwd_volume_m3_ha, na.rm=TRUE),
                se_cwd_volume_m3_ha = sqrt(var_cwd_volume_m3_ha),
                rse_cwd_volume_m3_ha = se_cwd_volume_m3_ha/cwd_volume_m3_ha*100,
                cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE),
                var_cwd_carbon_stock_tC_ha = sum(weight_WMA^2*var_cwd_carbon_stock_tC_ha, na.rm=TRUE),
                se_cwd_carbon_stock_tC_ha = sqrt(var_cwd_carbon_stock_tC_ha),
                rse_cwd_carbon_stock_tC_ha = se_cwd_carbon_stock_tC_ha/cwd_carbon_stock_tC_ha*100,.groups = 'drop')
    
    
    
    
    
  }else{ # 표본점별 고사량 계산
    
    bm <- bm_temp
    
  }
  
  
  
  
  
  
  
  return(bm)
  
}



#' @description
#' cwd_biomass_tsvis() is a function that
#'
#' @param data : A `list` produced by \code{\link{read_NFI}} that contains 'plot' and 'cwd' data frames.
#' @param grpby : grpby
#' @param strat : 흉고단면적/개체수
#' @param Stockedland : A logical value indicating whether to include only stocked land or also include other types of land.



cwd_biomass_tsvis <- function(data, grpby=NULL, strat="FORTYP_SUB", Stockedland=TRUE){
  
  
  # 경고
  
  required_names <- c("plot", "cwd")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  if (!is.null(grpby)){
    if(grpby==strat){
      stop("param 'grpby' is the same as param 'strat'")
    }
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }
  }
  
  if (!is.null(grpby2)){
    if(!is.character(grpby2)) {
      stop("param 'grpby2' must be 'character'")
    }
  }
  
  if (!is.null(strat)){
    if(!is.character(strat)) {
      stop("param 'strat' must be 'character'")
    }
  }
  
  
  # 전처리
  
  if (Stockedland){ #임목지
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  df <- left_join(data$cwd[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'SP', 'SPCD', 'DECAYCD', 
                               'CONDEC_CLASS_CD', 'VOL', grpby2)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', 'LAND_USECD',
                               'NONFR_INCL_AREA_SUBP', strat, grpby)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  
  

  if (!is.numeric(df$VOL)){
    df$VOL <- as.numeric(df$VOL)
  } 

  
  plot_id <- 'SUB_PLOT'
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  df <- cwd_bm_df(df)
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  grpby2  <- rlang::syms(grpby2)
  strat<- rlang::sym(strat)
  
  
  # 표본점별 고사량 계산
  bm_temp <- df %>% 
    group_by(CYCLE, !!plot_id, INVYR, !!strat, !!!grpby, !!!grpby2, tree_area) %>% 
    summarise(cwd_volume_m3 = sum(VOL, na.rm=TRUE),
              cwd_carbon_stock_tC = sum(carbon_stock, na.rm=TRUE), .groups = 'drop')
  
  condition <- (names(bm_temp) %in% c("cwd_volume_m3","cwd_carbon_stock_tC"))
  
  condition_ha <- c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha")
  bm_temp[condition_ha] <-  NA
  bm_temp <- as.data.frame(bm_temp)
  
  condition_ha <- (names(bm_temp) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  bm_temp[condition_ha] <- 
    lapply(bm_temp[condition], function(x) (x/bm_temp$tree_area))
  
  condition <- (names(bm_temp) %in% c("cwd_volume_m3","cwd_carbon_stock_tC"))
  
  bm_temp[condition] <- NULL
  bm_temp$tree_area <- NULL
  bm_temp$largetreearea <- NULL
  
  
  # 사후층화이중추출법 및 가중이동평균 고사량 계산
  # Double sampling for post-strat(forest stand)
  weight_grpby <- data$plot %>% 
    group_by(!!!grpby) %>% 
    summarise(plot_num_all = n(),.groups = 'drop')
  
  
  weight_year <- data$plot %>% 
    group_by(CYCLE, INVYR, !!!grpby) %>% 
    summarise(plot_num_year = n(),.groups = 'drop')
  
  
  weight_stand <- data$plot %>% 
    group_by(CYCLE, INVYR, !!strat, !!!grpby) %>% 
    summarise(plot_num_stand = n(),.groups = 'drop')
  
  weight_DSS <- full_join(weight_stand, weight_year, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name)))))
  weight_DSS$weight_DSS <- weight_DSS$plot_num_stand/weight_DSS$plot_num_year
  
  
  # plot to stand 고사량 계산
  bm_temp_DSS <- bm_temp %>% 
    group_by(CYCLE, INVYR, !!strat, !!!grpby, !!!grpby2) %>% 
    summarise(var_cwd_volume_m3_ha =  var(cwd_volume_m3_ha, na.rm=TRUE),
              cwd_volume_m3_ha = sum(cwd_volume_m3_ha, na.rm=TRUE),
              var_cwd_carbon_stock_tC_ha =  var(cwd_carbon_stock_tC_ha, na.rm=TRUE),
              cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  
  bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
  
  
  condition_DSS <- c("w_cwd_volume_m3_ha","w_cwd_carbon_stock_tC_ha")
  bm_temp_DSS[condition_DSS] <-  NA
  bm_temp_DSS <- as.data.frame(bm_temp_DSS)
  
  condition <- (names(bm_temp_DSS) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  bm_temp_DSS[condition] <- 
    lapply(bm_temp_DSS[condition], function(x) ((x/bm_temp_DSS$plot_num_stand)))
  
  condition_DSS <- (names(bm_temp_DSS) %in% c("w_cwd_volume_m3_ha","w_cwd_carbon_stock_tC_ha"))
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  condition_var <- (names(bm_temp_DSS) %in% c("var_cwd_volume_m3_ha","var_cwd_carbon_stock_tC_ha"))
  bm_temp_DSS[condition_var] <- 
    lapply(bm_temp_DSS[condition_var], function(x) ((bm_temp_DSS$weight_DSS)^2*(x/bm_temp_DSS$plot_num_stand)))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
    summarise(w_cwd_volume_m3_ha = sum(w_cwd_volume_m3_ha, na.rm=TRUE),
              w_cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  # stand to study area 고사량 계산
  bm_temp_DSS[condition_DSS] <-  NULL
  bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name))),
                                                           as.character(unlist(lapply(grpby2, quo_name)))))
  
  
  bm_temp_DSS$var_cwd_volume_m3_ha <- bm_temp_DSS$var_cwd_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_volume_m3_ha-bm_temp_DSS$w_cwd_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_cwd_carbon_stock_tC_ha <- bm_temp_DSS$var_cwd_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$cwd_carbon_stock_tC_ha-bm_temp_DSS$w_cwd_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
  
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
    summarise(cwd_volume_m3_ha = sum(w_cwd_volume_m3_ha, na.rm=TRUE),
              var_cwd_volume_m3_ha = sum(var_cwd_volume_m3_ha, na.rm=TRUE),
              cwd_carbon_stock_tC_ha = sum(w_cwd_carbon_stock_tC_ha, na.rm=TRUE),
              var_cwd_carbon_stock_tC_ha = sum(var_cwd_carbon_stock_tC_ha, na.rm=TRUE), .groups = 'drop')
  
  
  # Weighted Moving Average(to combine annual inventory field data)
  weight_WMA <- full_join(weight_year, weight_grpby, by =c(as.character(unlist(lapply(grpby, quo_name)))))
  weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
  
  
  bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(grpby, quo_name)))))
  
  condition <- (names(bm_temp_WMA) %in% c("cwd_volume_m3_ha","cwd_carbon_stock_tC_ha"))
  
  
  bm_temp_WMA[condition] <- 
    lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
  
  
  bm <- bm_temp_WMA %>% 
    group_by(!!!grpby, !!!grpby2) %>% 
    summarise(cwd_volume_m3_ha = sum(cwd_volume_m3_ha, na.rm=TRUE),
              var_cwd_volume_m3_ha = sum(weight_WMA^2*var_cwd_volume_m3_ha, na.rm=TRUE),
              se_cwd_volume_m3_ha = sqrt(var_cwd_volume_m3_ha),
              rse_cwd_volume_m3_ha = se_cwd_volume_m3_ha/cwd_volume_m3_ha*100,
              cwd_carbon_stock_tC_ha = sum(cwd_carbon_stock_tC_ha, na.rm=TRUE),
              var_cwd_carbon_stock_tC_ha = sum(weight_WMA^2*var_cwd_carbon_stock_tC_ha, na.rm=TRUE),
              se_cwd_carbon_stock_tC_ha = sqrt(var_cwd_carbon_stock_tC_ha),
              rse_cwd_carbon_stock_tC_ha = se_cwd_carbon_stock_tC_ha/cwd_carbon_stock_tC_ha*100,.groups = 'drop')
  

  return(bm)
  
}


