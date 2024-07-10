
#' bm_df()
#' 
#' @description
#' bm_df() is a function that calculates the biomass of individual trees using species-specific national emission factors.
#' It is an internal function used within the biomass_NFI() function.
#'
#' @param data : data
#' @noRd
 
## Calculates biomass --------------------------------------------------------------
## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)*(44/12)-----------------
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)*(1+뿌리함량비)*(탄소전환계수)*(44/12)-----------------
bm_df <- function(data){
  
  
  output <- data  %>% mutate(species_bm = case_when(
    
    
    ##species-specific
    ## Pinus densiflora in Gangwondo(Yeongju-si, Bonghwa-gun, Uljin-gun, Yeongyang-gun, Gangwon-do)
    ## 강원지방소나무(영주시, 봉화군, 울진군, 영양군, 강원도) --------------------------------------------------------------
    (SPCD =="14994" & ( SGG_CD == 47210 |  SGG_CD == 47920 |  SGG_CD == 47930 | SGG_CD == 47760 | SIDO_CD == 42 )) 
    ~ "14994_GW" , 
    
    SPCD =="14994" ~ "14994", # Pinus densiflora (중부지방소나무)
    SPCD =="14964" ~ "14964" , # Larix kaempferi (일본잎갈나무)
    SPCD =="14987" ~ "14987", # Pinus rigida (리기다소나무)
    SPCD =="15003" ~ "15003", # Pinus thunbergii (곰솔)
    SPCD =="14973" ~ "14973", # Pinus koraiensis (잣나무)
    SPCD =="15014" ~ "15014" , # Cryptomeria japonica (삼나무)
    SPCD =="14973" ~ "14973" , # Chamaecyparis obtusa (편백)
    SPCD =="6617" ~ "6617" , # Quercus variabilis (굴참나무)
    SPCD =="6556" ~ "6556" , # Quercus mongolica (신갈나무)
    SPCD =="6512" ~ "6512" , # Quercus acutissima (상수리나무)
    SPCD =="6591" ~ "6591" , # Quercus serrata (졸참나무)
    SPCD =="6505" ~ "6505" , # Quercus acuta (붉가시나무)
    SPCD =="1959" ~ "1959" , # Robinia pseudoacacia (아까시나무)
    SPCD =="895" ~ "895" , # Betula pendula (자작나무)
    SPCD =="11588" ~ "11588" , # Liriodendron tulipifera (백합나무)
    SPCD =="19592" ~ "19592" , # Populus × tomentiglandulosa (은사시나무)
    SPCD =="6476" ~ "6476" , # Castanea crenata (밤나무)
    
    (DECEVER_CD == 1) ~ "EVERDEC" , # Other evergreen broad-leaved species
    (CONDEC_CLASS_CD ==1) ~ "OTHER_DEC" , # Other deciduous species
    (CONDEC_CLASS_CD ==0) ~ "OTHER_CON", # Other conifer species
    TRUE ~ as.character(NA)
    
    # Bambusoideae (대나무) ?? 
    
  ))
  

  output <- left_join(output, bio_coeff, by= c("species_bm" ="SPCD") )
  
  ## Calculating aboveground biomass--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)-------------------------------
  output$AG_biomass <- (output$VOL_EST)*(output$wood_density)*(output$biomass_expan)
  ## Calculating biomass--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)-----------------
  output$T_biomass <- output$AG_biomass*(1+output$root_shoot_ratio)
  ## Calculating carbon storage-----------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)-----------------------
  output$CF <- ifelse(output$CONDEC_CLASS_CD ==1, 0.48, 0.51 ) # Carbon fraction 0.51 (coniferous) or 0.48 (broadleaf)
  output$carbon_stock <- output$T_biomass*output$CF
  ## Calculating carbon dioxide storage--------------------------------------------------------------
  ## species ~ Volume*(Wood density)*(Biomass expansion factor)*(1+Root to shoot ratio)*(Carbon fraction)*(44/12)--------------------
  output$co2_stock = output$carbon_stock*(44/12)
  
  
  return(output)
  
}







#' Calculates biomass of tree
#' 
#' @description
#' biomass_NFI() is a function that estimates volume, aboveground biomass, biomass, carbon storage and carbon dioxide storage for each plot, the entire study area, or groups within the study area.
#' When calculating biomass for individual trees and plots, Users can specify whether to include large tree survey plots, to focus only on tall trees and Stocked land, to treat cluster plots as single plots.
#' Users can choose the criteria for post-stratification and can also group results by plot or tree characteristics.
#' 
#' @details
#' Biomass is calculated using methodologies employed for national statistics. 
#' It applies national carbon emission factors to calculate biomass at the individual tree level. 
#' To estimate biomass per hectare at the cluster or subplot level, Users can choose whether to include only basic survey trees or both basic survey trees and large tree survey trees.
#' The function uses the calculated per hectare biomass at the plot level to derive annual statistics, applying the Double Sampling for Post-stratification (DSS) method. 
#' Since only 20% of the total plots are surveyed each year, the derived annual statistics are integrated into a single time point. 
#' The Weighted Moving Average (WMA) method, which assigns weights to the proportion of plots collected over time, is used to calculate these statistics. 
#' The function also estimates the variance, standard error, and relative standard error of the estimated mean values.
#'
#' @param data : A `list` generated by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.
#' @param byplot : A logical flag; whether to calculate for each plot separately or for the entire dataset.
#' @param grpby : A character vector; variables from 'plot' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param grpby2 : A character vector; variables from 'tree' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param strat : A character vector; the variable used for post-stratification. In the National Forest Inventory of Korea, it is typically used by forest type.
#' @param clusterplot : A logical flag; whether to calculate for cluster plot collectively or calculate for each subplot separately.
#' @param largetreearea : A logical flag; whether to include a large tree plot as well, or only a tree plot.
#' @param Stockedland : A logical flag; whether to include only stocked land or also include other types of land.
#' @param talltree : A logical flag; whether to include only tall trees or also shrubs.
#' 
#' @return A `data.frame` that includes biomass for plot or study areas
#' 
#' @examples
#' \dontrun{
#' biomass_NFI(NFI5, grpby="map")
#' }
#' 
#' @references 
#' Son, Y., Kim, R., Lee, K., Pyo, J., Kim, S., Hwang, J., Lee, S., & Park, H. (2014). Carbon emission factors and biomass allometric equations by species in Korea. Korea Forest Research Institute.
#' Yim, J., Moon, G., Lee, M., Kang, J., Won, M., Ahn, E., & Jeon, J. (2021). 2020 Forest inventory of Korea. Korea Forest Research Institute.
#' 
#' @export 


biomass_NFI <- function(data, byplot= FALSE, grpby=NULL, grpby2= NULL, strat="FORTYP_SUB", clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  
  ## error message-------------------------------------------------------------- 
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  
  if (clusterplot){
    if(strat=="FORTYP_SUB"){
      warning("When the param 'clusterplot' is set to TRUE, param 'strat' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
    
      strat <- c("FORTYP_CLST")
    }
  }
  
  
  
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
  
  
  ## Preprocessing-------------------------------------------------------------- 
  if (Stockedland){ 
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  if(talltree){
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
  }
  
   
  df <- left_join(data$tree[,c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD','SP', 'SPCD',
                                'CONDEC_CLASS_CD', 'DECEVER_CD', 'DBH', 'VOL_EST',  'LARGEP_TREE', grpby2)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD",
                               'NONFR_INCL_AREA_SUBP', 'NONFR_INCL_AREA_LARGEP', "SGG_CD", 'SIDO_CD', strat, grpby)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))

  if (!is.numeric(df$VOL_EST)){
    df$VOL_EST <- as.numeric(df$VOL_EST)
  } 
  
  
  
  if(!largetreearea){ 
    df <- df %>% filter(df$LARGEP_TREE == 0)
  }else{
    df$largetree <- ifelse(df$DBH>=30, 1, 0)
    df$largetree_area <- 0.08 - ((df$NONFR_INCL_AREA_LARGEP*10)/10000) # unit m2/10
  }
  
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  strat<- rlang::sym(strat)
  grpby2  <- rlang::syms(grpby2)
  
  if(!largetreearea){
    largetree <- NULL
  }
  if(byplot){
    strat <- NULL
  }
  
  
  # 1.Biomass calculation by cluster or subplot
  if(clusterplot){ #  1.1 Biomass calculation by cluster plots
    
    plot_area <- df[-which(duplicated(df[c('SUB_PLOT', 'CYCLE')])),c('CYCLE', 'INVYR', 'CLST_PLOT', 'SUB_PLOT', 'largetree_area', 'tree_area')]
    
    plot_area <- plot_area %>%
      group_by(CYCLE, !!plot_id, INVYR) %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, !!!grpby, !!!grpby2, !!strat) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    bm_temp <- full_join(bm_temp, plot_area, by=c('CYCLE', 'INVYR', quo_name(plot_id)))
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    
    if(!largetreearea){ # 1.1.1 Biomass calculation by cluster plots excluding large tree survey plots 
      
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
      
      
    }else{ # 1.1.2 Biomass calculation by cluster plots including large tree survey plots 
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{ # 1.2 Biomass calculation by subplots 
    
    
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, !!strat, largetree, !!!grpby, !!!grpby2, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    

    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    if(!largetreearea){ # 1.2.1 Biomass calculation by subplots excluding large tree survey plots  
      
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
      
      
      
    }else{ # 1.2.2 Biomass calculation by subplots including large tree survey plots  
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  if(!byplot){ # 2.1 Biomass calculation using post-stratified double sampling and weighted moving average methods
    
    # 2.1.1 Double sampling for post-strat(by forest stand)
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
    

    # 2.1.2 Aggregating calculated biomass per plot by forest type.
    bm_temp_DSS <- bm_temp %>% 
      group_by(CYCLE, INVYR, !!strat, !!!grpby, !!!grpby2) %>% 
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
    
    
    bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
    
    
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
      group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
      summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
                w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
                w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
                w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
                w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
    
    
    # 2.1.3 Aggregating calculated biomass per forest type by study area.
    bm_temp_DSS[condition_DSS] <-  NULL
    bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name))),
                                                             as.character(unlist(lapply(grpby2, quo_name)))))
    
    bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
    bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
    
    bm_temp_DSS[condition_DSS] <- 
      lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
    
    bm_temp_WMA <- bm_temp_DSS %>% 
      group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
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
    
    
    # 2.1.4 Weighted Moving Average(to combine annual inventory field data)
    weight_WMA <- full_join(weight_year, weight_grpby, by =c(as.character(unlist(lapply(grpby, quo_name)))))
    weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
    
    
    bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(grpby, quo_name)))))
    
    condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
    
    
    bm_temp_WMA[condition] <- 
      lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
    
    
    bm <- bm_temp_WMA %>% 
      group_by(CYCLE, !!!grpby, !!!grpby2) %>% 
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
    
    
    
    
    
  }else{ # 2.2 Biomass calculation by plot
    
    bm <- bm_temp
    
  }
  
  
  
  
  
  
  
  return(bm)
  
}



#' biomass_tsvis()
#' 
#' @description
#' biomass_tsvis() is a function that
#' It is an internal function used within the tsvis_NFI() function.
#' biomass_NFI() calculates biomass by cycle, while this function calculates biomass by 5 year.
#'
#' @param data : A `list` generated by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.
#' @param grpby : A character vector; variables from 'plot' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param grpby2 : A character vector; variables from 'tree' tables for grouping. Use \code{c()} to combine multiple variables.
#' @param strat : A character vector; the variable used for post-stratification. In the National Forest Inventory of Korea, it is typically used by forest type.
#' @param clusterplot : A logical flag; whether to calculate for cluster plot collectively or calculate for each subplot separately.
#' @param largetreearea : A logical flag; whether to include a large tree plot as well, or only a tree plot.
#' @param Stockedland : A logical flag; whether to include only stocked land or also include other types of land.
#' @param talltree : A logical flag; whether to include only tall trees or also shrubs.
#' 
#' @noRd



biomass_tsvis <- function(data, grpby=NULL, grpby2=NULL, strat="FORTYP_SUB", clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  ## error message--------------------------------------------------------------  
  required_names <- c("plot", "tree")
  
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
  
  if (clusterplot){
    if(strat=="FORTYP_SUB"){
      warning("When the param 'clusterplot' is set to TRUE, param 'strat' uses FORTYP_CLST (the forest type for the cluster plot) instead of FORTYP_SUB (the forest type for each subplot).")
      
      strat <- c("FORTYP_CLST")
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
  
  
  
  ## Preprocessing-------------------------------------------------------------- 
  if (Stockedland){ 
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  if(talltree){
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
  }
  
  df <- left_join(data$tree[,c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD','SP', 'SPCD',
                               'CONDEC_CLASS_CD', 'DECEVER_CD', 'DBH', 'VOL_EST',  'LARGEP_TREE', grpby2 )], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD",
                               'NONFR_INCL_AREA_SUBP', 'NONFR_INCL_AREA_LARGEP', "SGG_CD", 'SIDO_CD', strat, grpby)],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  

  if (!is.numeric(df$VOL_EST)){
    df$VOL_EST <- as.numeric(df$VOL_EST)
  } 
  
  if(!largetreearea){ 
    df <- df %>% filter(df$LARGEP_TREE == 0)
  }else{
    df$largetree <- ifelse(df$DBH>=30, 1, 0)
    df$largetree_area <- 0.08 - ((df$NONFR_INCL_AREA_LARGEP*10)/10000) # unit m2/10
  }
  
  
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  
  df <- bm_df(df)
  
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  grpby2  <- rlang::syms(grpby2)
  strat<- rlang::sym(strat)
  
  if(!largetreearea){
    largetree <- NULL
  }
  
  
  # 1.Biomass calculation by cluster or subplot
  if(clusterplot){ # 1.1 Biomass calculation by cluster plots
    
    plot_area <- df[-which(duplicated(df[c('SUB_PLOT', 'CYCLE')])),c('CYCLE', 'INVYR', 'CLST_PLOT', 'SUB_PLOT', 'largetree_area', 'tree_area')]

    
    plot_area <- plot_area %>%
      group_by(CYCLE, !!plot_id,  INVYR) %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
 
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, !!!grpby, !!!grpby2, !!strat) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    
    bm_temp <- full_join(bm_temp, plot_area, by=c('CYCLE', 'INVYR', quo_name(plot_id)))
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    
    if(!largetreearea){ # 1.1.1 Biomass calculation by cluster plots excluding large tree survey plots
      
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
      
      
    }else{ # 1.1.2 Biomass calculation by cluster plots including large tree survey plots
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
    
    
    
    
    
    
  }else{ # 1.2 Biomass calculation by subplots 
    
    bm_temp <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, !!strat, largetree, !!!grpby, !!!grpby2, largetree_area, tree_area) %>% 
      summarise(volume_m3 = sum(VOL_EST, na.rm=TRUE),
                biomass_ton = sum(T_biomass, na.rm=TRUE),
                AG_biomass_ton = sum(AG_biomass, na.rm=TRUE),
                carbon_stock_tC = sum(carbon_stock, na.rm=TRUE),
                co2_stock_tCO2 = sum(co2_stock, na.rm=TRUE),.groups = 'drop')
    
    
    condition <- (names(bm_temp) %in% c("volume_m3","biomass_ton","AG_biomass_ton","carbon_stock_tC","co2_stock_tCO2"))
    
    if(!largetreearea){ # 1.2.1 Biomass calculation by subplots excluding large tree survey plots
      
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
      
      
    }else{ # 1.2.2 Biomass calculation by subplots including large tree survey plots
      
      bm_temp[condition] <- 
        lapply(bm_temp[condition], function(x) ifelse(bm_temp$largetree == 1, 
                                                      x/(bm_temp$largetree_area),
                                                      x/(bm_temp$tree_area)))
      
      bm_temp <- bm_temp %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2, !!strat) %>% 
        summarise(volume_m3_ha = sum(volume_m3, na.rm=TRUE),
                  biomass_ton_ha = sum(biomass_ton, na.rm=TRUE),
                  AG_biomass_ton_ha = sum(AG_biomass_ton, na.rm=TRUE),
                  carbon_stock_tC_ha = sum(carbon_stock_tC, na.rm=TRUE),
                  co2_stock_tCO2_ha = sum(co2_stock_tCO2, na.rm=TRUE),.groups = 'drop')
    }
  }
  
  # 2.1 Biomass calculation using post-stratified double sampling and weighted moving average methods
  # 2.1.1 Double sampling for post-strat(forest stand)
  weight_grpby <- data$plot %>%  # not CYCLE
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
  
  
  # 2.1.2 Aggregating calculated biomass per plot by forest type.
  bm_temp_DSS <- bm_temp %>% 
    group_by(CYCLE, INVYR, !!strat, !!!grpby, !!!grpby2) %>% 
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
  
  
  bm_temp_DSS <- full_join(bm_temp_DSS, weight_DSS, by =c("CYCLE", "INVYR", quo_name(strat), as.character(unlist(lapply(grpby, quo_name)))))
  
  
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
    group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
    summarise(w_volume_m3_ha = sum(w_volume_m3_ha, na.rm=TRUE),
              w_biomass_ton_ha = sum(w_biomass_ton_ha, na.rm=TRUE),
              w_AG_biomass_ton_ha = sum(w_AG_biomass_ton_ha, na.rm=TRUE),
              w_carbon_stock_tC_ha = sum(w_carbon_stock_tC_ha, na.rm=TRUE),
              w_co2_stock_tCO2_ha = sum(w_co2_stock_tCO2_ha, na.rm=TRUE), .groups = 'drop')
  
  
  # 2.1.3 Aggregating calculated biomass per forest type by study area.
  bm_temp_DSS[condition_DSS] <-  NULL
  bm_temp_DSS <- left_join(bm_temp_DSS, bm_temp_WMA, by =c("CYCLE", "INVYR", as.character(unlist(lapply(grpby, quo_name))),
                                                           as.character(unlist(lapply(grpby2, quo_name)))))
  
  bm_temp_DSS$var_volume_m3_ha <- bm_temp_DSS$var_volume_m3_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$volume_m3_ha-bm_temp_DSS$w_volume_m3_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_biomass_ton_ha <- bm_temp_DSS$var_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$biomass_ton_ha-bm_temp_DSS$w_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_AG_biomass_ton_ha <- bm_temp_DSS$var_AG_biomass_ton_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$AG_biomass_ton_ha-bm_temp_DSS$w_AG_biomass_ton_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_carbon_stock_tC_ha <- bm_temp_DSS$var_carbon_stock_tC_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$carbon_stock_tC_ha-bm_temp_DSS$w_carbon_stock_tC_ha)^2/bm_temp_DSS$plot_num_year)
  bm_temp_DSS$var_co2_stock_tCO2_ha <- bm_temp_DSS$var_co2_stock_tCO2_ha + (bm_temp_DSS$weight_DSS*(bm_temp_DSS$co2_stock_tCO2_ha-bm_temp_DSS$w_co2_stock_tCO2_ha)^2/bm_temp_DSS$plot_num_year)
  
  bm_temp_DSS[condition_DSS] <- 
    lapply(bm_temp_DSS[condition], function(x) (x*bm_temp_DSS$weight_DSS))
  
  
  bm_temp_WMA <- bm_temp_DSS %>% 
    group_by(CYCLE, INVYR, !!!grpby, !!!grpby2) %>% 
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
  
  
  # 2.1.4 Weighted Moving Average(to combine annual inventory field data)
  weight_WMA <- full_join(weight_year, weight_grpby, by =c(as.character(unlist(lapply(grpby, quo_name)))))
  weight_WMA$weight_WMA <- weight_WMA$plot_num_year/weight_WMA$plot_num_all
  
  
  bm_temp_WMA <- full_join(bm_temp_WMA, weight_WMA, by =c("CYCLE","INVYR", as.character(unlist(lapply(grpby, quo_name)))))
  
  condition <- (names(bm_temp_WMA) %in% c("volume_m3_ha","biomass_ton_ha","AG_biomass_ton_ha","carbon_stock_tC_ha","co2_stock_tCO2_ha"))
  
  
  bm_temp_WMA[condition] <- 
    lapply(bm_temp_WMA[condition], function(x) (x*bm_temp_WMA$weight_WMA))
  
  
  bm <- bm_temp_WMA %>% 
    group_by(!!!grpby, !!!grpby2) %>% 
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


