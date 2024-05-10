#' summary_NFI Function
#'
#' This function 
#' @param data : data
#' @param grpby : 표본점번호
#' @param grpby2 : 표본점번호
#' @param byplot : byplot
#' @param clusterplot : byplot TRUE 집락 부
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @keywords plot
#' @return Descriptive_statistics
#' @export 
#' 
#' 


##

summary_NFI<- function(data, grpby=NULL, grpby2= NULL, byplot= FALSE, clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  

  # 경고
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  if (!is.null(grpby)){
    
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }}
  
  
  if (!is.null(grpby2)){
    
    if(!is.character(grpby2)) {
      stop("param 'grpby2' must be 'character'")
    }}
  
  
  
  # 전처리 
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  
  if (Stockedland){ #임목지
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  if(talltree){#수목형태구분
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
  }
  
  if(!largetreearea){ #대경목조사원내존재여부
    data$tree <- data$tree %>% filter(LARGEP_TREE == 0)
  }
  

  
  df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE",'INVYR', 'WDY_PLNTS_TYP_CD','CCL','CCLCD', 'SP',
                                'DBH', 'basal_area', 'HT_EST', 'VOL_EST', 'LARGEP_TREE', grpby2)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', 
                                "LAND_USE", "LAND_USECD", 'NONFR_INCL_AREA_LARGEP', 'NONFR_INCL_AREA_SUBP', grpby)], 
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE", "INVYR"))
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  grpby2  <- rlang::syms(grpby2)
  
  df$tree_area <- 0.04 - ((df$NONFR_INCL_AREA_SUBP*10)/10000)
  df$largetree_area <- 0.08 - ((df$NONFR_INCL_AREA_LARGEP*10)/10000)
  df$largetree <- ifelse(df$DBH>=30, 1, 0)

  
  
  
  
  
  
  # 대상지 또는 표본점별 개체수 우세목수고 등
  
  if(!byplot){ # 대상지별 개체수 우세목수고 등
    
    stat_num <- df %>%
      group_by(CYCLE, !!!grpby, !!!grpby2) %>%
      summarise(num_clusterplot= n_distinct(CLST_PLOT),
                num_subplot= n_distinct(SUB_PLOT),
                num_largetree_subplot= n_distinct(SUB_PLOT[LARGEP_TREE==1]),
                num_dbh30_subplot= n_distinct(SUB_PLOT[largetree==1]),
                num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(CCLCD=="a", na.rm=TRUE), 
                num_species= n_distinct(SP), .groups = 'drop')

    
    stat_mean <- df %>% 
      group_by(CYCLE, !!plot_id, !!!grpby, !!!grpby2) %>% 
      summarise(mean_DBH_temp = mean(DBH, na.rm=TRUE), 
                mean_H_temp = mean(HT_EST, na.rm=TRUE),
                mean_dominant_H_temp = mean(HT_EST[CCLCD=="a"], na.rm=TRUE),
                .groups = 'drop')
    
     
    stat_mean <- stat_mean %>% 
      group_by(CYCLE, !!!grpby, !!!grpby2) %>% 
      summarise(mean_DBH = mean(mean_DBH_temp, na.rm=TRUE), 
                se_DBH =  plotrix::std.error(mean_DBH_temp, na.rm=TRUE),
                mean_H = mean(mean_H_temp, na.rm=TRUE),
                se_H =  plotrix::std.error(mean_H_temp, na.rm=TRUE),
                mean_dominant_H = mean(mean_dominant_H_temp, na.rm=TRUE),
                se_dominant_H =  plotrix::std.error(mean_dominant_H_temp, na.rm=TRUE),
                .groups = 'drop')
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('CYCLE', as.character(unlist(lapply(grpby, quo_name))),
                                                     as.character(unlist(lapply(grpby2, quo_name))))) 
    
    
  }else{ # 표본점별 개체수 우세목수고 등
    

    stat_num <- df %>%
      group_by(CYCLE, !!plot_id, INVYR, !!!grpby, !!!grpby2) %>%
      summarise(num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(CCLCD=="a", na.rm=TRUE), 
                num_species= n_distinct(SP), .groups = 'drop')
    
      
    stat_mean <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, !!!grpby, !!!grpby2) %>% 
      summarise(mean_DBH = mean(DBH, na.rm=TRUE), 
                mean_H = mean(HT_EST, na.rm=TRUE),
                mean_dominant_H = mean(HT_EST[CCLCD=="a"], na.rm=TRUE),
                .groups = 'drop')
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('CYCLE', quo_name(plot_id), 'INVYR', as.character(unlist(lapply(grpby, quo_name))), 
                                                     as.character(unlist(lapply(grpby2, quo_name))))) ##grpby 확인
    
  }
  
  
  ## 집락 또는 부표본점별 ha당 개체수 등 
  
  if(clusterplot){ # 집락표본점별 ha당 개체수 등
    
    
    plot_area <- df[-which(duplicated(df$SUB_PLOT)), c('CYCLE', 'INVYR', 'CLST_PLOT', 'SUB_PLOT', 'largetree_area', 'tree_area')]
    
    plot_area <- plot_area %>%
      group_by(CYCLE, !!plot_id,  INVYR) %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    
    stat_ha <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, !!!grpby, !!!grpby2) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(VOL_EST, na.rm=TRUE),
                .groups = 'drop')
    
    
    stat_ha <- full_join(stat_ha, plot_area, by=c('CYCLE', 'INVYR', quo_name(plot_id))) #, as.character(unlist(lapply(grpby, quo_name)))
    
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    
    if(!largetreearea){ ## 집락표본점별 ha당 개체수 등 -대경목조사원 제외
      
      
      condition_ha <- c("tree_n_ha","basal_m2_ha","volume_m3_ha")
      stat_ha[condition_ha] <-  NA
      stat_ha <- as.data.frame(stat_ha)
      
      condition_ha <- (names(stat_ha) %in% c("tree_n_ha","basal_m2_ha","volume_m3_ha"))
      
      stat_ha[condition_ha] <- 
        lapply(stat_ha[condition], function(x) (x/stat_ha$tree_area))
      
      
      stat_ha[condition] <- NULL
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      
      
      
      
      
    }else{ ## 집락표본점별 ha당 개체수 등 -대경목조사원 포함 
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2) %>% 
        summarise(tree_n_ha = sum(tree_temp, na.rm=TRUE),
                  basal_m2_ha = sum(basal_temp, na.rm=TRUE),
                  volume_m3_ha = sum(volume_temp, na.rm=TRUE),.groups = 'drop')
    }
    
    
  }else{ ## 부표본점별 ha당 개체수 등
    
    stat_ha <- df %>% 
      group_by(CYCLE, !!plot_id, INVYR, largetree, largetree_area, tree_area, !!!grpby, !!!grpby2) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(VOL_EST, na.rm=TRUE),
                .groups = 'drop')
    
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    if(!largetreearea){## 부표본점별 ha당 개체수 등 -대경목조사원 제외
      
      condition_ha <- c("tree_n_ha","basal_m2_ha","volume_m3_ha")
      stat_ha[condition_ha] <-  NA
      stat_ha <- as.data.frame(stat_ha)
      
      condition_ha <- (names(stat_ha) %in% c("tree_n_ha","basal_m2_ha","volume_m3_ha"))
      
      stat_ha[condition_ha] <- 
        lapply(stat_ha[condition], function(x) (x/stat_ha$tree_area))
      
      
      stat_ha[condition] <- NULL
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      
      
      
    }else{ ## 부표본점별 ha당 개체수 등 -대경목조사원 포함
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(CYCLE, INVYR, !!plot_id, !!!grpby, !!!grpby2) %>% 
        summarise(tree_n_ha = sum(tree_temp, na.rm=TRUE),
                  basal_m2_ha = sum(basal_temp, na.rm=TRUE),
                  volume_m3_ha = sum(volume_temp, na.rm=TRUE),.groups = 'drop')
    }
    
  }
  
  
  
  
  
  ##  대상지 또는 표본점별 개체수 우세목수고 등과 집락 또는 부표본점별 ha당 개체수 등 join 
  if(!byplot){
    
    stat_ha <- stat_ha %>% 
      group_by(CYCLE, !!!grpby, !!!grpby2) %>% 
      summarise(mean_tree_n_ha = mean(tree_n_ha, na.rm=TRUE),
                se_tree_n_ha =  plotrix::std.error(tree_n_ha, na.rm=TRUE),
                mean_basal_m2_ha = mean(basal_m2_ha, na.rm=TRUE),
                se_basal_m2_ha =  plotrix::std.error(basal_m2_ha, na.rm=TRUE),
                mean_volume_m3_ha = mean(volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha =  plotrix::std.error(volume_m3_ha, na.rm=TRUE),.groups = 'drop')
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('CYCLE', as.character(unlist(lapply(grpby, quo_name))), 
                                                    as.character(unlist(lapply(grpby2, quo_name)))))
    
    
  }else{
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('CYCLE', quo_name(plot_id), 'INVYR', as.character(unlist(lapply(grpby, quo_name))), 
                                                    as.character(unlist(lapply(grpby2, quo_name))))) 
  }
  
  

  
  
  
  
  return(stat_data)
  
} 




