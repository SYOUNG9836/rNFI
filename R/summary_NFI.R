#'Descriptive_statistics Function
#'
#' This function 
#' @param data : data
#' @param grpby : 표본점번호
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param byplot : byplot
#' @param clusterplot : byplot TRUE 집락 부 
#' @keywords plot
#' @return Descriptive_statistics
#' @export 
#' 
#' 


##

summary_NFI<- function(data, grpby=NULL, byplot= FALSE, clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  if (!is.null(grpby)){
    
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }
    #if(byplot){
    #  warning("param 'grpby' has priority over param 'byplot'")
    #}
    
    temp_grpby <- data %>%
      group_by(data$'조사차기', !!!grpby) %>%
      summarise(num_clusterplot = n_distinct(!!plot_id), .groups = "keep")
    
    temp_grpby <- temp_grpby %>% rename("order"= "data$조사차기")
    
    
    temp_grpby <- temp_grpby %>%
      group_by(order) %>%
      summarise(num_clusterplot = sum(num_clusterplot))
    
    
    temp_all <- data %>%
      group_by(data$'조사차기') %>%
      summarise(num_clusterplot = n_distinct(!!plot_id))
    
    
    if (any(temp_grpby$num_clusterplot != temp_all$num_clusterplot)){
      warning("plots have many grpby attributes.")
      
    }
    
  }
  
  
  if (Stockedland){
    data <- data %>% filter(data$'토지이용' == "임목지")
  }
  
  if(talltree){
    data <- data %>% filter(data$'수목형태구분' == "교목")
  }
  
  if(largetreearea){
    data$largetree_area <- 0.08 - ((data$'대경목조사원 비산림면적'*10)/10000)
    data$largetree <- ifelse(data$'흉고직경'>=30, 1, 0)
    
  }else{
    
    data <- data %>% filter(data$'대경목조사원내존재여부' == 0)
  }
  
  data$tree_area <- 0.04 - ((data$'기본조사원 비산림면적'*10)/10000)
  
  
  if(clusterplot){
    plot_id <- c('집락번호')
  }else{
    plot_id <- c('표본점번호')
  }
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  
  if(!largetreearea){
    largetree <- NULL
  }
  
  
  
  if(!byplot){
    
    
    stat_num <- data %>%
      group_by(data$'조사차기', !!!grpby) %>%
      summarise(num_clusterplot= n_distinct(get('집락번호')),
                num_subplot= n_distinct(get('표본점번호')),
                num_largetree_subplot= n_distinct(get('표본점번호')[get('대경목조사원내존재여부')==1]),
                num_dbh30_subplot= n_distinct(get('표본점번호')[largetree==1]),
                num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(get('수관급')=="우세목", na.rm=TRUE), 
                num_species= n_distinct(get('수종명')), .groups = 'drop')
    
    stat_num <- stat_num %>% rename("order"= "data$조사차기")
    
    
    
    stat_mean <- data %>% 
      group_by(data$'조사차기', !!plot_id, data$'조사연도', !!!grpby) %>% 
      summarise(mean_DBH_temp = mean(get('흉고직경'), na.rm=TRUE), 
                mean_H_temp = mean(get('추정수고'), na.rm=TRUE),
                mean_dominant_H_temp = mean(get('추정수고')[get('수관급')=="우세목"], na.rm=TRUE),
                .groups = 'drop')
    
    stat_mean <- stat_mean %>% rename("order"= "data$조사차기", "year"= "data$조사연도")
    
    
    stat_mean <- stat_mean %>% 
      group_by(order, !!!grpby) %>% 
      summarise(mean_DBH = mean(mean_DBH_temp, na.rm=TRUE), 
                se_DBH =  plotrix::std.error(mean_DBH_temp, na.rm=TRUE),
                mean_H = mean(mean_H_temp, na.rm=TRUE),
                se_H =  plotrix::std.error(mean_H_temp, na.rm=TRUE),
                mean_dominant_H = mean(mean_dominant_H_temp, na.rm=TRUE),
                se_dominant_H =  plotrix::std.error(mean_dominant_H_temp, na.rm=TRUE),
                .groups = 'drop')
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('order', as.character(unlist(lapply(grpby, quo_name))))) 
    
    
  }else{
    
    stat_num <- data %>%
      group_by(data$'조사차기', !!plot_id, data$'조사연도', !!!grpby) %>%
      summarise(num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(get('수관급')=="우세목", na.rm=TRUE), 
                num_species= n_distinct(get('수종명')), .groups = 'drop')
    
    stat_num <- stat_num %>% rename("order"= "data$조사차기",  "year"= "data$조사연도")
    
    
    
    stat_mean <- data %>% 
      group_by(data$'조사차기', !!plot_id, data$'조사연도', !!!grpby) %>% 
      summarise(mean_DBH = mean(get('흉고직경'), na.rm=TRUE), 
                mean_H = mean(get('추정수고'), na.rm=TRUE),
                mean_dominant_H = mean(get('추정수고')[get('수관급')=="우세목"], na.rm=TRUE),
                .groups = 'drop')
    
    stat_mean <- stat_mean %>% rename("order"= "data$조사차기",  "year"= "data$조사연도")
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('order', quo_name(plot_id), 'year')) ##grpby 확인
    
  }

  
  if(clusterplot){
    
    
    data_area <- data[-which(duplicated(data$'표본점번호')),c('조사차기', '조사연도', '집락번호', '표본점번호', 'largetree_area', 'tree_area')]
    
    data_area <- data_area %>%
      group_by(data_area$'조사차기', !!plot_id,  data_area$'조사연도') %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    
    data_area <- data_area %>% rename('order' = "data_area$조사차기",  "year"= "data_area$조사연도") 
    
    stat_ha <- data %>% 
      group_by(data$'조사차기', !!plot_id, data$'조사연도', largetree, !!!grpby) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(get('추정간재적'), na.rm=TRUE),
                .groups = 'drop')
    
    stat_ha <- stat_ha %>% rename('order' = "data$조사차기",  "year"= "data$조사연도")  
    
    
    stat_ha <- full_join(stat_ha, data_area, by=c('order', 'year', quo_name(plot_id)))
    
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    
    if(!largetreearea){
      
      stat_ha[condition] <- stat_ha[condition]/stat_ha$tree_area
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      
    }else{
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(order, year, !!plot_id, !!!grpby) %>% 
        summarise(tree_n_ha = sum(tree_n_ha, na.rm=TRUE),
                  basal_m2_ha = sum(basal_m2_ha, na.rm=TRUE),
                  volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),.groups = 'drop')
    }
    
    
  }else{ 
    
    stat_ha <- data %>% 
      group_by(data$'조사차기', !!plot_id, data$'조사연도', largetree, largetree_area, tree_area, !!!grpby) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(get('추정간재적'), na.rm=TRUE),
                .groups = 'drop')

    stat_ha <- stat_ha %>% rename('order' = "data$조사차기",  "year"= "data$조사연도")
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    if(!largetreearea){
      
      stat_ha[condition] <- stat_ha[condition]/stat_ha$tree_area
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      
    }else{
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(order, year, !!plot_id, !!!grpby) %>% 
        summarise(tree_n_ha = sum(tree_n_ha, na.rm=TRUE),
                  basal_m2_ha = sum(basal_m2_ha, na.rm=TRUE),
                  volume_m3_ha = sum(volume_m3_ha, na.rm=TRUE),.groups = 'drop')
    }
    
  }
  
  
  
  

 


  if(!byplot){
    
    stat_ha <- stat_ha %>% 
      group_by(order, !!!grpby) %>% 
      summarise(mean_tree_n_ha = mean(tree_n_ha, na.rm=TRUE),
                se_tree_n_ha =  plotrix::std.error(tree_n_ha, na.rm=TRUE),
                mean_basal_m2_ha = mean(basal_m2_ha, na.rm=TRUE),
                se_basal_m2_ha =  plotrix::std.error(basal_m2_ha, na.rm=TRUE),
                mean_volume_m3_ha = mean(volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha =  plotrix::std.error(volume_m3_ha, na.rm=TRUE),.groups = 'drop')
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('order', as.character(unlist(lapply(grpby, quo_name)))))
    
    
  }else{
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('order', quo_name(plot_id), 'year')) ##grpby 확인
  }
    
  
 
  
  
  

  return(stat_data)
  
} 




