#' summary_NFI Function
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

summary_NFI<- function(data, grpby=NULL, grpby2= NULL, byplot= FALSE, clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  if(clusterplot){
    plot_id <- c('집락번호')
  }else{
    plot_id <- c('표본점번호')
  }
  
  
  if (!is.null(grpby)){
    
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }}
  
  
  if (!is.null(grpby2)){
    
    if(!is.character(grpby2)) {
      stop("param 'grpby2' must be 'character'")
    }}
  
  
  
  df <- left_join(data$tree[, c('집락번호', '표본점번호',"조사차기", '수목형태구분','수관급', '수종명', 
                                '흉고직경', 'basal_area', '추정수고', '추정간재적', '대경목조사원내존재여부', grpby2)], 
                  data$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도', 
                                "토지이용",'대경목조사원 비산림면적', '기본조사원 비산림면적', grpby)], 
                  by = c("집락번호", "표본점번호", "조사차기"))
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  grpby2  <- rlang::syms(grpby2)

  
  if (!is.null(grpby)){
    
    temp_grpby <- df %>%
      group_by(df$'조사차기', !!!grpby) %>%
      summarise(num_clusterplot = n_distinct(!!plot_id), .groups = "keep")
    
    temp_grpby <- temp_grpby %>% rename("order"= "df$조사차기")
    
    
    temp_grpby <- temp_grpby %>%
      group_by(order) %>%
      summarise(num_clusterplot = sum(num_clusterplot))
    
    
    temp_all <- df %>%
      group_by(df$'조사차기') %>%
      summarise(num_clusterplot = n_distinct(!!plot_id))
    
    
    if (any(temp_grpby$num_clusterplot != temp_all$num_clusterplot)){
      warning("plots have many grpby attributes.")
      
    }
    
  }
  
  
  if (Stockedland){
    df <- df %>% filter(df$'토지이용' == "임목지")
  }
  
  if(talltree){
    df <- df %>% filter(df$'수목형태구분' == "교목")
  }
  
  if(largetreearea){
    df$largetree_area <- 0.08 - ((df$'대경목조사원 비산림면적'*10)/10000)
    df$largetree <- ifelse(df$'흉고직경'>=30, 1, 0)
    
  }else{
    
    df <- df %>% filter(df$'대경목조사원내존재여부' == 0)
  }
  
  df$tree_area <- 0.04 - ((df$'기본조사원 비산림면적'*10)/10000)
  

  
  if(!largetreearea){
    largetree <- NULL
  }
  
  
  
  if(!byplot){
    
    
    stat_num <- df %>%
      group_by(df$'조사차기', !!!grpby, !!!grpby2) %>%
      summarise(num_clusterplot= n_distinct(get('집락번호')),
                num_subplot= n_distinct(get('표본점번호')),
                num_largetree_subplot= n_distinct(get('표본점번호')[get('대경목조사원내존재여부')==1]),
                num_dbh30_subplot= n_distinct(get('표본점번호')[largetree==1]),
                num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(get('수관급')=="우세목", na.rm=TRUE), 
                num_species= n_distinct(get('수종명')), .groups = 'drop')
    
    stat_num <- stat_num %>% rename("order"= "df$조사차기")
    
    
    
    stat_mean <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', !!!grpby, !!!grpby2) %>% 
      summarise(mean_DBH_temp = mean(get('흉고직경'), na.rm=TRUE), 
                mean_H_temp = mean(get('추정수고'), na.rm=TRUE),
                mean_dominant_H_temp = mean(get('추정수고')[get('수관급')=="우세목"], na.rm=TRUE),
                .groups = 'drop')
    
    stat_mean <- stat_mean %>% rename("order"= "df$조사차기", "year"= "df$조사연도")
    
    
    stat_mean <- stat_mean %>% 
      group_by(order, !!!grpby, , !!!grpby2) %>% 
      summarise(mean_DBH = mean(mean_DBH_temp, na.rm=TRUE), 
                se_DBH =  plotrix::std.error(mean_DBH_temp, na.rm=TRUE),
                mean_H = mean(mean_H_temp, na.rm=TRUE),
                se_H =  plotrix::std.error(mean_H_temp, na.rm=TRUE),
                mean_dominant_H = mean(mean_dominant_H_temp, na.rm=TRUE),
                se_dominant_H =  plotrix::std.error(mean_dominant_H_temp, na.rm=TRUE),
                .groups = 'drop')
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('order', as.character(unlist(lapply(grpby, quo_name))))) 
    
    
  }else{
    
    stat_num <- df %>%
      group_by(df$'조사차기', !!plot_id, df$'조사연도', !!!grpby, !!!grpby2) %>%
      summarise(num_tree = n(),
                num_largetree = sum(largetree, na.rm=TRUE),
                num_dominanttree = sum(get('수관급')=="우세목", na.rm=TRUE), 
                num_species= n_distinct(get('수종명')), .groups = 'drop')
    
    stat_num <- stat_num %>% rename("order"= "df$조사차기",  "year"= "df$조사연도")
    
    
    
    stat_mean <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', !!!grpby, !!!grpby2) %>% 
      summarise(mean_DBH = mean(get('흉고직경'), na.rm=TRUE), 
                mean_H = mean(get('추정수고'), na.rm=TRUE),
                mean_dominant_H = mean(get('추정수고')[get('수관급')=="우세목"], na.rm=TRUE),
                .groups = 'drop')
    
    stat_mean <- stat_mean %>% rename("order"= "df$조사차기",  "year"= "df$조사연도")
    
    
    stat_temp <- full_join(stat_num, stat_mean, by=c('order', quo_name(plot_id), 'year')) ##grpby 확인
    
  }
  
  
  if(clusterplot){
    
    
    plot_area <- df[-which(duplicated(df$'표본점번호')),c('조사차기', '조사연도', '집락번호', '표본점번호', 'largetree_area', 'tree_area')]
    
    plot_area <- plot_area %>%
      group_by(plot_area$'조사차기', !!plot_id,  plot_area$'조사연도') %>%
      summarise(largetree_area = sum(largetree_area, na.rm=TRUE),
                tree_area= sum(tree_area, na.rm=TRUE),.groups = 'drop')
    
    
    plot_area <- plot_area %>% rename('order' = "plot_area$조사차기",  "year"= "plot_area$조사연도") 
    
    stat_ha <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', largetree, !!!grpby, !!!grpby2) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(get('추정간재적'), na.rm=TRUE),
                .groups = 'drop')
    
    stat_ha <- stat_ha %>% rename('order' = "df$조사차기",  "year"= "df$조사연도")  
    
    
    stat_ha <- full_join(stat_ha, plot_area, by=c('order', 'year', quo_name(plot_id)))
    
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    
    if(!largetreearea){
      
      
      condition_ha <- c("tree_n_ha","basal_m2_ha","volume_m3_ha")
      stat_ha[condition_ha] <-  NA
      stat_ha <- as.data.frame(stat_ha)
      
      condition_ha <- (names(stat_ha) %in% c("tree_n_ha","basal_m2_ha","volume_m3_ha"))
      
      stat_ha[condition_ha] <- 
        lapply(stat_ha[condition], function(x) (x/stat_ha$tree_area))
      
      
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      stat_ha[condition] <- NULL
      
      
      
      
    }else{
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!!grpby2) %>% 
        summarise(tree_n_ha = sum(tree_temp, na.rm=TRUE),
                  basal_m2_ha = sum(basal_temp, na.rm=TRUE),
                  volume_m3_ha = sum(volume_temp, na.rm=TRUE),.groups = 'drop')
    }
    
    
  }else{ 
    
    stat_ha <- df %>% 
      group_by(df$'조사차기', !!plot_id, df$'조사연도', largetree, largetree_area, tree_area, !!!grpby, !!!grpby2) %>% 
      summarise(tree_temp = n(), 
                basal_temp= sum(basal_area, na.rm=TRUE),
                volume_temp= sum(get('추정간재적'), na.rm=TRUE),
                .groups = 'drop')
    
    stat_ha <- stat_ha %>% rename('order' = "df$조사차기",  "year"= "df$조사연도")
    condition <- (names(stat_ha) %in% c("tree_temp","basal_temp","volume_temp"))
    
    
    if(!largetreearea){
      
      condition_ha <- c("tree_n_ha","basal_m2_ha","volume_m3_ha")
      stat_ha[condition_ha] <-  NA
      stat_ha <- as.data.frame(stat_ha)
      
      condition_ha <- (names(stat_ha) %in% c("tree_n_ha","basal_m2_ha","volume_m3_ha"))
      
      stat_ha[condition_ha] <- 
        lapply(stat_ha[condition], function(x) (x/stat_ha$tree_area))
      
      
      stat_ha$tree_area <- NULL
      stat_ha$largetreearea <- NULL
      stat_ha[condition] <- NULL
      
      
    }else{
      
      stat_ha[condition] <- lapply(stat_ha[condition], function(x) ifelse(stat_ha$largetree == 1, 
                                                                          x/(stat_ha$largetree_area),
                                                                          x/(stat_ha$tree_area)))
      
      stat_ha <- stat_ha %>% 
        group_by(order, year, !!plot_id, !!!grpby, !!!grpby2) %>% 
        summarise(tree_n_ha = sum(tree_temp, na.rm=TRUE),
                  basal_m2_ha = sum(basal_temp, na.rm=TRUE),
                  volume_m3_ha = sum(volume_temp, na.rm=TRUE),.groups = 'drop')
    }
    
  }
  
  
  
  
  
  
  
  
  if(!byplot){
    
    stat_ha <- stat_ha %>% 
      group_by(order, !!!grpby, !!!grpby2) %>% 
      summarise(mean_tree_n_ha = mean(tree_n_ha, na.rm=TRUE),
                se_tree_n_ha =  plotrix::std.error(tree_n_ha, na.rm=TRUE),
                mean_basal_m2_ha = mean(basal_m2_ha, na.rm=TRUE),
                se_basal_m2_ha =  plotrix::std.error(basal_m2_ha, na.rm=TRUE),
                mean_volume_m3_ha = mean(volume_m3_ha, na.rm=TRUE),
                se_volume_m3_ha =  plotrix::std.error(volume_m3_ha, na.rm=TRUE),.groups = 'drop')
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('order', as.character(unlist(lapply(grpby, quo_name))), 
                                                    as.character(unlist(lapply(grpby2, quo_name)))))
    
    
  }else{
    
    stat_data <- full_join(stat_temp, stat_ha, by=c('order', quo_name(plot_id), 'year')) ##grpby 확인
  }
  
  
  stat_data <- stat_data %>% rename("조사차기"= "order")
  
  
  
  
  return(stat_data)
  
} 




