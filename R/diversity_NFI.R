#' diversity Function
#'
#' This function 
#' @param data : data
#' @param grpby : grpby
#' @param basal : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param byplot : byplot
#' @param clusterplot : byplot TRUE 집락 부 
#' @keywords plot
#' @return diversity
#' @export 
#' 
#' 


##  

diversity_NFI <- function(data, grpby=NULL, byplot= FALSE,  basal=TRUE, clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  
  if(clusterplot){
    plot_id <- c('집락번호')
  }else{
    plot_id <- c('표본점번호')
  }
  
  
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
  
  if(!largetreearea){
    data <- data %>% filter(data$'대경목조사원내존재여부' == 0)
  }
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  
  
  if(basal){
    
    data_temp <- data %>%
      group_by(data$'조사차기', !!plot_id, !!!grpby, data$'수종명') %>%
      summarise(value = sum(basal_area), .groups = 'drop')
    
  }else{
    
    data_temp <- data %>%
      group_by(data$'조사차기', !!plot_id, !!!grpby, data$'수종명') %>%
      summarise(value = n(), .groups = 'drop')
    
  }
  
  
  data_temp <- data_temp %>% tidyr::spread(key = "data$수종명", value = value )
  
  
  indices <- data_temp[,1:(length(grpby)+2)]
  abundance.matrix <- data_temp[,-c(1:(length(grpby)+2))]
  abundance.matrix[is.na(abundance.matrix)] <- 0
  
  indices$Richness <- rowSums(abundance.matrix>0)
  indices$Shannon <- vegan::diversity(abundance.matrix) # shannon is default
  indices$simpson <- vegan::diversity(abundance.matrix, "simpson")
  indices$evenness  <- indices$Shannon/log(indices$Richness)
  
  indices <- indices %>% rename("order"= "data$조사차기")
  
  
  if(!byplot){
    
    indices <- indices %>% 
      group_by(order, !!!grpby) %>% 
      summarise(mean_Richness = mean(Richness , na.rm=TRUE),
                se_Richness =  plotrix::std.error(Richness, na.rm=TRUE),
                mean_Shannon = mean(Shannon, na.rm=TRUE),
                se_Shannon =  plotrix::std.error(Shannon, na.rm=TRUE),
                mean_simpson = mean(simpson, na.rm=TRUE),
                se_simpson =  plotrix::std.error(simpson, na.rm=TRUE),
                mean_evenness = mean(evenness, na.rm=TRUE),
                se_evenness =  plotrix::std.error(evenness, na.rm=TRUE),.groups = 'drop')
    
   
  }
  
  
  return(indices)
  
  
}
