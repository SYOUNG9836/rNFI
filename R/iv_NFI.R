#' iv_NFI Function
#'
#' This function 
#' @param data : data
#' @param frequency : 빈도사용여부
#' @param clusterplot : 빈도집락부선택
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @keywords plot
#' @return importance value
#' @export 
#' 
#' 


##  

iv_NFI <- function(data, frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  df <- left_join(data$tree[, c('집락번호', '표본점번호',"조사차기", '수목형태구분','수종명', 
                                'basal_area', '대경목조사원내존재여부')], 
                  data$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도', grpby)])
  
  
  
  if (Stockedland){
    df <- df %>% filter(df$'토지이용' == "임목지")
  }
  
  if(talltree){
    df <- df %>% filter(df$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    df <- df %>% filter(df$'대경목조사원내존재여부' == 0)
  }
  
  if(clusterplot){
    iv_temp <- df %>% 
      group_by(df$'조사차기', df$"집락번호" , df$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }else{
    iv_temp <- df %>% 
      group_by(df$'조사차기', df$"표본점번호" , df$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }
  
  
  colnames(iv_temp) <- c("조사차기", "plot","species", "count","basal")
  iv_temp <- data.frame(iv_temp)
  
  ##importancevalue
  
  iv_temp_2<-BiodiversityR::importancevalue.comp(iv_temp, site='plot', species='species', count='count', 
                                                      basal='basal', factor="조사차기")
  
  for(i in 2:length(iv_temp_2)){
    iv_temp_2[[i]] <- as.data.frame(iv_temp_2[[i]])
    iv_temp_2[[i]]$species <- rownames(iv_temp_2[[i]])
    rownames(iv_temp_2[[i]]) <- NULL
    iv_temp_2[[i]]$'조사차기' <- iv_temp_2[[1]][i-1]
    
  }
  
  iv_temp_2[[1]] <- NULL
  
  iv <- data.table::rbindlist(iv_temp_2, fill=TRUE, use.names=TRUE)
  iv <- as.data.frame(iv)
  
  if(frequency){
    iv$importance.value <- iv$importance.value/3
  }else{
    
    iv$importance.value <- (iv$density.percent + iv$dominance.percent)/2
  }
  
  
  
  
  return(iv)
  
}



#' importancevalue Function
#'
#' This function 
#' @param data : data
#' @param frequency : 빈도사용여부
#' @param clusterplot : 빈도집락부선택
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @keywords plot
#' 
#' 


##  

iv_tsvis <- function(data, frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  df <- left_join(data$tree[, c('집락번호', '표본점번호',"조사차기", '수목형태구분','수종명', 
                                'basal_area', '대경목조사원내존재여부')], 
                  data$plot[,c('집락번호', '표본점번호', "조사차기", '조사연도', grpby)])
  
  
  if (Stockedland){
    df <- df %>% filter(df$'토지이용' == "임목지")
  }
  
  if(talltree){
    df <- df %>% filter(df$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    df <- df %>% filter(df$'대경목조사원내존재여부' == 0)
  }
  
  if(clusterplot){
    iv_temp <- df %>% 
      group_by(df$"집락번호" , df$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }else{
    iv_temp <- df %>% 
      group_by(df$"표본점번호" , df$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }
  
  
  colnames(iv_temp) <- c("plot","species", "count","basal")
  iv_temp <- data.frame(iv_temp)
  
  ##importancevalue
  iv <-BiodiversityR::importancevalue(iv_temp, site='plot', species='species', count='count', 
                                                 basal='basal', factor="", level="")
  
  
  iv  <- as.data.frame(iv) 
  iv$species <- rownames(iv)
  rownames(iv) <- NULL
  
  
  
  if(frequency){
    iv$importance.value <- iv$importance.value/3
  }else{
    
    iv$importance.value <- (iv$density.percent + iv$dominance.percent)/2
    iv$frequency <- NULL
    iv$frequency.percent <- NULL
  }
  
  
  return(iv)
  
}




