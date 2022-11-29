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
#' @return importancevalue
#' @export 
#' 
#' 


##  

importancevalue_NFI <- function(data, frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  if (Stockedland){
    data <- data %>% filter(data$'토지이용' == "임목지")
  }
  
  if(talltree){
    data <- data %>% filter(data$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    data <- data %>% filter(data$'대경목조사원내존재여부' == 0)
  }
  
  if(clusterplot){
    data_iv <- data %>% 
      group_by(data$'조사차기', data$"집락번호" , data$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }else{
    data_iv <- data %>% 
      group_by(data$'조사차기', data$"표본점번호" , data$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }
  
  
  colnames(data_iv) <- c("조사차기", "plot","species", "count","basal")
  data_iv <- data.frame(data_iv)
  
  ##importancevalue
  
  data_iv_result<-BiodiversityR::importancevalue.comp(data_iv, site='plot', species='species', count='count', 
                                                      basal='basal', factor="조사차기")
  
  for(i in 2:length(data_iv_result)){
    data_iv_result[[i]] <- as.data.frame(data_iv_result[[i]])
    data_iv_result[[i]]$species <- rownames(data_iv_result[[i]])
    rownames(data_iv_result[[i]]) <- NULL
    data_iv_result[[i]]$'조사차기' <- data_iv_result[[1]][i-1]
    
  }
  
  data_iv_result[[1]] <- NULL
  
  data_iv_df <- data.table::rbindlist(data_iv_result, fill=TRUE, use.names=TRUE)
  data_iv_df <- as.data.frame(data_iv_df)
  
  if(frequency){
    data_iv_df$importance.value <- data_iv_df$importance.value/3
  }else{
    
    data_iv_df$importance.value <- (data_iv_df$density.percent + data_iv_df$dominance.percent)/2
  }
  
  
  
  
  return(data_iv_df)
  
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

importancevalue_evaluate <- function(data, frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  if (Stockedland){
    data <- data %>% filter(data$'토지이용' == "임목지")
  }
  
  if(talltree){
    data <- data %>% filter(data$'수목형태구분' == "교목")
  }
  
  if(!largetreearea){
    data <- data %>% filter(data$'대경목조사원내존재여부' == 0)
  }
  
  if(clusterplot){
    data_iv <- data %>% 
      group_by(data$"집락번호" , data$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }else{
    data_iv <- data %>% 
      group_by(data$"표본점번호" , data$"수종명") %>% 
      summarise(count = n(), basal =sum(basal_area, na.rm=T),.groups = 'drop')
    
  }
  
  
  colnames(data_iv) <- c("plot","species", "count","basal")
  data_iv <- data.frame(data_iv)
  
  ##importancevalue
  data_iv_result<-BiodiversityR::importancevalue(data_iv, site='plot', species='species', count='count', 
                                                 basal='basal', factor="", level="")
  
  
  data_iv_result  <- as.data.frame(data_iv_result) 
  data_iv_result$species <- rownames(data_iv_result)
  rownames(data_iv_result) <- NULL
  
 
  
  if(frequency){
    data_iv_result$importance.value <- data_iv_result$importance.value/3
  }else{
    
    data_iv_result$importance.value <- (data_iv_result$density.percent + data_iv_result$dominance.percent)/2
    data_iv_result$frequency <- NULL
    data_iv_result$frequency.percent <- NULL
  }
  
  
  return(data_iv_result)
  
}




