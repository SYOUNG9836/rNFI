

#' diversity Function
#'
#' This function 
#' @param data : data
#' @keywords plot
#' @export 
#' 
#' 


##  

diversity_NFI <- function(data){
  
  
  data_temp <- data %>%
    group_by(data$"표본점번호", data$'수종명') %>%
    summarise(count = n(), .groups = 'drop')
  
  data_temp <- data_temp %>% tidyr::spread(key = `data$수종명`, value = count )
  
  
  indices <- data_temp[,1]
  abundance.matrix <- data_temp[,-1]
  abundance.matrix[is.na(abundance.matrix)] <- 0
  
  indices$Richness <- rowSums(abundance.matrix>0)
  indices$Shannon <- diversity(abundance.matrix) # shannon is default
  indices$simpson <- diversity(abundance.matrix, "simpson")
  
  
  
  
  
  
  
  
}

#' importancevalue Function
#'
#' This function 
#' @param data : data
#' @keywords plot
#' @export 
#' 
#' 


##  

importancevalue_NFI <- function(data){
  
  data_temp <- data # %>% filter(data$'수목형태구분' == c("교목"))
  data_temp$basal <- 0.0000785*(data_temp$'흉고직경')^2
  data_iv <- data_temp %>% group_by(data_temp$"표본점번호" , data_temp$"수종명") %>% summarise(count = n(), basal =sum(basal, na.rm=T),.groups = 'drop')
  
  colnames(data_iv) <- c("plot","species", "count","basal")
  data_iv <- data.frame(data_iv)
  
  ##importancevalue
  data_iv_result<-importancevalue(data_iv, site='plot', species='species', count='count', 
                                  basal='basal', factor="", level="")
  
  str(data_iv_result)
  
  
  data_iv_result  <- as.data.frame(data_iv_result) # %>% select(importance.value)
  data_iv_result$species <- rownames(data_iv_result)
  rownames(data_iv_result) <- NULL
  
}

#'Descriptive_statistics Function
#'
#' This function 
#' @param data : data
#' @keywords plot
#' @export 
#' 
#' 


##


Descriptive_statistics<- function(data, grpby="표본점번호"){
  
  data_temp <- data
  data_temp$basal <- 0.0000785*(data_temp$'흉고직경')^2
  
  data_temp <- data_temp %>%
    group_by(data_temp[,grpby]) %>%
    summarise(count = n(), mean_DBH = mean(get('흉고직경'), na.rm=TRUE), 
              mean_H = mean(get('추정수고'), na.rm=TRUE),
              mean_dominant_H = mean(get('추정수고')[get('수관급')=="우세목"], na.rm=TRUE),
              mean_basal= mean(basal, na.rm=TRUE),
              mean_volume= mean(get('추정간재적'), na.rm=TRUE),.groups = 'drop')
  
}


