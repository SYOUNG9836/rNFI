#' iv_NFI Function
#'
#' This function 
#' @param data : data
#' @param sp : sp
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

iv_NFI <- function(data, sp="SP" , frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  ##경고 
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  ##전처리
  if (Stockedland){ #임목지
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  if(talltree){#수목형태구분
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
  }
  
  if(!largetreearea){ #대경목조사원내존재여부
    data$tree <- data$tree %>% filter(SUBPTYP == 0)
  }
 
  df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD', 
                                'basal_area', 'SUBPTYP', sp)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR','LAND_USE', "LAND_USECD")],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  
  sp<- rlang::sym(sp)

  
  if(clusterplot){
    iv_temp <- df %>% 
      group_by(CYCLE, CLST_PLOT , !!sp) %>% 
      summarise(count = n(), basal = sum(basal_area, na.rm=T),.groups = 'drop')
    plot_id <- c('CLST_PLOT')
    
    
  }else{
    iv_temp <- df %>% 
      group_by(CYCLE, SUB_PLOT , !!sp) %>% 
      summarise(count = n(), basal = sum(basal_area, na.rm=T),.groups = 'drop')
    plot_id <- c('SUB_PLOT')
  }
  
  plot_id  <- rlang::sym(plot_id)
  
  colnames(iv_temp) <- c("CYCLE", quo_name(plot_id), "SP", "count","basal")
  iv_temp <- data.frame(iv_temp)
  
  
  
  
  ##차수별 importancevalue 구하기
  iv_temp_2<-BiodiversityR::importancevalue.comp(iv_temp, site=quo_name(plot_id), species='SP', count='count', 
                                                      basal='basal', factor="CYCLE")
  
  for(i in 2:length(iv_temp_2)){
    iv_temp_2[[i]] <- as.data.frame(iv_temp_2[[i]])
    iv_temp_2[[i]]$species <- rownames(iv_temp_2[[i]])
    rownames(iv_temp_2[[i]]) <- NULL
    iv_temp_2[[i]]$CYCLE <- iv_temp_2[[1]][i-1]
    
  }
  
  iv_temp_2[[1]] <- NULL
  
  iv <- data.table::rbindlist(iv_temp_2, fill=TRUE, use.names=TRUE)
  iv <- as.data.frame(iv)
  

  
  
  
  ## 빈도포함여부 
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
#' @param sp : sp
#' @param frequency : 빈도사용여부
#' @param clusterplot : 빈도집락부선택
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @keywords plot
#' 
#' 


##  그림용 function / 내부용

iv_tsvis <- function(data, sp="SP" , frequency=TRUE , clusterplot=TRUE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  ## 경고
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  
  ## 전처리
  if (Stockedland){ #임목지
    data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
  }
  
  if(talltree){#수목형태구분
    data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
  }
  
  if(!largetreearea){ #대경목조사원내존재여부
    data$tree <- data$tree %>% filter(SUBPTYP == 0)
  }
  
  
  df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD', 
                                'basal_area', 'SUBPTYP', sp)], 
                  data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR','LAND_USE', "LAND_USECD")],
                  by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
  
  sp<- rlang::sym(sp)
  
  if(clusterplot){
    iv_temp <- df %>% 
      group_by(CLST_PLOT , !!sp) %>% 
      summarise(count = n(), basal = sum(basal_area, na.rm=T),.groups = 'drop')
    plot_id <- c('CLST_PLOT')
    
  }else{
    iv_temp <- df %>% 
      group_by(SUB_PLOT , !!sp) %>% 
      summarise(count = n(), basal = sum(basal_area, na.rm=T),.groups = 'drop')
    plot_id <- c('SUB_PLOT')
    
  }
  
  plot_id  <- rlang::sym(plot_id)
  
  colnames(iv_temp) <- c(quo_name(plot_id), "SP", "count","basal")
  iv_temp <- data.frame(iv_temp)
  
  
  
  ##importancevalue 구하기
  iv <-BiodiversityR::importancevalue(iv_temp, site=quo_name(plot_id), species='SP', count='count', 
                                                 basal='basal', factor="", level="")
  
  
  iv  <- as.data.frame(iv) 
  iv$species <- rownames(iv)
  rownames(iv) <- NULL
  
  
  ## 빈도포함여부 
  if(frequency){
    iv$importance.value <- iv$importance.value/3
  }else{
    
    iv$importance.value <- (iv$density.percent + iv$dominance.percent)/2
    iv$frequency <- NULL
    iv$frequency.percent <- NULL
  }
  
  
  return(iv)
  
}




