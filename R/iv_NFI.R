#' iv_NFI()
#' 
#' 
#' @description
#' iv_NFI() is a function that calculates the importance values of tree species based on frequency, density and coverage.
#' please refer to the \code{\link[BiodiversityR]{importancevalue}} function in the \pkg{BiodiversityR} package.
#' 
#' @details
#' The importance value is calculated as the mean from (i) the relative frequency; (ii) the relative density; and (iii) the relative coverage. The importance value ranges between 0 and 100.
#' Frequency is calculated as the number of plots where a species is observed divided by the total number of survey plots. Relative frequency is calculated by dividing the frequency by the sum of the frequencies of all species, multiplied by 100 (to obtain a percentage).
#' Density is calculated as the total number of individuals of a species. Relative density is calculated by dividing the density by the sum of the densities of all species, multiplied by 100 (to obtain a percentage).
#' Coverage is calculated as the total basal area of a species. Relative dominance is calculated by dividing the dominance by the sum of the dominance of all species, multiplied by 100 (to obtain a percentage).
#'
#' @param data : A `list` produced by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.
#' @param sp : A character vector indicating the column name of tree species. e.g,. SP, Genus.
#' @param frequency : A logical flag indicating whether to use frequency in importance calculations.
#' @param clusterplot : A logical flag indicating whether to calculate for cluster plot collectively or calculate for each subplot separately.
#' @param largetreearea : A logical flag indicating whether to include a large tree plot as well, or only a tree plot.
#' @param Stockedland : A logical flag indicating whether to include only stocked land or also include other types of land.
#' @param talltree : A logical flag indicating whether to include only tall trees or also shrubs.
#' 
#' @return  A `data.frame` that includes importance value for tree species.
#' 
#' @seealso
#' \code{\link[BiodiversityR]{importancevalue}} for calculating the importance values.
#' 
#' @examples
#' \dontrun{iv_NFI()}
#' 
#' @note 
#' 수종분류 미흡 속별로 계산해야함 
#' 
#' @export 


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
    data$tree <- data$tree %>% filter(LARGEP_TREE == 0)
  }
 
  df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD', 
                                'basal_area', 'LARGEP_TREE', sp)], 
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



 
#' iv_tsvis()
#' 
#' 
#' @description
#' iv_tsvis() is a function that calculates the importance values of tree species based on frequency, density and coverage.
#' 
#' @details 
#' \code{\link[rNFI]{iv_NFI}}
#' 
#' @param data : A `list` produced by \code{\link{read_NFI}} that contains 'plot' and 'tree' data frames.
#' @param sp : A character vector indicating the column name of tree species.
#' @param frequency : A logical flag indicating whether to use frequency in importance calculations.
#' @param clusterplot : A logical flag indicating whether to calculate for cluster plot collectively or calculate for each subplot separately.
#' @param largetreearea : A logical flag indicating whether to include a large tree plot as well, or only a tree plot.
#' @param Stockedland : A logical flag indicating whether to include only stocked land or also include other types of land.
#' @param talltree : A logical flag indicating whether to include only tall trees or also shrubs.
#' 
#' @return  A `data.frame` that includes importance value for tree species.
#' 
#' @export 


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
    data$tree <- data$tree %>% filter(LARGEP_TREE == 0)
  }
  
  
  df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT',"CYCLE", 'WDY_PLNTS_TYP_CD', 
                                'basal_area', 'LARGEP_TREE', sp)], 
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




