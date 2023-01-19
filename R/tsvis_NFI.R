#' tsvis_NFI() Function
#'
#' This function loop function
#' @param data : data 
#' @param grpby : grpby 시도코드; 시군구코드; 읍면동 코드
#' @param y : y
#' @param type : biomass; iv
#' @param output : poly; line 
#' @param strat : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param clusterplot : byplot TRUE 집락
#' @param frequency : 빈도사용여부
#' @return plot data
#' @export


tsvis_NFI <- function(data, grpby=NULL, y=NULL, type = "biomass", output ="line", strat="stand", 
                         clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE, frequency= TRUE){
  
  theme_set(theme_bw())
  theme_update(text = element_text(size=13))
  
  
  if(type != "iv" & output != "table"){
    
    if(is.null(y)){
      stop("param 'y' is a required parameter")
    }else{
      
      if(!is.character(y)) {
        stop("param 'y' must be 'character'")
      }
      
      y  <- rlang::sym(y)
      
      
    }
    
  }
  
  
  if(type != "iv" & output == "poly"){
    
    if(!stringr::str_detect(grpby,'_CD$')){
      
      stop("param 'grpby' must be in administrative district code")
    }
  }
  
  
  if(type == "iv" & output == "poly"){
    
    stop("Importance value cannot be visualized by poly")
  }
  

  tsvis_list <- vector("list", length = (length(unique(data$plot$"조사연도"))-4))
  
  for(i in 1:(length(unique(data$plot$"조사연도"))-4)){ 
    
    s_year <- min(data$plot$"조사연도")+i-1
    e_year <- min(data$plot$"조사연도")+i-1+4
    
    data_temp <- lapply(data, function(x) x %>% filter(x$"조사연도"  >= s_year & x$"조사연도" <= e_year))
    
    
    if(type == "biomass"){
      tsvis_list[[i]] <- biomass_tsvis(data_temp, grpby = grpby, strat = strat, clusterplot = clusterplot,
                                         largetreearea = largetreearea, Stockedland = Stockedland, talltree = talltree)
      tsvis_list[[i]]$year <- e_year
    }else if(type == "cwd"){
      tsvis_list[[i]] <- cwd_biomass_tsvis(data_temp, grpby = grpby, strat = strat, Stockedland = Stockedland)
      tsvis_list[[i]]$year <- e_year
    }else if(type == "iv"){
      tsvis_list[[i]] <- iv_tsvis(data_temp, frequency = frequency, clusterplot = clusterplot,
                                                 largetreearea = largetreearea, Stockedland = Stockedland, talltree = talltree)
      tsvis_list[[i]]$year <- e_year
    }else{
      stop(paste( type, ' does not exist.'))
    }
  }
  
  
  
  tsvis_df <- data.table::rbindlist(tsvis_list, fill=TRUE, use.names=TRUE)
  tsvis_df <- as.data.frame(tsvis_df)
  tsvis_df$year <- as.character(tsvis_df$year)
  
  
  
  if(type =="biomass" | type =="cwd"){
    
    if(output =="table"){
      
      tsvis <- tsvis_df
      
    }else if(output =="poly"){
      
      
      
      district_code[,1] <- (gsub("-", "", district_code[,1]))
      tsvis_df$name <- unlist(lapply(tsvis_df[, grpby], 
                                    FUN=function(x){district_code$"법정동명"[which(x==district_code$"법정동코드")]}))
      
      
      if(nchar(tsvis_df[, grpby][1]) == 10){
        
        bm_poly <- right_join(emd, tsvis_df, by=c("EMD_CD" = quo_name(grpby)))
        #bm_poly <- sf::st_as_sf( bm_poly )
        
        tsvis <- ggplot(bm_poly) + 
          geom_sf(aes(fill = !!y , geometry = geometry))+
          coord_sf(expand = FALSE, lims_method = "geometry_bbox")+
          #scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
          facet_wrap(~year)+
          theme(axis.text.x = element_text(angle =90, vjust = 1))+
          #ggspatial::annotation_scale(location = "bl", width_hint = 0.1) +
          #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
          #                       pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
          #                       style = north_arrow_fancy_orienteering)+
          scale_fill_viridis_c(direction = -1,  alpha = .7)
        
      }else if(nchar(tsvis_df[,grpby][1]) == 5){
        
        bm_poly <- right_join(sgg, tsvis_df, by=c("SIG_CD" = quo_name(grpby)))
        #bm_poly <- sf::st_as_sf( bm_poly )
        
        tsvis <- ggplot(bm_poly) + 
          geom_sf( aes(fill = !!y , geometry = geometry))+
          coord_sf(expand = FALSE, lims_method = "geometry_bbox")+
          #scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
          facet_wrap(~year)+
          theme(axis.text.x = element_text(angle =90, vjust = 1))+
          #ggspatial::annotation_scale(location = "bl", width_hint = 0.1) +
          #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
          #                       pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
          #                       style = north_arrow_fancy_orienteering)+
          scale_fill_viridis_c(direction = -1,  alpha = .7)
        
      }else{
        
        bm_poly <- right_join(do, tsvis_df, by=c("CTPRVN_CD" = quo_name(grpby)))
        #bm_poly <- sf::st_as_sf( bm_poly )
        
        tsvis <- ggplot(bm_poly) + 
          geom_sf(aes(fill = !!y , geometry = geometry))+
          coord_sf(expand = FALSE, lims_method = "geometry_bbox")+
          facet_wrap(~year)+
          theme(axis.text.x = element_text(angle =90, vjust = 1))+
          #ggspatial::annotation_scale(location = "bl", width_hint = 0.1) +
          #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
          #                       pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
          #                       style = north_arrow_fancy_orienteering)+
          scale_fill_viridis_c(direction = -1,  alpha = .7)
        
      }
      
    }else if(output =="line"){
      
      tsvis_df$name <- tsvis_df[,grpby]
      
      tsvis <- ggplot(tsvis_df) + 
        geom_line(aes(x=year, y=!!y, group = name, color = reorder(name, -!!y)), size = 1.1)+ 
        theme(axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=4))+
        theme(plot.margin = unit(c(0.3,0.1,0.5,0.6), "cm"), legend.title = element_blank()) + 
        guides(fill = guide_legend(reverse = TRUE))
      
    }else{
      
      stop(paste( output, ' does not exist.'))
      
    }
  }else if(type=="iv"){
    
    if(output =="table"){
      
      tsvis <- tsvis_df
      
    } else if(output =="line"){
      
      tsvis <- tsvis_df %>% 
        filter(species %in% reorder(species, importance.value)[1:20]) %>%
        ggplot() + 
        geom_line(aes(x=year, y=importance.value, group = species, color = reorder(species, -importance.value)), size = 1.1)+ 
        theme(axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=4))+
        theme(plot.margin = unit(c(0.3,0.1,0.5,0.6), "cm"), legend.title = element_blank()) + 
        guides(fill = guide_legend(reverse = TRUE))
      
      
    } else{
      
      stop(paste( output, ' does not exist.'))
      
    }
    
    
    
    
  }
  
  return(tsvis)
  
}







