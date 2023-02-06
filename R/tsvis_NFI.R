#' tsvis_NFI() Function
#'
#' This function loop function
#' @param data : data 
#' @param grpby : grpby 시도코드; 시군구코드; 읍면동 코드
#' @param y : y
#' @param type : biomass; iv
#' @param output : map; line 
#' @param strat : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param clusterplot : byplot TRUE 집락
#' @param frequency : 빈도사용여부
#' @return plot data
#' @export


tsvis_NFI <- function(data, grpby=NULL, y=NULL, type = "biomass", output ="line", strat="stand_subplot", 
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
      
      if( y %in%  c("volume", "biomass", "AG_biomass", "carbon", "co2")){
  
      }else{
        stop("param 'y' must be one of 'volume', 'biomass', 'AG_biomass', 'carbon', 'co2'")
      }
      
      
      
      
    }
    
  }
  
  
  if(type != "iv" & output == "map"){
    
    if(!stringr::str_detect(grpby,'_CD$')){
      
      stop("param 'grpby' must be in administrative district code")
    }
  }
  
  
  if(type == "iv" & output == "map"){
    
    stop("Importance value cannot be visualized by map")
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
      
    }else if(output =="map"){
      
      
      
      district_code[,1] <- (gsub("-", "", district_code[,1]))
      tsvis_df$name <- unlist(lapply(tsvis_df[, grpby], 
                                    FUN=function(x){district_code$"법정동명"[which(x==district_code$"법정동코드")]}))
      
      
      if(nchar(tsvis_df[, grpby][1]) == 10){
        
        bm_map <- right_join(emd, tsvis_df, by=c("EMD_CD" = quo_name(grpby)))
        #bm_map <- sf::st_as_sf( bm_map )
        
      }else if(nchar(tsvis_df[,grpby][1]) == 5){
        
        bm_map <- right_join(sgg, tsvis_df, by=c("SIG_CD" = quo_name(grpby)))
        #bm_map <- sf::st_as_sf( bm_map )
        
      }else{
        
        bm_map <- right_join(do, tsvis_df, by=c("CTPRVN_CD" = quo_name(grpby)))
        #bm_map <- sf::st_as_sf( bm_map )
      }
      
      value <- colnames(bm_map)[grep(paste0("^", y, "|^cwd_", y), colnames(bm_map))]
      se <- colnames(bm_map)[grep(paste0("^rse_", y, "|^rse_cwd_", y), colnames(bm_map))]
      
      value  <- rlang::sym(value)
      se  <- rlang::sym(se)
      
      ylab <- case_when(
        y == "volume" ~ "Volume (m3/ha)",
        y == "biomass" ~ "Biomass (ton/ha)",
        y == "AG_biomass" ~ "AG biomass (ton/ha)",
        y == "carbon" ~ "Carbon stock (tC/ha)",
        y == "co2" ~ "CO2 stock (tCO2/ha)",
        TRUE ~ NA_character_
        
      )
      
     tsvis_value <- ggplot(bm_map) + 
        geom_sf(aes(fill = !!value , geometry = geometry))+
        coord_sf(expand = FALSE, lims_method = "geometry_bbox")+
        #scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
        facet_wrap(~year)+
        theme(axis.text.x = element_text(angle =90, vjust = 1))+
        #ggspatial::annotation_scale(location = "bl", width_hint = 0.1) +
        #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
        #                       pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
        #                       style = north_arrow_fancy_orienteering)+
        scale_fill_viridis_c(direction = -1,  alpha = .7)+
        labs(fill=ylab)
      
      
      tsvis_se <- ggplot(bm_map)+ 
        geom_sf(aes(fill = !!se , geometry = geometry))+
        coord_sf(expand = FALSE, lims_method = "geometry_bbox")+
        facet_wrap(~year)+
        theme(axis.text.x = element_text(angle =90, vjust = 1))+
        #ggspatial::annotation_scale(location = "bl", width_hint = 0.1) +
        #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
        #                       pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
        #                       style = north_arrow_fancy_orienteering)+
        scale_fill_viridis_c(direction = -1,  alpha = .7, option="magma")+
        labs(fill=paste0("RSE (%)", paste(rep(" ", nchar(value)-1), collapse = "")))
      
      tsvis <- cowplot::plot_grid(tsvis_value, tsvis_se)
      
    }else if(output =="line"){
      
      tsvis_df$name <- tsvis_df[,grpby]
      
      value <- colnames(tsvis_df)[grep(paste0("^", y, "|^cwd_", y), colnames(tsvis_df))]
      se <- colnames(tsvis_df)[grep(paste0("^rse_", y, "|^rse_cwd_", y), colnames(tsvis_df))]
      
      value  <- rlang::sym(value)
      se  <- rlang::sym(se)
      
      tsvis <- ggplot(tsvis_df, aes(x=year)) + 
        geom_line(aes(y=!!value, group = name, color = reorder(name, -!!value)), size = 1.1)+ 
        geom_point(aes(y=!!value, group = name, color = reorder(name, -!!value)), size = 3)+ 
        geom_errorbar(aes(ymin=!!value-!!se, ymax=!!value+!!se, color = reorder(name, -!!value)),
                      position=position_dodge(0.9),width=0.2, size=0.8)+ 
        theme(axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=4),
              plot.margin = unit(c(0.3,0.1,0.5,0.6), "cm"), 
              legend.title = element_blank())+
        scale_color_viridis_d()+ 
        labs(x="Year", y=ylab)
      
    }else if(output =="bar"){
      
      tsvis_df$name <- tsvis_df[,grpby]
      
      value <- colnames(tsvis_df)[grep(paste0("^", y, "|^cwd_", y), colnames(tsvis_df))]
      se <- colnames(tsvis_df)[grep(paste0("^rse_", y, "|^rse_cwd_", y), colnames(tsvis_df))]
      
      value  <- rlang::sym(value)
      se  <- rlang::sym(se)
      
      tsvis <- ggplot(tsvis_df, aes(x=year, group = name))+
        geom_bar(aes(y=!!value, fill = reorder(name, -!!value)), size = 1, stat='identity', position = 'dodge')+ 
        geom_errorbar(aes(ymin=!!value-!!se, ymax=!!value+!!se),
                      position=position_dodge(0.9),width=0.2, size=0.8)+
        theme(axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_text(vjust=4),
              plot.margin = unit(c(0.3,0.1,0.5,0.6), "cm"), 
              legend.title = element_blank())+
        scale_fill_viridis_d()+ 
        labs(x="Year", y=ylab)
        
      
    
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
              axis.title.y = element_text(vjust=4),
              plot.margin = unit(c(0.3,0.1,0.5,0.6), "cm"), 
              legend.title = element_blank()) + 
        labs(x= "Year", y="Importance value (%)")
      
      
    } else if(output =="bar"){
      
    
      tsvis <- tsvis_df %>% 
        filter(species %in% reorder(species, importance.value)[1:20]) %>%
        ggplot(aes(y = importance.value, x = reorder(species, importance.value))) +
        geom_bar(aes(fill = reorder(species, importance.value)), width=0.6, stat="identity") +
        #geom_text(aes(label= paste0(round(importance.value, 1),"%")), size=3.5)+
        #scale_y_continuous(limits = c(min(Gangwon_iv$importance.value) - 1, max(Gangwon_iv$importance.value) + 6)) +
        coord_flip() +
        facet_wrap(~year)+
        theme(axis.title.x = element_text(vjust=-1.5),
              axis.title.y = element_blank(),
              plot.margin = unit(c(0.3,0.2,0.5,0.1), "cm"),
              legend.position = "none")+ 
        labs(y="Importance value (%)")
      
      
    } else{
      
      stop(paste( output, ' does not exist.'))
      
    }
    
    
    
    
  }
  
  return(tsvis)
  
}







