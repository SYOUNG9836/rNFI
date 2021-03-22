
##--------------------------------------------------------------
## 바이오 매스 지상부+지하부 0, 지상부 0 지하부x 
## 탄소흡수량  
## plot별로 수종별로 영급별로 
## 공간구획, 위경도 또는 clipnfi와 연관 
##  
## 추정간재적 단위 - m3
## 읍면동 .rda 올리면 읍면동 코드로 면적 계산해서 /ha 바이오매스
##--------------------------------------------------------------


##--------------------------------------------------------------
## biomass 구하는 함수
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)
##--------------------------------------------------------------

T__biomass <- function(data){
  
  t_bm <- data  %>% dplyr::mutate(biomass = dplyr::case_when(
    
    
    ##--------------------------------------------------------------
    ## 강원지방소나무
    ##--------------------------------------------------------------
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "강원지방소나무",6]),
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "중부지방소나무",6]),
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4])*(1+bio_coeff[bio_coeff[,1] == "낙엽송",6]),
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4])*(1+bio_coeff[bio_coeff[,1] == "리기다소나무",6]),
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4])*(1+bio_coeff[bio_coeff[,1] == "곰솔",6]),
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4])*(1+bio_coeff[bio_coeff[,1] == "잣나무",6]),
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4])*(1+bio_coeff[bio_coeff[,1] == "삼나무",6]),
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4])*(1+bio_coeff[bio_coeff[,1] == "편백",6]),
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4])*(1+bio_coeff[bio_coeff[,1] == "굴참나무",6]),
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4])*(1+bio_coeff[bio_coeff[,1] == "신갈나무",6]),
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4])*(1+bio_coeff[bio_coeff[,1] == "상수리나무",6]),
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4])*(1+bio_coeff[bio_coeff[,1] == "졸참나무",6]),
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4])*(1+bio_coeff[bio_coeff[,1] == "붉가시나무",6]),
    
    
    ##--------------------------------------------------------------
    ## 기타 활엽수 및 기타 침엽수
    ##--------------------------------------------------------------
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 활엽수",6]),
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 침엽수",6]),
    TRUE ~ NA_real_
    
  ))
  
  
  
  
  return(t_bm)
  
}


##--------------------------------------------------------------
## 지상부 biomass 구하기 
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)
##--------------------------------------------------------------
AG_biomass <- function(data){
  
  ag_bm <- data  %>% dplyr::mutate(AG_biomass = dplyr::case_when(
    
    
    ##--------------------------------------------------------------
    ## 강원지방소나무
    ##--------------------------------------------------------------
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4]),
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4]),
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4]),
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4]),
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4]),
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4]),
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4]),
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4]),
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4]),
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4]),
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4]),
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4]),
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4]),
    
    
    ##--------------------------------------------------------------
    ## 기타 활엽수 및 기타 침엽수
    ##--------------------------------------------------------------
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4]),
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4]),
    TRUE ~ NA_real_
    
  ))
  
  
  
  return(ag_bm)
  
}

##--------------------------------------------------------------
## 탄소흡수량 구하기 
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))*(44/12)
##--------------------------------------------------------------
carbon_stock <- function(data){
  
  carbn_s <- data  %>% dplyr::mutate(carbon_stock = dplyr::case_when(
    
    
    ##--------------------------------------------------------------
    ## 강원지방소나무
    ##--------------------------------------------------------------
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "강원지방소나무",6])*0.51,
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "중부지방소나무",6])*0.51,
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4])*(1+bio_coeff[bio_coeff[,1] == "낙엽송",6])*0.51,
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4])*(1+bio_coeff[bio_coeff[,1] == "리기다소나무",6])*0.51,
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4])*(1+bio_coeff[bio_coeff[,1] == "곰솔",6])*0.51,
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4])*(1+bio_coeff[bio_coeff[,1] == "잣나무",6])*0.51,
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4])*(1+bio_coeff[bio_coeff[,1] == "삼나무",6])*0.51,
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4])*(1+bio_coeff[bio_coeff[,1] == "편백",6])*0.51*0.51,
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4])*(1+bio_coeff[bio_coeff[,1] == "굴참나무",6])*0.48,
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4])*(1+bio_coeff[bio_coeff[,1] == "신갈나무",6])*0.48,
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4])*(1+bio_coeff[bio_coeff[,1] == "상수리나무",6])*0.48,
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4])*(1+bio_coeff[bio_coeff[,1] == "졸참나무",6])*0.48,
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4])*(1+bio_coeff[bio_coeff[,1] == "붉가시나무",6])*0.48,
    
    
    ##--------------------------------------------------------------
    ## 기타 활엽수 및 기타 침엽수
    ##--------------------------------------------------------------
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 활엽수",6])*0.48,
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 침엽수",6])*0.51,
    TRUE ~ NA_real_
    
  ))
  
  
  
  
  return(carbn_s)
  
}

##--------------------------------------------------------------
## 탄소흡수량 구하기 
## 수종 ~ 추정간재적*(목재기본밀도)*(바이오매스 확장계수)(1+뿌리함량비)*(0.51(침) or 0.48(활))*(44/12)
##--------------------------------------------------------------
co2_stock <- function(data){
  
  co2_s <- data  %>% dplyr::mutate(co2_stock = dplyr::case_when(
    
    
    ##--------------------------------------------------------------
    ## 강원지방소나무
    ##--------------------------------------------------------------
    (data$'수종명' =="소나무" && (data$'시군구' == "영주군" || data$'시군구' == "봉화군" || data$'시군구' == "울진군" || data$'시군구' == "영양군" || data$'광역시도' == "강원도" )) 
    ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "강원지방소나무",2])*(bio_coeff[bio_coeff[,1] == "강원지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "강원지방소나무",6])*0.51*(44/12),
    
    data$'수종명' =="소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "중부지방소나무",2])*(bio_coeff[bio_coeff[,1] == "중부지방소나무",4])*(1+bio_coeff[bio_coeff[,1] == "중부지방소나무",6])*0.51*(44/12),
    data$'수종명' =="낙엽송" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "낙엽송",2])*(bio_coeff[bio_coeff[,1] == "낙엽송",4])*(1+bio_coeff[bio_coeff[,1] == "낙엽송",6])*0.51*(44/12),
    data$'수종명' =="리기다소나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "리기다소나무",2])*(bio_coeff[bio_coeff[,1] == "리기다소나무",4])*(1+bio_coeff[bio_coeff[,1] == "리기다소나무",6])*0.51*(44/12),
    data$'수종명' =="곰솔" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "곰솔",2])*(bio_coeff[bio_coeff[,1] == "곰솔",4])*(1+bio_coeff[bio_coeff[,1] == "곰솔",6])*0.51*(44/12),
    data$'수종명' =="잣나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "잣나무",2])*(bio_coeff[bio_coeff[,1] == "잣나무",4])*(1+bio_coeff[bio_coeff[,1] == "잣나무",6])*0.51*(44/12),
    data$'수종명' =="삼나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "삼나무",2])*(bio_coeff[bio_coeff[,1] == "삼나무",4])*(1+bio_coeff[bio_coeff[,1] == "삼나무",6])*0.51*(44/12),
    data$'수종명' =="편백" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "편백",2])*(bio_coeff[bio_coeff[,1] == "편백",4])*(1+bio_coeff[bio_coeff[,1] == "편백",6])*0.51*(44/12)*0.51*(44/12),
    data$'수종명' =="굴참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "굴참나무",2])*(bio_coeff[bio_coeff[,1] == "굴참나무",4])*(1+bio_coeff[bio_coeff[,1] == "굴참나무",6])*0.48*(44/12),
    data$'수종명' =="신갈나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "신갈나무",2])*(bio_coeff[bio_coeff[,1] == "신갈나무",4])*(1+bio_coeff[bio_coeff[,1] == "신갈나무",6])*0.48*(44/12),
    data$'수종명' =="상수리나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "상수리나무",2])*(bio_coeff[bio_coeff[,1] == "상수리나무",4])*(1+bio_coeff[bio_coeff[,1] == "상수리나무",6])*0.48*(44/12),
    data$'수종명' =="졸참나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "졸참나무",2])*(bio_coeff[bio_coeff[,1] == "졸참나무",4])*(1+bio_coeff[bio_coeff[,1] == "졸참나무",6])*0.48*(44/12),
    data$'수종명' =="붉가시나무" ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "붉가시나무",2])*(bio_coeff[bio_coeff[,1] == "붉가시나무",4])*(1+bio_coeff[bio_coeff[,1] == "붉가시나무",6])*0.48*(44/12),
    
    
    ##--------------------------------------------------------------
    ## 기타 활엽수 및 기타 침엽수
    ##--------------------------------------------------------------
    (data$'침활구분' =="활엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 활엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 활엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 활엽수",6])*0.48*(44/12),
    (data$'침활구분' =="침엽수") ~ (data$'추정간재적')*(bio_coeff[bio_coeff[,1] == "기타 침엽수",2])*(bio_coeff[bio_coeff[,1] == "기타 침엽수",4])*(1+bio_coeff[bio_coeff[,1] == "기타 침엽수",6])*0.51*(44/12),
    TRUE ~ NA_real_
    
  ))
  
  
  
  
  return(co2_s)
  
}

#' biomass() Function
#'
#' This function 
#' @param data : data
#' @param byplot : byplot
#' @param grpby : grpby
#' @keywords biomass
#' @export 


biomass <- function(data, byplot= FALSE, grpby=NULL){
  
  ##--------------------------------------------------------------
  ## df 추정간재적 type이 num이 아닌 경우 as.numeric
  ##--------------------------------------------------------------
  if (!is.numeric(data$'추정간재적')){
    data$'추정간재적' <- as.numeric(data$'추정간재적')} 
  
  
  bm_temp <- T__biomass(data)
  bm_temp <- AG_biomass(bm_temp)
  bm_temp <- carbon_stock(bm_temp)
  bm_temp <- co2_stock(bm_temp)
  bm <- bm_temp
  
  ##--------------------------------------------------------------
  ## 표본점 번호별 바이오매스 확인 
  ##--------------------------------------------------------------
  
  if (byplot){
    ##--------------------------------------------------------------------------
    ## 수종명, 수관급, 형질급, 광역시도, 시군구, 읍면동, 지형, 임상, 경급, 영급 
    ##--------------------------------------------------------------------------
    if (!is.null(grpby)){
      
      bm <- bm %>% dplyr::filter(bm$'수목형태구분'=="교목") 
      bm <- bm %>% 
        dplyr::group_by(bm$'표본점번호', bm[,grpby]) %>% 
        dplyr::summarise(volume_sum =sum(get('추정간재적')),
                         biomass_sum = sum(biomass),
                         AG_biomass_sum = sum(AG_biomass),
                         carbon_stock_sum = sum(carbon_stock),
                         co2_stock_sum = sum(co2_stock), .groups = 'drop')
    }
    else{
      bm <- bm %>% dplyr::filter(bm$'수목형태구분'=="교목") 
      bm <- bm %>%  
        dplyr::group_by(bm$'표본점번호') %>% 
        dplyr::summarise(volume_sum =sum(get('추정간재적')),
                         biomass_sum = sum(biomass),
                         AG_biomass_sum = sum(AG_biomass),
                         carbon_stock_sum = sum(carbon_stock),
                         co2_stock_sum = sum(co2_stock), .groups = 'drop')
    }
    
    
  }
  
  ##--------------------------------------------------------------
  ## 개별 수목별 바이오매스
  ##--------------------------------------------------------------
  else{
    
    if (!is.null(grpby)){
      
      bm <- bm %>% dplyr::filter(bm$'수목형태구분'=="교목") 
      bm <- bm %>%
        dplyr::group_by(bm[,grpby]) %>% 
        dplyr::summarise(volume_sum =sum(get('추정간재적')),
                         biomass_sum = sum(biomass),
                         AG_biomass_sum = sum(AG_biomass),
                         carbon_stock_sum = sum(carbon_stock),
                         co2_stock_sum = sum(co2_stock), .groups = 'drop')
    }}
  
  return(bm)
  
  
}
