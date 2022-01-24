
#   Author: Maryam Khodaverdi 
#   Date: 06/21
#   Desc: formatting string OS levels in R

daily_OS_levels <- function(daily_string_level_sql) 
{
    library(tidyverse)
    find_indx <- function(x) {
        v <- unique(as.integer(unlist(strsplit(x,",",fixed=T))))
        v <- v[!is.na(v) & (v >= 0 & v < 28)] + 1
        return(v)
    }
    day_names <- paste0("day",seq(1,28))
    df <- daily_string_level_sql %>% 
	select(person_id, date_of_earliest_covid_diagnosis, hsp_days, oxy_days, vent_days, ecmo_days, death_days)
    npt <- dim(df)[1]
    days <- matrix(rep(1,28*npt),nrow=npt, ncol=28)
    df <- cbind(df,as.data.frame(days))
    names(df)[8:35] <- day_names

    df2 <- df 
    nrow <- dim(df2)[1]
    days <- as.matrix(df2[,8:35])

    idx_hsp <- lapply(df2$hsp_days, find_indx)
    idx_oxy <- lapply(df2$oxy_days, find_indx)
    idx_vent <- lapply(df2$vent_days, find_indx)
    idx_ecmo <- lapply(df2$ecmo_days, find_indx)
    idx_death <- lapply(df2$death_days, find_indx)

    ndropped = 0
    for (j in 1:nrow) {
        # Apply level 3 sequences
        k <- idx_hsp[[j]]
        if(length(k) > 0)
            days[j,k] <- 3     
        else
            ndropped = ndropped + 1

        # Apply level 5 sequences
        k <- idx_oxy[[j]]
        if(length(k) > 0)
            days[j,k] <- 5     
        else
            ndropped = ndropped + 1

        # Apply level 7 sequences
        k <- idx_vent[[j]]
        if(length(k) > 0)
            days[j,k] <- 7     
        else
            ndropped = ndropped + 1
        
        # Apply level 9 sequences
        k <- idx_ecmo[[j]]
        if(length(k) > 0)
            days[j,k] <- 9     
        else
            ndropped = ndropped + 1

        # Apply level 11 sequences
        k <- idx_death[[j]]
        if(length(k) > 0){
            kk <- seq(min(k),28)  
            days[j,kk] <- 11 }    
        else{
            ndropped = ndropped + 1}
    }
    cat("dropped ",ndropped,"empty rows\n")
    df[,8:35] <- days

    return(df[,c(1,2,8:35)])
}



weekly_OS_levels <- function(daily_OS_levels) 
{ 
    library(dplyr)
    df <- covid_pts_daily_outcome
    df <- df %>% 
        group_by(person_id, date_of_earliest_covid_diagnosis) %>% 
        summarise(week1=pmax(day1,day2,day3,day4,day5,day6,day7) 
            , week2=pmax(day8,day9,day10,day11,day12,day13,day14)
            , week3=pmax(day15,day16,day17,day18,day19,day20,day21)
            , week4=pmax(day22,day23,day24,day25,day26,day27,day28)) 
    df <- data.frame(df)
    return(df)
}


