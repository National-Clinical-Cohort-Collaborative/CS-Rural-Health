library(tidyverse)
library(magrittr)
library(dplyr)   
library(gtsummary)

@transform_pandas(
    Output(rid="ri.vector.main.execute.04d082b7-cc7e-4615-a33e-a15e745172aa"),
    lat_lon_summary=Input(rid="ri.foundry.main.dataset.ce8c767b-3c15-44e6-a1c3-fbab8a4a1c4c")
)
final_heat_map <- function(lat_lon_summary) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)

mybreaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)

local_df <- lat_lon_summary %>% 
dplyr::select(long, lat, case_count, group) %>%
arrange(case_count) %>%
mutate(case_count=case_count/1000)

usa <- map_data("state")

gg1 <- ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), color="white", fill="grey", alpha=0.3) +
    geom_point(data=local_df, aes(x=long, y=lat, size=case_count, color= case_count, alpha= case_count), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Population (in T)", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Population (in T)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
    scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in T)" ) +
    theme_void() +  xlim(-125,-65) + ylim(20,50) + coord_map() + 
    guides( colour = guide_legend()) +
    #ggtitle("N3C Patient Population") +
    theme(
      legend.position = c(1, 0.6),
      legend.justification = c("right", "top"),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 20, hjust=0.1, color = "#303B41", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )

plot(gg1)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.4f562135-cfdd-4465-985a-363c61a81274"),
    lat_lon_summary=Input(rid="ri.foundry.main.dataset.ce8c767b-3c15-44e6-a1c3-fbab8a4a1c4c")
)
final_heat_map2 <- function(lat_lon_summary) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)

mybreaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0 , 2.5)

local_df <- lat_lon_summary %>% 
dplyr::select(long, lat, case_count, group) %>%
arrange(case_count) %>%
mutate(case_count=case_count/1000)

usa <- map_data("state")

gg1 <- ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), color="white", fill="grey", alpha=0.3) +
    geom_point(data=local_df, aes(x=long, y=lat, size=case_count, color= case_count, alpha= case_count), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Population (in T)", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Population (in T)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
    scale_color_viridis(option="magma",trans="log", breaks=mybreaks, name="Population (in T)", limits = c(-1, 10) ) +
    theme_void() +  xlim(-125,-65) + ylim(20,50) + coord_map() + 
    guides( colour = guide_legend()) +
    #ggtitle("N3C Patient Population") +
    theme(
      legend.position = c(.95, 0.6),
      legend.justification = c("right", "top"),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 20, hjust=0.1, color = "#303B41", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )

plot(gg1)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.fb5b1b6f-e953-4a76-aba9-fa47f29cd42b"),
    lat_long_summary_nonurban_adjacent_rural=Input(rid="ri.foundry.main.dataset.d94caaba-ac4b-419f-a26e-af25419fb7fe")
)
final_heat_map_nonurban_adjacent_rural <- function(lat_long_summary_nonurban_adjacent_rural) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)

mybreaks <- c(0.1, 0.2, 0.3, 0.4, 0.5) #, 0.75, 1.0, 1.5, 2.0 , 2.5)

local_df <- lat_long_summary_nonurban_adjacent_rural %>% 
dplyr::select(long, lat, case_count, group) %>%
arrange(case_count) %>%
mutate(case_count=case_count/1000)

usa <- map_data("state")

gg1 <- ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), color="white", fill="grey", alpha=0.3) +
    geom_point(data=local_df, aes(x=long, y=lat, size=case_count, color= case_count, alpha= case_count), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Population (in T)", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Population (in T)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
    scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in T)", limits = c(-1, 10) ) +
    theme_void() +  xlim(-125,-65) + ylim(20,50) + coord_map() + 
    guides( colour = guide_legend()) +
    #ggtitle("N3C Patient Population") +
    theme(
      legend.position = c(.95, 0.6),
      legend.justification = c("right", "top"),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 20, hjust=0.1, color = "#303B41", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )

plot(gg1)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.dcf4da52-ed4a-46ab-b9f9-3a1f32d07aa7"),
    lat_lon_summary_urban=Input(rid="ri.foundry.main.dataset.c1b03bb3-cb19-4254-a7bd-003a79790248")
)
final_heat_map_urban <- function(lat_lon_summary_urban) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)

mybreaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0 , 2.5)

local_df <- lat_lon_summary_urban %>% 
dplyr::select(long, lat, case_count, group) %>%
arrange(case_count) %>%
mutate(case_count=case_count/1000)

usa <- map_data("state")

gg1 <- ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), color="white", fill="grey", alpha=0.3) +
    geom_point(data=local_df, aes(x=long, y=lat, size=case_count, color= case_count, alpha= case_count), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Population (in T)", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Population (in T)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
    scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in T)", limits = c(-1, 10) ) +
    theme_void() +  xlim(-125,-65) + ylim(20,50) + coord_map() + 
    guides( colour = guide_legend()) +
    #ggtitle("N3C Patient Population") +
    theme(
      legend.position = c(.95, 0.6),
      legend.justification = c("right", "top"),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 20, hjust=0.1, color = "#303B41", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )

plot(gg1)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.55f0926c-41da-4199-be6c-0c2a06aec99e"),
    lat_long_summary_urban_adjacent_rural=Input(rid="ri.foundry.main.dataset.32ed912f-cfad-4767-9dd6-1019b06fdfc6")
)
final_heat_map_urban_adjacent_rural <- function(lat_long_summary_urban_adjacent_rural) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)

mybreaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0 , 2.5)

local_df <- lat_long_summary_urban_adjacent_rural %>% 
dplyr::select(long, lat, case_count, group) %>%
arrange(case_count) %>%
mutate(case_count=case_count/1000)

usa <- map_data("state")

gg1 <- ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group), color="white", fill="grey", alpha=0.3) +
    geom_point(data=local_df, aes(x=long, y=lat, size=case_count, color= case_count, alpha= case_count), shape=20, stroke=FALSE) +
    scale_size_continuous(name="Population (in T)", trans="log", range=c(1,12), breaks=mybreaks) +
    scale_alpha_continuous(name="Population (in T)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
    scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in T)", limits = c(-1, 10) ) +
    theme_void() +  xlim(-125,-65) + ylim(20,50) + coord_map() + 
    guides( colour = guide_legend()) +
    #ggtitle("N3C Patient Population") +
    theme(
      legend.position = c(.95, 0.6),
      legend.justification = c("right", "top"),
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 20, hjust=0.1, color = "#303B41", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )

plot(gg1)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.30e7a210-bdbf-42ae-a8a9-7066412a53e2"),
    converted_Summary=Input(rid="ri.foundry.main.dataset.a45f06c0-1c1c-4902-be31-9fe4caf907cc")
)
function1 <- function(converted_Summary) {
    library(gtsummary)
   print(converted_Summary %>% summary())
    local_df <- converted_Summary %>% dplyr::select(age_at_visit_start_in_years_int,
Race,
Ethnicity,
gender_concept_name,
smoking_status,
InpatientOrED,
AKI_in_hospital,
Q_Score,
BMI,
in_death_table,
MI,
CHF,
PVD,
stroke,
dementia,
pulmonary,
rheumatic,
PUD,
liver_mild,
liversevere,
diabetes,
dmcx,
paralysis,
renal,
cancer,
mets,
hiv,
RUCA1,
RUCA2,
RUCC_2013) 
    print(table1 <- local_df %>% tbl_summary(by = in_death_table) %>% add_p())
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8c7d432f-b995-4c7f-8506-2546bf3458bb"),
    lat_lon_summary=Input(rid="ri.foundry.main.dataset.ce8c767b-3c15-44e6-a1c3-fbab8a4a1c4c")
)
heat_map_all <- function(lat_lon_summary) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
gusa <- map_data("state")
gcounty <- map_data("county")
local_df <- lat_lon_summary %>% dplyr::select(long, lat, case_count, group)
# <- left_join(gcounty, local_df, c(("long", "Longitude"),("lat", "Latitude")))
usa <- ggplot(local_df) +
    geom_polygon(aes(long, lat, group = group, fill = case_count),
                 color = "grey", size = 0.1) +
    geom_polygon(aes(long, lat, group = group),
                 fill = NA, data = gusa, color = "lightgrey") +
    coord_map("bonne", parameters=45) + ggthemes::theme_map()
plot(usa)
return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.42d7cfc4-51bd-4690-884b-7bc37c56c4d7"),
    lat_lon_summary=Input(rid="ri.foundry.main.dataset.ce8c767b-3c15-44e6-a1c3-fbab8a4a1c4c")
)
heat_map_all_copied <- function(lat_lon_summary) {
library(ggplot2)
library(wesanderson) # for color selection
library(maps)
library(dplyr)
library(ggthemes)
library(mapproj)
library(viridis)
local_df <- lat_lon_summary %>% dplyr::select(long, lat, case_count, group)
usa <- map_data("state")
gg1 <- ggplot() +
  geom_polygon(data=usa, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=local_df, aes(x=long, y=lat, size=case_count, color=case_count)) +
  scale_size_continuous(range=c(1,12)) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  theme_void() + coord_map() 

plot(gg1)
return(NULL)
}

