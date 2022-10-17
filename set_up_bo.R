library(tidyverse)
library(ggplot2)
library(RODBC)
library(DBI)
library(odbc)
library(RPostgreSQL)
library(RPostgreSQL)
library(ggforce)
library(data.table)
library(reactable)
source("./sim.R")
pw <- { "Ve@26989134"}
myconn <- DBI::dbConnect(odbc::odbc(),"Snowflake", SNOWFLAKE_UID = "eherrero@texasrangers.com", pwd=pw)
rm(pw)

pro <- "	  
SELECT 
	BATTER_TXR_PLAYER_ID AS id,
	BATTER_NAME_LAST_FIRST AS name,
	avg(BATTER_SPRINT_SPEED) AS speed,
	CAST(sum(CAST(IS_PLATE_APPEARANCE AS int))AS numeric) AS PA,
	CAST(sum(CAST(IS_SO AS int))AS numeric) /
		 			NULLIF(CAST(sum(CAST(IS_PLATE_APPEARANCE AS int)) AS NUMERIC), 0) AS K_Rate,
	CAST(sum(CAST(IS_BB AS int))AS numeric) /
		 			NULLIF(CAST(sum(CAST(IS_PLATE_APPEARANCE AS int)) AS NUMERIC), 0)  AS BB_Rate,
	CAST(sum(CAST(IS_HIT_BY_PITCH AS int))AS numeric) /
		 			NULLIF(CAST(sum(CAST(IS_PLATE_APPEARANCE AS int)) AS NUMERIC), 0)  AS hbp_Rate,
	CAST(sum(CAST(IS_IN_PLAY AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_PLATE_APPEARANCE THEN 1 ELSE 0 end) AS NUMERIC),0) AS bip_rate,
	B.GB_rate,
	B.FB_rate,
	B.LD_rate,
	B.PO_rate,
	CAST(sum(CAST(IS_HIT AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_PLATE_APPEARANCE THEN 1 ELSE 0 end) AS NUMERIC),0) AS hit_rate,
	CAST(sum(CAST(IS_2B AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_HIT THEN 1 ELSE 0 end) AS NUMERIC),0) AS double_rate,
	CAST(sum(CAST(IS_3B AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_HIT THEN 1 ELSE 0 end) AS NUMERIC),0) AS triple_rate,	
	CAST(sum(CAST(IS_HR AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_HIT THEN 1 ELSE 0 end) AS NUMERIC),0) AS hr_rate
FROM VIEW_WHEELHOUSE_PITCHES_MERGE A
JOIN (SELECT 
	BATTER_TXR_PLAYER_ID AS ID,
	BATTER_NAME_LAST_FIRST AS name,
	CAST(sum(CAST(IS_GROUNDBALL AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_IN_PLAY_OUT  THEN 1 ELSE 0 end) AS NUMERIC),0) AS GB_rate,
	CAST(sum(CAST(IS_FLYBALL AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_IN_PLAY_OUT  THEN 1 ELSE 0 end) AS NUMERIC),0) AS FB_rate,
	CAST(sum(CAST(IS_LINEDRIVE AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_IN_PLAY_OUT  THEN 1 ELSE 0 end) AS NUMERIC),0) AS LD_rate,	
	CAST(sum(CAST(IS_POPUP AS INT)) AS NUMERIC)/ 
					NULLIF(CAST(sum(CASE WHEN IS_IN_PLAY_OUT  THEN 1 ELSE 0 end) AS NUMERIC),0) AS PO_rate
FROM VIEW_WHEELHOUSE_PITCHES_MERGE 
WHERE AWAY_COMPETITION_LEVEL_ABBREV = 'MLB' AND 
	  \"YEAR\" = 2022 AND GAME_TYPE_CODE = 'R' 
GROUP BY 
	BATTER_TXR_PLAYER_ID,
	BATTER_NAME_LAST_FIRST) B ON A.BATTER_TXR_PLAYER_ID = B.ID AND A.BATTER_NAME_LAST_FIRST = B.NAME
WHERE AWAY_COMPETITION_LEVEL_ABBREV = 'MLB' AND 
	  \"YEAR\" = 2022 AND GAME_TYPE_CODE = 'R' 
GROUP BY 
	BATTER_TXR_PLAYER_ID,
	BATTER_NAME_LAST_FIRST,	
	B.GB_rate,
	B.FB_rate,
	B.LD_rate,
	B.PO_rate
HAVING PA > 50
	  "
data <- DBI::dbGetQuery(myconn, pro)

#ggplot(data, aes(SPEED)) + geom_boxplot() 
#IQR(data$SPEED)
iqr_1 = mean(data$SPEED) - IQR(data$SPEED)
iqr_3 = mean(data$SPEED) + IQR(data$SPEED)
iqr_2 = mean(data$SPEED)

data %>%
  mutate(speed_range = ifelse(SPEED > iqr_3, "very fast",
                              ifelse(between(SPEED, iqr_2, iqr_3), "fast",
                                     ifelse(between(SPEED, iqr_1, iqr_2), "slow",
                                            "very slow")))) %>%
    mutate(single_rate = 1 - DOUBLE_RATE - TRIPLE_RATE - HR_RATE)-> data


data %>% 
  filter(NAME %in% c("Semien, Marcus", "Seager, Corey","Lowe, Nathaniel", "Garcia, Adolis",
                   "Jung, Josh", "Heim, Jonah", "Taveras, Leody", "Thompson, Bubba", "Nimmo, Brandon")) -> lineup

#lineup <- data[c(507,275, 467,296,43, 155, 499, 446, 188),]

perms <- data.table(permutations(n = 9, r = 9, v = 1:9)) %>% mutate(runs = 0)
best_lineup_order <- best_lineup(lineup, perms)




