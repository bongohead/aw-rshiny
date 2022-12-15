SQLITE_DIR = 'C:/Users/Charles/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db'
XLSX_DIR = 'D:/OneDrive/__Projects/aw-rshiny/inputs.xlsx'

library(RSQLite)
library(tidyverse)
library(shiny)
library(highcharter)
library(DT)
library(lubridate)

catDf =
	readxl::read_xlsx(XLSX_DIR, sheet = 'category', na = c('', 'NA')) %>%
	rowwise(.) %>%
	dplyr::mutate(
		.,
		rgbaColor =
			grDevices::col2rgb(color)[, 1] %>%
			paste0(., collapse = ',') %>%
			paste0('rgba(', ., ',1.0)')
	) %>%
	dplyr::ungroup(.)


taskDf =
	readxl::read_xlsx(XLSX_DIR, sheet = 'task')

topLevelDomains =
	taskDf %>%
	dplyr::filter(., str_sub(task, 1, 1) == '*') %>%
	dplyr::mutate(., x = str_replace(task, coll('*.'), '')) %>%
	{c(., .$x)}

conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)

bucketDf =
	dbGetQuery(conn, 'SELECT * FROM bucketmodel')

rawDf =
	dbGetQuery(
		conn, 
		'
		SELECT eventmodel.*, bucketmodel.type AS bucket_name, bucketmodel.type AS bucket_type
		FROM eventmodel
		INNER JOIN bucketmodel
		ON eventmodel.bucket_id = bucketmodel.key
		WHERE duration >= 1 ORDER BY timestamp ASC;
		') %>%
	as_tibble(.) %>% 
	dplyr::mutate(., timestamp = lubridate::with_tz(lubridate::ymd_hms(timestamp, tz = 'UTC'), Sys.timezone())) %>%
	dplyr::mutate(., datastr = map(datastr, function(x) as_tibble(jsonlite::fromJSON(x)))) %>%
	tidyr::unnest(., datastr)

rawDf %>% dplyr::filter(., bucket_type == 'afkstatus') %>% dplyr::mutate(., time_end = timestamp + seconds(duration) - seconds(1)) %>% dplyr::group_by(bucket_name, bucket_type, timestamp, datastr) %>% dplyr::arrange(desc(duration)) %>% dplyr::summarize(., duration = head(duration, 1), time_end = head(time_end, 1)) %>% View(.)

	# There are no overlaps with currentwindow
	# Combining it with web.tab.current buckets will cause some small overlap, they are minor so just disregard
	# The more important thing is to filter out afk time
df =
	dplyr::bind_rows(
		rawDf %>% dplyr::filter(., bucket_type == 'currentwindow' & !app %in% c('firefox.exe', 'chrome.exe')),
		rawDf %>% dplyr::filter(., bucket_type == 'web.tab.current')
	)



%>%
	# dplyr::mutate(., afk = ifelse(bucket_id == 1, ifelse(datastr == '{\"status\": \"afk\"}', TRUE, FALSE), NA)) %>%
	# tidyr::fill(., afk) 
