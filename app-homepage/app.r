SQLITE_DIR = 'C:/Users/bongohead/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db'
XLSX_DIR = 'D:/OneDrive/__Projects/aw-rshiny/inputs.xlsx'

# Can comment out the ones used in call.r if used in prod
library(RSQLite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(shiny)
library(shinyjs)
library(DT)
library(highcharter)
library(lubridate)
AFK_BUCKET_ID = 1


getDomains = function(vec) {
	PROTOCOL_REGEX <- "^(?:(?:[[:alpha:]+.-]+)://)?"
	PREFIX_REGEX <- "(?:www\\.)?"
	HOSTNAME_REGEX <- "([^/]+)"
	REST_REGEX <- ".*$"
	URL_REGEX <- paste0(PROTOCOL_REGEX, PREFIX_REGEX, HOSTNAME_REGEX, REST_REGEX)
	gsub(URL_REGEX, "\\1", vec)
}


ui = tagList(
	suppressDependencies('bootstrap'),
	tags$head(
		# Note the wrapping of the string in HTML()
		tags$style(HTML("
			#settings-container table td {
				padding: 0.1rem;
				font-size: .95rem;
				text-align: left;
			}
			
			#catTable table td {
				padding: .3rem .6rem;
			}
			
			body {
				background-color: #edf2f9;
			}
			
			.container {
				max-width: 1140px !important;
			}
			
		")),
		tags$title('AW UI'),
		tags$link(rel = 'shortcut icon', href = 'favicon.png'),
		tags$script(src = 'https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/js/bootstrap.bundle.min.js'),
		tags$link(rel='stylesheet', href = 'https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css'),
		useShinyjs()
	),
	
	div(
		class = 'container-fluid p-0',
		
		div(
			class = 'container-fluid pt-3 pb-4',
			div(
				class = 'container',
				div(
					class = 'row justify-content-center',
					div(
						class = 'col-auto',
						h3('AW Interface'),
					),
					div(
						class = 'col-auto',
						selectInput(
							'freq',
							label = NULL,
							choices = list('Day' = 'd', "Week" = 'w', 'Month' = 'm', 'Year' = 'y'), selected = 'w'
						)
					)

				),
				div(
					class = 'row justify-content-center my-2',
					div(
						class = 'col-auto',
						div(
							class = 'btn-group',
							role = 'group',
							actionButton('prev', 'Back', class = 'btn-primary'),
							actionButton('currentdate', 'Current Date', class = 'btn-primary disabled'),
							actionButton('fwd', 'Next', class = 'btn-primary')
						)
						
					)
				),
				div(
					class = 'row justify-content-center',
					div(class = 'col-sm-12 col-md-12 col-lg-12 col-xl-12', highchartOutput('tsPlot', height = 300)),
				)
				
			)
		),
		# div(
		# 	class = 'container',
		# 	div(
		# 		class = 'row justify-content-center my-4',
		# 		div(class = 'col-sm-12 col-md-12 col-lg-12 col-xl-12', highchartOutput('tsPlot', height = 150)),
		# 	)
		# ),

		div(
			class = 'container-fluid mt-4',
			style = 'background-image: linear-gradient(to right bottom, rgb(255, 255, 255), rgba(229, 231, 235, 0.2), rgb(255, 255, 255))',

			div(
				class= 'container',
				hr(),
				h2('Data'),
				
				div(
					class = 'row my-4',
					div(class = 'col-md-12 col-lg-7 col-xxl-8', highchartOutput('catPlot', height = 300)),
					div(class = 'col-md-12 col-lg-5 col-xxl-4', style = 'font-size: .85rem', dataTableOutput('catTable')),
				),
				div(
					class = 'row justify-content-center',
					div(class = 'col-md-12 col-lg-5', dataTableOutput('taskTable')),
					div(class = 'col-md-12 col-lg-7', highchartOutput(outputId = "taskPlot")),

					# highchartOutput(outputId = "taskPlot"),
					# div(class = 'col-auto', dataTableOutput('taskTable'))
				)
			)
		),
		hr(),
		div(
			id = 'settings-container',
			class = 'container',
			h3('Settings'),
			div(
				class = 'row justify-content-center',
				div(class = 'col-auto mx-3', dataTableOutput('catParams')),
				div(class = 'col-auto mx-3', dataTableOutput('taskParams')),
				)
			)
		)
	
)

#' Helper functions

#' Takes a date/datetime start date argument and rolls it forward/back by one period,
#' and provides an end date one period after. Useful for going back/forwards.
get_shifted_start_end_dates = function(start_date, freq, forward = T) {
	
	if (forward == F) {
		new_start_date = start_date %>% {
			if (freq == 'd') . - days(1)
			else if (freq == 'w') . - days(7)
			else if (freq == 'm') add_with_rollback(., months(-1), roll_to_first = TRUE)
			else if (freq == 'y') add_with_rollback(., years(-1), roll_to_first = TRUE)
		}
	} else {
		new_start_date = start_date %>% {
			if (freq == 'd') . + days(1)
			else if (freq == 'w') . + days(7)
			else if (freq == 'm') add_with_rollback(., months(1), roll_to_first = TRUE)
			else if (freq == 'y') add_with_rollback(., years(1), roll_to_first = TRUE)
		}
	}
	new_end_date = new_start_date %>% {
		if (freq == 'd') . + hours(24)
		else if (freq == 'w') ceiling_date(., 'week', week_start = 1) + hours(4)
		else if (freq == 'm') ceiling_date(., 'month') + hours(4)
		else if (freq == 'y') ceiling_date(., 'year') + hours(4)
	}
	

	return(list(start_date = new_start_date, end_date = new_end_date))
}

#' Get a dataframe split into its subfrequency-level rows, given a subfrequency and a date/datetime range.
get_df_split_by_subfreq = function(subfreq, start_date, end_date) {
	subfreq_df = {
		if (subfreq%in% c('md', 'yd'))
			tibble(
				order = 1:length(seq(start_date, end_date, by = '1 day')),
				time_label = seq(start_date, end_date, by = '1 day') %>% format(., '%m/%d'),
				start = seq(start_date, end_date, by = '1 day'),
				end = seq(start_date, end_date, by = '1 day') + days(1) - seconds(1)
			)
		else if (subfreq == 'd')
			# Floro_dates and hours(4) adjustments are to handle DST
			tibble(
				order = 1:7,
				time_label = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'),
				start = seq(floor_date(start_date, 'day'), floor_date(end_date, 'day') - days(1), by = '1 day') + hours(4),
				end = seq(floor_date(start_date, 'day'), floor_date(end_date, 'day') - days(1), by = '1 day') + days(1) - seconds(1) + hours(4)
			)
		else if (subfreq == 'h')
			tibble(
				order = 1:24,
				time_label =
					seq(start_date, end_date, by = '1 hour') %>%
					head(., -1) %>%
					{paste0(as.numeric(format(., '%I')), str_to_lower(str_sub(format(., '%p'), 1, 1)))},
				start = seq(start_date, end_date, by = '1 hour') %>% head(., -1),
				end = (seq(start_date, end_date, by = '1 hour') + hours(1) - seconds(1)) %>% head(., -1)
			)
		else NULL
	}
	
	return(subfreq_df)
}

server = function(input, output, session) {

	catDf =
		readxl::read_xlsx(XLSX_DIR, sheet = 'category', na = c('', 'NA')) %>%
		rowwise(.) %>%
		mutate(
			.,
			rgbaColor =
				grDevices::col2rgb(color)[, 1] %>%
				paste0(., collapse = ',') %>%
				paste0('rgba(', ., ',1.0)')
		) %>%
		ungroup(.)
	
	taskDf = readxl::read_xlsx(XLSX_DIR, sheet = 'task')
	
	topLevelDomains =
		taskDf %>%
		filter(., str_sub(task, 1, 1) == '*') %>%
		mutate(., x = str_replace(task, coll('*.'), '')) %>%
		.$x
		#{c(., .$x)}
	
	# Debug
	# input = list(freq = 'w')
	# state = list()
	state = reactiveValues(
		freq = NULL,
		dateMax = NULL,
		subFreq = NULL,
		subFreqDf = NULL
		)
	# Uncomment below for debug
	# input = list(
	# 	freq = 'w'
	# 	)
	# state = list(
	# 	freq = NULL,
	# 	dateMax = NULL,
	# 	subFreq = NULL,
	# 	subFreqDf = NULL
	# )
	
	
	observeEvent(input$freq, {
		
		state$freq = input$freq

		state$dateMax = {
			if (state$freq == 'd') ceiling_date(now() - hours(4), 'day') + hours(4)
			else if (state$freq == 'w') as_datetime(ceiling_date(now() - hours(4), 'week', week_start = 1), tz = Sys.timezone()) + hours(4)
			else if (state$freq == 'm') as_datetime(ceiling_date(now() - hours(4), 'month'), tz = Sys.timezone()) + hours(4)
			else if (state$freq == 'y') as_datetime(ceiling_date(now() - hours(4), 'year'), tz = Sys.timezone()) + hours(4)
		}
		state$dateMin = {
			if (state$freq == 'd') floor_date(now() - hours(4), 'day') + hours(4)
			else if (state$freq == 'w') as_datetime(floor_date(now() - hours(4), 'week', week_start = 1), tz = Sys.timezone()) + hours(4)
			else if (state$freq == 'm') as_datetime(floor_date(now() - hours(4), 'month'), tz = Sys.timezone()) + hours(4)
			else if (state$freq == 'y') as_datetime(floor_date(now() - hours(4), 'year'), tz = Sys.timezone()) + hours(4)
		}
		state$subFreq = {
			if (state$freq == 'y') 'yd'
			else if (state$freq == 'm') 'md'
			else if (state$freq == 'w') 'd'
			else if (state$freq == 'd') 'h'
		}
		
		state$subFreqDf = get_df_split_by_subfreq(state$subFreq, state$dateMin, state$dateMax)
		
		updateActionButton(
			session,
			'currentdate',
			label =
				paste0(
					{
						if (state$freq == 'd') format(state$dateMin, '%A %B %d')
						else if (state$freq == 'w') paste0(format(state$dateMin, '%A %b %d'), ' - ', format(state$dateMax - lubridate::days(1), '%A %b %d'))
						else if (state$freq == 'm') paste0(format(state$dateMin, '%B %Y'))
						else if (state$freq == 'y') paste0(format(state$dateMin, '%Y'))
					},
					' (', toupper(state$freq), ')'
				)
			)
		
	}, ignoreInit = FALSE)
	
	
	
	observeEvent(input$prev, {
		shifted_dates = get_shifted_start_end_dates(state$dateMin, state$freq, forward = F)
		state$dateMin = shifted_dates$start_date 
		state$dateMax = shifted_dates$end_date
		state$subFreqDf = get_df_split_by_subfreq(state$subFreq, state$dateMin, state$dateMax)
	}, ignoreInit = TRUE)
	
	
	observeEvent(input$fwd, {
		shifted_dates = get_shifted_start_end_dates(state$dateMin, state$freq, forward = T)
		state$dateMin = shifted_dates$start_date
		state$dateMax = shifted_dates$end_date
		state$subFreqDf = get_df_split_by_subfreq(state$subFreq, state$dateMin, state$dateMax)
	}, ignoreInit = TRUE)
	
	
	# Update displayed date 
	observeEvent(c(input$fwd, input$prev, input$freq), {
		

		updateActionButton(
			session,
			'currentdate',
			label =
				paste0(
					{
						if (state$freq == 'd') format(state$dateMin, '%A %B %d')
						else if (state$freq == 'w') paste0(
							format(state$dateMin, '%A %b %d'), ' - ', format(state$dateMax - days(1), '%A %b %d')
							)
						else if (state$freq %in% c('m', 'y')) paste0(format(state$dateMin, '%B %Y'))
					},
					' (', toupper(state$freq), ')'
				)
		)
		
		# Hide when coutns too low
		counts = get_counts()
		print(counts)
		{
			if (counts$prev_count == 0) shinyjs::disable("prev")
			else shinyjs::enable('prev')
		}
		{
			if (counts$next_count == 0) shinyjs::disable("fwd")
			else shinyjs::enable('fwd')
		}
		
		
	}, ignoreInit = FALSE)
	

	# Get SQL data
	get_raw_df = reactive({
		
		conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)
		
		raw_df =
			dbGetQuery(
				conn,
				str_glue(
					"SELECT
						id, bucket_id,
						DATETIME(timestamp, 'localtime') AS timestamp, -- Convert UTC to local
						duration, datastr
					FROM eventmodel
					WHERE
						duration >= 1
						AND DATETIME(timestamp, 'localtime') BETWEEN '{state$dateMin}' AND '{state$dateMax}'
					ORDER BY timestamp ASC"
					)
				) %>%
			as_tibble(.) %>%
			mutate(., afk = ifelse(bucket_id == AFK_BUCKET_ID, ifelse(datastr == '{\"status\": \"afk\"}', T, F), NA)) %>%
			fill(., afk) %>%
			mutate(., timestamp = with_tz(timestamp, Sys.timezone())) 
		
		return(raw_df)
	})
	
	get_counts = reactive({
		
		conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)
		
		prev_period = get_shifted_start_end_dates(state$dateMin, state$freq, forward = F)
		this_period = list(start_date = state$dateMin, end_date = state$dateMax)
		next_period = get_shifted_start_end_dates(state$dateMin, state$freq, forward = T)
		
		counts_df = dbGetQuery(
			conn,
			str_glue(
				"SELECT
					CASE 
						WHEN DATETIME(timestamp, 'localtime') < '{this_period$start_date}' THEN 'prev_count'
						WHEN DATETIME(timestamp, 'localtime') < '{next_period$start_date}' THEN 'this_count'
						ELSE 'next_count'
					END AS time_group,
					COUNT(*) AS count  
				FROM eventmodel
				WHERE DATETIME(timestamp, 'localtime') BETWEEN '{prev_period$start_date}' AND '{next_period$end_date}'
				GROUP BY time_group
				ORDER BY timestamp ASC"
				)
			)
		
		prev_count = filter(counts_df, time_group == 'prev_count') %>% {if (nrow(.) == 0) 0  else .$count}
		this_count = filter(counts_df, time_group == 'this_count') %>% {if (nrow(.) == 0) 0  else .$count}
		next_count = filter(counts_df, time_group == 'next_count') %>% {if (nrow(.) == 0) 0  else .$count}
		
		counts = list(prev_count = prev_count, this_count = this_count, next_count = next_count)
		
		return(counts)
	})
	
	
	# Return list of fully aggregated df (taskTimeDf) and daily grouped df (subTaskTimeDf)
	get_task_time_df = reactive({
		
		raw_df = get_raw_df()
		
		# Added 1/21/22 
		# Fast CPP Parser
		# Significantly faster than previous code
		json_parsed_data =
			RcppSimdJson::fparse(raw_df$datastr, empty_array = NA, empty_object = NA, single_null = NA) %>%
			purrr::map(., function(x)
				list(
					app = {if (!is.null(x$app)) x$app else NA},
					title = {if (!is.null(x$title)) x$title else NA},
					url = {if (!is.null(x$url)) x$url else NA}
				)
			) %>%
			purrr::transpose(.) %>%
			map(., unlist) %>%
			as_tibble(.)
		
		taskTimeDf0 =
			raw_df %>%
			bind_cols(., json_parsed_data) %>%
			filter(., (bucket_id == AFK_BUCKET_ID & afk == TRUE) | bucket_id != AFK_BUCKET_ID & duration != 0) %>%
			# Replaced with json_parsed_data
			# rowwise(.) %>%
			# dplyr::mutate(., datastr = map(datastr, function(x) as_tibble(jsonlite::fromJSON(x)))) %>%
			# tidyr::unnest(., datastr) %>%
 			# Replace entries that are x.domain.com with domain.com if in topLevelDomains
			mutate(
				.,
				url = getDomains(url),
				url2 = ifelse(str_count(url, coll('.')) == 2, str_sub(str_replace(url, '[^.]+', ''), 2), url),
				url = ifelse(url2 %in% topLevelDomains, url2, url)
			) %>%
			select(., -url2) %>%
			# Get rid of firefox due to overlapping with url entries and AFK entries
			filter(., (is.na(app) | app != 'firefox.exe')) %>%
			# Now shortern the duration of any entries where the time overlaps with the second entry (this seems to be an AW bug)
			arrange(., timestamp) %>%
			mutate(., timeEnded = timestamp + seconds(duration)) %>%
			mutate(., duration = ifelse(timeEnded > dplyr::lead(timestamp, 1) & !is.na(dplyr::lead(timestamp, 1)), dplyr::lead(timestamp, 1) - timestamp, duration)) %>%
			# This gets rid of remaining afk entries (some will be marked after afk has already started, these are not removed in the previous step)
			filter(., afk == FALSE) %>%
			# Order desc
			arrange(., desc(timestamp)) %>%
			mutate(., task = ifelse(is.na(url), app, url))
		
		
		# Aggregate up to subfrequency and clean
		subTaskTimeDf =
			# 12/15/22 replaced manually split and join with dplyr equi-join. Improved ms speed 200x on test set
			# state$subFreqDf %>%
			# group_by(., order) %>%
			# group_split(.) %>%
			# lapply(., function(subFreqDf)
			# 	filter(taskTimeDf0, timestamp >= subFreqDf$start & timestamp <= subFreqDf$end) %>%
			# 		bind_cols(., subFreqDf[, c('order', 'time_label')])
			# 	) %>%
			# bind_rows(.) %>%
			taskTimeDf0 %>% inner_join(., state$subFreqDf, by = join_by(timestamp >= start, timestamp <= end)) %>%
			select(., -start, -end) %>%
			group_by(., task, order, time_label) %>%
			summarize(., minutes = round(sum(duration)/60), .groups = 'drop') %>%
			mutate(., hours = round(minutes/60, 2)) %>%
			left_join(., mutate(taskDf, task = str_replace(task, coll('*.'), '')), by = 'task') %>%
			mutate(., category = ifelse(is.na(category), 'unknown', category)) %>%
			arrange(., desc(minutes)) %>%
			filter(., task != 'LockApp.exe')
		
		taskTimeDf =
			subTaskTimeDf %>%
			group_by(., task) %>%
			summarize(., minutes = sum(minutes), hours = sum(hours), category = head(category, 1)) %>%
			arrange(., desc(minutes))
			
		return(list(taskTimeDf = taskTimeDf, subTaskTimeDf = subTaskTimeDf))
	})
	
	
	get_cat_time_df = reactive({
		
		task_time_df = get_task_time_df()
		
		subTaskTimeDf = task_time_df$subTaskTimeDf

		subCatTimeDf =
			subTaskTimeDf %>%
			group_by(., category, time_label) %>%
			summarize(., minutes = sum(minutes), .groups = 'drop') %>% 
			mutate(., hours = round(minutes/60, 2)) %>%
			left_join(., catDf, by = 'category')
		
		catTimeDf =
			subCatTimeDf %>%
			group_by(., category, name, prod_index, parent_category, color, rgbaColor) %>%
			summarize(., minutes = sum(minutes), hours = sum(hours), .groups = 'drop')

		return(list(subCatTimeDf = subCatTimeDf, catTimeDf = catTimeDf))
	})
	
	
	# getProdTimeDf = reactive({
	# 	
	# 	catTimeDf = getCatTimeDf()$catTimeDf
	# 	
	# 	prodTimeDf =
	# 		catTimeDf %>%
	# 		dplyr::group_by(., prod_index) %>%
	# 		dplyr::summarize(., minutes = sum(minutes)) %>%
	# 		dplyr::mutate(., hours = round(minutes/60, 2))
	# 	
	# 	return(prodTimeDf)
	# 	
	# })
	
	
	
	# output$prodPlot =
	# 	renderHighchart({
	# 		chartDf =
	# 			getProdTimeDf() %>%
	# 			dplyr::mutate(., time = {if (max(.$minutes) >= 120) hours else minutes})
	# 		
	# 		textStr = {if (max(chartDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
	# 		
	# 		highchart() %>%
	# 			hc_add_series(
	# 				.,
	# 				type = 'column',
	# 				data = chartDf,
	# 				mapping = hcaes(x = prod_index, y = time, color = prod_index),
	# 				dataLabels = list(enabled = TRUE)
	# 				) %>%
	# 			hc_xAxis(categories = chartDf$prod_index) %>%
	# 			hc_yAxis(title = list(text = textStr)) %>%
	# 			hc_size(height = 200) %>%
	# 			hc_legend(enabled = FALSE) %>%
	# 			hc_add_theme(hc_theme_ft())
	# 	})
	
	output$tsPlot =  renderHighchart({
		# https://stackoverflow.com/questions/46671973/highcharter-stacked-column-groupings-not-using-hchart
		cat_time_df = get_cat_time_df()
		subCatTimeDf = cat_time_df$subCatTimeDf
		
		series_list =
			expand_grid(category = catDf$category, time_label = state$subFreqDf$time_label) %>%
			left_join(
				.,
				subCatTimeDf[, c('category', 'time_label', 'minutes', 'hours')],
				by = c('time_label', 'category')
			) %>%
			left_join(., catDf, by = 'category') %>%
			mutate(
				.,
				minutes = ifelse(is.na(minutes), 0, minutes),
				hours = ifelse(is.na(hours), 0, hours)
			) %>%
			group_by(., category, name, color) %>%
			group_split(.) %>%
			lapply(., function(df)
				list(
					name = head(df$name, 1),
					type = 'column',
					color = head(df$color, 1),
					data = purrr::transpose(df) %>% purrr::map(., ~ list(.$time_label, {if (max(subCatTimeDf$minutes) >= 120) .$hours else .$minutes}))
				)
			)
		
		textStr = {if (max(subCatTimeDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
		
		highchart() %>%
			hc_chart(borderColor = 'black', borderWidth = 1, borderRadius = 5) %>%
			hc_add_series_list(series_list) %>%
			hc_xAxis(., categories = state$subFreqDf$time_label) %>%
			hc_plotOptions(column = list(stacking = "normal")) %>%
			hc_legend(enabled = FALSE) %>%
			hc_add_theme(hc_theme_bloom())
		})
	
	output$catPlot = renderHighchart({
			
		catTimeDf = get_cat_time_df()$catTimeDf
		
		chartDf =
			catTimeDf %>%
			dplyr::mutate(., time = {if (max(.$minutes) >= 120) hours else minutes})
		
		textStr = {if (max(chartDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
		
		chart_data = imap(purrr::transpose(chartDf), function(x, i) list(x = i - 1, y = x$time, color = x$color, name = x$name))
		
		highchart() %>%
			hc_add_series(type = 'bar', data = chart_data) %>%
			hc_xAxis(categories = map_chr(chart_data, ~ .$name)) %>%
			hc_yAxis(title = list(text = textStr)) %>%
			hc_legend(enabled = FALSE) %>%
			hc_title(text = 'Categories') %>%
			hc_add_theme(hc_theme_bloom())
		})


	output$catTable = renderDT({
			
		catTimeDf = get_cat_time_df()$catTimeDf
		
		datatable(
			catTimeDf %>%
				rowwise(.) %>%
				dplyr::mutate(
					.,
					minutes = lubridate::dminutes(minutes),
					time = c(minutes %/% lubridate::dhours(1), minutes %/% lubridate::dminutes(1) %% 60) %>% str_pad(., 2, pad = '0') %>% paste0(., collapse = ':')
					) %>%
				dplyr::ungroup(.) %>%
				dplyr::select(., name, time),
			colnames = c('Category' = 'name', 'Duration' = 'time'),
			options = list(
				pageLength = 10,
				dom = paste0(
					"<'row justify-content-end'<'col-auto'>>",
					"<'row justify-content-center'<'col-12'tr>>",
					"<'row justify-content-end'<'col-auto'>>"
					),
				order = list(list(1, 'desc'))
				),
			rownames = FALSE
			)
		})
	

	output$taskPlot = renderHighchart2({
			
		taskTimeDf = get_task_time_df()$taskTimeDf
		
		chartDf =
			taskTimeDf %>%
			head(., 10) %>%
			dplyr::left_join(., catDf, by = 'category') %>%
			dplyr::mutate(., time = {if (max(.$minutes) >= 120) hours else minutes})

		textStr = {if (max(chartDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
		
		chart_data = imap(purrr::transpose(chartDf), function(x, i) list(x = i - 1, y = x$time, color = x$color, name = x$task))
		
		highchart() %>%
			hc_add_series(type = 'column', data = chart_data) %>%
			hc_xAxis(categories = map_chr(chart_data, ~ .$name)) %>%
			hc_yAxis(title = list(text = textStr)) %>%
			hc_legend(enabled = FALSE) %>%
			hc_title(text = 'Top 10 Tasks') %>%
			hc_add_theme(hc_theme_bloom())
		})

	output$taskTable = DT::renderDT({
			
		taskTimeDf = get_task_time_df()$taskTimeDf
		
		datatable(
			taskTimeDf %>%
				dplyr::select(., task, category, minutes),
			colnames = c('Task' = 'task', 'Category' = 'category', 'Duration (Minutes)' = 'minutes'),
			editable = 'row',
			options = list(
				dom = paste0(
					"<'row justify-content-end'<'col-auto'>>",
					"<'row justify-content-center'<'col-12'tr>>",
					"<'row justify-content-end'<'col-auto'p>>"
					),
				order = list(list(2, 'desc')),
				columnDefs = list(
					)
				),
			rownames = FALSE
			) %>%
			formatStyle(
				.,
				'Category',
				target = 'row',
				backgroundColor =
					styleEqual(
						catDf$category,
						# Convert colors to RGB hex
						catDf %>%
							rowwise(.) %>%
							dplyr::mutate(
								.,
								color =
									grDevices::col2rgb(color)[, 1] %>%
									paste0(., collapse = ',') %>%
									paste0('rgba(', ., ',.5)')
								) %>%
							.$color
					)
				)
		})
	

	
	output$taskParams = DT::renderDT({
		datatable(
			taskDf,
			options = list(
				dom = paste0(
					"<'row justify-content-end'<'col-auto'>>",
					"<'row justify-content-center'<'col-12'tr>>",
					"<'row justify-content-end'<'col-auto'p>>"
				),
				order = list(list(1, 'desc')),
				columnDefs = list(
				)
			),
			rownames = FALSE
			)
		})
	
	output$catParams = DT::renderDT({
		datatable(
			catDf,
			options = list(
				dom = paste0(
					"<'row justify-content-end'<'col-auto'>>",
					"<'row justify-content-center'<'col-12'tr>>",
					"<'row justify-content-end'<'col-auto'p>>"
				),
				order = list(list(1, 'desc')),
				columnDefs = list(
				)
			),
			rownames = FALSE
			)
		})
	

}



shinyApp(ui, server)