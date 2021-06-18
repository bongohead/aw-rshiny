SQLITE_DIR = 'C:/Users/Charles/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db'
XLSX_DIR = 'D:/OneDrive/__Projects/aw-rshiny/inputs.xlsx'

library(RSQLite)
library(tidyverse)
library(shiny)
library(highcharter)
library(DT)
library(lubridate)

 
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
		")),
		tags$title('AW UI'),
		tags$link(rel = 'shortcut icon', href = 'https://test.macrodawg.com/dogwake.png'),
		tags$script(src = 'https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/js/bootstrap.min.js'),
		tags$link(rel='stylesheet', href = 'https://econforecasting.com/static/style-bs.css')
		
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
						h3('ActivityWatch Interface'),
					),
					div(
						class = 'col-auto',
						selectInput(
							'freq',
							label = NULL,
							choices = list('Hour' = 'h', 'Day' = 'd', "Week" = 'w', 'Month' = 'm'), selected = 'w'
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
					div(class = 'col-sm-12 col-md-12 col-lg-12 col-xl-12', highchartOutput('tsPlot', height = 150)),
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
			class = 'container-fluid',
			style = 'height:2rem;background-image:linear-gradient(45deg, var(--bs-econgreen) 50%, var(--bs-econpale) 50%)',
			div(
				class = 'container',
				h4('Data', style = 'color:white')
			)
		),
		div(
			class = 'container-fluid py-4',
			style = 'background-image: linear-gradient(165deg, rgba(235, 245, 247, .5) 50%, rgba(255, 255, 255, .3) 50%)',
			div(
				class= 'container',
				div(
					class = 'row',
					div(class = 'col-md-12 col-lg-6', highchartOutput('catPlot', height = 300)),
					div(class = 'col-md-12 col-lg-6', style = 'font-size: .75rem', dataTableOutput('catTable')),
				),
				div(
					class = 'row justify-content-center',
					highchartOutput(outputId = "taskPlot"),
					div(class = 'col-auto', dataTableOutput('taskTable'))
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


server = function(input, output, session) {

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
	
	
	
	observeEvent(input$freq, {
		
		state$freq = input$freq

		state$dateMax = {
			if (state$freq == 'h') lubridate::as_datetime(Sys.time(), tz = Sys.timezone())
			else if (state$freq == 'd') lubridate::ceiling_date(Sys.time() - lubridate::hours(4), 'day') + lubridate::hours(4)
			else if (state$freq == 'w') lubridate::as_datetime(lubridate::ceiling_date(Sys.time(), 'week', week_start = 1), tz = Sys.timezone()) + lubridate::hours(4)
			else if (state$freq == 'm') lubridate::as_datetime(lubridate::ceiling_date(Sys.time(), 'month'), tz = Sys.timezone()) + lubridate::hours(4)
		}
		state$dateMin = {
			if (state$freq == 'h') lubridate::as_datetime(Sys.time() - lubridate::hours(1), tz = Sys.timezone())
			else if (state$freq == 'd') lubridate::floor_date(Sys.time() - lubridate::hours(4), 'day') + lubridate::hours(4)
			else if (state$freq == 'w') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'week', week_start = 1), tz = Sys.timezone()) + lubridate::hours(4)
			else if (state$freq == 'm') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'month'), tz = Sys.timezone()) + lubridate::hours(4)
		}
		
		state$subFreq = {
			if (state$freq == 'm') 'md'
			else if (state$freq == 'w') 'd'
			else if (state$freq == 'd') 'h'
			else if (state$freq == 'h') 'm'
		}
		
		state$subFreqDf = {
			if (state$subFreq == 'md')
				tibble(
					order = 1:length(seq(state$dateMin, state$dateMax, by = '1 day')),
					time_label = seq(state$dateMin, state$dateMax, by = '1 day') %>% format(., '%m/%d'),
					start = seq(state$dateMin, state$dateMax, by = '1 day'),
					end = seq(state$dateMin, state$dateMax, by = '1 day') + days(1) - seconds(1)
				)
			
			else if (state$subFreq == 'd')
				tibble(
					order = 1:7,
					time_label = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'),
					start = seq(state$dateMin, state$dateMax - lubridate::days(1), by = '1 day'),
					end = seq(state$dateMin, state$dateMax - lubridate::days(1), by = '1 day') + days(1) - seconds(1)
				)
			
			else if (state$subFreq == 'h')
				tibble(
					order = 1:24,
					time_label = seq(state$dateMin, state$dateMax, by = '1 hour') %>% head(., -1) %>% {paste0(as.numeric(format(., '%I')), str_to_lower(str_sub(format(., '%p'), 1, 1)))},
					start = seq(state$dateMin, state$dateMax, by = '1 hour') %>% head(., -1),
					end = (seq(state$dateMin, state$dateMax, by = '1 hour') + hours(1) - seconds(1))%>% head(., -1)
				)
			else if (state$subFreq == 'm')
				tibble(
					order = 1:60,
					time_label = seq(state$dateMin, state$dateMax, by = '1 min') %>% format(., '%I:%M'),
					start = seq(state$dateMin, state$dateMax, by = '1 min'),
					end = seq(state$dateMin, state$dateMax, by = '1 min') + minutes(1) - seconds(1)
				)
			
			else NULL
		}
		
		updateActionButton(
			session,
			'currentdate',
			label =
				paste0(
					{
						if (state$freq == 'h') paste0(format(state$dateMin, '%I:%M%p'), ' - ', format(state$dateMax, '%I:%M%p'))
						else if (state$freq == 'd') format(state$dateMin, '%A %B %d')
						else if (state$freq == 'w') paste0(format(state$dateMin, '%A %b %d'), ' - ', format(state$dateMax - lubridate::days(1), '%A %b %d'))
						else if (state$freq == 'm') paste0(format(state$dateMin, '%m/%d/%Y'))
					},
					' (', toupper(state$freq), ')'
				)
			)
		
	}, ignoreInit = FALSE)
	
	
	observeEvent(input$prev, {
		state$dateMin = state$dateMin %>% {
			if (state$freq == 'h') . - lubridate::hours(1)
			else if (state$freq == 'd') . - lubridate::days(1)
			else if (state$freq == 'w') . - lubridate::days(7)
			else if (state$freq == 'm') lubridate::add_with_rollback(., lubridate::months(- 1), roll_to_first = TRUE)
		}
		
		state$dateMax = state$dateMin %>% {
			if (state$freq == 'h') . + lubridate::hours(1)
			else if (state$freq == 'd') . + lubridate::hours(24)
			else if (state$freq == 'w') lubridate::ceiling_date(., 'week', week_start = 1)
			else if (state$freq == 'm') lubridate::ceiling_date(., 'month')
		}
		
		# Also update subfreqDf - same code as previous
		state$subFreqDf = {
			if (state$subFreq == 'md')
				tibble(
					order = 1:length(seq(state$dateMin, state$dateMax, by = '1 day')),
					time_label = seq(state$dateMin, state$dateMax, by = '1 day') %>% format(., '%m/%d'),
					start = seq(state$dateMin, state$dateMax, by = '1 day'),
					end = seq(state$dateMin, state$dateMax, by = '1 day') + days(1) - seconds(1)
				)
			
			else if (state$subFreq == 'd')
				tibble(
					order = 1:7,
					time_label = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'),
					start = seq(state$dateMin, state$dateMax - lubridate::days(1), by = '1 day'),
					end = seq(state$dateMin, state$dateMax - lubridate::days(1), by = '1 day') + days(1) - seconds(1)
				)
			
			else if (state$subFreq == 'h')
				tibble(
					order = 1:24,
					time_label = seq(state$dateMin, state$dateMax, by = '1 hour') %>% head(., -1) %>% {paste0(as.numeric(format(., '%I')), str_to_lower(str_sub(format(., '%p'), 1, 1)))},
					start = seq(state$dateMin, state$dateMax, by = '1 hour') %>% head(., -1),
					end = (seq(state$dateMin, state$dateMax, by = '1 hour') + hours(1) - seconds(1))%>% head(., -1)
				)
			else if (state$subFreq == 'm')
				tibble(
					order = 1:60,
					time_label = seq(state$dateMin, state$dateMax, by = '1 min') %>% format(., '%I:%M'),
					start = seq(state$dateMin, state$dateMax, by = '1 min'),
					end = seq(state$dateMin, state$dateMax, by = '1 min') + minutes(1) - seconds(1)
				)
			
			else NULL
		}
		
	}, ignoreInit = TRUE)
	
	
	observeEvent(input$fwd, {
		state$dateMin = state$dateMin %>% {
			if (state$freq == 'h') . + lubridate::hours(1)
			else if (state$freq == 'd') . + lubridate::days(1)
			else if (state$freq == 'w') . + lubridate::days(7)
			else if (state$freq == 'm') lubridate::add_with_rollback(., lubridate::months(- 1), roll_to_first = TRUE)
		}
		
		state$dateMax = state$dateMin %>% {
			if (state$freq == 'h') . + lubridate::hours(1)
			else if (state$freq == 'd') . + lubridate::hours(24)
			else if (state$freq == 'w') lubridate::ceiling_date(., 'week', week_start = 1)
			else if (state$freq == 'm') lubridate::ceiling_date(., 'month')
		}
	}, ignoreInit = TRUE)
	
	
	# Update displayed date 
	observeEvent(c(input$fwd, input$prev, input$freq), {
		updateActionButton(
			session,
			'currentdate',
			label =
				paste0(
					{
						if (state$freq == 'h') paste0(format(state$dateMin, '%I:%M%p'), ' - ', format(state$dateMax, '%I:%M%p'))
						else if (state$freq == 'd') format(state$dateMin, '%A %B %d')
						else if (state$freq == 'w') paste0(format(state$dateMin, '%A %b %d'), ' - ', format(state$dateMax - lubridate::days(1), '%A %b %d'))
						else if (state$freq == 'm') paste0(format(state$dateMin, '%m/%d/%Y'))
					},
					' (', toupper(state$freq), ')'
				)
		)
	}, ignoreInit = FALSE)
	
	
	# Get SQL data
	getRawDf = reactive({
		conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)
		rawDf =
			dbGetQuery(conn, 'SELECT * FROM eventmodel WHERE duration >= 1 ORDER BY timestamp ASC') %>%
			as_tibble(.) %>%
			dplyr::mutate(., afk = ifelse(bucket_id == 3, ifelse(datastr == '{\"status\": \"afk\"}', TRUE, FALSE), NA)) %>%
			tidyr::fill(., afk) 
		return(rawDf)
	})
	
	# Filter data by date range
	getFilteredDf = reactive({
		
		rawDf = getRawDf()

		filteredDf =
			rawDf %>%
			# Adjust to system timezone
			dplyr::mutate(., timestamp = lubridate::with_tz(lubridate::ymd_hms(timestamp, tz = 'UTC'), Sys.timezone())) %>%
			dplyr::filter(., timestamp >= state$dateMin & timestamp <= state$dateMax)
		
		return(filteredDf)
	})
	
	
	# Return list of fully aggregated df (taskTimeDf) and daily grouped df (subTaskTimeDf)
	getTaskTimeDf = reactive({
		
		filteredDf = getFilteredDf()
		
		taskTimeDf0 =
			filteredDf %>%
			dplyr::filter(., (bucket_id == 3 & afk == TRUE) | bucket_id != 3 & duration != 0) %>%
			rowwise(.) %>%
			dplyr::mutate(., datastr = map(datastr, function(x) as_tibble(jsonlite::fromJSON(x)))) %>%
			tidyr::unnest(., datastr) %>%

			 			# Replace entries that are x.domain.com with domain.com if in topLevelDomains
			dplyr::mutate(
				.,
				url = getDomains(url),
				url2 = ifelse(str_count(url, coll('.')) == 2, str_sub(str_replace(url, '[^.]+', ''), 2), url),
				url = ifelse(url2 %in% topLevelDomains, url2, url)
			) %>%
			dplyr::select(., -url2) %>%
			# Get rid of firefox due to overlapping with url entries and AFK entries
			dplyr::filter(., (is.na(app) | app != 'firefox.exe')) %>%
			# Now shortern the duration of any entries where the time overlaps with the second entry (this seems to be an AW bug)
			dplyr::arrange(., timestamp) %>%
			dplyr::mutate(., timeEnded = timestamp + lubridate::seconds(duration)) %>%
			dplyr::mutate(., duration = ifelse(timeEnded > dplyr::lead(timestamp, 1) & !is.na(dplyr::lead(timestamp, 1)), dplyr::lead(timestamp, 1) - timestamp, duration)) %>%
			# This gets rid of remaining afk entries (some will be marked after afk has already started, these are not removed in the previous step)
			dplyr::filter(., afk == FALSE) %>%
			# Order desc
			dplyr::arrange(., desc(timestamp)) %>%
			dplyr::mutate(., task = ifelse(is.na(url), app, url))
		
		
		# Aggregate up to subfrequency and clean
		subTaskTimeDf =
			state$subFreqDf %>%
			dplyr::group_by(., order) %>%
			dplyr::group_split(.) %>%
			lapply(., function(subFreqDf)
				dplyr::filter(taskTimeDf0, timestamp >= subFreqDf$start & timestamp <= subFreqDf$end) %>%
					dplyr::bind_cols(., subFreqDf[, c('order', 'time_label')])
				) %>%
			dplyr::bind_rows(.) %>%
			dplyr::group_by(
				.,
				task,
				order,
				time_label,
				) %>%
			dplyr::summarize(., minutes = round(sum(duration)/60), .groups = 'drop') %>%
			dplyr::mutate(., hours = round(minutes/60, 2)) %>%
			dplyr::left_join(., taskDf %>% dplyr::mutate(., task = str_replace(task, coll('*.'), '')), by = 'task') %>%
			dplyr::mutate(., category = ifelse(is.na(category), 'unknown', category)) %>%
			dplyr::arrange(., desc(minutes)) %>%
			dplyr::filter(., task != 'LockApp.exe')
		
		taskTimeDf =
			subTaskTimeDf %>%
			dplyr::group_by(., task) %>%
			dplyr::summarize(., minutes = sum(minutes), hours = sum(hours), category = head(category, 1)) %>%
			dplyr::arrange(., desc(minutes))
			
		return(list(taskTimeDf = taskTimeDf, subTaskTimeDf = subTaskTimeDf))
	})
	
	
	getCatTimeDf = reactive({
		
		subTaskTimeDf = getTaskTimeDf()$subTaskTimeDf

		subCatTimeDf =
			subTaskTimeDf %>%
			dplyr::group_by(., category, time_label) %>%
			dplyr::summarize(., minutes = sum(minutes), .groups = 'drop') %>% 
			dplyr::mutate(., hours = round(minutes/60, 2)) %>%
			dplyr::left_join(., catDf, by = 'category')
		
		
		catTimeDf =
			subCatTimeDf %>%
			dplyr::group_by(., category, name, prod_index, parent_category, color, rgbaColor) %>%
			dplyr::summarize(., minutes = sum(minutes), hours = sum(hours), .groups = 'drop')

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
	
	output$tsPlot = 
		renderHighchart({
		
			# https://stackoverflow.com/questions/46671973/highcharter-stacked-column-groupings-not-using-hchart
			subCatTimeDf = getCatTimeDf()$subCatTimeDf
			
			seriesList =
				purrr::cross_df(list(category = catDf$category, time_label = state$subFreqDf$time_label)) %>%
				dplyr::left_join(
					.,
					subCatTimeDf[, c('category', 'time_label', 'minutes', 'hours')],
					by = c('time_label', 'category')
				) %>%
				dplyr::left_join(., catDf, by = 'category') %>%
				dplyr::mutate(
					.,
					minutes = ifelse(is.na(minutes), 0, minutes),
					hours = ifelse(is.na(hours), 0, hours)
				) %>%
				dplyr::group_by(., category, name, color) %>%
				dplyr::group_split(.) %>%
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
				hc_add_series_list(seriesList) %>%
				hc_xAxis(., categories = state$subFreqDf$time_label) %>%
				hc_plotOptions(column = list(stacking = "normal")) %>%
				hc_legend(enabled = FALSE) %>%
				hc_add_theme(hc_theme_538())

		})
	
	output$catPlot =
		renderHighchart({
			
			catTimeDf = getCatTimeDf()$catTimeDf
			
			chartDf =
				catTimeDf %>%
				dplyr::mutate(., time = {if (max(.$minutes) >= 120) hours else minutes})
			
			textStr = {if (max(chartDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
			
			highchart() %>%
				hc_add_series(., type = 'bar', data = chartDf, mapping = hcaes(x = name, y = time, color = color)) %>%
				hc_xAxis(categories = chartDf$name) %>%
				hc_yAxis(title = list(text = textStr)) %>%
				hc_size(height = 300) %>%
				hc_legend(enabled = FALSE) %>%
				hc_title(text = 'Categories') %>%
				hc_add_theme(hc_theme_ft())
		})
	
	output$catTable =
		renderDT({
			
			catTimeDf = getCatTimeDf()$catTimeDf
			
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
					order = list(list(1, 'desc'))
					),
				rownames = FALSE
				)
			})
	

	output$taskPlot =
		renderHighchart2({
			
			taskTimeDf = getTaskTimeDf()$taskTimeDf
			
			chartDf =
				taskTimeDf %>%
				head(., 20) %>%
				dplyr::left_join(., catDf, by = 'category') %>%
				dplyr::mutate(., time = {if (max(.$minutes) >= 120) hours else minutes})

			textStr = {if (max(chartDf$minutes) >= 120) 'Duration (Hours)' else 'Duration (Minutes)'}
			
			highchart() %>%
				hc_add_series(., type = 'column', data = chartDf, mapping = hcaes(x = task, y = time, color = color)) %>%
				hc_xAxis(categories = chartDf$task) %>%
				hc_yAxis(title = list(text = textStr)) %>%
				hc_legend(enabled = FALSE) %>%
				hc_title(text = 'Tasks') %>%
				hc_add_theme(hc_theme_ft())
			})

	output$taskTable =
		DT::renderDT({
			
			taskTimeDf = getTaskTimeDf()$taskTimeDf
			
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
										paste0('rgba(', ., ',.2)')
									) %>%
								.$color
						)
					)
			})
	

	
	output$taskParams = 
		DT::renderDT({
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
	
	output$catParams = 
		DT::renderDT({
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