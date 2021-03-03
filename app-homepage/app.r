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
		tags$script(src = 'https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta2/dist/js/bootstrap.bundle.min.js'),
		tags$link(rel='stylesheet', href = 'https://econforecasting.com/static/style-bs.css')
		
	),
	
	
	div(
		class = 'container-fluid',
		
		div(
			class = 'container',
			h3('AW GUI')
			),
		div(
			class = 'container',
			div(
				div(
					class = 'row',
					div(
						class = 'btn-group',
						role = 'group',
						actionButton('prev', 'Back', class = 'btn-primary'),
						actionButton('currentdate', 'Current Date', class = 'btn-primary disabled'),
						actionButton('fwd', 'Next', class = 'btn-primary')
					),
					selectInput(
						'freq',
						h4('Select Me'),
						choices = list('Hour' = 'h', 'Day' = 'd', "Week" = 'w', 'Month' = 'm'), selected = 'd'
						)
					)
				)
			),
		
		div(
			class = 'container',
			div(
				class = 'row',
				div(class = 'col-md-12 col-lg-6', highchartOutput('catPlot', height = 300)),
				div(class = 'col-md-12 col-lg-6', dataTableOutput('catTable')),
				)
			),
		
		div(
			class = 'container',
			div(
				class = 'row justify-content-center',
				highchartOutput(outputId = "taskPlot"),
				div(class = 'col-auto', dataTableOutput('taskTable'))
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
		{c(., .$x)}
	
	
	
	
	getRawDf = reactive({
		conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)
		rawDf =
			dbGetQuery(conn, 'SELECT * FROM eventmodel WHERE duration >= 1') %>%
			as_tibble(.)
		return(rawDf)
	})
	
	
	state = reactiveValues(
		freq = NULL,
		dateMax = NULL
		)
	
	
	
	
	observeEvent(input$freq, {
		state$freq = input$freq
		state$dateMax = Sys.time()
		state$dateMin = {
			if (state$freq == 'h') lubridate::as_datetime(Sys.time() - lubridate::hours(1), tz = Sys.timezone())
			else if (state$freq == 'd') lubridate::floor_date(Sys.time() - lubridate::hours(4), 'day') + lubridate::hours(4)
			else if (state$freq == 'w') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'week', week_start = 1), tz = Sys.timezone()) + lubridate::hours(4)
			else if (state$freq == 'm') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'month'), tz = Sys.timezone())
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
	
	getFilteredDf = reactive({
		
		rawDf = getRawDf()
		
		# dateMax = if (dateMax == )
		

		
		filteredDf =
			rawDf %>%
			# Adjust to system timezone
			dplyr::mutate(., timestamp = lubridate::with_tz(lubridate::ymd_hms(timestamp, tz = 'UTC'), Sys.timezone())) %>%
			dplyr::filter(., timestamp >= state$dateMin & timestamp <= state$dateMax)
		
		return(filteredDf)
	})
	
	
	getTaskTimeDf = reactive({
		
		filteredDf = getFilteredDf()
		
		taskTimeDf =
			filteredDf %>%
			dplyr::filter(., bucket_id != 2 & duration != 0) %>%
			rowwise(.) %>%
			dplyr::mutate(., datastr = map(datastr, function(x) as_tibble(jsonlite::fromJSON(x)))) %>%
			tidyr::unnest(., datastr) %>%
			dplyr::transmute(
				.,
				timestamp,
				duration,
				app,
				title,
				url = getDomains(url)
			) %>%
			# Replace entries that are x.domain.com with domain.com if in topLevelDomains
			dplyr::mutate(
				.,
				url2 = ifelse(str_count(url, coll('.')) == 2, str_sub(str_replace(url, '[^.]+', ''), 2), url),
				url = ifelse(url2 %in% topLevelDomains, url2, url)
			) %>%
			dplyr::select(., -url2) %>%
			# Get rid of firefox due to overlapping with url entries
			dplyr::filter(., is.na(app) | app != 'firefox.exe') %>%
			# Now shortern the duration of any entries where the time overlaps with the second entry (this seems to be an AW bug)
			dplyr::arrange(., timestamp) %>%
			dplyr::mutate(., timeEnded = timestamp + lubridate::seconds(duration)) %>%
			dplyr::mutate(., duration = ifelse(timeEnded > dplyr::lead(timestamp, 1) & !is.na(dplyr::lead(timestamp, 1)), dplyr::lead(timestamp, 1) - timestamp, duration)) %>%
			# Order desc
			dplyr::arrange(., desc(timestamp)) %>%
			dplyr::mutate(., task = ifelse(is.na(url), app, url)) %>%
			dplyr::group_by(., task) %>%
			dplyr::summarize(., minutes = round(sum(duration)/60)) %>%
			dplyr::mutate(., hours = round(minutes/60, 2)) %>%
			dplyr::left_join(., taskDf %>% dplyr::mutate(., task = str_replace(task, coll('*.'), '')), by = 'task') %>%
			dplyr::mutate(., category = ifelse(is.na(category), 'unknown', category)) %>%
			dplyr::arrange(., desc(minutes)) %>%
			dplyr::filter(., !task %in% c('newtab', 'LockApp.exe'))
		
		return(taskTimeDf)
	})
	
	
	getCatTimeDf = reactive({
		
		taskTimeDf = getTaskTimeDf()
		
		catTimeDf =
			taskTimeDf %>%
			dplyr::group_by(., category) %>%
			dplyr::summarize(., minutes = sum(minutes)) %>% 
			dplyr::mutate(., hours = round(minutes/60, 2)) %>%
			dplyr::left_join(., catDf, by = 'category')
			
		return(catTimeDf)
	})
	
	
	output$catPlot =
		renderHighchart({
			chartDf =
				getCatTimeDf() %>%
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
			datatable(
				getCatTimeDf() %>%
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
			
			chartDf =
				getTaskTimeDf() %>%
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
			datatable(
				getTaskTimeDf() %>%
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