SQLITE_DIR = 'C:/Users/Charles/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db'

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
			selectInput(
				'freq',
				h3("Select box"), 
				choices = list('Hour' = 'h', 'Day' = 'd', "Week" = 'w', 'Month' = 'm'), selected = 'd')
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


server = function(input, output) {

	catDf =
		tribble(
			~ category, ~ name, ~ prod_index, ~ parent_category, ~ color,
			'econ', 'Economics & Data Science', 1, NA, '#7CB5EC',
			'dev', 'Development & Programming', 1, NA, '#8085E9',
			'prod', 'Personal Productivity', 1, NA, '#F15C80',
			'util', 'Misc Utilities', 0, NA, '#90ED7D',
			'unknown', 'Uncategorized', 0, NA, '#000000',
			'ent', 'Entertainment', -1, NA, '#F7A35C'
		) %>%
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
		tribble(
			~ task, ~ category,
			'rstudio.exe', 'econ',
			'explorer.exe', 'util',
			'stackoverflow.com', 'dev',
			'rstudio.github.io', 'dev',
			'cran.r-project.org', 'econ',
			'notepad++.exe', 'dev',
			'econforecasting.com', 'econ',
			'google.com', 'util',
			'shiny.rstudio.com', 'econ',
			'github.com', 'dev',
			'rdocumentation.org', 'econ',
			'jsfiddle.net', 'dev',
			'db.rstudio.com', 'econ',
			'datacamp.com', 'econ',
			'api.highcharts.com', 'econ',
			'jkunst.com', 'dev',
			'lubridate.tidyverse.org', 'econ',
			'localhost:1993', 'prod',
			'rdrr.io', 'econ',
			'datascienceplus.com', 'econ',
			'notepad.exe', 'dev',
			'cmd.exe', 'dev'
		)
	
	
	getRawDf = reactive({
		conn = dbConnect(RSQLite::SQLite(), SQLITE_DIR)
		rawDf =
			dbGetQuery(conn, 'SELECT * FROM eventmodel WHERE duration >= 0') %>%
			as_tibble(.)
		return(rawDf)
	})
	
	
	getFilteredDf = reactive({
		
		rawDf = getRawDf()
		
		dateMin = {
			if (input$freq == 'h') lubridate::as_datetime(Sys.time() - lubridate::hours(1), tz = Sys.timezone())
			else if (input$freq == 'd') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'day'), tz = Sys.timezone())
			else if (input$freq == 'w') lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'week'), tz = Sys.timezone())
			else lubridate::as_datetime(lubridate::floor_date(Sys.time(), 'month'), tz = Sys.timezone())
		}
		
		filteredDf =
			rawDf %>%
			# Adjust to system timezone
			dplyr::mutate(., timestamp = lubridate::with_tz(lubridate::ymd_hms(timestamp, tz = 'UTC'), Sys.timezone())) %>%
			dplyr::filter(., timestamp >= dateMin)
		
		return(filteredDf)
	})
	
	
	getTaskTimeDf = reactive({
		
		filteredDf = getFilteredDf()
		
		taskTimeDf =
			filteredDf %>%
			dplyr::filter(., bucket_id != 2 & duration != 0) %>%
			purrr::transpose(.) %>%
			purrr::map_dfr(., function(x) {
				if (is.null(x$datastr)) return();
				res = jsonlite::fromJSON(x$datastr)
				tibble(
					timestamp = x$timestamp,
					duration = x$duration,
					app = {if ('app' %in% names(res)) res$app else NA},
					title = {if ('title' %in% names(res)) res$title else NA},
					url = {
						if ('url' %in% names(res)) getDomains(res$url)
						else NA
					}
				)
			}) %>%
			dplyr::filter(., is.na(app) | app != 'firefox.exe') %>%
			dplyr::arrange(desc(timestamp)) %>%
			dplyr::mutate(., task = ifelse(is.na(url), app, url)) %>%
			dplyr::group_by(., task) %>%
			dplyr::summarize(., minutes = round(sum(duration)/60)) %>%
			dplyr::mutate(., hours = round(minutes/60, 2)) %>%
			dplyr::left_join(taskDf, by = 'task') %>%
			dplyr::mutate(., category = ifelse(is.na(category), 'unknown', category)) %>%
			dplyr::arrange(., desc(minutes))
		
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
				
			# catTimeDf %>%
			# 	purrr::transpose(.) %>%
			# 	purrr::reduce(., function(accum, x)
			# 		hc_add_series(
			# 			accum,
			# 			type = 'bar',
			# 			data = tibble(name = x$name, minutes = x$minutes),
			# 			mapping = hcaes(x = name, y = minutes),
			# 			color = x$color,
			# 			name = x$name
			# 			),
			# 		.init = highchart()
			# 		) %>%
			# 	hc_xAxis(categories = catTimeDf$name) %>%
			# 	hc_yAxis(title = list(text = 'Duration (Minutes)')) %>%
			# 	hc_size(height = 300)
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
					pageLength = 5,
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