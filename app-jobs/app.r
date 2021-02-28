XLSX_DIR = 'D:/OneDrive/jobs.xlsx'

library(tidyverse)
library(shiny)
library(highcharter)
library(DT)
library(lubridate)


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
			div(
				class = 'row justify-content-center',
				highchartOutput(outputId = "sankeyPlot", height = 600),
				# div(class = 'col-auto', dataTableOutput('taskTable'))
			)
		)
	)
	
)


server = function(input, output) {
	
	getDataDf = reactive({
		readxl::read_xlsx(XLSX_DIR) %>%
			dplyr::rename_with(., function(x) str_replace(x, ' ', '')) %>%
			dplyr::mutate(., across(contains('Date'), function(x) as.Date(x, origin = '1899-12-30'))) %>%
			.[1:(nrow(.) - 1), ] %>% 
			return(.)
	})

	output$sankeyPlot =
		renderHighchart({
			
			dataDf =
				getDataDf() %>%
				dplyr::mutate(
					.,
					Source = paste0('sc', Source),
					Portal = paste0('pr', Portal),
					InitialReply = ifelse(is.na(InitialReply), 'N', InitialReply),
					InitialReply = paste0('ir', InitialReply),
					Interview = ifelse(is.na(Interview), 'N', Interview),
					Interview = paste0('i1', Interview),
				)
			
			# Max 4 columns
			nodeOptions = 
				list(
					list(
						id = 'scGlassdoor',
						column = 0,
						name = 'Found Through Glassdoor/LinkedIn',
						color = 'Teal'
					),
					list(
						id = 'scIndeed',
						column = 0,
						name = 'Found Through Indeed',
						color = 'Skyblue'
					),
					list(
						id = 'scCompany Site',
						column = 0,
						name = 'Found Through Company Site',
						color = 'gold'
					),
					list(
						id = 'scReferral',
						column = 0,
						name = 'Referral/Contacted by Company',
						color = 'Orange'
					),
					list(
						id = 'prCompany Site',
						column = 1,
						name = 'Applied Through Company Portal',
						color = 'goldenrod'
					),
					list(
						id = 'prIndeed',
						column = 1,
						name = 'Applied Through Indeed',
						color = 'lightblue'
					),
					list(
						id = 'prNo Formal Application',
						column = 1,
						name = 'No Formal Application',
						color = 'Brown'
					),
					list(
						id = 'irA',
						column = 2,
						name = 'Passed Initial Screen',
						color = 'green'
					),
					list(
						id = 'irD',
						column = 2,
						name = 'Denied',
						color = 'red'
					),
					list(
						id = 'irN',
						column = 2,
						name = 'TBD',
						color = 'black'
					),
					list(
						id = 'i1N',
						column = 3,
						name = 'TBD',
						color = 'black'
					),
					list(
						id = 'i1A',
						column = 3,
						name = 'Interview',
						color = 'green'
					),
					list(
						id = 'i1D',
						column = 3,
						name = 'Denied',
						color = 'red'
					)
				)
			
			chartDfs = list()
			
			chartDfs[[1]] =
				dataDf %>%
				dplyr::select(., Source, Portal) %>%
				dplyr::group_by(., Source, Portal) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Source, to = Portal, weight = n)
			
			chartDfs[[2]] =
				dataDf %>%
				dplyr::select(., Portal, InitialReply) %>%
				dplyr::group_by(., Portal, InitialReply) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Portal, to = InitialReply, weight = n)
			
			chartDfs[[3]] =
				dataDf %>%
				# dplyr::filter(., InitialReply == 'irA') %>%
				dplyr::mutate(., Interview = ifelse(InitialReply != 'irA', paste0('i1', str_sub(InitialReply, 3)), Interview)) %>%
				dplyr::select(., InitialReply, Interview) %>%
				dplyr::group_by(., InitialReply, Interview) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = InitialReply, to = Interview, weight = n)
			
			chartDf = chartDfs %>% dplyr::bind_rows(.)


			highchart() %>%
				hc_chart(type = 'sankey') %>%
				hc_add_series(
					keys = list('from', 'to', 'weight'),
					data = chartDf,
					type = 'sankey',
					name = 'Sankey Plot',
					nodes = nodeOptions,
					nodePadding = 50
					) %>%
				hc_plotOptions(
					sankey = list(
						dataLabels = list(
							allowOverlap = FALSE,
							enabled = TRUE,
							nodeFormatter = JS('function() {
								return this.point.name + "<br>" + this.point.sum;
							}'),
							x = 10,
							align = 'left'
						)
					)
				) %>%
				hc_size(height = 600) %>%
				# hc_xAxis(categories = chartDf$name) %>%
				# hc_yAxis(title = list(text = textStr)) %>%
				# hc_size(height = 300) %>%
				# hc_legend(enabled = FALSE) %>%
				hc_title(text = 'Charles Job Applications 2/22 - Current') %>%
				hc_add_theme(hc_theme_538())
		})

	

	
}



shinyApp(ui, server)