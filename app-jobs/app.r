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
	
	getRawDf = reactive({
		readxl::read_xlsx(XLSX_DIR) %>%
			dplyr::rename_with(., function(x) str_replace(x, ' ', '')) %>%
			dplyr::mutate(., across(contains('Date'), function(x) as.Date(x, origin = '1899-12-30'))) %>%
			.[1:(nrow(.) - 1), ] %>% 
			return(.)
	})

	output$sankeyPlot =
		renderHighchart({
			
			dataDf =
				getRawDf() %>%
				dplyr::mutate(
					.,
					Source = paste0('sc', Source),
					Portal = paste0('pr', Portal),
					InitialReply = ifelse(is.na(InitialReply), 'N', InitialReply),
					InitialReply = paste0('ir', InitialReply),
					Interview =
						ifelse(is.na(InitialReply) & is.na(Interview), 'N',
							   ifelse(is.na(Interview) & InitialReply != 'irA', str_sub(InitialReply, 3),
							   	   ifelse(is.na(Interview) & InitialReply == 'irA', 'N',
							   	   	   Interview
							   	   	   )
							   	   )
							   ),
					Interview = paste0('i1', Interview),
					Interview2 =
						ifelse(is.na(Interview) & is.na(Interview2), 'N',
							   ifelse(is.na(Interview2) & Interview != 'i1A', str_sub(Interview, 3),
							   	   ifelse(is.na(Interview2) & Interview == 'i1A', 'N',
							   	   	   Interview2
							   	   )
							   )
						),
					Interview2 = paste0('i2', Interview2),
					Interview3 = 
						ifelse(is.na(Interview2) & is.na(Interview3), 'N',
							   ifelse(is.na(Interview3) & Interview2 != 'i2A', str_sub(Interview2, 3),
							   	   ifelse(is.na(Interview3) & Interview2 == 'i2A', 'N',
							   	   	   Interview3
							   	   )
							   )
						),
					Interview3 = paste0('i3', Interview3),
					Interview4 = 
						ifelse(is.na(Interview3) & is.na(Interview4), 'N',
							   ifelse(is.na(Interview4) & Interview3 != 'i3A', str_sub(Interview3, 3),
							   	   ifelse(is.na(Interview4) & Interview3 == 'i3A', 'N',
							   	   	   Interview4
							   	   )
							   )
						),
					Interview4 = paste0('i4', Interview4),
					Interview5 = 
						ifelse(is.na(Interview4) & is.na(Interview5), 'N',
							   ifelse(is.na(Interview5) & Interview4 != 'i4A', str_sub(Interview4, 3),
							   	   ifelse(is.na(Interview5) & Interview4 == 'i4A', 'N',
							   	   	   Interview5
							   	   )
							   )
						),
					Interview5 = paste0('i5', Interview5)
					
					
				)
			
			# Max 4 columns
			nodeOptions = 
				list(
					list(
						id = 'scGlassdoor',
						column = 0,
						name = 'Found Posting Through Glassdoor',
						color = 'Teal'
					),
					list(
						id = 'scOther',
						column = 0,
						name = 'Found Posting Through Other',
						color = 'oceanblue'
					),
					
					list(
						id = 'scGoogle',
						column = 0,
						name = 'Found Posting Through Google',
						color = 'Blue'
					),
					
					list(
						id = 'scIndeed',
						column = 0,
						name = 'Found Posting Through Indeed',
						color = 'Skyblue'
					),
					list(
						id = 'scCompany Site',
						column = 0,
						name = 'Found Posting Through Company Site',
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
						id = 'prOther',
						column = 1,
						name = 'Applied Through Other Platform',
						color = 'Pink'
					),
					list(
						id = 'irA',
						column = 2,
						name = 'Passed Initial Screen',
						color = 'lightgreen'
					),
					list(
						id = 'irD',
						column = 2,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'irN',
						column = 2,
						name = 'No Response',
						color = 'black'
					),
					list(
						id = 'irW',
						column = 2,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i1N',
						column = 3,
						name = 'No Response',
						color = 'black'
					),
					list(
						id = 'i1A',
						column = 3,
						name = 'First Round',
						color = 'MediumSpringGreen'
					),
					list(
						id = 'i1D',
						column = 3,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'i1W',
						column = 3,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i1G',
						column = 3,
						name = 'Ghost',
						color = 'Maroon'
					),
					
					list(
						id = 'i2N',
						column = 4,
						name = 'Awaiting/No Response',
						color = 'black'
					),
					list(
						id = 'i2A',
						column = 4,
						name = 'Second Round',
						color = 'forestgreen'
					),
					list(
						id = 'i2D',
						column = 4,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'i2W',
						column = 4,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i2G',
						column = 4,
						name = 'Ghost',
						color = 'Maroon'
					),
					list(
						id = 'i3N',
						column = 5,
						name = 'Awaiting/No Response',
						color = 'black'
					),
					list(
						id = 'i3A',
						column = 5,
						name = 'Third Round',
						color = 'forestgreen'
					),
					list(
						id = 'i3D',
						column = 5,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'i3W',
						column = 5,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i3G',
						column = 5,
						name = 'Ghost',
						color = 'Maroon'
					),
					list(
						id = 'i3O',
						column = 5,
						name = 'Job Offer',
						color = 'lawngreen'
					),
					list(
						id = 'i4N',
						column = 6,
						name = 'Awaiting/No Response',
						color = 'black'
					),
					list(
						id = 'i4A',
						column = 6,
						name = 'Fourth Round',
						color = 'olivedrab'
					),
					list(
						id = 'i4D',
						column = 6,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'i4W',
						column = 6,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i4G',
						column = 6,
						name = 'Ghost',
						color = 'Maroon'
					),
					list(
						id = 'i4O',
						column = 6,
						name = 'Job Offer',
						color = 'lawngreen'
					),
					list(
						id = 'i5N',
						column = 7,
						name = 'Awaiting/No Response',
						color = 'black'
					),
					list(
						id = 'i5A',
						column = 7,
						name = 'Third Round',
						color = 'green'
					),
					list(
						id = 'i5D',
						column = 7,
						name = 'Rejection',
						color = 'red'
					),
					list(
						id = 'i5W',
						column = 7,
						name = 'Withdrew Application',
						color = 'purple'
					),
					list(
						id = 'i5G',
						column = 7,
						name = 'Ghost',
						color = 'Maroon'
					),
					list(
						id = 'i5O',
						column = 7,
						name = 'Job Offer',
						color = 'lawngreen'
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
			
			chartDfs[[4]] =
				dataDf %>%
				# dplyr::filter(., InitialReply == 'irA') %>%
				dplyr::mutate(., Interview2 = ifelse(Interview != 'i1A', paste0('i2', str_sub(Interview, 3)), Interview2)) %>%
				dplyr::select(., Interview, Interview2) %>%
				dplyr::group_by(., Interview, Interview2) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Interview, to = Interview2, weight = n)
			
			chartDfs[[5]] =
				dataDf %>%
				dplyr::mutate(., Interview3 = ifelse(Interview2 != 'i2A', paste0('i3', str_sub(Interview2, 3)), Interview3)) %>%
				dplyr::select(., Interview2, Interview3) %>%
				dplyr::group_by(., Interview2, Interview3) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Interview2, to = Interview3, weight = n)
			
			chartDfs[[6]] =
				dataDf %>%
				dplyr::mutate(., Interview4 = ifelse(Interview3 != 'i3A', paste0('i4', str_sub(Interview3, 3)), Interview4)) %>%
				dplyr::select(., Interview3, Interview4) %>%
				dplyr::group_by(., Interview3, Interview4) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Interview3, to = Interview4, weight = n)
			
			chartDfs[[7]] =
				dataDf %>%
				dplyr::mutate(., Interview5 = ifelse(Interview4 != 'i4A', paste0('i5', str_sub(Interview4, 3)), Interview5)) %>%
				dplyr::select(., Interview4, Interview5) %>%
				dplyr::group_by(., Interview4, Interview5) %>%
				dplyr::summarize(., n = n(), .groups = 'drop') %>%
				dplyr::rename(., from = Interview4, to = Interview5, weight = n)
			
			
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
				# hc_chart(inverted = TRUE) %>%
				# hc_xAxis(categories = chartDf$name) %>%
				# hc_yAxis(title = list(text = textStr)) %>%
				# hc_size(height = 300) %>%
				# hc_legend(enabled = FALSE) %>%
				hc_title(text = 'Charles Job Applications 2/22 - Current') %>%
				hc_add_theme(hc_theme_538()) %>%
				hc_exporting(
					enabled = TRUE, # always enabled
					filename = "custom-file-name"
				)
			
		})

	

	
}



shinyApp(ui, server)