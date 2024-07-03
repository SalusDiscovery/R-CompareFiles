library(shiny)
library(diffobj)
library(purrr)
library(fansi)
library(crayon)
library(tools)
library(pdftools)
library(stringr)
library(readtext)

#### Great Resource ####
# https://ladal.edu.au/pdf2txt.html#OCR_with_pdftools




wdiff_internal <- function(original, edited)
{
	a <- unlist(strsplit(original, sep))
	b <- unlist(strsplit(edited, sep))
	
	dat <- diffobj::ses_dat(a, b)
	
	diff <- dat[['val']]
	del <- dat[['op']] == 'Delete'
	ins <- dat[['op']] == 'Insert'
	if(any(del))
	{
		diff[del] <- paste("[-", diff[del], "-]", sep='')
	}
	if(any(ins))
	{
		diff[ins] <- paste("{+", diff[ins], "+}", sep='')
	}
	if(any(!ins & !del))
	{
		diff[!ins & !del] <- paste("", diff[!ins & !del], sep='')
	}
	return(paste(diff, collapse = " "))
}

wdiff <- function(original, edited, sep = " ")
{
	purrr::map2_chr(original, edited, ~wdiff_internal(.x, .y, sep = sep))
}

read_pdf_image <- function(file)
{
	x <- tesseract::ocr(file, engine = tesseract("eng"))
	return(x)
}

read_pdf_text <- function(file)
{
	x <- pdftools::pdf_text(file)
	return(x)
}

read_text <- function(file)
{
	x <- readtext::readtext(file)
	return(x)
}

get_text <- function(file, asImage=F)
{
	text <- NULL
	if(file_ext(file) %in% c('pdf'))
	{
		if(asImage)
		{
			text <- read_pdf_image(file)
		}
		else
		{
			text <- read_pdf_text(file)
		}
	}
	else
	{
		text <- read_text(file)
	}
	text <- collapseText(text)
	return(text)
}

collapseText <- function(x)
{
	x <- paste(x, collapse = " ")
	x <- gsub("[\r\n]", " ", x)
	# x <- stringr::str_squish(x)
	x <- gsub("[\\|]", "", x)
	return(x)
}

getCompare <- function(original_txt, edited_txt)
{
	ret <- wdiff(original_txt, edited_txt)
	ret <- gsub("[--]", "", ret, fixed=T) # Remove extraneous deletions
	ret <- gsub("{++}", "", ret, fixed=T) # Remove extraneous additions
	ret <- gsub("(\\-\\])(\\s+)(\\[\\-)", "\\2", ret) # Merge consecutive deletions
	ret <- gsub("(\\+\\})(\\s+)(\\{\\+)", "\\2", ret) # Merge consecutive additions
	ret <- gsub("(\\[\\-)(.*?)(\\-\\])", red('[\\2]'), ret) # Deletions are red
	ret <- gsub("(\\{\\+)(.*?)(\\+\\})", green('{\\2}'), ret) # Additions are green
	return(ret)
}

ansi2html <- function(ansi)
{
	HTML(sprintf(
		"<pre>%s</pre>",
		gsub("\n", "<br/>", as.character(sgr_to_html(paste(strwrap(ansi, width = 100), collapse = "\n"))))
	))
}

shinyApp(
	ui = fluidPage( 
		# Application title
		titlePanel("Compare Files Word-by-Word"),
		
		# Sidebar with a slider input for number of bins 
		sidebarLayout(
			sidebarPanel(
				fileInput(
					inputId = "file1", 
					label = "Original File"
				),
				checkboxInput("file1.image", "Treat as pdf image.", value = FALSE),
				fileInput(
					inputId = "file2", 
					label = "New File"
				),
				checkboxInput("file2.image", "Treat as pdf image.", value = FALSE),
			),
			mainPanel(
				htmlOutput("compare") #, fill=F, style = "word-wrap: break-word;")
			)
		)
	),
	
	server = function(input, output) {
		
		file1.text <- reactive({
			req(input$file1)
			print(input$file1)# Ensure input$file is available
			ret <- get_text(input$file1$datapath, input$file1.image)
			return(ret)
		})
		
		file2.text <- reactive({
			req(input$file2)
			print(input$file2)# Ensure input$file is available
			ret <- get_text(input$file2$datapath, input$file2.image)
			return(ret)
		})
		
		output$compare <- renderUI({
			if(!isTruthy(input$file1) || !isTruthy(input$file2))
			{
				return("Need two files uploaded to compare.")
			}
			else
			{
				if(!isTruthy(file1.text()) || !isTruthy(file2.text()))
				{
					return("Unable to compare. If using pdf, try checking 'Treat pdf as image'.")
				}
			}
			req(file1.text(), file2.text())
			result <- getCompare(file1.text(), file2.text())
			ret <- ansi2html(result)
			return(ret)
		})
	}
) 