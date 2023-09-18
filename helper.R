library(shiny)
library(magrittr)
library(tidyr)
library(dplyr)

na_formatter <- function(df) {
  
  # Loop through to change NA to what we preferred.
  # char -> "NA" others -> NA
  for (i in 1:ncol(df) ) {
    if (class(df[,i]) == "character") {
      df[,i] <- df[,i] %>% replace_na("NA")
    }
  }
  return(df)
}


input_constructer <- function (n,name,types,df) {
  widgets <- list()
  for (i in 1:n) {
    widget <- input_widget_constructor(i,name[i],types[i],df[i])
    widgets <- append(widgets, list(widget))
  }
  return(widgets)
}

input_widget_constructor <- function (n,name,type,df) {
  # print(paste(name, type))
  if (type == "character") {
    return(
      checkboxGroupInput(
        name,
        name,
        choices = unique(df[,1]),
        selected = unique(df[,1])
      )
    )
  }
  else if (type == "Date") {
    return(dateRangeInput(
      name,
      name,
      start = "1900-01-01"
    ))
  }
  else {
    max_min <- c(max(df,na.rm = TRUE), min(df,na.rm = TRUE))
    print(max_min)
    return(
      sliderInput(
        name,
        h3(name),
        min = max_min[2],
        max = max_min[1],
        value = c(max_min[2],max_min[1])
      )
    )
  }
}

filter_maker <- function(col_names, type, data, input) {
  output_filter_list <- list(data)
  
  for (i in 1:length(col_names)) {
    if (type[i] == "character") {
      output_filter_list <- append(
        output_filter_list,
        list(data[[col_names[i]]] %in% input[[col_names[i]]])
      )
    }
    else if (type[i] == "Date") {
      output_filter_list <- append(
        output_filter_list,
        list(data[[col_names[i]]] >= input[[col_names[i]]][1] & data[[col_names[i]]] <= input[[col_names[i]]][2] | is.na(data[[col_names[i]]]))
      )
    }
    else {
      output_filter_list <- append(
        output_filter_list,
        list(data[[col_names[i]]] >= input[[col_names[i]]][1] & data[[col_names[i]]] <= input[[col_names[i]]][2] | is.na(data[[col_names[i]]]))
      )
    }
  }
  # print(output_filter_list)
  return(output_filter_list)
}