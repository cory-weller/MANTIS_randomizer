#!/usr/bin/env Rscript

# Load Packages
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
    library(BiocManager)
if (!require("wpm", quietly = TRUE))
    BiocManager::install("wpm")
    library(wpm)

required <- c('shiny','ggplot2','DT','data.table','foreach','readxl')

for(package in required) {
  if (!require(package, quietly = TRUE, character.only=TRUE)) {
      install.packages(package, character.only=TRUE)
  }
}

library(shiny)
library(ggplot2)
library(DT)
library(data.table)
library(wpm)
library(foreach)
library(readxl)


fluidPage(
    titlePanel("MANTIS Plate Randomizer"),
    fileInput("upload", NULL, accept = c(".csv", ".tsv", ".xlsx", '.txt')),
    selectInput("nwells", "Select # of Wells", choices = c("96", "384")),
    fluidRow(column(3,dataTableOutput('uploadpreview'))),
    fluidRow(column(3,dataTableOutput('downloadpreview')))
)
