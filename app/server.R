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


randomize <- function(indt, n_wells) {
    samples <- copy(indt)
    setDT(samples)
    samples[, Group := rleid(Reagent, Volume)]

    samples.expanded <- foreach(grp=samples$Group, .combine='rbind') %do% {
        samples.tmp <- samples[Group==grp]
        samples.tmp <- samples.tmp[base::rep(1,samples.tmp$Replicates)]
        samples.tmp[, 'Replicates' := NULL]
        samples.tmp[, replicate:=1:.N]
        samples.tmp
    }

    samples.expanded[, 'ID' := 1:.N]
    samples.expanded[, 'Sample' := paste0('s', 1:.N)]
    setkey(samples.expanded, Sample)

    volumes <- copy(samples)
    
    df <- as.data.frame(samples.expanded[, c('Sample','Group','ID')])

    n_plates <- ceiling(nrow(df)/n_wells)
    forbiddenwells <- ''
    fixedwells <- ''
    if(n_wells==96) {
        use_dims <- list(8,12)
    } else if(n_wells==384) {
        use_dims <- list(16,24)
    }
    a <- wrapperWPM(
        user_df=df,
        plate_dims=use_dims,
        nb_plates=n_plates,
        forbidden_wells=forbiddenwells,
        fixed_wells=fixedwells,
        spatial_constraint="none")

    setDT(a)

    setkey(a, Sample)

    dt <- merge(a, df)[Group.x == Group.y][ID.x == ID.y]

    dt <- dt[, c('Sample','Well','Row','Column','Plate')]


    setkey(dt, Sample)
    setkey(samples.expanded, Sample)

    dt.final <- merge(dt, samples.expanded)[order(ID)]

    setcolorder(dt.final,  c('Well','Reagent','Volume','Sample','Row','Column','Plate','Group','replicate','ID'))
    dt.final[, 'N_wells' := n_wells]
    return(dt.final)
}


function(input, output){

  # Reactive expression with the data, in this case iris
  #thedata <- reactive(iris)

data_in <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
        csv = fread(input$upload$datapath, sep = ","),
        tsv = fread(input$upload$datapath, sep = "\t"),
        tab = fread(input$upload$datapath, sep = "\t"),
        txt = fread(input$upload$datapath),
        xlsx = as.data.table(read_excel(input$upload$datapath, 1)),
        validate("Invalid file; Please upload a proper tab- or comma-separated file (or .xlsx)")
    )
})

nwells <- reactive({
    req(input$nwells)
    as.numeric(input$nwells)
})

data_out <- reactive({
    dat.out <- randomize(data_in(), nwells())
    return(dat.out)
})


output$uploadpreview <- renderDataTable(data_in(), extensions = 'Buttons',
                options = list(dom = 'Bfrti',
                buttons = c('csv', 'excel'))
)
output$downloadpreview <- renderDataTable(data_out(), extensions = 'Buttons', server = F,
                options = list(dom = 'Bfrti',
                                pageLength=1e6,
                            buttons = c('csv', 'excel')
                            )
)
  
}
