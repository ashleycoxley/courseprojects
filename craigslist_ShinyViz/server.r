library(ggplot2)
library(tm)
library(reshape)
library(ggmap)
library(maptools)
library(sp)
library(rgdal)
library(ggmap)
gpclibPermit()


getData <- function() {
  file <- 'craigs_parsed_w_item.csv'
  craigs <- read.csv(file, header=F)
  colnames(craigs) <- c('region', 'neighborhood', 'title', 'text', 'item')  
  
  craigs_minus <- subset(craigs, region != 'scz')
  craigs_minus$region <- factor(craigs_minus$region)
  
  # stem titles
  craigs_minus[,5] <- as.character(craigs_minus[,5])
  stemmed_items <- lapply(craigs_minus[,5], stemDocument)
  craigs_minus$item <- unlist(stemmed_items)
  
  # get frequencies
  region_freq <- as.data.frame(table(craigs_minus$item, craigs_minus$region))
  colnames(region_freq) <- c('word', 'region', 'regionfreq')
  freqs <- cast(region_freq, word~region, value='regionfreq')
  freqs$overall <- rowSums(freqs)
  
  # order words by overall frequency
  ordered <- freqs[order(freqs$overall, decreasing=TRUE), ]
  ordered$word <- factor(ordered$word, levels = ordered$word)
  
  return(ordered)
}

getFreq <- function() {
  file <- 'posting_freqs.csv'
  post_freq <- read.csv(file, header=T)
  
  return(post_freq)
}

getMap <- function() {
  counties.sp <- readOGR(dsn='bayarea_county', layer='bayarea_county')
  joined_id <- c(1,1,8,8,4,5,6,1,8) 
  joined <- unionSpatialPolygons(counties.sp, joined_id)
  joined.points = fortify(joined, region="id")
  
  ordered <- getData()
  totals <- colSums(ordered[,2:6])
  
  chair <- ordered[which(ordered$word=='chair'), 2:6] / totals
  desk <- ordered[which(ordered$word=='desk'), 2:6] / totals
  box <- ordered[which(ordered$word=='box'), 2:6] / totals
  tv <- ordered[which(ordered$word=='tv'), 2:6] / totals
  dirt <- ordered[which(ordered$word=='dirt'), 2:6] / totals
  sofa <- ordered[which(ordered$word=='sofa'), 2:6] / totals
  firewood <- ordered[which(ordered$word=='firewood'), 2:6] / totals
  
  ch_freqs <- t(data.frame(rbind(chair, desk, box, tv, dirt, sofa, firewood)))
  colnames(ch_freqs) <- c('chair', 'desk', 'box', 'tv', 'dirt', 'sofa', 'firewood')
  ch_freqs <- as.data.frame(ch_freqs)
  ch_freqs$id <- c(1, 8, 5, 6, 4)
  
  regionfreq <- merge(joined.points, ch_freqs, by='id')
  return(regionfreq)
}

plotFreq <- function(localFrame, showRegion) {
  if (length(showRegion) == 0){
    localFrame$showRegion <- localFrame$overall
  }
  
  else if (length(showRegion) == 1){
    localFrame$showRegion <- localFrame[,showRegion]
  }
  
  else {
    localFrame$showRegion <- rowSums(localFrame[,c(showRegion)])
  }
  
  barPlot <- ggplot(subset(localFrame, overall > 10 & word != "NA"), 
                    aes(x=word, y=showRegion)) +
    geom_bar(stat='identity', fill='#6a51a3') +
    scale_y_continuous(limits = c(0, 74)) +
    theme(axis.text.x = element_text(angle=35, hjust=1, vjust=1, size=12)) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())
  
  return(barPlot)
}

plotBubble <- function(freqTable) {
  
  bubble <- ggplot(freqTable, aes(
    x = lens,
    y = normed,
    color = region,
    size = pops)) +
    geom_point() +
    scale_x_continuous(limits = c(130, 190)) +
    scale_y_continuous(limits = c(1.5, 4)) +
    geom_text(aes(label=labels,
                  hjust = 1,
                  vjust = .8),
              color = "grey40",
              size = 4) +
    guides(size=FALSE) +
    theme(legend.position="none") +
    scale_size_area(max_size=14) +
    scale_color_brewer(type='qual', palette=7) +
    xlab('Posts per 10K Population') +
    ylab('Median Posting Length (Characters)') +
    theme(axis.ticks.x = element_blank()) +
    theme(axis.ticks.y = element_blank())
    
  return(bubble)
}


plotChoropleth <- function(mapData, showWord) {
  
  mapData$showWord <- mapData[,showWord]
  
  choropleth <- ggplot(mapData) +
    aes(long,lat,group=group) + 
    geom_polygon(aes(fill=showWord)) +
    geom_path(color="white") +
    #coord_equal() +
    scale_fill_continuous(low='#dadaeb', high='#756bb1', name = "Item Density") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  return(choropleth)
}

plotRatios <- function(localFrame, word1, word2) {
  melted <- melt(localFrame)
  
  ratios <- ggplot(subset(melted, word %in% c(word1, word2)), 
                   aes(x=region, y=value, fill=word)) +
    geom_bar(position='fill', stat='identity') +
    scale_fill_manual(values=c('#6a51a3', '#bcbddc'), name = "") +
    scale_x_discrete(labels = c('eby' = 'East Bay', 'nby' = 'North Bay', 'pen' = 'Peninsula', 'sby' = 'South Bay', 'sfc' = 'San Francisco')) +
    coord_flip() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_text(size=14))
  
  return(ratios)
}

globalData <- getData()
freq <- getFreq()
mapData <- getMap()

shinyServer(function(input, output) {
  
  cat("Press \"ESC\" to exit...\n")
  
  localFrame <- globalData
  freqData <- freq
  mapFreq <- mapData
  
  output$barPlot <- renderPlot({
  barPlot <- plotFreq(
    localFrame,
    input$showRegion
  )
  print(barPlot)
  })
  
  output$choropleth <- renderPlot({
    choro <- plotChoropleth(
      mapData,
      input$showWord
      )
    print(choro)
  }, height=400, width=525)
  
  output$bubblePlot <- renderPlot({
    bubblePlot <- plotBubble(
      freqData
      )
    print(bubblePlot)
  })
  
  output$ratioPlot <- renderPlot({
    ratioPlot <- plotRatios(
      localFrame,
      input$word1,
      input$word2
      )
    print(ratioPlot)
  })
})