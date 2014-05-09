library(shiny)

shinyUI(
  pageWithSidebar(    
    headerPanel('Craigslist Free in the Bay Area'),
    sidebarPanel(width=3,
    conditionalPanel(condition = 'input.conditionedPanels == 1',
                       checkboxGroupInput(
                         'showRegion', 
                         "Region", 
                         c('San Francisco' = 'sfc', 'Peninsula' = 'pen', 'East Bay' = 'eby', 'North Bay' = 'nby', 'South Bay' = 'sby')
                         )
                     ),
    
    conditionalPanel(condition = 'input.conditionedPanels == 2',
                       radioButtons(
                         'showWord',
                         'Item',
                         c('chair', 'desk', 'box', 'tv', 'dirt', 'sofa', 'firewood')
                         )
                     ),
    
    conditionalPanel(condition = 'input.conditionedPanels == 3',
                     radioButtons(
                       'word1',
                       'Item 1',
                       c('chair', 'desk', 'box', 'tv', 'dirt', 'sofa', 'firewood')
                       ),
                     radioButtons(
                       'word2',
                       'Item 2',
                       c('chair', 'desk', 'box', 'tv', 'dirt', 'sofa', 'firewood'),
                       selected = 'desk'
                       )
                     ),
                 
    conditionalPanel(condition = 'input.conditionedPanels == 4',
                     sidebarPanel()
                     )
    
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Item Histogram",
                 value = 1,
                 plotOutput('barPlot')
                 ),
        tabPanel('Item Density Map',
                 value = 2,
                 plotOutput('choropleth', width = "100%")
                 ),
        tabPanel('Item Ratios',
                 value = 3,
                 plotOutput('ratioPlot')
                 ),
        tabPanel("Posting Habits",
                 value = 4,
                 plotOutput('bubblePlot')
                 ),
        id = "conditionedPanels"
      )
    )
  )
)

