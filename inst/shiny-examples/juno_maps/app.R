library(shiny)
library(plotly)
library(tidyverse)

# load data
df <- read.csv('www/juno_maps_data.csv')

# set filter data based on input data - !need to consolidate country-code matching first!
#crop_list <- unique(unlist(strsplit(as.character(df$crop), ",")))
#disease_list <- unique(unlist(strsplit(as.character(df$disease), ",")))
#country_list <- unique(unlist(strsplit(as.character(df$country), ",")))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visualise Juno data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          "Select data to map",
          br(),br(),
          selectInput("crop", "Crop:",
                      c("All" = "all",
                        "Cotton" = "cotton",
                        "Crops" = "crops",
                        "Nuts" = "nuts",
                        "Plants" = "plants",
                        "Woody plants" = "woody plants",
                        "Fruits" = "fruits",
                        "Grasses" = "grasses",
                        "Noxious plants" = "noxious plants",
                        "Ornamental plants" = "ornamental plants",
                        "Seedlings" = "seedlings",
                        "Herbs" = "herbs",
                        "Algae" = "algae",
                        "Aquatic plants" = "aquatic plants",
                        "Cereals" = "cereals",
                        "Coffee beans" = "coffee beans",
                        "Cut flowers" = "cut flowers",
                        "Grain" = "grain",
                        "Legumes" = "legumes",
                        "Miscanthus" = "miscanthus",
                        "Oilseeds" = "oilseeds",
                        "Pulp" = "pulp",
                        "Spices" = "spices",
                        "Sugarcane" = "sugarcane",
                        "Switchgrass" = "switchgrass",
                        "Tuber" = "tuber",
                        "Vegetables" = "vegetables",
                        "Stimulants" = "stimulants",
                        "Sugarbeet" = "sugarbeet",
                        "Cut foliage" = "cut foliage",
                        "Opium" = "opium",
                        "Sugarbeet juice" = "sugarbeet juice",
                        "Cocoa products" = "cocoa products",
                        "Pseudocereals" = "pseudocereals",
                        "Cellulose products" = "cellulose products",
                        "Decorative greenery" = "decorative greenery")),
          selectInput("disease", "Disease:",
                      c("All" = "all",
                        "Acute oak decline" = "acute oak decline",
                        "Almond leaf scorch" = "almond leaf scorch",
                        "Alternaria brown spot" = "alternaria brown spot",
                        "Anthracnose" = "anthracnose",
                        "Anthracnosis" = "anthracnosis",
                        "Apple scab" = "apple scab",
                        "Ascochyta blight on peas" = "ascochyta blight on peas",
                        "Ascochyta leaf blight" = "ascochyta leaf blight",
                        "Bacterial brown spot" = "bacterial brown spot",
                        "Bacterial canker" = "bacterial canker",
                        "Bacterial diseases of plants" = "bacterial diseases of plants",
                        "Bacterial leaf streak" = "bacterial leaf streak",
                        "Bacterial ring rot" = "bacterial ring rot",
                        "Bacterial soft rot" = "bacterial soft rot",
                        "Bacterial speck" = "bacterial speck",
                        "Bacterial wetwood" = "bacterial wetwood",
                        "Bacterial wilt" = "bacterial wilt",
                        "Bakanae disease" = "bakanae disease",
                        "Banana bacterial wilt" = "banana bacterial wilt",
                        "Banana xanthomonas wilt" = "banana xanthomonas wilt",
                        "Bayoud" = "bayoud",
                        "Bean anthracnose" = "bean anthracnose",
                        "Beech bark disease" = "beech bark disease",
                        "Bitter pit" = "bitter pit",
                        "Black leaf streak disease" = "black leaf streak disease",
                        "Black point disease" = "black point disease",
                        "Black shank disease" = "black shank disease",
                        "Black sigatoka" = "black sigatoka",
                        "Blackleg of potato" = "blackleg of potato",
                        "Blast disease" = "blast disease",
                        "Blasts" = "blasts",
                        "Blight" = "blight",
                        "Blister rust" = "blister rust",
                        "Blue mould" = "blue mould",
                        "Blue rot" = "blue rot",
                        "Blueberry mosaic disease" = "blueberry mosaic disease",
                        "Boxwood blight" = "boxwood blight",
                        "Brown rot disease" = "brown rot disease",
                        "Buckeye rot" = "buckeye rot",
                        "Bull's eye rot" = "bull's eye rot",
                        "Butt rot" = "butt rot",
                        "Butternut canker" = "butternut canker",
                        "Cankers" = "cankers",
                        "Cecidia" = "cecidia",
                        "Cercosporiosis" = "cercosporiosis",
                        "Charcoal rot" = "charcoal rot",
                        "Chestnut blight" = "chestnut blight",
                        "Chickpea ascochyta blight" = "chickpea ascochyta blight",
                        "Chickpea blight" = "chickpea blight",
                        "Chlorosis" = "chlorosis",
                        "Chocolate spot disease" = "chocolate spot disease",
                        "Citrus black spot" = "citrus black spot",
                        "Citrus variegated chlorosis" = "citrus variegated chlorosis",
                        "Cladosporiosis" = "cladosporiosis",
                        "Clubroot" = "clubroot",
                        "Cobweb disease" = "cobweb disease",
                        "Coffee berry disease" = "coffee berry disease",
                        "Corky bark" = "corky bark",
                        "Corn rust" = "corn rust",
                        "Crown gall" = "crown gall",
                        "Crown rot" = "crown rot",
                        "Damping off" = "damping off",
                        "Dieback" = "dieback",
                        "Downy mildew" = "downy mildew",
                        "Dutch elm disease" = "dutch elm disease",
                        "Ear rot" = "ear rot",
                        "Early blight" = "early blight",
                        "Ergot" = "ergot",
                        "Etiolation" = "etiolation",
                        "False smut" = "false smut",
                        "Fig endosepsis" = "fig endosepsis",
                        "Fire blight" = "fire blight",
                        "Foliar diseases" = "foliar diseases",
                        "Foot rot" = "foot rot",
                        "Footrot" = "footrot",
                        "Forest decline" = "forest decline",
                        "Forest dieback" = "forest dieback",
                        "Frogeye leaf spot" = "frogeye leaf spot",
                        "Fruit cracking" = "fruit cracking",
                        "Fruit diseases" = "fruit diseases",
                        "Fungal brown spot" = "fungal brown spot",
                        "Fungal diseases" = "fungal diseases",
                        "Fungal wilt" = "fungal wilt",
                        "Fusariosis" = "fusariosis",
                        "Fusarium head blight" = "fusarium head blight",
                        "Fusarium root and stalk rot" = "fusarium root and stalk rot",
                        "Fusarium root rot of beans" = "fusarium root rot of beans",
                        "Fusarium wilt" = "fusarium wilt",
                        "Fusarium wilt of banana" = "fusarium wilt of banana",
                        "Grapevine leafroll disease" = "grapevine leafroll disease",
                        "Gray mold" = "gray mold",
                        "Green mould" = "green mould",
                        "Green mould rot" = "green mould rot",
                        "Green rot" = "green rot",
                        "Greening" = "greening",
                        "Greening disease" = "greening disease",
                        "Grey leaf spot disease" = "grey leaf spot disease",
                        "Gummosis" = "gummosis",
                        "Head rot" = "head rot",
                        "Heart rot" = "heart rot",
                        "Hollow heart" = "hollow heart",
                        "Hyperhydricity" = "hyperhydricity",
                        "Ink disease" = "ink disease",
                        "Internal browning" = "internal browning",
                        "Kober stem grooving" = "kober stem grooving",
                        "Laurel wilt" = "laurel wilt",
                        "Leaf blotch" = "leaf blotch",
                        "Leaf curls" = "leaf curls",
                        "Leaf rust" = "leaf rust",
                        "Lentil blight" = "lentil blight",
                        "Lophodermium needle cast" = "lophodermium needle cast",
                        "Maize stalk rot" = "maize stalk rot",
                        "Maize wallaby ear disease" = "maize wallaby ear disease",
                        "Mango malformation disease" = "mango malformation disease",
                        "Mildews" = "mildews",
                        "Net blotch" = "net blotch",
                        "Oleocellosis" = "oleocellosis",
                        "Olive knot disease" = "olive knot disease",
                        "Panama disease" = "panama disease",
                        "Panama disease of banana" = "panama disease of banana",
                        "Pecan scab" = "pecan scab",
                        "Phoma leaf spot disease" = "phoma leaf spot disease",
                        "Phoma stem canker disease" = "phoma stem canker disease",
                        "Phyllody" = "phyllody",
                        "Phytoplasmal diseases" = "phytoplasmal diseases",
                        "Pierce's disease" = "pierce's disease",
                        "Plant diseases" = "plant diseases",
                        "Plant galls" = "plant galls",
                        "Plant rots" = "plant rots",
                        "Plant tumours" = "plant tumours",
                        "Plum pockets" = "plum pockets",
                        "Pocket plums" = "pocket plums",
                        "Pod rot" = "pod rot",
                        "Postharvest diseases" = "postharvest diseases",
                        "Potato blight" = "potato blight",
                        "Powdery mildew" = "powdery mildew",
                        "Ratoon stunting disease" = "ratoon stunting disease",
                        "Replant disease" = "replant disease",
                        "Rhizomania" = "rhizomania",
                        "Rice blast disease" = "rice blast disease",
                        "Root diseases" = "root diseases",
                        "Root rot" = "root rot",
                        "Root rots" = "root rots",
                        "Root tumors" = "root tumors",
                        "Rose rosette disease" = "rose rosette disease",
                        "Rots" = "rots",
                        "Rugose wood complex" = "rugose wood complex",
                        "Rupestris stem pitting" = "rupestris stem pitting",
                        "Rust diseases" = "rust diseases",
                        "Rusts" = "rusts",
                        "Scab diseases" = "scab diseases",
                        "Scabs" = "scabs",
                        "Scald" = "scald",
                        "Scald diseases" = "scald diseases",
                        "Scorch" = "scorch",
                        "Seed rot" = "seed rot",
                        "Seed-borne diseases" = "seed-borne diseases",
                        "Seedling death" = "seedling death",
                        "Seedling diseases" = "seedling diseases",
                        "Sharka disease" = "sharka disease",
                        "Shot holes" = "shot holes",
                        "Silver scurf" = "silver scurf",
                        "Smut diseases" = "smut diseases",
                        "Smuts" = "smuts",
                        "Snow blight" = "snow blight",
                        "Snow molds" = "snow molds",
                        "Soil-borne diseases" = "soil-borne diseases",
                        "Sooty molds" = "sooty molds",
                        "Southern corn leaf blight" = "southern corn leaf blight",
                        "Southern leaf blight of maize" = "southern leaf blight of maize",
                        "Soybean sudden death syndrome" = "soybean sudden death syndrome",
                        "Spear rot" = "spear rot",
                        "Spots" = "spots",
                        "Spring dead spot" = "spring dead spot",
                        "Stem rot" = "stem rot",
                        "Stripe rust" = "stripe rust",
                        "Sudden oak death" = "sudden oak death",
                        "Sugarcane rust" = "sugarcane rust",
                        "Sweet potato scab" = "sweet potato scab",
                        "Swiss needle cast" = "swiss needle cast",
                        "Take-all disease" = "take-all disease",
                        "Terminal crook disease" = "terminal crook disease",
                        "Tree diseases" = "tree diseases",
                        "Vascular wilt" = "vascular wilt",
                        "Verticillium wilt" = "verticillium wilt",
                        "Viral diseases of plants" = "viral diseases of plants",
                        "Vitrification" = "vitrification",
                        "Weather fleck" = "weather fleck",
                        "White rot" = "white rot",
                        "Wildfire disease" = "wildfire disease",
                        "Wilts" = "wilts",
                        "Witches broom" = "witches broom",
                        "Woolliness" = "woolliness",
                        "Yellow sigatoka" = "yellow sigatoka",
                        "Yellow vine disease" = "yellow vine disease",
                        "Zebra chip disease" = "zebra chip disease",
                        "Zonate leaf spot" = "zonate leaf spot")),
          actionButton('display', 'Display'),
          width=3
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("choropleth"),
          width=9
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  
  rv$df <- df
  data <- df %>% 
    group_by(code, country) %>% 
    summarise(value = sum(value))
  data <- subset(data, code != "#N/A")
  data$crop <- 'all'
  data$disease <- 'all'
  data <- data[, c('crop', 'disease', 'country', 'code', 'value')]
  rv$data <- subset(data, value!=0)
  
  # generate choropleth
  output$choropleth <- renderPlotly({
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
    print('success')
    
    colours <- rev(c('#3b771e', '#f0faeb'))
    
    fig <- plot_geo(rv$data)
    fig <- fig %>% add_trace(
      z = ~value, color = ~value, colors = colours,
      text = ~country, locations = ~code, marker = list(line = l)
    )
    fig <- fig %>% colorbar(title = 'Records')
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
    
  })
  
  observeEvent(input$display, {
    
    if(input$crop=='all' && input$disease=='all'){
      rv$df <- df
      data <- rv$df %>% 
        group_by(code, country) %>% 
        summarise(value = sum(value))
      data <- subset(data, code != "#N/A")
      data$crop <- 'all'
      data$disease <- 'all'
      data <- data[, c('crop', 'disease', 'country', 'code', 'value')]
      rv$data <- subset(data, value!=0)
    } else if(input$crop=='all'){
      rv$df <- subset(df, disease==input$disease)
      data <- rv$df %>% 
        group_by(code, country) %>% 
        summarise(value = sum(value))
      data <- subset(data, code != "#N/A")
      data$disease <- input$disease
      data$crop <- input$crop
      data <- data[, c('crop', 'disease', 'country', 'code', 'value')]
      rv$data <- subset(data, value!=0)
    } else if(input$disease=='all'){
      rv$df <- subset(df, crop==input$crop)
      data <- rv$df %>% 
        group_by(code, country) %>% 
        summarise(value = sum(value))
      data <- subset(data, code != "#N/A")
      data$crop <- input$crop
      data$disease <- input$disease
      data <- data[, c('crop', 'disease', 'country', 'code', 'value')]
      rv$data <- subset(data, value!=0)
    } else {
      rv$df <- filter(df, crop==input$crop & disease==input$disease)
      data <- rv$df %>% 
        group_by(code, country) %>% 
        summarise(value = sum(value))
      data <- subset(data, code != "#N/A")
      data$crop <- input$crop
      data$disease <- input$disease
      data <- data[, c('crop', 'disease', 'country', 'code', 'value')]
      rv$data <- subset(data, value!=0)
    }
    print(paste(input$crop, input$disease))
    print(rv$data)
    
    
    # generate choropleth
    output$choropleth <- renderPlotly({
        
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator')
      )
      print('success')
      
      colours <- rev(c('#ed4029', '#f89d29', '#f9c216', '#66cc33'))
      
      fig <- plot_geo(rv$data)
      fig <- fig %>% add_trace(
        z = ~value, color = ~value, colors = colours,
        text = ~country, locations = ~code, marker = list(line = l)
      )
      fig <- fig %>% colorbar(title = 'Records')
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
      
    })
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
