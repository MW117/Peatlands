#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This app generates a sidebar where you select a number of restoration periods, and it generates
# settings elements for each period

# The respective settings are indexed. So for the start date for period 1, you can call this with
# input$start_date1. For the peatland type for period 4, it's input$peatland_type4

library(shiny)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(janitor)
library(ggplot2)
library(plotly)

#function to create a list of the UI elements require for a single restoration period
peatland_ui = function(period_n) { #period_n is an index to identify periods
  list(
    "Period " %>% str_c(period_n) %>% h3,
    
    
    
    sliderInput("DA" %>% str_c(period_n), "First and Last Years of Peatland Restoration Period",
                min = 2020,
                max = 2100,
                value = c(2020, 2045),
                step = 1,
    ),
    
    
    numericInput("RPYMB" %>% str_c(period_n), "Modified Bog", min = 0, value = 0, step = 100),
    numericInput("RPYWood" %>% str_c(period_n), "Woodland", min = 0, value = 0, step = 100), 
    numericInput("RPYMEB" %>% str_c(period_n), "Modified Eroded Bog", min = 0, value = 0, step = 100),
    numericInput("RPYIG" %>% str_c(period_n), "Intensive Grassland", min = 0, value = 0, step = 100), 
    numericInput("RPYDE" %>% str_c(period_n), "Domestic Extracted", min = 0, value = 0, step = 100),
    numericInput("RPYEG" %>% str_c(period_n), "Extensive Grassland", min = 0, value = 0, step = 100),
    numericInput("RPYCrop" %>% str_c(period_n), "Cropland", min = 0, value = 0, step = 100),  
    numericInput("RPYIE" %>% str_c(period_n), "Industrial Extracted", min = 0, value = 0, step = 100)
  )
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Peatland calculator"),
  
  # Sidebar with a slider input and numeric inputs
  sidebarLayout(
    sidebarPanel(
      #slider to select the number of periods required
      sliderInput("periods",
                  "Number of restoration periods:",
                  min = 1,
                  max = 10,
                  value = 1),
      uiOutput("period_settings"), #the dynamically generated UI for the settings
      
      hr(),
      checkboxInput("showtable", "Display Summary Table?", TRUE),
      checkboxInput("plotcost", "Display Cost plot?", TRUE),
      checkboxInput("plotcumulativecost", "Display Cumulative Cost plot?", TRUE),
      checkboxInput("plotemissionssaved", "Display Emissions Saved plot?", TRUE),
      checkboxInput("plotcumulativeemissionssaved", "Display Cumulative Emissions Saved plot?", TRUE),
      checkboxInput("plotrestored", "Display Restored land type plot?", TRUE),
      checkboxInput("plotcumulativerestored", "Display Cumulative Restored land type plot?", TRUE),
      checkboxInput("plotcremaininglandcover", "Display Remaining Land Cover Cost plot?", TRUE)
    ),
    
    # Show text and plots
    mainPanel(
      uiOutput("text"),
      br(),
      tableOutput("peatlandsummarytable"),
      br(),
      plotlyOutput("CostPlot"),
      br(),
      plotlyOutput("CumulativeCostPlot"),
      br(),
      plotlyOutput("EmissionsPlot"),
      br(),
      plotlyOutput("CumulativeEmissionsPlot"),
      br(),
      plotlyOutput("RestoredPlot"),
      br(),
      plotlyOutput("CumulativeRestoredPlot"),
      br(),
      plotlyOutput("RemainingLandCoverPlot")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$period_settings <- renderUI({ #this updates the UI element each time input$periods changes
    1:input$periods %>%
      map(peatland_ui) #runs peatland_ui for integer values up to input_periods
  })
  
  # Names for various vectors
  RPY_names <- list("RPY_Crop","RPY_IG","RPY_EG","RPY_IE","RPY_Wood","RPY_DE","RPY_MEB","RPY_MB")
  RPY_names_cul <- list("Cul_R_Crop","Cul_R_IG","Cul_R_EG","Cul_R_IE","Cul_R_Wood","Cul_R_DE","Cul_R_MEB","Cul_R_MB","Cul_Total_RPY")
  RPY_names_remaining <- list("Crop_Remain","IG_Remain","EG_Remain","IE_Remain","Wood_Remain","DE_Remain","MEB_Remain","MB_Remain")
  
  Peatland_data1 = matrix(c(
    "Cropland",	9590,	63.55,	667,
    "Intensive_Grassland",	11291,	27.02,	7174,
    "Extensive_Grassland", 	43623,	91.92,	1565,
    "Industrial_Extracted",	2962,	45.08,	8193,
    "Woodland",	785132,	5.26,	9715,
    "Domestic_Extracted",	66266,	7.18,	982,
    "Modified_Eroded_Bog",	401674,	5.01,	7317,
    "Modified_Bog",	434490,	6.59,	805),
    nrow = 8, ncol = 4, byrow = TRUE);
  
  colnames(Peatland_data1) <- (c("Category",	"Current_Hectares",	"Emissions_Saved_tCO2_per_ha_per_year",	"Restoration_Cost"))
  
  Peatland_data1 <- as.data.frame((Peatland_data1))
  
  # Creat dataframe for data 
  Peatland_data1$Category = as.character(Peatland_data1$Category)
  Peatland_data1$Current_Hectares = as.numeric(as.character(Peatland_data1$Current_Hectares))
  Peatland_data1$Emissions_Saved_tCO2_per_ha_per_year = as.numeric(as.character(Peatland_data1$Emissions_Saved_tCO2_per_ha_per_year))
  Peatland_data1$Restoration_Cost = as.numeric(as.character(Peatland_data1$Restoration_Cost))
  
  # Initialise vectors
  LStart_Date = vector(,0)
  LEnd_Date = vector(,0)
  LRPY_Crop = vector(,0)
  LRPY_IG = vector(,0)
  LRPY_EG = vector(,0)
  LRPY_IE = vector(,0)
  LRPY_Wood = vector(,0)
  LRPY_DE = vector(,0)
  LRPY_MEB = vector(,0)
  LRPY_MB = vector(,0)
  
  
  # Fucntion to calculate the additional hectares of each type of peatland to be restored over the various periods
  peatland_function_0 <- function(Numb_periods = 1){
    for (i in 1 : Numb_periods){
      
      nam1 <- paste("DA", i, sep = "")
      nam3 <- paste("RPYCrop", i, sep = "")
      nam4 <- paste("RPYIG", i, sep = "")
      nam5 <- paste("RPYEG", i, sep = "")
      nam6 <- paste("RPYIE", i, sep = "")
      nam7 <- paste("RPYWood", i, sep = "")
      nam8 <- paste("RPYDE", i, sep = "")
      nam9 <- paste("RPYMEB", i, sep = "")
      nam10 <- paste("RPYMB", i, sep = "")
      
      Int_Date <- input[[nam1]]
      Int_Start_Date <- Int_Date[1]
      Int_End_Date <- Int_Date[2]
      
      Int_RPY_Crop <- input[[nam3]]
      Int_RPY_IG <- input[[nam4]]
      Int_RPY_EG <- input[[nam5]]
      Int_RPY_IE <- input[[nam6]]
      Int_RPY_Wood <- input[[nam7]]
      Int_RPY_DE <- input[[nam8]]
      Int_RPY_MEB <- input[[nam9]]
      Int_RPY_MB <- input[[nam10]]
      
      
      LStart_Date <- c(LStart_Date, Int_Start_Date)
      LEnd_Date <- c(LEnd_Date, Int_End_Date)
      LRPY_Crop <- c(LRPY_Crop, Int_RPY_Crop)
      LRPY_IG <- c(LRPY_IG, Int_RPY_IG)
      LRPY_EG <- c(LRPY_EG, Int_RPY_EG)
      LRPY_IE <- c(LRPY_IE, Int_RPY_IE)
      LRPY_Wood <- c(LRPY_Wood, Int_RPY_Wood)
      LRPY_DE <- c(LRPY_DE, Int_RPY_DE)
      LRPY_MEB <- c(LRPY_MEB, Int_RPY_MEB)
      LRPY_MB <- c(LRPY_MB, Int_RPY_MB)
      
    }
    return(list("SD" = LStart_Date, "ED" = LEnd_Date, "Crop" = LRPY_Crop, "IG" = LRPY_IG, "EG" = LRPY_EG, "IE" = LRPY_IE, "Wood" = LRPY_Wood,
                "DE" = LRPY_DE, "MEB" = LRPY_MEB, "MB" = LRPY_MB))
  }
  
  
  
  # Function calculates all outputs
  peatland_function_1 <- function(periodnumb = 1){
    
    dataf <- peatland_function_0(Numb_periods = periodnumb)
    
    FStart_Date <- dataf$SD
    FEnd_Date <- dataf$ED
    FRPY_Crop <- dataf$Crop
    FRPY_IG <- dataf$IG
    FRPY_EG <- dataf$EG
    FRPY_IE <- dataf$IE
    FRPY_Wood <- dataf$Wood
    FRPY_DE <- dataf$DE
    FRPY_MEB <- dataf$MEB
    FRPY_MB <- dataf$MB
    
    
    # Loops through the various periods of restoration to add the additional types and amounts of peatland restored for the different years
    
    for (i in 1:periodnumb){
      
      
      Year <- FStart_Date[i]:FEnd_Date[i];
      Time_line <- Year - min(Year) + 1;
      
      
      # For first period, there are no previous periods to add the newest periods restoration to
      if (i == 1){
        Cost_per_year = numeric(length(Year));
        Cost_total_mat <- cbind(Year,Cost_per_year);
        
        Additional_Emissions_saved_per_year = numeric(length(Year));
        Emissions_total_mat <- cbind(Year,Additional_Emissions_saved_per_year);
        
        omega2 = matrix(0,length(Year),8);
        colnames(omega2) <- (RPY_names);
        Restored_total_mat <- cbind(Year,omega2);
        
        Cost_total_mat <- as.data.frame((Cost_total_mat));
        Emissions_total_mat <- as.data.frame((Emissions_total_mat));
        Restored_total_mat <- as.data.frame((Restored_total_mat));
      }
      
      RPY_Crop <- as.integer(FRPY_Crop[i])
      RPY_IG <- as.integer(FRPY_IG[i])
      RPY_EG <- as.integer(FRPY_EG[i])
      RPY_IE <- as.integer(FRPY_IE[i])
      RPY_Wood <- as.integer(FRPY_Wood[i])
      RPY_DE <- as.integer(FRPY_DE[i])
      RPY_MEB <- as.integer(FRPY_MEB[i])
      RPY_MB <- as.integer(FRPY_MB[i])
      
      RPY <- as.numeric(c(RPY_Crop,RPY_IG,RPY_EG,RPY_IE,RPY_Wood,RPY_DE,RPY_MEB,RPY_MB));
      
      Cost_per_year_alpha <- RPY * Peatland_data1$Restoration_Cost;
      Cost_per_year <- sum(Cost_per_year_alpha);
      Cost_per_year <- (replicate(length(Time_line),Cost_per_year));
      Cost_period_mat <- cbind(Year,Cost_per_year);
      
      Additional_Emissions_saved_per_year_alpha <- RPY * Peatland_data1$Emissions_Saved_tCO2_per_ha_per_year;
      Additional_Emissions_saved_per_year <- sum(Additional_Emissions_saved_per_year_alpha);
      Additional_Emissions_saved_per_year <- (replicate(length(Time_line),Additional_Emissions_saved_per_year));
      Emissions_period_mat <- cbind(Year,Additional_Emissions_saved_per_year);
      
      
      Restored_per_year <- t(replicate(length(Time_line),RPY));
      colnames(Restored_per_year) <- (RPY_names);
      Restored_period_mat <- cbind(Year,Restored_per_year);
      
      Cost_period_mat <- as.data.frame((Cost_period_mat));
      Emissions_period_mat <- as.data.frame((Emissions_period_mat));
      Restored_period_mat <- as.data.frame((Restored_period_mat));
      
      
      Cost_total_mat <- bind_rows(Cost_total_mat, Cost_period_mat);
      Cost_total_mat <- Cost_total_mat %>% group_by(Year) %>% summarise(Cost_per_year = sum(Cost_per_year));
      
      Emissions_total_mat <- bind_rows(Emissions_total_mat, Emissions_period_mat);
      Emissions_total_mat <- Emissions_total_mat %>% group_by(Year) %>% summarise(Additional_Emissions_saved_per_year = sum(Additional_Emissions_saved_per_year));
      
      Restored_total_mat <- bind_rows(Restored_total_mat, Restored_period_mat);
      Restored_total_mat <- Restored_total_mat %>% group_by(Year) %>% summarise(RPY_Crop = sum(RPY_Crop),
                                                                                RPY_IG = sum(RPY_IG),
                                                                                RPY_EG = sum(RPY_EG),
                                                                                RPY_IE = sum(RPY_IE),
                                                                                RPY_Wood = sum(RPY_Wood),
                                                                                RPY_DE = sum(RPY_DE),
                                                                                RPY_MEB = sum(RPY_MEB),
                                                                                RPY_MB = sum(RPY_MB));
      
      
      
      Restored_total_mat$Total_RPY <- Restored_total_mat$RPY_Crop + Restored_total_mat$RPY_IG + Restored_total_mat$RPY_EG + Restored_total_mat$RPY_IE + 
        Restored_total_mat$RPY_Wood + Restored_total_mat$RPY_DE + Restored_total_mat$RPY_MEB + Restored_total_mat$RPY_MB;
      
      
      
    }
    
    
    
    # Creates a time series for the various data points for the entire programme, across all periods of restoration
    # This is the data plotted in the app
    
    Year <- min(Cost_total_mat$Year):max(Cost_total_mat$Year);
    Year <- as.data.frame(Year);
    
    Cost_total_mat <- full_join(Year,Cost_total_mat, by = "Year");
    Emissions_total_mat <- full_join(Year,Emissions_total_mat, by = "Year");
    Restored_total_mat <- full_join(Year,Restored_total_mat, by = "Year");
    
    
    Cost_total_mat[is.na(Cost_total_mat)] <- 0
    Emissions_total_mat[is.na(Emissions_total_mat)] <- 0
    Restored_total_mat[is.na(Restored_total_mat)] <- 0
    
    
    
    Cumulative_Cost <-  cumsum(Cost_total_mat$Cost_per_year);
    Cost_frame <- cbind(Cost_total_mat,Cumulative_Cost);
    
    
    
    Emissions_saved_per_year <- cumsum(Emissions_total_mat$Additional_Emissions_saved_per_year);
    Cumulative_Emissions_saved <-  cumsum(Emissions_saved_per_year);
    Emissions_frame <- cbind(Emissions_total_mat,Emissions_saved_per_year,Cumulative_Emissions_saved);
    
    
    
    Cumulative_Restored <- apply(Restored_total_mat,2,cumsum);
    Cumulative_Restored = subset(Cumulative_Restored, select = -c(Year));
    colnames(Cumulative_Restored) <- (RPY_names_cul);
    Restored_frame <- cbind(Restored_total_mat,Cumulative_Restored);
    
    
    
    Current_Hectares_mat <- Peatland_data1$Current_Hectares;
    Current_Hectares_mat <- t(replicate(nrow(Year),Current_Hectares_mat));
    colnames(Current_Hectares_mat) <- (RPY_names_remaining);
    Current_Hectares_mat <- cbind(Year,Current_Hectares_mat);
    
    Restored_frame$Crop_Remain <- (Peatland_data1$Current_Hectares[1] - Restored_frame$Cul_R_Crop);
    Restored_frame$IG_Remain <- (Peatland_data1$Current_Hectares[2] - Restored_frame$Cul_R_IG);
    Restored_frame$EG_Remain <- (Peatland_data1$Current_Hectares[3] - Restored_frame$Cul_R_EG);
    Restored_frame$IE_Remain <- (Peatland_data1$Current_Hectares[4] - Restored_frame$Cul_R_IE);
    Restored_frame$Wood_Remain <- (Peatland_data1$Current_Hectares[5] - Restored_frame$Cul_R_Wood);
    Restored_frame$DE_Remain <- (Peatland_data1$Current_Hectares[6] - Restored_frame$Cul_R_DE);
    Restored_frame$MEB_Remain <- (Peatland_data1$Current_Hectares[7] - Restored_frame$Cul_R_MEB);
    Restored_frame$MB_Remain <- (Peatland_data1$Current_Hectares[8] - Restored_frame$Cul_R_MB);
    
    return(list("CostFrame" = Cost_frame, "EmissionsFrame" = Emissions_frame, "RestoredFrame" = Restored_frame))
    
  }
  
  # Functions to create the various plots and the information table at the top of the app
  
  peatland_function_plotcost <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    
    if(plotData == TRUE){
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Restoration Cost per Year (£)",
        titlefont = f
      )
      
      data <- data1$CostFrame
      
      fig <- plot_ly(data, x = ~Year, y = ~Cost_per_year, name = 'Cost', type = 'scatter', mode = 'lines+markers',
                     line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4)) 
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Restoration Cost per Year (£)", hovermode = "x unified")
      fig
    }
  }
  
  
  peatland_function_plotcumulativecost <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    

    if(plotData == TRUE){
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Total Restoration Cost to Date (£)",
        titlefont = f
      )
      
      data <- data1$CostFrame
      
      fig <- plot_ly(data, x = ~Year, y = ~Cumulative_Cost, name = 'Cumulative Cost', type = 'scatter', mode = 'lines+markers',
                     line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4)) 
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Total Restoration Cost to Date (£)", hovermode = "x unified")
      fig
    }
  }
  
  
  peatland_function_plotemissionssaved <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(plotData == TRUE){
      target_years_A = c(2025, 2030, 2032, 2035, 2040, 2045);
      target_years_1 = c(2020, 2025, 2030, 2035, 2040, 2045);
      targets_1 = c( 201693.45, 1112929.34, 944775.19, 943830.67, 1191975.27, 1603103.53);
      
      targets_mat <- as.data.frame(cbind(target_years_A, target_years_1, targets_1));
      
      Emissions_targets_mat <- full_join(data1$EmissionsFrame, targets_mat, by = c("Year" = "target_years_A"));
      
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Emissions Saved per Year (tCO2)",
        titlefont = f
      )
      
      data <- Emissions_targets_mat
      
      fig <- plot_ly(data, x = ~Year, y = ~Emissions_saved_per_year, name = 'Emmision Saved', type = 'scatter', mode = 'markers',
                     line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4))
      fig <- fig %>% add_trace(data, x = ~target_years_1, y = ~targets_1, name = 'Target', type = 'scatter', mode = 'markers',
                               line = list(color = 'rgb(0, 255, 255)', width = 1), marker = list(color = 'rgb(0, 255, 255)', width = 4)) 
      
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Emissions Saved per Year (tCO2)", hovermode = "x unified")
      fig
    }
  }
  
  
  peatland_function_plotcumulativeemissionssaved <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(plotData == TRUE){f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Year",
      titlefont = f
    )
    y <- list(
      title = "Total Emissions Saved to Date (tCO2)",
      titlefont = f
    )
    
    data <- data1$EmissionsFrame
    
    fig <- plot_ly(data, x = ~Year, y = ~Cumulative_Emissions_saved, name = 'Cumulative Emmision Saved', type = 'scatter', mode = 'markers',
                   line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4))
    
    fig <- fig %>%
      layout(xaxis = x, yaxis = y, title = "Total Emissions Saved to Date (tCO2)", hovermode = "x unified")
    fig
    }
  }
  
  
  peatland_function_plotrestored <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(plotData == TRUE){
      
      data <- data1$RestoredFrame
      TRPYMB <- tail(data1$RestoredFrame$Cul_R_MB, n=1)
      TRPYWood <- tail(data1$RestoredFrame$Cul_R_Wood, n=1)
      TRPYMEB <- tail(data1$RestoredFrame$Cul_R_MEB, n=1)
      TRPYIG <- tail(data1$RestoredFrame$Cul_R_IG, n=1)
      TRPYDE <- tail(data1$RestoredFrame$Cul_R_DE, n=1)
      TRPYEG <- tail(data1$RestoredFrame$Cul_R_EG, n=1)
      TRPYCrop <- tail(data1$RestoredFrame$Cul_R_Crop, n=1)
      TRPYIE <- tail(data1$RestoredFrame$Cul_R_IE, n=1)
      
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Hectares of Land Restored per Year",
        titlefont = f
      )
      
      
      fig <- plot_ly(data, x = ~Year, y = ~Total_RPY, name = 'Total Land', type = 'scatter', mode = 'markers',
                     line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4))
      
      if(TRPYMB != 0){
        fig <- fig %>% add_trace(y = ~RPY_MB, name = 'Modified Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 0, 0)', width = 1), marker = list(color = 'rgb(255, 0, 0)', width = 4))
      }
      if(TRPYWood != 0){
        fig <- fig %>% add_trace(y = ~RPY_Wood, name = 'Woodland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 128, 0)', width = 1), marker = list(color = 'rgb(255, 128, 0)', width = 4))
      }
      if(TRPYMEB != 0){
        fig <- fig %>% add_trace(y = ~RPY_MEB, name = 'Modified Eroded Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 255, 0)', width = 1), marker = list(color = 'rgb(255, 255, 0)', width = 4))
      }
      if(TRPYIG != 0){
        fig <- fig %>% add_trace(y = ~RPY_IG, name = 'Intensive Grassland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 255, 0)', width = 1), marker = list(color = 'rgb(0, 255, 0)', width = 4))
      }
      if(TRPYDE != 0){
        fig <- fig %>% add_trace(y = ~RPY_DE, name = 'Domestic Extracted', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 255, 255)', width = 1), marker = list(color = 'rgb(0, 255, 255)', width = 4))
      }
      if(TRPYEG != 0){
        fig <- fig %>% add_trace(y = ~RPY_EG, name = 'Extensive Grassland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 0, 255)', width = 1), marker = list(color = 'rgb(0, 0, 255)', width = 4)) 
      }
      if(TRPYCrop != 0){
        fig <- fig %>% add_trace(y = ~RPY_Crop, name = 'Cropland Restored', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(128, 0, 255)', width = 1), marker = list(color = 'rgb(128, 0, 255)', width = 4))
      }
      if(TRPYIE != 0){
        fig <- fig %>% add_trace(y = ~RPY_IE, name = 'Industrial Extracted', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 0, 255)', width = 1), marker = list(color = 'rgb(255, 0, 255)', width = 4)) 
      }
      
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Hectares of Land Restored per Year", hovermode = "x unified")
      fig
    }
  }
  
  
  peatland_function_plotcumulativerestored <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(plotData == TRUE){
      
      data <- data1$RestoredFrame
      TRPYMB <- tail(data1$RestoredFrame$Cul_R_MB, n=1)
      TRPYWood <- tail(data1$RestoredFrame$Cul_R_Wood, n=1)
      TRPYMEB <- tail(data1$RestoredFrame$Cul_R_MEB, n=1)
      TRPYIG <- tail(data1$RestoredFrame$Cul_R_IG, n=1)
      TRPYDE <- tail(data1$RestoredFrame$Cul_R_DE, n=1)
      TRPYEG <- tail(data1$RestoredFrame$Cul_R_EG, n=1)
      TRPYCrop <- tail(data1$RestoredFrame$Cul_R_Crop, n=1)
      TRPYIE <- tail(data1$RestoredFrame$Cul_R_IE, n=1)
      
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Total Hectares of Land Restored to Date",
        titlefont = f
      )
      
      
      fig <- plot_ly(data, x = ~Year, y = ~Cul_Total_RPY, name = 'Total Land', type = 'scatter', mode = 'markers',
                     line = list(color = 'rgb(0, 0, 0)', width = 1), marker = list(color = 'rgb(0, 0, 0)', width = 4))
      
      if(TRPYMB != 0){
        fig <- fig %>% add_trace(y = ~Cul_R_MB, name = 'Modified Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 0, 0)', width = 1), marker = list(color = 'rgb(255, 0, 0)', width = 4))
      }
      if(TRPYWood != 0){
        fig <- fig %>% add_trace(y = ~Cul_R_Wood, name = 'Woodland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 128, 0)', width = 1), marker = list(color = 'rgb(255, 128, 0)', width = 4))
      }
      if(TRPYMEB != 0){
        fig <- fig %>% add_trace(y = ~Cul_R_MEB, name = 'Modified Eroded Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 255, 0)', width = 1), marker = list(color = 'rgb(255, 255, 0)', width = 4))
      }
      if(TRPYIG != 0){fig <- fig %>% add_trace(y = ~Cul_R_IG, name = 'Intensive Grassland', type = 'scatter', mode = 'markers',
                                               line = list(color = 'rgb(0, 255, 0)', width = 1), marker = list(color = 'rgb(0, 255, 0)', width = 4))
      }
      if(TRPYDE != 0){fig <- fig %>% add_trace(y = ~Cul_R_DE, name = 'Domestic Extracted', type = 'scatter', mode = 'markers',
                                               line = list(color = 'rgb(0, 255, 255)', width = 1), marker = list(color = 'rgb(0, 255, 255)', width = 4))
      }
      if(TRPYEG != 0){fig <- fig %>% add_trace(y = ~Cul_R_EG, name = 'Extensive Grassland', type = 'scatter', mode = 'markers',
                                               line = list(color = 'rgb(0, 0, 255)', width = 1), marker = list(color = 'rgb(0, 0, 255)', width = 4)) 
      }
      if(TRPYCrop != 0){fig <- fig %>% add_trace(y = ~Cul_R_Crop, name = 'Cropland', type = 'scatter', mode = 'markers',
                                                 line = list(color = 'rgb(128, 0, 255)', width = 1), marker = list(color = 'rgb(128, 0, 255)', width = 4))
      }
      if(TRPYIE != 0){fig <- fig %>% add_trace(y = ~Cul_R_IE, name = 'Industrial Extracted', type = 'scatter', mode = 'markers',
                                               line = list(color = 'rgb(255, 0, 255)', width = 1), marker = list(color = 'rgb(255, 0, 255)', width = 4)) 
      }
      
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Total Hectares of Land Restored to Date", hovermode = "x unified")
      fig
    }
  }
  
  
  peatland_function_plotcremaininglandcover <- function(plotData = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(plotData == TRUE){
      
      
      data <- data1$RestoredFrame
      TRPYMB <- tail(data1$RestoredFrame$Cul_R_MB, n=1)
      TRPYWood <- tail(data1$RestoredFrame$Cul_R_Wood, n=1)
      TRPYMEB <- tail(data1$RestoredFrame$Cul_R_MEB, n=1)
      TRPYIG <- tail(data1$RestoredFrame$Cul_R_IG, n=1)
      TRPYDE <- tail(data1$RestoredFrame$Cul_R_DE, n=1)
      TRPYEG <- tail(data1$RestoredFrame$Cul_R_EG, n=1)
      TRPYCrop <- tail(data1$RestoredFrame$Cul_R_Crop, n=1)
      TRPYIE <- tail(data1$RestoredFrame$Cul_R_IE, n=1)
      
      f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
      x <- list(
        title = "Year",
        titlefont = f
      )
      y <- list(
        title = "Remaining Hectares of Land Cover",
        titlefont = f
      )
      
      
      
      fig <- plot_ly(data, x = ~Year, y = ~(MB_Remain/MB_Remain), type = 'scatter', mode = 'markers',
                     line = list(color = 'rgb(255, 255, 255)', width = 0), marker = list(color = 'rgb(255, 255, 255)', width = 1))
      if(TRPYMB != 0){
        fig <- fig %>% add_trace(y = ~MB_Remain, name = 'Modified Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 0, 0)', width = 1), marker = list(color = 'rgb(255, 0, 0)', width = 4))
      }
      if(TRPYWood != 0){
        fig <- fig %>% add_trace(y = ~Wood_Remain, name = 'Woodland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 128, 0)', width = 1), marker = list(color = 'rgb(255, 128, 0)', width = 4))
      }
      if(TRPYMEB != 0){
        fig <- fig %>% add_trace(y = ~MEB_Remain, name = 'Modified Eroded Bog', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 255, 0)', width = 1), marker = list(color = 'rgb(255, 255, 0)', width = 4))
      }
      if(TRPYIG != 0){
        fig <- fig %>% add_trace(y = ~IG_Remain, name = 'Intensive Grassland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 255, 0)', width = 1), marker = list(color = 'rgb(0, 255, 0)', width = 4))
      }
      if(TRPYDE != 0){
        fig <- fig %>% add_trace(y = ~DE_Remain, name = 'Domestic Extracted', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 255, 255)', width = 1), marker = list(color = 'rgb(0, 255, 255)', width = 4))
      }
      if(TRPYEG != 0){
        fig <- fig %>% add_trace(y = ~EG_Remain, name = 'Extensive Grassland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(0, 0, 255)', width = 1), marker = list(color = 'rgb(0, 0, 255)', width = 4)) 
      }
      if(TRPYCrop != 0){
        fig <- fig %>% add_trace(y = ~Crop_Remain, name = 'Cropland', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(128, 0, 255)', width = 1), marker = list(color = 'rgb(128, 0, 255)', width = 4))
      }
      if(TRPYIE != 0){
        fig <- fig %>% add_trace(y = ~IE_Remain, name = 'Industrial Extracted', type = 'scatter', mode = 'markers',
                                 line = list(color = 'rgb(255, 0, 255)', width = 1), marker = list(color = 'rgb(255, 0, 255)', width = 4)) 
      }
      
      fig <- fig %>%
        layout(xaxis = x, yaxis = y, title = "Remaining Hectares of Land Cover", hovermode = "x unified")
      fig
    }
  }
  
  peatland_function_table <- function(showtable = TRUE){
    
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    if(showtable == TRUE){
      TRPYMB <- tail(data1$RestoredFrame$Cul_R_MB, n=1)
      TRPYWood <- tail(data1$RestoredFrame$Cul_R_Wood, n=1)
      TRPYMEB <- tail(data1$RestoredFrame$Cul_R_MEB, n=1)
      TRPYIG <- tail(data1$RestoredFrame$Cul_R_IG, n=1)
      TRPYDE <- tail(data1$RestoredFrame$Cul_R_DE, n=1)
      TRPYEG <- tail(data1$RestoredFrame$Cul_R_EG, n=1)
      TRPYCrop <- tail(data1$RestoredFrame$Cul_R_Crop, n=1)
      TRPYIE <- tail(data1$RestoredFrame$Cul_R_IE, n=1)
      
      RMB <- tail(data1$RestoredFrame$MB_Remain, n=1)
      RWood <- tail(data1$RestoredFrame$Wood_Remain, n=1)
      RMEB <- tail(data1$RestoredFrame$MEB_Remain, n=1)
      RIG <- tail(data1$RestoredFrame$IG_Remain, n=1)
      RDE <- tail(data1$RestoredFrame$DE_Remain, n=1)
      REG <- tail(data1$RestoredFrame$EG_Remain, n=1)
      RCrop <- tail(data1$RestoredFrame$Crop_Remain, n=1)
      RIE <- tail(data1$RestoredFrame$IE_Remain, n=1)
      
      TPY = vector(,0)
      TRPY = vector(,0)
      RPL = vector(,0)
      
      if(TRPYMB != 0){
        TPY = append(TPY, "Modified Bog")
        TRPY = append(TRPY, TRPYMB)
        RPL = append(RPL, RMB)
      }
      
      if(TRPYWood != 0){
        TPY = append(TPY, "Woodland")
        TRPY = append(TRPY, TRPYWood)
        RPL = append(RPL, RWood)
      }
      
      if(TRPYMEB != 0){
        TPY = append(TPY, "Modified Eroded Bog")
        TRPY = append(TRPY, TRPYMEB)
        RPL = append(RPL, RMEB)
      }
      
      if(TRPYIG != 0){
        TPY = append(TPY, "Intensive Grassland")
        TRPY = append(TRPY, TRPYIG)
        RPL = append(RPL, RIG)
      }  
      
      if(TRPYDE != 0){
        TPY = append(TPY, "Domestic Extracted")
        TRPY = append(TRPY, TRPYDE)
        RPL = append(RPL, RDE)
      }
      
      if(TRPYEG != 0){
        TPY = append(TPY, "Extensive Grassland")
        TRPY = append(TRPY, TRPYEG)
        RPL = append(RPL, REG)
      }
      
      if(TRPYCrop != 0){
        TPY = append(TPY, "Cropland")
        TRPY = append(TRPY, TRPYCrop)
        RPL = append(RPL, RCrop)
      }
      
      if(TRPYIE != 0){
        TPY = append(TPY, "Industrial Extracted")
        TRPY = append(TRPY, TRPYIE)
        RPL = append(RPL, RIE)
      }
      
      peatlandsummarytable <- data.frame(TPY, TRPY, RPL)
      
      colnames(peatlandsummarytable) <- c('Land Cover', 'Hectares Restored', 'Hectares Remaining')
      
      return(peatlandsummarytable)
    }
  }
    
      
  
  output$text <- renderText({
    data1 <- peatland_function_1(periodnumb = input$periods)
    
    TCOST <- tail(data1$CostFrame$Cumulative_Cost, n=1)
    TESPY <- tail(data1$EmissionsFrame$Emissions_saved_per_year, n=1)
    TES <- tail(data1$EmissionsFrame$Cumulative_Emissions_saved, n=1)
    
    
    TRPYMB <- tail(data1$RestoredFrame$Cul_R_MB, n=1)
    TRPYWood <- tail(data1$RestoredFrame$Cul_R_Wood, n=1)
    TRPYMEB <- tail(data1$RestoredFrame$Cul_R_MEB, n=1)
    TRPYIG <- tail(data1$RestoredFrame$Cul_R_IG, n=1)
    TRPYDE <- tail(data1$RestoredFrame$Cul_R_DE, n=1)
    TRPYEG <- tail(data1$RestoredFrame$Cul_R_EG, n=1)
    TRPYCrop <- tail(data1$RestoredFrame$Cul_R_Crop, n=1)
    TRPYIE <- tail(data1$RestoredFrame$Cul_R_IE, n=1)
    
    
    RMB <- tail(data1$RestoredFrame$MB_Remain, n=1)
    RWood <- tail(data1$RestoredFrame$Wood_Remain, n=1)
    RMEB <- tail(data1$RestoredFrame$MEB_Remain, n=1)
    RIG <- tail(data1$RestoredFrame$IG_Remain, n=1)
    RDE <- tail(data1$RestoredFrame$DE_Remain, n=1)
    REG <- tail(data1$RestoredFrame$EG_Remain, n=1)
    RCrop <- tail(data1$RestoredFrame$Crop_Remain, n=1)
    RIE <- tail(data1$RestoredFrame$IE_Remain, n=1)
    
    if( (RMB < 0) | (RWood < 0) | (RMEB < 0) | (RIG < 0) | (RDE < 0) | (REG < 0)| (RCrop < 0)| (RIE < 0) ){
      text_1 <- paste(
        "<h3>", "Summary", "</h3>",
        
        "<b> ERROR! RAN OUT OF LAND! <b> <br>")
    }
    else{
      text_1 <- paste(
        "<h3>", "Summary", "</h3>",
        
        "Total Cost of restoration: £", format(TCOST, big.mark = ","),
        "<br>",
        
        "CO2 Emissions saved per year in final year: ", format(TESPY, big.mark = ","),  " Tonnes",
        "<br>",
        
        "Total CO2 Emissions Saved: ", format(TES, big.mark = ","),  " Tonnes",
        "<br>",
        
        "Total Peatland restored: ", format(TRPYMB+TRPYWood+TRPYMEB+TRPYIG+TRPYDE+TRPYEG+TRPYCrop+TRPYIE, big.mark = ","),  " Hectares",
        "<br>")
    }
    
    
    
    if(RMB < 0){
      text_2 <- paste(
        "<b> ERROR: RAN OUT OF Modified Bog! <b> <br>"
      )
    }
    else{
      text_2 <- paste(
        " "
      )
    }
    if(RWood < 0){
      text_3 <- paste(
        "<b> ERROR: RAN OUT OF Woodland! <b> <br>"
      )
    }
    else{
      text_3 <- paste(
        " "
      )
    }    
    if(RMEB < 0){
      text_4 <- paste(
        "<b> ERROR: RAN OUT OF Modified Eroded Bog! <b> <br>"
      )
    }
    else{
      text_4 <- paste(
        " "
      )
    }     
    if(RIG < 0){
      text_5 <- paste(
        "<b> ERROR: RAN OUT OF Intensive Grassland! <b> <br>"
      )
    }
    else{
      text_5 <- paste(
        " "
      )
    }    
    if(RDE < 0){
      text_6 <- paste(
        "<b> ERROR: RAN OUT OF Domestic Extracted! <b> <br>"
      )
    }
    else{
      text_6 <- paste(
        " "
      )
    }       
    if(REG < 0){
      text_7 <- paste(
        "<b> ERROR: RAN OUT OF Extensive Grassland! <b> <br>"
      )
    }
    else{
      text_7 <- paste(
        " "
      )
    }       
    if(RCrop < 0){
      text_8 <- paste(
        "<b> ERROR: RAN OUT OF Cropland! <b> <br>"
      )
    }
    else{
      text_8 <- paste(
        " "
      )
    }           
    if(RIE < 0){
      text_9 <- paste(
        "<b> ERROR: RAN OUT OF Industrial Extracted! <b> <br>"
      )
    }
    else{
      text_9 <- paste(
        " "
      )
    }          
   
    
    paste(text_1, text_2, text_3, text_4, text_5, text_6, text_7, text_8, text_9)
    
    
  })
  
  output$peatlandsummarytable <- renderTable({
    peatland_function_table(showtable = input$showtable)
  })
  
  
  output$CostPlot <- renderPlotly({
    peatland_function_plotcost(plotData = input$plotcost)
  })
  
  output$CumulativeCostPlot <- renderPlotly({
    peatland_function_plotcumulativecost(plotData = input$plotcumulativecost)
  })
  
  output$EmissionsPlot <- renderPlotly({
    peatland_function_plotemissionssaved(plotData = input$plotemissionssaved)
  })    
  
  output$CumulativeEmissionsPlot <- renderPlotly({
    peatland_function_plotcumulativeemissionssaved(plotData = input$plotcumulativeemissionssaved)
  })      
  
  output$RestoredPlot <- renderPlotly({
    peatland_function_plotrestored(plotData = input$plotrestored)
  })   
  
  output$CumulativeRestoredPlot <- renderPlotly({
    peatland_function_plotcumulativerestored(plotData = input$plotcumulativerestored)
  })   
  
  output$RemainingLandCoverPlot <- renderPlotly({
    peatland_function_plotcremaininglandcover(plotData = input$plotcremaininglandcover)
  })   
}

# Run the application
shinyApp(ui = ui, server = server)

