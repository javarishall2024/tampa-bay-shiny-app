#Sports Management Analytics Final Project
 #Greg Halperin and Javaris Hall


setwd("C:/Users/grego/OneDrive/Documents/Northwestern/2024/Sports Management Analytics- MSDS 457/Final Project")

library(devtools)
#Make sure to uninstall baseballr, then run the below code to get the correct version of baseballr
#devtools::install_github(repo = "BillPetti/baseballr")
library(baseballr)
library(dplyr)
library(paletteer)
library(ggplot2)
library(ggpubr)
library(plotly)
library(shinythemes)
library(DT)

pitcherIDS<- read.csv("TBPitcherIDs.csv")
hitterIDS <- read.csv("TBBatterIDs.csv")

# Initialize an empty data frame to store all data
pitchers <- data.frame()
hitters <- data.frame()


# For loop to iterate through each Pitcher's ID and scrape data to fill
# pitchers data frame

for(i in 1:nrow(pitcherIDS)) {
  pitcher_id <- pitcherIDS$MLBAMID[i]
  
  # Scraping player stats using baseballr
  pitcher_data <- tryCatch({
    pitcher_stats <- statcast_search_pitchers("2024-03-28", "2024-09-30", pitcher_id)
    pitcher_stats
  }, error = function(e) {
    message("Error for player ID: ", pitcher_id)
    return(NULL)
  })
  
  # Add to the list if successful
  if (!is.null(pitcher_data)) {
    pitchers <- rbind(pitchers,pitcher_data)
  }
}

#Adding a pitcher team variable to pull from in Shiny App
pitchers <- pitchers %>%
  mutate(pitch_team = ifelse(inning_topbot == "Top", home_team, away_team))

#Remove unnecessary columns

pitchers <- pitchers %>%
  select(-spin_dir, -spin_rate_deprecated, -break_angle_deprecated,-sv_id,
         -tfs_deprecated, -tfs_zulu_deprecated, -umpire, , -hit_distance_sc,
         -fielder_2, -fielder_3, -fielder_4, -fielder_5,
         -fielder_6, -fielder_7, -fielder_8, -fielder_9, -delta_home_win_exp,
         -delta_pitcher_run_exp, -if_fielding_alignment, -of_fielding_alignment, 
         -home_score, -away_score, -hyper_speed, -home_score_diff, -bat_score_diff,
         -home_win_exp, -bat_win_exp, -age_pit_legacy, -age_bat_legacy, -age_pit,
         -age_bat, -batter_days_since_prev_game, -batter_days_until_next_game,
         -pitcher_days_until_next_game, -pitcher_days_since_prev_game, -delta_run_exp,
         -outs_when_up, -at_bat_number, -post_bat_score, -break_length_deprecated,
         -bat_score, -fld_score, -post_away_score, -post_home_score, -post_fld_score)

#Edit pitch_type to be named correctly

pitchers$pitch_name[pitchers$pitch_type == "FS"] <- "Splitter"
pitchers$pitch_name[pitchers$pitch_type == "SI"] <- "Sinker"
pitchers$pitch_name[pitchers$pitch_type == "SL"] <- "Slider"
pitchers$pitch_name[pitchers$pitch_type == "FF"] <- "Four-Seam Fastball"
pitchers$pitch_name[pitchers$pitch_type == "ST"] <- "Sweeper"
pitchers$pitch_name[pitchers$pitch_type == "FC"] <- "Cutter"
pitchers$pitch_name[pitchers$pitch_type == "CU"] <- "Curveball"
pitchers$pitch_name[pitchers$pitch_type == "CH"] <- "Changeup"
pitchers$pitch_name[pitchers$pitch_type == "SV"] <- "Sweeper"

#Add any additional pitches in the future

#Eliminate pitches that have a blank pitch_type
pitchers <- pitchers %>% filter(pitch_type != "")


#Pitch Color Palette
pitch_colors <- c(
  "Four-Seam Fastball" = "#FF0000",  # Red
  "Sinker" = "#FF8C00",             # Orange
  "Changeup" = "#00AA00",           # Green
  "Slider" = "#0000FF",             # Blue
  "Curveball" = "#800080",          # Purple
  "Knuckle Curve" = "#702670",      # Midnight Purple
  "Cutter" = "#FF69B4",             # Pink
  "Splitter" = "#8B4513",           # Brown
  "Sweeper" = "#4B0082"             # Indigo
)


########### Define the movement plot function
create_movement_plotly <- function(data = pitchers, full_data = pitchers, player = NULL, selected_pitches = NULL) {
  
  # First get the handedness of the selected pitcher
  pitcher_hand <- full_data %>%
    filter(player_name == player) %>%
    pull(p_throws) %>%
    unique()  # Create a color mapping for pitch types
  
  
  # Filter and process base data
  plot_data <- data %>%
    {if (!is.null(player)) filter(., player_name == player) else .} %>%
    filter(!is.na(pfx_x), !is.na(pfx_z), !is.na(release_speed)) %>%
    mutate(
      release_speed = round(release_speed, 1),
      pfx_z = round(pfx_z * 12, 1),
      pfx_x = round(pfx_x * 12, 1)
    )
  
  # Calculate pitcher's average movement for each pitch type
  pitcher_avg <- plot_data %>%
    group_by(pitch_name) %>%
    summarize(
      avg_pfx_x = mean(pfx_x),
      avg_pfx_z = mean(pfx_z),
      avg_speed = mean(release_speed),
      avg_spin = mean(release_spin_rate)
    )
  
  # Calculate league average movement for each pitch type using full_data, matching pitcher handedness
  league_avg <- full_data %>%
    filter(!is.na(pfx_x), 
           !is.na(pfx_z), 
           !is.na(release_speed),
           p_throws == pitcher_hand) %>%  # Filter for same-handed pitchers only
    mutate(
      pfx_z = pfx_z * 12,
      pfx_x = pfx_x * 12
    ) %>%
    group_by(pitch_name) %>%
    summarize(
      avg_pfx_x = mean(pfx_x),
      avg_pfx_z = mean(pfx_z),
      avg_speed = mean(release_speed),
      avg_spin = mean(release_spin_rate)
    ) %>%
    filter(pitch_name %in% selected_pitches)
  
  # Create hover text for individual pitches
  hover_text <- with(plot_data,
                     paste("Pitch Type:", pitch_name,
                           "<br>Velocity:", release_speed, "mph",
                           "<br>Vertical Movement:", pfx_z, "in",
                           "<br>Horizontal Movement:", pfx_x, "in",
                           "<br>Spin Rate:", release_spin_rate, "rpm",
                           "<br>Result:", description))
  
  # Create hover text for pitcher average points
  pitcher_hover_text <- with(pitcher_avg,
                             paste(if(!is.null(player)) player else "Pitcher", "Average", pitch_name,
                                   "<br>Velocity:", round(avg_speed, 1), "mph",
                                   "<br>Vertical Movement:", round(avg_pfx_z, 1), "in",
                                   "<br>Horizontal Movement:", round(avg_pfx_x, 1), "in",
                                   "<br>Spin Rate:", round(avg_spin, 0), "rpm"))
  
  # Create hover text for league average points
  league_hover_text <- with(league_avg,
                            paste("League Average", pitch_name,
                                  "<br>Velocity:", round(avg_speed, 1), "mph",
                                  "<br>Vertical Movement:", round(avg_pfx_z, 1), "in",
                                  "<br>Horizontal Movement:", round(avg_pfx_x, 1), "in"))
  
  # Create the base plot with individual pitches
  p <- plot_ly()
  
  # First group: All individual pitches
  for (pitch in unique(plot_data$pitch_name)) {
    pitch_data <- plot_data[plot_data$pitch_name == pitch,]
    p <- p %>% add_trace(
      data = pitch_data,
      x = ~pfx_x,
      y = ~pfx_z,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 6, opacity = 0.6, color = pitch_colors[pitch]),
      text = ~pitch_name,
      hovertext = hover_text[plot_data$pitch_name == pitch],
      hoverinfo = 'text',
      showlegend = TRUE,
      name = paste0(pitch),
      legendgroup = "individual",
      legendrank = 1  # This group appears first
    )
  }
  
  # Add a blank trace as a spacer
  p <- p %>% add_trace(
    x = NULL,
    y = NULL,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'rgba(0,0,0,0)'),
    showlegend = TRUE,
    name = "          ",  # Spaces for visual separation
    legendgroup = "spacer1",
    legendrank = 2,
    hoverinfo = 'none'
  )
  
  # Second group: All pitcher averages
  for (pitch in unique(pitcher_avg$pitch_name)) {
    pitch_data <- pitcher_avg[pitcher_avg$pitch_name == pitch,]
    p <- p %>% add_trace(
      data = pitch_data,
      x = ~avg_pfx_x,
      y = ~avg_pfx_z,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 13,
        opacity = 1,
        color = pitch_colors[pitch],
        line = list(color = 'white', width = 1.5),
        symbol = 'diamond'
      ),
      hovertext = pitcher_hover_text[pitcher_avg$pitch_name == pitch],
      hoverinfo = 'text',
      showlegend = TRUE,
      name = paste0("Pitcher Avg - ", pitch),
      legendgroup = "pitcher_avg",
      legendrank = 3  # This group appears second
    )
  }
  
  # Add another spacer
  p <- p %>% add_trace(
    x = NULL,
    y = NULL,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'rgba(0,0,0,0)'),
    showlegend = TRUE,
    name = "          ",  # Spaces for visual separation
    legendgroup = "spacer2",
    legendrank = 4,
    hoverinfo = 'none'
  )
  
  # Third group: All league averages
  for (pitch in unique(league_avg$pitch_name)) {
    pitch_data <- league_avg[league_avg$pitch_name == pitch,]
    p <- p %>% add_trace(
      data = pitch_data,
      x = ~avg_pfx_x,
      y = ~avg_pfx_z,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = 15,
        opacity = 1,
        color = pitch_colors[pitch],
        line = list(color = 'white', width = 1.5),
        symbol = 'star'
      ),
      hovertext = league_hover_text[league_avg$pitch_name == pitch],
      hoverinfo = 'text',
      showlegend = TRUE,
      name = paste0("League Avg - ", pitch),
      legendgroup = "league_avg",
      legendrank = 5  # This group appears last
    )
  }
  
  # Update layout
  p <- p %>%
    layout(
      title = if (!is.null(player)) {
        paste("Pitch Movement -", player)
      } else {
        "Pitch Movement"
      },
      xaxis = list(
        title = "Horizontal Movement (inches)",
        zeroline = TRUE,
        zerolinecolor = 'rgba(0,0,0,0.2)',
        range = c(25, -25)
      ),
      yaxis = list(
        title = "Vertical Movement (inches)",
        zeroline = TRUE,
        zerolinecolor = 'rgba(0,0,0,0.2)',
        range = c(-25, 25)
      ),
      legend = list(
        x = 1.1,
        y = 1,
        yanchor = "top",
        xanchor = "left",
        itemsizing = "constant",
        itemwidth = 30,
        lineheight = 1.2,
        font = list(size = 12),
        tracegroupgap = 20  # Add gap between legend groups
      ),
      shapes = list(
        list(
          type = "line",
          x0 = -20,
          x1 = 20,
          y0 = 0,
          y1 = 0,
          line = list(
            color = "rgba(0,0,0,0.2)",
            dash = "dash"
          )
        ),
        list(
          type = "line",
          x0 = 0,
          x1 = 0,
          y0 = -20,
          y1 = 20,
          line = list(
            color = "rgba(0,0,0,0.2)",
            dash = "dash"
          )
        )
      ),
      margin = list(r = 150)
    )
  
  return(p)
}

############ Define the strike zone plot function
create_strikezone_plotly <- function(data, player = NULL) {
  # Convert to data.frame for easier manipulation with dplyr
  plot_data <- as.data.frame(data)
  
  # Filter data
  plot_data <- data %>%
    {if (!is.null(player)) filter(., player_name == player) else .} %>%
    filter(!is.na(pfx_x), !is.na(pfx_z), !is.na(release_speed)) %>%
    mutate(
      release_speed = round(release_speed, 1),
      pfx_z = round(pfx_z * 12, 1),
      pfx_x = round(pfx_x * 12, 1)
    )
  
  # Create hover text
  hover_text <- with(plot_data,
                     paste("Pitch Type:", pitch_name,
                           "<br>Velocity:", release_speed, "mph",
                           "<br>Vertical Movement:", pfx_z, "in",
                           "<br>Horizontal Movement:", pfx_x, "in",
                           "<br>Spin Rate:", release_spin_rate, "rpm",
                           "<br>Result:", description))
  
  # Strike Zone dimensions (in feet)
  Left <- -0.708333   # -8.5/12
  Right <- 0.708333   # 8.5/12
  Bottom <- 1.5       # Approximately 18 inches
  Top <- 3.5         # Approximately 42 inches
  
  # Create the plot
  p <- plot_ly(plot_data,
               x = ~plate_x,
               y = ~plate_z,
               type = 'scatter',
               mode = 'markers',
               color = ~pitch_name,  
               colors = pitch_colors,
               marker = list(size = 8,
                             opacity = 0.6),
               hovertext = hover_text,
               hoverinfo = 'text') %>%
    layout(
      title = list(
        text = paste("Strike Zone Plot -", player),
        x = 0.5
      ),
      xaxis = list(
        title = "Horizontal Location (ft)",
        zeroline = TRUE,
        zerolinecolor = 'rgba(0,0,0,0.2)',
        range = c(2.5, -2.5),
        showticklabels = FALSE,
        showgrid = FALSE,
        title = TRUE
      ),
      yaxis = list(
        title = "Vertical Location (ft)",
        zeroline = TRUE,
        zerolinecolor = 'rgba(0,0,0,0.2)',
        range = c(0, 5),
        showticklabels = FALSE,
        showgrid = FALSE,
        title = TRUE
      ),
      shapes = list(
        # Strike Zone Box
        list(
          type = "rect",
          x0 = Left,
          x1 = Right,
          y0 = Bottom,
          y1 = Top,
          line = list(color = "black", width = 2),
          fillcolor = "transparent"
        ),
        # Home Plate
        list(
          type = "path",
          path = sprintf('M %f,%f L %f,%f L 0,%f L %f,%f L %f,%f Z',
                         Left, 0,        # Bottom left
                         Left, 0.354,    # Up on left
                         0.708,          # Top point
                         Right, 0.354,   # Down on right
                         Right, 0),      # Bottom right
          line = list(color = "black"),
          fillcolor = "transparent"
        )
      ),
      showlegend = TRUE,
      legend = list(
        title = list(text = "Pitch Type"),
        x = 1.1,
        y = 0.9
      )
    )
  
  return(p)
}








ui <- navbarPage("Reports", theme = shinytheme("flatly"),
                 tabPanel("Advance Reports",
                          tabsetPanel(
                            tabPanel("Pitchers",
                                     tabsetPanel(
                                       tabPanel("Pitch Usage",
                                                sidebarLayout(
                                                  # Sidebar Panel
                                                  sidebarPanel(
                                                    width = 3,
                                                    selectInput("PitcherTeam", label = "Select Team",
                                                                choices = levels(as.factor(pitchers$pitch_team))),
                                                    
                                                    selectInput("Pitcher", label = "Select Pitcher",
                                                                choices = levels(as.factor(pitchers$player_name))),
                                                    
                                                    dateRangeInput("Date", label = "Select Date Range",
                                                                   start = min(pitchers$game_date),
                                                                   end = max(pitchers$game_date),
                                                                   min = min(pitchers$game_date),
                                                                   max = max(pitchers$game_date),
                                                                   format = "yyyy-mm-dd",
                                                                   separator = "to"),
                                                    
                                                    radioButtons("BatterSide", label = "Batter Side",
                                                                 choices = c("Both" = "Both", 
                                                                             "Right Handed" = "R", 
                                                                             "Left Handed" = "L"),
                                                                 selected = "Both"),
                                                    
                                                    checkboxGroupInput("viz_type", label = "Select Visual",
                                                                       choices = c("Pitch Metrics" = "metrics",
                                                                                   "Pitch Usage Chart" = "usage",
                                                                                   "Location Heatmaps" = "heatmaps",
                                                                                   "Pitch Movement Chart" = "movement",
                                                                                   "Pitch Locations" = "locations"
                                                                       ),
                                                                       selected = "metrics"),
                                                    checkboxGroupInput("PitchType", label = "Pitch Type",
                                                                       choices = levels(as.factor(pitchers$pitch_name))),
                                                    
                                                  ),
                                                  
                                                  # Main Panel
                                                  mainPanel(
                                                    conditionalPanel(
                                                      condition = "input.viz_type.includes('metrics')",
                                                      DT::dataTableOutput("pitch_metrics_table"),
                                                    ),
                                                    
                                                    br(),
                                                    br(),
                                                    br(),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.viz_type.includes('usage')",
                                                      plotOutput("usage_chart", height = "600px")
                                                    ),
                                                    br(),
                                                    
                                                    # Error message UI
                                                    htmlOutput("error_message"),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.viz_type.includes('heatmaps')",
                                                      plotOutput("heatmap_plots", height = "700px",
                                                                 width = "90%")
                                                    ),
                                                    
                                                    br(),
                                                    br(),
                                                    br(),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.viz_type.includes('movement')",
                                                      plotlyOutput("movement_plot", height = "600px",
                                                                   width = "90%")
                                                    ),
                                                    
                                                    br(),
                                                    br(),
                                                    br(),
                                                    
                                                    conditionalPanel(
                                                      condition = "input.viz_type.includes('locations')",
                                                      plotlyOutput("location_plot", height = "600px",
                                                                   width = "90%")
                                                    )
                                                  )))))))
)

server = function(input, output, session) {
  
  # Trying to catch any Errors
  safeRender <- function(expr) {
    tryCatch(
      expr,
      error = function(e) NULL,
      warning = function(w) NULL
    )
  }  
  # Reactive filtered dataset (without pitch type filter)
  # Replace your existing filtered_data reactive with this version
  filtered_data <- reactive({
    req(input$PitcherTeam, input$Pitcher, input$Date, input$BatterSide)
    
    filtered <- pitchers %>%
      filter(
        pitch_team == input$PitcherTeam,
        player_name == input$Pitcher,
        game_date >= input$Date[1],
        game_date <= input$Date[2],
        if(input$BatterSide == "Both") {
          TRUE
        } else {
          stand == input$BatterSide
        }
      )
    
    filtered
  })  
  ###### Error Message
  output$error_message <- renderUI({
    # Only check for pitch type selection if certain visualizations are selected
    if (any(c("heatmaps", "movement", "locations") %in% input$viz_type)) {
      tryCatch({
        # Check date range first
        if (input$Date[1] > input$Date[2]) {
          return(HTML(sprintf(
            '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
           <strong>Error:</strong> Start date (%s) must be before or equal to end date (%s)</div>',
            format(input$Date[1], "%B %d, %Y"),
            format(input$Date[2], "%B %d, %Y")
          )))
        }
        
        # Then check filtered data
        fd <- filtered_data()
        if (nrow(fd) == 0) {
          return(HTML(sprintf(
            '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
           No data available for %s between %s and %s. Please try a different date range or pitcher.</div>',
            input$Pitcher,
            format(input$Date[1], "%B %d, %Y"),
            format(input$Date[2], "%B %d, %Y")
          )))
        }
        
        # Finally check pitch type selection
        if (length(input$PitchType) == 0) {
          return(HTML(
            '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
           Please select at least one pitch type to view the selected visualizations</div>'
          ))
        }
        
        # If we get here, everything is okay
        return(NULL)
        
      }, error = function(e) {
        # Catch any other errors
        HTML(sprintf(
          '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
         <strong>Error:</strong> %s</div>',
          conditionMessage(e)
        ))
      })
    } else {
      # For visualizations that don't require pitch type selection, only check date and data
      tryCatch({
        if (input$Date[1] > input$Date[2]) {
          return(HTML(sprintf(
            '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
           <strong>Error:</strong> Start date (%s) must be before or equal to end date (%s)</div>',
            format(input$Date[1], "%B %d, %Y"),
            format(input$Date[2], "%B %d, %Y")
          )))
        }
        
        fd <- filtered_data()
        if (nrow(fd) == 0) {
          return(HTML(sprintf(
            '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
           <strong>Error:</strong> No data available for %s between %s and %s. Please try a different date range or pitcher.</div>',
            input$Pitcher,
            format(input$Date[1], "%B %d, %Y"),
            format(input$Date[2], "%B %d, %Y")
          )))
        }
        
        return(NULL)
      }, error = function(e) {
        HTML(sprintf(
          '<div style="color: red; background-color: #ffe6e6; padding: 10px; margin: 10px 0; border-radius: 5px; border: 1px solid #ff9999;">
         <strong>Error:</strong> %s</div>',
          conditionMessage(e)
        ))
      })
    }
  })  
  
  
  # Additional reactive for heatmaps with pitch type filter
  filtered_data_pitches <- reactive({
    req(filtered_data(), input$PitchType)
    
    validate(
      need(length(input$PitchType) > 0, 
           "Please select at least one pitch type")
    )
    
    filtered <- filtered_data() %>%
      filter(pitch_name %in% input$PitchType)
    
    filtered
  })
  
  # Adding Reactives for UI updates
  observeEvent(input$PitcherTeam, {
    updateSelectInput(session,
                      "Pitcher", "Select Pitcher",
                      choices = levels(factor(filter(pitchers,
                                                     pitch_team == input$PitcherTeam)$player_name)))
  })
  
  observeEvent(input$Pitcher, {
    updateCheckboxGroupInput(session,
                             "PitchType", "Pitch Type",
                             choices = levels(factor(filter(pitchers,
                                                            player_name == input$Pitcher)$pitch_name)))
  })
  
  observeEvent(input$Pitcher, {
    updateDateRangeInput(session,
                         "Date", "Select Date Range",
                         start = min(pitchers$game_date),
                         end = max(pitchers$game_date))
  })
  
  observeEvent(input$Pitcher, {
    updateRadioButtons(session,
                       "BatterSide", "Batter Side",
                       choices = c("All" = "Both",
                                   "Right" = "R",
                                   "Left" = "L"),
                       selected = "Both")
  })
  
  ############### Usage Chart Output
  output$usage_chart <- renderPlot({
    safeRender({
      req(filtered_data())
      
      # Create frequency table
      freq <- filtered_data() %>%
        group_by(balls, strikes, pitch_name) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(balls, strikes) %>%
        mutate(total = sum(count),
               `Usage %` = round((count/total)*100, digits = 1)) %>%
        ungroup()
      
      # Create count label
      freq$count_situation <- paste(freq$balls, "-", freq$strikes)
      
      # Create the chart
      ggplot(freq, aes(x = count_situation, y = pitch_name, fill = `Usage %`)) +
        geom_tile() +
        geom_text(aes(label = `Usage %`), color = "black", cex = 5) +
        scale_fill_gradient(low = "lightblue", high = "#ff9999") +
        theme_minimal() +
        labs(title = paste("Pitch Usage by Count -", input$Pitcher),
             subtitle = paste("vs", input$BatterSide, "handed batters"),
             x = "Count",
             y = "Pitch Type",
             fill = "Usage %") +
        theme(
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 14),
          legend.title = element_text(size = 14),  
          legend.text = element_text(size = 12),   
          legend.key.size = unit(1, "cm")  ,
          panel.grid = element_blank()
        ) +
        scale_x_discrete(position = "top") +
        scale_y_discrete(limits = rev)
    })
  })
  ############# Metrics Table Output
  output$pitch_metrics_table <- DT::renderDataTable({
    safeRender({
      req(filtered_data())
      
      # First get count of pitches
      pitch_counts <- filtered_data() %>%
        group_by(pitch_name) %>%
        summarise(count = n())
      
      # Filter to only include pitches with sufficient data (e.g., more than 5 pitches)
      valid_pitches <- pitch_counts %>%
        filter(count >= 5) %>%
        pull(pitch_name)
      
      
      metrics <- filtered_data() %>%
        filter(pitch_name %in% valid_pitches) %>%
        group_by(pitch_name) %>%
        summarise(
          count = n(),
          `Pitch` = first(pitch_name),  # Create the Pitch column directly
          `Usage Rate` = round(n()/nrow(filtered_data()) * 100, 1),
          `Velocity` = round(mean(release_speed, na.rm = TRUE), 1),
          `Spin Rate` = round(mean(release_spin_rate, na.rm = TRUE), 0),
          `Vertical Break` = round(mean(pfx_z * 12, na.rm = TRUE), 1),
          `Horizontal Break` = round(mean(pfx_x * 12, na.rm = TRUE), 1),
          `Extension` = round(mean(release_extension, na.rm = TRUE), 1),
          `Launch Angle` = ifelse(sum(!is.na(launch_angle)) > 0,
                                  round(mean(launch_angle, na.rm = TRUE), 1),
                                  NA),
          `Exit Velocity` = ifelse(sum(!is.na(launch_speed)) > 0, 
                                   round(mean(launch_speed, na.rm = TRUE), 1), 
                                   NA),
          'OPS' = round({
            hits = sum(events %in% c("single", "double", "triple", "home_run"), na.rm = TRUE)
            atbats = sum(events %in% c("single", "double", "triple", "home_run", "field_out", "strikeout", "strikeout_double_play"), na.rm = TRUE)
            bases = sum(events == "single", na.rm = TRUE) + 
              2*sum(events == "double", na.rm = TRUE) + 
              3*sum(events == "triple", na.rm = TRUE) + 
              4*sum(events == "home_run", na.rm = TRUE)
            walks = sum(events %in% c("walk", "hit_by_pitch"), na.rm = TRUE)
            pa = atbats + walks
            
            ba = if(atbats > 0) hits/atbats else NA
            slg = if(atbats > 0) bases/atbats else NA
            obp = if(pa > 0) (hits + walks)/pa else NA
            
            if(is.na(obp) || is.na(slg)) NA else obp + slg
          }, 3),
          'wOBA' = round({
            wBB = 0.69
            wHBP = 0.722
            w1B = 0.879
            w2B = 1.242
            w3B = 1.569
            wHR = 2.015
            
            numerator = (wBB * sum(events == "walk", na.rm = TRUE) +
                           wHBP * sum(events == "hit_by_pitch", na.rm = TRUE) +
                           w1B * sum(events == "single", na.rm = TRUE) +
                           w2B * sum(events == "double", na.rm = TRUE) +
                           w3B * sum(events == "triple", na.rm = TRUE) +
                           wHR * sum(events == "home_run", na.rm = TRUE))
            
            denominator = sum(events %in% c("single", "double", "triple", "home_run", 
                                            "field_out", "strikeout", "strikeout_double_play",
                                            "walk", "hit_by_pitch"), na.rm = TRUE)
            
            if(denominator > 0) numerator / denominator else NA
          }, 3),
          `Hard Hit%` = ifelse(sum(!is.na(launch_speed)) > 1,
                               round(100 * sum(launch_speed >= 95, na.rm = TRUE) / 
                                       sum(!is.na(launch_speed)), 1),
                               NA),
          `Barrel%` = ifelse(sum(!is.na(launch_speed) & !is.na(launch_angle)) > 1,
                             round(100 * sum(launch_speed >= 95 & 
                                               launch_angle >= 5 & 
                                               launch_angle <= 35, na.rm = TRUE) / 
                                     sum(!is.na(launch_speed) & !is.na(launch_angle)), 1),
                             NA),
          `Whiff%` = ifelse(sum(description %in% c("swinging_strike", "foul", "hit_into_play")) > 1,
                            round(100 * sum(description == "swinging_strike", na.rm = TRUE) / 
                                    sum(description %in% c("swinging_strike", "foul", "hit_into_play"), na.rm = TRUE), 1),
                            NA),
        ) %>%
        arrange(desc(count)) %>%
        select(-count, -pitch_name)  # Remove the original pitch_name and count columns
      
      dt <- DT::datatable(
        metrics,
        options = list(
          scrollX = TRUE,
          pageLength = -1,
          dom = 't'
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          columns = names(metrics),
          backgroundColor = '#f7f7f7',
          fontSize = '12px',
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'Whiff%',
          backgroundColor = DT::styleInterval(
            c(20, 30),
            c('#ff9999', 'white', 'lightblue')
          ),
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'Exit Velocity',
          backgroundColor = DT::styleInterval(
            c(80, 90),
            c('lightblue', 'white', '#ff9999')
          ),
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'Barrel%',
          backgroundColor = DT::styleInterval(
            c(4, 10),
            c('lightblue', 'white', '#ff9999')
          ),
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'Hard Hit%',
          backgroundColor = DT::styleInterval(
            c(25, 40),
            c('lightblue', 'white', '#ff9999')
          ),
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'OPS',
          backgroundColor = DT::styleInterval(
            c(0.600, 0.900),
            c('lightblue', 'white', '#ff9999')
          ),
          textAlign = 'center'
        ) %>%
        DT::formatStyle(
          'wOBA',
          backgroundColor = DT::styleInterval(
            c(0.250, 0.370),
            c('lightblue', 'white', '#ff9999')
          ),
          textAlign = 'center'
        )
      
      dt
    })
  })
  
  ############## Heatmap Output
  output$heatmap_plots <- renderPlot({
    # Initial requirement check
    req(filtered_data_pitches())
    
    # Wrap everything in a try-catch to prevent [object Object] error
    tryCatch({
      # Strike zone parameters
      Left <- -8.5/12
      Right <- 8.5/12
      Bottom <- 18.29/12
      Top <- 44.08/12
      Width <- (Right - Left) / 3
      Height <- (Top - Bottom) / 3
      
      # Color palette
      heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", 
                                                                          n = 9, direction = -1))(16)
      
      # Get data and check which pitches have enough data points
      valid_data <- filtered_data_pitches() %>%
        group_by(pitch_name) %>%
        summarise(
          count = sum(!is.na(plate_x) & !is.na(plate_z)),
          .groups = 'drop'
        ) %>%
        filter(count >= 3)  # Require at least 3 pitches for a heatmap
      
      # If no valid pitches, return empty plot
      if(nrow(valid_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Insufficient data for heatmap display") +
                 theme_void())
      }
      
      # Initialize empty plot list
      plot_list <- list()
      
      # Loop through valid pitches only
      for(valid_pitch in valid_data$pitch_name) {
        # Get data for this pitch
        this_pitch_data <- filtered_data_pitches() %>%
          filter(
            pitch_name == valid_pitch,
            !is.na(plate_x),
            !is.na(plate_z)
          )
        
        # Calculate usage percentage
        total_pitches <- nrow(filtered_data_pitches())
        usage_pct <- if(total_pitches > 0) {
          round((nrow(this_pitch_data) / total_pitches) * 100, 1)
        } else {
          0
        }
        
        # Create basic plot without density
        p <- ggplot(this_pitch_data, mapping = aes(x=plate_x, y=plate_z))
        
        p <- tryCatch({
          p + stat_density2d_filled()
        }, error = function(e) {
          p + geom_point(alpha = 0.5)
        })
        
        # Add the rest of the plot elements
        p <- p +
          scale_fill_manual(values = c(heat_colors_interpolated), 
                            aesthetics = c("fill", "color")) +
          # Strike Zone Box
          geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom) +
          geom_segment(x = Left, y = Top, xend = Right, yend = Top) +
          geom_segment(x = Left, y = Bottom, xend = Left, yend = Top) +
          geom_segment(x = Right, y = Bottom, xend = Right, yend = Top) +
          
          # Inner Lines
          geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height) +
          geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height) +
          geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top) +
          geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top) +
          
          # Home Plate
          geom_segment(x = Left, y = 0, xend = Right, yend = 0) +
          geom_segment(x = Left, y = 0, xend = Left, yend = 4.25/12) +
          geom_segment(x = Left, y = 4.25/12, xend = 0, yend = 8.5/12) +
          geom_segment(x = Right, y = 4.25/12, xend = Right, yend = 0) +
          geom_segment(x = 0, y = 8.5/12, xend = Right, yend = 4.25/12) +
          
          xlim(3,-3) + ylim(0, 5) +
          ggtitle(paste(input$Pitcher, valid_pitch, "Location")) +
          theme(
            legend.position = "none",
            plot.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(color = "black", size = 1.5, fill = NA)
          )
        
        # Add to plot list
        plot_list[[valid_pitch]] <- p
      }
      
      # If we have any plots, arrange them, otherwise show message
      if(length(plot_list) > 0) {
        do.call(ggarrange, c(plot_list, 
                             list(ncol = 2, 
                                  nrow = ceiling(length(plot_list)/2))))
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No valid pitch data available for heatmap display") +
          theme_void()
      }
    }, error = function(e) {
      # If any error occurs, return informative plot
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Unable to generate heatmaps for selected parameters") +
        theme_void()
    })
  })
  
  ########## Movement Plot Output
  output$movement_plot <- renderPlotly({
    safeRender({
      req(filtered_data_pitches(), input$PitchType)
      
      create_movement_plotly(filtered_data_pitches(), pitchers, input$Pitcher, input$PitchType)
    })
  })
  ########## Location Plot Output
  output$location_plot <- renderPlotly({
    safeRender({
      req(filtered_data_pitches())
      
      create_strikezone_plotly(filtered_data_pitches(), input$Pitcher)
    })
  })
  
}

shinyApp(ui = ui, server = server)

#rsconnect::deployApp(appDir = "C:/Users/grego/OneDrive/Documents/Northwestern/2024/Sports Management Analytics- MSDS 457/Final Project/ShinyApp")