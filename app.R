library(shiny)
library(shinyWidgets)
library(tidyverse)
library(scrollytell)
library(extrafont)
library(plotly)

load('dream_team_data.RData')

# UI ----
ui <- fluidPage(
  title = "1992: Basketball's Dream Summer",
  theme = 'style.css', 
  
  # Import Google font
  HTML("<style>
@import url('https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap');
</style>"),
  
  # Code to suppress warning messages while data is loading on-screen 
  # reference (https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs)
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"),
  
  # Parallax introductory image
  # Photo reference: https://www.nba.com/history/top-moments/1992-dream-team-usa-basketball
  fluidRow(
    # Parallax reference: https://www.w3schools.com/howto/howto_css_parallax.asp
      HTML(
        '<style>
        .parallax {
          /* The image used */
          background-image: url("team_photo2.jpg");
        
          /* Set a specific height */
          height: 540px;
          
          /* Create the parallax scrolling effect */
          background-attachment: fixed;
          background-position: center;
          background-repeat: no-repeat;
          background-size: cover;
          filter: grayscale(15%) blur(0px) sharpen(2px);
          }
        </style>
        <!-- Container element -->
        <div class="parallax"></div>'
        )
      ),
  
  # Article title
  fluidRow(HTML("<center>
                <h1>1992: Basketball's Dream Summer</h1>
                <p style='size:18px';> by <a href='https://datascott.com/' target='_blank'>Scott Davis</a></p>
                </center>")
           ),
  br(),
  
  fluidRow(
    column(1),
    column(10,
           # Introduction
           fluidRow(id='text',
                    column(2),
                    column(8, 
                           br(),
                           HTML("<p><span style='font-size:30px'><b>Basketball fans</b></span>. If you could travel to a place and time where local teams (at every level) were at the center of the basketball discussion, where and when would it be?
                           <br><br>
                           For me, that question's easy. I lived it. Growing up in Michigan and reaching peak basketball fandom in the late 1980's and early 1990's, I witnessed championship caliber basketball on multiple levels. The Detroit Pistons' group of <a href='http://www.espn.com/30for30/film/_/page/badboys' target='_blank'>Bad Boys</a> were winning back-to-back titles in the NBA, and the University of Michigan's <a href='http://www.espn.com/watch/film/932e63d5-df33-4303-bc0f-590f6037b3a9/the-fab-five' target='_blank'>Fab Five</a> were all the rage in the NCAA.
           <br><br>
           While these teams were collecting wins and polarizing their respective leagues, another team, soon to be assembled, would unite basketball fans across the country. Prior to the 1992 Summer Olympics, rules dictated that men's basketball teams had to be comprised of amateurs and/or non-NBA professionals. As a result, the United States gathered the country's top collegiate talent to compete on the global stage, often against international teams made up of professionals. During the summer of 1992, however, everything changed. The ban on NBA players was lifted, and Team USA assembled it's first roster composed entirely of NBA players (well, one college player), aptly referred to as The Dream Team.</p>"),
                           br()
                    ),
                    column(2)
           ),
           
           br(), 
           br(),
           
           # Scrolly container
           scrolly_container("roster",
                             scrolly_graph(br(), 
                                           br(),
                                           conditionalPanel(condition = 'input.roster != "intro"',
                                                            div(uiOutput('metric_list'), style = 'font-size: 80%')),
                                           HTML('<center>'),
                                           plotlyOutput('metrics', height = '560px'),
                                           HTML('</center>')
                                           ),
                             
                             scrolly_sections(
                               scrolly_section(id = 'intro',
                                               HTML('<h2><b>The Roster</b></h2>'),
                                               HTML("<p>If ever a team existed that required no introduction, the 1992 Dream Team is it. The roster was stacked with 11-of-12 future hall of famers and served as a who's who of NBA elite. Below is a look at the players and their career stats (the age and NBA team listed are as of the 1992 Olympics).</p>")
                                               ),
                               HTML('<center>'),
                               scrolly_section(id = "Jordan",
                                               tags$img(src = 'jordan.png'),
                                               HTML(team_data$Label[team_data$Name == 'Michael Jordan'])
                                               ),
                               scrolly_section(id = "Magic",
                                               tags$img(src = 'magic.png'),
                                               HTML(team_data$Label[team_data$Name == 'Magic Johnson'])
                                               ),
                               scrolly_section(id = "Bird",
                                               tags$img(src = 'bird.png'),
                                               HTML(team_data$Label[team_data$Name == 'Larry Bird'])
                                               ),
                               scrolly_section(id = "Barkley",
                                               tags$img(src = 'barkley.png'), 
                                               HTML(team_data$Label[team_data$Name == 'Charles Barkley'])
                                               ),
                               scrolly_section(id = "Ewing",
                                               tags$img(src = 'ewing.png'),
                                               HTML(team_data$Label[team_data$Name == 'Patrick Ewing'])
                                               ),
                               scrolly_section(id = "Malone",
                                               tags$img(src = 'malone.png'),
                                               HTML(team_data$Label[team_data$Name == 'Karl Malone'])
                                               ),
                               scrolly_section(id = "Pippen",
                                               tags$img(src = 'pippen.png'),
                                               HTML(team_data$Label[team_data$Name == 'Scottie Pippen'])
                                               ),
                               scrolly_section(id = "Robinson",
                                               tags$img(src = 'robinson.jpg'),
                                               HTML(team_data$Label[team_data$Name == 'David Robinson'])
                                               ),
                               scrolly_section(id = "Mullin",
                                               tags$img(src = 'mullin.png'),
                                               HTML(team_data$Label[team_data$Name == 'Chris Mullin'])
                                               ),
                               scrolly_section(id = "Drexler",
                                               tags$img(src = 'drexler.png'),
                                               HTML(team_data$Label[team_data$Name == 'Clyde Drexler'])
                                               ),
                               scrolly_section(id = "Stockton",
                                               tags$img(src = 'stockton.jpg'),
                                               HTML(team_data$Label[team_data$Name == 'John Stockton'])
                                               ),
                               scrolly_section(id = "Laettner",
                                               tags$img(src = 'laettner.jpg'),
                                               HTML(team_data$Label[team_data$Name == 'Christian Laettner'])
                                               ),
                               HTML('</center>'),
                               scrolly_section(id = "1992",
                                               HTML('<p>During the <b><font color="#002a54">1991-92 season</font></b>, which served as a warm-up to the Olympic games, two players did not play professionally: Christian Laettner, who was at Duke, and Magic Johnson, who suddenly retired. With the exception of an injury-plagued Larry Bird and young David Robinson, the rest of the team was very much in the primes of their careers.</p>'),
                                               br(), 
                                               br(),
                                               br()
                                               ),
                               scrolly_section(id = "roster_blank",
                                               br()
                                               )
                               ) # close scrolly_sections
                             ), # close scrolly_Container
           
           div(fluidRow(id = 'text',
                    column(2),
                    column(8, 
                           br(),
                           HTML("<p><span style='font-size:30px'><b>The tournament</b></span>. As expected, once the men's Olympic basketball competition began, there was only one team that could leave Barcelona with the gold medal.  The United States cruised through all eight of its contests. In fact, during several games, it appeared as though the opposing team was just happy to be on the same floor as the NBA players.<br><br>
                                
                                Team USA averaged 117 points per game and held opponents to a measly 73 points per game over the two-week stretch.  Charles Barkley (18 PPG) and Michael Jordan (14.9 PPG) led the way in scoring.<br><br>
                                Below is a look at player contributions in each of their eight victories.</p>"),
                           br()
                           ),
                    column(2)
                    ), style = 'margin-top: -300px;'),
           
           br(),
           br(),
           br(),
           
           fluidRow(plotlyOutput("boxscore.plot", height = '520px')),
           
           fluidRow(id = 'text',
                        column(2),
                        column(8, 
                               br(),
                               HTML("<p><span style='font-size:30px'><b>The impact</b></span>. The Dream Team's gold medal finish at the 1992 Olympics avenged a third place finish in 1988 and put the USA back on top of the basketball world - by a large margin. NBA popularity skyrocketed around the globe, thanks in part to the larger-than-life personalities of superstars like Michael Jordan, Magic Johnson, and Charles Barkley.<br><br>
                                    
                                    Team USA's decisive victory sent a message to the rest of the world that in order to compete for a championship on the global stage, huge improvements in skill and player development was essential. The world responded.  By 2004, international teams and players had elevated their games so much so that team USA settled for the bronze medal that year.<br><br>
                                    
                                    Since the 1992 games, numerous international players (such as Dirk Nowitzki, Manu Ginobili, Tony Parker, Pau and Marc Gasol) have come to the NBA, won championships, and found great success. While team USA is still a major favorite to stay on top, the Dream Team's influence on the international competition has ensured that this will be no easy task.<br><br>
                                    
                                    The game of basketball is better for it. </p>"),
                               br()
                        ),
                        column(2)
                    ),
           br(),
           br(),
           hr(),
           br(),
           
           fluidRow(
                    column(2),
                    column(8,
                           HTML("<p><i>
                <span style='font-size:18px'><u>References</u></span><br>
                <span style='font-size:14px'>
                Data and photos were collected from several sites in order to create this web application, including
                <a href='https://www.nba.com' target='_blank'>nba.com</a> (career statistics, team photo, and player photos excluding J. Stockton and C. Laettner), 
                <a href='https://www.basketball-reference.com' target='_blank'>basketball-reference.com</a> (player photos of J. Stockton and C. Laettner), and 
                <a href='https://www.archive.fiba.com' target='_blank'>archive.fiba.com</a> (box scores for the 1992 Olympic men's basketball tournament). 
                The R packages powering this site include 
                <a href='https://www.tidyverse.org/' target='_blank'>tidyverse</a>,
                <a href='http://shiny.rstudio.com/' target='_blank'>shiny</a>,
                <a href='https://github.com/dreamRs/shinyWidgets' target='_blank'>shinyWidgets</a>,
                <a href='https://github.com/JohnCoene/scrollytell' target='_blank'>scrollytell</a>,
                <a href='https://plot.ly/r/' target='_blank'>plotly</a>,
                <a href='https://github.com/tidyverse/glue' target='_blank'>glue</a>, and 
                <a href='https://readxl.tidyverse.org/' target='_blank'>readxl</a>.
                </span>
                </i></p>")
                           ),
                    column(2)
                    ),
           br(),
           br()
           
           ),
    column(1)
    
    ) # close fluidRow
  ) # close UI fluidPage

# Server ----
server <- function(input, output, session) {
  
  # Roster ----
  # Reactive Value to track scrolly sections
  roster.rx <- reactiveValues(d = NULL)
  
  # Update the reactive value as users scroll through site
  observeEvent(input$roster, {
    if(input$roster %in% roster.rx$d) {
      roster.rx$d <<- roster.rx$d[1:match(input$roster, roster.rx$d)]
    } else if(input$roster == '1992') {
      roster.rx$d <<- roster.rx$d
    } else {
      roster.rx$d <<- unique(c(roster.rx$d, input$roster)) 
    }
  })
  
  
  # Metric plot----
  # Radio buttons to select metric to plot
  output$metric_list <- renderUI({
      radioGroupButtons(
        inputId = "metric", label = "",
        choices = c('Points', 'Rebounds', 'Assists', 'Steals', 'Blocks', 'FG%', '3P%', 'FT%', 'Minutes'),
        selected = 'Points',
        size = 'sm',
        justified = TRUE,
        status = "default"
      )
  })
  
  # Plot of career stats/metrics
  output$metrics <- renderPlotly ({
    
    df <- stats %>%
      filter(Season < 99) %>% 
      filter(str_detect(Name, paste0(roster.rx$d, collapse = '|'))) %>%
      select(Name, Season, YEAR, TEAM, input$metric) %>%
      mutate(Color = if_else(str_detect(Name, input$roster), 'tomato', 'lightgrey'))
    
    colnames(df)[5] <- 'Value'
    
    line.colors <- df$Color
    names(line.colors) <- as.character(df$Name)
    
    stats.ref <- stats %>% select(Season, input$metric)
    colnames(stats.ref)[2] <- 'Value'
    
    g <- ggplot(data = df, aes(Season, Value, 
                               # Tooltip
                               text = glue::glue('{Name} ({TEAM})
                                                 {YEAR} season
                                                 {Value} {input$metric}'))) +
      
      geom_line(aes(group = Name, color = Name), size = 0.85) +
      scale_color_manual(values = line.colors) +
      theme_minimal() +
      labs(x = 'season', y = '') +
      scale_x_continuous(limits = c(1, 19),
                         breaks = c(1:max(stats$Season[stats$Season < 99]))
                         ) +
      scale_y_continuous(limits = c(0, 
                                              (round(max(stats.ref$Value[stats.ref$Season < 99])) + 2)),
                         breaks = seq(0, 
                                      ifelse(str_detect(input$metric, '%'), 100, round(max(stats.ref$Value[stats.ref$Season < 99])) + 2), 
                                      ifelse(str_detect(input$metric, '%'), 10, 2))
                         ) +
      theme(legend.position = 'none',
            panel.grid = element_line(color = 'grey96'))
    
    # When users scroll to the section "1992", add geom_points for the 1991-92 season
    if(input$roster == '1992') {
      d <- df %>%
        filter(YEAR == '1991-92') 
      
      g <- g +
        geom_point(data = d, aes(Season, Value, group = Name), color = '#002a54', size = 3, shape = 21, stroke = 1.5)
    }
    
    # Title - change based on radiobutton selection
    if(str_detect(input$metric, '%')) {
      Title <- 'career stats (season percentage)'
    } else {
        Title <- 'career stats (per game)'
        }
    
    ggplotly(g, tooltip = 'text') %>%
      layout(title = list(text = glue::glue('<sub><i>{Title}</i></sub>'), x = 0.04),
             font = list(family = 'Roboto Slab')) %>%
      config(displaylogo = F, collaborate = F, cloud = F, displayModeBar = F)
    
  })
  
  # Box Scores ----
  output$boxscore.plot <- renderPlotly({
    
    final.scores <- box_scores %>%
      select(Game, Team, Score) %>%
      mutate(Game = factor(glue::glue('Game {Game}')),
             PlotPTS = 0,
             Label = ifelse(Team == 'USA', 
                            glue::glue('<b>{Game}</b>:  {Team} {Score}'),
                            glue::glue('{Team} {Score}'))) %>%
      distinct() %>%
      mutate(PlotScore = ifelse(Team == 'USA', -175, 110))
    
    
    bs <- box_scores %>%
      mutate(Name = ifelse(Name == 'E. JOHNSON', 'M. JOHNSON', Name)) %>%
      mutate(PlotPTS = ifelse(Team == 'USA', -1 * PTS, PTS)) %>%
      mutate(Game = factor(glue::glue('Game {Game}'))) %>%
      mutate(Tooltip = glue::glue('{Name}, {PTS}')) %>%
      arrange(Game, PlotPTS)
    
    
    usa <- bs %>% filter(Team == 'USA')
    non.usa <- bs %>% filter(Team != 'USA') %>% arrange(Game, desc(PlotPTS))
    
    # for vertical line segments
    segments <- data.frame(x = c(0.75, 8.2, 0.75, 8.2, 0.75, 8.2),
                           y = c(0, 0, 85, 85, -85, -85),
                           Group = c('min', 'min', 'zero', 'zero', 'max', 'max'),
                           stringsAsFactors = F)
    
    # Plot
    g <- ggplot() +
      geom_bar(data = usa, stat = 'identity', 
                           aes(x = reorder(Game, desc(Game)), y = PlotPTS, group = Game, fill = PlotPTS,
                               text = glue::glue('{Name}
                                                 {PTS} pts, {REB} reb, {AST} ast, {STL} stl, {BLK} blk')), 
                           color = 'white', width = 0.5) +
      geom_bar(data = non.usa, stat = 'identity',
                           aes(x = reorder(Game, desc(Game)), y = PlotPTS, group = Game, fill = PlotPTS,
                               text = glue::glue('{Name}
                                                 {PTS} pts, {REB} reb, {AST} ast, {STL} stl, {BLK} blk')),
                           color = 'white', width = 0.5) +
      scale_y_continuous(limits = c(-190, 140)) +
      theme_minimal(base_family = 'Roboto Slab') +
      geom_line(data = segments, aes(x, y, group = Group), color = 'grey65', linetype = 3, size = 0.45) +
      annotate('text', x = 0.35, y = 85, label = '85', color = 'grey65', size = 3.5) +
      annotate('text', x = 0.35, y = -85, label = '85', color = 'grey65', size = 3.5) +
      geom_text(data = final.scores, aes(Game, PlotScore, label = Label), family = 'Roboto Slab', hjust = 0) +
      coord_flip() +
      scale_fill_gradient2(low = 'navyblue', mid = 'grey95', high = 'red4') +
      labs(x = NULL, y = NULL) +
      annotate('text', x = -1, y = 0, label = '') +
      annotate('text', x = 0.35, y = 0, label = 'points', color = 'grey65', size = 3.5) +
      theme(panel.grid = element_blank(),
            axis.text = element_blank(),
            legend.position = 'none',
            plot.margin = margin(0.5,0.5,0.5,0.5,'cm'))
    
    
    ggplotly(g, tooltip = 'text') %>%
      layout(title = list(text = "<b>1992 Olympic Games: Men's Basketball Tournament</b><br><sup>July 26 - August 8 (Barcelona, Spain)<br>&nbsp;</sup>", x = 0.06),
             font = list(family = 'Roboto Slab'),
             margin=list(t=50),
             hoverlabel = list(bgcolor = 'whitesmoke', color = 'DarkGray')
             ) %>%
      config(displaylogo = F, showSendToCloud = F, displayModeBar = F)

  })
  
  # Scrollytell
  output$roster <- renderScrollytell({scrollytell()}) 
  observe({cat("section:", input$scr, "\n")})
  
  # Update restart.txt to clear cache
  session$onSessionEnded(function() {
    restart = data.frame(`modified time` = lubridate::now())
    write.table(restart, file = 'restart.txt', row.names = F)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
