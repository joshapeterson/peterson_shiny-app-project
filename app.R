# Libraries ----------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(validate)
library(hrbrthemes)
library(waiter)
library(DT)

# Loading Data -------------------------------------------------------------------

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

# Global -------------------------------------------------------------------------

# Data Prep ----------------------------------------------------------------------

df1 <- nobel_winners %>% 
        select("category", "organization_country", "prize_share", "prize_year", "birth_date")

df2 <- nobel_winner_all_pubs %>% 
        select("laureate_id", "prize_year", "pub_year", "pub_year")

df1$birth_date = as.Date(df1$birth_date, "%Y-%m-%d")

df1$year_of_birth = as.numeric(format(df1$birth_date, "%Y"))

by_nobel <- df1 %>% 
        group_by(category)

by_nobel_2 <- df2 %>% 
        group_by(laureate_id) %>% 
        summarize(min_prize_year = min(prize_year))

by_nobel_3 <- df2 %>%
        group_by(laureate_id) %>% 
        summarize(min_pub_year = min(pub_year))

by_nobel_4 <- inner_join(by_nobel_2, by_nobel_3, by="laureate_id")

by_nobel_4 <- by_nobel_4 %>% 
        mutate(academic_experience = min_prize_year-min_pub_year) 

by_nobel_5 <- inner_join(nobel_winner_all_pubs,by_nobel_4, by="laureate_id")

grouped_nobel_5 <- by_nobel_5 %>% 
        group_by(laureate_id)

dt <- nobel_winners %>% 
        select(category, birth_country, death_country, organization_country, prize_year) %>% 
        filter(!is.na(organization_country)) %>% 
        group_by(organization_country) %>% 
        summarize(Frequency = n())

# Extract Data for Motivation Analysis -------------------------------------------

motivation_text <- nobel_winners %>%
        select("prize_year", "category", "motivation")

# Extract Data for Paper Title Analysis ------------------------------------------

paper_title_text <- nobel_winner_all_pubs %>%
        select("prize_year", "category", "title", "is_prize_winning_paper")


paper_title_text$title <- gsub("[0-9]", "", paper_title_text$title)

# Selections ---------------------------------------------------------------------

cat_selection <- levels(factor(motivation_text$category))

cat_selection_lower <- list(
        "Chemistry" = "chemistry",
        "Medicine" = "medicine",
        "Physics" = "physics"
)

prize_selection <- list(
        "Yes" = "YES",
        "No" = "NO"
)

country_selection <- levels(factor(df1$organization_country))

# Functions ----------------------------------------------------------------------

getFreq_motivation <- function(data, prize_category, year1, year2) {
        
        tidy_motivation <- data %>%
                unnest_tokens(word, motivation) %>%
                anti_join(stop_words)
        
        if (prize_category != "All") {
                tidy_motivation_n <- tidy_motivation %>%
                        filter(category == prize_category) %>%
                        filter(prize_year >= year1 & prize_year <= year2) %>%
                        count(word) %>%
                        na.omit() %>%
                        arrange(desc(n))
        } else {
                tidy_motivation_n <- tidy_motivation %>%
                        filter(prize_year >= year1 & prize_year <= year2) %>%
                        count(word) %>%
                        na.omit() %>%
                        arrange(desc(n))        
        }
        
        return(tidy_motivation_n)
}

getFreq_paper_titles <- function(data, prize_category, year1, year2, is_prize_winner) {
        
        tidy_title <- data %>%
                unnest_tokens(word, title) %>%
                anti_join(stop_words)
        
        if (prize_category != "All") {
                tidy_title_n <- tidy_title %>%
                        filter(category == prize_category) %>%
                        filter(is_prize_winning_paper == is_prize_winner) %>%
                        filter(prize_year >= year1 & prize_year <= year2) %>%
                        count(word) %>%
                        na.omit() %>%
                        arrange(desc(n))
        } else {
                tidy_title_n <- tidy_title %>%
                        filter(is_prize_winning_paper == is_prize_winner) %>%
                        filter(prize_year >= year1 & prize_year <= year2) %>%
                        count(word) %>%
                        na.omit() %>%
                        arrange(desc(n))        
        }
        
        return(tidy_title_n)
}

# ui -----------------------------------------------------------------------------

ui <- dashboardPage(
        skin = "black",

        # Header -----------------------------------------------------------------
        
        dashboardHeader(title = "Nobel Prize Winners", titleWidth = 275),
        
        # Sidebar ----------------------------------------------------------------
        
        dashboardSidebar(
                width = 275,
                sidebarMenu(
                        menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
                        menuItem("Overview", tabName = "overview", icon = icon("trophy")),
                        menuItem("Motivation and Paper Title Analysis", tabName = "motivationAndPaperTitleAnalysis", icon = icon("file-word")),
                        menuItem("Publication and Age Analysis", tabName = "pathToSuccess", icon = icon("bar-chart")),
                        menuItem("Additional Resources", icon = icon("book"),
                                menuSubItem("Sources", tabName = "sources"),
                                menuSubItem("Supplementary Materials", tabName = "supplementaryMaterials"))
                )
        ),

        # Body -------------------------------------------------------------------
        
        dashboardBody(
                tabItems(
                        # Introduction Body --------------------------------------
                        tabItem("introduction",
                                fluidRow(
                                        box(
                                                title = "Purpose", status = "primary", solidHeader = TRUE, width = 12,
                                                p("The purpose of this application is to provide insights to the Swedish and Norwegian institutions 
                                                responsible for awarding the Nobel Prize. In this application, analysis of past Nobel Prize winners is provided
                                                to help those institutions make informed decisions regarding future recipients of the Nobel Prize."),
                                            )
                                ),
                                fluidRow(
                                        box(
                                                title = "What You'll Find", status = "primary", solidHeader = TRUE, width = 8,
                                                h4(strong("Overview")),
                                                p("The Overview tab provides a general historical overview of Nobel Prize winners since the prize was first
                                                  awarded. You will be able to find the number of Nobel Prizes awarded, the number of award recipients and 
                                                  the number of female award recipients. A list of all Nobel Prize winners is provided as well.
                                                  Furthermore, you can find the top journals and organizations that have produced the most Nobel Prize winners.
                                                  A list of all journals and organizations is provided as well."),
                                                h4(strong("Motivation and Paper Title Analysis")),
                                                p("The Motivation and Paper Title Analysis section provides text analysis of the respective institutions'
                                                  motivation for awarding a Nobel Prize to recipients historically. Additionally, text analysis of the paper titles
                                                  for the categories of Chemisty, Medicine and Physics is provided for reference as well. This will allow the 
                                                  institutions awarding prizes in those categories to understand which types of subjects have typically 
                                                  been awarded a Nobel Prize."),
                                                h4(strong("Publication and Age Analysis")),
                                                p("This section provides an analysis of the number of publications and average academic experience of Nobel Prize
                                                  winners. Additionally, this section provides an analysis of the age of past Nobel Prize winners."),
                                                h4(strong("Additional Resources")),
                                                p("This section provides a list of resources referenced as well as links to supplimentary materials
                                                  that are related to this application."),
                                        ),
                                        box(
                                                title = "Authors", status = "primary", solidHeader = TRUE, width = 4,
                                                p(
                                                        tags$ul(
                                                                tags$li("Dipin Kasana"),
                                                                tags$li("Kelly O'Shields"),
                                                                tags$li("Joshua Peterson")
                                                        )
                                                )
                                        )
                                )
                                
                        ),
                        # Overview Body -----------------------------------------------------------------
                        tabItem("overview",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Winners"),
                                                         column(width = 4,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_winner", "Select Prize Category:", choices = c("All", cat_selection)),
                                                                        sliderInput("year_winner", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE))
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section provides an overview of past Nobel Prize winners. The above filters can be
                                                                          used to filter by prize category and year to update the outputs on the right.")
                                                                )
                                                         ),
                                                         column(width = 8,
                                                                valueBoxOutput("total_awards"),
                                                                valueBoxOutput("total_winners"),
                                                                valueBoxOutput("total_female"),
                                                                box(
                                                                        title = "List of Nobel Prize Winners", status = "primary", solidHeader = TRUE, width = 12,
                                                                        DTOutput("winner_table")
                                                                )
                                                         )
                                                ),
                                                tabPanel(strong("Journals and Organizations"),
                                                         column(width = 4,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_journal_org", "Select Prize Category for Journals:", choices = c("All", cat_selection_lower)),
                                                                        selectInput("category_journal_org2", "Select Prize Category for Organizations:", choices = c("All", cat_selection)),
                                                                        sliderInput("year_journal_org", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE))
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section provides an overview of journals and organizations that have
                                                                        produced Nobel Prize winners in the past. The above filters can be
                                                                          used to filter by prize category and year to update the outputs on the right.")
                                                                )
                                                         ),
                                                         column(width = 8,
                                                                tabBox(
                                                                        title = "", id = "tabset_motivation", width = 12, height = "600px",
                                                                        tabPanel("Top Journals", plotOutput("plot_journals"), height = "600px"),
                                                                        tabPanel("Top Organizations", plotOutput("plot_organizations"), height = "600px")
                                                                ),
                                                                tabBox(
                                                                        title = "", id = "tabset_motivation", width = 12, height = "600px",
                                                                        tabPanel("List of Journals", DTOutput("table_journals"), height = "600px"),
                                                                        tabPanel("List of Organizations", DTOutput("table_organizations"), height = "600px")
                                                                )
                                                         )
                                                )
                                        )
                                )
                        ),
                        # Paper and Motivation Analysis Body --------------------------------------------
                        tabItem("motivationAndPaperTitleAnalysis",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Motivation for Award"),
                                                        column(width = 4,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_motivation", "Select Prize Category:", choices = c("All", cat_selection)), 
                                                                        sliderInput("year_motivation", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                        hr(),
                                                                        sliderInput(inputId = "maxwords_motivation", label = "Max Number of Words for Word Cloud:", min = 5, max = 50, value = 25)
                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section shows which words can be most commonly found within the 
                                                                          stated motivation for an award recipient recieving an award. Additionally, 
                                                                           a count of the most common words can be found on the Word Counts tab as well."),
                                                                )
                                                         ),
                                                         column(width = 8,
                                                                tabBox(
                                                                        title = strong("Motivation Analysis"), id = "tabset_motivation", width = 12, height = "600px",
                                                                        tabPanel("Word Cloud", plotOutput("motivation_wordcloud"), height = "600px"),
                                                                        tabPanel("Word Counts", plotOutput("motivation_freq"), height = "600px")
                                                                )
                                                         )
                                                ),
                                                tabPanel(strong("Paper Titles"),
                                                         column(width = 4,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_paper", "Select Prize Category:", choices = c("All", cat_selection_lower)), 
                                                                        selectInput("category_paper_winner", "Prize Winning Paper:", choices = prize_selection), 
                                                                        sliderInput("year_paper", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                        hr(),
                                                                        sliderInput(inputId = "maxwords_title", label = "Max Number of Words for Word Cloud:", min = 5, max = 50, value = 25)                                                                ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This section shows which words can be most commonly found within the 
                                                                          titles of publications produced by Nobel Prize winners. Additionally, 
                                                                          a count of the most common words can be found on the Word Counts tab as well.")
                                                                )
                                                         ),
                                                         column(width = 8,
                                                                tabBox(
                                                                        title = strong("Paper Title Analysis"), id = "tabset_paper", width = 12, height = "600px",
                                                                        tabPanel("Word Cloud", plotOutput("title_wordcloud"), height = "600px"),
                                                                        tabPanel("Word Counts", plotOutput("title_freq"), height = "600px")
                                                                )
                                                         )
                                                )
                                        )
                                )
                        ),
                        # Analysis of Winners Body -----------------------------------
                        tabItem("pathToSuccess",
                                fluidRow(
                                        tabsetPanel(
                                                tabPanel(strong("Publication Analysis"),
                                                        column(width = 4,
                                                                 box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        sliderInput("prize_year_1", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                 ),
                                                                 box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        h4("Top Plot:"),
                                                                        p("This plot provides the distribution of the number of publications for Nobel Prize winners among three areas of study.
                                                                          You can select the points in this plot to find out the number of publications
                                                                          represented by each point. The output can be found on the Selection tab."),
                                                                        hr(),
                                                                        h4("Bottom Plot:"), 
                                                                        p("This plot provides the average academic experience of the winners prior to receiving the Nobel Prize."),
                                                                        hr(),
                                                                 )
                                                         ),
                                                        column(width = 8,
                                                                 tabBox(
                                                                        title = strong("Publication Analysis"), id = "tabset_violin", width = 12, 
                                                                        tabPanel("Plot", plotOutput("exp1", brush = "violinBrush")),
                                                                        tabPanel("Selection", DTOutput("violinTable"))
                                                                 ),
                                                                 box(
                                                                        title = "Average Academic Experience Prior to Winning the Nobel Prize", status = "primary", solidHeader = TRUE, width = 12, 
                                                                        plotOutput("exp2")
                                                                 )
                                                        )
                                                ),
                                                tabPanel(strong("Age Analysis"),
                                                        column(width = 4,
                                                                box(
                                                                        title = "Filters", status = "primary", solidHeader = TRUE, width = 12,
                                                                        selectInput("category_age", "Select Prize Category:", choices = c("All", cat_selection)), 
                                                                        hr(),
                                                                        sliderInput("prize_year_2", "Select Prize Year(s):", 1901, 2016, c(1990, 2016), sep = "", animate = animationOptions(interval = 500, loop = FALSE)),
                                                                 ),
                                                                box(
                                                                        title = "Description", status = "primary", solidHeader = TRUE, width = 12,
                                                                        p("This plot illustrates the average age of Nobel Prize recipients, according to country, since 1901.
                                                                          The number of individuals included in the average can be found in the table below"),
                                                                 )
                                                        ),
                                                        column(width = 8,
                                                                 box(
                                                                        title = "Average Age of the Nobel Prize Winners by Country", status = "primary", solidHeader = TRUE, width = 12,
                                                                        plotOutput("exp3")
                                                                 ),
                                                                 box(
                                                                        title = "Frequency of Nobel Prize Winners by Country", status = "primary", solidHeader = TRUE, width = 12,
                                                                        DT::dataTableOutput("dt_table")
                                                                 )
                                                        )
                                                )
                                        )
                                )
                        ),
                        # References Body ----------------------------------------
                        tabItem("sources",
                                box(width = 12,
                                        h3(strong(p("Sources"))),
                                        h4("Data Sources:"),
                                        h5(strong("Nobel Winners Dataset")),
                                        p("Kaggle, 2017: ", a(href = "https://www.kaggle.com/nobelfoundation/nobel-laureates#archive.csv", "https://www.kaggle.com/nobelfoundation/nobel-laureates#archive.csv")),
                                        h5(strong("Nobel Winners - All Publications Dataset")),
                                        p("Li, Jichao; Yin, Yian; Fortunato, Santo; Wang Dashun, 2018, A dataset of publication records for Nobel laureates,
                                        https://doi.org/10.7910/DVN/6NJ5RN, Harvard Dataverse, V1, UNF:6:/Mr84aTKPhJytkmsz1tgZQ== [fileUNF]"),
                                        p("Harvard Dataverse, 2018: ", a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6NJ5RN", "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/6NJ5RN")),
                                        h4("Information Source:"),
                                        h5(strong("TidyTuesday")),
                                        p("TidyTuesday, 2019: ", a(href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14", "https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-05-14"))
                                )
                        ),
                        # Supplementary Materials Body ---------------------------
                        tabItem("supplementaryMaterials",
                                box(width = 12,
                                        h3(strong(p("Supplimentary Materials"))),
                                        h4("The Code for this Application is Available at GitHub:"), 
                                        a(href = "https://github.com/joshapeterson/dsba-5122_final-project", "https://github.com/joshapeterson/dsba-5122_final-project"),
                                        h4("Final Project Report: "),
                                        a(href = "https://dsba5122-final-project-group13.netlify.com/", "https://dsba5122-final-project-group13.netlify.com/"),
                                        h4("Final Project Presentation: "), 
                                        a(href = "https://joshapeterson.github.io/dsba-5122_final-presentation/#1", "https://joshapeterson.github.io/dsba-5122_final-presentation/#1")
                                )
                        )
                        
                ),
                use_waiter(),
                show_waiter_on_load(spin_squares())
        )
)

# server -------------------------------------------------------------------------

server <- function(input, output, session) {
        
        # Waiter -----------------------------------------------------------------
        
        Sys.sleep(3)
        hide_waiter()
        
        # Winner Reactive --------------------------------------------------------
        
        output$total_awards <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize) %>%
                                count(prize)      
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize) %>%
                                count(prize)                  
                }

                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Nobel Prize Awards Given",
                        icon = icon("trophy"),
                        color = "blue"
                )
        })
        
        output$total_winners <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                }
                
                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Award Recipients",
                        icon = icon("user"),
                        color = "maroon"
                )
        })
        
        output$total_female <- renderValueBox({
                if (input$category_winner != "All"){
                        sum_of_prize <- nobel_winners %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                filter(gender == "Female") %>%
                                count(prize)
                } else {
                        sum_of_prize <- nobel_winners %>%
                                filter(gender == "Female") %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                count(prize)
                }
                
                
                valueBox(
                        paste0(sum(sum_of_prize$n)),
                        "Female Award Recipients",
                        icon = icon("venus"),
                        color = "olive"
                )
        })
        
        output$winner_table <- renderDT(
                if (input$category_winner != "All"){
                        winner_data <- nobel_winners %>%
                                count(laureate_id)
                        
                        winner_data <- inner_join(nobel_winners, winner_data, by = "laureate_id") %>%
                                filter(category == input$category_winner) %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                arrange(desc(laureate_id)) %>%
                                arrange(desc(n)) %>%
                                select("prize", "motivation", "full_name", "gender", "organization_name", "n")
                        
                        winner_data2 <- winner_data %>%
                                select("full_name", everything())
                        
                        return(winner_data2)
                        
                } else {
                        winner_data <- nobel_winners %>%
                                count(laureate_id)
                        
                        winner_data <- inner_join(nobel_winners, winner_data, by = "laureate_id") %>%
                                filter(prize_year >= input$year_winner[1] & prize_year <= input$year_winner[2]) %>%
                                arrange(desc(laureate_id)) %>%
                                arrange(desc(n)) %>%
                                select("prize", "motivation", "full_name", "gender", "organization_name", "n")
                        
                        winner_data2 <- winner_data %>%
                                select("full_name", everything())
                        
                        return(winner_data2)
                },
                options = list(
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$table_journals <- renderDT(
                if (input$category_journal_org != "All"){
                        journal_data <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(category == input$category_journal_org) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        return(journal_data)
                        
                } else {
                        journal_data <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        return(journal_data)
                },
                options = list(
                        pageLength = 10,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$table_organizations <- renderDT(
                if (input$category_journal_org2 != "All"){
                        organization_data <- nobel_winners %>%
                                filter(!is.na(organization_name)) %>% 
                                filter(category == input$category_journal_org2) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        organization_data <- inner_join(nobel_winners, organization_data, by = "organization_name") %>%
                                select("organization_name", "organization_city", "organization_country", "n") %>%
                                arrange(desc(n))
                        
                        return(organization_data)
                        
                } else {
                        organization_data <- nobel_winners %>%
                                filter(!is.na(organization_name)) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        organization_data <- inner_join(nobel_winners, organization_data, by = "organization_name") %>%
                                select("organization_name", "organization_city", "organization_country", "n") %>%
                                arrange(desc(n))
                        
                        return(organization_data)
                },
                options = list(
                        pageLength = 10,
                        lengthMenu = c(5, 10, 15)
                )
        )
        
        output$plot_journals <- renderPlot({
                if (input$category_journal_org != "All"){
                        text <- nobel_winner_all_pubs %>%
                                filter(category == input$category_journal_org) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(!is.na(journal)) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(journal, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Nobel Prize Winners by Journal",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                } else {
                        text <- nobel_winner_all_pubs %>%
                                filter(is_prize_winning_paper == "YES") %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(journal)) %>%
                                count(journal) %>%
                                arrange(desc(n))
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(journal, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Nobel Prize Winners by Journal",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                }
        })
        
        output$plot_organizations <- renderPlot({
                if (input$category_journal_org2 != "All"){
                        text <- nobel_winners %>%
                                filter(category == input$category_journal_org2) %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(organization_name)) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(organization_name, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Nobel Prize Winners or Orginization",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white") 
                } else {
                        text <- nobel_winners %>%
                                filter(prize_year >= input$year_journal_org[1] & prize_year <= input$year_journal_org[2]) %>%
                                filter(!is.na(organization_name)) %>%
                                count(organization_name) %>%
                                arrange(desc(n))
                        
                        text %>%
                                top_n(5, n) %>%
                                ggplot(aes(reorder(organization_name, n), n)) +
                                geom_col(fill = "#01579b") +
                                coord_flip() +
                                labs(
                                        title = "Number of Nobel Prize Winners or Orginization",
                                        x = "",
                                        y = "Count"
                                ) +
                                theme_minimal() +
                                theme(text = element_text(size = 14)) +
                                geom_text(aes(label = n), hjust = 2, color = "white")
                }
        })

        # Motivation Reactive ----------------------------------------------------
        
        motivation_freq <- reactive(
                withProgress({
                        setProgress(message = "Processing text...")
                        getFreq_motivation(motivation_text, input$category_motivation, input$year_motivation[1], input$year_motivation[2])
                })
        )
        
        output$motivation_wordcloud <- renderPlot({
                validate(
                        need(motivation_freq()$n != "", "No Data Available")
                )
                
                text <- motivation_freq()
                
                text %>%
                        with(
                                wordcloud(
                                        words = word,
                                        freq = n,
                                        random.order = FALSE,
                                        max.words = input$maxwords_motivation,
                                        scale = c(3.5, 0.75),
                                        colors = brewer.pal(8, "Set2")
                                )
                        )
        })
        
        output$motivation_freq <- renderPlot({
                validate(
                        need(motivation_freq()$n != "", "No Data Available")
                )
                
                text <- motivation_freq()
                
                text %>%
                        top_n(5) %>%
                        ggplot(aes(reorder(word, n), n)) +
                        geom_col(fill = "#01579b") +
                        coord_flip() +
                        labs(
                                title = "Top Words in Motivation for the Award",
                                subtitle = "These are the most common words among the described motivation for awarding the Nobel Prize",
                                x = "",
                                y = "Count of Word"
                        ) +
                        theme_minimal() +
                        theme(text = element_text(size = 14)) +
                        geom_text(aes(label = n), hjust = 2, color = "white")
        })
        
        # Paper Title Reactive --------------------------------------------------------
        
        title_freq <- reactive(
                withProgress({
                        setProgress(message = "Processing text...")
                        getFreq_paper_titles(paper_title_text, input$category_paper, input$year_paper[1], input$year_paper[2], input$category_paper_winner)
                })
        )
        
        output$title_wordcloud <- renderPlot({
                validate(
                        need(title_freq()$n != "", "No Data Available")
                )
                
                text <- title_freq()
                
                text %>%
                        with(
                                wordcloud(
                                        words = word,
                                        freq = n,
                                        random.order = FALSE,
                                        max.words = 30,
                                        scale = c(3.5, 0.75),
                                        colors = brewer.pal(8, "Set2")
                                )
                        )
        })
        
        output$title_freq <- renderPlot({
                validate(
                        need(title_freq()$n != "", "No Data Available")
                )
                
                text <- title_freq()
                
                text %>%
                        top_n(5) %>%
                        ggplot(aes(reorder(word, n), n)) +
                        geom_col(fill = "#01579b") +
                        coord_flip() +
                        labs(
                                title = "Top Words among Paper Titles",
                                subtitle = "These are the most common words among titles of published paper",
                                x = "",
                                y = "Count of Word"
                        ) +
                        theme_minimal() +
                        theme(text = element_text(size = 14)) +
                        geom_text(aes(label = n), hjust = 2, color = "white")
        })
        
        # Analysis of Winners Reactive ------------------------------------------------
        
        data1 <- reactive(
                if (input$category_age != "All"){
                        by_nobel %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(!is.na(prize_year)) %>% 
                                filter(!is.na(birth_date)) %>% 
                                filter(category == input$category_age) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                mutate(years_exp = prize_year - year_of_birth) %>% 
                                group_by(organization_country) %>% 
                                summarize(avg_exp = mean(years_exp))     
                } else {
                        by_nobel %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(!is.na(prize_year)) %>% 
                                filter(!is.na(birth_date)) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                mutate(years_exp = prize_year - year_of_birth) %>% 
                                group_by(organization_country) %>% 
                                summarize(avg_exp = mean(years_exp)) 
                }

        )
        
        output$exp3 <- renderPlot({
                validate(
                        need(data1()$avg_exp != "", "No Data Available")
                )
                
                ggplot(data1()) + 
                        geom_col(aes(reorder(organization_country, avg_exp), avg_exp), width = 0.5, fill="#01579b") + 
                        theme_minimal() + 
                        labs(y= "Average Age of the Winner", x = "", title = "") + 
                        coord_flip() +
                        theme(text = element_text(size = 14)) +
                        theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"))
        })
        
        output$dt_table <- DT::renderDataTable(DT::datatable({
                if (input$category_age != "All"){
                        dt <- nobel_winners %>% 
                                select(category, birth_country, death_country, organization_country, prize_year) %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(category == input$category_age) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                count(organization_country) %>%
                                arrange(desc(n))
                } else {
                        dt <- nobel_winners %>% 
                                select(category, birth_country, death_country, organization_country, prize_year) %>% 
                                filter(!is.na(organization_country)) %>% 
                                filter(prize_year >= input$prize_year_2[1] & prize_year <= input$prize_year_2[2]) %>% 
                                count(organization_country) %>%
                                arrange(desc(n))   
                }
        })
        )
        data2 <- reactive(
                grouped_nobel_5 %>% 
                        filter(!is.na(category)) %>% 
                        filter(!is.na(academic_experience)) %>% 
                        filter(is_prize_winning_paper == "YES") %>%
                        filter(prize_year >= input$prize_year_1[1] & prize_year <= input$prize_year_1[2]) %>%
                        group_by(category) %>% 
                        summarize(avg_aca_exp = mean(academic_experience))
        )
        
        output$exp2 <- renderPlot({
                ggplot(data2()) + 
                        geom_col(aes(reorder(category, avg_aca_exp), avg_aca_exp), width = 0.5, fill="#01579b") + 
                        theme_minimal() + 
                        labs(y= "Years of Experience", x = "", title = "") + 
                        coord_flip() + 
                        scale_x_discrete(labels=c("chemistry" = "Chemistry", "medicine" = "Medicine", "physics" = "Physics")) + 
                        scale_y_continuous(limits=c(0, 50)) +
                        theme(text = element_text(size = 14)) + 
                        theme(axis.line = element_line(colour = "black", size = 0.3, linetype = "solid"))
        })
        
        data3 <- reactive(
                by_nobel_5 %>% 
                        select(laureate_id, category, is_prize_winning_paper, prize_year) %>%
                        filter(prize_year >= input$prize_year_1[1] & prize_year <= input$prize_year_1[2]) %>%
                        group_by(laureate_id, category) %>% 
                        summarise(freq = n())
        )
        
        output$exp1<- renderPlot({
                ggplot(data3()) + 
                        geom_violin(aes(category, freq), fill="#01579b", alpha = 0.8) + 
                        geom_jitter(aes(category, freq), width = 0.1, height = 0.1, alpha = 0.4) + 
                        theme_minimal() + 
                        labs(title = "Number of Nobel Prize Winner Publications by Category", x="" , y="Number of Publications") + 
                        scale_x_discrete(labels=c("chemistry" = "Chemistry", "medicine" = "Medicine", "physics" = "Physics")) +
                        theme(axis.line = element_line(colour = "black", size = 0.5, linetype = "solid")) + scale_y_continuous(limits=c(0, 2000)) + 
                        theme(text = element_text(size = 14))
        })
        
        output$violinTable <- renderDT({
                output <- brushedPoints(data3(), input$violinBrush) %>%
                        inner_join(nobel_winner_all_pubs, data3(), by = "laureate_id") %>%
                        select("laureate_name", "affiliation") %>%
                        count(laureate_name)
        })
}

# app build ----------------------------------------------------------------------

shinyApp(ui, server)
