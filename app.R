rm(list = ls())


####Read in Libraries####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(DT)
library(leaflet)
library(sf)
library(tigris)
library(DBI)
library(dbplyr)
library(RPostgres)
library(lubridate)
library(scales)
library(bslib)
library(RColorBrewer)
library(plotly)
library(matrixStats)
library(R.utils)
library(readxl)


####Read in Data####
##Relevant Questions
relevant_questions <- 
  c("There is legislation in Congress called the Safer Supervision Act, which would reform the federal parole and probation system to include the policies we just discussed. Do you support or oppose the Safer Supervision Act?", 
    "If your member of Congress voted for the Safer Supervision Act, would you be more likely or less likely to vote for them for re-election?", 
    "Would you support or oppose President Trump implementing some of these parole and supervised release policies discussed by executive order?", 
    "Having heard more, do you support or oppose the Safer Supervision Act?", 
    "Now, I am going to read you a series of statements about the criminal justice system. For each, please tell me whether you agree or disagree with them. Any criminal justice reform must put public safety/community safety as its top priority", 
    "Now, I am going to read you a series of statements about the criminal justice system. For each, please tell me whether you agree or disagree with them. Getting non-violent offenders trained, out of prison, and back into the workforce [creates safer communities/helps taxpayers and local businesses]", 
    "Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it. Creating incentives for good and productive behavior by former inmates under supervised release that can lead to an early end of their parole", 
    "Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it. Moving away from a one-size-fits-all approach that ensures supervision upon release from prison is imposed only when a judge deems it is warranted by the facts of each case", 
    "Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it. Allowing more judicial discretion when those on parole commit technical violations like missing a phone check-in instead of automatically sending them back to prison", 
    "Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it. Ending the need for supervised release for low-risk former inmates so they can focus on getting good jobs and contributing to society while freeing up resources to focus supervision on high-risk former inmates", 
    "Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation. Our parole and probation resources should return to their original mission, focused on people with high risks. Federal probation officers report significant caseloads that can exceed 100 cases per officer and include many people who pose no safety risk. This creates a burden for the officers and limits their ability to provide appropriate supervision for those who need it", 
    "Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation. Every dollar saved on unnecessary supervision or incarceration is a dollar that can support protecting communities from more serious crime", 
    "Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation. Placing people on parole who pose low risks to public safety is not only a waste of resources, it creates unnecessary roadblocks to success that could otherwise make communities safer and boost the economy. Things like random check-ins during the workday, surprise visits by parole officers at places of work, and travel restrictions make it difficult for those on parole to find and hold good jobs, setting them up for failure", 
    "Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation. In recent years, twice as many people on supervised release had their sentences revoked for simple technical violations than for new arrests for actual crimes committed")

##Poll Data XLSX
poll_data <- 
  "data-raw" %>% 
  here("voter-poll-data-raw.xlsx") %>% 
  read_excel() %>% 
  suppressMessages() %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("dem_cat") %>% 
  mutate(dem_cat = 
           dem_cat %>% 
           str_replace_all("^[`]", ""), 
         dem_cat = 
           ifelse(str_detect(dem_cat, "^[.][.][.][:digit:]") == TRUE, NA, dem_cat), 
         dem_cat = 
           ifelse(V1 == "TOTAL (A)", "TOTAL (A)", dem_cat)) %>% 
  tidyr::fill(dem_cat, .direction = "down") %>% 
  mutate(V1 = 
           ifelse(dem_cat == V1, V1, paste(dem_cat, V1, sep = "_cat_cat_cat_"))) %>% 
  dplyr::select(-dem_cat) %>% 
  t() %>%
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename("x1" = "na")%>% 
  mutate(table_flag = 
           ifelse(str_detect(x1, "^Table [:digit:]") == TRUE, 1, 0) %>% 
           replace_na(0) %>% 
           cumsum()) %>% 
  relocate(table_flag, .before = "x1") %>% 
  mutate(x1 = 
           x1 %>% 
           replace_na("99999999")) %>% 
  filter(str_detect(x1, "^Table [:digit:]") == FALSE) %>% 
  filter(str_detect(x1, "^Question [:digit:]") == FALSE) %>% 
  mutate(x1 = 
           x1 %>% 
           na_if("99999999")) %>% 
  group_by(table_flag) %>%
  mutate(table_flag_row_num = 1:n()) %>% 
  filter(!(is.na(x1) == TRUE & table_flag_row_num == max(table_flag_row_num, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(question_flag = 
           ifelse(is.na(x1) == TRUE, 0, NA) %>% 
           lead(1)) %>% 
  relocate(question_flag, .after = "x1") %>% 
  group_by(table_flag) %>% 
  tidyr::fill(question_flag, .direction = "up") %>% 
  mutate(question_flag = 
           question_flag %>% 
           replace_na(1) %>% 
           cumsum()) %>% 
  ungroup() %>% 
  group_by(across(-c("x1", "table_flag_row_num"))) %>% 
  dplyr::summarize(x1 = 
                     x1 %>% 
                     str_trim(side = "both") %>% 
                     paste(collapse = " "), 
                   
                   table_flag_row_num = 
                     table_flag_row_num %>% 
                     min(na.rm = TRUE)) %>% 
  ungroup() %>% 
  relocate(x1, .after = "table_flag") %>% 
  mutate(x1 = 
           x1 %>% 
           na_if("NA")) %>% 
  filter(!(question_flag == 1 & is.na(x1) == TRUE)) %>% 
  mutate(question = 
           ifelse(question_flag == 0, x1, NA)) %>% 
  group_by(table_flag) %>% 
  tidyr::fill(question, .direction = "down") %>% 
  ungroup() %>% 
  relocate(question, .after = "table_flag") %>% 
  filter(!(question_flag == 0)) %>% 
  mutate(across(c("table_flag", "question", "question_flag", "table_flag_row_num"), 
                ~ifelse(x1 == "BASE=TOTAL SAMPLE", NA, .x))) %>% 
  distinct() %>% 
  filter(str_detect(x1, "^[*][*]D/S") == FALSE) %>% 
  mutate(table_flag = 
           table_flag %>% 
           as.character(), 
         table_flag = 
           ifelse(table_flag == "2" & x1 == "REFUSED", "2,3", table_flag),
         table_flag_adjust = 
           ifelse(table_flag == "2" & str_detect(x1, "^M |^F ") == TRUE, 1, NA)) %>% 
  tidyr::fill(table_flag_adjust, .direction = "down") %>% 
  separate_rows(table_flag, sep = ",") %>% 
  mutate(table_flag = 
           table_flag %>% 
           as.numeric(), 
         table_flag_adjust = 
           table_flag_adjust %>% 
           replace_na(0),
         table_flag = 
           ifelse(table_flag_adjust == 1, table_flag + 1, table_flag), 
         question = 
           question %>% 
           str_replace_all("[.]$", ""),
         x1 = 
           x1 %>% 
           str_replace_all("^`", "")) %>% 
  dplyr::select(-c(table_flag_adjust, table_flag_row_num)) %>% 
  pivot_longer(cols = matches("_[a-z]$"), 
               names_to = "demographic_cat", 
               values_to = "values") %>% 
  mutate(values = 
           values %>% 
           str_replace_all("%$", "") %>% 
           str_trim(side = "both") %>% 
           na_if("-") %>% 
           as.numeric(),
         demographic_cat = 
           demographic_cat %>% 
           str_replace_all("^x([[:digit:]]+)", "\\1")) %>% 
  separate_wider_delim(cols = "demographic_cat",
                       delim = "_cat_cat_cat_",
                       names = c("demographic_category", "demographic_variable"), 
                       too_few = "align_start", 
                       cols_remove = TRUE) %>% 
  dplyr::select(-question_flag) %>% 
  rename("question_name" = "question", 
         "question_id" = "table_flag", 
         "polling_response" = "x1") %>% 
  mutate(total_counts = 
           ifelse(polling_response == "BASE=TOTAL SAMPLE", values, NA)) %>% 
  group_by(demographic_category, demographic_variable) %>% 
  tidyr::fill(total_counts, .direction = "down") %>% 
  ungroup() %>% 
  filter(!(polling_response == "BASE=TOTAL SAMPLE")) %>% 
  mutate(across(c("demographic_variable", "demographic_category"), 
                ~.x %>% 
                  str_replace_all("_[:letter:]$", "")), 
         question_name = 
           question_name %>% 
           str_replace_all(" NA ", " ")) %>% 
  filter(question_name %in% relevant_questions) %>% 
  mutate(response_type = 
           case_when(str_detect(question_name, "For each, please tell me if you support or oppose it") == TRUE ~ "support_oppose",
                     str_detect(question_name, "For each, please tell me whether you agree or disagree with them") == TRUE ~ "agree_disagree",
                     str_detect(question_name, "For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation") == TRUE ~ "convincing_unconvincing",
                     str_detect(question_name, "support or oppose") == TRUE ~ "support_oppose",
                     str_detect(question_name, "more likely or less likely") == TRUE ~ "more_likely_less_likely", 
                     TRUE ~ NA), 
         question_type = 
           case_when(str_detect(question_name, "^Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation.") == TRUE ~ "safer_supporter", 
                     str_detect(question_name, "^Now, I am going to read several types of reforms to the federal parole and supervised inmate release system") == TRUE ~ "supervision_reforms", 
                     str_detect(question_name, "^Now, I am going to read you a series of statements about the criminal justice system") == TRUE ~ "criminal_justice_reforms", 
                     TRUE ~ "general_safer"), 
         question_name = 
           question_name %>% 
           str_replace_all("Now, I am going to read you a series of statements about the criminal justice system. For each, please tell me whether you agree or disagree with them. ", "") %>% 
           str_replace_all("Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it. ", "") %>% 
           str_replace_all("Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation. ", ""), 
         question_name_preamble = 
           case_when(question_type == "safer_supporter" ~ "Each of the following statements comes from supporters of the Safer Supervision Act. For each, please tell me whether the statement is convincing or unconvincing as a reason to support the legislation.", 
                     question_type == "supervision_reforms" ~ "Now, I am going to read several types of reforms to the federal parole and supervised inmate release system. For each, please tell me if you support or oppose it.", 
                     question_type == "criminal_justice_reforms" ~ "Now, I am going to read you a series of statements about the criminal justice system. For each, please tell me whether you agree or disagree with them.", 
                     TRUE ~ "")) %>% 
  mutate(polling_response =
           polling_response %>%
           str_to_title()) %>%
  filter(!(polling_response %in% c("Mov Sup", "Mov Opp"))) %>%
  mutate(polling_response =
           polling_response %>%
           str_replace_all("Dk/Refused", "Don't Know/Refused to Answer")) %>%
  filter(str_detect(polling_response, "^Total") == FALSE) %>% 
  mutate(polling_response_coded = 
           case_when(polling_response %in% c("Strongly Disagree", "Strongly Oppose", "Very Unconvincing", "Much Less Likely") ~ 1, 
                     polling_response %in% c("Somewhat Disagree", "Somewhat Oppose", "Somewhat Unconvincing", "Somewhat Less Likely") ~ 2, 
                     polling_response %in% c("Somewhat Agree", "Somewhat Support", "Somewhat Convincing", "Somewhat More Likely") ~ 4, 
                     polling_response %in% c("Strongly Agree", "Strongly Support", "Very Convincing", "Much More Likely") ~ 5, 
                     polling_response %in% c("Unsure", "Not Sure", "No Impact") ~ 3, 
                     polling_response == "Don't Know/Refused to Answer" ~ NA)) %>% 
  mutate(demographic_category = 
           demographic_category %>% 
           str_replace_all("2024_prez_vote", "2024 Presidential Vote") %>% 
           str_replace_all("total", "Total") %>% 
           str_replace_all("party", "Party Affiliation") %>% 
           str_replace_all("age", "Age") %>% 
           str_replace_all("gender", "Gender") %>% 
           str_replace_all("personal_impacts", "Systems-Impacted Status") %>% 
           str_replace_all("income", "Income") %>% 
           str_replace_all("age_gender", "Age and Gender") %>% 
           str_replace_all("race", "Race/Ethnicity") %>% 
           str_replace_all("area", "Type of Residential Area"), 
         demographic_variable = 
           case_when(demographic_variable == "oth_nv" ~ "Other", 
                     demographic_variable == "gop" ~ "Republican",
                     demographic_variable == "ind" ~ "Independent", 
                     demographic_variable == "dem" ~ "Democrat", 
                     demographic_variable == "any" ~ "Any Impact",
                     demographic_variable == "pers" ~ "Personally Impacted", 
                     demographic_variable == "pers_incar" ~ "Personally Incarcerated", 
                     demographic_variable == "pers_prob" ~ "Personally on Supervision",
                     demographic_variable == "know_incar" ~ "Knows Someone Incarcerated",
                     demographic_variable == "know_prob" ~ "Knows Someone on Supervision", 
                     demographic_variable == "18_34" ~ "18-34", 
                     demographic_variable == "35_49" ~ "35-49", 
                     demographic_variable == "50_64" ~ "50-64", 
                     demographic_variable == "65" ~ "65+", 
                     demographic_variable == "hisp" ~ "Hispanic", 
                     demographic_variable == "sub" ~ "Suburban",
                     demographic_variable == "40k_80k" ~ "40-80k", 
                     demographic_variable == "80_150k" ~ "80-150k", 
                     TRUE ~ str_to_title(demographic_variable)), 
         values = 
           values %>% 
           replace_na(0), 
         values = values * 100, 
         across(c("question_name", "question_name_preamble"), 
                ~.x %>% 
                  str_replace_all("parole and probation", "community supervision") %>% 
                  str_replace_all("parole and supervised release", "community supervision") %>% 
                  str_replace_all("federal parole and supervised inmate release system", "community supervision") %>% 
                  str_replace_all("parole", "community supervision") %>% 
                  str_replace_all("Federal probation officers", "Federal supervision officers"))) %>% 
  filter(demographic_category %in% c("2024 Presidential Vote", 
                                     "Total", 
                                     "Party Affiliation", 
                                     "Age", 
                                     "Gender", 
                                     "Systems-Impacted Status", 
                                     "Income", 
                                     "Age and Gender", 
                                     "Race/Ethnicity", 
                                     "Type of Residential Area")) 


####Set up for App####
##Define Theme
my_theme <- bs_theme(
  version = 4,
  bg = "#FDF6E3",          
  fg = "#2C2C2C",     
  primary = "#009E73", 
  secondary = "#FFBF00",
  warning = "#FFBF00",
  info = "#4A90E2",        
  success = "#2CA02C",     
  danger = "#D55E00",      
  base_font = font_google("Roboto")
)


####App####
##UI
ui <- navbarPage("Safer Supervision Polling Data Dashboard",
                 theme = my_theme,
                 tags$head(
                   tags$style(HTML("
                    /* Fixed navbar */
                    .navbar {
                      position: fixed !important;
                      top: 0 !important;
                      left: 0 !important;
                      right: 0 !important;
                      z-index: 1030 !important;
                      margin-bottom: 0 !important;
                    }
                    
                    /* Add padding to body to account for fixed navbar */
                    body {
                      padding-top: 60px !important;
                    }
                    
                    /* Section headers */
                    .section-preamble {
                      font-size: 20px;
                      font-weight: bold;
                      margin-top: 30px;
                      margin-bottom: 15px;
                      color: #2C2C2C;
                      padding: 10px;
                      background-color: #E8F4F8;
                      border-left: 5px solid #009E73;
                    }
                    
                    .question-header {
                      font-size: 16px;
                      font-weight: 600;
                      margin-top: 20px;
                      margin-bottom: 10px;
                      color: #2C2C2C;
                    }
                    
                    /* Box styling */
                    .box {
                      width: 100% !important;
                      overflow: visible !important;
                      margin-bottom: 20px !important;
                    }
                  "))
                 ),
                 
                 tabPanel("Main Dashboard",
                          tags$head(
                            tags$style(HTML("
                                 #sidebar {
                                   position: fixed;
                                   left: -280px;
                                   top: 60px;
                                   width: 280px;
                                   height: calc(100vh - 60px);
                                   background-color: #FDF6E3;
                                   transition: left 0.3s ease;
                                   z-index: 1000;
                                   overflow-y: auto;
                                   padding: 15px;
                                   border-right: 2px solid #009E73;
                                   box-shadow: 2px 0 5px rgba(0,0,0,0.1);
                                 }
                                 #sidebar.open {
                                   left: 0px;
                                 }
                                 #toggleBtn {
                                   position: fixed;
                                   left: 0px;
                                   top: 80px;
                                   width: 40px;
                                   height: 60px;
                                   background-color: #009E73;
                                   border: none;
                                   border-radius: 0 8px 8px 0;
                                   cursor: pointer;
                                   z-index: 999;
                                   transition: left 0.3s ease;
                                   color: white;
                                   font-size: 20px;
                                   display: flex;
                                   align-items: center;
                                   justify-content: center;
                                 }
                                 #toggleBtn.open {
                                   left: 280px;
                                 }
                                 #main-content {
                                   margin-left: 0px;
                                   padding: 20px;
                                   transition: margin-left 0.3s ease;
                                 }
                               "))
                                              ),
                                              tags$button(
                                                id = "toggleBtn",
                                                onclick = "
                                 var sidebar = document.getElementById('sidebar');
                                 var btn = document.getElementById('toggleBtn');
                                 var mainContent = document.getElementById('main-content');
                                 sidebar.classList.toggle('open');
                                 btn.classList.toggle('open');
                                 btn.innerHTML = sidebar.classList.contains('open') ? '❮' : '❯';
                               ",
                            "❯"
                          ),
                          tags$div(
                            id = "sidebar",
                            h4("Navigation", style = "color: #009E73; font-weight: bold; margin-top: 0;"),
                            hr(),
                            uiOutput("navigation_menu")
                          ),
                          tags$div(
                            id = "main-content",
                            uiOutput("main_dashboard_content")
                          )
                 ),
                 
                 tabPanel("Demographic Visualizations",
                          tags$head(
                            tags$style(HTML("
                                 #sidebar_detail {
                                   position: fixed;
                                   left: -280px;
                                   top: 60px;
                                   width: 280px;
                                   height: calc(100vh - 60px);
                                   background-color: #FDF6E3;
                                   transition: left 0.3s ease;
                                   z-index: 1000;
                                   overflow-y: auto;
                                   padding: 15px;
                                   border-right: 2px solid #009E73;
                                   box-shadow: 2px 0 5px rgba(0,0,0,0.1);
                                 }
                                 #sidebar_detail.open {
                                   left: 0px;
                                 }
                                 #toggleBtnDetail {
                                   position: fixed;
                                   left: 0px;
                                   top: 80px;
                                   width: 40px;
                                   height: 60px;
                                   background-color: #009E73;
                                   border: none;
                                   border-radius: 0 8px 8px 0;
                                   cursor: pointer;
                                   z-index: 999;
                                   transition: left 0.3s ease;
                                   color: white;
                                   font-size: 20px;
                                   display: flex;
                                   align-items: center;
                                   justify-content: center;
                                 }
                                 #toggleBtnDetail.open {
                                   left: 280px;
                                 }
                                 #main-content-detail {
                                   margin-left: 0px;
                                   padding: 20px;
                                   transition: margin-left 0.3s ease;
                                 }
                               "))
                                              ),
                                              tags$button(
                                                id = "toggleBtnDetail",
                                                onclick = "
                                 var sidebar = document.getElementById('sidebar_detail');
                                 var btn = document.getElementById('toggleBtnDetail');
                                 sidebar.classList.toggle('open');
                                 btn.classList.toggle('open');
                                 btn.innerHTML = sidebar.classList.contains('open') ? '❮' : '❯';
                               ",
                            "❯"
                          ),
                          tags$div(
                            id = "sidebar_detail",
                            h4("Navigation", style = "color: #009E73; font-weight: bold; margin-top: 0;"),
                            hr(),
                            uiOutput("navigation_menu_detail")
                          ),
                          tags$div(
                            id = "main-content-detail",
                            fluidRow(
                              column(12,
                                     selectInput("demographic_filter", 
                                                 "Select Demographic Category:",
                                                 choices = c("2024 Presidential Vote", 
                                                             "Party Affiliation",
                                                             "Age",
                                                             "Gender",
                                                             "Systems-Impacted Status",
                                                             "Income",
                                                             "Race/Ethnicity",
                                                             "Type of Residential Area"),
                                                 selected = "Party Affiliation")
                              )
                            ),
                            uiOutput("detailed_dashboard_content")
                          )
                 )
)

##Server
server <- function(input, output, session){
  
  filtered_polling_data <- reactive({
    poll_data %>%
      filter(demographic_category == "Total") %>%
      arrange(question_type, question_id, desc(polling_response_coded))
  })
  
  get_response_color <- function(coded_value) {
    result <- ifelse(is.na(coded_value), "#CCCCCC",
                     ifelse(coded_value == 1, "#D55E00",  # Red
                            ifelse(coded_value == 2, "#FF8C42",  # Orange-red
                                   ifelse(coded_value == 3, "#FFBF00",  # Yellow
                                          ifelse(coded_value == 4, "#90EE90",  # Light green
                                                 ifelse(coded_value == 5, "#009E73",  # Green
                                                        "#CCCCCC"))))))  # Default gray
    return(result)
  }
  
  # output$main_dashboard_content <- renderUI({
  #   df <- filtered_polling_data()
  #   
  #   question_types <- 
  #     df %>%
  #     select(question_type, question_name_preamble, question_name) %>%
  #     distinct() %>%
  #     arrange(question_type, question_name)
  #   
  #   ui_elements <- list()
  #   current_preamble <- NULL
  #   
  #   for(i in 1:nrow(question_types)) {
  #     q_type <- question_types$question_type[i]
  #     q_preamble <- question_types$question_name_preamble[i]
  #     q_name <- question_types$question_name[i]
  #     
  #     if(is.null(current_preamble) || current_preamble != q_preamble) {
  #       ui_elements <- append(ui_elements, list(
  #         div(class = "section-preamble", q_preamble)
  #       ))
  #       current_preamble <- q_preamble
  #     }
  #     
  #     ui_elements <- append(ui_elements, list(
  #       div(class = "question-header", q_name)
  #     ))
  #     
  #     plot_id <- paste0("plot_", gsub("[^A-Za-z0-9]", "_", q_name))
  #     ui_elements <- append(ui_elements, list(
  #       box(
  #         width = 12,
  #         plotlyOutput(plot_id, height = 300)
  #       )
  #     ))
  #   }
  #   
  #   do.call(tagList, ui_elements)
  # })
  
  output$navigation_menu <- renderUI({
    df <- filtered_polling_data()
    
    # Define question type order and labels
    question_type_order <- c("general_safer", "criminal_justice_reforms", 
                             "supervision_reforms", "safer_supporter")
    question_type_labels <- c(
      "general_safer" = "Existing Voter Sentiment on Safer Supervision",
      "criminal_justice_reforms" = "Voter Sentiment on Criminal Justice Reforms",
      "supervision_reforms" = "Voter Sentiment on Supervision Reforms",
      "safer_supporter" = "Gauging Further Voter Support for Safer Supervision"
    )
    
    question_types <- df %>%
      select(question_type, question_name, question_id) %>%
      distinct() %>%
      mutate(question_type = factor(question_type, levels = question_type_order)) %>%
      arrange(question_type, question_id)
    
    nav_elements <- list()
    current_section <- NULL
    
    for(i in 1:nrow(question_types)) {
      q_type <- as.character(question_types$question_type[i])
      q_name <- question_types$question_name[i]
      
      # Add section header
      if(is.null(current_section) || current_section != q_type) {
        nav_elements <- append(nav_elements, list(
          tags$div(
            style = "font-size: 14px; font-weight: bold; margin-top: 15px; 
                   margin-bottom: 8px; color: #009E73;",
            question_type_labels[q_type]
          )
        ))
        current_section <- q_type
      }
      
      # Add question link (without question ID)
      question_anchor <- paste0("question_", gsub("[^A-Za-z0-9]", "_", q_name))
      short_name <- if(nchar(q_name) > 50) paste0(substr(q_name, 1, 47), "...") else q_name
      
      nav_elements <- append(nav_elements, list(
        tags$a(
          href = paste0("#", question_anchor),
          style = "display: block; padding: 5px 10px; margin-bottom: 5px; 
                 color: #4A90E2; text-decoration: none; font-size: 12px;
                 border-left: 2px solid transparent; transition: all 0.2s;",
          onmouseover = "this.style.borderLeft='2px solid #4A90E2'; this.style.backgroundColor='#F5F5F5';",
          onmouseout = "this.style.borderLeft='2px solid transparent'; this.style.backgroundColor='transparent';",
          short_name  # CHANGED: removed the Q# prefix
        )
      ))
    }
    
    do.call(tagList, nav_elements)
  })
  
  output$main_dashboard_content <- renderUI({
    df <- filtered_polling_data()
    
    # Define question type order and labels
    question_type_order <- c("general_safer", "criminal_justice_reforms", 
                             "supervision_reforms", "safer_supporter")
    question_type_labels <- c(
      "general_safer" = "Existing Voter Sentiment on Safer Supervision",
      "criminal_justice_reforms" = "Voter Sentiment on Criminal Justice Reforms",
      "supervision_reforms" = "Voter Sentiment on Supervision Reforms",
      "safer_supporter" = "Gauging Further Voter Support for Safer Supervision"
    )
    
    question_types <- 
      df %>%
      select(question_type, question_name_preamble, question_name, question_id) %>%
      distinct() %>%
      mutate(question_type = factor(question_type, levels = question_type_order)) %>%
      arrange(question_type, question_id)
    
    ui_elements <- list()
    current_question_type <- NULL
    current_preamble <- NULL
    
    for(i in 1:nrow(question_types)) {
      q_type <- as.character(question_types$question_type[i])
      q_preamble <- question_types$question_name_preamble[i]
      q_name <- question_types$question_name[i]
      
      # Add section label for new question type
      if(is.null(current_question_type) || current_question_type != q_type) {
        ui_elements <- append(ui_elements, list(
          div(style = "font-size: 24px; font-weight: bold; margin-top: 40px; margin-bottom: 20px; 
                     color: #009E73; border-bottom: 3px solid #009E73; padding-bottom: 10px;",
              question_type_labels[q_type])
        ))
        current_question_type <- q_type
        current_preamble <- NULL  # Reset preamble when changing sections
      }
      
      # Add preamble if it's new
      if(!is.na(q_preamble) && q_preamble != "" && 
         (is.null(current_preamble) || current_preamble != q_preamble)) {
        ui_elements <- append(ui_elements, list(
          div(style = "font-size: 14px; font-style: italic; margin-top: 20px; 
             margin-bottom: 15px; color: #555; padding: 8px; 
             background-color: #F5F5F5; border-left: 3px solid #4A90E2;",
              q_preamble)
        ))
        current_preamble <- q_preamble
      }
      
      # Add question header with anchor
      question_anchor <- paste0("question_", gsub("[^A-Za-z0-9]", "_", q_name))
      ui_elements <- append(ui_elements, list(
        tags$div(
          id = question_anchor,  # Anchor for navigation
          style = "font-size: 15px; font-weight: 600; margin-top: 15px; 
             margin-bottom: 10px; color: #2C2C2C; scroll-margin-top: 70px;",
          q_name
        )
      ))
      
      # Add plot for this question
      plot_id <- paste0("plot_", gsub("[^A-Za-z0-9]", "_", q_name))
      ui_elements <- append(ui_elements, list(
        box(
          width = 12,
          plotlyOutput(plot_id, height = 300)
        )
      ))
    }
    
    do.call(tagList, ui_elements)
  })
  
  observe({
    df <- filtered_polling_data()
    
    question_types <- df %>%
      select(question_name) %>%
      distinct() %>%
      pull(question_name)
    
    for(q_name in question_types) {
      local({
        question_name_local <- q_name
        plot_id <- paste0("plot_", gsub("[^A-Za-z0-9]", "_", question_name_local))
        
        output[[plot_id]] <- renderPlotly({
          plot_data <- df %>%
            filter(question_name == question_name_local) %>%
            arrange(desc(polling_response_coded)) %>%
            mutate(
              # Normalize values to sum to exactly 100
              values_normalized = values / sum(values, na.rm = TRUE) * 100,
              polling_response = factor(polling_response, 
                                        levels = unique(polling_response)),
              color = sapply(polling_response_coded, get_response_color),
              hover_text = paste0(
                "Response: ", polling_response, "\n",
                "Percentage: ", round(values, 1), "%"  # Show original rounded value in tooltip
              )
            )
          
          p <- 
            plot_data %>% 
            ggplot(aes(x = "Total", 
                       y = values_normalized,
                       fill = polling_response,
                       text = hover_text)) +
            geom_bar(stat = "identity", position = "stack", width = 0.5) +
            scale_fill_manual(values = setNames(plot_data$color, 
                                                plot_data$polling_response),
                              name = "Response") +
            scale_y_continuous(breaks = c(0, 25, 50, 75, 100),  # ADD THIS
                               labels = function(x) paste0(x, "%"),
                               limits = c(0, 100)) +
            labs(x = NULL, y = NULL) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(size = 10),  # CHANGE
              axis.ticks.x = element_line(),          # CHANGE
              legend.position = "right",
              panel.grid.major.x = element_blank()
            ) +
            coord_flip()
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              showlegend = TRUE,
              legend = list(
                orientation = "h",  # CHANGE: horizontal orientation
                x = 0.5,           # CHANGE: center horizontally
                xanchor = "center", # CHANGE: anchor at center
                y = -0.2,          # CHANGE: position below the plot
                yanchor = "top"    # CHANGE: anchor at top of legend
              ),
              margin = list(l = 50, r = 50, t = 10, b = 80)  # CHANGE: increase bottom margin for legend
            )
        })
      })
    }
  })
  
  # Filtered data for detailed analysis
  filtered_polling_data_detail <- reactive({
    req(input$demographic_filter)
    
    poll_data %>%
      filter(demographic_category %in% c("Total", input$demographic_filter)) %>%
      arrange(question_type, question_id, demographic_category, demographic_variable, desc(polling_response_coded))
  })
  
  # Navigation menu for detailed analysis
  output$navigation_menu_detail <- renderUI({
    df <- filtered_polling_data_detail()
    
    # Define question type order and labels
    question_type_order <- c("general_safer", "criminal_justice_reforms", 
                             "supervision_reforms", "safer_supporter")
    question_type_labels <- c(
      "general_safer" = "Existing Voter Sentiment on Safer Supervision",
      "criminal_justice_reforms" = "Voter Sentiment on Criminal Justice Reforms",
      "supervision_reforms" = "Voter Sentiment on Supervision Reforms",
      "safer_supporter" = "Gauging Further Voter Support for Safer Supervision"
    )
    
    question_types <- df %>%
      select(question_type, question_name, question_id) %>%
      distinct() %>%
      mutate(question_type = factor(question_type, levels = question_type_order)) %>%
      arrange(question_type, question_id)
    
    nav_elements <- list()
    current_section <- NULL
    
    for(i in 1:nrow(question_types)) {
      q_type <- as.character(question_types$question_type[i])
      q_name <- question_types$question_name[i]
      
      # Add section header
      if(is.null(current_section) || current_section != q_type) {
        nav_elements <- append(nav_elements, list(
          tags$div(
            style = "font-size: 14px; font-weight: bold; margin-top: 15px; 
                   margin-bottom: 8px; color: #009E73;",
            question_type_labels[q_type]
          )
        ))
        current_section <- q_type
      }
      
      # Add question link
      question_anchor <- paste0("question_detail_", gsub("[^A-Za-z0-9]", "_", q_name))
      short_name <- if(nchar(q_name) > 50) paste0(substr(q_name, 1, 47), "...") else q_name
      
      nav_elements <- append(nav_elements, list(
        tags$a(
          href = paste0("#", question_anchor),
          style = "display: block; padding: 5px 10px; margin-bottom: 5px; 
                 color: #4A90E2; text-decoration: none; font-size: 12px;
                 border-left: 2px solid transparent; transition: all 0.2s;",
          onmouseover = "this.style.borderLeft='2px solid #4A90E2'; this.style.backgroundColor='#F5F5F5';",
          onmouseout = "this.style.borderLeft='2px solid transparent'; this.style.backgroundColor='transparent';",
          short_name
        )
      ))
    }
    
    do.call(tagList, nav_elements)
  })
  
  # Main content for detailed analysis
  output$detailed_dashboard_content <- renderUI({
    df <- filtered_polling_data_detail()
    
    # Define question type order and labels
    question_type_order <- c("general_safer", "criminal_justice_reforms", 
                             "supervision_reforms", "safer_supporter")
    question_type_labels <- c(
      "general_safer" = "Existing Voter Sentiment on Safer Supervision",
      "criminal_justice_reforms" = "Voter Sentiment on Criminal Justice Reforms",
      "supervision_reforms" = "Voter Sentiment on Supervision Reforms",
      "safer_supporter" = "Gauging Further Voter Support for Safer Supervision"
    )
    
    question_types <- 
      df %>%
      select(question_type, question_name_preamble, question_name, question_id) %>%
      distinct() %>%
      mutate(question_type = factor(question_type, levels = question_type_order)) %>%
      arrange(question_type, question_id)
    
    ui_elements <- list()
    current_question_type <- NULL
    current_preamble <- NULL
    
    for(i in 1:nrow(question_types)) {
      q_type <- as.character(question_types$question_type[i])
      q_preamble <- question_types$question_name_preamble[i]
      q_name <- question_types$question_name[i]
      
      # Add section label for new question type
      if(is.null(current_question_type) || current_question_type != q_type) {
        ui_elements <- append(ui_elements, list(
          div(style = "font-size: 24px; font-weight: bold; margin-top: 40px; margin-bottom: 20px; 
                   color: #009E73; border-bottom: 3px solid #009E73; padding-bottom: 10px;",
              question_type_labels[q_type])
        ))
        current_question_type <- q_type
        current_preamble <- NULL
      }
      
      # Add preamble if it's new
      if(!is.na(q_preamble) && q_preamble != "" && 
         (is.null(current_preamble) || current_preamble != q_preamble)) {
        ui_elements <- append(ui_elements, list(
          div(style = "font-size: 14px; font-style: italic; margin-top: 20px; 
               margin-bottom: 15px; color: #555; padding: 8px; 
               background-color: #F5F5F5; border-left: 3px solid #4A90E2;",
              q_preamble)
        ))
        current_preamble <- q_preamble
      }
      
      # Add question header with anchor
      question_anchor <- paste0("question_detail_", gsub("[^A-Za-z0-9]", "_", q_name))
      ui_elements <- append(ui_elements, list(
        tags$div(
          id = question_anchor,
          style = "font-size: 15px; font-weight: 600; margin-top: 15px; 
               margin-bottom: 10px; color: #2C2C2C; scroll-margin-top: 70px;",
          q_name
        )
      ))
      
      # Add plot for this question
      plot_id <- paste0("plot_detail_", gsub("[^A-Za-z0-9]", "_", q_name))
      ui_elements <- append(ui_elements, list(
        box(
          width = 12,
          plotlyOutput(plot_id, height = 400)  # Taller for multiple demographics
        )
      ))
    }
    
    do.call(tagList, ui_elements)
  })
  
  # Generate plots dynamically for detailed analysis
  observe({
    df <- filtered_polling_data_detail()
    
    question_types <- df %>%
      select(question_name) %>%
      distinct() %>%
      pull(question_name)
    
    for(q_name in question_types) {
      local({
        question_name_local <- q_name
        plot_id <- paste0("plot_detail_", gsub("[^A-Za-z0-9]", "_", question_name_local))
        
        output[[plot_id]] <- renderPlotly({
          plot_data <- 
            df %>%
            filter(question_name == question_name_local) %>%
            mutate(
              demo_label = ifelse(demographic_category == "Total", 
                                  "Total", 
                                  paste0(demographic_variable))
            ) %>%
            arrange(demo_label) %>%  
            group_by(demo_label) %>%
            arrange(desc(polling_response_coded), .by_group = TRUE) %>%
            mutate(
              values_normalized = values / sum(values, na.rm = TRUE) * 100,
              polling_response = factor(polling_response, 
                                        levels = unique(polling_response)),
              color = sapply(polling_response_coded, get_response_color),
              hover_text = paste0(
                "Demographic: ", demo_label, "\n",
                "Response: ", polling_response, "\n",
                "Percentage: ", round(values, 1), "%"
              )
            ) %>%
            ungroup() %>%
            mutate(
              # Create factor levels: alphabetical, then Total at the END (so it appears at TOP after flip)
              demo_label = factor(demo_label, 
                                  levels = rev(c("Total", "40k", "40-80k", "80-150k", "150k", sort(unique(demo_label[!(demo_label %in% c("Total", "150k", "80-150k", "40-80k", "40k"))])))))
            )
          
          p <- 
            plot_data %>% 
            ggplot(aes(x = demo_label, 
                       y = values_normalized,
                       fill = polling_response,
                       text = hover_text)) +
            geom_bar(stat = "identity", position = "stack", width = 0.7) +
            scale_fill_manual(values = setNames(unique(plot_data$color), 
                                                unique(plot_data$polling_response)),
                              name = "Response") +
            scale_y_continuous(breaks = c(0, 25, 50, 75, 100),
                               labels = function(x) paste0(x, "%"),
                               limits = c(0, 100)) +
            labs(x = NULL, y = NULL) +
            theme_minimal() +
            theme(
              axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
              axis.ticks.x = element_line(),
              legend.position = "right",
              panel.grid.major.x = element_blank()
            ) +
            coord_flip()
          
          ggplotly(p, tooltip = "text") %>%
            layout(
              showlegend = TRUE,
              legend = list(
                orientation = "h",
                x = 0.5,
                xanchor = "center",
                y = -0.2,
                yanchor = "top"
              ),
              margin = list(l = 100, r = 50, t = 10, b = 80)
            )
        })
      })
    }
  })
}

####Run App####
shinyApp(ui, server)

