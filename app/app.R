library(shiny)
library(shinydashboard)
library(httr)
library(DT)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(readxl)
library(shinymanager)






credentials <- data.frame(
  user = c("nif"),
  password = c("forum24"),
  admin = TRUE,
  stringsAsFactors = FALSE
)



# Specify the path to your Excel file
excel_file <- "NIF Consolidated Contact Lists March 2024.xlsm"

# Read selected sheets into a list of dataframes
sheet_names <- c("Plenary", "Platform", "Access", "Advocacy", "Comms", "Finance", "HR", "Logistics", 
                 "North West", "North-Central",  "IM_M&E Working Group", "Protection", "PLRCAP")
dfs <- lapply(sheet_names, read_excel, path = excel_file)

# Combine all sheets into one dataframe
combined_df <- bind_rows(dfs)

# Update "MAILING LIST" column
combined_df$`MAILING LIST` <- ifelse(combined_df$`MAILING LIST` %in% c("Plenary/Platform", "Plenary/Platform/NW"), 
                                     "Platform", combined_df$`MAILING LIST`)

# Read additional individual sheets
NE <- read_excel(excel_file, sheet = "North-East")
un <- read_excel(excel_file, sheet = "UN")
donor <- read_excel(excel_file, sheet = "Donors")
df1 <- read_excel(excel_file, sheet = "Organizations")
nngo <- read_excel(excel_file, sheet = "NNGO")
plrcap <- read_excel(excel_file, sheet = "PLRCAP")
PB <- read_excel(excel_file, sheet = "PBCF")




all_choices <- function(x) c("ALL", sort(unique(x)))


ui <- secure_app(
  
  ui =   dashboardPage(
    skin = "black",  
    title = "Nigeria INGO Forum Contact List",
    
    dashboardHeader(
      titleWidth = 800,
      
      
      title = tags$div(
        tags$img(src = "https://ingoforum.ng/assets/images/NIF-Logo-Full.svg", height = 60),
        tags$span(
          "NIF Contact and Mailing Lists Management Tool   ",
          tags$span("(For Internal Use Only)", style = "color: #FF0000;font-size: 13px;"),
          style = "padding: 20px; font-size: 25px; font-weight: bold; color: #00203B;"
        ),
        
        style = "text-align: center; display: inline-block; width: 100%;"
      )
    ),
    dashboardSidebar(
      br(), br(),
      selectInput("organization", "Select Organization", choices = all_choices(combined_df$Organization)),
      br(), br(),
      
      br(), br(),
      
      br(), br(),
      fluidRow(
        column(12, actionButton("go_to_kobo", "Update"))
      ),
      tags$script(HTML("
    $(document).on('click', '#go_to_kobo', function() {
      window.open('https://ee-eu.kobotoolbox.org/x/Xlg0gs4z', '_blank');
    });
  ")),
      collapsed = FALSE,
      width = 300
    ),
    
    
    
    
    
    dashboardBody(
      
      fluidRow(style = "background-color: #00203B;", # Change this to your desired color
               tabBox(
                 width = 1000,
                 id = "tabset1", height = "500px",
                 tags$head(
                   tags$style(HTML("
      .candidate-info {
        border: 1px solid #00203B;
        padding: 10px;
        margin-bottom: 10px;
        border-radius: 5px;
        background-color: white;
        color: black;
        font-size: 14px; 
      }
      .candidate-info strong {
        color: black;
          font-size: 16px; 
      }
    "))
                 ),
                 
                 tabsetPanel(
                   id = "tabs",
                   
                   
                   tabPanel("Access",
                            fluidRow(
                              column(12, br(),DTOutput("PlotA"))
                            )
                   ),
                   tabPanel("Advocacy",
                            fluidRow(
                              column(12,br(), DTOutput("PlotAd"))
                            )
                   ),
                   tabPanel("Comms",
                            fluidRow(
                              column(12, br(),DTOutput("PlotCom"))
                            )
                   ),
                   tabPanel("Finance",
                            fluidRow(
                              column(12, br(),DTOutput("PlotFi"))
                            )
                   ),
                   tabPanel("Human Resource",
                            fluidRow(
                              column(12, br(),DTOutput("PlotHR"))
                            )
                   ),
                   tabPanel("Logistics",
                            fluidRow(
                              column(12, br(),DTOutput("PlotLo"))
                            )
                   ),
                   tabPanel("IM/M&E",
                            fluidRow(
                              column(12, br(),DTOutput("PlotIM"))
                            )
                   ),
                   tabPanel("Platform",
                            fluidRow(
                              column(12, br(),DTOutput("PlotPla"))
                            )
                   ),
                   tabPanel("Plenary",
                            fluidRow(
                              column(12,br(), DTOutput("PlotPle"))
                            )
                   ),
                   tabPanel("Protection",
                            fluidRow(
                              column(12,br(), DTOutput("PlotPro"))
                            )
                   ),
                   tabPanel("North West",
                            fluidRow(
                              column(12,br(), DTOutput("PlotNW"))
                            )
                   ),
                   tabPanel("North Central",
                            fluidRow(
                              column(12,br(), DTOutput("PlotNC"))
                            )
                   ),
                   tabPanel("North East",
                            fluidRow(
                              column(12,br(), DTOutput("PlotNE"))
                            )
                   ),
                   
                   tabPanel("PBCF",
                            fluidRow(
                              column(12,br(), DTOutput("PlotPB"))
                            )
                   ),
                   tabPanel("PLRCAP",
                            fluidRow(
                              column(12,br(), DTOutput("Plot26"))
                            )
                   ),
                   
                   
                   
                   tabPanel("UN", 
                            fluidRow(
                              column(3, 
                                     br(),
                                     selectInput("Acro", "Select Agency", choices = all_choices(un$Acronym)),
                                     br(),
                                     
                                     selectInput("Location1", "Select Location", choices = all_choices(un$Location)),
                                     br(),
                                     
                                     
                                     br(),
                                     br(),
                                     box(
                                       title = "Distribution List",
                                       width = 11,
                                       textOutput("text_output1"),
                                       
                                       br(),
                                       
                                       br()
                                       
                                       
                                     ),
                                     br(),
                                     
                                     
                                     tags$img(src = "https://logos-world.net/wp-content/uploads/2021/11/UN-Logo.png", height = 150),
                              ),
                              column(9,br(), DTOutput("Plot2"))
                            )
                   ),
                   tabPanel("Donors", 
                            fluidRow(
                              column(3, 
                                     br(),
                                     selectInput("donor", "Select Donor", choices = all_choices(donor$Donor)),
                                     
                                     
                                     selectInput("status", "Select Type", choices = all_choices(donor$status)),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     box(
                                       title = "Distribution List",
                                       width = 11,
                                       textOutput("text_output13"),
                                       
                                       br(),
                                       
                                       br()
                                       
                                       
                                       
                                       
                                     ),
                                     br(),
                                     br(),
                                     br(),
                                     #   tags$img(src = "https://www.logolynx.com/images/logolynx/17/17bda0dbff2619f1db407ecd1be92734.jpeg", height = 170),
                                     
                                     
                              ),
                              column(9,br(),  DTOutput("Plot3"))
                            )
                   ),
                   
                 )
                 
               ),
               tags$head(
                 tags$style(
                   HTML("
        .my-fluid-row {
          background-color: #00203B;
        }
      ")
                 )
               ),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br()
               
               
      )
    )
  ),
  
  
  theme = shinythemes::shinytheme("flatly"),
  background = "url('https://img.freepik.com/premium-vector/web-icon-background-computer-vector-icon-background_645658-707.jpg?size=626&ext=jpg');",
  tags_top = tags$div(
    tags$img(
      src = "https://ingoforum.ng/assets/images/NIF-Logo-Full.svg", width = 150, height = 60
    )
  )
)






server <- function(input, output, session) {
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Mailing List", paste0(input$mailing_list), icon = icon("list"),
      color = "green"
    )
  })
  
  
  
  
  output$progressBox1 <- renderInfoBox({
    platform_info <- switch(input$organization,
                            "Action Against Hunger" = "ACF/AAH (Code: A101)",
                            "ActionAid Nigeria" = "AAN (Code: A103)",
                            "Alliance for International Medical Action" = "ALIMA (Code: A104)",
                            "BBC Media Action" = "BBC MA (Code: B105)",
                            "British Red Cross" = "BRC (Code: B106)",
                            "Catholic Agency for Overseas Development" = "CAFOD (Code: C107)",
                            "Catholic Relief Services" = "CRS (Code: C113)",
                            "CBM International" = "CBM (Code: C110)",
                            "Centre for Civilians in Conflict" = "CIVIC (Code: C111)",
                            "Centre for Humanitarian Dialogue" = "HD Centre (Code: H119)",
                            "Christian Aid" = "CAID (Code: C108)",
                            "Clear Global" = "CG (Code: CG168)",
                            "Cooperative for Assistance and Relief Everywhere" = "CARE (Code: C109)",
                            "Cooperazione Internazionale" = "COOPI (Code: C112)",
                            "CUSO International" = "CUSO (Code: CU170)",
                            "Danish Refugee Council" = "DRC (Code: D114)",
                            "eHealth Africa" = "EHA (Code: E116)",
                            "Equal Access International" = "EAI (Code: E115)",
                            "Family Health International 360" = "FHI360 (Code: F117)",
                            "Global Alliance for Improved Nutrition" = "GAIN (Code: GA166)",
                            "Heartland Alliance International" = "HAI (Code: H118)",
                            "Helen Keller International" = "HKI (Code: H120)",
                            "3iSolutions" = "iMMAP Fr ",
                            "Information Management and Mine Action Programs" = "iMMAP Inc (Code: IM125)",
                            "International Alert" = "IA (Code: IA121)",
                            "International Committee of the Red Cross" = "ICRC (Code: IC122)",
                            "International Federation of Red Cross and Red Crescent Societies" = "IFRC (Code: IF123)",
                            "International Medical Corps" = "IMC (Code: IM124)",
                            "International NGO Safety Organization" = "INSO (Code: IM126)",
                            "International Rescue Committee" = "IRC (Code: IR128)",
                            "INTERSOS Humanitarian Aid Organisation" = "INTERSOS (Code: IN126)",
                            "Jesuit Refugee Service" = "JRS (Code: JR129)",
                            "Malteser International" = "MAL (Code: ML131)",
                            "Medecins Du Monde" = "MDM (Code: MD133)",
                            "Medecins Sans Frontieres Belgium" = "MSF OCB (Code: MSF136)",
                            "Medecins Sans Frontieres France" = "MSF OCP (Code: MSF139)",
                            "Medecins Sans Frontieres Holland" = "MSF OCA (Code: MSF135)",
                            "Medecins Sans Frontieres Spain" = "MSF OCBA (Code: MSF137)",
                            "Medecins Sans Frontieres Switzerland" = "MSF OCG (Code: MSF138)",
                            "Medecins Sans Frontieres West and Central Africa" = "MSF WaCA (Code: MSF140)",
                            "Mercy Corps Nigeria" = "MCN (Code: MCN132)",
                            "Mines Advisory Group" = "MAG (Code: MG130)",
                            "MSI Nigeria Reproductive Choices" = "MSION (Code: MSS141)",
                            "Norwegian Church Aid" = "NCA (Code: NC142)",
                            "Norwegian Refugee Council" = "NRC (Code: NR144)",
                            "Nutrition International" = "NI (Code: NR143)",
                            "Oxfam International" = "Oxfam (Code: OF145)",
                            "Pact West Africa" = "Pact (Code: PA146)",
                            "Plan International" = "PLAN (Code: PL147)",
                            "Premiere Urgence Internationale" = "PUI (Code: PI148)",
                            "Project Hope" = "PH (Code: PH176)",
                            "Reconnect Health Development Initiative" = "RHDI (Code: RD149)",
                            "Resolve to Save Lives Nigeria LTD/GTE" = "RTSL (Code: NIFT415)",
                            "Save the Children International" = "SCI (Code: SC151)",
                            "Search for Common Ground" = "SFCG (Code: SG152)",
                            "Secours Islamique France" = "SIF (Code: SI154)",
                            "SNV Netherlands Development Organisation" = "SNV (Code: SN163)",
                            "Solidarites International" = "SI (Code: SI153)",
                            "SOS Children's Villages" = "SOS CV (Code: SO155)",
                            "Street Child" = "SC (Code: SC150)",
                            "Tearfund" = "TF (Code: TR157)",
                            "Terre des Hommes" = "TDH (Code: TH156)",
                            "The MENTOR Initiative" = "MENTOR (Code: MEN134)",
                            "Voluntary Service Overseas" = "VSO (Code: VS162)",
                            "WaterAid Nigeria" = "WAN (Code: WN159)",
                            "Women for Women International" = "WfWI (Code: WW160)",
                            "ZOA International" = "ZOA (Code: ZA161)",
                            "Population Media Center" = "PMC (Code: NIFo281)",
                            "ALL" = "Select Organization"
    )
    
    
    infoBox(
      title = "ACRYM/CODE", 
      value = tags$div(style = "font-size: 14px;", paste0(platform_info)),
      icon = icon("credit-card"),
      color = "green"
    )
  })
  
  output$progressBox3 <- renderInfoBox({
    selected_org <- input$organization
    
    
    
    
    infoBox(
      title = "Organization",
      value = tags$div(style = "font-size: 14px;", paste0(selected_org)),
      icon = icon("location"),
      color = "green"
    )
    
  })
  output$progressBox2 <- renderInfoBox({
    selected_org <- input$organization
    address <- df1[df1$Organization == selected_org, "Address"]
    
    
    
    infoBox(
      title = "Address",
      value = tags$div(style = "font-size: 10px;", paste0(address)),
      icon = icon("map"),
      color = "green"
    )
    
  })
  
  output$logo_output <- renderUI({
    selected_org <- input$organization
    logo_url <- df1[df1$Organization == selected_org, "url"]
    if (length(logo_url) == 0) {
      return(NULL)
    }
    tags$img(src = logo_url, height = 70, width = 120)
  })
  
  output$text_output18 <- renderText({
    
    "nngo@INGOFORUM.NG"
  })
  output$text <- renderText({
    selected_org <- input$organization
    address <- df1[df1$Organization == selected_org, "Address"]
    
    
    paste0(address)
    
  })
  
  output$text_output112 <- renderText({
    paste0(url[input$organization == organization])
  })
  
  
  output$text_output1 <- renderText({
    
    platform_info <- switch(input$Location1,
                            "Abuja" = "un_abuja@INGOFORUM.NG",
                            "Maiduguri" = "un_maiduguri@INGOFORUM.NG",
                            "ALL" = "un@INGOFORUM.NG"
    )
    platform_info
  })
  
  output$text_output13 <- renderText({
    
    platform_info <- switch(input$status,
                            
                            "Embassy" = "embassy@INGOFORUM.NG",
                            "Non Embassy" = "non_embassy@INGOFORUM.NG",
                            "ALL" = "donors@INGOFORUM.NG"
                            
                            
    )
    platform_info
  })
  
  
  output$text_outputAd <- renderText({
    
    #   "Communication" = "comms@INGOFORUM.NG", #: This platform focuses on sharing information, updates, and discussions related to communication strategies and (joint) efforts and activities. It includes discussions on topics such as media relations, public relations, social media, branding, and messaging. ",
    "advocacy@INGOFORUM.NG" #: This platform allows for information sharing and discussions on joint advocacy efforts, providing a space for exchange of ideas, strategies, and best practices to raise awareness, promote humanitarian action, and advocate for the needs and rights of affected populations across Nigeria. ",
    
  })
  
  
  
  output$text_output <- renderText({
    
    "access@INGOFORUM.NG"
  })
  
  
  output$PlotPB <- renderDT({
    
    filtered_df <- PB 
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename( `Phone +234 (0)`=  `Phone number` ) %>%
      select(Organization, Name,  Position,  Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(filtered_df, rownames = FALSE,filter = 'top',   extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                   
                                                                                                                   buttons = list(
                                                                                                                     
                                                                                                                     list(
                                                                                                                       extend = "excel",
                                                                                                                       text = 'Export',
                                                                                                                       exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                     )
                                                                                                                     
                                                                                                                   )
                                                                                                                   
                                                                                                                   
                                                                                                                   
                                                                                                                   
    ))
  })
  
  output$PlotNE <- renderDT({
    
    filtered_df <- NE %>% filter(`MAILING LIST` == "North East" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotA <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` == "Access" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  
  output$PlotCom <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==  "Communication" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotFi <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==   "Finance" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name, Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotHR <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==    "Human Resources" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  output$PlotLo <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==    "Logistics" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  
  output$PlotPla <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==    "Platform") 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  
  
  
  output$PlotPle <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==    "Plenary") 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  output$Plot26 <- renderDT({
    
    filtered_df1 <- plrcap
    
    
    
    
    
    # Select desired columns
    filtered_df1 <- filtered_df1 %>% 
      rename(Organization = organization, Name = name, Location = location, Position = position, Email = email, `Phone +234(0)`=  phone ) %>%
      select(Organization, Name, Position, Location, Email, `Phone +234(0)`) 
    
    datatable(unique(filtered_df1), rownames = FALSE, filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             ) 
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotPro <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==     "Protection" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  
  output$PlotIM <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==      "IM/M&E" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotNC <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==      "North-Central" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$PlotNW <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` ==      "North West" ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top',  extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  output$PlotAd <- renderDT({
    
    filtered_df <- combined_df %>% filter(`MAILING LIST` == "Advocacy"  ) 
    
    
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>%
      rename(Name = `DISPLAY NAME`, Location = CITY, Position = TITLE, Email = `EMAIL ADDRESS`, `Phone +234 (0)`=  `MOBILE PHONE` ) %>%
      select(Organization, Name,  Position, Location, Email,  `Phone +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             )
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$Plot2 <- renderDT({
    
    filtered_df1 <- un 
    
    
    
    # Filter based on selected Agency, if not "ALL"
    if (input$Acro != "ALL") {
      filtered_df1 <- filtered_df1 %>% filter(Acronym == input$Acro)
    }
    # Filter based on selected Location, if not "ALL"
    if (input$Location1 != "ALL") {
      filtered_df1 <- filtered_df1 %>% filter(Location == input$Location1)
    }
    
    # Select desired columns
    filtered_df1 <- filtered_df1 %>% 
      select(Agency, Name, Position, Email) 
    datatable(unique(filtered_df1), rownames = FALSE, filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             ) 
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  
  output$Plot1 <- renderDT({
    filtered_df <- combined_df
    
    # Filter based on selected mailing list, if not "ALL"
    if (input$mailing_list != "ALL") {
      filtered_df <- filtered_df %>% filter(`MAILING LIST` == input$mailing_list)
      
    }
    
    # Filter based on selected organization, if not "ALL"
    if (input$organization != "ALL") {
      filtered_df <- filtered_df %>% filter(Organization == input$organization)
      
    }
    
    # Filter based on selected Location, if not "ALL"
    if (input$Location != "ALL") {
      filtered_df <- filtered_df %>% filter(CITY == input$Location)
    }
    
    
    
    # Select desired columns
    filtered_df <- filtered_df %>% 
      rename(Name = `DISPLAY NAME`, Position = TITLE, Email = `EMAIL ADDRESS`, `PHONE +234 (0)`=  `MOBILE PHONE` ) %>% 
      select(Name,  Position, Email,  `PHONE +234 (0)`)
    
    
    
    
    datatable(unique(filtered_df), rownames = FALSE,  filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 10000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                           
                                                                                                                           buttons = list(
                                                                                                                             
                                                                                                                             list(
                                                                                                                               extend = "excel",
                                                                                                                               text = 'Export',
                                                                                                                               exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                             ) 
                                                                                                                             
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           
    ))
  })
  
  output$Plot4 <- renderDT({
    
    filtered_df12 <- nngo
    
    # Filter based on selected Agency, if not "ALL"
    if (input$N != "ALL") {
      filtered_df12 <- filtered_df12 %>% filter(Organization == input$N)
    }
    
    
    # Select desired columns
    filtered_df12 <- filtered_df12 
    datatable(unique(filtered_df12), rownames = FALSE, filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                            
                                                                                                                            buttons = list(
                                                                                                                              
                                                                                                                              list(
                                                                                                                                extend = "excel",
                                                                                                                                text = 'Export',
                                                                                                                                exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                              )
                                                                                                                              
                                                                                                                            )
                                                                                                                            
                                                                                                                            
                                                                                                                            
                                                                                                                            
    ))
  })
  
  output$Plot3 <- renderDT({
    filtered_df <- donor
    # Filter based on selected Donor, if not "ALL"
    if (input$donor != "ALL") {
      filtered_df <- filtered_df %>% filter(Donor == input$donor)
    }
    
    if (input$status != "ALL") {
      filtered_df <- filtered_df %>% filter(status == input$status)
    }
    
    filtered_df <- filtered_df %>% 
      select(Donor, Name, Position, Email) 
    datatable(unique(filtered_df), rownames = FALSE, filter = 'top', extensions = c("Buttons", "Select"),  options = list(pageLength = 1000, dom = "BRSpfrti",  select = TRUE,
                                                                                                                          
                                                                                                                          buttons = list(
                                                                                                                            
                                                                                                                            list(
                                                                                                                              extend = "excel",
                                                                                                                              text = 'Export',
                                                                                                                              exportOptions = list(modifier = list(selected = TRUE))
                                                                                                                            )
                                                                                                                            
                                                                                                                          )
                                                                                                                          
                                                                                                                          
                                                                                                                          
                                                                                                                          
    ))
  })
  
  # Authenticate
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
}

shinyApp(ui, server)
