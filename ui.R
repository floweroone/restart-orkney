library(shiny)
library(bslib)

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2f7fb7",
    base_font = font_google("Nunito")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #b9dff2 0%, #eaf6fb 45%, #d9eee2 100%);
        color: #1f2d36;
      }

      .main-wrapper {
        max-width: 1150px;
        margin: 25px auto;
      }

      .top-banner {
        background-color: #073763;
        color: white;
        padding: 40px 30px;
        border-radius: 18px 18px 0 0;
        box-shadow: 0 4px 18px rgba(0,0,0,0.18);

        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 30px;
      }

      .banner-text {
        flex: 1;
      }

      .top-logo {
        max-width: 260px;
        width: 28%;
        height: auto;
        background: white;
        padding: 14px 24px;
        border-radius: 16px;
      }

      .top-banner h1 {
        margin: 0;
        font-weight: 800;
        font-size: 54px;
        line-height: 1.1;
      }

      .top-banner p {
        margin: 14px 0 0 0;
        color: #d8ecf8;
        font-size: 22px;
      }

      .content-box {
        background: rgba(255,255,255,0.92);
        padding: 28px;
        border-radius: 0 0 18px 18px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.12);
      }

      .card-box {
        background: white;
        border-radius: 18px;
        padding: 24px;
        margin-bottom: 22px;
        border: 1px solid #d7e8ef;
        box-shadow: 0 3px 12px rgba(0,0,0,0.07);
      }

      .green-card {
        background: #d9eee2;
        border-left: 7px solid #7bb99a;
      }

      .blue-card {
        background: #e6f4fb;
        border-left: 7px solid #5fa8d3;
      }

      .section-title {
        color: #073763;
        font-weight: 750;
        margin-bottom: 12px;
      }

      .hours-list {
        margin-bottom: 0;
        padding-left: 18px;
        font-size: 16px;
      }

      .btn-primary {
        background-color: #2f7fb7;
        border-color: #2f7fb7;
        border-radius: 10px;
        padding: 10px 18px;
        font-weight: 700;
      }

      .btn-primary:hover {
        background-color: #246491;
        border-color: #246491;
      }

      table {
        background: white;
        border-radius: 12px;
        overflow: hidden;
        width: 100%;
      }

      th {
        background-color: #073763 !important;
        color: white !important;
      }

      td, th {
        padding: 12px !important;
      }

      label {
        font-weight: 700;
        color: #1f2d36;
      }

      .small-note {
        color: #4b5b66;
        font-size: 14px;
      }

      iframe {
        border-radius: 14px;
      }

      @media screen and (max-width: 800px) {
        .top-banner {
          flex-direction: column;
          text-align: center;
        }

        .top-logo {
          width: 70%;
          max-width: 300px;
        }

        .top-banner h1 {
          font-size: 38px;
        }

        .top-banner p {
          font-size: 18px;
        }
      }
    "))
  ),
  
  div(
    class = "main-wrapper",
    
    div(
      class = "top-banner",
      
      div(
        class = "banner-text",
        h1("Restart Orkney Delivery Ferry Finder"),
        p("Plan delivery drop-offs and pick-ups from Restart Orkney to Orkney’s outer islands.")
      ),
      
      tags$img(
        src = "employability-orkney.jpg",
        class = "top-logo"
      )
    ),
    
    div(
      class = "content-box",
      
      div(
        class = "card-box green-card",
        h3(class = "section-title", "About the Delivery Service"),
        p(
          "Restart Orkney provides an important service for donating, collecting, and reusing furniture and household goods. ",
          "While customers on mainland Orkney can more easily visit the showroom or arrange local pick-ups, ",
          "customers on the outer islands may need to plan around ferry travel in order to donate items, arrange collections, or receive deliveries."
        ),
        p(
          "This ferry finder is designed to make that process easier by bringing the relevant ferry timetable information into one place. ",
          "Instead of manually searching through multiple ferry schedules, users can choose a date and island to find possible same-day round-trip delivery options from Kirkwall."
        ),
        p(
          "The tool is based on drop-off and pick-up from Restart Orkney in Kirkwall, ",
          "with service to selected islands when ferry times allow a same-day return before closing."
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        div(
          class = "card-box green-card",
          h3(class = "section-title", "Services"),
          p("Restart Orkney can help customers and donors with several collection and delivery services:"),
          tags$ul(
            class = "hours-list",
            tags$li(
              strong("Delivery of purchased items:"),
              " Delivery for furniture or household items bought from the Restart Orkney showroom."
            ),
            tags$li(
              strong("Pick-up of donated items:"),
              " Collection of furniture or household goods you would like to donate."
            ),
            tags$li(
              strong("House clear-outs:"),
              " Support with clearing out larger quantities of reusable household items."
            )
          )
        ),
        
        div(
          class = "card-box blue-card",
          h3(class = "section-title", "Restart Orkney Open Hours"),
          tags$ul(
            class = "hours-list",
            tags$li(strong("Monday–Friday:"), " 09:00–16:00"),
            tags$li(strong("Saturday:"), " 10:00–16:00"),
            tags$li(strong("Sunday:"), " Closed")
          )
        )
      ),
      
      layout_columns(
        col_widths = c(4, 8),
        
        div(
          class = "card-box",
          h3(class = "section-title", "Find Delivery Ferry Options"),
          p("Choose a delivery date and island to see possible same-day round-trip ferry options."),
          
          dateInput(
            inputId = "date",
            label = "Choose ideal delivery date:",
            value = as.Date("2026-07-06"),
            format = "dd MM yyyy",
            weekstart = 1
          ),
          
          selectInput(
            inputId = "destination",
            label = "Choose delivery island:",
            choices = sort(unique(all_timetables$location_clean[
              all_timetables$location_clean != "Kirkwall"
            ]))
          ),
          
          actionButton(
            inputId = "search",
            label = "Find delivery options",
            class = "btn-primary"
          ),
          
          tags$br(),
          tags$br(),
          
          p(
            class = "small-note",
            "Some ferry routes repeat by weekday, while North Ronaldsay uses specific sailing dates."
          )
        ),
        
        div(
          class = "card-box",
          h3(class = "section-title", "Available Round-Trip Delivery Options"),
          tableOutput("results")
        )
      ),
      
      div(
        class = "card-box blue-card",
        h3(class = "section-title", "How This Helps"),
        p(
          "Inter-island delivery can be difficult to coordinate because ferry times, return routes, customer availability, ",
          "and Restart Orkney’s opening hours all need to line up. This tool helps narrow down the possible options automatically."
        ),
        p(
          "Customers can use it to identify dates and ferry times that may work for them before contacting Restart Orkney. ",
          "Staff and volunteers can also use it as a quick planning tool when arranging pick-ups, deliveries, or collections across the islands."
        )
      ),
      
      div(
        class = "card-box",
        h3(class = "section-title", "Live AIS Map Around Orkney"),
        p("Use this map to view live ferry and vessel movement around Orkney."),
        
        tags$iframe(
          src = "https://www.marinetraffic.com/en/ais/embed/zoom:9/centery:58.95/centerx:-2.95/maptype:0/shownames:true",
          width = "100%",
          height = "650",
          style = "border: none;"
        ),
        
        tags$br(),
        tags$br(),
        
        tags$a(
          "Open Orkney Ferries AIS Map",
          href = "https://www.orkneyferries.co.uk/info/ais",
          target = "_blank",
          class = "btn btn-primary"
        )
      )
    )
  )
)