library(shiny)
library(shinyjs)
library(shinycssloaders)
library(bslib)
library(terra)
library(yaml)

# Load config file
config <- yaml::read_yaml("config.yml")
m49_orig_data_path <- config$data_location$m49_data_orig
m49_data_path <- config$data_location$m49_data

# ---- DYNAMIC COUNTRY CHOICES ----

# Read the country master list for country names
m49_orig <- terra::vect(m49_orig_data_path)

# List all overlay map files in the maps folder
map_files <- list.files("maps", pattern = "^[A-Z]{3}_overlay\\.png$", full.names = FALSE)

# Extract ISO3 codes from those filenames
iso3_with_maps <- toupper(sub("_overlay\\.png$", "", map_files))

# Get list of unique ISO3 codes from your country master list
master_iso3 <- toupper(unique(m49_orig$ADM0_A3))

# Exclude NA and blank ISO3 codes
master_iso3_valid <- master_iso3[!is.na(master_iso3) & nzchar(master_iso3)]

# Keep only ISO3 codes for which a map exists
iso3_valid <- master_iso3_valid[master_iso3_valid %in% iso3_with_maps]

# For each of these codes, find the country name (from m49 object)
display_names <- sapply(iso3_valid, function(code) {
  idx <- which(toupper(m49_orig$ADM0_A3) == code)[1]
  if (!is.na(idx)) m49_orig$ADMIN[idx] else NA
})

# Sort display names alphabetically
ord <- order(display_names, na.last = TRUE)

# Assemble named vector for Shiny dropdown (names = country name, values = ISO3 code)
iso3_choices_named <- setNames(iso3_valid, display_names)[ord]

# ---- HELPER FUNCTION TO DETECT YEARS ----
get_bii_years <- function(iso3) {
  # List all BII map files for this country that match the pattern bii_YYYY.png
  bii_files <- list.files("maps", pattern = paste0("^", iso3, "_bii_[0-9]{4}\\.png$"), full.names = FALSE)
  
  if (length(bii_files) == 0) return(c(NA, NA))
  
  # Extract years from filenames
  years <- as.numeric(gsub(paste0("^", iso3, "_bii_(\\d{4})\\.png$"), "\\1", bii_files))
  years <- sort(years)
  
  # Return first and last year
  if (length(years) >= 2) {
    return(c(years[1], years[length(years)]))
  } else if (length(years) == 1) {
    return(c(years[1], NA))
  } else {
    return(c(NA, NA))
  }
}

# Make maps folder accessible for modal images
addResourcePath("maps", "maps")

# ---- UI ----
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Nunito"),
    bg = "#f6f8fa",
    fg = "#363640",
    primary = "#4a80d2"
  ),
  tags$head(
    tags$style(HTML(
      ".nav-tabs > li > a, .nav-tabs > li > a:focus, .nav-tabs > li > a:hover {
        font-size: 13px !important;
        padding-left: 10px !important;
        padding-right: 10px !important;
      }
      .small-btn, .small-btn .btn {
        font-size: 16px !important;
        padding: 4px 12px !important;
        height: 36px !important;
        min-width: 80px;
      }
      .shiny-image-output img {
        width: 100% !important;
        height: auto !important;
        max-height: none !important;
        display: block !important;
        vertical-align: top !important;
        cursor: pointer !important;
        transition: opacity 0.2s;
      }
      .shiny-image-output img:hover {
        opacity: 0.9;
      }
      .shiny-image-output {
        margin: 0 !important;
        padding: 0 !important;
        display: block !important;
        line-height: 0 !important;
        max-height: none !important;
        width: 100% !important;
        height: auto !important;
      }
      .spinner-container {
        margin: 0 !important;
        padding: 0 !important;
        line-height: 0 !important;
        display: block !important;
        height: auto !important;
      }
      .shiny-spinner-output-container {
        line-height: 0 !important;
        margin: 0 !important;
        padding: 0 !important;
        display: block !important;
        height: auto !important;
      }
      .info-icon {
        display: inline-block;
        margin-left: 8px;
        cursor: help;
        color: #0d47a1;
        font-weight: bold;
        font-size: 1.1rem;
        position: relative;
        float: right;
      }
      .info-icon .tooltip-text {
        visibility: hidden;
        width: 280px;
        background-color: #2c3e50;
        color: #fff;
        text-align: left;
        border-radius: 6px;
        padding: 10px;
        position: absolute;
        z-index: 10000;
        top: 125%;
        right: 0;
        opacity: 0;
        transition: opacity 0.3s;
        font-size: 0.85rem;
        font-weight: normal;
        line-height: 1.4;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
        pointer-events: none;
        white-space: normal;
      }
      .info-icon .tooltip-text::after {
        content: '';
        position: absolute;
        bottom: 100%;
        right: 10px;
        margin-left: -5px;
        border-width: 5px;
        border-style: solid;
        border-color: transparent transparent #2c3e50 transparent;
      }
      .info-icon:hover .tooltip-text,
      .info-icon:focus .tooltip-text {
        visibility: visible;
        opacity: 1;
      }
      .info-icon:focus-visible {
        outline: 2px solid #4a80d2;
        outline-offset: 2px;
        border-radius: 3px;
      }
      h3 {
        overflow: visible !important;
      }
      "
    ))
  ),
  div(
    style = paste(
      "display: flex; align-items: center;",
      "padding-bottom: 16px;",
      "padding-top: 16px;"
    ),
    tags$img(
      src = "NHM_logo_small_blue.png",
      height = "70px",
      style = "margin-right: 20px; margin-left: 18px;"
    ),
    span(
      style = paste(
        "font-size: 2rem;",
        "font-weight: 650;",
        "color: #4a4e55;",
        "font-family: 'Segoe UI', Arial, sans-serif;",
        "letter-spacing: 0.8px;"
      ),
      "30by30 BII Country Map Explorer"
    )
  ),
  tabsetPanel(
    type = "tabs",
    # ---- Map by Country Tab ----
    tabPanel(
      "Maps by Country",
      br(),
      sidebarLayout(
        # ---- Left sidebar ----
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "country",
            label = "Select Country",
            choices = iso3_choices_named,
            selectize = TRUE
          ),
          actionButton("refresh", "Load Data", class = "small-btn"),
          br(),
          tags$div(
            style = "background: #f3e5f5; border: 1px solid #ce93d8; border-left: 4px solid #ab47bc; padding: 16px; margin-top: 12px; border-radius: 4px;",
            tags$div(
              style = "color: #6a1b9a; font-size: 13px; font-weight: 600; margin-bottom: 10px; letter-spacing: 0.5px;",
              "Limited Release"
            ),
            tags$div(
              style = "color: #4a148c; font-size: 12px; line-height: 1.7;",
              "This application is in limited release with a selection of countries. These are preliminary results and subject to change."
            )
          ),
          tags$div(
            style = "background: #ffffff; border: 1px solid #d1d5db; border-left: 4px solid #6b7280; padding: 16px; margin-top: 8px; border-radius: 4px;",
            tags$div(
              style = "color: #374151; font-size: 13px; font-weight: 600; margin-bottom: 10px; text-transform: uppercase; letter-spacing: 0.5px;",
              "Data Coverage"
            ),
            tags$div(
              style = "color: #4b5563; font-size: 12px; line-height: 1.7;",
              "• Remote or overseas territories may be excluded from analysis.", tags$br(),
              "• Territorial units smaller than 1,000 km² are not included in this dataset."
            )
          ),
          br(),
          tags$div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 5px solid #667eea; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
            tags$div(
              style = "color: #495057; font-size: 12px; line-height: 1.5;",
              tags$strong("Biodiversity Intactness Index (BII) v3.1.0"), tags$br(),
              "Produced by ",
              tags$a(href = "https://biodiversity-futures-lab.github.io/", target = "_blank", "The Biodiversity Futures Lab"),
              " at the Natural History Museum."
            )
          )
        ),
        # ---- Main panel with all maps shown ----
        mainPanel(
          width = 9,
          splitLayout(
            cellWidths = c("50%", "50%"),
            div(
              div(
                style = "position: relative; margin-bottom: 5px;",
                h3(
                  style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
                  span(
                    style = "flex: 1; padding-right: 40px;",
                    "Protected Areas and Critical Ecosystem Areas Overlap"
                  ),
                  span(
                    class = "info-icon",
                    style = "position: absolute; right: 14px;",
                    tabindex = "0",
                    role = "button",
                    `aria-label` = "More information",
                    "ⓘ",
                    span(class = "tooltip-text", "Overlap between protected areas (WDPA) and the top 30% critical ecosystem service areas (CNA).")
                  )
                ),
                withSpinner(imageOutput("overlay_map", click = "overlay_map_click")),
                downloadButton("download_overlay_map", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              ),
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_first_header"),
                withSpinner(imageOutput("bii_first", click = "bii_first_click")),
                downloadButton("download_bii_first", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              ),
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_change_WDPA_header"),
                withSpinner(imageOutput("bii_change_WDPA", click = "bii_change_WDPA_click")),
                downloadButton("download_bii_change_WDPA", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              ), 
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_change_WDPA_CNA_header"),
                withSpinner(imageOutput("bii_change_WDPA_CNA", click = "bii_change_WDPA_CNA_click")),
                downloadButton("download_bii_change_WDPA_CNA", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              )
            ),
            div(
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_change_header"),
                withSpinner(imageOutput("bii_change", click = "bii_change_click")),
                downloadButton("download_bii_change", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              ),
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_last_header"),
                withSpinner(imageOutput("bii_last", click = "bii_last_click")),
                downloadButton("download_bii_last", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              ),
              div(
                style = "position: relative; margin-bottom: 5px;",
                uiOutput("bii_change_CNA_header"),
                withSpinner(imageOutput("bii_change_CNA", click = "bii_change_CNA_click")),
                downloadButton("download_bii_change_CNA", "",
                               style = "position: absolute; left: 10px; bottom: 10px; z-index: 10;")
              )
            )
          )
        )
      )
    ),
    # ---- Info Tab ----
    tabPanel(
      "About",
      br(),
      fluidRow(
        column(
          width = 10,
          offset = 1,
          # About 30by30 - Hero Section
          tags$div(
            style = "background: #4a80d2; color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
            h2("🌍 About 30by30", style = "font-weight: bold; color: white; margin-top: 0;"),
            p(style = "font-size: 16px; line-height: 1.6;", 
              "30by30 is a global commitment established in the Kunming–Montreal Global Biodiversity Framework to protect 30% of the world's land and waters by 2030. The initiative aims to expand and strengthen protected and conserved areas to safeguard biodiversity and support human wellbeing.")
          ),
          
          # About This App
          tags$div(
            style = "background: #f8f9fa; padding: 25px; border-radius: 10px; margin-bottom: 25px; border-left: 5px solid #4a80d2;",
            h3("📊 About This App", style = "font-weight: bold; color: #2c3e50; margin-top: 0;"),
            p(style = "color: #495057;", "This app provides country-level maps showing:"),
            tags$ul(
              style = "color: #495057; line-height: 1.8;",
              tags$li(tags$strong("Biodiversity Intactness Index (BII)"), " for different time periods"),
              tags$li(tags$strong("Changes in BII over time"),
                      tags$ul(
                        style = "margin-top: 8px;",
                        tags$li("Within existing protected areas (WDPA)"),
                        tags$li("Within Critical Natural Assets (CNA)")
                      )
              )
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-top: 15px; border-left: 3px solid #28a745;",
              p(style = "margin: 0; color: #495057;", 
                "By combining biodiversity trends with conservation designations, the maps help users explore how well current protected areas and critical natural assets align.")
            ),
            tags$div(
              style = "margin-top: 20px; text-align: center;",
              tags$a(
                href = "#", 
                onclick = "$('a[data-value=\"Maps by Country\"]').tab('show'); return false;",
                style = "background: #4a80d2; color: white; padding: 12px 30px; border-radius: 25px; text-decoration: none; display: inline-block; font-weight: 600; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
                "🗺️ Explore Maps by Country"
              )
            )
          ),
          
          # NHM's Analysis
          tags$div(
            style = "background: #fff8e1; padding: 25px; border-radius: 10px; margin-bottom: 25px; border-left: 5px solid #ffa726;",
            h3("🔬 NHM's 30by30 Analysis", style = "font-weight: bold; color: #e65100; margin-top: 0;"),
            p(style = "color: #5d4037;", 
              "As part of the Natural History Museum's contribution to global 30by30 efforts, the Museum has developed a national-level analysis to help identify where conservation could have the greatest impact."),
            p(style = "color: #5d4037;", 
              "Using data from Chaplin-Kramer et al. (2023) on ecosystem service provision, NHM identified the top 30% of land within each country that delivers the most important Nature's Contributions to People (NCP). These areas are termed ", 
              tags$strong("Critical Natural Assets (CNA)"), "."),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin: 15px 0;",
              p(style = "margin: 0 0 10px 0; color: #5d4037; font-weight: 600;", 
                "By comparing CNA with the World Database on Protected Areas (WDPA), the analysis highlights:"),
              tags$div(
                style = "color: #5d4037; line-height: 1.8; margin: 0;",
                tags$div(style = "margin-bottom: 10px;", "✅ Where protected areas already overlap areas providing essential ecosystem services"),
                tags$div("🎯 Where opportunities remain to strengthen or expand protection toward 30by30 goals")
              )
            ),
            p(style = "color: #5d4037;", 
              "This app makes those comparisons visible alongside observed biodiversity trends."),
            p(style = "margin-top: 15px;", 
              tags$a(href = "https://www.nhm.ac.uk/our-science/services/data/biodiversity-intactness-index/policy/30by30.html", 
                     target = "_blank",
                     style = "color: #e65100; font-weight: 600; text-decoration: underline;",
                     "Learn more about NHM's 30by30 work →"))
          ),
          
          # Data Sources
          tags$div(
            style = "background: #e3f2fd; padding: 25px; border-radius: 10px; margin-bottom: 25px; border-left: 5px solid #1976d2;",
            h3("📚 Data Sources", style = "font-weight: bold; color: #0d47a1; margin-top: 0;"),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(style = "color: #1976d2;", "Biodiversity Intactness Index (BII) v3.1.0"),
              br(),
              tags$span(style = "color: #495057;", "Produced by ",
                        tags$a(href = "https://biodiversity-futures-lab.github.io/", target = "_blank", style = "color: #1976d2;", "The Biodiversity Futures Lab"),
                        " at the Natural History Museum.")
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(style = "color: #1976d2;", "Biodiversity intactness is declining in areas critical for delivery of nature-based ecosystem services [Manuscript in preparation]."),
              br(),
              tags$span(style = "color: #495057;", "G. Albaladejo-Robles et al. (2026).")
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(style = "color: #1976d2;", "World Database on Protected Areas (WDPA)"),
              br(),
              tags$span(style = "color: #495057;", "UNEP-WCMC and IUCN (2025), Cambridge, UK: UNEP-WCMC and IUCN. Available at: ",
                        tags$a(href = "https://www.protectedplanet.net", "www.protectedplanet.net", target = "_blank", style = "color: #1976d2;"))
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px;",
              tags$strong(style = "color: #1976d2;", "Critical Natural Assets (Ecosystem Services)"),
              br(),
              tags$span(style = "color: #495057;", "Chaplin-Kramer, R., et al. (2023). Mapping the planet's critical natural assets. Nature Ecology & Evolution.")
            )
          ),
          
          # Contact
          tags$div(
            style = "background: #f5f5f5; padding: 20px; border-radius: 10px; text-align: center;",
            h3("📧 Contact", style = "font-weight: bold; color: #4a4e55; margin-top: 0;"),
            p(style = "margin: 0;", "For questions or issues, please contact: ", 
              tags$a(href = "mailto:charlotte.mcginty@nhm.ac.uk", 
                     style = "color: #4a80d2; font-weight: 600;",
                     "charlotte.mcginty@nhm.ac.uk")
            )
          )
        )
      )
    )
  )
)

# ---- SERVER ----

server <- function(input, output, session) {
  
  # Reactive to get years for the selected country
  bii_years <- reactive({
    req(input$country)
    get_bii_years(input$country)
  })
  
  # ---- DYNAMIC HEADERS ----
  output$bii_first_header <- renderUI({
    years <- bii_years()
    year_label <- if (!is.na(years[1])) years[1] else "First Year"
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        paste("Biodiversity Intactness Index", year_label)
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Biodiversity Intactness Index (0-100%) for", year_label, "."))
      )
    )
  })
  
  output$bii_last_header <- renderUI({
    years <- bii_years()
    year_label <- if (!is.na(years[2])) years[2] else "Last Year"
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        paste("Biodiversity Intactness Index", year_label)
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Biodiversity Intactness Index (0-100%) for", year_label, "."))
      )
    )
  })
  
  # Dynamic headers for BII change maps
  output$bii_change_header <- renderUI({
    years <- bii_years()
    year_range <- if (!is.na(years[1]) && !is.na(years[2])) {
      paste("between", years[1], "and", years[2])
    } else {
      "over time"
    }
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        "Biodiversity Intactness Index Change"
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Change in Biodiversity Intactness Index", year_range))
      )
    )
  })
  
  output$bii_change_WDPA_header <- renderUI({
    years <- bii_years()
    year_range <- if (!is.na(years[1]) && !is.na(years[2])) {
      paste("between", years[1], "and", years[2])
    } else {
      "over time"
    }
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        "Biodiversity Intactness Index Change in Protected Areas"
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Change in Biodiversity Intactness Index", year_range, "within protected areas (WDPA)."))
      )
    )
  })
  
  output$bii_change_WDPA_CNA_header <- renderUI({
    years <- bii_years()
    year_range <- if (!is.na(years[1]) && !is.na(years[2])) {
      paste("between", years[1], "and", years[2])
    } else {
      "over time"
    }
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        "Biodiversity Intactness Index Change in Protected Areas and Critical Ecosystem Areas"
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Change in Biodiversity Intactness Index", year_range, "where protected areas (WDPA) and critical ecosystem services (CNA) overlap."))
      )
    )
  })
  
  output$bii_change_CNA_header <- renderUI({
    years <- bii_years()
    year_range <- if (!is.na(years[1]) && !is.na(years[2])) {
      paste("between", years[1], "and", years[2])
    } else {
      "over time"
    }
    h3(
      style = "font-size: 1rem; font-weight: bold; background: #e3f2fd; color: #0d47a1; padding: 6px 14px; border-radius: 8px; margin-bottom: 5px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
      span(
        style = "flex: 1; padding-right: 40px;",
        "Biodiversity Intactness Index Change in Critical Ecosystem Areas"
      ),
      span(
        class = "info-icon",
        style = "position: absolute; right: 14px;",
        tabindex = "0",
        role = "button",
        `aria-label` = "More information",
        HTML("ⓘ"),
        span(class = "tooltip-text", paste("Change in Biodiversity Intactness Index", year_range, "within the top 30% critical ecosystem service areas (CNA)."))
      )
    )
  })
  
  # ---- CLICK HANDLERS FOR MODAL VIEW ----
  
  observeEvent(input$overlay_map_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_overlay.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_overlay.png"), style = "width: 100%; height: auto;"),
        title = "Protected Areas and Critical Ecosystem Areas Overlap",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_first_click, {
    req(input$country)
    years <- bii_years()
    year_file <- if (!is.na(years[1])) years[1] else return()
    img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_", year_file, ".png"), style = "width: 100%; height: auto;"),
        title = paste("Biodiversity Intactness Index", year_file),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_last_click, {
    req(input$country)
    years <- bii_years()
    year_file <- if (!is.na(years[2])) years[2] else return()
    img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_", year_file, ".png"), style = "width: 100%; height: auto;"),
        title = paste("Biodiversity Intactness Index", year_file),
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_change_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_change.png"), style = "width: 100%; height: auto;"),
        title = "Biodiversity Intactness Index Change",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_change_WDPA_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_change_WDPA.png"), style = "width: 100%; height: auto;"),
        title = "Biodiversity Intactness Index Change in Protected Areas",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_change_CNA_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_CNA.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_change_CNA.png"), style = "width: 100%; height: auto;"),
        title = "Biodiversity Intactness Index Change in Critical Ecosystem Areas",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$bii_change_WDPA_CNA_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA_CNA.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(src = paste0("maps/", input$country, "_bii_change_WDPA_CNA.png"), style = "width: 100%; height: auto;"),
        title = "Biodiversity Intactness Index Change in Protected Areas and Critical Ecosystem Areas",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # ---- DISPLAY MAPS (LOAD PRE-GENERATED PNGS) ----
  
  # Overlay plot
  output$overlay_map <- renderImage({
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_overlay.png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("WDPA/CNA Overlay for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII First Year
  output$bii_first <- renderImage({
    req(input$country)
    years <- bii_years()
    year_file <- if (!is.na(years[1])) years[1] else return(list(src = NULL, alt = "Map not available."))
    img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII", year_file, "for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII Last Year
  output$bii_last <- renderImage({
    req(input$country)
    years <- bii_years()
    year_file <- if (!is.na(years[2])) years[2] else return(list(src = NULL, alt = "Map not available."))
    img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII", year_file, "for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII Change
  output$bii_change <- renderImage({
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change.png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII Change for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII Change WDPA
  output$bii_change_WDPA <- renderImage({
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA.png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII Change WDPA for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII Change CNA
  output$bii_change_CNA <- renderImage({
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_CNA.png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII Change CNA for", input$country)
    )
  }, deleteFile = FALSE)
  
  # BII Change WDPA/CNA
  output$bii_change_WDPA_CNA <- renderImage({
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA_CNA.png"))
    if (!file.exists(img_path)) {
      return(list(src = NULL, alt = "Map not available."))
    }
    list(
      src = img_path,
      contentType = 'image/png',
      alt = paste("BII Change WDPA/CNA for", input$country)
    )
  }, deleteFile = FALSE)
  
  
  # ---- DOWNLOAD HANDLERS (COPY PRE-GENERATED MAPS) ----
  output$download_overlay_map <- downloadHandler(
    filename = function() paste0("WDPA_CNA_Overlap_", input$country, ".png"),
    content = function(file) {
      shinyjs::disable("download_overlay_map")
      on.exit(shinyjs::enable("download_overlay_map"), add = TRUE)
      img_path <- file.path("maps", paste0(input$country, "_overlay.png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_first <- downloadHandler(
    filename = function() {
      years <- bii_years()
      year_label <- if (!is.na(years[1])) years[1] else "first"
      paste0("BII_", year_label, "_", input$country, ".png")
    },
    content = function(file) {
      shinyjs::disable("download_bii_first")
      on.exit(shinyjs::enable("download_bii_first"), add = TRUE)
      years <- bii_years()
      year_file <- if (!is.na(years[1])) years[1] else stop("Year not available")
      img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_last <- downloadHandler(
    filename = function() {
      years <- bii_years()
      year_label <- if (!is.na(years[2])) years[2] else "last"
      paste0("BII_", year_label, "_", input$country, ".png")
    },
    content = function(file) {
      shinyjs::disable("download_bii_last")
      on.exit(shinyjs::enable("download_bii_last"), add = TRUE)
      years <- bii_years()
      year_file <- if (!is.na(years[2])) years[2] else stop("Year not available")
      img_path <- file.path("maps", paste0(input$country, "_bii_", year_file, ".png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_change <- downloadHandler(
    filename = function() paste0("BII_change_", input$country, ".png"),
    content = function(file) {
      shinyjs::disable("download_bii_change")
      on.exit(shinyjs::enable("download_bii_change"), add = TRUE)
      img_path <- file.path("maps", paste0(input$country, "_bii_change.png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_change_WDPA <- downloadHandler(
    filename = function() paste0("BII_change_WDPA_", input$country, ".png"),
    content = function(file) {
      shinyjs::disable("download_bii_change_WDPA")
      on.exit(shinyjs::enable("download_bii_change_WDPA"), add = TRUE)
      img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA.png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_change_CNA <- downloadHandler(
    filename = function() paste0("BII_change_CNA_", input$country, ".png"),
    content = function(file) {
      shinyjs::disable("download_bii_change_CNA")
      on.exit(shinyjs::enable("download_bii_change_CNA"), add = TRUE)
      img_path <- file.path("maps", paste0(input$country, "_bii_change_CNA.png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
  
  output$download_bii_change_WDPA_CNA <- downloadHandler(
    filename = function() paste0("BII_change_WDPA_CNA_", input$country, ".png"),
    content = function(file) {
      shinyjs::disable("download_bii_change_WDPA_CNA")
      on.exit(shinyjs::enable("download_bii_change_WDPA_CNA"), add = TRUE)
      img_path <- file.path("maps", paste0(input$country, "_bii_change_WDPA_CNA.png"))
      if (!file.exists(img_path)) {
        stop("Map not available for this country")
      }
      if (!file.copy(img_path, file, overwrite = TRUE)) {
        stop("Failed to copy map file")
      }
    }
  )
    
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)