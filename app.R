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
map_files <- list.files(
  "maps",
  pattern = "^[A-Z]{3}_overlay\\.png$",
  full.names = FALSE
)

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
  bii_files <- list.files(
    "maps",
    pattern = paste0("^", iso3, "_bii_[0-9]{4}\\.png$"),
    full.names = FALSE
  )

  if (length(bii_files) == 0) {
    return(c(NA, NA))
  }

  # Extract years from filenames
  years <- as.numeric(gsub(
    paste0("^", iso3, "_bii_(\\d{4})\\.png$"),
    "\\1",
    bii_files
  ))
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
  lang = "en",
  tags$head(
    tags$title("30by30 BII Country Map Explorer")
  )
  useShinyjs(),
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Nunito"),
    bg = "#f6f8fa",
    fg = "#363640",
    primary = "#4a80d2"
  ),
  tags$head(
    tags$script(HTML(
      "
        (function() {
          function labelCountrySelect() {
            var select = document.getElementById('country');

            if (!select) {
              return;
            }

            var group = select.closest('.form-group');
            var label = group ? group.querySelector('label') : null;

            if (label) {
              label.id = 'country-label';
            }

            select.setAttribute('aria-label', 'Select Country');
            select.setAttribute('aria-labelledby', 'country-label');

            var visibleInput = select.parentElement
              ? select.parentElement.querySelector('.selectize-control input')
              : null;

            if (visibleInput) {
              visibleInput.setAttribute('aria-label', 'Select Country');
              visibleInput.setAttribute('aria-labelledby', 'country-label');
            }
          }

          document.addEventListener('DOMContentLoaded', labelCountrySelect);
          document.addEventListener('shiny:connected', labelCountrySelect);
          document.addEventListener('shiny:bound', labelCountrySelect);

          var observer = new MutationObserver(labelCountrySelect);

          observer.observe(document.documentElement, {
            childList: true,
            subtree: true
          });
        })();
      "
    )),
    tags$style(HTML(
      ":root {
        --app-primary: #245aab;
        --app-primary-dark: #174a9c;
        --app-primary-border: #1d4787;
        --app-success: #2e7d32;
        --app-text: #374151;
        --app-text-muted: #4b5563;
        --app-border: #d9dee7;
        --app-border-light: #e2e8f0;
        --app-surface: #ffffff;
        --app-surface-muted: #f8fafc;
        --app-radius: 10px;
        --app-shadow: 0 4px 14px rgba(31, 41, 55, 0.06);
        --app-focus: 0 0 0 3px rgba(36, 90, 171, 0.16);
      }
      .nav-tabs {
        border-bottom: 1px solid #9ca3af !important;
      }
      .nav-tabs .nav-link,
      .nav-tabs .nav-link:visited,
      .nav-tabs .nav-link:focus,
      .nav-tabs .nav-link:hover,
      .nav-tabs > li > a,
      .nav-tabs > li > a:visited,
      .nav-tabs > li > a:focus,
      .nav-tabs > li > a:hover {
        background-color: #f6f8fa !important;
        border: 1px solid transparent !important;
        border-bottom-color: #9ca3af !important;
        color: #174a9c !important;
        font-size: 14px !important;
        padding-left: 10px !important;
        padding-right: 10px !important;
      }
      .nav-tabs .nav-link.active,
      .nav-tabs .nav-link.active:focus,
      .nav-tabs .nav-link.active:hover,
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #f6f8fa !important;
        border-color: #9ca3af #9ca3af #f6f8fa !important;
        color: #2e7d32 !important;
        font-weight: 600;
      }
      .country-sidebar {
        background: #ffffff !important;
        border: 1px solid #d9dee7 !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 16px rgba(31, 41, 55, 0.07) !important;
        padding: 20px !important;
      }
      .country-sidebar .control-label {
        color: #374151 !important;
        font-size: 0.82rem !important;
        font-weight: 700 !important;
        letter-spacing: 0.04em;
        margin-bottom: 8px !important;
      }
      .country-sidebar .selectize-control.single .selectize-input {
        background: #f8fafc !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 8px !important;
        box-shadow: none !important;
        color: #1f2937 !important;
        min-height: 42px;
        padding: 10px 12px !important;
      }
      .country-sidebar .selectize-control.single .selectize-input.focus {
        border-color: #245aab !important;
        box-shadow: 0 0 0 3px rgba(36, 90, 171, 0.16) !important;
      }
      .country-sidebar .selectize-dropdown {
        border: 1px solid #cbd5e1 !important;
        border-radius: 8px !important;
        box-shadow: 0 8px 20px rgba(31, 41, 55, 0.12) !important;
        overflow: hidden;
      }
      .sidebar-card {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 10px;
        margin-top: 14px;
        padding: 15px 16px;
      }
      .sidebar-card-title {
        color: #374151;
        font-size: 0.78rem;
        font-weight: 700;
        letter-spacing: 0.07em;
        margin-bottom: 7px;
        text-transform: uppercase;
      }
      .sidebar-card-text {
        color: #4b5563;
        font-size: 0.8rem;
        line-height: 1.65;
      }
      .release-card {
        background: #f7f9fc;
        border-left: 3px solid #245aab;
      }
      .source-card {
        background: #ffffff;
        border-left: 3px solid #94a3b8;
      }
      .about-content {
        color: #374151;
      }
      .about-hero,
      .about-card,
      .about-bii-card {
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        box-shadow: 0 4px 14px rgba(31, 41, 55, 0.06);
        margin-bottom: 24px;
        padding: 25px;
      }
      .about-hero {
        background: #f8fafc;
        border-left: 4px solid #245aab;
      }
      .about-hero-title,
      .about-card-title,
      .about-bii-title {
        color: #26364a;
        font-weight: 700;
        margin-top: 0;
      }
      .about-hero-text,
      .about-bii-text,
      .about-card p,
      .about-card li {
        color: #4b5563;
        line-height: 1.7;
      }
      .about-bii-card {
        background: #ffffff;
        border-left: 4px solid #2e7d32;
      }
      .about-bii-highlight {
        background: #f7faf8;
        border: 1px solid #d7e8da;
        border-radius: 8px;
        color: #374151;
        margin-top: 16px;
        padding: 14px 16px;
      }
      .about-analysis-card {
        background: #fbfaf6;
        border-left: 4px solid #9a7b2f;
      }
      .about-blue-card {
        background: #ffffff;
        border-left: 4px solid #5b7fa8;
      }
      .about-contact-card {
        background: #f8fafc;
        text-align: center;
      }
      .about-card a {
        color: #174a9c !important;
      }
      .about-explore-button {
        background: #245aab !important;
        border: 1px solid #1d4787 !important;
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(36, 90, 171, 0.2);
        color: #ffffff !important;
        display: inline-block;
        font-weight: 600;
        padding: 12px 30px;
        text-decoration: none !important;
      }
      .about-card a.about-explore-button,
      .about-card a.about-explore-button:visited,
      .about-card a.about-explore-button:hover,
      .about-card a.about-explore-button:focus {
        background: #245aab !important;
        border-color: #1d4787 !important;
        color: #ffffff !important;
        text-decoration: none !important;
      }
      .about-card a.about-explore-button:hover,
      .about-card a.about-explore-button:focus {
        background: #174a9c !important;
        border-color: #123b7a !important;
      }
      .map-actions {
        display: flex;
        gap: 8px;
        margin-top: 8px;
      }
      .map-icon-button,
      .map-download-button {
        align-items: center;
        background: var(--app-surface) !important;
        border: 1px solid var(--app-primary-border) !important;
        border-radius: 6px;
        color: var(--app-primary) !important;
        cursor: pointer;
        display: inline-flex;
        font-size: 1rem !important;
        font-weight: 700;
        height: 34px;
        justify-content: center;
        line-height: 1;
        margin: 0 !important;
        min-width: 34px;
        padding: 6px 9px !important;
        text-decoration: none !important;
      }
      .map-icon-button:hover,
      .map-icon-button:focus-visible,
      .map-download-button:hover,
      .map-download-button:focus-visible {
        background: var(--app-primary) !important;
        border-color: var(--app-primary-border) !important;
        color: #ffffff !important;
        outline: none;
      }
      .map-icon-button:focus-visible,
      .map-download-button:focus-visible {
        box-shadow: var(--app-focus);
      }
      .accessible-link,
      .accessible-link:visited,
      .accessible-link:hover,
      .accessible-link:focus {
        color: #174a9c !important;
        text-decoration: underline;
      }
      a.shiny-download-link[id^=\"download_\"],
      a[id^=\"download_\"] {
        background-color: #ffffff !important;
        border: 1px solid #374151 !important;
        color: #1f2937 !important;
        font-size: 14px !important;
        text-decoration: none !important;
      }
      a.shiny-download-link[id^=\"download_\"]:hover,
      a.shiny-download-link[id^=\"download_\"]:focus,
      a[id^=\"download_\"]:hover,
      a[id^=\"download_\"]:focus {
        background-color: #f3f4f6 !important;
        border-color: #111827 !important;
        color: #111827 !important;
      }
      .small-btn,
      .small-btn .btn {
        background-color: #245aab !important;
        border: 1px solid #1d4787 !important;
        color: #ffffff !important;
        font-size: 16px !important;
        font-weight: 600 !important;
        padding: 4px 12px !important;
        height: 36px !important;
        min-width: 80px;
      }
      .small-btn:hover,
      .small-btn:focus,
      .small-btn .btn:hover,
      .small-btn .btn:focus {
        background-color: #174a9c !important;
        border-color: #123b7a !important;
        color: #ffffff !important;
      }
      .small-btn:disabled,
      .small-btn.disabled,
      .small-btn .btn:disabled,
      .small-btn .btn.disabled {
        background-color: #6b7280 !important;
        border-color: #4b5563 !important;
        color: #ffffff !important;
        opacity: 1 !important;
      }
      .map-page-one {
        display: grid;
        grid-template-columns: minmax(0, 0.45fr) minmax(0, 0.55fr);
        grid-template-rows: auto auto;
        gap: 16px;
        align-items: start;
      }
      .map-page-one .map-card {
        position: relative;
        min-width: 0;
      }
      .map-page-one .bii-card {
        grid-column: 1;
      }
      .map-page-one .bii-card:nth-child(1) {
        grid-row: 1;
      }
      .map-page-one .bii-card:nth-child(2) {
        grid-row: 2;
      }
      .map-page-one .overlay-card {
        grid-column: 2;
        grid-row: 1 / span 2;
        width: 100%;
        align-self: stretch;
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
      .loader {
        color: #374151 !important;
        font-size: 1rem !important;
        font-weight: 600 !important;
        line-height: 1.5 !important;
        text-align: center !important;
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

  h1("30x30 BII Country Map Explorer", class = "visually-hidden"),

  div(
    style = paste(
      "display: flex; align-items: center;",
      "padding-bottom: 16px;",
      "padding-top: 16px;"
    ),
    tags$img(
      src = "NHM_logo_small_blue.png",
      alt = "Natural History Museum Logo",
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
      "30x30 BII Country Map Explorer"
    )
  ),
  tabsetPanel(
    type = "tabs",
    # ---- Map by Country Tab ----
    tabPanel(
      "Maps by Country",
      h2("Maps by Country", class = "visually-hidden"),
      br(),
      sidebarLayout(
        # ---- Left sidebar ----
        sidebarPanel(
          width = 3,
          class = "country-sidebar",
          selectInput(
            inputId = "country",
            label = "Select Country",
            choices = iso3_choices_named,
            selectize = TRUE
          ),
          br(),
          tags$div(
            class = "sidebar-card release-card",
            tags$div(class = "sidebar-card-title", "Limited Release"),
            tags$div(
              class = "sidebar-card-text",
              "This application is in limited release with a selection of countries. These are preliminary results and subject to change."
            )
          ),
          tags$div(
            class = "sidebar-card",
            tags$div(class = "sidebar-card-title", "Data Coverage Note"),
            tags$div(
              class = "sidebar-card-text",
              "• Remote or overseas territories may be excluded.",
              tags$br(),
              "• Territorial units smaller than 1,000 km² are not included in this dataset."
            )
          ),
          tags$div(
            class = "sidebar-card source-card",
            tags$div(
              class = "sidebar-card-text",
              tags$strong("Biodiversity Intactness Index (BII) v3.1.0"),
              tags$br(),
              "Produced by ",
              tags$a(
                href = "https://biodiversity-futures-lab.github.io/",
                target = "_blank",
                class = "accessible-link",
                "The Biodiversity Futures Lab"
              ),
              " at the Natural History Museum."
            )
          )
        ),
        # ---- Main panel with paged maps ----
        mainPanel(
          width = 9,
          div(
            style = "display: none;",
            numericInput(
              "map_page",
              "Current map page",
              value = 1,
              min = 1,
              max = 2,
              step = 1
            )
          ),
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 16px;",
            actionButton(
              "map_previous",
              "Previous",
              class = "small-btn"
            ),
            span(),
            actionButton(
              "map_next",
              "Next",
              class = "small-btn"
            )
          ),
          conditionalPanel(
            condition = "input.map_page == 1",
            div(
              class = "map-page-one",
              div(
                class = "map-card bii-card",
                uiOutput("bii_first_header"),
                withSpinner(imageOutput(
                  "bii_first",
                  click = "bii_first_click"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_first",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII map",
                    onclick = "Shiny.setInputValue('bii_first_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_first",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII map",
                    `aria-label` = "Download BII map"
                  )
                )
              ),
              div(
                class = "map-card bii-card",
                uiOutput("bii_last_header"),
                withSpinner(imageOutput("bii_last", click = "bii_last_click")),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_last",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII map",
                    onclick = "Shiny.setInputValue('bii_last_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_last",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII map",
                    `aria-label` = "Download BII map"
                  )
                )
              ),
              div(
                class = "map-card overlay-card",
                h3(
                  style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
                    span(
                      class = "tooltip-text",
                      "Overlap between protected areas (WDPA) and the top 30% critical ecosystem service areas (CNA)."
                    )
                  )
                ),
                withSpinner(imageOutput(
                  "overlay_map",
                  click = "overlay_map_click",
                  width = "100%"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_overlay_map",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger overlap map",
                    onclick = "Shiny.setInputValue('overlay_map_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_overlay_map",
                    "↓",
                    class = "map-download-button",
                    title = "Download overlap map",
                    `aria-label` = "Download overlap map"
                  )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.map_page == 2",
            div(
              style = "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 16px;",
              div(
                style = "position: relative;",
                uiOutput("bii_change_header"),
                withSpinner(imageOutput(
                  "bii_change",
                  click = "bii_change_click"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_change",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII change map",
                    onclick = "Shiny.setInputValue('bii_change_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_change",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII change map",
                    `aria-label` = "Download BII change map"
                  )
                )
              ),
              div(
                style = "position: relative;",
                uiOutput("bii_change_WDPA_header"),
                withSpinner(imageOutput(
                  "bii_change_WDPA",
                  click = "bii_change_WDPA_click"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_change_WDPA",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII change WDPA map",
                    onclick = "Shiny.setInputValue('bii_change_WDPA_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_change_WDPA",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII change WDPA map",
                    `aria-label` = "Download BII change WDPA map"
                  )
                )
              ),
              div(
                style = "position: relative;",
                uiOutput("bii_change_CNA_header"),
                withSpinner(imageOutput(
                  "bii_change_CNA",
                  click = "bii_change_CNA_click"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_change_CNA",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII change CNA map",
                    onclick = "Shiny.setInputValue('bii_change_CNA_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_change_CNA",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII change CNA map",
                    `aria-label` = "Download BII change CNA map"
                  )
                )
              ),
              div(
                style = "position: relative;",
                uiOutput("bii_change_WDPA_CNA_header"),
                withSpinner(imageOutput(
                  "bii_change_WDPA_CNA",
                  click = "bii_change_WDPA_CNA_click"
                )),
                div(
                  class = "map-actions",
                  actionButton(
                    "view_bii_change_WDPA_CNA",
                    "↗",
                    class = "map-icon-button",
                    title = "View larger map",
                    `aria-label` = "View larger BII change WDPA/CNA map",
                    onclick = "Shiny.setInputValue('bii_change_WDPA_CNA_click', Date.now(), {priority: 'event'});"
                  ),
                  downloadButton(
                    "download_bii_change_WDPA_CNA",
                    "↓",
                    class = "map-download-button",
                    title = "Download BII change WDPA/CNA map",
                    `aria-label` = "Download BII change WDPA/CNA map"
                  )
                )
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
          class = "about-content",
          # Biodiversity Intactness Index overview
          tags$div(
            class = "about-bii-card",
            h2(
              "🌿 Biodiversity Intactness Index",
              class = "about-bii-title"
            ),
            p(
              class = "about-bii-text",
              "The Biodiversity Intactness Index (BII) measures how much the composition and abundance of species in an area remain similar to those expected in a minimally disturbed reference state. It provides a consistent way to explore changes in biodiversity across countries and through time."
            ),
            tags$div(
              class = "about-bii-highlight",
              tags$strong("How to read the index: "),
              "higher values indicate greater biodiversity intactness, while lower values indicate greater change from the reference state."
            )
          ),

          # About 30by30 - Hero Section
          tags$div(
            class = "about-hero",
            h2(
              "🌍 30 by 30",
              class = "about-hero-title"
            ),
            p(
              class = "about-hero-text",
              "30by30 is a global commitment established in the Kunming–Montreal Global Biodiversity Framework to protect 30% of the world's land and waters by 2030. The initiative aims to expand and strengthen protected and conserved areas to safeguard biodiversity and support human wellbeing."
            )
          ),

          # About This App
          tags$div(
            class = "about-card about-card-accent",
            h3(
              "📊 About This App",
              class = "about-card-title"
            ),
            p(
              style = "color: #495057;",
              "This app provides country-level maps showing:"
            ),
            tags$ul(
              style = "color: #495057; line-height: 1.8;",
              tags$li(
                tags$strong("Biodiversity Intactness Index (BII)"),
                " for different time periods"
              ),
              tags$li(
                tags$strong("Changes in BII over time"),
                tags$ul(
                  style = "margin-top: 8px;",
                  tags$li("Within existing protected areas (WDPA)"),
                  tags$li("Within Critical Natural Assets (CNA)")
                )
              )
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-top: 15px; border-left: 3px solid #28a745;",
              p(
                style = "margin: 0; color: #495057;",
                "By combining biodiversity trends with conservation designations, the maps help users explore how well current protected areas and critical natural assets align."
              )
            ),
            tags$div(
              style = "margin-top: 20px; text-align: center;",
              tags$a(
                href = "#",
                onclick = "$('a[data-value=\"Maps by Country\"]').tab('show'); return false;",
                class = "about-explore-button",
                "🗺️ Explore Maps by Country"
              )
            )
          ),

          # NHM's Analysis
          tags$div(
            class = "about-card about-analysis-card",
            h3(
              "🔬 NHM's 30by30 Analysis",
              class = "about-card-title"
            ),
            p(
              style = "color: #5d4037;",
              "As part of the Natural History Museum's contribution to global 30by30 efforts, the Museum has developed a national-level analysis to help identify where conservation could have the greatest impact."
            ),
            p(
              style = "color: #5d4037;",
              "Using data from Chaplin-Kramer et al. (2023) on ecosystem service provision, NHM identified the top 30% of land within each country that delivers the most important Nature's Contributions to People (NCP). These areas are termed ",
              tags$strong("Critical Natural Assets (CNA)"),
              "."
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin: 15px 0;",
              p(
                style = "margin: 0 0 10px 0; color: #5d4037; font-weight: 600;",
                "By comparing CNA with the World Database on Protected Areas (WDPA), the analysis highlights:"
              ),
              tags$div(
                style = "color: #5d4037; line-height: 1.8; margin: 0;",
                tags$div(
                  style = "margin-bottom: 10px;",
                  "✅ Where protected areas already overlap areas providing essential ecosystem services"
                ),
                tags$div(
                  "🎯 Where opportunities remain to strengthen or expand protection toward 30by30 goals"
                )
              )
            ),
            p(
              style = "color: #5d4037;",
              "This app makes those comparisons visible alongside observed biodiversity trends."
            ),
            p(
              style = "margin-top: 15px;",
              tags$a(
                href = "https://www.nhm.ac.uk/our-science/services/data/biodiversity-intactness-index/policy/30by30.html",
                target = "_blank",
                style = "color: #e65100; font-weight: 600; text-decoration: underline;",
                "Learn more about NHM's 30by30 work →"
              )
            )
          ),

          # Associated Publication
          tags$div(
            class = "about-card about-blue-card",
            h3(
              "📄 Associated Publication",
              class = "about-card-title"
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(
                style = "color: #1976d2;",
                "G. Albaladejo-Robles et al. (2026)."
              ),
              tags$span(
                style = "color: #495057;",
                "Biodiversity intactness is declining in areas critical for delivery of nature-based ecosystem services [Manuscript in preparation]."
              )
            )
          ),

          # Data Sources
          tags$div(
            class = "about-card about-blue-card",
            h3(
              "📚 Data Sources",
              class = "about-card-title"
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(
                style = "color: #1976d2;",
                "Biodiversity Intactness Index (BII) v3.1.0"
              ),
              br(),
              tags$span(
                style = "color: #495057;",
                "Produced by ",
                tags$a(
                  href = "https://biodiversity-futures-lab.github.io/",
                  target = "_blank",
                  style = "color: #1976d2;",
                  "The Biodiversity Futures Lab"
                ),
                " at the Natural History Museum."
              )
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong(
                style = "color: #1976d2;",
                "World Database on Protected Areas (WDPA)"
              ),
              br(),
              tags$span(
                style = "color: #495057;",
                "UNEP-WCMC and IUCN (2025), Cambridge, UK: UNEP-WCMC and IUCN. Available at: ",
                tags$a(
                  href = "https://www.protectedplanet.net",
                  "www.protectedplanet.net",
                  target = "_blank",
                  style = "color: #1976d2;"
                )
              )
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px;",
              tags$strong(
                style = "color: #1976d2;",
                "Critical Natural Assets (Ecosystem Services)"
              ),
              br(),
              tags$span(
                style = "color: #495057;",
                "Chaplin-Kramer, R., et al. (2023). Mapping the planet's critical natural assets. Nature Ecology & Evolution."
              )
            )
          ),

          # Source Code
          tags$div(
            class = "about-card about-blue-card",
            h3(
              "💻 Source Code",
              class = "about-card-title"
            ),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 6px;",
              tags$span(
                style = "color: #495057;",
                "The code for this app is available on GitHub: ",
                tags$a(
                  href = "https://github.com/Biodiversity-Futures-Lab/30by30-map-explorer",
                  "https://github.com/Biodiversity-Futures-Lab/30by30-map-explorer",
                  target = "_blank",
                  style = "color: #1976d2; font-weight: 600; text-decoration: underline;"
                )
              )
            )
          ),

          # Contact
          tags$div(
            class = "about-card about-contact-card",
            h3(
              "📧 Contact",
              class = "about-card-title"
            ),
            p(
              style = "margin: 0;",
              "For questions or issues, please contact: ",
              tags$a(
                href = "mailto:charlotte.mcginty@nhm.ac.uk",
                style = "color: #4a80d2; font-weight: 600;",
                "charlotte.mcginty@nhm.ac.uk"
              )
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

  # ---- MAP PAGINATION ----
  map_page <- reactiveVal(1)
  map_page_count <- 2
  map_count <- 7

  observeEvent(input$map_previous, {
    map_page(max(1, map_page() - 1))
    updateNumericInput(session, "map_page", value = map_page())
  })

  observeEvent(input$map_next, {
    map_page(min(map_page_count, map_page() + 1))
    updateNumericInput(session, "map_page", value = map_page())
  })

  output$map_page_label <- renderText({
    if (map_page() == 1) {
      "BII maps and overlap"
    } else {
      "BII change maps"
    }
  })

  observe({
    updateActionButton(session, "map_previous", disabled = map_page() == 1)
    updateActionButton(
      session,
      "map_next",
      disabled = map_page() == map_page_count
    )
  })

  # ---- DYNAMIC HEADERS ----
  output$bii_first_header <- renderUI({
    years <- bii_years()
    year_label <- if (!is.na(years[1])) years[1] else "First Year"
    h3(
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste("Biodiversity Intactness Index (0-100%) for", year_label, ".")
        )
      )
    )
  })

  output$bii_last_header <- renderUI({
    years <- bii_years()
    year_label <- if (!is.na(years[2])) years[2] else "Last Year"
    h3(
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste("Biodiversity Intactness Index (0-100%) for", year_label, ".")
        )
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
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste("Change in Biodiversity Intactness Index", year_range)
        )
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
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste(
            "Change in Biodiversity Intactness Index",
            year_range,
            "within protected areas (WDPA)."
          )
        )
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
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste(
            "Change in Biodiversity Intactness Index",
            year_range,
            "where protected areas (WDPA) and critical ecosystem services (CNA) overlap."
          )
        )
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
      style = "font-size: 1rem; font-weight: 600; background: transparent; color: #3f4650; padding: 8px 4px; border-bottom: 1px solid #d1d5db; border-radius: 0; margin-bottom: 8px; white-space: normal; word-wrap: break-word; overflow-wrap: break-word; min-height: 50px; display: flex; align-items: center; position: relative;",
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
        span(
          class = "tooltip-text",
          paste(
            "Change in Biodiversity Intactness Index",
            year_range,
            "within the top 30% critical ecosystem service areas (CNA)."
          )
        )
      )
    )
  })

  # ---- CLICK HANDLERS FOR MODAL VIEW ----

  observeEvent(input$overlay_map_click, {
    req(input$country)
    img_path <- file.path("maps", paste0(input$country, "_overlay.png"))
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(
          src = paste0("maps/", input$country, "_overlay.png"),
          alt = paste(
            "Protected Areas and Critical Ecosystem Areas Overlap map for",
            input$country
          ),
          style = "width: 100%; height: auto;"
        ),
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
    img_path <- file.path(
      "maps",
      paste0(input$country, "_bii_", year_file, ".png")
    )
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(
          src = paste0("maps/", input$country, "_bii_", year_file, ".png"),
          alt = paste(
            "Biodiversity Intactness Index map for",
            input$country,
            "in",
            year_file
          ),
          style = "width: 100%; height: auto;"
        ),
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
    img_path <- file.path(
      "maps",
      paste0(input$country, "_bii_", year_file, ".png")
    )
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(
          src = paste0("maps/", input$country, "_bii_", year_file, ".png"),
          alt = paste(
            "Biodiversity Intactness Index map for",
            input$country,
            "in",
            year_file
          ),
          style = "width: 100%; height: auto;"
        ),
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
        tags$img(
          src = paste0("maps/", input$country, "_bii_change.png"),
          alt = paste(
            "Biodiversity Intactness Index Change map for",
            input$country
          ),
          style = "width: 100%; height: auto;"
        ),
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
        tags$img(
          src = paste0("maps/", input$country, "_bii_change_WDPA.png"),
          alt = paste(
            "Biodiversity Intactness Index Change in Protected Areas map for",
            input$country
          ),
          style = "width: 100%; height: auto;"
        ),
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
        tags$img(
          src = paste0("maps/", input$country, "_bii_change_CNA.png"),
          alt = paste(
            "Biodiversity Intactness Index Change in Critical Ecosystem Areas map for",
            input$country
          ),
          style = "width: 100%; height: auto;"
        ),
        title = "Biodiversity Intactness Index Change in Critical Ecosystem Areas",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  observeEvent(input$bii_change_WDPA_CNA_click, {
    req(input$country)
    img_path <- file.path(
      "maps",
      paste0(input$country, "_bii_change_WDPA_CNA.png")
    )
    if (file.exists(img_path)) {
      showModal(modalDialog(
        tags$img(
          src = paste0("maps/", input$country, "_bii_change_WDPA_CNA.png"),
          alt = paste(
            "Biodiversity Intactness Index Change in Protected Areas and Critical Ecosystem Areas map for",
            input$country
          ),
          style = "width: 100%; height: auto;"
        ),
        title = "Biodiversity Intactness Index Change in Protected Areas and Critical Ecosystem Areas",
        size = "l",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })

  # ---- DISPLAY MAPS (LOAD PRE-GENERATED PNGS) ----

  # Overlay plot
  output$overlay_map <- renderImage(
    {
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
    },
    deleteFile = FALSE
  )

  # BII First Year
  output$bii_first <- renderImage(
    {
      req(input$country)
      years <- bii_years()
      year_file <- if (!is.na(years[1])) {
        years[1]
      } else {
        return(list(src = NULL, alt = "Map not available."))
      }
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_", year_file, ".png")
      )
      if (!file.exists(img_path)) {
        return(list(src = NULL, alt = "Map not available."))
      }
      list(
        src = img_path,
        contentType = 'image/png',
        alt = paste("BII", year_file, "for", input$country)
      )
    },
    deleteFile = FALSE
  )

  # BII Last Year
  output$bii_last <- renderImage(
    {
      req(input$country)
      years <- bii_years()
      year_file <- if (!is.na(years[2])) {
        years[2]
      } else {
        return(list(src = NULL, alt = "Map not available."))
      }
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_", year_file, ".png")
      )
      if (!file.exists(img_path)) {
        return(list(src = NULL, alt = "Map not available."))
      }
      list(
        src = img_path,
        contentType = 'image/png',
        alt = paste("BII", year_file, "for", input$country)
      )
    },
    deleteFile = FALSE
  )

  # BII Change
  output$bii_change <- renderImage(
    {
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
    },
    deleteFile = FALSE
  )

  # BII Change WDPA
  output$bii_change_WDPA <- renderImage(
    {
      req(input$country)
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_WDPA.png")
      )
      if (!file.exists(img_path)) {
        return(list(src = NULL, alt = "Map not available."))
      }
      list(
        src = img_path,
        contentType = 'image/png',
        alt = paste("BII Change WDPA for", input$country)
      )
    },
    deleteFile = FALSE
  )

  # BII Change CNA
  output$bii_change_CNA <- renderImage(
    {
      req(input$country)
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_CNA.png")
      )
      if (!file.exists(img_path)) {
        return(list(src = NULL, alt = "Map not available."))
      }
      list(
        src = img_path,
        contentType = 'image/png',
        alt = paste("BII Change CNA for", input$country)
      )
    },
    deleteFile = FALSE
  )

  # BII Change WDPA/CNA
  output$bii_change_WDPA_CNA <- renderImage(
    {
      req(input$country)
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_WDPA_CNA.png")
      )
      if (!file.exists(img_path)) {
        return(list(src = NULL, alt = "Map not available."))
      }
      list(
        src = img_path,
        contentType = 'image/png',
        alt = paste("BII Change WDPA/CNA for", input$country)
      )
    },
    deleteFile = FALSE
  )

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
      year_file <- if (!is.na(years[1])) {
        years[1]
      } else {
        stop("Year not available")
      }
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_", year_file, ".png")
      )
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
      year_file <- if (!is.na(years[2])) {
        years[2]
      } else {
        stop("Year not available")
      }
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_", year_file, ".png")
      )
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
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_WDPA.png")
      )
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
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_CNA.png")
      )
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
      img_path <- file.path(
        "maps",
        paste0(input$country, "_bii_change_WDPA_CNA.png")
      )
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
