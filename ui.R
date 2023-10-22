library(shiny)
library(shinydashboard)
# library(rhandsontable)
library(shinydashboardPlus)
library(shinyWidgets)
library(fresh)
library(shinyjs)
# library(mapview)
library(excelR)
library(markdown)

# source("global.R")

mytheme <- create_theme(
  adminlte_color(
    light_blue = COLOR_DARK
  ),
  adminlte_sidebar(
    dark_bg = COLOR_LIGHT,
    dark_hover_bg = "#A9D08F",
    dark_hover_color = "#000",
    dark_color = COLOR_DARK,
    dark_submenu_color = COLOR_LIGHT, #"#D1EDC0",
    dark_submenu_hover_color = "#FFF",
    dark_submenu_bg = COLOR_DARK #"#5A6E2E" 
    
  ),
  adminlte_global(
    content_bg = "#F1FFEB",
    box_bg = "#FFF", 
    info_box_bg = "#E0FFD1"
  )
)

create_scalar_tab <- function(title, table){
  tagList(h2(title), hr(class = "green"),
    fluidRow(
      apply(par_scalar_title_df[par_scalar_title_df$table == table,], 1, function(x){
        box(title = x["title"], collapsible = TRUE,
            width = as.numeric(x["width"]), collapsed = T,
            dropdownMenu = boxDropdown(
              icon = icon("ellipsis-vertical"),
              boxDropdownItem(id = paste0("load_", x["id"]), 
                              paste("Load", x["title"]),
                              icon = icon("folder-open")),
              boxDropdownItem(
                downloadButton(paste0("download_", x["id"]),
                               paste("Save", x["title"])))
            ),
            uiOutput(paste0("out_", x["id"]))
        )
      })
    )
  )
}

ui <- dashboardPage(title = "R-Fallow",

  dashboardHeader( 
    title = tagList(
      span(class = "logo-lg", 
           HTML("<b style='color:#FFF;background:#000;border-radius:20px;'>
                &nbsp;R&nbsp;</b>FALLOW Model")),
      img(src = "rfallow_icon.svg", style = "width: 20px")
  )),
  
  dashboardSidebar(minified = TRUE, 
    sidebarMenu(
      id = "sidemenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Input Parameters", tabName = "parameters", icon = icon("sliders"), 
         menuSubItem("Initial Input", tabName = "general", icon = icon("wrench")),
         menuSubItem("Land Cover", tabName = "inp_landcover", icon = icon("layer-group")),
         menuSubItem("Spatial Data", tabName = "inp_spatial", icon = icon("earth-asia")),
         menuSubItem("Biophysics by Land Cover", tabName = "inp_biophysic_lc", icon = icon("seedling")),
         menuSubItem("Biophysics by Livelihood", tabName = "inp_biophysic_ll", icon = icon("seedling")),
         menuSubItem("Economics by Land Cover", tabName = "inp_economic_lc", icon = icon("coins")),
         menuSubItem("Economics by Livelihood", tabName = "inp_economic_ll", icon = icon("coins")),
         menuSubItem("Socio-Cultural", tabName = "inp_socio_ll", icon = icon("users")),
         menuSubItem("Others", tabName = "inp_other", icon = icon("landmark")),
         menuSubItem("Scenario", tabName = "inp_scenario", icon = icon("diagram-project")),
         menuSubItem("Checklist Summary", tabName = "inp_summary", icon = icon("check-to-slot"))),
      menuItem("Run Simulation", tabName = "run_tab", icon = icon("play")),
      menuItem("User Manual", tabName = "manual_tab", icon = icon("book")),
      menuItem("About", tabName = "about_tab", icon = icon("circle-info"))),
    fixedPanel(bottom = 0, left = 0, width = "200px", 
               div(HTML("&copy; World Agroforestry (ICRAF)"), 
                   style = "text-align:center; color:#5A6E2E"))
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .drop_box {
        border-radius: 4px;
        border: 2px dashed LightGray;
        padding: 2px;
      }
      
      .delete_box {
        border-radius: 4px;
        border: 2px dashed red;
        margin: 30px 5px 5px;
        padding: 5px;
        background-color: #FEF2F1; 
        display: inline-block;
      }
      
      .circle {
        font-size: 90px;
        text-align: center;
        font-weight: bold;
        line-height: 100px;
        color:#FFF;
        background:#C6E0B3;
        border-radius:50px;
        width:100px;
      }
      
      hr.green {
        border-top: 3px solid #C6E0B3;
      }
      
      .greenbox {
        background:#C6E0B3; 
        margin:20px 0px; 
        padding:10px;
        border-radius:5px;
      }
    "))),

    tags$script(
      "function dbFunction(x) {
        Shiny.setInputValue('double_clicked', x.id, {priority: 'event'});
      }"
    ),
    
    useShinyjs(),
    use_theme(mytheme),
    # setShadow(class = "box"),
    
    tags$script(src="jexcel.js"),
    tags$link(rel="stylesheet", href="jexcel.css", type="text/css"),
    tags$script(src="jsuites.js"),
    tags$link(rel="stylesheet", href="jsuites.css", type="text/css"),
    tags$link(rel="stylesheet", type = "text/css", href = "table.css"),

    tabItems(
      tabItem(tabName = "home",
              h3(HTML("<b class='h1' >F</b>orest, <b class='h1'>A</b>groforest, 
                      <b class='h1' >L</b>ow-value <b class='h1'>L</b>and
                      <b class='h1'>O</b>r <b class='h1' >W</b>asteland?")),
              hr(class = "green"),
              img(src = "images/front_cover.png", width = "100%"), tags$br(), tags$br(),
              includeMarkdown("home.md")
      ),
      tabItem(tabName = "general", h2("Parameter Input Options"),
              hr(class = "green"),
        fluidRow(      
          column(2, div("1", class = "circle")),      
          column(10,  h3(icon("file", style = "margin: 10px;"), "New Parameter"),
            HTML("<p>The parameterization should be started by the initialization of
                 <b>Land Cover</b> map and definition</p>"),
            actionButton("start_button", "Go to Land Cover initialization...")) 
        ),

        hr(class = "green"),
        fluidRow(      
          column(2, div("2", class = "circle")),
          column(10, h3(icon("folder-open", style = "margin: 10px;"), "Load parameter"),
                 HTML("Upload the <b>saved parameters</b> files (.zip)"),
            fileInput("upload_parameter", NULL, accept = ".zip")),
        ),
        hr(class = "green"),
        fluidRow(      
          column(2, div("3", class = "circle")),
          column(10, h3(icon("file-import", style = "margin: 10px;"), "Import parameter"),
                 HTML("Import paramaters from the <b>FALLOW-PCRaster</b> version (.zip)"),
            fileInput("import_parameter", NULL, accept = ".zip"),
            uiOutput("loadedParams"))
        ),
        hr(class = "green"),
        progressBar(id = "progress_input", value = 0, status = "warning", title = "")
      ),
      
      ################################################################
      #### LAND COVER INPUT ##########################################
      ################################################################
      
      tabItem(
        tabName = "inp_landcover", 
        h2("Land Cover, Land Use and Livelihood Setting"),
        div(class = "greenbox",
            HTML("<h4>Upload the initial '<b>Land Cover</b>' map</h4>"),
            fileInput("upload_initlc", NULL, accept = ".tif")),
        fluidRow(
          box(id = "box_map_display", title = "Map display", collapsible = TRUE, width = 12, collapsed = F, 
              dropdownMenu = boxDropdown(icon = icon("ellipsis-vertical"),
                boxDropdownItem(id = "refresh_lcmap", "Refresh map", icon = icon("arrows-rotate"))),
              sidebar = boxSidebar(
                id = "inp_initlc_sidebar",
                icon = icon("chart-simple"),
                background = COLOR_DARK,
                uiOutput("out_tot_area"),
                plotOutput("plot_lc_area", height = "300px")
              ),
              uiOutput("inp_initlc_display")
          ),
          box(id = "box_lc_table",title = "Land Cover", collapsible = TRUE, width = 12, collapsed = F,
              dropdownMenu = boxDropdown(icon = icon("ellipsis-vertical"),
                 boxDropdownItem(id = "load_lc", "Load land cover setting...",
                                 icon = icon("folder-open")),
                 dropdownDivider(),
                 boxDropdownItem(id = "import_lc", "Import land cover parameter...",
                                 icon = icon("file-import")),
                 boxDropdownItem(id = "import_pal", "Import color palette...",
                                 icon = icon("palette")),
                 dropdownDivider(),
                 boxDropdownItem(downloadButton("download_lc", "Save land cover setting"))
              ),
              excelOutput("inp_initlc_list", height = "100%")
          ),
          box(id = "box_lu_table", title = "Land Use", collapsible = T,  width = 7, collapsed = T,
              dropdownMenu = boxDropdown(icon = icon("ellipsis-vertical"),
                boxDropdownItem(id = "load_saved_lu_palette", "Load saved land use palette...", 
                                icon = icon("folder-open")),
                 boxDropdownItem(id = "import_lu_palette", "Import land use palette...",
                                 icon = icon("file-import")), 
                dropdownDivider(),
                 boxDropdownItem(downloadButton("download_lu", "Save land use palette"))
              ),
              excelOutput("inp_lu", height = "100%")
          ),
          box(id = "box_ll_table", title = "Livelihood Types", collapsible = T,  
              width = 5, collapsed = T, excelOutput("inp_ll", height = "100%")
          )
      )),
      
      ################################################################
      #### SPATIAL DATA INPUT ########################################
      ################################################################
      
      tabItem(
        tabName = "inp_spatial", h2("Spatial Data Input"),
        div(class = "greenbox",  
          h4("Upload the spatial map files"),      
          tags$i("* you may upload multiple files at once, or in a compressed zip file"),
          fileInput("upload_spatial", NULL, 
                    accept = c("image/tiff", "application/zip", ""), multiple = T)
        ),
        fluidRow(
           apply(map_input_df, 1, function(x){
             box(title = x["box_title"], width = 8, collapsible = T, collapsed = T,
                 dropdownMenu = boxDropdown(icon = icon("ellipsis-vertical"),
                    boxDropdownItem(id = paste0("load_", x["box_id"], "_map"),
                                    paste("Load", x["box_id"], "map configuration"), 
                                    icon = icon("folder-open")),
                    boxDropdownItem(
                      downloadButton(paste0("download_", x["box_id"], "_map"),
                                     paste("Save", x["box_id"], "map configuration")))
                 ),
                 uiOutput(paste0(x["box_id"], "_maps"))
             )
           })
        ),
        fixedPanel(top = 310, right = 0, width = "27%", draggable = T,
                   box(title = "Uploaded maps", width = 12, uiOutput("map_upload_box"))),
        fixedPanel(bottom = 0, width = "600px", uiOutput("map_display"))
      ),

      ################################################################
      #### SCALAR INPUT ##############################################
      ################################################################
      
      tabItem(tabName = "inp_biophysic_lc", do.call(create_scalar_tab, tab_scalar_df[1,])),
      tabItem(tabName = "inp_biophysic_ll", do.call(create_scalar_tab, tab_scalar_df[2,])),
      tabItem(tabName = "inp_economic_lc", do.call(create_scalar_tab, tab_scalar_df[3,])),
      tabItem(tabName = "inp_economic_ll", do.call(create_scalar_tab, tab_scalar_df[4,])),
      tabItem(tabName = "inp_socio_ll", do.call(create_scalar_tab, tab_scalar_df[5,])),
      
      tabItem(tabName = "inp_other", h2("Other Parameters Setting"), 
              hr(class = "green"),
        fluidRow(
          apply(other_inp_df, 1, function(x){
            box(title = x["title"], collapsible = TRUE, 
                width = as.numeric(x["width"]), collapsed = T,
                dropdownMenu = boxDropdown(
                  icon = icon("ellipsis-vertical"),
                  boxDropdownItem(id = paste0("load_", x["id"]), 
                                  paste("Load", x["title"]),
                                  icon = icon("folder-open")),
                  boxDropdownItem(downloadButton(paste0("download_", x["id"]),
                                                 paste("Save", x["title"])))
                ),
                uiOutput(paste0("out_", x["id"]))
            )
          })
        )
      ),
      
      tabItem(tabName = "inp_scenario", h2("Scenario Setting"), 
              hr(class = "green"),
              tags$i("Under development..."), uiOutput("out_scenario")),
      
      tabItem(tabName = "inp_summary", h2("Parameter Checklist Summary"), 
              hr(class = "green"),
              uiOutput("out_summary"),
              div(class = "greenbox", style = "text-align:center;",
                tags$b( #icon("floppy-disk"), 
                 "Save all parameters in zip file:"),
              downloadButton("download_params", "Download the parameters"))
              ),
      
      ###############################################################
      ### RUN #######################################################
      ###############################################################
      
      tabItem(tabName = "run_tab", h2("Run Simulation"), hr(class = "green"),
          fluidRow(
            column(4, numericInput("simtime", "Simulation time (years)", 
                                   value = 30)),
            column(3, disabled(
              actionBttn("run_button", "Run", icon = icon("play"),
                         style="unite", color = "danger")),
              style="margin-top: 10px;")
          ),
          progressBar(id = "progress_iteration", value = 0, status = "warning",
                      title = "Iteration (year)", total = 30),
          progressBar(id = "progress_detail", value = 0, status = "danger",
                      title = "Progress detail"),
          uiOutput("output_selector"), 
          fluidRow(id ="add_output")
      ),
      
      tabItem(tabName = "manual_tab", h2("User Manual"), hr(class = "green"),
              includeMarkdown("rfallow_manual.md")),
      
      tabItem(tabName = "about_tab", h2("About"), hr(class = "green"),
              includeMarkdown("about.md"))
      
    ))

)
