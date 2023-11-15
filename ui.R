# #Base GUI
# library(shiny)
# library(shinyjs)
# # library(shinydashboard)
# # library(shinydashboardPlus)
# library(bs4Dash)
# library(shinyWidgets)
# #Extra GUI
# library(shinyjs)
# library(shinyjqui) #drag n drop
# library(openxlsx2) #xls IO
# library(RColorBrewer)
# library(areaplot)
# library(fresh) #color theme
# library(excelR) #table UI
# library(markdown)
# 
# library(thematic)
# 
# #Map
# library(stars)
# library(mapview)
# library(leaflet)
# library(leafem)
# #Utility
# library(dplyr)
# library(reshape)
# library(yaml)


thematic_shiny(bg = NA)

theme <- create_theme(
  bs4dash_vars(
    gray_100 = "#E1EED7",
    gray_200 = "#CFE5C1",
    gray_300 = "#B4D69D",
    gray_400 = "#A5CE8A",
    gray_500 = "#89BF65",
    gray_600 = "#689F43",
    gray_700 = "#4A712F",
    gray_800 = "#334F21",
    gray_900 = "#253918",
    gray_x_light = "#385624",
    blue = "#5E913D"
    
  ),
  bs4dash_layout(
    main_bg = "#F0F7EB"
  ),
 
  bs4dash_sidebar_light(
    bg = "#D2E6C4",
    hover_bg = "#96C676",
    color = "#385624",
    hover_color = "#000",
    submenu_bg = "#C3DEB1",
    submenu_color = "#385624",
    submenu_hover_bg = "#FFF",
    submenu_active_color = "#000",
    submenu_active_bg = "#F0F7EB"
  ),
  
  bs4dash_sidebar_dark(
    bg = "#1C2B12",
    # hover_bg = NULL,
    # color = NULL,
    # hover_color = NULL,
    # active_color = NULL,
    # submenu_bg = NULL,
    # submenu_color = NULL,
    # submenu_hover_color = NULL,
    # submenu_hover_bg = NULL,
    # submenu_active_color = NULL,
    submenu_active_bg = "#D2E6C4"
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

jsCode <- "shinyjs.getDim = function(id){
  const elem = document.getElementById(id);
  var el = $('#' + id);
  let width = elem.clientWidth;
  let height = elem.clientHeight;
  Shiny.onInputChange('el_height', height);
  Shiny.onInputChange('el_width', width);
  let node = elem.nodeName;
  let nodeID = elem.id;
  let h = '90vh';
  Shiny.onInputChange('dim', {id, width, height, node, nodeID, h});
}

shinyjs.setMin = function(id){
  let h = 'height';
  Shiny.setInputValue(h.concat(id), '400px');
}

shinyjs.setMax = function(id){
   let h = 'height';
   Shiny.setInputValue(h.concat(id), '90vh');
}
  
"

ui <- dashboardPage(
  freshTheme = theme,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = tags$b("R-Fallow"),
      image = "images/rfallow_logo.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    skin = "light",
    sidebarMenu(
      id = "sidemenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Input Parameters", tabName = "parameters", icon = icon("sliders"), 
               startExpanded = TRUE,
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
      menuItem("About", tabName = "about_tab", icon = icon("circle-info")))
    # fixedPanel(bottom = 0, left = 0, width = "200px", 
    #            div(HTML("&copy; World Agroforestry (ICRAF)"), 
    #                style = "text-align:center; color:#5A6E2E"))
  ),
  
  body = dashboardBody(
    useShinyjs(),
    # extendShinyjs(text = jsCode, functions = c("setMax", "setMin")),#, "setMin"
    # extendShinyjs(text = jsCode, functions = c("getDim", "setMin", "setMax")),#, "setMin"
    
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
        background-color: #FEF2F140; 
        display: inline-block;
      }
      

      
      hr.green {
        border-top: 3px solid #C6E0B350;
      }
      
      .greenbox {
        background:#72A12130; 
        margin:10px 0px; 
        padding:10px;
        border-radius:5px;
      }
      
      .dark-mode .content-wrapper {
          background-color: #121C0C;
          color: #61953E; 
      }
      
      .navbar-light {
          background-color: #E1EFD8 !important;
      }
      
      .navbar-dark {
          background-color: #18250F !important;
      }
      
      .bg-dark {
          background-color: #121C0C !important;
      }

        
    "))),

    tags$script(
      "function dbFunction(x) {
        Shiny.setInputValue('double_clicked', x.id, {priority: 'event'});
      }"
    ),
    

        
    tags$script(src="jexcel.js"),
    tags$link(rel="stylesheet", href="jexcel.css", type="text/css"),
    tags$script(src="jsuites.js"),
    tags$link(rel="stylesheet", href="jsuites.css", type="text/css"),
    tags$link(rel="stylesheet", type = "text/css", href = "table.css"),

    tabItems(
      tabItem(tabName = "home",
              h3(img(src = "images/rfallow_logo.png", width = "50px"), HTML("<b class='h1' >F</b>orest, <b class='h1'>A</b>groforest, 
                      <b class='h1' >L</b>ow-value <b class='h1'>L</b>and
                      <b class='h1'>O</b>r <b class='h1' >W</b>asteland?")),
              hr(class = "green"),
              img(src = "images/front_cover.png", width = "100%"), tags$br(), tags$br(),
              includeMarkdown("home.md")
      ),
      tabItem(tabName = "general", h2("Parameter Input Options"),
              hr(class = "green"),
        fluidRow(     
          column(2, icon("file"), style = "margin: auto;font-size: 40px; text-align: center;"),      
          column(7,  h4("New parameter"),
            HTML("The parameterization should be started by the initialization of
                 <b>Land Cover</b> map and class definition")),
          column(3, style = "margin: auto;",
            actionButton("start_button", "Go to Land Cover initialization...")) 
        ),

        hr(class = "green"),
        fluidRow(      
          column(2, icon("folder-open"), style = "margin: auto; font-size: 40px; text-align: center;"),
          column(5, h4("Load parameter"), 
                 HTML("Upload the <b>saved parameters</b> files (.zip)")),
          column(5, fileInput("upload_parameter", NULL, accept = ".zip")),
        ),
        hr(class = "green"),
        fluidRow(      
          column(2, icon("file-import"), style = "margin: auto; font-size: 40px; text-align: center;"),
          column(5, h4("Import parameter"),
                 HTML("Import paramaters from the <b>FALLOW-PCRaster</b> version (.zip)")),
          column(5, fileInput("import_parameter", NULL, accept = ".zip"),
            uiOutput("loadedParams"))
        ),
        hr(class = "green"),
        fluidRow(      
          column(2, icon("earth-asia"), style = "margin: auto; font-size: 40px; text-align: center;"),
          column(10, h4("Example parameters"),
                 uiOutput("example_parameters"))
        ),
        hr(class = "green"),
        shinyWidgets::progressBar(id = "progress_input", value = 0, status = "warning", title = "")
        
        
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
          box(id = "box_map_display", title = "Land cover map", collapsible = TRUE, height = "400px",
              width = 6, collapsed = F, maximizable = T, 
              dropdownMenu = boxDropdown(icon = icon("ellipsis-vertical"),
                boxDropdownItem(id = "refresh_lcmap", "Refresh map", icon = icon("arrows-rotate"))),
              # sidebar = boxSidebar(
              #   id = "inp_initlc_sidebar",
              #   icon = icon("chart-simple"),
              #   background = COLOR_DARK,
              #   uiOutput("out_tot_area"),
              #   plotOutput("plot_lc_area", height = "300px")
              # ),
              uiOutput("inp_initlc_display")
          ),
          box(id = "box_lc_chart", title = "Total area", collapsible = TRUE, height = "400px",
              width = 6, collapsed = F, 
              uiOutput("out_tot_area"),
              plotOutput("plot_lc_area")

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
        fixedPanel(top = 290, right = 15, width = "25%", draggable = T,
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
          shinyWidgets::progressBar(id = "progress_iteration", value = 0, status = "warning",
                      title = "Iteration (year)", total = 30),
          shinyWidgets::progressBar(id = "progress_detail", value = 0, status = "danger",
                      title = "Progress detail"),

          uiOutput("output_selector"), 
          # boxLayout(
          #   type = "deck", sortable(div(id ="add_output")))
          fluidRow(id ="add_output"
            # sortable(div(id ="add_output"), width = 6)
          )
      ),
      
      tabItem(tabName = "manual_tab", h2("User Manual"), hr(class = "green"),
              includeMarkdown("rfallow_manual.md")),
      
      tabItem(tabName = "about_tab", h2("About"), hr(class = "green"),
              includeMarkdown("about.md"))
      
    ))

)
