
try(source("2.libraries.r", encoding = "UTF-8"))
print(getwd())
#print(list.files(getwd()))
print(list.files('/var/lib/1.Data/'))
convertMenuItem <- function(mi,tabName) {
  # mi <<- mi
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# 2. UI                                 ####
ui <- bs4DashPage(
  dark = NULL,
  # 2.1. Header                         ####
  header = bs4Dash::bs4DashNavbar(
    tags$head(
      includeCSS("www/CSS.css")
    ),
    tags$style(HTML(
      "<script>
           $( document ).ready(function() {
              $(\".tab-content [type='checkbox']\").on('click', function(){
                var is_checked = this.checked;
                if(typeof is_checked === 'boolean' && is_checked === true){
                  setTimeout(function() {
                    window.scrollTo(0,document.body.scrollHeight);
                  }, 200)
                }
              })
           })
        </script>")),
    status = 'light',
    controlbarIcon = shiny::icon("info"),
    textOutput("texttitle")
    #Не удаляйте пж
    # bs4Dash::actionButton("help_button", "Справка по данному разделу", icon = icon("info"),status = "info", flat = TRUE)
  ),#,status = "info"
  # 2.2. Sidebar                        ####
  sidebar = bs4DashSidebar(
    minified = FALSE,
    width = "300px",
    
    imageOutput("logo", height = "70px"),
    bs4SidebarMenu(
      id = "sbMenu",
      tags$style(HTML('.sidebar{font-family: Panton;
                                 color: black}
                        a:hover{ background: #191a1a }
                        }
                      ')),
      # 2.2.1. Menu                     ####
      uiOutput("ui_output0"),
      bs4SidebarMenuItem("Мониторинг",
                         selectizeInput('indicator', label = "Выберите индикатор оси Y", choices = indicator_input,
                                        options = list(create = TRUE), multiple = TRUE, selected = indicator_input[1]),
                         materialSwitch("test_check", "Изменить ось X", value = FALSE,status = "warning"),
                         selectizeInput(inputId = "indicator_second_indicator",
                                        label = "Выберите индикатор оси X",
                                        choices = indicator_input,
                                        options = list(create = TRUE), selected = indicator_input[2], multiple = FALSE),
                         materialSwitch("tf_vis", "2,5-D карта", value = FALSE,status = "warning"),
                         materialSwitch("norm", "Нормировать", value = FALSE,status = "warning"),
                         icon = icon("project-diagram"), tabName = 'monitoring'),
      bs4SidebarMenuItem("Оценка влияния",
                         selectizeInput('foo', label = "Выберите город",
                                        choices = location_inp_2,
                                        options = list(create = TRUE),
                                        selected = location_inp_2[1]),
                         materialSwitch("all_citys_check", "Показать все города", value = FALSE, status = "warning"),
                         selectizeInput('fo', label = "Выберите целевой индикатор",
                                        choices = indicator_inp_2,
                                        options = list(create = TRUE),
                                        selected = indicator_inp_2[1]),
                         materialSwitch("test_check2", "Выбрать индикатор оси X", value = FALSE, status = "warning"),
                         selectizeInput(inputId = "indicator_second_indicator2",
                                        label = "Выберите индикатор оси X",
                                        choices = indicator_inp_2,
                                        options = list(create = TRUE), selected = indicator_inp_2[3], multiple = FALSE),
                         selectizeInput('f', label = "Выберите управляемый индикатор",
                                        choices = indicator_inp_2,
                                        options = list(create = TRUE),
                                        selected = indicator_inp_2[2]),
                         icon = icon("project-diagram"), tabName = 'influense'),
      shinyjs::hidden(bs4SidebarMenuItem("hiddenCharts", tabName = "monitoring")),
      shinyjs::hidden(bs4SidebarMenuItem("hiddenCharts2", tabName = "influense"))
    )
  ),
  # 2.3. Controlbar (left)              ####
  controlbar = bs4Dash::bs4DashControlbar(
    tags$ol(
      style = "font-size:12px;",
      tags$h5 (tags$b("Ссылки", style = "margin-top: 15px; margin-bottom: 15px;")),
      tags$li( a(href = "https://the-digital-twin.com/", target = "_blank", "Digital Twin's website")),  # tags$li(
      tags$li( a(href = "https://teb.dtwin.city/"      , target = "_blank", "Energy Equilibrium")),  # tags$li(
      tags$li( a(href = "https://esg.dtwin.city/"     , target = "_blank", "Influence of ESG on the city")),  # tags$li(
      tags$li( a(href = "https://asset.dtwin.city/"   , target = "_blank", "Enterprise management")),  # tags$li(
      tags$li( a(href = "https://energy.dtwin.city/"   , target = "_blank", "International energetics")),  # tags$li(
      tags$li( a(href = "https://plan.dtwin.city/"   , target = "_blank", "Strategic Planning Platform")),  # tags$li(
      tags$li( a(href = "https://www.linkedin.com/company/dtwin/mycompany/" , target = "_blank", "Linkedin developers"))  # tags$li(
    )
  ),
  
  # 2.4. Body                           ####
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    use_theme(create_theme(
      bs4dash_status(light = "#787878"),
      bs4dash_status(light = "#1c1c1c", primary = "#FFA500"),
      bs4dash_vars(
        navbar_light_color = "#787878",
        navbar_light_active_color = "white",
        navbar_light_hover_color = "white"
      ),
      bs4dash_sidebar_light(
        bg = "white",
        color = '#1c1c1c',
        hover_color  = '#787878',
        submenu_active_bg  = '#787878'
          # header_color = 'black'
      ),
      bs4dash_sidebar_dark(
        bg = "white",
        color = '#1c1c1c',
        hover_color  = '#787878',
        submenu_active_bg   = '#787878'
          # header_color = 'black'
      ),
      bs4dash_layout(
        sidebar_width = "340px",
        main_bg = "#1c1c1c"
      ),
      bs4dash_color(
        gray_900 = "#FFF"
      )
    )),
    bs4TabItems(
      bs4TabItem(tabName = "monitoring",uiOutput("ui_0")),
      bs4TabItem(tabName = "influense",uiOutput("ui_1"))
    ),
    tags$style(HTML('*{font-family: Panton}')),
    uiOutput("ui_output")
  )
)
