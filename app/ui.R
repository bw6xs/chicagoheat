
# Define the user interface for the application

shinyUI(
  fluidPage(

    list(tags$head(HTML('<link rel="icon", href="logo.png",
                        type="image/png" />'))),
    div(style="padding: 1px 1px; width: '100%'",
        titlePanel(
          title="", windowTitle=""
        )
    ),

    navbarPage(title="", 
               inverse = F,
               theme = shinytheme("lumen"), 
### 
### BEGIN CODE FOR THE HOME PAGE
### 
               tabPanel("Home Page", value="Home Page", icon = icon("home"),
                        fluidRow(column(12, h1("Chicago Heat Vulnerability", align='center'))), 
                        fluidRow(column(12, div(img(src="bean.jpg", height="150", width="200", align='center'), "", align='center'))), 
                        br(),
                        uiOutput("Component1"),
                        br(),
                        fluidRow(column(12, panel_div(class_type = "primary", panel_title = "Overview", content = HTML("<style>p {padding-left: 10pt; padding-right: 10pt; text-align: justify; text-justify: inter-word;}</style><p>In July of 1995 one of the worst weather-related disasters in the history of Illinois occurred when over a five-day period from July 12 to July 16 over 700 people died in Cook County<sup>1</sup>. The Centers for Disease Control and Prevention concluded that victims of the 1995 heat wave tended to share one or more of the following characteristics: living alone, not leaving home daily, lacking access to transportation, being sick or bedridden, not having social contacts nearby, and not having access to an air conditioner. Another notable heat wave occurred in July 1999, but death tolls were not as high as in 1995 with this reduction attributed in part to implementation of the city's Extreme Weather Operations Plan, but also to the fact that humidity levels were lower.</p><p> The motivation for developing this tool is simple&#8212;the experience of Chicago in the 1990s notwithstanding, heat waves are becoming more frequent and more deadly<sup>2</sup>. In 2003 there were over 14,000 heat-related deaths in France and over 2,300 people died in India in the spring of 2015. Storms, floods, and sea level rise often spring to mind when we think about climate change impacts, but the reality is that urban planners need to pay more attention to extreme heat events and the health impacts of rising temperatures in urban areas. This has implications not just for <strong>land use planning</strong> and the <strong>built environment</strong>, but also requires a deeper understanding of vulnerability and how the distribution of <strong>vulnerable populations</strong> in space changes over time.</p><p>  Vulnerability is typically understood as a function of: (1) sensitivity, (2) adaptive capacity, and (3) exposure<sup>3</sup>. Chicago Heat Vulnerability Mapper separates <strong>sensitivity</strong> and <strong>adaptive capacity</strong> from <strong>exposure</strong> to more easily show the location of the most vulnerable populations as well as which areas of the Chicago region <sup>4</sup> are the hottest and how these patterns change over time. The <strong>Heat Vulnerability Index (HVI)</strong> is based on data collected by the U.S. Census Bureau and measures where residents of the Chicago region with <strong>high sensitivity</strong> to heat and <strong>low adapative capacity</strong> to heat are located. The methodology for calculating the HVI is adapted from previous studies and begins with the 16 variables shown below, each of which captures one or more aspects of sensitivity or adaptive capacity with respect to heat. Because the boundaries of Census tracts change over time, the Geolytics Neighborhood Change Database, which reconciles those boundaries using a weighting approach documented <a href='http://geolytics.com/USCensus,Neighborhood-Change-Database-1970-2000,Data,Geography,Products.asp' target='_new'>here</a>. There are 1,977 census tracts and seven counties in the Chicago region. </p><div align='center'><img src='inputvars.png', width = '600', height = '300'></div><br><p>A statistical technique called factor analysis was used to reduce the original 16 variables to four synthetic variables and the resulting scores were coded using the following scheme to derive the Heat Vulnerability Index:</p><BR><div align='center'><img src='coding.jpg', width = '300', height = '200'></div><BR><p> Using standard deviations above and below the mean to derive the HVI helps to mitigate the impacts of very large or very small values, better accommodates the warming trend observable over time in the climate data for the region, and helps to facilitate 'apples-to-apples' comparisons of HVI values. In every case, <strong>higher HVI values</strong> are consistent with <strong>increased vulnerability</strong> (higher sensitivity + lower adaptive capacity) to heat.</p><p>The exposure component is represented using estimates of land surface temperature<sup>5</sup> and the National Weather Service's <a href='http://www.nws.noaa.gov/om/heat/heat_index.shtml' target='_new'> heat index</a>.</p>"))), align='center'), 
                        fluidRow(
                          column(12, panel_div(class_type = "primary", panel_title = "Directions",
                                              content = HTML("<style>p {padding-left: 30pt; padding-right: 30pt; text-align: justify; text-justify: inter-word;}</style><p>Click the buttons at the top of this page to access visualization tools designed for exploring the intersection of <strong>high sensitivity + low adaptive capacity</strong> to heat with exposure to heat over time in the Chicago region. The 'Surface Temperatures' tab shows the distribution of the Heat Vulnerability Index alongside estimates of maximum temperatures for June, July, August, as well as the entire summer. It also includes five key risk factors as a complement to the HVI: (1) age and social isolation (i.e., elderly single-person households), (2) poverty, (3) linguistic isolation, (4) disability status, and (5) older housing stock (i.e., less likely to have air conditioning) for 2013. The 'Vulnerable Populations' tab allows users to map statistically significant clusters of high and low values of the Heat Vulnerability Index in 1990, 2000, and 2010 in addition to the key risk factors for 2013. The 'Past Heat Events' tab presents an interpolated estimate of the National Weather Service <a href='http://www.nws.noaa.gov/om/heat/heat_index.shtml' target='_new'>heat index</a> at the time of historical heat events for which fatalities were recorded in the Chicago region. Finally, the 'Future Scenarios' tab allows users to visualize climate model predictions of maximum daily temperatures for the region in the year 2030 under different greenhouse gas <strong> concentration </strong> (not emissions) trajectories alongside extrapolated values of the Heat Vulnerability Index, consistent with the trend observed over the past two decades.</p>")))),
                        fluidRow(
                            column(12, panel_div("primary", panel_title = "Application Developers", 
                                    content = HTML("<style>p {padding-left: 50pt; padding-right: 50pt;}</style><p>Prof. Bev Wilson: <a href='mailto:bw6xs@virginia.edu?Subject=Chicago%20Heat%20Vulnerability%20Mapper%20Help' target='_top'>Bev Wilson</a><br><br> &#13Prof. Arnab Chakraborty: <a href='mailto:arnab@illinois.edu?Subject=Chicago%20Heat%20Vulnerability%20Mapper%20Help' target='_top'>Arnab Chakraborty<br>&#13</a></p>")))),
                            
                         br(),
                       fluidRow(column(4, ""), column(4, tags$img(src='logos.png', align='center', height="100", width="175"), align='center'), column(4, "")),
                       # br(),
                       # fluidRow(column(4, ""), column(4, tags$img(src='faa.png', height="85", width="149"), align='center'), column(4, "")
                       #           ),
                      br(),
                      hr(),
                      fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p><sup>1</sup> Klinenberg, E. (2002). Heat wave: A social autopsy of disaster in Chicago. Chicago, IL: University of Chicago Press.</p><p><sup>2</sup> Habeeb, D., Vargo, J., & Stone Jr, B. (2015). Rising heat wave trends in large U.S. cities. Natural Hazards, 76(3), 1651-1665.</p><p><sup>3</sup> Wilhelmi, O.V., & Hayden, M.H. (2010). Connecting people and place: A new framework for reducing urban vulnerability to extreme heat. Environmental Research Letters, 5(1), 1-7.</p><p><sup>4</sup> The Chicago region is defined here as Cook, DuPage, Kane, Kendall, Lake, McHenry, and Will counties in Illinois.</p> <p><sup>5</sup> Thornton, P.E., Thornton, M.M., Mayer, B.W., Wilhelmi, N., Wei, Y., Devarakonda, R., & Cook, R.B. (2014). Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 2. Data set. Available on-line [http://daac.ornl.gov] from Oak Ridge National Laboratory Distributed Active Archive Center, Oak Ridge, Tennessee, USA.</p>")))),
                       hr(),
                       fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Copyright &copy; 2017 University of Illinois at Urbana-Champaign. <b>Chicago Heat Vulnerability Mapper</b> is powered by R and based upon work supported in the Lincoln Institute of Land Policy under Grant #UBW073015. Any opinions, findings, and conclusions or recommendations expressed are those of the author(s) and do not necessarily reflect the views of the Lincoln Institute.</p>")))),
                       br()
               ),
### 
### BEGIN CODE FOR THE SURFACE TEMPERATURES PAGE
### 
               tabPanel("Surface Temperatures", value="Surface Temperatures", icon = icon("fas fa-sun", tags$style('.tab { margin-left: 40px; }')),
                        h1("Where Does Vulnerability to Heat Intersect Exposure to Heat?", align = "center"),
                        br(),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(
### CHANGE DEFAULT COLOR OF SIDEBAR PANEL
#                           tags$style('.well {background-color: #E95420; color: #FFFFFF;}'),
                           tags$style('.well {background-color: #158cba;; color: #FFFFFF;}'),
                           tags$style('.p {padding-left: 10pt;   padding-right: 10pt;}'),
#                           helpText("Select A Year", style="font-size: 125%; color: #FFFFFF;"),
                           br(),
                           selectInput("index", "Heat Vulnerability Index Year", 
                                        choices = c("1990" = "1990", "2000" = "2000", "2010" = "2010"), selected="1990"),
                           selectInput("var", "Specific Month Or Entire Summer?",
                                        choices = c("Entire Summer" = "summer", "Specific Month" = "monthly"), selected="summer"),
                           conditionalPanel(
                              condition = "input.var == 'monthly'",
                              selectInput("chosenMonth", "Month", 
                                          choices = c("June" = "june", "July" = "july", "August" = "august"))),
                           br(),
                           br(),
                           checkboxInput("checkbox_2013_data", "Explore Individual Risk Factors for 2013", value = FALSE),
                           conditionalPanel(
                                condition = "input.checkbox_2013_data == true",
                           br(),
                           selectInput("rf_2013", "Risk Factor", 
                                choices = c("Disability" = "disability", "Elderly Living Alone" = "oldalone", "Limited English" = "language", "Older Housing" = "oldhousing", "Poverty" = "poverty"))),
                           br(),
                           br(),
                           tags$div(actionButton("intersect", "Show Intersection"), align='center'),
                           br(),
                           br()
                          ),
                          mainPanel(
                            leafletOutput("stmap", width = "800", height = "800"),
                            br())
                        ),
                          br(),
                          hr(),
                          HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>This tab shows the derived Heat Vulnerability Index for 1990, 2000, and 2010 for Census tracts within the Chicago region. As a complement to the HVI, data layers showing individual risk factors such as age, poverty, linguistic isolation, disability status, and older housing stock (i.e., less likely to have air conditioning) are also avilable from the American Community Survey. Because the ACS data are collected using a rolling samples methodology<sup>1</sup>, these layers most closely capture conditions as of 2013.</p><p>The temperature layers available here are derived from the <a href='https://daymet.ornl.gov/overview.html'>Daymet dataset</a> maintained by Oak Ridge National Laboratory. These data are interpolated from weather station data<sup>2</sup> at a 1 square kilometer spatial resolution with daily, monthly, and annual estimates of climatological variables like maximum temperature available in netCDF format for all of North America. Using guidelines<sup>3</sup> provided by the U.S. Environmental Protection Agency for distinguishing extreme heat, grid cells were extracted from the Daymet layers where the estimated maximum temperature exceeded the 95th percentile for the time period under consideration and these areas of extreme heat exposure are available in this part of <strong>Chicago Heat Vulnerability Mapper</strong>.</p><p>Three different basemap options are available: (1) OpenStreetMap, (2) CartoDB, and (3) ESRI World Imagery. These basemaps help to orient users of <strong>Chicago Heat Vulnerability Mapper</strong> and show the location of landmarks like cities and towns, roads, and high resolution satellite imagery.</p>"),
                          br(),
                          fluidRow(column(4, ""), column(4, tags$img(src='logos.png', align='center', height="100", width="175"), align='center'), column(4, "")),
                          br(),
                          hr(),
                          fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p><sup>1</sup> Nesse K., & Rahe M.L. (2015). Conflicts in the use of the ACS by federal agencies between statutory requirements and survey methodology. Population Research and Policy Review, 34(4), 461-480.</p><p><sup>2</sup> Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 204-251.</p><p><sup>3</sup> U.S. EPA. (2006). Excessive Heat Events Guidebook, EPA 430-B-06-005. Washington, DC: U.S. EPA, Office of Atmospheric Programs.</p>
                                                              ")))),
                          hr(),
                          fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Copyright &copy; 2017 University of Illinois at Urbana-Champaign. <b>Chicago Heat Vulnerability Mapper</b> is powered by R and based upon work supported in the Lincoln Institute of Land Policy under Grant #UBW073015. Any opinions, findings, and conclusions or recommendations expressed are those of the author(s) and do not necessarily reflect the views of the Lincoln Institute.</p>")))),
                          br()
                ),
### 
### BEGIN CODE FOR THE VULNERABLE POPULATIONS PAGE
### 
               tabPanel("Vulnerable Populations", value="Vulnerable Populations", icon = icon("medkit"),
                        h1("Where Are Statistically Significant Clusters of Vulnerable Populations?", align = "center"),
                        br(),
                        hr(),
                        fluidPage(
                          fluidRow(column(1, ""), column(4, leafletOutput("vpmap", width = "1200", height = "800")), column(1, "")),
                          br(),
                          br(),
                          fluidRow(column(12, h3("Explore Clustering of Individual Risk Factors in 2013"), align='center')), 
                          fluidRow(column(12, selectInput("hotspot", label = "", 
                              choices = c("Disability" = "disability", "Elderly Living Alone" = "oldalone", "Limited English" = "language", "Poverty" = "poverty")), align='center')),
                          br(),
                          fluidRow(column(12, h6("Note: There were no statistically significant clusters of older housing units at the 95 or 99 percent confidence levels."), align='center')), 
                          br(),
                          hr(),
                          fluidRow(column(12, HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Residents of the Chicago region who are most vulnerable to extreme heat are not evenly distributed in space and the clustering patterns that characterize this distribution change over time. This part of <strong>Chicago Heat Vulnerability Mapper</strong> shows statistically significant clusters of higher vulnerability using the Getis-Ord Gi* statistic<sup>1</sup>. For Census years (i.e., 1990, 2000, and 2010), the Heat Vulnerability Index is used as an input to the Getis-Ord Gi* analysis and for 2013, several important risk factors<sup>2</sup> are analyzed because the sixteen variables used to derived the HVI are not available. These include age, poverty, linguistic isolation, disability status, and older housing stock (i.e., less likely to have air conditioning). The red and pink polygons represent Census tracts that exhibit clustering of high vulnerability at the 99% and 95% confidence levels, while the dark blue and light blue polygons correspond to clusters of low vulnerability to heat at the 99% and 95% confidence levels. There were no instances of persistent high vulnerability clusters across the three index years (i.e., 1990, 2000, and 2010).</p><p>Each of the data layers made available here for 2013 are based on 2011-2015 American Community Survey 5-Year Estimates compiled by the U.S. Census Bureau. The Getis-Ord Gi* statistic was calculated in ArcGIS<sup>TM</sup> 10.5 using an inverse distance weighting approach and with the False Discovery Rate correction <sup>3</sup> applied for all data layers.</p>"))),
                          br(),
                          fluidRow(column(4, ""), column(4, tags$img(src='logos.png', align='center', height="100", width="175"), align='center'), column(4, "")),
                          hr(),
                          fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p><sup>1</sup> Getis, A., & Ord, J. K. (1992). The analysis of spatial association by use of distance statistics. Geographical Analysis, 24(3), 189-206.</p><p><sup>2</sup> Klinenberg, E. (2002). Heat wave: A social autopsy of disaster in Chicago. Chicago, IL: University of Chicago Press.</p><p><sup>3</sup> Caldas de Castro, M., & Singer, B.H. (2006). Controlling the false discovery rate: A new application to account for multiple and dependent tests in local statistics of spatial association. Geographical Analysis, 38(2), 180-208.</p>")))),
                          hr(),
                          fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Copyright &copy; 2017 University of Illinois at Urbana-Champaign. <b>Chicago Heat Vulnerability Mapper</b> is powered by R and based upon work supported in the Lincoln Institute of Land Policy under Grant #UBW073015. Any opinions, findings, and conclusions or recommendations expressed are those of the author(s) and do not necessarily reflect the views of the Lincoln Institute.</p>")))),
                          br()
                        )),
### 
### BEGIN CODE FOR THE PAST HEAT EVENTS PAGE
### 
               tabPanel("Past Heat Events", value="Past Heat Events", icon = icon("fast-backward"),
                        h1("What Were Conditions Like During Past Heat Events?", align = "center"),
                        br(),
                        hr(),
                        sidebarLayout(
### CHANGE DEFAULT COLOR OF SIDEBAR PANEL
                          mainPanel(
                            leafletOutput("pemap", width = "800", height = "800"),
                            br(),
                            div(
                            )
                          ),
                          sidebarPanel(
#                            tags$style('.well {background-color: #E95420; color: #FFFFFF;}'),
                            tags$style('.well {background-color: #158cba; color: #FFFFFF;}'),
                            tags$style('.p {padding-left: 10pt;   padding-right: 10pt;}'),
#                            selectInput("integer", "Year:", choices = valid.years, selected=character(0)), 
                            selectInput("integer", "Year:", choices = sort(unique(deaths$Year)), selected = 1995), 
                            conditionalPanel(
                              condition = "input.integer > 0",
                              selectInput("heatevent", "Heat Event:", choices = as.list(subset(deaths, deaths$Year == 1995, Date))), 
                              actionButton("button", "Add to Map")),
                            br(),
                            br(),
                            actionButton("intersect_pe", "Show Intersection")
                          )),
                            br(),
                            hr(),
                            fluidRow(column(12, HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Many of the datasets included elsewhere in <strong>Chicago Heat Vulnerability Mapper</strong> focus on surface temperature, but the heat index is a more useful indicator for studying health effects of heat waves because it takes into account humidity. The National Weather Service initiates alert procedures when the heat index is expected to exceed 105 degrees F for at least 2 consecutive days. The figure below shows the relationship between temperature and humidity as it relates to the heat index or 'how hot it really feels' at a given point in time.</p><br>
<div align='center'><img src='heatindexchart-650.jpg', width = '455', height = '280'><h6>Source: National Weather Service.</h6></div><BR><p>This part of <strong>Chicago Heat Vulnerability Mapper</strong> allows users to visualize the maximum heat index for a specified day during the summer months (i.e., June through August) since 1995 for which there were heat related deaths recorded<sup>1</sup> within the seven county<sup>2</sup> Chicago metropolitan region. From 1995 to 2016, there were 54 separate heat events for which at least one direct death was reported. As shown below, the majority of these occurred during the heat wave that began July 12, 1995. Since 2006, there have been 22 separate heat events and a total of 124 direct deaths in the seven county Chicago region.</p><div align='center'><img src='heat_deaths_1996_2016.png', width = '450', height = '400'></div><p>The heat index layers included in <strong>Chicago Heat Vulnerability Mapper</strong> are derived from temperature and humidity information (sometimes dew point) collected at Automated Surface Observing System (ASOS) weather stations in the Chicago area with data records that extend back to at least 1995. The figure below shows the location of these weather stations.</p><div align='center'><img src='asos_stations.png', width = '500', height = '500'></div><p>The heat index layers available here are derived using a simple inverse distance weighting interpolation technique<sup>3</sup>, which means that the values of the NWS heat index at each of the ASOS stations is assumed to decline in a linear fashion as one moves away from the station.</p>"))),
                            br(),
                            fluidRow(column(4, ""), column(4, tags$img(src='logos.png', align='center', height="100", width="175"), align='center'), column(4, "")),
                            hr(),
                            fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p><sup>1</sup> According to statistics compiled in the National Weather Service's <a href='https://www.ncdc.noaa.gov/stormevents/choosedates.jsp?statefips=17%2CILLINOIS' target='_new'>Storm Events Database</a>.</p><p><sup>2</sup> Includes Cook, DuPage, Kane, Kendall, Lake, McHenry, and Will counties in Illinois.</p><p><sup>3</sup> Pebesma, E.J. (2004). Multivariable geostatistics in S: The gstat package. Computers & Geosciences, 30(7), 683-691.</p>")))),
                            hr(),
                            fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Copyright &copy; 2017 University of Illinois at Urbana-Champaign. <b>Chicago Heat Vulnerability Mapper</b> is powered by R and based upon work supported in the Lincoln Institute of Land Policy under Grant #UBW073015. Any opinions, findings, and conclusions or recommendations expressed are those of the author(s) and do not necessarily reflect the views of the Lincoln Institute.</p>")))),
                            br()
                            ),

### 
### BEGIN CODE FOR THE FUTURE SCENARIOS PAGE
### 
               tabPanel("Future Scenarios", value="Future Scenarios", icon = icon("fast-forward"),
                        h1("What Might Conditions Be In The Future?", align = "center"),
                        hr(),
                        sidebarLayout(
                          ### CHANGE DEFAULT COLOR OF SIDEBAR PANEL
                          mainPanel(
                            leafletOutput("fsmap", width = "800", height = "800")),
                          sidebarPanel(
#                            tags$style('.well {background-color: #E95420; color: #FFFFFF;}'),
                            tags$style('.well {background-color: #158cba; color: #FFFFFF;}'),
                            tags$style('.p {padding-left: 10pt;   padding-right: 10pt;}'),
                            tags$style('.a {color: #FFFFFF;}'),
                            selectInput("future", "Select An RCP:", 
                                        choices = c("RCP 2.6" = "RCP 2.6", "RCP 4.5" = "RCP 4.5", "RCP 6.0" = "RCP 6.0", "RCP 8.5" = "RCP 8.5"), selected = "RCP 4.5"),
                            br(),
                            tags$div(actionButton("intersect_fs", "Show Intersection"), align='center')
                          )),
                        br(),
                        hr(),
                        fluidRow(column(12, HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>There is now consensus among climate scientists that the Earth's climate is changing and that human activities, namely greenhouse gas emissions, are the driving factor<sup>1</sup>. The combined effect of rising mean temperatures associated with global climate change and the urban heat island effect<sup>2</sup> could have dire consequences for urban residents. In fact, one recent climate model suggests that heat waves of the type that happened in 1995 will likely occur 2 to 5 times per decade by mid-century in the Chicago region<sup>3</sup>. This part of <strong>Chicago Heat Vulnerability Mapper</strong> incorporates <a href='http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/techmemo/downscaled_climate.pdf' target='_top'>downscaled climate model projections </a> produced by the CSIRO-MK3-6-0 model, which performed best in a study<sup>4</sup> that evaluated the accuracy of nine of the World Climate Research Programme's Coupled Model Intercomparison Project Phase 5 (CMIP5) models in the Chicago region.</p>

<p>In 2007 the Intergovernmental Panel on Climate Change (IPCC) requested that climate researchers develop a set of scenarios that could be used to model and evaluate the impact of policy decisions. The representative concentration pathways (RCPs) are one part of a larger scenario development initiative and allow modelers to proceed with their work while more specific socio-economic and emissions scenarios are being developed<sup>5</sup>. Greenhouse gases released into the atmosphere amplify the natural radiative forcing of the sun and contribute to rising mean temperatures. The RCPs are named for the expected heat trapping capacity of the Earth in Watts per square meter in the year 2100 given various concentrations (not emissions) of greenhouse gases in the atmosphere.  There are important differences in each of the four scenarios and for example, 'RCP 2.6 features a strong mitigation assumption, with emissions peaking in the middle of the century and then becoming negative later on' while emissions continue to rise for the entirety of the 21st Century under RCP 8.5<sup>6</sup>.</p><p>A series of videos created by NASA showing how temperature and precipitation patterns vary across these four scenarios can be accessed by clicking the links below:

<div align='center'><ul style='list-style-type: none;'><li><a href='https://vimeo.com/70669286' target=_new> CMIP5 - 21st Century Temperature and Precipitation Scenarios - RCP 2.6</a></li> <li><a href='https://vimeo.com/70670321' target=_new> CMIP5 - 21st Century Temperature and Precipitation Scenarios - RCP 4.5</a></li> <li><a href='https://vimeo.com/70673711' target=_new> CMIP5 - 21st Century Temperature and Precipitation Scenarios - RCP 6.0</a></li> <li><a href='https://vimeo.com/70674503' target=_new> CMIP5 - 21st Century Temperature and Precipitation Scenarios - RCP 8.5</a></li></ul>
</div></p>
                                                 
                                                 <p>In order to forecast what the distribution of vulnerability to heat might look like in the year 2030, a linear regression model was estimated using the HVI values for all 1,977 Census tracts in 1990 and 2000 as well as dummy variables for the individual counties as predictors of the HVI value in 2010. This simple regression model explained over 70 percent of the variation in the data and the coefficients were used to predict values of the HVI in the year 2030 (i.e., a consistent time step). These HVI values represent a 'business-as-usual' scenario for the target year and highlight an important conceptual gap in how to model and forecast the spatial distribution of vulnerability. This part of <strong>Chicago Heat Vulnerability Mapper</strong> allows users to explore what the intersection of high vulnerability (i.e., upper tercile) and high heat exposure (i.e., 95th percentile of maximum temperature for an individual month or the entire summer).</p>"))),
                        br(),
                        fluidRow(column(4, ""), column(4, tags$img(src='logos.png', align='center', height="100", width="175"), align='center'), column(4, "")),
                        hr(),
                        fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p><sup>1</sup> Carlton, J. S., Perry-Hill, R., Huber, M., & Prokopy, L. S. (2015). The climate change consensus extends beyond climate scientists. Environmental Research Letters, 10(9), 094025.</p><p><sup>2</sup> U.S. Environmental Protection Agency. (2014). Reducing Urban Heat Islands: Compendium of Strategies. Retrieved from &nbsp;https://www.epa.gov/sites/production/files/2014-06/documents/basicscompendium.pdf</p><p><sup>3</sup> Hayhoe, K., Sheridan, S., Kalkstein, L., & Greene, S. (2010). Climate change, heat waves, and mortality projections for Chicago. Journal of Great Lakes Research, 36, 65-73.</p><p><sup>4</sup> Sailor, D.J. (2014). Risks of summertime extreme thermal conditions in buildings as a result of climate change and exacerbation of urban heat islands. Building and Environment, 78, 81-88.</p><p><sup>5</sup> Van Vuuren, D. P., Edmonds, J., Kainuma, M., Riahi, K., Thomson, A., Hibbard, K., et al. (2011). The representative concentration pathways: An overview. Climatic Change, 109(1-2), 5-31.</p><p><sup>6</sup> Brekke, L., Thrasher, B. L., Maurer, E. P., & Pruitt, T. (2013). Downscaled CMIP3 and CMIP5 climate projections: release of downscaled CMIP5 climate projections, comparison with preceding information, and summary of user needs. Denver, CO: U.S. Department of the Interior, Bureau of Reclamation, Technical Service Center.</p>")))),
                        hr(),
                        fluidRow(column(12, tags$small(HTML("<style>p {padding-left: 50pt; padding-right: 50pt; text-align: justify; text-justify: inter-word;}</style><p>Copyright &copy; 2017 University of Illinois at Urbana-Champaign. <b>Chicago Heat Vulnerability Mapper</b> is powered by R and based upon work supported in the Lincoln Institute of Land Policy under Grant #UBW073015. Any opinions, findings, and conclusions or recommendations expressed are those of the author(s) and do not necessarily reflect the views of the Lincoln Institute.</p>")))),
                        br()
               )
        )
))

# END OF THE ui.R SCRIPT
