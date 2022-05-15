dashboardPage(
  dashboardHeader(
    title = "HR Dashboard"
  ),
  
  dashboardSidebar(
    collapsed = F,
    sidebarMenu(
      menuItem(
        text = "Overview",
        tabName = "Overview",
        icon = icon("globe-asia")
      ),
      menuItem(
        text = "HR Analysis",
        tabName = "HR-analysis",
        icon = icon("chart-line")
      ),
      menuItem(
        text = "Data Set",
        tabName = "Data",
        icon = icon("server")
      ),
      menuItem("Source Code", icon = icon("file-code-o"), 
               href = "https://github.com/NabiilahArdini/Eco-Status")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # TAB 1
      
      tabItem(tabName = "Overview",
              fluidPage(
                fluidRow(
                  h2(tags$b("Human Resources Data Analysis")),
                  br(),
                  div(style = "text-align:justify", 
                      p("Human Resources (HR) data can be hard to come by, 
                        and HR professionals generally lag behind with respect to analytics. 
                        Thus, this dashboard represents some of the HR data analysis based on data set from
                        fictitious company with the core data set contains names, DOBs, age, gender, marital status,
                        date of hire, reasons for termination, department, whether they are active or terminated, 
                        position title, pay rate, manager name, and performance score."),
                      p("Note that the data set isn't perfect. By design, there are some data may not be visualized due to lack of information."),
                      br())
                ),
                
                fluidRow(
                  valueBoxOutput(width = 3, outputId = "overviewAllEmp"),
                  valueBoxOutput(width = 3, outputId = "overviewActiveEmp"),
                  valueBoxOutput(width = 3, outputId = "overviewTermvEmp"),
                  valueBoxOutput(width = 3, outputId = "overviewTermcEmp")
                ),
                
                fluidRow(
                  box(width=12,
                      plotlyOutput("overviewEmpTrend"),
                      title = tags$b("Total Employee Trend by Year"))
                ),
                
                fluidRow(
                  box(
                    width=6,
                    echarts4rOutput(outputId = "overviewEmpByGender"),
                    title = tags$b("Total Employee by Gender")
                  ),
                  box(
                    width=6,
                    echarts4rOutput(outputId = "overviewEmpByMarital"),
                    title = tags$b("Total Employee by Marital Status")
                  )
                ),
                
                fluidRow(
                  box(
                    width=6,
                    echarts4rOutput(outputId = "overviewEmpByCitizen"),
                    title = tags$b("Total Employee by Citizenship")
                  ),
                  box(
                    width=6,
                    echarts4rOutput(outputId = "overviewEmpByRace"),
                    title = tags$b("Total Employee by Race")
                  )
                )
              )
      ),
      
      # TAB 2
      
      tabItem(
        tabName = "HR-analysis",
        fluidPage(
          tags$head(
            tags$style(HTML(".shiny-output-error-validation {
            color: grey;
            }"))
          ),
          
          fluidRow(
            box(
              background = "green",
              width = 4,
              height = 80,
              sliderInput(
                inputId="yearSelector",
                label="Range of Year:",
                min=min(year(hrdt_clean$DateofHire)),
                max=max(year(hrdt_clean$DateofHire)),
                value=2018,
                sep="")
            ),
            box(
              background = "green",
              width = 4,
              height = 80,
              selectInput(
                inputId = "stateSelector",
                label = "Select State :",
                choices = c("All",sort(levels(hrdt_clean$State))),
                selected = "All", 
              )
            ),
            box(
              background = "green",
              width = 4,
              height = 80,
              selectInput(
                inputId = "statusSelector",
                label = "Select Employment Status :",
                choices = c("All",sort(levels(hrdt_clean$EmploymentStatus))),
                selected = "All"
              )
            )  
          ),
          
          fluidRow(
            box(
              width=6,
              plotlyOutput(outputId = "empByDept"),
              title = tags$b("Total Employee by Department")
            ),
            box(
              width=6,
              plotlyOutput(outputId = "salByDept"),
              title = tags$b("Average Salary by Department")
            )
          ),    
          
          fluidRow(
            box(
              width=6,
              plotlyOutput(outputId = "empByGen"),
              title = tags$b("Generation Proportion by Department")
            ),
            box(
              width=6,
              plotlyOutput(outputId = "empByStage"),
              title = tags$b("Work Stage Proportion by Department")
            )
          ),
          
          fluidRow(
            box(
              height = 660,
              fluidRow(
                box(
                  background = "green",
                  width = 12,
                  height = 70,
                  radioButtons(
                    inputId = "typeSelector",
                    label = "Select Type:", 
                    choices = c("Employee Generation","Employee Work Stage"),
                    inline = T,
                    selected = "Employee Generation"))
              ),
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput(outputId = "empByPerf"),
                  title = tags$b("Employee Performance Proportion")     
                )
              )
            ),
            box(
              height = 660,
              div(style = "text-align:justify", 
                  h4(strong("Employee Generation"), style = "color:green"), 
                  strong("Baby Boomers"), ": born between 1946 and 1964", br(),
                  strong("Gen X"), ": born between 1965 and 1979", br(),
                  strong("Millennials"), ": born between 1980 and 1995", br(),
                  strong("Gen Z"), ": born between 1996 and 2011", br(),
                  strong("Gen Alpha"), ": born between 2012 and 2025", br(),
                  h4(strong("Employee Work Stage"), style = "color:green"),
                  strong("Idealize (<1 year)"), ": High motivation and excitement, 
                  the job duties seem challenging and rewarding. Perceive their supervisor as someone 
                  they can work for and is an individual that will be committed to their success; 
                  Their co-workers seem like nice people that will help them transition smoothly into the new job.", br(),
                  strong("Honeymoon (1-2 years)"), ": Learning new rules, the culture, the supervisor’s leadership style, 
                  processes, procedures, rules, products and people; Begin to understand the subtleties of office politics and 
                  etiquette. They may have some doubts about your ability to do this new job, wondering who they can trust and will be allies and mentors.", br(),
                  strong("Real Reconciliation (3-5 years)"), ": Reality sets in and a gap forms between the ideal and the real. 
                  The job and work environment turn out to be different than what they envisioned. Their co-workers and supervisor are not exactly 
                  what they thought they would be like. They may feel disappointed or frustrated.", br(),
                  strong("Peak Performance (6-10 years)"), ": Their highest degree of mastery of the job and highest productivity, their energy is focused 
                  on accomplishing tasks and goals. Fully integrated into the work group and organization, 
                  able to better negotiate changes effectively, have taken on increased responsibilities and roles.", br(),
                  strong("Plateau (>10 years)"), ": Their professional development in this job has leveled off or declined. They are not learning 
                  very much and their enthusiasm has waned, come in and do their job and then leave.", br()
                  )
            )
          ))
        ),
      
      # TAB 3
      
      tabItem(
        tabName = "Data",
        h2(tags$b("Human Resources Data Set")),
        tags$i("https://www.kaggle.com/datasets/rhuebner/human-resources-data-set"),
        br(),
        br(),
        dataTableOutput("hrDataSet")
      )
    )
  )
)
 