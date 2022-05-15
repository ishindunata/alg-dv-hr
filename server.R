shinyServer(function(input, output) {

  # SPARK VALUE BOX THEME ------------------------------------------------------
  valueBoxSpark <- 
    function(value, 
             title, 
             sparkobj = NULL, 
             subtitle, 
             info = NULL, 
             icon = NULL, 
             color = "aqua",  
             width = 12, 
             href = NULL){
      shinydashboard:::validateColor(color)
      
      if (!is.null(icon))
        shinydashboard:::tagAssert(icon, type = "i")
      
      info_icon <- tags$small(
        tags$i(
          class = "fa fa-info-circle fa-lg",
          title = info,
          `data-toggle` = "tooltip",
          style = "color: rgba(255, 255, 255, 0.75);"
        ),
        # bs3 pull-right 
        # bs4 float-right
        class = "pull-right float-right"
      )
      
      boxContent <- div(
        class = paste0("small-box bg-", color),
        div(
          class = "inner",
          tags$small(title),
          if (!is.null(sparkobj)) info_icon,
          h3(value),
          if (!is.null(sparkobj)) sparkobj,
          p(subtitle)
        ),
        # bs3 icon-large
        # bs4 icon
        if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
      )
      
      if (!is.null(href)) 
        boxContent <- a(href = href, boxContent)
      
      div(
        class = if (!is.null(width)) paste0("col-sm-", width), 
        boxContent
      )
    }
  
     output$overviewEmpTrend <- renderPlotly({
      hrdt_merge_trend <- hrdt_trend %>% mutate(Type = "All Employee") %>% 
        bind_rows(hrdt_active_trend %>% mutate(Type = "Active Employee")) %>% 
        bind_rows(hrdt_termv_trend %>% mutate(Type = "Voluntarily Terminated Employee")) %>% 
        bind_rows(hrdt_termc_trend %>% mutate(Type = "Terminated for Cause Employee"))
      
      hrdt_merge_trend <- hrdt_merge_trend %>% 
        mutate(
          Type = factor(Type, levels = c("All Employee", "Active Employee", "Voluntarily Terminated Employee", "Terminated for Cause Employee"))
        )
      
      plot0 <- ggplot(hrdt_merge_trend,aes(y = cnt_emp,x = HireYr, group = Type, text = label)) + 
        geom_line(aes(color = Type)) +
        geom_point(aes(color = Type)) +
        scale_color_manual(values=c("#04B486", "#DF0101","#fad414", "#13acf2")) +
        scale_x_continuous(breaks = hrdt_merge_trend$HireYr, labels = hrdt_merge_trend$HireYr)
      labs(
        title = "Total Employee Trend",
        x = "Year",
        y = "Total Employee"
      ) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              axis.title = element_text(size = 5),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line.y = element_line(colour = "grey"),
              axis.line.x = element_blank(),
              panel.grid = element_blank())
      
      ggplotly(plot0, tooltip = "text") %>% 
        layout(legend = list(title="", x = 100, y = 0.5),
               xaxis = list(title="Year", titlefont = list(size = 12)),
               yaxis = list(title="Total Employee", titlefont = list(size = 12)))
    })
    
    output$overviewAllEmp <- renderValueBox({
      hrdt_Growth <- 
        hrdt_clean %>% 
        group_by(HireYr) %>% 
        summarise(`Total Emp` = n_distinct(EmployeeName)) %>% 
        mutate_each(funs(factor(.)), c("HireYr")) %>% 
        mutate(GrowthValue = `Total Emp` - lag(`Total Emp`),
               GrowthPerc = (`Total Emp` - lag(`Total Emp`)) / `Total Emp`) %>%
        mutate(GrowthValue = replace_na(GrowthValue, 0),
               GrowthPerc = replace_na(GrowthPerc, 0))
      
      hchrdt_all <- 
        hrdt_trend %>% 
        hchart("area", hcaes(x = HireYr, y = cnt_emp), name = "Total Employee") %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      vbhrdt_all <- valueBoxSpark(
        value = comma(length(unique(hrdt_clean$EmployeeName))),
        title = HTML(paste(toupper("Total Employee"), br(), 
                               toupper("All Status"))),
        sparkobj = hchrdt_all,
        info = "This is total all employee from year 2006 until 2018",
        subtitle = tags$p(tagList("Average growth per year ",
                           HTML("&darr;"),
                           percent(mean(hrdt_Growth$GrowthPerc), 
                                   decimal.mark = ".", 
                                   accuracy = .01)), style = "font-size: 85%;"), 
        icon = icon("users"),
        width = 4,
        color = "green",
        href = NULL
      )
      
      vbhrdt_all
      
    })
    
    output$overviewActiveEmp <- renderValueBox({
      hrdt_active_Growth <- 
        hrdt_clean %>% 
        filter(EmploymentStatus=="Active") %>% 
        group_by(HireYr) %>% 
        summarise(`Total Emp` = n_distinct(EmployeeName)) %>% 
        mutate_each(funs(factor(.)), c("HireYr")) %>% 
        mutate(GrowthValue = `Total Emp` - lag(`Total Emp`),
               GrowthPerc = (`Total Emp` - lag(`Total Emp`)) / `Total Emp`) %>%
        mutate(GrowthValue = replace_na(GrowthValue, 0),
               GrowthPerc = replace_na(GrowthPerc, 0))
      
      hchrdt_active <- 
        hrdt_active_trend %>% 
        hchart("area", hcaes(x = HireYr, y = cnt_emp), name = "Total Employee") %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      vbhrdt_active <- valueBoxSpark(
        value = comma(hrdt_active$cnt),
        title = HTML(paste(toupper("Total Employee"), br(), 
                           toupper("Active Status"))),
        sparkobj = hchrdt_active,
        info = "This is total active employee from year 2006 until 2018",
        subtitle = tags$p(tagList("Average growth per year ",
                           HTML("&darr;"),
                           percent(mean(hrdt_active_Growth$GrowthPerc), 
                                   decimal.mark = ".", 
                                   accuracy = .01)), style = "font-size: 85%;"), 
        icon = icon("user"),
        width = 4,
        color = "red",
        href = NULL
      )
      
      vbhrdt_active
      
    })
    
    output$overviewTermvEmp <- renderValueBox({
      hrdt_termv_Growth <- 
        hrdt_clean %>% 
        filter(EmploymentStatus=="Voluntarily Terminated") %>% 
        group_by(HireYr) %>% 
        summarise(`Total Emp` = n_distinct(EmployeeName)) %>% 
        mutate_each(funs(factor(.)), c("HireYr")) %>% 
        mutate(GrowthValue = `Total Emp` - lag(`Total Emp`),
               GrowthPerc = (`Total Emp` - lag(`Total Emp`)) / `Total Emp`) %>%
        mutate(GrowthValue = replace_na(GrowthValue, 0),
               GrowthPerc = replace_na(GrowthPerc, 0))
      
      hchrdt_termv <- 
        hrdt_termv_trend %>% 
        hchart("area", hcaes(x = HireYr, y = cnt_emp), name = "Total Employee") %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      vbhrdt_termv <- valueBoxSpark(
        value = comma(hrdt_termv$cnt),
        title = HTML(paste(toupper("Total Employee"), br(), 
                           toupper("Voluntarily Terminated Status"))),
        sparkobj = hchrdt_termv,
        info = "This is total voluntarily terminated employee from year 2006 until 2018",
        subtitle = tags$p(tagList("Average growth per year ",
                           HTML("&darr;"),
                           percent(mean(hrdt_termv_Growth$GrowthPerc), 
                                   decimal.mark = ".", 
                                   accuracy = .01)), style = "font-size: 85%"), 
        icon = icon("user-minus"),
        width = 4,
        color = "yellow",
        href = NULL
      )
      
      vbhrdt_termv
      
    })
    
    output$overviewTermcEmp <- renderValueBox({
      hrdt_termc_Growth <- 
        hrdt_clean %>% 
        filter(EmploymentStatus=="Terminated for Cause") %>% 
        group_by(HireYr) %>% 
        summarise(`Total Emp` = n_distinct(EmployeeName)) %>% 
        mutate_each(funs(factor(.)), c("HireYr")) %>% 
        mutate(GrowthValue = `Total Emp` - lag(`Total Emp`),
               GrowthPerc = (`Total Emp` - lag(`Total Emp`)) / `Total Emp`) %>%
        mutate(GrowthValue = replace_na(GrowthValue, 0),
               GrowthPerc = replace_na(GrowthPerc, 0))
      
      hchrdt_termc <- 
        hrdt_termc_trend %>% 
        hchart("area", hcaes(x = HireYr, y = cnt_emp), name = "Total Employee") %>% 
        hc_size(height = 100) %>% 
        hc_credits(enabled = FALSE) %>% 
        hc_add_theme(hc_theme_sparkline_vb()) 
      
      vbhrdt_termc <- valueBoxSpark(
        value = comma(hrdt_termc$cnt),
        title = HTML(paste(toupper("Total Employee"), br(), 
                           toupper("Terminated for Cause Status"))),
        sparkobj = hchrdt_termc,
        info = "This is total terminated for cause employee from year 2006 until 2018",
        subtitle = tags$p(tagList("Average growth per year ",
                           HTML("&darr;"),
                           percent(mean(hrdt_termc_Growth$GrowthPerc), 
                                   decimal.mark = ".", 
                                   accuracy = .01)), style = "font-size: 85%;"), 
        icon = icon("user-slash"),
        width = 4,
        color = "light-blue",
        href = NULL
      )
      
      vbhrdt_termc
      
    })
    
    output$overviewEmpByGender <- renderEcharts4r({
      hrdt_clean %>% 
        group_by(Sex) %>% 
        summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
        arrange(cnt_emp) %>% 
        e_chart(Sex) %>%
        e_pie(cnt_emp, radius = c ("30%", "75%")) %>%
        e_legend(F) %>% 
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br>Total Employee: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  "))
    })
    
    output$overviewEmpByMarital <- renderEcharts4r({
      hrdt_clean %>% 
        group_by(MaritalDesc) %>% 
        summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
        arrange(cnt_emp) %>% 
        e_chart(MaritalDesc) %>%
        e_pie(cnt_emp, radius = c ("30%", "75%")) %>%
        e_legend(F) %>% 
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br>Total Employee: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  "))
    })
    
    output$overviewEmpByCitizen <- renderEcharts4r({
      hrdt_clean %>% 
        group_by(CitizenDesc) %>% 
        summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
        arrange(cnt_emp) %>% 
        e_chart(CitizenDesc) %>%
        e_pie(cnt_emp, radius = c ("30%", "75%")) %>%
        e_legend(F) %>% 
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br>Total Employee: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  "))
    })
    
    output$overviewEmpByRace <- renderEcharts4r({
      hrdt_clean %>% 
        group_by(RaceDesc) %>% 
        summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
        arrange(cnt_emp) %>% 
        e_chart(RaceDesc) %>%
        e_pie(cnt_emp, radius = c ("30%", "75%")) %>%
        e_legend(F) %>% 
        e_tooltip(formatter = htmlwidgets::JS("
                                        function(params)
                                        {
                                            return `<strong>${params.name}</strong>
                                                    <br>Total Employee: ${params.value}
                                                    <br/>Percent: ${params.percent}%`
                                        }  "))
    })
    
    output$empByDept <- renderPlotly({
      
      # First: Setup the filters. If NULL: all categories else: chosen ctaegories
      sel_year <- if (input$yearSelector>2006) seq(2006,2006+(input$yearSelector-2006)) else input$yearSelector
      sel_state <- if (input$stateSelector=="All") levels(hrdt_clean$State) else input$stateSelector
      sel_status <- if (input$statusSelector=="All") levels(hrdt_clean$EmploymentStatus) else input$statusSelector
        
      hrdt_dept_count <- hrdt_clean %>% 
        filter(
          HireYr %in% sel_year,
          State %in% sel_state,
          EmploymentStatus %in% sel_status
        ) %>%
        group_by(Department) %>% 
        summarise(count=n_distinct(EmployeeName)) %>% 
        ungroup() %>% 
        arrange(-count)
      
      hrdt_dept_count2 <- hrdt_dept_count %>% 
        mutate(label = glue(
          "Total Employee: {comma(count)}"
        ))

      validate(
        need(nrow(hrdt_dept_count2) > 0, 
             "Based on the data set, no data exists with the selected inputs. Please select other inputs.")
      )

      plot1 <- ggplot(data = hrdt_dept_count2, aes(x = count, 
                                                   y = reorder(Department, count), # reorder(A, berdasarkan B)
                                                   text = label)) + 
        geom_col(aes(fill = count)) +
        scale_fill_gradient(low="red", high="black") +
        labs(x = "Total Employee",
             y = NULL) +
        theme_minimal() +
        theme(legend.position = "none") 
      
      if (input$yearSelector>2006) {
        ggplotly(plot1, tooltip = "text") %>% 
          layout(title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: 2006-{input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      } else {
        ggplotly(plot1, tooltip = "text") %>% 
          layout(title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: {input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      }
    })
    
    output$salByDept <- renderPlotly({
      
      # First: Setup the filters. If NULL: all categories else: chosen ctaegories
      sel_year <- if (input$yearSelector>2006) seq(2006,2006+(input$yearSelector-2006)) else input$yearSelector
      sel_state <- if (input$stateSelector=="All") levels(hrdt_clean$State) else input$stateSelector
      sel_status <- if (input$statusSelector=="All") levels(hrdt_clean$EmploymentStatus) else input$statusSelector
        
      hrdt_dept_sal_avg <- hrdt_clean %>% 
        filter(
          HireYr %in% sel_year,
          State %in% sel_state,
          EmploymentStatus %in% sel_status
        ) %>% 
        group_by(Department) %>% 
        summarise(mean_sal=mean(Salary)) %>% 
        ungroup() %>% 
        arrange(-mean_sal)
      
      hrdt_dept_sal_avg2 <- hrdt_dept_sal_avg %>% 
        mutate(label = paste0(
          "Average Salary: $", comma(mean_sal,accuracy = 0.01))
        )
      
      validate(
        need(nrow(hrdt_dept_sal_avg2) > 0, 
             "Based on the data set, no data exists with the selected inputs. Please select other inputs.")
      )
      
      plot2 <- ggplot(hrdt_dept_sal_avg2, aes(x = reorder(Department, mean_sal), 
                                              y = mean_sal,
                                              text = label)) +
        geom_segment(aes(x=reorder(Department, mean_sal), xend=reorder(Department, mean_sal), y=0,yend=mean_sal), color="red") +
        geom_point(color="black") +
        coord_flip() +
        labs(x = NULL,
             y = "Average Salary") +
        scale_y_continuous(label = dollar_format(accuracy = 0.01), breaks=seq(0,300000,50000)) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 30, hjust = 1)) 
      
      if (input$yearSelector>2006) {
        ggplotly(plot2, tooltip = "text") %>% 
          layout(title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: 2006-{input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))      
      } else {
        ggplotly(plot2, tooltip = "text") %>% 
          layout(title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: {input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      }
    })
    
    output$empByGen <- renderPlotly({
      
      # First: Setup the filters. If NULL: all categories else: chosen ctaegories
      sel_year <- if (input$yearSelector>2006) seq(2006,2006+(input$yearSelector-2006)) else input$yearSelector
      sel_state <- if (input$stateSelector=="All") levels(hrdt_clean$State) else input$stateSelector
      sel_status <- if (input$statusSelector=="All") levels(hrdt_clean$EmploymentStatus) else input$statusSelector
      
      hrdt_state_gen <- hrdt_clean %>%
        filter(
          HireYr %in% sel_year,
          State %in% sel_state,
          EmploymentStatus %in% sel_status
        ) %>%
        count(Department, Generation) %>% 
        group_by(Department) %>% 
        mutate(pct = prop.table(n) * 100)

      hrdt_state_gen2 <- hrdt_state_gen %>% 
        mutate(
          label = glue(
            "Total Employee: {comma(n)}"
          ))
      
      validate(
        need(nrow(hrdt_state_gen2) > 0, 
             "Based on the data set, no data exists with the selected inputs. Please select other inputs.")
      )
      
      plot3 <- ggplot(hrdt_state_gen2, aes(x = Department, y = n, fill=Generation, text = label)) +
        geom_col(position = "fill", stat = "identity") +
        geom_text(aes(label=paste0(sprintf("%.2f", pct),"%")), colour = "black", size = 3,
                  position=position_fill(vjust=0.5)) +
        geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.3, col = "white") +
        # scale_fill_manual(values = c("#04B486", "#DF0101","#fad414", "#13acf2", "#c913f2")) +
        scale_y_continuous(label = percent_format(accuracy = 0.01)) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line.y = element_line(colour = "grey"),
              axis.line.x = element_blank(),
              panel.grid = element_blank())
      
      if (input$yearSelector>2006) {
        ggplotly(plot3, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: 2006-{input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))     
      } else {
        ggplotly(plot3, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector}, Year: {input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      }
    })
    
    output$empByStage <- renderPlotly({
      
      # First: Setup the filters. If NULL: all categories else: chosen ctaegories
      sel_year <- if (input$yearSelector>2006) seq(2006,2006+(input$yearSelector-2006)) else input$yearSelector
      sel_state <- if (input$stateSelector=="All") levels(hrdt_clean$State) else input$stateSelector
      sel_status <- if (input$statusSelector=="All") levels(hrdt_clean$EmploymentStatus) else input$statusSelector
      
      hrdt_state_ws <- hrdt_clean %>%
        filter(
          HireYr %in% sel_year,
          State %in% sel_state,
          EmploymentStatus %in% sel_status
        ) %>%
        count(Department, WorkStage) %>% 
        group_by(Department) %>% 
        mutate(pct = prop.table(n) * 100)
      
      hrdt_state_ws2 <- hrdt_state_ws %>% 
        mutate(
          label = glue(
            "Total Employee: {comma(n)}"
          ))
      
      validate(
        need(nrow(hrdt_state_ws2) > 0, 
             "Based on the data set, no data exists with the selected inputs. Please select other inputs.")
      )
      
      plot4 <- ggplot(hrdt_state_ws2, aes(x = Department, y = n, fill=WorkStage, text = label)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label=paste0(sprintf("%.2f", pct),"%")), colour = "black", size = 3,
                  position=position_fill(vjust=0.5)) +
        geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.3, col = "white") +
        # scale_fill_manual(values = c("#04B486", "#DF0101","#fad414", "#13acf2", "#c913f2")) +
        scale_y_continuous(label = percent_format(accuracy = 0.01)) +
        labs(title = NULL,
             x = NULL,
             y = NULL) +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              plot.title = element_text(size=10, hjust=0, vjust=0),
              panel.background = element_rect(fill = "#ffffff"),
              axis.line.y = element_line(colour = "grey"),
              axis.line.x = element_blank(),
              panel.grid = element_blank())
      
      if (input$yearSelector>2006) {
        ggplotly(plot4, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector} , Year: 2006-{input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      } else {
        ggplotly(plot4, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>{input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector} , Year: {input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      }
    })
    
    output$empByPerf <- renderPlotly({
      # First: Setup the filters. If NULL: all categories else: chosen ctaegories
      sel_year <- if (input$yearSelector>2006) seq(2006,2006+(input$yearSelector-2006)) else input$yearSelector
      sel_state <- if (input$stateSelector=="All") levels(hrdt_clean$State) else input$stateSelector
      sel_status <- if (input$statusSelector=="All") levels(hrdt_clean$EmploymentStatus) else input$statusSelector

      if (input$typeSelector=="Employee Generation") {
        hrdt_state_perf <- hrdt_clean %>%
          filter(
            HireYr %in% sel_year,
            State %in% sel_state,
            EmploymentStatus %in% sel_status
          ) %>%
          count(Generation, PerformanceScore) %>% 
          group_by(Generation) %>% 
          mutate(pct = prop.table(n) * 100)
      } else {
        hrdt_state_perf <- hrdt_clean %>%
          filter(
            HireYr %in% sel_year,
            State %in% sel_state,
            EmploymentStatus %in% sel_status
          ) %>%
          count(WorkStage, PerformanceScore) %>% 
          group_by(WorkStage) %>% 
          mutate(pct = prop.table(n) * 100)
      }
      
      hrdt_state_perf2 <- hrdt_state_perf %>% 
        mutate(
          label = glue(
            "Total Employee: {comma(n)}"
          ))
      
      validate(
        need(nrow(hrdt_state_perf2) > 0, 
             "Based on the data set, no data exists with the selected inputs. Please select other inputs.")
      )
      
      if (input$typeSelector=="Employee Generation") {
        plot5 <- ggplot(hrdt_state_perf2, aes(x = Generation, y = n, fill=PerformanceScore, text = label)) +
          geom_bar(position = "fill", stat = "identity") +
          geom_text(aes(label=paste0(sprintf("%.2f", pct),"%")), colour = "black", size = 3,
                    position=position_fill(vjust=0.5)) +
          geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.3, col = "white") +
          # scale_fill_manual(values = c("#04B486", "#DF0101","#fad414", "#13acf2", "#c913f2")) +
          scale_y_continuous(label = percent_format(accuracy = 0.01)) +
          labs(title = NULL,
               x = NULL,
               y = NULL) +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                plot.title = element_text(size=10, hjust=0, vjust=0),
                panel.background = element_rect(fill = "#ffffff"),
                axis.line.y = element_line(colour = "grey"),
                axis.line.x = element_blank(),
                panel.grid = element_blank())
      } else {
      plot5 <- ggplot(hrdt_state_perf2, aes(x = WorkStage, y = n, fill=PerformanceScore, text = label)) +
          geom_bar(position = "fill", stat = "identity") +
          geom_text(aes(label=paste0(sprintf("%.2f", pct),"%")), colour = "black", size = 3,
                    position=position_fill(vjust=0.5)) +
          geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.3, col = "white") +
          # scale_fill_manual(values = c("#04B486", "#DF0101","#fad414", "#13acf2", "#c913f2")) +
          scale_y_continuous(label = percent_format(accuracy = 0.01)) +
          labs(title = NULL,
               x = NULL,
               y = NULL) +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 30, hjust = 1),
                plot.title = element_text(size=10, hjust=0, vjust=0),
                panel.background = element_rect(fill = "#ffffff"),
                axis.line.y = element_line(colour = "grey"),
                axis.line.x = element_blank(),
                panel.grid = element_blank())
      }

      if (input$yearSelector>2006) {
        ggplotly(plot5, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>by {input$typeSelector}, {input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector} , Year: 2006-{input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))    
      } else {
        ggplotly(plot5, tooltip = "text") %>% 
          layout(legend = list(title="", orientation = "h", x = 0, y = -0.5, font=list(size=10)),
                 title=list(text=paste0(glue("<b>by {input$typeSelector}, {input$statusSelector} Status</b>"),
                                        "<br>",
                                        "<sup>",
                                        glue("State: {input$stateSelector} , Year: {input$yearSelector}"), "</sup>"))
                 , font=list(
                   family = "Arial",
                   size = 11))
      }
    })
    
    output$hrDataSet <- renderDataTable({
      options(DT.options = list(pageLength = 20))
      
      datatable(hrdt_clean, options = list(scrollX = T))
    })
})
