# 3. Блок функций             ####
func2 <- function(x,  indic = "Численность населения на 1 января текущего года", loc = 'Барнаул', revuyt=0, year=2021){#x <- df
  indicator_read <- indicator$code[which(indicator$long_name == indic)]
  location_read  <- location$oktmo[which(location$long_name == loc)]
  
  df_1 <- x[year < 2042 & indicator == indicator_read & location == location_read]
  if (revuyt != 0) {
    df_1$dyn_calc[which(df_1$year == year)] <- revuyt
  }
  return(df_1)
}

func <- function(x,  indic_to="Численность населения на 1 января текущего года", indic_from="Численность населения на 1 января текущего года", 
                 persent = rep(0,20), loc = 'Барнаул', second_indic = NULL, check2 = FALSE, all_citys_check = FALSE){#x <- df #second_indic <- "Total living area" # all_citys_check=1 #check2 = TRUE
  if (is.null(all_citys_check)){
    all_citys_check = FALSE
  }
  if(length(indic_to) == 0){
    indic_to <- "Численность населения на 1 января текущего года"
  }
  if(length(indic_from) == 0){
    indic_from <- "Численность населения на 1 января текущего года"
  }
  if(loc == ""){
    loc = "Барнаул"
  }
  if (all_citys_check == TRUE & check2 == TRUE) {
    indicator_to <- indicator$code[which(indicator$long_name == indic_to)]
    indicator_from <- indicator$code[which(indicator$long_name == indic_from)]
    indicator_secont <- indicator$code[which(indicator$long_name == second_indic)]
    location_read  <- location$oktmo[which(location$long_name == loc)]
    df_1 <- x %>% filter((year == 2022) & (indicator == indicator_to | indicator == indicator_secont)) %>% 
      select(location, dyn_calc,indicator,name_location)
    df_1 <- df_1 %>%  pivot_wider(names_from = indicator,values_from = dyn_calc) %>% 
      select(location, x = 3 ,Cityes = 4,name_location)
    df_2 <- x %>% filter((year < 2042) & (indicator == indicator_to | indicator == indicator_secont)  & location == location_read)
    
    new_dyn_calc_time <- new_dyn_calc %>% filter(from == indicator_from & to == indicator_to & location == location_read)
    
    df_2 <- df_2 %>% left_join( indicator2 %>%  select(code,long_name),by = c("indicator" = "code")) %>% mutate(data_new_old = "old")
    
    
    df_2_2 <- df_2 %>% mutate(data_new_old = "new")
    
    summ_new_per_values <- function(n){#n <- 2021
      df_2_2[which(df_2_2$year >= n & df_2_2$indicator == indicator_to),"dyn_calc"] <<- df_2_2[which(df_2_2$year >= n & df_2_2$indicator == indicator_to),"dyn_calc"] -
        new_dyn_calc_time[which((new_dyn_calc_time$tau/10) >= n - 2021),"eff_nat"]*persent[n - 2021]
    }
    2022:2041 %>% map_dfr(~ summ_new_per_values(.))
    df_2 <- rbind(df_2,df_2_2)
    
    df_2 <- df_2[which(df_2$year > 2021),]
    df_2 <- df_2 %>% select(indicator, value = `dyn_calc`, year,location,data_new_old) %>% 
      pivot_wider(names_from = indicator) %>% select(x = 4, y = 5,location = 2, new_old = 3, year = 1)
    df_99 <- x %>% filter (indicator == indicator_to | indicator == indicator_secont) %>% 
      select(location, dyn_calc,indicator,name_location, year) %>%  pivot_wider(names_from = indicator,values_from = dyn_calc) %>% 
      select(year, x = 4 ,y = 5)
    
    center_mass_in_years <- function(dtfr,year_chouse){#dtfr <- df_1 #year_chouse <- 2001
      datafrau <- dtfr[which(dtfr$year == year_chouse),]
      return(c(mean(datafrau$x),mean(datafrau$y)))
    }
    df_mass_center <- data.frame(x = numeric(21), y = numeric(21), stringsAsFactors = FALSE)
    for(i in 23:43){
      df_mass_center$x[i-22] <- center_mass_in_years(df_99,2000 + i - 1)[1]
      df_mass_center$y[i-22] <- center_mass_in_years(df_99,2000 + i - 1)[2]
    }
    
    k <- x
    k[which(x$year < 2042 & x$location == location_read & (x$indicator == indicator_to | x$indicator == indicator_secont)),"dyn_calc"] <- df_2_2$dyn_calc
    df_999 <- k %>% filter(indicator == indicator_to | indicator == indicator_secont) %>% 
      select(location, dyn_calc,indicator,name_location, year) %>%  pivot_wider(names_from = indicator,values_from = dyn_calc) %>% 
      select(year, x = 4 ,y = 5)
    
    df_mass_new <- data.frame(x = numeric(21), y = numeric(21), stringsAsFactors = FALSE)
    for (i in 23:43) {
      df_mass_new$x[i - 22] <- center_mass_in_years(df_999,2000 + i - 1)[1]
      df_mass_new$y[i - 22] <- center_mass_in_years(df_999,2000 + i - 1)[2]
    }
    plot_ly() %>%
      add_markers(x = df_1$x, y = df_1$Cityes, mode = 'markers', name = 'Другие Города', color = I("gray"),
                  hovertemplate = paste0("Город :", df_1$name_location,"<br>", second_indic,": ", df_1$x,"<br>", indic_to,": ", df_1$Cityes)) %>%
      add_paths(x = df_2$x[which(df_2$new_old == "new")], y = df_2$y[which(df_2$new_old == "new")], 
                mode = 'lines+markers', name = 'Ваш сценарий', color = I("orange")) %>%
      add_paths(x = df_2$x[which(df_2$new_old == "old")], y = df_2$y[which(df_2$new_old == "old")], 
                mode = 'lines+markers', name = 'Инерционный сценарий', color = I("deepskyblue")) %>%
      add_annotations( x = df_2$x[which(df_2$new_old == "new" & (df_2$year %% 5 == 0 | df_2$year == 2022))],
                       y = df_2$y[which(df_2$new_old == "new" & (df_2$year %% 5 == 0 | df_2$year == 2022))],
                       text = df_2$year[which(df_2$new_old == "new" & (df_2$year %% 5 == 0 | df_2$year == 2022))],
                       yanchor = 'top', xref = "x",yref = "y",
                       font = list(color = '#545454',
                                   family = 'Panton',
                                   size = 12)) %>%
      add_markers(x = df_2$x[which(df_2$new_old == "new" & (df_2$year %% 5 == 0 | df_2$year == 2022))], y = df_2$y[which(df_2$new_old == "new" & (df_2$year %% 5 == 0 | df_2$year == 2022))], mode = 'markers', color = I("deepskyblue"),name = 'Годы') %>%
      add_markers(x = df_2$x[which(df_2$new_old == "new" & df_2$year == 2041)], y = df_2$y[which(df_2$new_old == "new" & df_2$year == 2041)], mode = 'markers', name = 'Конец', color = I("red")) %>%
      add_paths(x = df_mass_new$x, y = df_mass_new$y, 
                mode = 'lines+markers', name = 'Средневзвешенное значение по городам, Ваш сценарий', color = I("orange"),line = list(width = 3)) %>%
      add_paths(x = df_mass_center$x, y = df_mass_center$y,
                mode = 'lines+markers', name = 'Средневзвешенное значение по городам, Инерционный сценарий', color = I("darkolivegreen4"),line = list(width = 3)) %>%
      layout(plot_bgcolor = '#343a40') %>%
      layout(paper_bgcolor = '#343a40') %>%
      layout(font = t) %>%
      layout(legend = list(
        orientation = "h",y = -0.25))  %>%
      layout(xaxis = list(gridcolor = '#6c757d'),
             yaxis = list(gridcolor = '#6c757d')) %>% 
      add_annotations(
        text = paste0(indic_to,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == indic_to)])]),
        x = 0,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(
          size = 19)
      ) %>%
      layout(title = FALSE, 
             xaxis = list(title = paste0(second_indic,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == second_indic)])]))) %>%
      layout(xaxis = list(type = "log"), yaxis = list(type = "log"))
  }else{
    if (check2 == FALSE){
      second_indic <- NULL
    }
    if (length(second_indic) != 0 & !is.null(second_indic)){
      indicator_to <- indicator$code[which(indicator$long_name == indic_to)]
      indicator_from <- indicator$code[which(indicator$long_name == indic_from)]
      indicator_secont <- indicator$code[which(indicator$long_name == second_indic)]
      location_read  <- location$oktmo[which(location$long_name == loc)]
      df_1 <- x %>% filter((year < 2042) & (indicator == indicator_to | indicator == indicator_secont)  & location == location_read)
      
      new_dyn_calc_time <- new_dyn_calc %>% filter(from == indicator_from & to == indicator_to & location == location_read)
      
      df_1 <- df_1 %>% left_join( indicator2 %>%  select(code,long_name),by = c("indicator" = "code")) %>% mutate(data_new_old = "old")
      
      df_1_2 <- df_1 %>% mutate(data_new_old = "new")
      
      summ_new_per_values <- function(n){#n <- 2021
        df_1_2[which(df_1_2$year >= n & df_1_2$indicator == indicator_to),"dyn_calc"] <<- df_1_2[which(df_1_2$year >= n & df_1_2$indicator == indicator_to),"dyn_calc"] -
          new_dyn_calc_time[which((new_dyn_calc_time$tau/10) >= n - 2021),"eff_nat"]*persent[n - 2021]
      }
      2022:2041 %>% map_dfr(~ summ_new_per_values(.))
      df_1 <- rbind(df_1,df_1_2)
      df_2 <- df_1 %>% select(long_name, value = `dyn_calc`, year,location,data_new_old) %>% as.tibble() %>% 
        pivot_wider(names_from = long_name) %>% select(x = 4, y = 5,location = 2, new_old = 3,year = 1)
      
      plot_ly() %>%
        add_paths(x = df_2$x[which(df_2$new_old == "new")], y =  df_2$y[which(df_2$new_old == "new")], mode = 'lines+markers', name = 'Ваш сценарий', color = I("orange")) %>%
        add_paths(x = df_2$x[which(df_2$new_old == "old")], y =  df_2$y[which(df_2$new_old == "old")], mode = 'lines+markers', name = 'Инерционный сценарий', color = I("deepskyblue")) %>%
        add_annotations( x = df_2$x[which(df_2$new_old == "new" & df_2$year %% 5 == 0)],
                         y = df_2$y[which(df_2$new_old == "new" & df_2$year %% 5 == 0)],
                         text = df_2$year[which(df_2$new_old == "new" & df_2$year %% 5 == 0)],
                         yanchor = 'top',
                         font = list(color = '#545454',
                                     family = 'Panton',
                                     size = 12)) %>%
        add_markers(x = df_2$x[which(df_2$new_old == "new" & df_2$year %% 5 == 0)], y = df_2$y[which(df_2$new_old == "new" & df_2$year %% 5 == 0)], mode = 'markers', color = I("deepskyblue"),name = 'Годы') %>%
        add_markers(x = df_2$x[which(df_2$new_old == "new" & df_2$year == 2041)], y = df_2$y[which(df_2$new_old == "new" & df_2$year == 2041)], mode = 'markers', name = 'Конец', color = I("red")) %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(indic_to,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == indic_to)])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 17)
        ) %>%
        layout(title = FALSE,xaxis = list(title = paste0(second_indic,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == second_indic)])])))
    }else{
      
      indicator_to <- indicator$code[which(indicator$long_name == indic_to)]
      indicator_from <- indicator$code[which(indicator$long_name == indic_from)]
      location_read  <- location$oktmo[which(location$long_name == loc)]
      df_1 <- x %>% filter((year < 2042) & indicator == indicator_to  & location == location_read)
      new_dyn_calc_time <- new_dyn_calc %>% filter(from == indicator_from & to == indicator_to & location == location_read)
      df_1 <- df_1 %>% left_join( indicator2 %>%  select(code,long_name),by = c("indicator" = "code")) %>% mutate(data_new_old = "old")
      
      df_1_2 <- df_1 %>% mutate(data_new_old = "new")
      
      summ_new_per_values <- function(n){#n <- 2022
        df_1_2[which(df_1_2$year >= n & df_1_2$indicator == indicator_to),"dyn_calc"] <<- df_1_2[which(df_1_2$year >= n & df_1_2$indicator == indicator_to),"dyn_calc"] -
          new_dyn_calc_time[which((new_dyn_calc_time$tau/10) >= n - 2021),"eff_nat"]*persent[n - 2021]
      }
      2022:2041 %>% map_dfr(~ summ_new_per_values(.))
      df_1 <- rbind(df_1,df_1_2)
      plot_ly() %>%
        
        add_trace(x = df_1$year[which(df_1$data_new_old == "new")], y = df_1$dyn_calc[which(df_1$data_new_old == "new")], mode = 'lines+markers', name = 'Ваш сценарий', color = I("orange")) %>%
        add_trace(x = df_1$year[which(df_1$data_new_old == "old")], y = df_1$dyn_calc[which(df_1$data_new_old == "old")], mode = 'lines+markers', name = 'Инерционный сценарий', color = I("deepskyblue")) %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(indic_to,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == indic_to)])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 17)
        ) %>%
        layout(title = FALSE, xaxis = list(title = "Год"))
      
    }
  }
}
func_for_map_plot <- function(x, indic="Численность населения на 1 января текущего года", loc = 'Ulyanovsk', norm=0, second_indic = NULL, check2 = 0){
  t <- list(
    family = "Panton",
    size = 14,
    color = 'white')
  if (check2 == 0){
    second_indic <- 0
  }
  name_indicator <- c(indic,second_indic)
  name_location <- loc
  indicator_read <- indicator$code[which(indicator$long_name %in% name_indicator)]
  location_read <- location$oktmo[which(location$long_name %in% name_location)]
  if (length(location_read) == 0){
    name_location = 'Барнаул'
    location_read <- location$oktmo[which(location$long_name %in% name_location)]
  }
  if (length(indicator_read) == 0){
    indicator_read = "C013"
  }
  df_1 <- df[location %in% location_read & indicator %in% indicator_read]
  
  if (length(location_read) > 1 ){
    indicator_read2 <- indicator_read
    indicator_read <- last(indicator_read)
    if (norm == 1){
      if (second_indic != 0) {
        f2 <- function(df1, n){
          f21 <- function(df11,n1,m){
            df1 <- df1 %>% filter(location == n1 & indicator == m) %>% 
              mutate(dyn_calc = (dyn_calc - min(dyn_calc))/(max(dyn_calc) - min(dyn_calc)))
            return(df1)
          }
          df_123 <-  1:length(indicator_read2) %>% map_dfr(~ f21(df1,n, indicator_read2[.]))
          return(df_123)
        }
        df_1 <-  1:length(location_read) %>% map_dfr(~ f2(df_1, location_read[.]))
      }else{
        f2 <- function(df1, n){
          df1 <- df1 %>% filter(location == n) %>% 
            mutate(dyn_calc = (dyn_calc - min(dyn_calc))/(max(dyn_calc) - min(dyn_calc)))
          return(df1)
        }
        df_1 <-  1:length(location_read) %>% map_dfr(~ f2(df_1, location_read[.]))
      }
    }else{
      min_first = round(min(df_1$dyn_calc)*0.99)  
      max_first = round(max(df_1$dyn_calc)*1.01)
      sep = round((max_first - min_first)/5) }
    if (length(second_indic) == 0 | second_indic == 0 ) {
      f3 <- function(df1, n, m){
        df1 <- df1 %>% filter(location == n  & indicator == m) %>% mutate(location = location2$long_name[which(location2$oktmo == n)])
        return(df1)
      }
      df_1 <-  1:length(location_read) %>% map_dfr(~ f3(df_1, location_read[.],indicator_read[1]))}
    else{
      indicator_read_twin <- indicator$code[which(indicator$long_name %in% c(last(indic),second_indic))]
      
      f2 <- function(df1, n){
        f21 <- function(df11,n1,m){
          df1 <- df1 %>% filter(location == n1 & indicator == m)
          return(df1)
        }
        df_123 <-  1:length(indicator_read_twin) %>% map_dfr(~ f21(df1,n, indicator_read_twin[.]))
        return(df_123)
      }
      df_1 <-  1:length(location_read) %>% map_dfr(~ f2(df_1, location_read[.]))
      
      
    }
    if (second_indic != 0){
      df_2 <- df_1 %>% select(indicator, value = `dyn_calc`, year,location) %>% as.tibble() %>% 
        pivot_wider(names_from = indicator) %>% select(x = 4, y = 3,location = 2, year)
      df_2 <- df_2 %>% left_join( location2 %>%  select(oktmo,long_name),by = c("location" = "oktmo"))
      names(df_2)[names(df_2) == 'long_name'] <- 'Citys'
      plot_ly() %>%
        add_trace(x = df_2$y, y = df_2$x, mode = 'scatter', color = df_2$Citys, colors = "Set1") %>%
        add_annotations(x = df_2$y[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        y = df_2$x[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        text = df_2$year[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        yanchor = 'top', xref = "x",yref = "y",
                        font = list(color = '#545454',
                                    family = 'Panton',
                                    size = 9)) %>%
        add_markers(x = df_2$y[which(df_2$year == 2049)], y = df_2$x[which(df_2$year == 2049)], mode = 'markers', name = 'Конец', color = "red") %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(last(indicator$long_name[which(indicator$long_name %in% indic)]),", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == last(indicator$long_name[which(indicator$long_name %in% indic)]))])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 19)
        ) %>%
        layout(title = FALSE, xaxis = list(title = paste0(second_indic,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == second_indic)])])))
    }else{
      df_2 <- df_1  %>% 
        select(location, value = `dyn_calc`, year,fact)
      f <-  plot_ly() %>%
        add_trace(x = df_2$year, y = df_2$value, mode = 'lines+markers', color = df_2$location, colors = "Set1") %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(last(indicator$long_name[which(indicator$long_name %in% indic)]),", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == last(indicator$long_name[which(indicator$long_name %in% indic)]))])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 19)
        ) %>%
        layout(title = FALSE, xaxis = list(title = "Year"))
      if (norm == 0){ f <- f  %>%  add_trace(x = df_2$year, y = df_2$fact, color =  df_2[,location:= paste0(location,"_2")]$location, mode = 'lines+markers', name = 'Факты', colors = "Set2")}
      print(f)
    }
    
  }
  else{
    location_read <- last(location_read)
    if (norm == 1){
      f2 <- function(df1, n){
        df1 <- df1 %>% filter(indicator == n) %>% 
          mutate(dyn_calc = (dyn_calc - min(dyn_calc))/(max(dyn_calc) - min(dyn_calc)))
        return(df1)
      }
      df_1 <-  1:length(indicator_read) %>% map_dfr(~ f2(df_1, indicator_read[.]))
      df_1 <- df_1 %>% filter((year < 2030) & indicator %in% indicator_read & location == location_read)
    }
    f3 <- function(df1, n){
      df1 <- df1 %>% filter(indicator == n) %>% mutate(indicator = indicator2$long_name[which(indicator2$code == n)])
      return(df1)
    }
    df_1 <-  1:length(indicator_read) %>% map_dfr(~ f3(df_1, indicator_read[.]))
    if (second_indic != 0){
      df_1 <- df_1[indicator ==  second_indic | indicator == last(indicator2$long_name[which(indicator2$long_name %in% indic)])]
      df_2 <- df_1 %>% select(indicator, value = `dyn_calc`, year) %>% as.tibble() %>% 
        pivot_wider(names_from = indicator) %>% select(x = 2, y = 3,year)
      plot_ly() %>%
        add_trace(x = df_2$x, y = df_2$y, mode = 'lines+markers',
                  hovertemplate = paste0("Год : ",df_2$year, "<br>", indic, ": ", round(df_2$y), "<br>", second_indic, ": ", round(df_2$x) )) %>%
        add_annotations(x = df_2$x[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        y = df_2$y[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        text = df_2$year[which(df_2$year %% 5 == 0 | df_2$year == 2049)],
                        yanchor = 'top', xref = "x",yref = "y",
                        font = list(color = '#545454',
                                    family = 'Panton',
                                    size = 9)) %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(last(indicator$long_name[which(indicator$long_name %in% indic)]),", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == last(indicator$long_name[which(indicator$long_name %in% indic)]))])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 19)
        ) %>%
        layout(title = FALSE, xaxis = list(title = paste0(second_indic,", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == second_indic)])])))
    }else{
      f <- plot_ly() %>%
        add_trace(x = df_1$year, y = df_1$dyn_calc, mode = 'lines+markers', color = df_1$indicator,
                  hovertemplate = paste0(round(df_1$dyn_calc))) %>%
        layout(plot_bgcolor = '#343a40') %>%
        layout(paper_bgcolor = '#343a40') %>%
        layout(font = t) %>%
        layout(legend = list(
          orientation = "h",y = -0.25))  %>%
        layout(xaxis = list(gridcolor = '#6c757d'),
               yaxis = list(gridcolor = '#6c757d')) %>% 
        add_annotations(
          text = paste0(last(indicator$long_name[which(indicator$long_name %in% indic)]),", ", spravka$unique_name[which(spravka$id == indicator$unit_id[which(indicator$long_name == last(indicator$long_name[which(indicator$long_name %in% indic)]))])]),
          x = 0,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(
            size = 19)
        ) %>%
        layout(title = FALSE, xaxis = list(title = "Год"))
      if (norm == 0){ f <- f  %>%   add_trace(x = df_1$year, y = df_1$fact, color =  df_1[,indicator:= paste0(indicator,"_2")]$indicator, mode = 'lines+markers', name = 'Факты')}#, colors = c("#30D5C8","#CCCCCC","#FFA500","#3D9970","#FD6969","#0000FF","#710000")
      print(f)
    }
    
  }
  
}

# 5. Подготовка карты         ####
func_choose_indicator_to_map <- function(indicator_to_map="Численность населения на 1 января текущего года"){
  if (length(indicator_to_map) == 0) {
    indicator_to_map = "Численность населения на 1 января текущего года"
  }
  
  df_for_map <- df %>%
    .[year == 2019 & location != "00000000" & name_indicator == indicator_to_map & !is.na(dyn_calc)]
  max_radius_to_circle = 69
  min_circle_to_map = min(df_for_map$dyn_calc)
  max_circle_to_map = max(df_for_map$dyn_calc)
  if (min_circle_to_map < 0) {
    min_circle_to_map = min_circle_to_map - min_circle_to_map + 1
    max_circle_to_map = max_circle_to_map - min_circle_to_map + 1
  }
  difference = max_circle_to_map - min_circle_to_map
  coefficient = max_radius_to_circle/difference #change
  min_dictanse = min_circle_to_map*coefficient
  if (max_circle_to_map < 70000) {
    df_for_map <- df_for_map %>% .[,radius := round((abs(df_for_map$dyn_calc*coefficient) - min_dictanse + 1)*500)]
  }else{
    df_for_map <- df_for_map %>% .[,radius :=  round((abs(df_for_map$dyn_calc*coefficient) - min_dictanse + 1)*500)]}
  df_for_map <- df_for_map %>% 
    .[,tooltip := paste0(name_location, ' <br>',name_indicator,' <br>',
                         dyn_calc)]
  return(df_for_map)
}
func_highlighting_city_point <- function(highlighting_city,q){
  if (q %in% highlighting_city){
    highlighting_city[which(highlighting_city == q)] <- " "
    df$color_of_city[which(df$long_name == q)] <- "#FFFFFFFF"
  }else{highlighting_city <- c(highlighting_city,q)}
  return(highlighting_city)
}
