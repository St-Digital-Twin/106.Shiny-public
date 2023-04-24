try(source("2.libraries.r", encoding = "UTF-8"))
try(source("2.R/1.function.R", encoding = "UTF-8"))
t <- list(family = "Panton", size = 14, color = 'white')
# 2. Исходные данные             ####
df <- qread("1.Data/1.input_data") %>% .[,year := as.numeric(year)] %>% .[year<2050]
location <- qread("1.Data/2.location") %>% .[str_length(oktmo) == 8]
indicator <- qread("1.Data/3.indicator")
new_dyn_calc <- qread("1.Data/4.integ_kor_matrics")
spravka <- qread("1.Data/5.idin_izm")
indicator2 <- indicator
location2 <- location

# rt <- merge(df[,-c("fact", "dyn_calc")],qread("1.Data/test1")%>% .[,year := as.numeric(year)], by.x = c("indicator", "location", "year"), by.y = c("indicator", "oktmo", "year"),all.x = TRUE)

# 2.1. Справочники              ####
indicator_input <- merge(as.data.table(unique(df$indicator)),indicator[,c("code","long_name")], by.x = "V1", by.y = "code", all.x = TRUE)$long_name
location_input <- merge(as.data.table(unique(df$location)),location[,c("oktmo","long_name")], by.x = "V1", by.y = "oktmo", all.x = TRUE)$long_name


location_inp_2 <- merge(as.data.table(unique(new_dyn_calc$location)[-1L]), location[,c("oktmo","long_name")], by.x = "V1", by.y = "oktmo", all.x = TRUE) %>% 
  .[long_name %in% location_input] %>% 
  .[,c("long_name")] %>% 
  deframe()
indicator_inp_2 <- merge(as.data.table(unique(new_dyn_calc$to)[-1L]), indicator[,c("code","long_name")], by.x = "V1", by.y = "code", all.x = TRUE) %>% 
  .[long_name %in% indicator_input] %>% 
  .[,c("long_name")] %>% 
  deframe() 

key = "pk.eyJ1IjoibmlraXRhLWJ1cmFrb3YiLCJhIjoiY2t3MGxnMTBpYjdpbTJ1cXdjeGN2eGJvNCJ9.faoxOUVwQQ4QK46LGBhYzg"
df_to_map <- func_choose_indicator_to_map()

