#Framingham Risk Score Calculation

#I create a database of registries from the "datos_dtc" longitudinal database where each row is a visit of each patient to 
#health care providers to recieve interventions within the program activities. I filter registries by date and select relevant clinical 
#variables. This results in a database of all controls per each patient in 2018

controles_2018<-datos_dtc %>% filter(fecha_control_o_de_atencion_ >= "2018-01-01") %>%  
  select(id, fecha_control_o_de_atencion_, tas, tad, hba1c, erc, dm, hta, disl, col_ldl, col_hdl, 
  col_total, tfg, afic_sexo, afif_nacimiento, peso, talla, nom_dpto)


# A funtion was created to select the latest observation of each variable within the 
# database created in the step before

f_ult<-function(x){
  df<- controles_2018[c("id", "fecha_control_o_de_atencion_", x)]
  df <- df[!is.na(df[x]), ]
  df %>% arrange(fecha_control_o_de_atencion_) %>% group_by(id) %>% 
  filter_all(all_vars(!is.na(.))) %>% filter(row_number() == n()) %>% 
    select(-fecha_control_o_de_atencion_)
} 

list_var<-lapply(names(controles_2018)[-c(1,2)], f_ult)

controles_2018<- unique(controles_2018["id"])

for (i in 1:length(list_var)) {
  controles_2018<-merge(controles_2018, list_var[[i]], by= "id", all.x = T)  
}

# A age variable was created by selecting the id and birth date of each patient in the original database 
# and merged to the resulting database of the prevoius step

f_control<-datos_dtc %>% filter(fecha_control_o_de_atencion_ >= "2018-01-01") %>%  
  select(id, fecha_control_o_de_atencion_) %>%  group_by(id) %>% arrange(fecha_control_o_de_atencion_) %>% 
filter(row_number() == n()) %>% distinct()

controles_2018<- merge(controles_2018, f_control, by = "id") 
controles_2018<-controles_2018 %>% group_by(id) %>% 
  mutate(edad_afil = floor(as.numeric((fecha_control_o_de_atencion_ - afif_nacimiento)/365.25, units = "days"))) %>%
  filter(!nom_dpto %in% c("BOGOTÁ, D. C.", "SANTANDER", "CASANARE", "PUTUMAYO", "ANTIOQUIA", "NARIÑO", 
                          "LA GUAJIRA", "BOYACÁ", "CUNDINAMARCA", ""))





