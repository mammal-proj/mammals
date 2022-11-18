tablaPie <- function(tabla, field.list, scale){
  
  if(length(field.list) > 0){
    if(all(c('adultos','macho') %in% field.list) | all(c('adultos','hembra') %in% field.list) | all(c('adultos','macho','hembra') %in% field.list)){
      field.list <- field.list[!(field.list %in% c('adultos'))]
    }
  if(scale == 'Lobera'){
    tabla_prep <- tabla %>% dplyr::select(all_of(field.list)) 
    tabla_melt <- tabla_prep %>% reshape2::melt(variable.name = 'Clase',value.name = 'Conteo') %>% 
      dplyr::select(Clase,Conteo)
    
  }
  if(scale == 'Islas'){
    tabla_prep <- tabla %>% dplyr::select(all_of(field.list)) %>% summarise_all(.funs = sum, na.rm = T)
    
    tabla_melt <- tabla_prep %>% 
      reshape2::melt(variable.name = 'Clase',
                     value.name = 'Conteo') %>%  
      dplyr::select(Clase,Conteo)
  }
    }
  if(length(field.list)==0){
    tabla_melt <- data.frame()
  }
  
   tabla_melt
   
}