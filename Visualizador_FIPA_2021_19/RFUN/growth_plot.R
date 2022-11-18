growth_plot <- function(tabla, modelo){
# set up table and R calc ----
tabla_gral <- tabla %>% 
  filter(variable == 'total') %>% 
  select(Isla,Year,value) %>%
  dplyr::arrange(Year) %>% 
  st_drop_geometry() %>% 
  mutate(N = lead(Year - lag(Year))) %>%
  mutate(R = lead(log(value / lag(value))) / N)

tabla_modelo <- tabla_gral %>% filter(!is.na(R))

# Modelos ----
# ajuste de parámetros
Nt <- tabla_modelo$value # definimos Nt a partir de la cantidad de humanos
t <- seq(1, length(Nt)) # definimos el parámetro t a partir de la cantidad de Years BP desde el presente

###Exponencial ----
expo <- nls(R ~ r * t, data = tabla_modelo, start = c(r = 0.1), trace = TRUE)
summary(expo)
aic_expo <- AIC(expo)
  
# calculo pseudo R2
r2_expo <- cor(predict(expo), tabla_modelo$R) ^ 2
  
# calculo valores predichos
pd_expo <- predict(expo)
pd_expo <- c(pd_expo,NA)
  

###competencia ----
comp <- nls(R ~ b * (1 - (value/k)), data = tabla_modelo,
            start = c(b = 0.1, k = 0.1), trace = TRUE)
summary(comp)
aic_comp <- AIC(comp)
  
# calculo pseudo R2
r2_comp <- cor(predict(comp), tabla_modelo$R) ^ 2
  
# calculo valores predichos
pd_comp <- predict(comp)
pd_comp <- c(pd_comp,NA)

#bind data
tabla_gral <- tabla_gral %>% cbind.data.frame(pd_expo,pd_comp)


## conditional plots
if(length(modelo) == 1){
  if('Exponencial' %in% modelo){
    #base plot
    pp <- plot_ly(tabla_gral, x = ~Year, y = ~R, type = 'scatter', 
                  mode = 'lines+markers',name = 'Observado')
    rr <- round(r2_expo,digits = 5)
    
    r_label <- paste('r<sup>2</sup>:',rr)
    
    label_r <- list(
      xref = 'x',
      yref = 'y',
      x = max(tabla_gral$Year,na.rm = T)-2,
      y = max(tabla_gral$R,na.rm = T),
      xanchor = 'left',
      yanchor = 'middle',
      text = r_label,
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    
    pp <- pp %>% 
      add_trace(y = ~pd_expo, name = 'Predichos exponencial', mode = 'lines+markers') %>% 
      layout(xaxis = list(title = ""),
             yaxis = list (title = "Tasa de Crecimiento"),
             annotations = label_r,
             legend = list(orientation = 'h'))
    
  }
  if('Competencia' %in% modelo){
    #base plot
    pp <- plot_ly(tabla_gral, x = ~Year, y = ~R, type = 'scatter', 
                  mode = 'lines+markers',name = 'Observado')
    rr <- round(r2_comp,digits = 5)
    r_label <- paste('r<sup>2</sup>:',rr)
    
    label_r <- list(
      xref = 'x',
      yref = 'y',
      x = max(tabla_gral$Year,na.rm = T)-2,
      y = max(tabla_gral$R,na.rm = T),
      xanchor = 'left',
      yanchor = 'middle',
      text = r_label,
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    
    pp <- pp %>% 
      add_trace(y = ~pd_comp, name = 'Predichos competencia', mode = 'lines+markers') %>% 
      layout(xaxis = list(title = ""),
             yaxis = list (title = "Tasa de Crecimiento"),
             annotations = label_r,
             legend = list(orientation = 'h'))
  }
}

if(length(modelo) > 1){
  ##Exponencial
  #base plot
  pp <- plot_ly(tabla_gral, x = ~Year, y = ~R, type = 'scatter', 
                mode = 'lines+markers',name = 'Observado')
  rr_expo <- round(r2_expo,digits = 2)
  
  r_label_expo <- paste('r<sup>2</sup>:',rr_expo)
  
  label_r_expo <- list(
    xref = 'x',
    yref = 'y',
    x = tail(tabla_gral$Year,1)-1,
    y = tail(tabla_gral$pd_expo,2)[1],
    xanchor = 'left',
    yanchor = 'middle',
    text = r_label_expo,
    font = list(family = 'Arial',
                size = 16,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)
  
  ## competencia
  
  rr_comp <- round(r2_comp,digits = 2)
  
  r_label_comp <- paste('r<sup>2</sup>:',rr_comp)
  
  label_r_comp <- list(
    xref = 'x',
    yref = 'y',
    x = tail(tabla_gral$Year,1)-1,
    y = tail(tabla_gral$pd_comp,2)[1],
    xanchor = 'left',
    yanchor = 'above',
    text = r_label_comp,
    font = list(family = 'Arial',
                size = 16,
                color = 'rgba(67,67,67,1)'),
    showarrow = FALSE)
  
  #grafico completo
  pp <- pp %>% 
    add_trace(y = ~pd_comp, name = 'Predichos competencia', mode = 'lines+markers') %>%
    add_trace(y = ~pd_expo, name = 'Predichos exponencial', mode = 'lines+markers') %>% 
    layout(xaxis = list(title = ""),
           yaxis = list (title = "Tasa de Crecimiento"),
           legend = list(orientation = 'h')) %>% 
    layout(annotations = label_r_expo) %>% 
    layout(annotations = label_r_comp)
    
}

pp

}