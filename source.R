
# import multiple files into separate data frames 

import_assign <- function(format = ".xlsx"){
  
  files <- rio::import_list(dir(pattern = format))
  
  filenames <- list.files(pattern = format)
  
  names <- gsub(filenames, pattern = format, replacement = "")
  
  for(i in 1:length(files)){
    assign(paste0(names[[i]]), files |> purrr::pluck(i), envir = .GlobalEnv)
  }
  
}


# import multiple sheets from a excel file

read_sheets <- function(filename){
  
  files <- readxl::excel_sheets(filename) |>
           purrr::set_names() |>  
           purrr::map(readxl::read_excel, path = filename)
  
  assign("files", files, envir=.GlobalEnv)
}


# import multiple files and join all into a single df
## bind_rows & add a column with information about file_names

import_join <- function(format = ".xlsx", name.df) {
  
  files <- list.files(pattern = format)
  
  filenames <- list.files(pattern = format)
  
  names <- gsub(filenames, pattern = format, replacement = "")
  
  df <- dplyr::tibble(file_names = names) %>% 
    dplyr::mutate(data = purrr::map(files, read_excel)) %>% 
    tidyr::unnest() 
  
  assign(name.df, df, envir=.GlobalEnv)
}


# Función que obtiene frecuencia absoluta y relativa

frecuencia <- function(variable){
  transform(table(variable), 
            Porcentaje = round(prop.table(Freq)*100, 1)) %>% 
    arrange(desc(Freq))
}

# Función que grafica frecuencias absolutas 

frec_abs <- function(data, var) {
  
  data %>% 
    count(.data[[var]]) %>% 
    filter(.data[[var]] != "NA") %>% 
    ggplot(aes(x = n,
               y = reorder(.data[[var]], n),
               fill = reorder(.data[[var]], n))) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_d() +
    labs(y = NULL, x = NULL) +
    geom_text(aes(label = n), 
              color = "black", hjust = -0.4, size = 5) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
}

frec_abs_2 <- function(data, var) {
  
  data %>% 
    count(.data[[var]]) %>% 
    filter(.data[[var]] != "NA") %>% 
    ggplot(aes(x = n,
               y = reorder(.data[[var]], n),
               fill = reorder(.data[[var]], n))) +
    geom_col(show.legend = FALSE) +
    scale_fill_viridis_d() +
    labs(y = NULL, x = NULL) +
    geom_text(aes(label = n),
              color = "white", hjust = 1.2, size = 5) +
    scale_x_continuous(breaks = seq(0, 130, 50)) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
}


# Función que grafica frecuencias relativas 

frec_rel <- function(data, var) {
  
  data %>% 
    count(.data[[var]]) %>% 
    filter(.data[[var]] != "NA") %>% 
    mutate(porcentaje = n/sum(n)*100) %>% 
    ggplot(aes(x = n,
               y = reorder(.data[[var]], n),
               fill = reorder(.data[[var]], n))) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    scale_fill_viridis_d() +
    labs(y = NULL, x = NULL) +
    geom_text(aes(label =  paste0(round(porcentaje),"%")),
              color = "black", hjust = -0.4, size = 5) +
    scale_x_continuous(breaks = seq(0, 220, 50)) +
    theme_minimal() 
  
}

filter_pob <- function(data, filtro) {
  
  if (filtro == "salud") {
    
    data %>%  
      filter(POB_ESTUDIO_E %in% c("Pacientes",
                                  "Cuidadores",
                                  "Personal_salud")) 
    
  }
  
  if (filtro == "educa") {
    
    data %>% 
      filter(POB_ESTUDIO_E %in% c("Estudiantes",
                                  "Padres ",
                                  "Docentes",
                                  "Directivos"))
    
  }
  
}



# Función que grafica porcentajes de población estudiada


gg_pob <- function(data, var1, var2) {
  
data %>% 
  count(.data[[var1]], .data[[var2]]) %>% 
  mutate(porcentaje = round(n/sum(n),3)*100) %>% 
  ggplot(aes(x = porcentaje,
             y = .data[[var1]],
             fill = .data[[var2]])) +
  geom_bar(stat = "identity", 
           position = "fill") +
  scale_fill_viridis_d() +
  labs(y = NULL,
       x = NULL,
       fill = "Población estudiada") +
  geom_text(aes(label = porcentaje), 
            size = 6,
            color = "white", 
            position = position_fill(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.justification = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())
  
}


# Función para analizar preguntas con respuesta múltiple

multResp<- function(data,var_start,var_end){
  data.frame(Freq = colSums(data[var_start:var_end]),
             Pct.of.Resp  = (colSums(data[var_start:var_end])/sum(data [var_start:var_end]))*100,
             Pct.of.Cases = (colSums(data[var_start:var_end])/nrow(data[var_start:var_end]))*100)
}


