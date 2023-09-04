#pacotes usados:
library(jsonlite)
library(yaml)
library(purrr)
library(tidyverse)

#Parte0:
require(jsonlite)
fromJSON("dados/relatorio1.json",simplifyDataFrame = FALSE)
fromJSON("dados/relatorio1.json")

#Parte1:
file.exists("configuracoes.yaml")

# Parte2:
lista = read_yaml("configuracoes.yaml")
##lista com nome do 4 relatorioX.json 

##lista com 4 listas:
ler = function(lista){
  path = "dados/"
  read_json(paste0(path,lista))
}

arquivos = map(lista$arquivos,ler)

## juntando as listas
arquivos_junto = c(arquivos[[1]], arquivos[[2]],arquivos[[3]],arquivos[[4]])


# Parte03: conversão para data frame

junta <- function(x){
  map(arquivos[[x]], data.frame)
}

df <- list_rbind(map(1:length(arquivos), function(x) {
  df <- junta(x)
  list_rbind(df)
}))

# Parte4: gráfico de dispersão dos horários de recalibragem

registros = df %>% unique() %>% 
  mutate(horario = lubridate::ymd_hms(horario)) %>%
  separate(col = horario, into = c("Data", "Hora"), sep = " ")


registros %>% filter(evento == "recalibragem") %>%
  ggplot(aes(x = Hora, y = evento))+ geom_point()+
  facet_wrap(~Data)



# Parte5: modelo de regressão
combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
l <- unlist(apply(combin, 1, list), recursive = FALSE)
conbin_res = lapply(l, function(x) names(x)[x])

modelos <- list()

for(i in 1:length(combin_res)){
  registros_lm <- registros %>% select(c("intensidade", combin_res[[i]])) %>%
    drop_na()
  modelos[[i]] <- lm(intensidade ~ ., registros_lm)
}

## prediz intensidade:
prediz_intens <- function(vars){
  combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
  l <- unlist(apply(combin, 1, list), recursive = FALSE)
  combin_res <- lapply(l, function(x) names(x)[x])
  argumento <- ""
}



                          


