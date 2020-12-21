### Desafio Coronavírus

### Limpando Plots, Console and Ambiente
rm(list = ls())
dev.off(dev.list()["RStudioGD"])
cat("\014")

### Lendo aquivos csv
caso_full <- read.csv("/AulaR/trabalho/caso_full.csv", fileEncoding = "UTF-8")
obito_cartorio <- read.csv("/AulaR/trabalho/obito_cartorio.csv", fileEncoding = "UTF-8")

### Selecionando observações
s_caso_full <- caso_full[which(caso_full$state == 'CE'), ]
s_obito_cartorio <- obito_cartorio[which(obito_cartorio$state == 'CE'), ]

### Retirando linhas com falta de observações
sum(complete.cases(s_caso_full))
s_caso_full <- s_caso_full[complete.cases(s_caso_full), ]

s_caso_full$date

# Função para conversão das colunas de datas
format_date <- function(date) {
  as.Date(date)
}

# Convertendo as colunas de datas
s_caso_full$date = format_date(s_caso_full$date)
s_obito_cartorio$date = format_date(s_obito_cartorio$date)


### Juntando seleções
j_casos <- merge(x = s_caso_full, y = s_obito_cartorio, by = "date", all.x = TRUE)

### Hora de criar gráficos
### Use a sua criatividade!!!

# Obtendo informações das tabelas
str(j_casos)
head(j_casos)


# Gráficos

# Gráfico 01

# Selecionando os números mais atuais para cada registro (dia 16/04/2020)
s_caso_full_last <- s_caso_full[which(s_caso_full$is_last == 'True'), ]

# Selecionando somente os registros referentes as cidades (excluindo o estado)
s_caso_full_last_city <- s_caso_full_last[which(s_caso_full_last$place_type == 'city'), ]

ggplot(s_caso_full_last_city, aes(x = reorder(city, -last_available_confirmed_per_100k_inhabitants), y = last_available_confirmed_per_100k_inhabitants)) +
  geom_col() +
  labs(title = "(01) Casos de Covid-19 no CE por 100k habitantes",
       subtitle = "Posição em 16/04/2020",
       x = "Cidade",
       y = "Quantidade") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico 02

# Selecionando somente os registros referentes às cidades (excluindo o estado)
s_caso_full_state <- s_caso_full[which(s_caso_full$place_type == 'state'), ]

ggplot(s_caso_full_state, aes(x = date, 
                              y = last_available_confirmed)) +
  geom_line(aes(group=1), colour="#56B4E9", size=1) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(02) Evolução de casos de Covid-19 - CE",
       x = "Dia",
       y = "Casos")

# Gráfico 03

ggplot(s_caso_full_state, aes(x = date, 
                              y = last_available_deaths)) +
  geom_line(aes(group=1), colour="#FF9999", size=1) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(03) Evolução de Mortes por Covid-19 - CE",
       x = "Dia",
       y = "Casos")

# Gráfico 04
ggplot() +
  geom_col(data=s_caso_full_state, aes(x = date, y = last_available_confirmed, fill="Doentes")) +
  geom_col(data=s_caso_full_state, aes(x = date, y = last_available_deaths, fill="Mortos")) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(04) Comparativo Doentes X Mortos por Covid-19 - CE",
       x = "Dia",
       y = "Quantidade") +
  guides(fill=guide_legend(title=NULL))

# Gráfico 05
s_caso_full_state$lethality = (s_caso_full_state$last_available_deaths 
                               / s_caso_full_state$last_available_confirmed)*100

ggplot() +
  geom_col(data=s_caso_full_state, aes(x=date, y=lethality), fill="#E69F00", size=1)+
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(05) Taxa de Letalidade por Covid-19 - CE",
       x = "Dia",
       y = "Letalidade em %")

# Gráfico 06

# Dividindo o número de novos casos no dia pelo total de casos confirmaos no dia anterior e salvando e uma nova coluna
s_caso_full_state$evolution_per_day = (s_caso_full_state$new_confirmed /
                                      (s_caso_full_state$last_available_confirmed - s_caso_full_state$new_confirmed))*100

ggplot(s_caso_full_state, aes(x = date, y = evolution_per_day)) +
  geom_line(aes(group=1), colour="#009E73", size=1) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(06) Taxa de Crescimento de Novos Casos de Covid-19 - CE",
       x = "Dia",
       y = "Crescimento em %")


# Gráfico 07

j_casos_state <- j_casos[which(j_casos$place_type == 'state'), ]

ggplot() +
  geom_col(data=j_casos_state, aes(x = date, y = new_deaths, fill="Mortes")) +
  geom_col(data=j_casos_state, aes(x = date, y = new_deaths_covid19, fill="Notificações")) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "(07) Mortes por Covid-19 X Casos Notificados nos Cartórios - CE",
       x = "Dia",
       y = "Quantidade") +
  guides(fill=guide_legend(title=NULL))
  

# Gráfico 08

# Total de mortes por doenças respiratórios (exceto Covid) por dia em 2019
j_casos_state$total_respiratory_deaths_except_covid_2019 = 
    j_casos_state$new_deaths_pneumonia_2019 + j_casos_state$new_deaths_respiratory_failure_2019

# Total de mortes por doenças respiratórios (exceto Covid) por dia em 2020
j_casos_state$total_respiratory_deaths_except_covid_2020 = 
  j_casos_state$new_deaths_pneumonia_2020 + j_casos_state$new_deaths_respiratory_failure_2020

ggplot() +
  geom_col(data=j_casos_state, aes(x = date, y = total_respiratory_deaths_except_covid_2019, fill="Respiratórias 2019")) +
  geom_col(data=j_casos_state, aes(x = date, y = total_respiratory_deaths_except_covid_2020, fill="Respiratórias 2020")) +
  geom_line(data=j_casos_state, aes(x = date, y = new_deaths_covid19, colour="Covid-19")) +
  scale_x_date(date_labels = "%d/%m", breaks = "1 day", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs(title = "(08) Mortes por Causas Respiratórias em 2019 e 2020 X Crescimento do Covid-19 - CE",
       x = "Dia",
       y = "Quantidade") +
  guides(fill=guide_legend(title=NULL))
