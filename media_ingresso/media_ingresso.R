DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
library("dplyr")
library("ggplot2")
library("tidyverse")

theme_set(theme_minimal())

DADOS_ALUNOS$admissionGrade = as.numeric(sub(",", ".", DADOS_ALUNOS$admissionGrade, fixed = TRUE))


media_geral <- DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2020.1") %>% dplyr::filter(admissionGrade != "-") %>% group_by(admissionYear) %>% dplyr::summarise(media_ingresso = mean(admissionGrade))

media_feminina <- DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2020.1") %>% dplyr::filter(admissionGrade != "-") %>% dplyr::filter(gender == "Feminino") %>% group_by(admissionYear) %>% dplyr::summarise(media_ingresso = mean(admissionGrade))

ggplot(data=media_geral, aes(x=as.factor(admissionYear), y=media_ingresso,group=1)) + 
  geom_line(color = "#fc0fc0", size=1.5) +
  geom_line(aes(y=media_feminina$media_ingresso), color="steelblue") + 
  labs(x = "Período", y = "Média da nota de ingresso", title = "Comparação entre média de ingresso feminina e geral do curso") + stat_summary(fun=sum, geom="line") 

