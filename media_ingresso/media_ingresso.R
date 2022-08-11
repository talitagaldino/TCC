DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
library("dplyr")
library("ggplot2")
library("tidyverse")

DADOS_ALUNOS$admissionGrade = as.numeric(sub(",", ".", DADOS_ALUNOS$admissionGrade, fixed = TRUE))

media_geral_remoto <- DADOS_ALUNOS %>% filter(!is.na(admissionGrade)) %>% filter(admissionGrade != 0) %>%dplyr::filter(admissionYear >= "2020") %>%  group_by(admissionYear, gender) %>% dplyr::summarise(media_ingresso = round(mean(admissionGrade),2))

media_geral_presencial <- DADOS_ALUNOS %>% filter(!is.na(admissionGrade)) %>% filter(admissionGrade != 0) %>% dplyr::filter(admissionYear >= "2000", admissionYear < "2020") %>%  group_by(admissionYear, gender) %>% dplyr::summarise(media_ingresso = round(mean(admissionGrade),2))

ggplot(media_geral_remoto, aes(x=as.factor(admissionYear), y=media_ingresso, group=gender)) +
  geom_line(aes(color=gender))+
  scale_y_continuous(breaks = seq(500, 800, 50), limits=c(500,800)) + scale_colour_manual(values = c("#FF69B4","#3CB371")) +
  labs(x="Período", y="Média de ingresso", color="Sexo") +  geom_point(aes(color=gender))+ theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
                                                                                                 panel.grid.major = element_line(colour = "grey"))


ggplot(media_geral_presencial, aes(x=as.factor(admissionYear), y=media_ingresso, group=gender)) +
  geom_line(aes(color=gender))+
 scale_y_continuous(breaks = seq(500, 800, 50), limits=c(500,800)) + scale_colour_manual(values = c("#DDA0DD","#B0E0E6")) +
  labs(x="Período", y="Média de ingresso", color="Sexo") + geom_point(aes(color=gender))+ theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
                                                                                                panel.grid.major = element_line(colour = "grey"))

media_fem_tabela_remoto <- media_geral_remoto %>% filter(gender == "Feminino")
mean(as.numeric(media_fem_tabela_remoto$media_ingresso))

media_mas_tabela_remoto <- media_geral_remoto %>% filter(gender == "Masculino")
mean(as.numeric(media_mas_tabela_remoto$media_ingresso))

media_fem_tabela_presencial <- media_geral_presencial %>% filter(gender == "Feminino")
mean(as.numeric(media_fem_tabela_presencial$media_ingresso))

media_mas_tabela_presencial <- media_geral_presencial %>% filter(gender == "Masculino")
mean(as.numeric(media_mas_tabela_presencial$media_ingresso))
