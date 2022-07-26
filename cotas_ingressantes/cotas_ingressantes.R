DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")
library("ggplot2")
theme_set(theme_bw())

AGRUPAMENTO <- DADOS_ALUNOS %>%  
  filter(affirmativePolicy != "N/A") %>%
  group_by(admissionYear, affirmativePolicy, gender) %>% summarise(quantidade = n())

PRESENCIAL <- AGRUPAMENTO %>% filter(admissionYear >= "2013.1", admissionYear < "2020")
REMOTO <- AGRUPAMENTO %>% filter(admissionYear >= "2020")
# criação de gráfico de cotas para os períodos presenciais das mulheres

MULHERES_PRESENCIAL <- PRESENCIAL %>% filter(gender == "Feminino")
ggplot(MULHERES_PRESENCIAL, aes(x=as.factor(admissionYear), y=quantidade, fill = affirmativePolicy)) +
  geom_col() + coord_flip()  + 
  labs(x = "Quantidade", y = "Período de ingresso", fill="Tipo de cota", title="Tipos de cotas utilizadas pelas mulheres nos períodos presenciais") + scale_y_continuous(breaks = seq(0,20,2))

HOMENS_PRESENCIAL <- PRESENCIAL %>% filter(gender=="Masculino") 
ggplot(HOMENS_PRESENCIAL, aes(x=as.factor(admissionYear), y=quantidade, fill = affirmativePolicy)) +
  geom_col() + coord_flip()  + 
  labs(x = "Quantidade", y = "Período de ingresso", fill="Tipo de cota", title="Tipos de cotas utilizadas pelos homens nos períodos presenciais") + scale_y_continuous(breaks = seq(0,60,5))

MULHERES_REMOTO <- REMOTO %>% filter(gender == "Feminino")
ggplot(MULHERES_REMOTO, aes(x=as.factor(admissionYear), y=quantidade, fill = affirmativePolicy)) +
  geom_col() + coord_flip()  + scale_fill_manual(values=c("#87CEEB", "#3CB371", "#9370DB", "#FF69B4")) +
  labs(x = "Quantidade", y = "Período de ingresso", fill="Tipo de cota", title="Tipos de cotas utilizadas pelas mulheres nos períodos remotos") + scale_y_continuous(breaks = seq(0,20,2))

HOMENS_REMOTO <- REMOTO %>% filter(gender=="Masculino") 
ggplot(HOMENS_REMOTO, aes(x=as.factor(admissionYear), y=quantidade, fill = affirmativePolicy)) +
  geom_col() + coord_flip()  + scale_fill_manual(values=c("#87CEEB", "#3CB371", "#9370DB", "#FF69B4")) + 
  labs(x = "Quantidade", y = "Período de ingresso", fill="Tipo de cota", title="Tipos de cotas utilizadas pelos homens nos períodos remotos") + scale_y_continuous(breaks = seq(0,60,5))
