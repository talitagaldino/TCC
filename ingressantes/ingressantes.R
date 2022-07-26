DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")
library("ggplot2")

ingressantes_remoto = DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2020.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)
ingressantes_porcentagem_remoto = ingressantes_remoto %>% group_by(admissionYear) %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))

ingressantes_presencial = DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2000.1", admissionYear < "2020.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)  
ingressantes_presencial_porcentagem = ingressantes_presencial  %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))

ggplot(ingressantes_porcentagem_remoto, aes(x=as.factor(admissionYear), y=Quantidade, fill=gender)) + 
  geom_col(position = "dodge") +   scale_fill_manual(values= c("#F5A9E1", "#819FF7")) +
  labs(title = "Ingressantes nos períodos remotos",
       x = "Período",
       y = "Quantidade de ingressantes",
       fill = "Gênero") + coord_flip() +
  scale_y_continuous(breaks=seq(0, 90, 10)) + theme_minimal()


ggplot(ingressantes_presencial_porcentagem, aes(x=as.factor(admissionYear), y=Quantidade, fill=gender)) + 
  geom_col(position = "dodge") +   scale_fill_manual(values= c("#DA81F5", "#01DF74")) +
  labs(title = "Ingressantes nos períodos presenciais",
       x = "Período",
       y = "Quantidade de ingressantes",
       fill = "Gênero") + coord_flip() + 
  scale_y_continuous(breaks=seq(0, 100, 10)) + theme_minimal()

