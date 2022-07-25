DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
library("dplyr")
library("ggplot2")
library("fmsb")


#filtragem pela forma de evasão

motivos_evasao <- DADOS_ALUNOS %>% dplyr::filter(statusYear >= "2020.1") %>% group_by(statusCode, gender) %>% summarise(quantidade = n())
motivos_evasao_sem_ativos <- motivos_evasao %>% dplyr::filter(statusCode != "ATIVO")

quantidade_ativos <- motivos_evasao %>% dplyr::filter(statusCode == "ATIVO")

ggplot(motivos_evasao_sem_ativos, aes(x = statusCode, y=quantidade, fill = factor(gender))) + 
  geom_bar(stat="identity") + geom_col(position = "stack") + coord_flip() + 
  scale_y_continuous(limits = c(0,150)) +
  labs(title = "Situação acadêmica nos períodos remotos",
       x = "Situação",
       y = "Quantidade",
       fill = "Gênero") + 
  scale_fill_manual(values=c("#FF69B4",
                             "#00BFFF"))
