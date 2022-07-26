DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")
theme_set(theme_bw())

library("dplyr")

limpa_dados = function(colunasMantidas, dataFrame){
  novaTabela <- dataFrame[,colunasMantidas]
  
  return (novaTabela)
}

DADOS_ALUNOS <- limpa_dados(c("gender", "anonymized_registration"), DADOS_ALUNOS)
DESEMPENHO <- limpa_dados(c("anonymized_registration", "subjectCode", "term", "status"), DESEMPENHO)

DISCIPLINAS <- merge(x = DESEMPENHO, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

TRANCAMENTOS_REMOTOS <- DISCIPLINAS %>% filter(term >= "2020") %>% filter(status == "Trancado" | status == "Cancelado")

ggplot(TRANCAMENTOS_REMOTOS, aes(x=as.factor(term), fill=gender)) + geom_bar() + facet_wrap(~status) +
  labs(x = "Período", y = "Quantidade de alunos", title = "Quantidade de trancamentos e cancelamentos nos períodos REMOTOS", fill="Gênero") +
  scale_fill_manual(values=c("#FF69B4","#3CB371")) + scale_y_continuous(breaks = seq(0,400,25))

TRANCAMENTOS_PRESENCIAIS <- DISCIPLINAS %>% filter(term >= "2000.1", term < "2020") %>% filter(status == "Trancado" | status == "Cancelado")

ggplot(TRANCAMENTOS_PRESENCIAIS, aes(x=as.factor(term), fill=gender)) + geom_bar() + facet_wrap(~status) +
  labs(x = "Período", y = "Quantidade de alunos", title = "Quantidade de trancamentos e cancelamentos nos períodos PRESENCIAIS", fill="Gênero") +
  scale_fill_manual(values=c("#DDA0DD","#B0E0E6")) + coord_flip() + scale_y_continuous(breaks = seq(0,150,25))
