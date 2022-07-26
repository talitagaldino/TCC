DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")

limpa_dados = function(colunasMantidas, dataFrame){
  novaTabela <- dataFrame[,colunasMantidas]
  
  return (novaTabela)
}

TABELA_EVASAO <- DADOS_ALUNOS %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO", statusCode != "CONCLUIDO - NAO COLOU GRAU", statusCode != "CANCELADO NOVO INGRESSO MESMO CURSO
")

EVASAO_PRESENCIAL <- TABELA_EVASAO %>% filter(statusYear >= "2000.1", statusYear < "2020")
EVASAO_PRESENCIAL <- limpa_dados(c("gender", "anonymized_registration", "completedTerms", "statusYear", "statusCode"), EVASAO_PRESENCIAL)

EVASAO_REMOTO <- TABELA_EVASAO %>% filter(statusYear >= "2020") 
EVASAO_REMOTO <- limpa_dados(c("gender", "anonymized_registration", "completedTerms", "statusYear", "statusCode"), EVASAO_REMOTO)

EVASAO_REMOTO_QUANTIDADE <- EVASAO_REMOTO %>% group_by(gender, completedTerms, statusCode) %>% summarise(quantidade = n())
cores_remoto <- c("#FF69B4","#3CB371")
ggplot(EVASAO_REMOTO_QUANTIDADE,aes(x=completedTerms, y=quantidade, fill = gender)) +
  geom_col() + 
  scale_fill_manual(values= cores_remoto) +
  labs(title = "Período de evasão dos alunos nos períodos REMOTOS",
       y = "Quantidade de alunos",
       x = "Períodos cursados",
       fill = "Gênero") + scale_x_continuous(breaks=seq(0, 15,1)) +
  scale_y_continuous(breaks=seq(0, 25,1))

EVASAO_PRESENCIAL_QUANTIDADE <- EVASAO_PRESENCIAL %>% group_by(gender, completedTerms, statusCode) %>% summarise(quantidade = n())
cores_presencial <- c("#DDA0DD","#B0E0E6")
ggplot(EVASAO_PRESENCIAL_QUANTIDADE,aes(x=completedTerms, y=quantidade, fill = gender)) +
  geom_col() + 
  scale_fill_manual(values= cores_presencial) +
  labs(title = "Período de evasão dos alunos nos períodos PRESENCIAIS",
       y = "Quantidade de alunos",
       x = "Períodos cursados",
       fill = "Gênero") + scale_x_continuous(breaks=seq(0, 16,1)) +
  scale_y_continuous(breaks=seq(0, 250,25))
