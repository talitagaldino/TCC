DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")

limpa_dados = function(colunasMantidas, dataFrame){
  novaTabela <- dataFrame[,colunasMantidas]
  
  return (novaTabela)
}

TABELA_EVASAO <- DADOS_ALUNOS %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO", statusCode != "CONCLUIDO - NAO COLOU GRAU")

EVASAO_PRESENCIAL <- TABELA_EVASAO %>% filter(statusYear >= "2000.1", statusYear < "2020")
EVASAO_PRESENCIAL <- limpa_dados(c("gender", "anonymized_registration", "completedTerms", "statusYear", "statusCode"), EVASAO_PRESENCIAL)

EVASAO_REMOTO <- TABELA_EVASAO %>% filter(statusYear >= "2020") 
EVASAO_REMOTO <- limpa_dados(c("gender", "anonymized_registration", "completedTerms", "statusYear", "statusCode"), EVASAO_REMOTO)
