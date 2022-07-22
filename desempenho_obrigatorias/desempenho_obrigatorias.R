DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ";")
DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")

library("dplyr")

#função para manter nas tabelas apenas as colunas selecionadas 

limpa_dados = function(colunasMantidas, dataFrame){
  novaTabela <- dataFrame[,colunasMantidas]
  
  return (novaTabela)
}

tabela_disciplinas_presencial = function(disciplinas, dataFrame, periodo_inicial, periodo_final){
  tabela <- dataFrame %>% dplyr::filter(subjectCode %in% disciplinas) %>% filter(term >= periodo_inicial, term < periodo_final)
  
  return(tabela)
}

tabela_disciplinas_remoto = function(disciplinas, dataFrame, periodo_inicial){
  tabela <- dataFrame %>% dplyr::filter(subjectCode %in% disciplinas) %>% filter(term >= periodo_inicial) %>% filter(status != "Em Curso")
  
  return (tabela)
}

DADOS_ALUNOS <- limpa_dados(c("gender", "anonymized_registration"), DADOS_ALUNOS)
DISCIPLINAS <- limpa_dados(c("curriculumCode", "subjectCode", "name"), DISCIPLINAS)
DESEMPENHO <- limpa_dados(c("anonymized_registration", "subjectCode", "term", "grade", "status"), DESEMPENHO)

#array de disciplinas obrigatórias de computação e gerais

obrigatorias_computacao <- c("1411305", "1411172", "1411311", "1411312", "1411306", "1411179", "1411180", "1411181", "1109113", "1411167", "1411168", "1411167", "1411168", "1411308", "1411171")
obrigatorias_geral <- c("1109049", "1109035", "1109126", "1109131", "1108089", "1114129", "1114107")

#merjando as tabelas de desempenho e disciplinas para que agora tenha o nome da disciplina associado ao aluno e a nota

merge_disc_notas = merge(x = DESEMPENHO, y = DISCIPLINAS, by = "subjectCode",all = TRUE)

CC_PRESENCIAL <- tabela_disciplinas_presencial(obrigatorias_computacao, merge_disc_notas, "2000.1", "2020")
#fazendo o merge para adicionar o genero
CC_PRESENCIAL<- merge(x = CC_PRESENCIAL, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

GERAIS_PRESENCIAL <- tabela_disciplinas_presencial(obrigatorias_geral, merge_disc_notas, "2000.1", "2020" )
#fazendo o merge para adicionar o genero
GERAIS_PRESENCIAL <- merge(x = GERAIS_PRESENCIAL, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

#para os períodos remotos, teremos

CC_REMOTO <- tabela_disciplinas_remoto(obrigatorias_computacao, merge_disc_notas, "2020")
#fazendo o merge para adicionar o genero
CC_REMOTO <- merge(x = CC_REMOTO, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

GERAIS_REMOTO <- tabela_disciplinas_remoto(obrigatorias_geral, merge_disc_notas, "2020")
#fazendo o merge para adicionar o genero
GERAIS_REMOTO <- merge(x = GERAIS_REMOTO, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

