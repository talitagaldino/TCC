DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ";")
DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")

library("dplyr")

#array de colunas que eu vou querer manter de cada tabela para a construção da minha nova tabela

cols.want_dados_alunos <- c("gender", "anonymized_registration")
cols.want_disciplinas <- c("curriculumCode", "subjectCode", "name")
cols.want_desempenho <- c("anonymized_registration", "subjectCode", "term", "grade", "status")

#mantendo apenas as colunas selecionadas 

DADOS_ALUNOS <-  DADOS_ALUNOS[,cols.want_dados_alunos]
DESEMPENHO <-  DESEMPENHO[,cols.want_desempenho]
DISCIPLINAS <-  DISCIPLINAS[,cols.want_disciplinas]

#array de disciplinas obrigatórias de computação e gerais

obrigatorias_computacao <- c("1411305", "1411172", "1411311", "1411312", "1411306", "1411179", "1411180", "1411181", "1109113", "1411167", "1411168", "1411167", "1411168", "1411308", "1411171")
obrigatorias_geral <- c("1109049", "1109035", "1109126", "1109131", "1108089", "1114129", "1114107")

#merjando as tabelas de desempenho e disciplinas para que agora tenha o nome da disciplina associado ao aluno e a nota

merge_disc_notas = merge(x = DESEMPENHO, y = DISCIPLINAS, by = "subjectCode",all = TRUE)

#pegando apenas as disciplinas que foram colocadas no array

obgtr_cc_presencial <- merge_disc_notas %>% dplyr::filter(subjectCode %in% obrigatorias_computacao) %>% filter(term > "2000.1")
obgtr_geral_presencial <- merge_disc_notas %>% dplyr::filter(subjectCode %in% obrigatorias_geral) %>% filter(term > "2000.1")

#fazendo o merge para adicionar o genero

tabela_final_obrigatorias_cc_presencial <- merge(x = obgtr_cc_presencial, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)
tabela_final_obrigatorias_gerais_presencial <- merge(x = obgtr_geral_presencial, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

#para os períodos remotos, teremos

obgtr_cc_remoto <- merge_disc_notas %>% dplyr::filter(subjectCode %in% obrigatorias_computacao) %>% filter(term > "2020.1")
obgtr_geral_remoto <- merge_disc_notas %>% dplyr::filter(subjectCode %in% obrigatorias_geral) %>% filter(term > "2020.1")

#fazendo o merge final para adicionar o genero

tabela_final_obrigatorias_cc_remoto <- merge(x = obgtr_cc_remoto, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)
tabela_final_obrigatorias_gerais_remoto <- merge(x = obgtr_geral_remoto, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

