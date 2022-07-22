DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ";")
DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")

library("dplyr")

grupo = DESEMPENHO %>% dplyr::filter(term >= "2020.0") %>% dplyr::filter(status == 'Cancelado') %>% dplyr::group_by(subjectCode) %>% summarise(alunos = n())

nome_canceladas = merge(grupo, DISCIPLINAS, all.x = TRUE)

graduadas = DADOS_ALUNOS %>%  dplyr::filter(statusYear >= "2020.1") %>% dplyr::filter(gender == 'Feminino') %>% dplyr::filter(statusCode == 'GRADUADO') %>% summarise(mulheres = n())
graduados = DADOS_ALUNOS %>%  dplyr::filter(statusYear >= "2020.1") %>% dplyr::filter(gender == 'Masculino') %>% dplyr::filter(statusCode == 'GRADUADO') %>% summarise(homens = n())

ingresso_meninas_ead = DADOS_ALUNOS  %>% dplyr::filter(gender == 'Feminino') %>% dplyr::filter(admissionYear >= '2020.1') %>% summarise(ingressantes = n())
ingresso_meninos_ead = DADOS_ALUNOS  %>% dplyr::filter(gender == 'Masculino') %>% dplyr::filter(admissionYear >= '2020.1') %>% summarise(ingressantes = n())
