DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ",")
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

gera_tabela_media = function(data){
  medias <- data %>% group_by(name,gender, subjectCode) %>% filter(grade != "-") %>% summarise(media = round(mean(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2))
  
  return (medias)
}

gera_tabela_mediana = function(data){
  medias <- data %>% group_by(name,gender, subjectCode) %>% filter(grade != "-") %>% summarise(mediana = round(median(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2))
  
  return (medias)
}

gera_grafico_desempenho = function(data, titulo, cores){
  data %>%
    ggplot(aes(x = media, y = reorder(name, media))) +
    geom_line(aes(group = name)) +
    geom_point(aes(color = factor(gender)), size=4) + theme_bw() + theme(panel.grid.major.y = element_line(linetype = "dashed")) +
     labs(title = titulo,
          x = "Média",
          y = "Disciplina", color="Gênero") + scale_colour_manual(values = cores) +
    scale_x_continuous(breaks=seq(5.0, 10.0, 0.2), limits=c(5, 10))
  
}

gera_grafico_MEDIANAS = function(data, cores){
  data %>%
    ggplot(aes(x = mediana, y = reorder(name, mediana))) +
    geom_line(aes(group = name)) +
    geom_point(aes(color = factor(gender)), size=4) + theme_bw() + theme(panel.grid.major.y = element_line(linetype = "dashed")) +
    labs(x = "Mediana",
         y = "Disciplina", color="Gênero") + scale_colour_manual(values = cores) +
    scale_x_continuous(breaks=seq(5.0, 10.0, 0.2), limits=c(5, 10)) + theme(axis.text.y = element_text(angle = 30), panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
                                                                          panel.grid.major = element_line(colour = "grey"))
  
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
CC_PRESENCIAL<- merge(x = CC_PRESENCIAL, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE) %>% filter(grade != "-")

GERAIS_PRESENCIAL <- tabela_disciplinas_presencial(obrigatorias_geral, merge_disc_notas, "2000.1", "2020" )
#fazendo o merge para adicionar o genero
GERAIS_PRESENCIAL <- merge(x = GERAIS_PRESENCIAL, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE) %>% filter(grade != "-")

#para os períodos remotos, teremos

CC_REMOTO <- tabela_disciplinas_remoto(obrigatorias_computacao, merge_disc_notas, "2020")
#fazendo o merge para adicionar o genero
CC_REMOTO <- merge(x = CC_REMOTO, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE) %>% filter(grade != "-")

GERAIS_REMOTO <- tabela_disciplinas_remoto(obrigatorias_geral, merge_disc_notas, "2020") 
#fazendo o merge para adicionar o genero

GERAIS_REMOTO <- merge(x = GERAIS_REMOTO, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE) %>% filter(grade != "-")

# Criação de tabela adicionando a média por gênero em cada uma dessas disciplinas 

MEDIAS_GERAIS_PRESENCIAL <- gera_tabela_media(GERAIS_PRESENCIAL)
MEDIAS_GERAIS_REMOTO <- gera_tabela_media(GERAIS_REMOTO)

MEDIAS_CC_PRESENCIAL <- gera_tabela_media(CC_PRESENCIAL)
MEDIAS_CC_REMOTO <- gera_tabela_media(CC_REMOTO)

PRESENCIAL_MEDIANA_GERAIS <- gera_tabela_mediana(GERAIS_PRESENCIAL)
REMOTO_MEDIANA_GERAIS <- gera_tabela_mediana(GERAIS_REMOTO)

PRESENCIAL_MEDIANA_CC <- gera_tabela_mediana(CC_PRESENCIAL)
REMOTO_MEDIANA_CC <- gera_tabela_mediana(CC_REMOTO)

# para os periodos presenciais teremos

gera_grafico_desempenho(MEDIAS_GERAIS_PRESENCIAL, "Média dos alunos em disciplinas de outros departamentos em períodos presenciais", c("#DA81F5", "#01DF74"))
gera_grafico_desempenho(MEDIAS_CC_PRESENCIAL, "Média dos alunos em disciplinas do departamento de computação em períodos presenciais", c("#DA81F5", "#01DF74"))

# já para os períodos remotos

gera_grafico_desempenho(MEDIAS_GERAIS_REMOTO, "Média dos alunos em disciplinas de outros departamentos em períodos REMOTOS", c("#F5A9E1", "#819FF7"))
gera_grafico_desempenho(MEDIAS_CC_REMOTO, "Média dos alunos em disciplinas do departamento de computação em períodos REMOTOS", c("#F5A9E1", "#819FF7"))

#MEDIANAS
# para os periodos presenciais teremos

gera_grafico_MEDIANAS(PRESENCIAL_MEDIANA_GERAIS, c("#DA81F5", "#01DF74"))
gera_grafico_MEDIANAS(PRESENCIAL_MEDIANA_CC,  c("#DA81F5", "#01DF74"))

# já para os períodos remotos

gera_grafico_MEDIANAS(REMOTO_MEDIANA_GERAIS, c("#F5A9E1", "#819FF7"))
gera_grafico_MEDIANAS(REMOTO_MEDIANA_CC, c("#F5A9E1", "#819FF7"))

