DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ",")
DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")
theme_set(theme_bw())

library("dplyr")
library("ggplot2")

limpa_dados = function(colunasMantidas, dataFrame){
  novaTabela <- dataFrame[,colunasMantidas]
  
  return (novaTabela)
}

cria_tabela_grupo_optativa = function(data, optativas, nome_grupo){
  nova_tabela <- data %>% dplyr::filter(name %in% optativas) %>% mutate(grupo = nome_grupo)
  return(nova_tabela)
}

cria_tabela_contagem_genero = function(data){
  tabela_contagem <- data %>% dplyr::group_by(name, gender, grupo) %>% summarise(quantidade = n())
  return(tabela_contagem)
}

cria_grafico_contagem = function(data, title, limite_maximo, intervalo, cores){
  ggplot(data, aes(x = name, quantidade, fill = gender)) + geom_bar(stat="identity", position = "dodge") + coord_flip() +
    labs(title=title, x = "Disciplinas", fill = "Sexo", y = "Quantidade de alunos") + scale_y_continuous(breaks = seq(0, limite_maximo, by = intervalo)) +
    scale_fill_manual(values=cores)
}

grafico_quantidade_grupos = function(data, limite_maximo, intervalo, cores){
  ggplot(data, aes(x=grupo,fill=gender))+
    coord_flip() + labs(x = "Grupo de optativas", fill = "Sexo", y = "Quantidade") +
    geom_bar(position="dodge") + scale_y_continuous(breaks = seq(0, limite_maximo, by = intervalo)) + scale_fill_manual(values=cores) + theme(legend.position = "top", panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),                                                                                                                                          panel.grid.major = element_line(colour = "grey"))
}

calcula_medias = function(data){
  medias <- data %>% filter(grade != "-") %>% group_by(gender, grupo) %>% 
    summarise(media = round(mean(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2), dp = round(sd(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2), mediana = round(median(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2)) %>%
    mutate(coeficiente = (dp/media)*100)
  return (medias) 
}

calcula_medianas = function(data){
  medianas <- data %>% filter(grade != "-") %>% group_by(gender, grupo) %>% summarise(mediana = round(median(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2))

  return (medianas) 
}

coeficiente_geral = function(data){
  result <- data %>% filter(grade != "-") %>% group_by(gender) %>% 
    summarise(media = round(mean(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2), dp = round(sd(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2), mediana = round(median(as.numeric(sub(",", ".",grade, fixed = TRUE))), 2)) %>%
    mutate(coeficiente = (dp/media)*100)
}

cria_grafico_medias = function(data, cores, title){
  data %>%
    ggplot(aes(x = media, y = reorder(grupo, media))) +
    geom_line(aes(group = grupo)) +
    geom_point(aes(color = gender), size=4) + theme(panel.grid.major.y = element_line(linetype = "dashed")) +
    scale_colour_manual(values = cores) +
    scale_x_continuous(breaks=seq(5.0, 10.0, 0.5), limits=c(5, 10)) +
    labs(title=title, x = "Média", color = "Sexo", y = "Grupo de optativas")
}

cria_grafico_medianas = function(data, cores){
  data %>%
    ggplot(aes(x = mediana, y = reorder(grupo, mediana))) +
    geom_line(aes(group = grupo)) +
    geom_point(aes(color = factor(gender)), size=4) + theme_bw() + theme(panel.grid.major.y = element_line(linetype = "dashed")) +
    labs(x = "Mediana",
         y = "Disciplina", color="Sexo") + scale_colour_manual(values = cores) +
    scale_x_continuous(breaks=seq(5.0, 10.0, 0.5), limits=c(5, 10)) + theme(legend.position = "top", panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
                                                                            panel.grid.major = element_line(colour = "grey"))
}



DADOS_ALUNOS <- limpa_dados(c("gender", "anonymized_registration"), DADOS_ALUNOS)
DISCIPLINAS <- limpa_dados(c("curriculumCode", "subjectCode", "name"), DISCIPLINAS)
DESEMPENHO <- limpa_dados(c("anonymized_registration", "subjectCode", "term", "grade", "status"), DESEMPENHO)

merge_disc_notas = merge(x = DESEMPENHO, y = DISCIPLINAS, by = "subjectCode")
DISC_GENERO_NOTA = merge(x = merge_disc_notas, y = DADOS_ALUNOS, by = "anonymized_registration",all.x = TRUE)

array_opt_dev_arq = c('ARQUITETURA DE SOFTWARE','PRINCIPIOS DE DESENVOLVIMENTO WEB','DESENV. DE APLICACOES CORP. AVANCADAS','INTERFACE HOMEM-MÁQUINA','RECUP. DE INFORMACAO E BUSCA NA WEB','TECC(PROGRAMAÇÃO PARA WEB)','TECC(JOGOS DIGITAIS)')
array_opt_dados = c('PROGRAMACAO EM BANCOS DE DADOS','ADM.DE SIST.GERENC.DE BANCO DE DADOS','BANCO DE DADOS II','RECONHEC. DE PADROES E REDES NEURAIS','SISTEMAS DE APOIO A DECISÃO','SISTEMAS DE INFORMAÇÕES GEOGRÁFICAS','TECC(CIENCIA DE DADOS DESCRITIVA)','TECC(CIÊNCIA DE DADOS PREDITIVA)','TECC(PROCESSAMENTO DE LINGUAGEM NATURAL)','TECC(VISUALIZAÇÃO DE DADOS)')
array_opt_gov_emp = c('ECONOMIA DE TECNOLOGIA DA INFORMACAO','EMPREENDEDORISMO EM SOFTWARE','TECC(GESTAO DE PROJETOS)','TECC(GOVERNANCA DA INTERNET)','TEEC(TRANSFORMAÇÃO DIGITAL)')
array_opt_infra_redes = c('DES DE SOFTWARE INTEG A OPER DA INFRAEST','GERENCIA DE REDES','INTERCONEXAO DE REDES DE COMPUTADORES','PROJETO DE REDES DE COMPUTADORES','PROVIS. E OPER. DE INFRAESTRUTURAS','SISTEMAS DISTRIBUIDOS','AVAL.DE DESEMPENHO DE SISTEMAS DISCRETOS')
array_opt_aplicacoes_obrigatorias = c('TECC(APLIC DE PARAD DE LING DE PROGRAM.)','TECC(APLICACOES DE TEORIA DOS GRAFOS)','TECC(PROJETO DE SISTEMAS OPERACIONAIS)')
array_opt_qualidade_sistema = c('VERIFICACAO E VALIDACAO DE SOFTWARE','SEGURANCA DE SISTEMAS','ADMINISTRACAO DE SISTEMAS','MÉTODOS E SOFTWARE NUMÉRICOS','METODOS FORMAIS','OTIMIZAÇÃO','TECC(PERCEPCAO COMPUTACIONAL','VISAO COMPUTACIONAL')
array_opt_aleatorias = c('COMPUTAÇÃO E MÚSICA','COMPUTAÇÃO GRÁFICA','ALGORITMOS AVANCADOS I','ALGORITMOS AVANCADOS II','ALGORITMOS AVANCADOS III','ALGORITMOS AVANCADOS IV')

# essas tabelas tem todos os alunos que a pagaram com suas notas, genero e o periodo que aquela disciplina foi paga
#isso permite a análise de DESEMPENHO dos alunos por genero

TAB_DEV_ARQ <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_dev_arq, "DESENVOLVIMENTO E ARQUITETURA")
TAB_DADOS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_dados, "DADOS")
TAB_GOV_EMP <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_gov_emp, "GOVERNANÇA E EMPREENDEDORISTO")
TAB_INFRA_REDES <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_infra_redes, "INFRAESTRUTURA E REDES")
TAB_APL_OBRIGATORIA <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_aplicacoes_obrigatorias, "APLICAÇÕES DE OBRIGATÓRIAS")
TAB_QUALIDADE_SIS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_qualidade_sistema, "QUALIDADE DE SISTEMAS")
TAB_ALEATORIAS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_aleatorias, "GERAIS")

# para que possa ser feita uma análise QUANTITATIVA de alunos nas disciplinas, será necessário a criação de novas tabelas que sera feita a seguir
#obs: análise para períodos PRESENCIAIS

CONTAGEM_DEV_ARQ_PRESENCIAL <- cria_tabela_contagem_genero(TAB_DEV_ARQ %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_DADOS_PRESENCIAL <- cria_tabela_contagem_genero(TAB_DADOS %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_GOV_EMP_PRESENCIAL <- cria_tabela_contagem_genero(TAB_GOV_EMP %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_INFRA_REDES_PRESENCIAL <- cria_tabela_contagem_genero(TAB_INFRA_REDES %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_APL_OBRIGATORIA_PRESENCIAL <- cria_tabela_contagem_genero(TAB_APL_OBRIGATORIA %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_QUALIDADE_SIS_PRESENCIAL <- cria_tabela_contagem_genero(TAB_QUALIDADE_SIS %>% filter(term >= "2000.1", term < "2020"))
CONTAGEM_ALEATORIAS_PRESENCIAL <- cria_tabela_contagem_genero(TAB_ALEATORIAS %>% filter(term >= "2000.1", term < "2020"))

# criação de gráfico quantitativo para comparar as disciplinas e o gênero nos periodos PRESENCIAIS
cores_presencial <- c("#DDA0DD","#B0E0E6")

cria_grafico_contagem(CONTAGEM_DEV_ARQ_PRESENCIAL, "Quantidade de alunos em optativas de desenvolvimento e arquitetura em períodos presenciais", 600, 50, cores_presencial)
cria_grafico_contagem(CONTAGEM_DADOS_PRESENCIAL, "Quantidade de alunos em optativas de dados em períodos presenciais", 2000, 200, cores_presencial)
cria_grafico_contagem(CONTAGEM_GOV_EMP_PRESENCIAL, "Quantidade de alunos em optativas de governança e empreendedorismo em períodos presenciais", 280, 20, cores_presencial)
cria_grafico_contagem(CONTAGEM_INFRA_REDES_PRESENCIAL, "Quantidade de alunos em optativas de infraestrutura e redes em períodos presenciais", 1700, 100, cores_presencial)
cria_grafico_contagem(CONTAGEM_APL_OBRIGATORIA_PRESENCIAL, "Quantidade de alunos em optativas aplicações de obrigatórias em períodos presenciais", 200, 50, cores_presencial)
cria_grafico_contagem(CONTAGEM_QUALIDADE_SIS_PRESENCIAL, "Quantidade de alunos em optativas de qualidade de sistemas em períodos presenciais", 200, 50, cores_presencial)
cria_grafico_contagem(CONTAGEM_ALEATORIAS_PRESENCIAL, "Quantidade de alunos em optativas gerais em períodos presenciais", 40, 10, cores_presencial)

# Já para os períodos remotos temos que repetir o mesmo apenas mudando o período

REMOTO_DEV_ARQ <- cria_tabela_contagem_genero(TAB_DEV_ARQ %>% filter(term >= "2020"))
REMOTO_DADOS <- cria_tabela_contagem_genero(TAB_DADOS %>% filter(term >= "2020"))
REMOTO_GOV_EMP <- cria_tabela_contagem_genero(TAB_GOV_EMP %>% filter(term >= "2020"))
REMOTO_INFRA_REDES <- cria_tabela_contagem_genero(TAB_INFRA_REDES %>% filter(term >= "2020"))
REMOTO_APL_OBRIGATORIA <- cria_tabela_contagem_genero(TAB_APL_OBRIGATORIA %>% filter(term >= "2020"))
REMOTO_QUALIDADE_SIS <- cria_tabela_contagem_genero(TAB_QUALIDADE_SIS %>% filter(term >= "2020"))
REMOTO_ALEATORIAS <- cria_tabela_contagem_genero(TAB_ALEATORIAS %>% filter(term >= "2020"))

# Criando os gráficos para os períodos REMOTOS
cores_remoto <- c("#FF69B4","#3CB371")

cria_grafico_contagem(REMOTO_DEV_ARQ, "Quantidade de alunos em optativas de desenvolvimento e arquitetura em períodos remotos", 150, 25, cores_remoto)
cria_grafico_contagem(REMOTO_DADOS, "Quantidade de alunos em optativas de dados em períodos remotos", 110, 25, cores_remoto)
cria_grafico_contagem(REMOTO_GOV_EMP, "Quantidade de alunos em optativas de governança e empreendedorismo em períodos remotos", 150, 25, cores_remoto)
cria_grafico_contagem(REMOTO_INFRA_REDES, "Quantidade de alunos em optativas de infraestrutura e redes em períodos remotos", 150, 25, cores_remoto)
cria_grafico_contagem(REMOTO_QUALIDADE_SIS, "Quantidade de alunos em optativas de qualidade de sistemas em períodos remotos", 200, 20, cores_remoto)
cria_grafico_contagem(REMOTO_ALEATORIAS, "Quantidade de alunos em optativas gerais em períodos remotos", 100, 25, cores_remoto)

# União de todas os grupos em uma tabela geral

TABELA_GERAL <- rbind(TAB_DEV_ARQ, TAB_DADOS, TAB_GOV_EMP, TAB_INFRA_REDES, TAB_APL_OBRIGATORIA, TAB_QUALIDADE_SIS, TAB_ALEATORIAS)
TABELA_GERAL_PRESENCIAL <- TABELA_GERAL %>% filter(term >= "2000.1", term < "2020")
TABELA_GERAL_REMOTO <- TABELA_GERAL %>% filter(term >= "2020")

grafico_quantidade_grupos(TABELA_GERAL_PRESENCIAL, 2800, 200, cores_presencial)
grafico_quantidade_grupos(TABELA_GERAL_REMOTO, 500, 50, cores_remoto)

# Agora, a partir das tabelas existentes serão criados os gráficos de desempenho em cada grupo para os períodos remotos X presenciais

#Desempenho REMOTO
# Para isso é necessário calcular a média daqueles alunos para cada grupo
medias_remoto <- calcula_medias(TABELA_GERAL_REMOTO)
COEFICIENTE_REMOTO <- coeficiente_geral(TABELA_GERAL_REMOTO)
#MEDIANAS

medianas_remoto <- calcula_medianas(TABELA_GERAL_REMOTO)
#Criação do gráfico de desempenho

cria_grafico_medias(medias_remoto, cores_remoto, "Desempenho dos alunos nas optativas nos períodos REMOTOS")

cria_grafico_medianas(medianas_remoto, cores_remoto)
media_medianas_remoto <- mean(medianas_remoto$mediana)
REMOTO_MEDIA_MEDIANAS <- medias_remoto %>%  group_by(gender) %>% summarise(media_mediana = mean(mediana))
mean(REMOTO_MEDIA_MEDIANAS$media_mediana)

#Desempenho PRESENCIAL
medias_presencial <- calcula_medias(TABELA_GERAL_PRESENCIAL)
COEFICIENTE_PRESENCIAL <- coeficiente_geral(TABELA_GERAL_PRESENCIAL)

PRESENCIAL_MEDIA_MEDIANAS <- medias_presencial %>%  group_by(gender, grupo) %>% summarise(media_mediana = mean(mediana))

cria_grafico_medias(medias_presencial, cores_presencial, "Desempenho dos alunos nas optativas nos períodos PRESENCIAIS")

medianas_presencial <- calcula_medianas(TABELA_GERAL_PRESENCIAL)
cria_grafico_medianas(medianas_presencial, cores_presencial)

media_medianas_presencial <- mean(medianas_presencial$mediana)

