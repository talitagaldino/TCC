DISCIPLINAS <- read.csv("./TCC/dadosfubica/subjects.csv", sep = ";")
DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
DESEMPENHO <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")

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
    labs(title=title, x = "Disciplinas", fill = "Gênero", y = "Quantidade de alunos") + scale_y_continuous(breaks = seq(0, limite_maximo, by = intervalo)) +
    scale_fill_manual(values=cores)
}

grafico_quantidade_grupos = function(data, title, limite_maximo, intervalo, cores){
  ggplot(data, aes(x=grupo,fill=gender))+
    theme_light() + coord_flip() + labs(title=title, x = "Quantidade", fill = "Gênero", y = "Grupo de optativas") +
    geom_bar(position="stack") + scale_y_continuous(breaks = seq(0, limite_maximo, by = intervalo)) + scale_fill_manual(values=cores)
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

TAB_DEV_ARQ <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_dev_arq, "OPTATIVA DEV ARQ")
TAB_DADOS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_dados, "OPTATIVA DADOS")
TAB_GOV_EMP <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_gov_emp, "OPTATIVA GOVERNANÇA E EMPREENDEDORISTO")
TAB_INFRA_REDES <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_infra_redes, "OPTATIVA INFRA E REDES")
TAB_APL_OBRIGATORIA <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_aplicacoes_obrigatorias, "OPTATIVA APLICAÇÃO DE OBRIGATÓRIAS")
TAB_QUALIDADE_SIS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_qualidade_sistema, "OPTATIVA QUALIDADE DE SISTEMAS")
TAB_ALEATORIAS <- cria_tabela_grupo_optativa(DISC_GENERO_NOTA, array_opt_aleatorias, "OPTATIVA ALETÓRIA")

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

cria_grafico_contagem(CONTAGEM_DEV_ARQ_PRESENCIAL, "Optativas de desenvolvimento e arquitetura", 600, 50, c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_DADOS_PRESENCIAL, "Optativas de dados", 2000, 200, c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_GOV_EMP_PRESENCIAL, "Optativas de governança e empreendedorismo", 280, 20, c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_INFRA_REDES_PRESENCIAL, "Optativas de infraestrutura e redes", 1700, 100, c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_APL_OBRIGATORIA_PRESENCIAL, "Optativas aplicações de obrigatórias", 200, 50,c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_QUALIDADE_SIS_PRESENCIAL, "Optativas de qualidade de sistemas", 200, 50, c("#DDA0DD","#B0E0E6"))
cria_grafico_contagem(CONTAGEM_ALEATORIAS_PRESENCIAL, "Optativas aleatórias", 40, 10, c("#DDA0DD","#B0E0E6"))

# Já para os períodos remotos temos que repetir o mesmo apenas mudando o período

REMOTO_DEV_ARQ <- cria_tabela_contagem_genero(TAB_DEV_ARQ %>% filter(term >= "2020"))
REMOTO_DADOS <- cria_tabela_contagem_genero(TAB_DADOS %>% filter(term >= "2020"))
REMOTO_GOV_EMP <- cria_tabela_contagem_genero(TAB_GOV_EMP %>% filter(term >= "2020"))
REMOTO_INFRA_REDES <- cria_tabela_contagem_genero(TAB_INFRA_REDES %>% filter(term >= "2020"))
REMOTO_APL_OBRIGATORIA <- cria_tabela_contagem_genero(TAB_APL_OBRIGATORIA %>% filter(term >= "2020"))
REMOTO_QUALIDADE_SIS <- cria_tabela_contagem_genero(TAB_QUALIDADE_SIS %>% filter(term >= "2020"))
REMOTO_ALEATORIAS <- cria_tabela_contagem_genero(TAB_ALEATORIAS %>% filter(term >= "2020"))

# Criando os gráficos para os períodos REMOTOS

cria_grafico_contagem(REMOTO_DEV_ARQ, "Optativas de desenvolvimento e arquitetura em períodos remotos", 150, 25, c("#FF69B4","#3CB371"))
cria_grafico_contagem(REMOTO_DADOS, "Optativas de dados em períodos remotos", 110, 25, c("#FF69B4","#3CB371"))
cria_grafico_contagem(REMOTO_GOV_EMP, "Optativas de governança e empreendedorismo em períodos remotos", 150, 25, c("#FF69B4","#3CB371"))
cria_grafico_contagem(REMOTO_INFRA_REDES, "Optativas de infraestrutura e redes em períodos remotos", 150, 25, c("#FF69B4","#3CB371"))
cria_grafico_contagem(REMOTO_QUALIDADE_SIS, "Optativas de qualidade de sistemas em períodos remotos", 200, 20, c("#FF69B4","#3CB371"))
cria_grafico_contagem(REMOTO_ALEATORIAS, "Optativas aleatórias em períodos remotos", 100, 25, c("#FF69B4","#3CB371"))

# Tentativa de fazer o total por gênero de cada grupo

TABELA_GERAL <- rbind(TAB_DEV_ARQ, TAB_DADOS, TAB_GOV_EMP, TAB_INFRA_REDES, TAB_APL_OBRIGATORIA, TAB_QUALIDADE_SIS, TAB_ALEATORIAS)
TABELA_GERAL_PRESENCIAL <- TABELA_GERAL %>% filter(term >= "2000.1", term < "2020")
TABELA_GERAL_REMOTO <- TABELA_GERAL %>% filter(term >= "2020")

grafico_quantidade_grupos(TABELA_GERAL_PRESENCIAL, title = "Gráfico de quantidade de grupo de optativas nos períodos presenciais", 2800, 200, c("#D8BFD8", "#B0E0E6"))
grafico_quantidade_grupos(TABELA_GERAL_REMOTO, title = "Gráfico de quantidade de grupo de optativas nos períodos remotos", 500, 50, c("#FF69B4", "#48D1CC"))
