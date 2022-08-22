DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
library("dplyr")
library("ggplot2")
theme_set(theme_bw())


#filtragem pela forma de evasão

motivos_evasao_remoto <- DADOS_ALUNOS %>% dplyr::filter(statusYear >= "2020") %>% group_by(statusCode, gender, statusYear) %>% summarise(quantidade = n())
motivos_evasao_presencial <- DADOS_ALUNOS %>% dplyr::filter(statusYear >= "2000", statusYear < "2020") %>% group_by(statusCode, gender) %>% summarise(quantidade = n())
motivos_evasao_presencial_periodo <- DADOS_ALUNOS %>% dplyr::filter(statusYear >= "2000", statusYear < "2020") %>% group_by(statusCode, gender, statusYear) %>% summarise(quantidade = n())

motivos_evasao_remoto %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO") %>% ggplot(aes(x = statusCode, y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + coord_flip() + 
  labs(x = "Situação",
       y = "Quantidade",
       fill = "Gênero") +
  scale_fill_manual(values= c("#F5A9E1", "#819FF7")) +
  theme(legend.position = "top") 

motivos_evasao_presencial %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO") %>% ggplot(aes(x = statusCode, y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + coord_flip() + 
  labs(x = "Situação",
       y = "Quantidade",
       fill = "Gênero") +
  scale_fill_manual(values= c("#DA81F5", "#01DF74")) + scale_y_continuous(breaks=seq(0, 300, 50), limits=c(0, 300)) + theme(legend.position = "top") 

TABELA_EVASAO_PRESENCIAL <- motivos_evasao_presencial %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO")
# Agora, fazendo a análise de ativos e graduados separadamente, por período temos

#Alunos que estão ativos no momento no curso

motivos_evasao_remoto %>% filter(statusCode == "ATIVO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 700, 100), limits=c(0, 700)) +
  labs(y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#F5A9E1", "#819FF7")) + theme(legend.position = "top") 

quantidade_ativos <- motivos_evasao_remoto %>% filter(statusCode == "ATIVO")
# Alunos graduados nos períodos REMOTOS

graduados_remoto <- motivos_evasao_remoto %>% filter(statusCode == "GRADUADO")
total_remoto <- sum(graduados_remoto$quantidade)

graduados_remotos_mulheres <- motivos_evasao_remoto %>% filter(statusCode == "GRADUADO") %>% filter(gender == 'Feminino')
num_mulheres_grad_remotos <- sum(graduados_remotos_mulheres$quantidade)

graduados_remotos_homens <- motivos_evasao_remoto %>% filter(statusCode == "GRADUADO") %>% filter(gender == 'Masculino')
num_homens_grad_remotos <- sum(graduados_remotos_homens$quantidade)

motivos_evasao_remoto %>% filter(statusCode == "GRADUADO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 80)) + coord_flip() +
  labs(y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#F5A9E1", "#819FF7")) + theme(legend.position = "top") 

# Alunos graduados nos períodos presenciais

graduados_presencial <- motivos_evasao_presencial_periodo %>% filter(statusCode == "GRADUADO")
total_presencial <- sum(graduados_presencial$quantidade)
media_presencial <- total_presencial/40

graduados_presencial_mulheres <- motivos_evasao_presencial_periodo %>% filter(statusCode == "GRADUADO") %>% filter(gender == 'Feminino')
num_mulheres_grad_presencial <- sum(graduados_presencial_mulheres$quantidade)

graduados_presencial_homens <- motivos_evasao_presencial_periodo %>% filter(statusCode == "GRADUADO") %>% filter(gender == 'Masculino')
num_homens_grad_presencial <- sum(graduados_presencial_homens$quantidade)

motivos_evasao_presencial_periodo %>% filter(statusCode == "GRADUADO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "stack") + scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 80)) + coord_flip() +
  labs(y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#DA81F5", "#01DF74")) + theme(legend.position = "top") 

graduados_total <- DADOS_ALUNOS %>% filter(statusCode == "GRADUADO")%>% group_by(statusCode, gender) %>% summarise(quantidade = n())

REMOTO_evasao_tabela <- motivos_evasao_remoto %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO")
total = sum(REMOTO_evasao_tabela$quantidade) 62
abandono_remoto <- REMOTO_evasao_tabela %>% filter(statusCode == "CANCELAMENTO POR ABANDONO")
