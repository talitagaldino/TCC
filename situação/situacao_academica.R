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
  labs(title = "Situação acadêmica nos períodos remotos",
       x = "Situação",
       y = "Quantidade",
       fill = "Gênero") +
  scale_fill_manual(values= c("#F5A9E1", "#819FF7"))

motivos_evasao_presencial %>% filter(statusCode != "ATIVO", statusCode != "GRADUADO") %>% ggplot(aes(x = statusCode, y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + coord_flip() + 
  labs(title = "Situação acadêmica nos períodos presenciais",
       x = "Situação",
       y = "Quantidade",
       fill = "Gênero") +
  scale_fill_manual(values= c("#DA81F5", "#01DF74")) + scale_y_continuous(breaks=seq(0, 300, 50), limits=c(0, 300))

# Agora, fazendo a análise de ativos e graduados separadamente, por período temos

#Alunos que estão ativos no momento no curso

motivos_evasao_remoto %>% filter(statusCode == "ATIVO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 700, 100), limits=c(0, 700)) +
  labs(title = "Alunos ativos no período 2021.2",
       y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#F5A9E1", "#819FF7"))

# Alunos graduados nos períodos REMOTOS

motivos_evasao_remoto %>% filter(statusCode == "GRADUADO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 80)) + coord_flip() +
  labs(title = "Alunos graduados nos períodos remotos",
       y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#F5A9E1", "#819FF7"))

# Alunos graduados nos períodos presenciais

motivos_evasao_presencial_periodo %>% filter(statusCode == "GRADUADO") %>%
  ggplot(aes(x=as.factor(statusYear), y = quantidade, fill = gender)) +
  geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 80, 10), limits=c(0, 80)) + coord_flip() +
  labs(title = "Alunos graduados nos períodos presenciais",
       y = "Quantidade",
       x = "Período",
       fill = "Gênero") + scale_fill_manual(values= c("#DA81F5", "#01DF74"))

