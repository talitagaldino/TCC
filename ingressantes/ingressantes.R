DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")
library("ggplot2")

ingressantes_remoto = DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2020.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)
ingressantes_porcentagem_remoto = ingressantes_remoto %>% group_by(admissionYear) %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))

ingressantes_presencial = DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2000.1", admissionYear < "2020.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)  
ingressantes_presencial_porcentagem = ingressantes_presencial  %>% group_by(admissionYear) %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))

ggplot(ingressantes_porcentagem_remoto, aes(x=as.factor(admissionYear), y=Quantidade, fill=gender)) + 
  geom_col(position = "dodge") +   scale_fill_manual(values= c("#F5A9E1", "#819FF7")) +
  labs(x = "Período",
       y = "Quantidade de ingressantes",
       fill = "Gênero") + coord_flip() +
  scale_y_continuous(breaks=seq(0, 90, 10)) + theme_minimal()


ggplot(ingressantes_presencial_porcentagem, aes(x=as.factor(admissionYear), y=Quantidade, fill=gender)) + 
  geom_col(position = "dodge") +   scale_fill_manual(values= c("#DA81F5", "#01DF74")) +
  labs(x = "Período",
       y = "Quantidade de ingressantes",
       fill = "Gênero") + coord_flip() + 
  scale_y_continuous(breaks=seq(0, 100, 10)) + theme_minimal()


ggplot(ingressantes_presencial_porcentagem, aes(x=as.factor(admissionYear), y=Quantidade, group=gender)) +
  geom_line(aes(color=gender))+
  geom_point(aes(color=gender))+  labs(x = "Período",
                                       y = "Quantidade de ingressantes",
                                       color = "Sexo") +
  scale_color_manual(values= c("#DA81F5", "#01DF74"))  + scale_y_continuous(breaks=seq(0, 100, 10)) +
  theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
        panel.grid.major = element_line(colour = "grey"))

ggplot(ingressantes_porcentagem_remoto, aes(x=as.factor(admissionYear), y=Quantidade, group=gender)) +
  geom_line(aes(color=gender))+
  geom_point(aes(color=gender))+  labs(x = "Período",
                                       y = "Quantidade de ingressantes",
                                       color = "Sexo") +
  scale_color_manual(values= c("#F5A9E1", "#819FF7")) + scale_y_continuous(breaks=seq(0, 100, 10)) +  theme(axis.text.x = element_text(angle = 90), panel.background = element_rect(fill="white"),panel.grid.minor.y = element_line(size=2),
                                                                                                            panel.grid.major = element_line(colour = "grey"))

media_masc_tabela <- ingressantes_presencial_porcentagem %>% filter(gender == "Masculino")
mean(as.numeric(media_masc_tabela$porcentagem))
typeof(media_masc_tabela$porcentagem)

media_fem_tabela <- ingressantes_presencial_porcentagem %>% filter(gender == "Feminino")
mean(as.numeric(media_fem_tabela$porcentagem))


media_masc_tabela_remoto <- ingressantes_porcentagem_remoto %>% filter(gender == "Masculino")
mean(as.numeric(media_masc_tabela_remoto$porcentagem))
typeof(media_masc_tabela$porcentagem)

media_fem_tabela_remoto <- ingressantes_porcentagem_remoto %>% filter(gender == "Feminino")
mean(as.numeric(media_fem_tabela_remoto$porcentagem))
