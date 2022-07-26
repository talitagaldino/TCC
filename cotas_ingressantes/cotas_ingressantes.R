DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")
library("ggplot2")

PRESENCIAL <- DADOS_ALUNOS %>% filter(admissionYear >= "2013.1", admissionYear < "2020") %>% 
  filter(affirmativePolicy != "N/A") %>%
  group_by(admissionYear, affirmativePolicy, gender) %>% summarise(quantidade = n())

ggplot(PRESENCIAL, aes(x=admissionYear,fill=affirmativePolicy))+
  theme_bw()+
  facet_wrap(~gender)+
  geom_bar()

ggplot(PRESENCIAL, aes(x=as.factor(admissionYear), y=quantidade, fill = affirmativePolicy)) +
  geom_col() + coord_flip()
