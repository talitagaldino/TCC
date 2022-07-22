DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")
dados_cadeiras <- read.csv("./TCC/dadosfubica/anonymized_enrollments.csv", sep = ";")
library("dplyr")

ggplot(DADOS_ALUNOS, aes(x=admissionYear,fill=affirmativePolicy))+
  theme_bw()+
  facet_wrap(~gender)+
  geom_bar()
