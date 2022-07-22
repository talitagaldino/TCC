DADOS_ALUNOS <- read.csv("./TCC/dadosfubica/anonymized_students.csv", sep = ";")

library("dplyr")
library("ggplot2")

ingressantes_remoto = DADOS_ALUNOS %>% dplyr::filter(admissionYear >= "2020.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)

ingressantes_porcentagem_remoto = ingressantes_remoto %>% group_by(admissionYear) %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))

cols <- c("#FF69B4", "#87CEFA")
qtdd=with(ingressantes_porcentagem_remoto, tapply(ingressantes_remoto$Quantidade,list(gender, admissionYear), mean))

barplot(qtdd,
        width = 0.000000000000005,
        beside = F,
        ylim=c(0,104),
        xlab = "Período",
        ylab = "Quantidade",
        col = cols,
        las=1)
legend("topright",
      legend = levels(ingressantes_porcentagem_remoto$gender),
       fill = cols,
       bty = "n")
box(bty = "L")

ingressantes_presencial = DADOS_ALUNOS %>% dplyr::filter(admissionYear < "2020.1")  %>% dplyr::filter(admissionYear > "2010.1") %>% dplyr::group_by(gender, admissionYear) %>% tally(name = "Quantidade", sort = TRUE)  
ingressantes_presencial_porcentagem = ingressantes_presencial  %>% mutate(porcentagem = format(round(Quantidade/sum(Quantidade) * 100,2), nsmall = 2))
qtdd_presencial=with(ingressantes_presencial_porcentagem, tapply(ingressantes_presencial$Quantidade,list(gender, admissionYear), mean))

cols_presencial <- c("#9370DB", "#3CB371")

barplot(qtdd_presencial,
        width = 0.000000000000005,
        beside = T,
        xlab = "Período",
        ylab = "Quantidade",
        col = cols_presencial,
        las=1)
legend("topright",
       legend = levels(ingressantes_presencial_porcentagem$gender),
       fill = cols_presencial,
       bty = "n")
box(bty = "L")
