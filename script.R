#################
### LIBRARIES ###
#################

library(dplyr)
library(arules)

################
### DOWNLOAD ###
################

url <- c('http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Dicion%C3%A1rio-Base-Domic%C3%ADlios-PDAD-2.xlsx',
         'http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base_Domicilios_PDAD_2015.csv',
         'http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Dicion%C3%A1rio-Base-Moradores-PDAD-2.xlsx',
         'http://www.codeplan.df.gov.br/wp-content/uploads/2018/02/Base_Moradores_PDAD_2015.csv')

arquivos <- c('dicionario_domicilios_PDAD.xlsx',
              'base_domicilios_PDAD.csv',
              'dicionario_moradores_PDAD.xlsx',
              'base_moradores.csv')

for(i in seq_along(url)){
  download.file(url = url[i], 
                destfile = arquivos[i])  
}

###############
### DATASET ###
###############

arquivos <- list.files()
moradores <- read.csv2('base_moradores.csv', 
                       stringsAsFactors = FALSE, 
                       fileEncoding = 'latin1')
glimpse(moradores)

moradores$FAIXA_ETARIA <- NULL

#################
### PROFILING ###
#################

table(moradores$A01_DOM_RA)

moradores %>%  
  mutate(pop_total = NFATOR_RA) %>%
  group_by(A01_DOM_RA) %>%
  summarise(sum(pop_total))


###################
### FACTOR DATA ###
###################

moradores$RA <- factor(moradores$A01_DOM_RA,
                       levels = 1:31,
                       labels = c('BRASÍLIA/PLANO PILOTO','GAMA','TAGUATINGA','BRAZLÂNDIA','SOBRADINHO',
                                  'PLANALTINA','PARANOÁ','NÚCLEO BANDEIRANTE','CEILÂNDIA',
                                  'GUARÁ','CRUZEIRO','SAMAMBAIA','SANTA MARIA',
                                  'SÃO SEBASTIÃO','RECANTO DAS EMAS',
                                  'LAGO SUL','RIACHO FUNDO','LAGO NORTE','CANDANGOLÂNDIA',
                                  'ÁGUAS CLARAS','RIACHO FUNDO II',
                                  'SUDOESTE/OCTOGONAL','VARJÃO','PARK WAY',
                                  'SCIA-ESTRUTURAL','SOBRADINHO II',
                                  'JARDIM BOTÂNICO','ITAPOÃ','SIA','VICENTE PIRES','FERCAL'))
  

moradores$sexo <- factor(moradores$D04_MOR_SEXO,
                         levels = c(1,2),
                         labels = c('Masculino','Feminino'))
                          
moradores$faixa_etaria <- discretize(moradores$D06_MOR_IDADE,
                                     method = 'fixed',
                                     breaks = c(-Inf,15,25,35,45,60,Inf),
                                     labels = c('0 a 15 anos','16 a 24 anos','25 a 34 anos','35 a 44 anos','45 a 59 anos','60 anos ou mais'))

# head(moradores)

###############
### TABELAS ###
###############

tabela <- 
  moradores %>%
  group_by(RA, sexo, faixa_etaria) %>%
  summarise(populacao = sum(NFATOR_RA))
# head(tabela)
# sum(tabela$populacao)

tabela <- 
  tabela %>%
  spread(sexo, populacao)
# head(tabela)

write.csv2(tabela, '20180801-Populacao-por-RA-sexo-faixa_et.csv', 
           row.names = FALSE, 
           fileEncoding = 'latin1')

