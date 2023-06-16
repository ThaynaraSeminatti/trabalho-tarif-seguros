#### pacotes
library('tidyverse')
library('gt')
<<<<<<< HEAD
library(gridExtra)
###### ler dados

dados = read.csv('dados_palio_1_0.csv',sep = ';')

######## agrupando as v. de indenizaccao e qnd. de eventos.####

dados_mod = dados %>% 
  mutate(FREQ_TOTAL = FREQ_COLISAO+FREQ_INCENDIO_ROUBO+FREQ_OUTRAS,
         INDENIZACAO_TOTAL_TOTAL = INDENI_INCENDIO_ROUBO+INDENI_COLI+INDEN_OUTRAS,
         UF = substr(REGIAO, start=1, stop=2),
         FAIXA_ETARIA = substr(FAIXA_ETARIA, start=7, stop=length(FAIXA_ETARIA)),
         FAIXA_ETARIA = if_else(FAIXA_ETARIA == 'que 55 anos', '55 ou mais',FAIXA_ETARIA),
         CUSTO_MEDIO = INDENIZACAO_TOTAL_TOTAL / FREQ_TOTAL) %>% 
  dplyr::select(UF, SEXO_CONDUTOR,FAIXA_ETARIA,EXPOSTOS,PREMIO_MEDIO,FREQ_TOTAL,INDENIZACAO_TOTAL_TOTAL,CUSTO_MEDIO)



### descritiva basica
summary(dados_mod[,4:8]) 
var(dados_mod$EXPOSTOS)
var(dados_mod$PREMIO_MEDIO)
var(dados_mod$FREQ_TOTAL)
var(dados_mod$INDENIZACAO_TOTAL_TOTAL)#### variancia extremamente alta

dados_mod = subset(dados_mod, subset = !(dados_mod$INDENIZACAO_TOTAL_TOTAL)>=2200000 )
#### remover esse dados
summary(dados_mod[,4:8]) 
var(dados_mod$EXPOSTOS)
var(dados_mod$PREMIO_MEDIO)
var(dados_mod$FREQ_TOTAL)
var(dados_mod$INDENIZACAO_TOTAL_TOTAL)
var(dados_mod$CUSTO_MEDIO)

### dados com muita sobredispersao
#var(x)>mEADIA(X)

##montar uma tabela com: 
#media, mediana, variancia (exp (neste caso colocar a soma, pois sera nosso total de segurados),
#prem medio, freq total, indenizacao)
#tudo que for em real, colocar em real(premio e indenizacao)

EXP = dados_mod %>% 
  ggplot(aes(EXPOSTOS)) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = 'exposição',y = '',title = 'Histogram/Boxplot - Exposições - Palio 1.0') + theme_bw()

EXP_BOX = dados_mod %>% 
  ggplot(aes(EXPOSTOS)) +
  geom_boxplot()+
  labs(x = 'exposição',y = '',title = ) + theme_bw()
grid.arrange(EXP, EXP_BOX, nrow = 2)



PREM_MEDIO = dados_mod %>% 
  ggplot(aes(PREMIO_MEDIO)) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = 'prêmio medio',y = '',title = 'Histograma - Boxplot - Prêmio médio - Palio 1.0') + theme_bw()

BOX_PREM_MEDIO = dados_mod %>% 
  ggplot(aes(PREMIO_MEDIO)) +
  geom_boxplot()+
  labs(x = 'prêmio medio',y = '') + theme_bw()

grid.arrange(PREM_MEDIO,BOX_PREM_MEDIO,nrow = 2)


FREQ_TOTAL = dados_mod %>% 
  ggplot(aes(FREQ_TOTAL)) +
  geom_histogram(aes(y = ..density..),bins = 5, fill = 'black')+
  labs(x = 'freq. sinistro',y = '',title = 'Histograma - Boxplot - Freq. Sinistro - Palio 1.0') + theme_bw()


BOX_FREQ_TOTAL = dados_mod %>% 
  ggplot(aes(FREQ_TOTAL)) +
  geom_boxplot()+
  labs(x = 'freq. sinistro',y = '',title = '') + theme_bw()

grid.arrange(FREQ_TOTAL,BOX_FREQ_TOTAL,nrow = 2)



INDEN_TOTAL = dados_mod %>% 
  ggplot(aes(INDENIZACAO_TOTAL_TOTAL)) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = 'nº de indenizações',y = '',title = 'Histograma - Boxplot - Indenizações - Palio 1.0') + theme_bw()

BOX_INDEN_TOTAL = dados_mod %>% 
  ggplot(aes(INDENIZACAO_TOTAL_TOTAL)) +
  geom_boxplot()+
  labs(x = 'nº de indenizações',y = '') + theme_bw()

grid.arrange(INDEN_TOTAL,BOX_INDEN_TOTAL,nrow = 2)


#custo medio

CUST_MEDIO = dados_mod %>% 
  ggplot(aes(CUSTO_MEDIO)) +
  geom_histogram(aes(y = ..density..),bins = 10, fill = 'black')+
  labs(x = 'nº de indenizações',y = '',title = 'Histograma - Boxplot - Custo médio - Palio 1.0') + theme_bw()

BOX_CUSTO_MEDIO = dados_mod %>% 
  ggplot(aes(CUSTO_MEDIO)) +
  geom_boxplot()+
  labs(x = 'nº de indenizações',y = '') + theme_bw()

grid.arrange(CUST_MEDIO,BOX_CUSTO_MEDIO,nrow = 2)



### exposicao----
## faixa etaria
EXP_FAIXA_ETARIA = dados_mod %>% 
  ggplot(aes(x = factor(FAIXA_ETARIA), y  = EXPOSTOS,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Nº de exposições: Faixa Etária - Palio 1.0', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

EXP_UF = dados_mod %>% 
  ggplot(aes(x = UF, y  = EXPOSTOS,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Nº de exposições: Região - Palio 1.0', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(EXP_FAIXA_ETARIA,EXP_UF,nrow = 1)
=======
###### ler dados

dados = read.csv('dados_palio_1_0.csv')

######## agrupando as v. de indenizaccao e qnd. de eventos.####

dados$Freq.Total = dados$Freq..Incêncio.e.Roubo+
  dados$Freq..Colisão  +
  dados$Freq..Outras

dados$Indenizacao = dados$Indeniz..Incêncio.e.Roubo..R..+
  dados$Indeniz..Colisão..R.. + dados$Indeniz..Outras..R..


##### agr. por estado
dados$Região_ = substr(dados$Região, start=1, stop=2)
dados_mod = dados %>%  select(Expostos, Faixa.Etária,Região_, Sexo.Condutor, Freq.Total,Indenizacao,Prêmio.Médio..R..)

summary(dados_mod)
dados_mod[dados$Freq.Total >= 4000,]

### analises descrites 

##exposicao----
## faixa etaria
dados_mod %>% 
  ggplot(aes(x = factor(Faixa.Etária), y  = Expostos,fill = Sexo.Condutor))+
  geom_col()

###regiao
dados_mod %>% 
  ggplot(aes(x = Região_, y = Expostos, fill = Sexo.Condutor))+
  geom_col()
>>>>>>> main


#### indenizaocao----
## faixa etaria
<<<<<<< HEAD
INDENIZACAO_FAIXA_ETARIA = dados_mod %>% 
  ggplot(aes(x = factor(FAIXA_ETARIA), y  = INDENIZACAO_TOTAL_TOTAL,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Nº de Indenização por faixa etária', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

INDENIZACAO_UF = dados_mod %>% 
  ggplot(aes(x = UF, y  = INDENIZACAO_TOTAL_TOTAL,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Nº de Indenização por Região', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(INDENIZACAO_FAIXA_ETARIA,INDENIZACAO_UF,nrow = 1)


## Freq. sinistro ####
FRE_SIN_FAIXA_ETARIA = dados_mod %>% 
  ggplot(aes(x = factor(FAIXA_ETARIA), y  = FREQ_TOTAL,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Freq. sinistro por faixa etária', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))


FREQ_SIN_UF = dados_mod %>% 
  ggplot(aes(x = UF, y  = FREQ_TOTAL,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Freq. sinistro por Região', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(FRE_SIN_FAIXA_ETARIA,FREQ_SIN_UF,nrow = 1)

#####premio medio----
PREMI_MEDIO_FAIXA_ETARIA = dados_mod %>% 
  ggplot(aes(x = factor(FAIXA_ETARIA), y  = PREMIO_MEDIO, fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Prêmio médio por faixa etária', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))


PREMI_MEDIO_UF = dados_mod %>% 
  ggplot(aes(x = UF, y  = PREMIO_MEDIO,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Prêmio médio por Região', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(PREMI_MEDIO_FAIXA_ETARIA,PREMI_MEDIO_UF,nrow = 2)

total.FREQ_TOTAL = sum(dados_mod$FREQ_TOTAL)


##CUSTO MEDIO----

CUSTO_MEDIO_FAIXA_ETARIA = dados_mod %>% 
  ggplot(aes(x = factor(FAIXA_ETARIA), y  = CUSTO_MEDIO, fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Custo médio por faixa etária', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))


CUSTO_MEDIO_UF = dados_mod %>% 
  ggplot(aes(x = UF, y  = CUSTO_MEDIO,fill = SEXO_CONDUTOR))+
  geom_col()+
  labs(x = '', y = '', title = 'Custo médio por Região', fill = 'Sexo:')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(CUSTO_MEDIO_FAIXA_ETARIA,CUSTO_MEDIO_UF,nrow = 2)


####tabelas
dados_mod %>% 
  select(SEXO_CONDUTOR, EXPOSTOS, FREQ_TOTAL, PREMIO_MEDIO,INDENIZACAO_TOTAL_TOTAL) %>%
  rename(Sexo = SEXO_CONDUTOR, Premio_medio = PREMIO_MEDIO) %>% 
  group_by(Sexo) %>% 
  summarise(FREQ_TOTAL = sum(FREQ_TOTAL),
            EXPOSTOS = sum(EXPOSTOS),
            INDENIZACAO_TOTAL = sum(INDENIZACAO_TOTAL_TOTAL),
            PREMIO_MEDIO = sum(Premio_medio),
            CUSTO_MEDIO = INDENIZACAO_TOTAL/FREQ_TOTAL) %>% 
  gt(rowname_col = 'Sexo')

####MUDAR PRA TABELAS NO EXCEL





fit <- glm(FREQ_TOTAL ~ UF+  FAIXA_ETARIA +
             offset(log(EXPOSTOS)), family = poisson(link = "log"),
           data = dados_mod)

summary(fit)

fit2 <- glm(FREQ_TOTAL ~ UF+  FAIXA_ETARIA , family = Gamma(link = "log"),
            data = dados_mod)
summary(fit2)### melhor modelo
=======
dados_mod %>% 
  ggplot(aes(x = factor(Faixa.Etária), y  = Indenizacao,fill = Sexo.Condutor))+
  geom_col()
###regiao
dados_mod %>% 
  ggplot(aes(x = Região_, y = Indenizacao, fill = Sexo.Condutor))+
  geom_col()


## Freq. sinistro ####
dados_mod %>% 
  ggplot(aes(x = factor(Faixa.Etária), y  = Freq.Total,fill = Sexo.Condutor))+
  geom_col()
###regiao
dados_mod %>% 
  ggplot(aes(x = Região_, y = Freq.Total, fill = Sexo.Condutor))+
  geom_col()



total.freq.total = sum(dados_mod$Freq.Total)
####tabelas
dados_mod %>% 
  select(Sexo.Condutor, Expostos, Freq.Total, Prêmio.Médio..R..,Indenizacao) %>%
  rename(Sexo = Sexo.Condutor, Premio_medio = Prêmio.Médio..R..) %>% 
  group_by(Sexo) %>% 
  summarise(Freq.Total = sum(Freq.Total),
            Expostos = sum(Expostos),
            Indenizacao = sum(Indenizacao),
            Premio_medio = sum(Premio_medio),
            Custo_medio = Indenizacao/Freq.Total) %>% 
    gt(rowname_col = 'Sexo')



#### boxplots

dados_mod %>% 
  ggplot(aes(x = Região_, y = Freq.Total/total.freq.total)) +
  geom_boxplot()

dados_mod %>% 
  ggplot(aes(x = Faixa.Etária, y = Freq.Total/total.freq.total)) +
  geom_boxplot()


###histogramas
dados_mod %>% 
  ggplot(aes(x = Indenizacao/Freq.Total, y =  ..density..))+## custo medio
  geom_histogram(binwidth = 500)

dados_mod %>% 
  ggplot(aes(x = Freq.Total,y = ..density..))+
  geom_histogram(binwidth = 800)


dados_mod %>% 
  ggplot(aes(x = Prêmio.Médio..R.., y = ..density..))+
  geom_histogram(binwidth = 100)

>>>>>>> main
