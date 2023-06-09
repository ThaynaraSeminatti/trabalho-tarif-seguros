#### pacotes
library('tidyverse')
library('gt')
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


#### indenizaocao----
## faixa etaria
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

