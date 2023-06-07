#### pacotes
library('tidyverse')

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
## faixa etaria
dados_mod %>% 
  ggplot(aes(x = factor(Faixa.Etária), y  = Expostos))+
  geom_col()
### sexo
dados_mod %>% 
  ggplot(aes(x = Sexo.Condutor, y = Expostos,fill = Faixa.Etária))+
  geom_col()
###regiao
dados_mod %>% 
  ggplot(aes(x = Região_, y = Expostos, fill = Sexo.Condutor))+
  geom_col()


#### indenizao
## faixa etaria
dados_mod %>% 
  ggplot(aes(x = factor(Faixa.Etária), y  = Indenizacao))+
  geom_col()
### sexo --- colocar tabe
dados_mod %>% 
  ggplot(aes(x = Sexo.Condutor, y = Indenizacao))+
  geom_col()
###regiao
dados_mod %>% 
  ggplot(aes(x = Região_, y = Expostos, fill = Sexo.Condutor))+
  geom_col()




dados_mod %>% 
  ggplot(aes(x = Região_, y = Freq.Total)) +
  geom_boxplot()
