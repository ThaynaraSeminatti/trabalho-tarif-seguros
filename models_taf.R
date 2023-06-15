require(glm2)
require(hnp)
require(pscl)
library(splitTools)
library(gridExtra)

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


set.seed(42) 


#FAZER UM STEPWISE PARA A FREQ SINISTRO----


####separar em treino e teste
ir <- dados_mod[c('FREQ_TOTAL','UF','FAIXA_ETARIA','EXPOSTOS')]
y <- multi_strata(ir, k = 5)
inds <- partition(
  y, p = c(train = 0.75,  test = 0.25), split_into_list = FALSE
)


dados_mod$TREIN_TEST = inds 
dados_treino = dados_mod[dados_mod$TREIN_TEST == 'train',]
dados_teste =  dados_mod[dados_mod$TREIN_TEST == 'test',]


##possion
null <- glm(FREQ_TOTAL ~ 1 , family = poisson(link = "log"),data = dados_treino)
summary(null)
pvalor = 1-pchisq(null$deviance/summary(null)$dispersion, null$df.residual); pvalor	# Compara??o com modelo saturado

sm <- step(null, direction = "both", scope = ~  UF + SEXO_CONDUTOR + FAIXA_ETARIA + EXPOSTOS)
summary(sm)
pvalor = 1-pchisq(sm$deviance/summary(sm)$dispersion, sm$df.residual); pvalor	# Compara??o com modelo saturado
plot(hnp(sm, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE), col = "azure4", pch = 19)	# An?lise de res?duos
###pessimo ajuste

## binomial negativa

null.nb <- glm.nb(FREQ_TOTAL ~ 1,data = dados_treino)
summary(null.nb)
pvalor = 1-pchisq(null.nb$deviance/summary(null.nb)$dispersion, null.nb$df.residual); pvalor	# Compara??o com modelo saturado

sm.nb <- step(null.nb, direction = "both", scope = ~ UF + SEXO_CONDUTOR + FAIXA_ETARIA + EXPOSTOS)
summary(sm.nb)
pvalor = 1-pchisq(sm.nb$deviance/summary(sm.nb)$dispersion, sm.nb$df.residual); pvalor	# Compara??o com modelo saturado
plot(hnp(sm.nb, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE), col = "gray", pch = 19)	# An?lise de res?duos

#ajustou melhor

dados_teste$FREQ_SINIS_PREDITO =  predict(sm.nb, dados_teste)

FREQ_SIN_PRED_UF = dados_teste %>% 
  group_by(UF , FAIXA_ETARIA) %>% 
  summarise(n = mean(FREQ_SINIS_PREDITO)) %>% 
  ggplot(aes(x = UF, y = n))+
  geom_col(fill = 'black')+  
  labs(x = '', y = '', title = 'Frequência de sinistro - Preditas - Palio 1.0')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

FREQ_SIN_PRED_FAIXA_ETARIA = dados_teste %>% 
  group_by(UF , FAIXA_ETARIA) %>% 
  summarise(n = mean(FREQ_SINIS_PREDITO)) %>% 
  ggplot(aes(x = FAIXA_ETARIA, y = n))+
  geom_col(fill = 'black')+  
  labs(x = '', y = '', title = 'Frequência de sinistro - Preditas - Palio 1.0')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(FREQ_SIN_PRED_UF , FREQ_SIN_PRED_FAIXA_ETARIA, nrow = 2 )


####STEPWISE PARA O PREMIO MEDIO -----
ir <- dados_mod[c('PREMIO_MEDIO','UF','FAIXA_ETARIA','EXPOSTOS')]
y <- multi_strata(ir, k = 5)
inds <- partition(
  y, p = c(train = 0.75,  test = 0.25), split_into_list = FALSE
)


dados_mod$TREIN_TEST = inds 
dados_treino = dados_mod[dados_mod$TREIN_TEST == 'train',]
dados_teste =  dados_mod[dados_mod$TREIN_TEST == 'test',]


##possion
null <- glm(PREMIO_MEDIO ~ 1 , family = poisson(link = "log"),data = dados_treino)
summary(null)
pvalor = 1-pchisq(null$deviance/summary(null)$dispersion, null$df.residual); pvalor	# Compara??o com modelo saturado

sm <- step(null, direction = "both", scope = ~  UF + SEXO_CONDUTOR + FAIXA_ETARIA + EXPOSTOS)
summary(sm)
pvalor = 1-pchisq(sm$deviance/summary(sm)$dispersion, sm$df.residual); pvalor	# Compara??o com modelo saturado
plot(hnp(sm, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE), col = "azure4", pch = 19)	# An?lise de res?duos
###pessimo ajuste

## binomial negativa

null.nb <- glm.nb(PREMIO_MEDIO ~ 1,data = dados_treino)
summary(null.nb)
pvalor = 1-pchisq(null.nb$deviance/summary(null.nb)$dispersion, null.nb$df.residual); pvalor	# Compara??o com modelo saturado

sm.nb <- step(null.nb, direction = "both", scope = ~ UF + SEXO_CONDUTOR + FAIXA_ETARIA + EXPOSTOS)
summary(sm.nb)
pvalor = 1-pchisq(sm.nb$deviance/summary(sm.nb)$dispersion, sm.nb$df.residual); pvalor	# Compara??o com modelo saturado
plot(hnp(sm.nb, type.resid = "deviance", how.many.out = TRUE, paint.out = TRUE), col = "gray", pch = 19)	# An?lise de res?duos


dados_teste$CUST_MEDIO_PREDITO =  predict(sm.nb, dados_teste)

CUST_MEDIO_PREDIT_UF = dados_teste %>% 
  group_by(UF , FAIXA_ETARIA) %>% 
  summarise(n = mean(CUST_MEDIO_PREDITO)) %>% 
  ggplot(aes(x = UF, y = n))+
  geom_col(fill = 'black')+  
  labs(x = '', y = '', title = 'Custo médio: UF - Preditas - Palio 1.0')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

CUST_MEDIO_PREDIT_FAIXA_ETARIA = dados_teste %>% 
  group_by(UF , FAIXA_ETARIA) %>% 
  summarise(n = mean(CUST_MEDIO_PREDITO)) %>% 
  ggplot(aes(x = FAIXA_ETARIA, y = n))+
  geom_col(fill = 'black')+  
  labs(x = '', y = '', title = 'Custo médio: Faixa Etaria - Preditas - Palio 1.0')+
  theme_bw()+theme(legend.position = "top")+
  scale_fill_manual(values = c('gray','black'))

grid.arrange(CUST_MEDIO_PREDIT_UF, CUST_MEDIO_PREDIT_FAIXA_ETARIA, nrow = 2)
