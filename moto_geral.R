################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS                             #
################################################################################

#Pacotes utilizados
pacotes <- c("beepr","car","correlation","cowplot","emojifont","equatiomatic",
             "fastDummies","ggraph","ggrepel","ggside","ggstance","jtools",
             "kableExtra","knitr","lmtest","magick","MASS","nortest","olsrr",
             "overdisp","PerformanceAnalytics","plotly","pscl","psych","questionr",
             "Rcpp","reshape2","rgl","see","readxl","splines","tidyquant","tidyverse",
             "Hmisc","metan","stargazer"
)

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
# DADOS                                                                        #
################################################################################

motos <- read_excel("moto.xlsx")
dados_motos <- dummy_columns(.data = motos,
                                   select_columns = "fim_carn",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T)
dados_motos <- dados_motos %>% 
            rename(fim_carn = fim_carn_1)

################################################################################
# ESTATISTICAS DESCRITIVAS                                                     #
################################################################################

#Estatísticas univariadas

summary(dados_motos)

################################################################################
# ESTUDO DE CORRELACOES                                                        #
################################################################################

#Chart.Correlation 'PerformanceAnalytics'

chart.Correlation((dados_motos[5:20]), histogram = TRUE)

#Pairs.Panels 'psych'

pairs.panels(dados_motos[5:20],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

#Corr_plot 'metan'

dados_motos %>%
  corr_plot(fim_carn,dias,dias_fds,dias_fer_pont,pluv,temp_media,qt_auto,
            qt_motoc,qt_caminh,pib_mensal,desocup_trim,desocup_trimmovbr,
            casos_covid,qt_imunizados,multas_detran,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")

#Correlacao Heatmap

ggplotly(
  dados_motos[,5:20] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 3) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

#Correlacao da variavel dependente com preditoras em tabela 

dados_motos_rho <- rcorr(as.matrix(dados_motos[,5:20]), type="pearson")
dados_motos_corr_coef <- dados_motos_rho$r
dados_motos_corr_coef <- subset(dados_motos_corr_coef, select = c(obitos))
dados_motos_corr_sig <- round(dados_motos_rho$P, 5)
dados_motos_corr_sig <- subset(dados_motos_corr_sig, select = c(obitos))
dados_motos_corr_sig_final<- cbind(dados_motos_corr_coef, dados_motos_corr_sig)
colnames (dados_motos_corr_sig_final) <- c ('Corr_Pearson', 'Nivel_Sig')
dados_motos_corr_sig_final %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

################################################################################
# TABELA FREQ                                                                  #
################################################################################

#Modelo para tabela de frequencia

freq(dados_motos$obitos) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

################################################################################
# HISTOGRAMAS                                                                  #
################################################################################

#Histograma da variavel dependente

ggplotly(
  dados_motos %>%
    ggplot(aes(x = obitos,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(dados_motos) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "grey70", 
                        high = "grey40") +
    labs(title = "Motos",
         x = "Quantidade de obitos de trânsito",
         y = "Frequência") +
    theme_bw(base_size=15)
)

################################################################################
#MODELO REGRESSAO MULTIPLA                                                     #
################################################################################

################################################################################
#MODELO REGRESSAO MULTIPLA LINEAR                                              #
################################################################################

#Estimação do modelo OLS linear
modelo_lm_motos <- lm(formula = obitos ~ fim_carn+dias+dias_fds+dias_fer_pont+
                    pluv+temp_media+qt_auto+qt_motoc+qt_caminh+pib_mensal+
                    desocup_trim+desocup_trimmovbr+casos_covid+qt_imunizados+multas_detran,
                    data = dados_motos)

#Parâmetros do modelo
summary(modelo_lm_motos)

# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_lm_motos, level = 0.95)

#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo_lm_motos)

#Teste de Shapiro-Francia para a normalidade dos resíduos
sf.test(modelo_lm_motos$residuals)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_lm_motos)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_lm_motos, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_lm_motos, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_lm_motos, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_lm_motos, scale = F, digits = 4)

################################################################################
#MODELO REGRESSAO MULTIPLA LINEAR COM STEPWISE                                 #
################################################################################

#Aplicando o procedimento Stepwise
modelo_lm_motos_step <- step(modelo_lm_motos, k = 3.841459)

#Parâmetros do modelo
summary(modelo_lm_motos_step)

# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_lm_motos_step, level = 0.95)

#Teste de Shapiro-Francia para a normalidade dos resíduos
sf.test(modelo_lm_motos_step$residuals)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_lm_motos_step)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_lm_motos_step, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_lm_motos_step, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_lm_motos_step, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_lm_motos_step, scale = F, digits = 4)

################################################################################
#MODELO REGRESSAO MULTIPLA NAO LINEAR                                          #
################################################################################

#Para calcular o lambda de Box-Cox, função 'powerTransform' do pacote 'car'
lambda_BC <- powerTransform(dados_motos$obitos)
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
dados_motos$obitosBC <- (((dados_motos$obitos ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

#Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_lm_motos_bc <- lm(formula = obitosBC ~ fim_carn+dias+dias_fds+dias_fer_pont+
                      pluv+temp_media+qt_auto+qt_motoc+qt_caminh+pib_mensal+
                      desocup_trim+desocup_trimmovbr+casos_covid+qt_imunizados+multas_detran,
                      data = dados_motos)

#Parâmetros do modelo
summary(modelo_lm_motos_bc)

# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_lm_motos_bc, level = 0.95)

#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo_lm_motos_bc)

#Teste de Shapiro-Francia para a normalidade dos resíduos
sf.test(modelo_lm_motos_bc$residuals)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_lm_motos_bc)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_lm_motos_bc, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_lm_motos_bc, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_lm_motos_bc, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_lm_motos_bc, scale = F, digits = 4)

################################################################################
#MODELO REGRESSAO MULTIPLA NAO LINEAR COM STEPWISE                             #
################################################################################

#Aplicando o procedimento Stepwise
modelo_lm_motos_step_bc <- step(modelo_lm_motos_bc, k = 3.841459)

#Parâmetros do modelo
summary(modelo_lm_motos_step_bc)

# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_lm_motos_step_bc, level = 0.95)

#Teste de Shapiro-Francia para os resíduos do modelo
sf.test(modelo_lm_motos_step_bc$residuals)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_lm_motos_step_bc)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_lm_motos_step_bc, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_lm_motos_step_bc, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_lm_motos_step_bc, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_lm_motos_step_bc, scale = F, digits = 4)

################################################################################
#REGRESSÃO PARA DADOS DE CONTAGEM                                              #
################################################################################

#Diagnóstico preliminar para observação de eventual igualdade entre a média e
#a variância da variável dependente 'obitos'
dados_motos %>%
  summarise(Média = mean(obitos),
            Variância = var(obitos)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)

#Estimação do modelo poisson
modelo_poisson_motos <- glm(formula = obitos ~ fim_carn+dias+dias_fds+dias_fer_pont+
                              pluv+temp_media+qt_auto+qt_motoc+qt_caminh+pib_mensal+
                              desocup_trim+desocup_trimmovbr+casos_covid+qt_imunizados+multas_detran,
                      data = dados_motos,
                      family = "poisson")

#Parâmetros do modelo
summary(modelo_poisson_motos)

#Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_poisson_motos, level = 0.95)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_poisson_motos, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_poisson_motos, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_poisson_motos, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_poisson_motos, scale = F, digits = 4)

################################################################################
#REGRESSÃO PARA DADOS DE CONTAGEM COM STEPWISE                                 #
################################################################################

#Estimação do modelo poisson com stepwise
modelo_poisson_motos_step <- step(modelo_poisson_motos, k = 3.841459)

#Parâmetros do modelo com stepwise
summary(modelo_poisson_motos_step)

#Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_poisson_motos_step, level = 0.95)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_poisson_motos_step, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_poisson_motos_step, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_poisson_motos_step, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_poisson_motos_step, scale = F, digits = 4)

################################################################################
#ESTIMAÇÃO DO MODELO BINOMIAL NEGATIVO                                         #
################################################################################

#Teste de Cameron e Trivedi (1990)
overdisp(x = dados_motos,
         dependent.position = 5,
         predictor.position = 6:20)

#Estimação do modelo binomial negativo pela função glm.nb do pacote MASS
#Modelo Binomial Negativo do Tipo 2 (NB2)
modelo_bneg_motos <- glm.nb(formula = obitos ~ fim_carn+dias+dias_fds+dias_fer_pont+
                              pluv+temp_media+qt_auto+qt_motoc+qt_caminh+pib_mensal+
                              desocup_trim+desocup_trimmovbr+casos_covid+qt_imunizados+multas_detran,
                      data = dados_motos)

#Parâmetro de forma da distribuição binomial negativa
1 / modelo_bneg_motos$theta #phi
modelo_bneg_motos$theta

#Estatística z de Wald do parâmetro theta para verificação da
#significância estatística
modelo_bneg_motos$theta / modelo_bneg_motos$SE.theta  #maior que 1.96

#Parâmetros do modelo_bneg
summary(modelo_bneg_motos)

#Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_bneg_motos, level = 0.95)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_bneg_motos, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_bneg_motos, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_bneg_motos, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_bneg_motos, scale = F, digits = 4)

################################################################################
#ESTIMAÇÃO DO MODELO BINOMIAL NEGATIVO COM STEPWISE                            #
################################################################################

#Estimação do modelo binomial negativo com stepwise
modelo_bneg_motos_step <- step(modelo_bneg_motos, k = 3.841459)

#Parâmetro de forma da distribuição binomial negativa com stepwise
1 / modelo_bneg_motos_step$theta #phi
modelo_bneg_motos_step$theta

#Estatística z de Wald do parâmetro theta para verificação da
#significância estatística com stepwise
modelo_bneg_motos_step$theta / modelo_bneg_motos_step$SE.theta  #maior que 1.96

#Parâmetros do modelo_bneg com stepwise
summary(modelo_bneg_motos_step)

#Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_bneg_motos_step, level = 0.95)

#Exibicao do modelo com pacote stargazer
stargazer(modelo_bneg_motos_step, nobs = T, type = "text")

#Visualização do modelo no ambiente Viewer com função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_bneg_motos_step, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

#Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_bneg_motos_step, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_bneg_motos_step, scale = F, digits = 4)

################################################################################
#COMPARACAO DOS MODELOS                                                        #
################################################################################

#Comparando os modelos
export_summs(modelo_lm_motos_step,modelo_lm_motos_step_bc,
             modelo_poisson_motos_step, modelo_bneg_motos_step,
             scale = F, digits = 4,
             model.names = c("LINEAR_STEP", "LINEAR_STEP_BC",
                             "POISSON_STEP","BNEG_STEP"))

data.frame(LL_Linear_Step = round(logLik(modelo_lm_motos_step), 1),
           LL_Linear_Step_Bc = round(logLik(modelo_lm_motos_step_bc), 1),
           LL_Poisson_Step = round(logLik(modelo_poisson_motos_step), 1),
           LL_BNeg_Step = round(logLik(modelo_bneg_motos_step), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Lrtest (likelihood ratio test para comparação de LL's entre modelos)
lrtest(modelo_lm_motos_step_bc,modelo_lm_motos_step)
lrtest(modelo_poisson_motos_step,modelo_lm_motos_step_bc)
lrtest(modelo_bneg_motos_step,modelo_poisson_motos_step)

#Gráfico para a comparação dos LL dos modelos
my_plot <-
  data.frame(Linear_Step = logLik(modelo_lm_motos_step),
             Linear_Step_Bc = logLik(modelo_lm_motos_step_bc),
             Poisson_Step = logLik(modelo_poisson_motos_step),
             BNeg_Step = logLik(modelo_bneg_motos_step)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = round(value, digits = 3)), 
            color = "black", 
            size = 3.7,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("gray","gray","gray","gray")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot
