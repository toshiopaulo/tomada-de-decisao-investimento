setwd("C:/Users/kiosh/OneDrive/Desktop/Cursos de BI/Analista de Inteligência de Mercado - DSA/02_Analise_Estatistica_de_Dados_e_ML/15-Projeto_Final/Projeto-EstatMLR")

library(data.table)
library(dplyr)
library(plyr)
library(readr)
library(xlsx)
library(ggplot2)
library(plotly)
library(tidyr)

dSet <- read.xlsx("Netflix_Data.xlsx", sheetName = "Dados")
colnames(dSet) <- c("period", "subscriptions", "paidMode", "trailMode",
                    "revenue", "cost", "mkt", "cProfit", "cMargin",
                    "cCustomerNoMkt", "rCustomer", "eCustomer", "segment")
str(dSet)
attach(dSet)

# Conversão
dSet$period <- parse_date(as.character(period), "%B %d,%Y")
dSet$period <- format.Date(dSet$period, "%Y-%m")

# Associando as métricas e verificando se os cálculos estão corretos
isTRUE(all(subscriptions == paidMode + trailMode))
isTRUE(all(revenue == cost + mkt + cProfit))
isTRUE(all(cMargin == round(cProfit/revenue, 3)))
isTRUE(all(cCustomerNoMkt == cost/subscriptions))
isTRUE(all(rCustomer == revenue/subscriptions))
isTRUE(all(eCustomer == rCustomer - cCustomerNoMkt))
detach(dSet)

# Criando um outro dataset para manipulação
nf <- dSet
nf <- nf %>% mutate(profitCustomer = cProfit/subscriptions) %>%
  select(!segment)

nfAnnual = nf %>% ddply( ~ year(parse_date(dSet$period, "%Y-%m")), summarise,
                         subscriptions = sum(subscriptions),
                         paidMode = sum(paidMode),
                         trailMode = sum(trailMode),
                         revenue = sum(revenue),
                         cost = sum(cost),
                         mkt = sum(mkt),
                         cProfit = sum(cProfit),
                         cMargin = round(cProfit/revenue, 3),
                         cCustomerNoMkt = cost/subscriptions,
                         rCustomer = revenue/subscriptions,
                         eCustomer = rCustomer - cCustomerNoMkt,
                         profitCustomer = cProfit/subscriptions)
colnames(nfAnnual)[1] <- "period"

# Plotando Revenue X Custo e Despesas
ggplot(nf, aes(x=period)) + geom_line(aes(y= revenue, group = 1), color = "blue") +
  geom_line(aes(y = (cost + mkt), group = 1), color = "red") +
  geom_point(aes(y= revenue), color = "blue") +
  geom_point(aes(y = (cost + mkt)), color = "red") +
  labs(title = "Revenue X Cost and Expense", x = "Period", y = "Value")
ggplotly()
# No gráfico, podemos perceber que o faturamento é sempre ascendete,
# porém é notável que a curva do custo e mkt se torna uma pouco mais íngrime
# a partir do Q2/2017 em comparação à curva de faturamento. 
# Para esse efeito, devemos analisar a porcentagem da margem de lucro.

# Plotando gráfico da taxa de Margem de Lucro por Quarter e por Ano
nf %>% ggplot(aes(period)) + geom_line(aes(y=cMargin*100), group = 1, color = "blue") +
  geom_point(aes(y=cMargin*100), color = "blue") +
  labs(title = "Profit Rate by Quarter", x = "Period", y = "Margin rate")
ggplotly()

nfAnnual %>% ggplot(aes(period)) + geom_line(aes(y=cMargin*100), group = 1, color = "blue") +
  geom_point(aes(y=cMargin*100), color = "blue") +
  labs(title = "Profit Rate by Year", x = "Period", y = "Margin rate")
ggplotly()
# Conseguimos perceber que a partir do Q2/2017 a empresa teve a menor taxa de
# lucro nos últimos dois meses. Analisando com mais detalhes, no ano de 2017,
# apenas o Q1 teve o melhor resultado histórico dentro do dataset, o que contribuiu
# para o crescimento anual de apenas 0.1% em relação ao período anterior.
# 
# Outro resultado anual que precisa ser ressaltado é o de 2018, que manteve o mesmo
# resultado do período anterior, apesar de ter o crescimento no Q1/2018, o Q4/2018
# obteve o pior resultado dos últimos 15 quarters. A partir do ano 2016, a taxa da Margem
# de lucro não obteve o mesmo ritmo de crescimento dos anos 2013, 2014 e 2015.

# Incluindo coluna da taxa de crescimento de lucro YoY e plotando
nfAnnual$profitYOY <- NA
for(i in 1:nrow(nfAnnual)){
  nfAnnual[i, 14] = ifelse(i == 1, 0,
                        round(nfAnnual[i,8]/nfAnnual[i-1,8] - 1, 3))
}

nfAnnual %>% ggplot(aes(period)) + geom_line(aes(y = profitYOY), group = 1) +
  geom_point(aes(y = profitYOY)) +
  labs(title = "Performance de Margem de Lucro YoY", x = "Desempenho Anual", y = "Ano")
ggplotly()
# Verificado no período anual, é possível perceber que houve crescimento em todos os períodos,
# porém conseguimos perceber que existe houve um comportamento de estabilidade de performance
# a partir de 2016, mantendo a taxa entre 21.4% a 24.5% de crescimento.

# Analisando a mesma taxa de performance por quarter.
nf$profitQOQ <- NA
for(i in 1:nrow(nf)){
  nf[i, 14] = ifelse(i == 1, 0,
                           round(nf[i,8]/nf[i-1,8] - 1, 3))
}

nf %>% ggplot(aes(period)) + geom_line(aes(y = profitQOQ), group = 1) +
  geom_point(aes(y = profitQOQ)) +
  labs(title = "Performance de Margem de Lucro QoQ", x = "Desempenho trimestral", y = "Trimestre")
ggplotly()
# Analisando o período trimestral, conseguimos identificar quais são os quarters responsáveis pela
# performance de faturamento de 2016, 2017 e 2018 se manterem entre 21.4% a 24.5%. Nos Q2 de 2016 e
# 2017 tiveram a pior performance nestes respectivos anos, sendo que Q2/2017 obteve a primeira 
# performance negativa do dataset, -8.5%, em relação ao quarter anterior (isso não representa prejuízo!!!)
# e se mantendo abaixo de zero até o Q4/2017. Já em 2018, a melhor performance histórica, em comparação
# com o quater anterior, do dataset, entretanto, obteve a quarta performance negativa de -14.3%, sendo
# a pior performance dos quarters.

# Analisando custo e despesa de marketing por ano e plotando
colnames(nfAnnual)[9] <- "profitRate"
nfAnnual <- nfAnnual %>% mutate(costRate = round(cost/revenue,3),
                                mktRate = round(mkt/revenue,3))

nfAnnualFinan <- nfAnnual %>% select(period, profitRate, costRate, mktRate) %>%
  gather("revenuePart", "rate", -period) %>% arrange(period)

nfAnnualFinan %>% ddply(~period, summarise, sum(rate)) # Verificando se a soma compõe próximo a 1 (por conta do arrendondamento).

ggplot(nfAnnualFinan, aes(x= period, y= rate)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = revenuePart)) +
  labs(title= "Particionamento de faturamento Anual", x= "Período", y= "Taxa")
ggplotly()
# Conseguimos perceber que a participação de custos anuais tiveram o declínio ano após ano.
# As despesas de mkt também tiveram a redução porém a partir de 2016 esse tipo de despesa teve uma participação maior.
# Nesse mesmo período de 2016, a participação do lucro se manteve em 33.7% e 33.8%, a princípio, não podemos dizer que
# existe uma correlação pois não foi comprovado por nenhum método estatístico.

# Verificando se existe a correlação entre despesa com mkt e com o faturamento
cor(nf$mkt, nf$revenue)
cor(nf$cost, nf$revenue)
# Conseguimos perceber que existe uma correlação positiva entre despesa de mkt e faturamento, 88.8%.
# Porém não necessariamente isso significa relação de causalidade. Um outro ponto, a correlação é
# maior (quase que perfeita) entre faturamento e custo.


# CONCLUSÃO
# Baseado no histórico do dataset entre 2012 e 2018, existe uma um crescimento ascente na participação do lucro
# anual e mantendo estável (ou quase) nos anos 2016, 2017 e 2018. Caso o perfil do investidor seja mais arrojado,
# não seria adequado recomendado a investir na empresa, pois não existe a tendência de alavancagem se os fatores
# (internos e externos) não se alterarem. Caso o investidor tenha um perfil moderado, seria interessante investir
# mais visando nos dividendos com a tendência de estabilidade, pois a participação da margem de lucro é próximo de
# 34% nos últimos 3 anos históricos do dataset. Lembrando que o ideal não seria basear apenas nos dados
# históricos para investir em uma empresa. 