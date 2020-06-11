############################# Scripts do Artigo sobre Mercado de Trabalho em Imperatriz

# Carregar pacotes 
library(sidrar)
library(tidyverse)
library(deflateBR)
library(scales)
library(stringr)
library(tidyr)
library(data.table)
library(ggcorrplot)
library(stringr)

# Evitar notacao cientifica 
options(scipen=999)

#Coletar dados no site do IBGE por meio de API disponibilizada pela instituicao
dados<-get_sidra(api="/t/5938/n6/2105302/v/37,497,498,513,517,525,553,6575/p/last%2011/d/v37%200,v497%202,v498%200,v513%200,v517%200,v525%200,v553%202,v6575%200") 

# Selecionar variáveis a serem manipuladas (por exclusao)

pib<-dados %>% 
  select(-`Município (Código)`, -`Variável (Código)`, -`Ano (Código)`, -`Unidade de Medida (Código)`, -`Unidade de Medida`, -Município)


# Criar serie anual para analise 

times<-seq(from = as.Date('2007-01-01'), to = as.Date('2017-01-01'), by='year') 

# Criar coluna com PIB real deflacionado
pib<-pib %>% 
  
  mutate(real = deflate(pib$Valor, times, "01/2017", "ipca"))

# Criar dataset para Evolução do PIB real
evopib<-pib %>%  
  
  filter(Variável=='Produto Interno Bruto a preços correntes')


# Criar coluna temporal no dataset
evopib$times<-seq(from = as.Date('2007-01-01'), to = as.Date('2017-01-01'), by='year')



# Plotar o Gráfico de evolução do pib real e nominal para comparacao


ggplot(evopib, aes(x=times))+ 
  
  geom_line(aes(y=Valor, colour='Nominal'), size=.8)+ 
  
  geom_line(aes(y=real, colour='Real'), size=.8)+ 
  
  scale_y_continuous(labels = number_format(accuracy = 0.01, big.mark = ".", 
                                            
                                            decimal.mark =","))+ 
  
  labs(title = "Gráfico 1 - Evolução do Produto Interno Bruto de Imperatriz (2006-2017)", 
       
       y = "R$ Milhares", 
       
       x = "Ano",  
       
       colour = "PIB",  
       
       caption = "Fonte: Sistema de Contas Nacionais Municipais/IBGE. 

       Deflator do PIB: IPCA. Elaboração própria.")+ 
  
  theme_minimal()+ 
  
  theme(axis.title = element_text(size=12))+ 
  
  theme(axis.title = element_text(size=9, face = "bold"))+ 
  
  theme(axis.text.x = element_text(size=10, face = "bold.italic"))+ 
  
  theme(plot.title=element_text(face="bold", size=13))





###Criar Gráfico com valores adicionados por setor   


pibset<-pib %>%  
  
  filter(`Variável`!="Produto Interno Bruto a preços correntes", 

         `Variável`!='Participação do produto interno bruto a preços correntes no produto interno bruto a preços correntes da microrregião geográfica',  
         
         `Variável`!='Participação do produto interno bruto a preços correntes no produto interno bruto a preços correntes da unidade da federação', 
         
         `Variável`!='Valor adicionado bruto a preços correntes total') 



pibset$times<-seq(from = as.Date('2007-01-01'), to = as.Date('2017-01-01'), by='year') 



pibset$Variável<-pibset$Variável%>%  
  
  fct_recode(Agropecuária = 'Valor adicionado bruto a preços correntes da agropecuária',  
             
             Indústria = 'Valor adicionado bruto a preços correntes da indústria',  
             
             Administracao = 'Valor adicionado bruto a preços correntes da administração, defesa, educação e saúde públicas e seguridade social',  
             
             Serviços = 'Valor adicionado bruto a preços correntes dos serviços, exclusive administração, defesa, educação e saúde públicas e seguridade social') 


ggplot(pibset, aes(x=times, y=real, colour = fct_reorder2(Variável, times, real)))+ 
  geom_line(size=.8)+ 
  scale_y_continuous(labels = number_format(accuracy = 0.01, big.mark = ".", decimal.mark =","))+ 
  scale_x_date(expand = c(0, 2))+ 
  labs(title = "Gráfico 2 - Valor adicionado bruto por setor de atividade em Imperatriz", 
       y = "R$ Milhares", 
       x = "Ano",  
       colour = "Setor de Atividade",  
       caption = "Fonte: IBGE. Deflator: IPCA")+
  theme_minimal()+ 
  
  theme(axis.title = element_text(size=12))+ 
  
  theme(axis.title = element_text(size=9, face = "bold"))+ 
  
  theme(axis.text.x = element_text(size=10, face = "bold.italic"))+ 
  
  theme(plot.title=element_text(face="bold", size=13))



#Identificar a variacao com dataframe menor
evopib<-evopib %>% 
  mutate(variacao = (real/lag(real, 1, order_by = Ano) - 1)*100)

#Calcular taxa de crescimento anual composta do PIB 
# resultado = ((valor final/valor inicial) ^ (1/n - anos)*100
((6683377/3373548)^(1/10) - 1)*100


#Percentual de crescimento. Calculo manual
(6683377-3373548)/3373548*100


# Variacao do Valor adicionado por setor para analise e descricao 
#Filtrar antes as variaveis no dataframe e criar criar colunas de variacao 

industria<-pibset %>% 
  filter(Variável == 'Indústria') %>% 
  select(Ano, real) %>% 
  mutate(variacao = (real/lag(real, 1, order_by = Ano) - 1)*100)

Servicos<-pibset %>% 
  filter(Variável == 'Serviços') %>% 
  mutate(variacao = (real/lag(real, 1, order_by = Ano) - 1)*100)

Administracao<-pibset %>% 
  filter(Variável == 'Administração') %>% 
  mutate(variacao = (real/lag(real, 1, order_by = Ano) - 1)*100)


Agropecuaria<-pibset %>% 
  filter(Variável == 'Agropecuária') %>% 
  mutate(variacao = (real/lag(real, 1, order_by = Ano) - 1)*100)



######Dados sobre trabalho####

empresas<-get_sidra(api ='/t/6449/n6/2105302/v/662,708,2585/p/all/c12762/all/d/v662%200')

#empresas2<-get_sidra(api ='/t/6449/n6/2105302/v/2585/p/all/c12762/all')

#empresas<-read.xlsx("empresas1.xlsx", sheetIndex = 1)

empresas<-empresas %>% 
  filter(!is.na(Ano) & !is.na(Valor))

empresas<-empresas %>% 
  select(Variável, Ano, `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`, Valor) %>% 
  dplyr::rename(cnae = `Classificação Nacional de Atividades Econômicas (CNAE 2.0)`)


#numero de empresas
numero<-empresas %>% 
  filter(Variável == 'Número de empresas e outras organizações' & cnae=='Total')

numero$times<-seq(from = as.Date('2006-01-01'), to = as.Date('2017-01-01'), by='year')

ggplot(numero, aes(x=Ano, y=Valor, group=1))+
  geom_line(size=.8, color='#8DA0CB')+
  labs(title = "Gráfico 4 - Evolução do número de empresas em Imperatriz (2006-2017)",
       y = "Estabelecimentos", x = " ", caption = "Fonte: Cadastro Central de Empresas/IBGE. Elaboração própria")+
  geom_text(aes(label =Valor))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(linetype = "dashed"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.title = element_text(size=9, face = "bold"))+
  theme(axis.text.x = element_text(size=10))+
  theme(plot.title=element_text(face="bold", size=13))

# pessoal ocupado 

po<-empresas %>% 
  filter(Variável == 'Pessoal ocupado assalariado' & cnae == 'Total')

po$times<-seq(from = as.Date('2006-01-01'), to = as.Date('2017-01-01'), by='year')


ggplot(po, aes(x=times, y=Valor))+
  geom_line(size=.8, color='#8DA0CB')+
  labs(title = "Número de vínculos formais de emprego em Imperatriz (2006-2017)",
       y = "Em mil", x = "Ano", caption = "Cadastro Central de Empresas/IBGE")+
  geom_text(aes(label = Valor))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(linetype = "dashed"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.title = element_text(size=9, face = "bold"))+
  theme(axis.text.x = element_text(size=10))+
  theme(plot.title=element_text(face="bold", size=13))

#numero de empresas por setor

setores<-empresas %>% 
  filter(cnae != 'Total') %>% 
  filter(!is.na(Valor)) %>% 
  arrange(desc(Valor))

# filtrar o numero de empresas e organizacoes
setores2<- setores %>% 
  filter(Variável =='Número de empresas e outras organizações')# %>% 
  #top_n(20)
  


# Trabalhar com a RAIS. Fizemos uso da RAIS identificada, então os dados sao de acesso controlado


rais2017<-fread('ESTB2017ID.txt')


#estabelecimento identificado
suzano<-rais2017 %>% 
  filter(`Razão Social` =='SUZANO PAP E CELULOSE SA')%>% 
  group_by(`Qtd Vínculos Ativos`)%>% 
  summarize()


#vinculos 

rais2017<-fread('ESTB2017ID.txt', dec = ',')

rais2016<-fread('ESTB2016ID.txt')

rais2014$`CNPJ / CEI`

#MATEUS<-rais2017 %>% 
  #filter(`CNPJ / CEI` =='3995515008141')

#mateus2<-rais2017 %>% 
  #select(`Raz?o Social`, `Qtd V?nculos Ativos`, UF) %>% 
  #filter(`Raz?o Social` =="MATEUS SUPERMERCADOS S.A.") %>% 
  #group_by(UF) %>% 
  #summarise(soma_valor = sum(`Qtd V?nculos Ativos`))



suzano2017<-rais2017 %>% 
  select(`Razão Social`, `Qtd Vínculos Ativos`, UF) %>% 
  filter(`Razão Social` =="SUZANO PAP E CELULOSE S A" | `Razão Social` =="SUZANO PAP E CELULOSE SA") %>% 
  group_by(UF) %>% 
  summarise(soma_valor = sum(`Qtd Vínculos Ativos`))

suzano2016<-rais2016%>% 
  select(`Razão Social`, `Qtd Vínculos Ativos`, UF) %>% 
  filter(`Razão Social` =="SUZANO PAP E CELULOSE S A" | `Raz?o Social` =="SUZANO PAP E CELULOSE SA") %>% 
  group_by(UF) %>% 
  summarise(soma_valor = sum(`Qtd Vínculos Ativos`))


## 

# Dados gerais da RAIS 

rsetor<-fread('raisnovo.csv', dec = ',')


names(rsetor)<-c("Ano", "Extrativa Mineral", "Ind. Transformação", "Utilidade Publica", "Construção", "Comércio", "Servicos", "Adm. Pública", "Agropecuária, Pesca e Extrativismo", "total")

rsetor_piv<-pivot_longer(rsetor, cols = c(c("Extrativa Mineral", "Ind. Transformação", "Utilidade Publica", "Construção", "Comércio", "Servicos", "Adm. Pública", "Agropecuária, Pesca e Extrativismo")),
                         names_to = 'setor')

# Numeros da RAIS 
ggplot(rsetor_piv, aes(x=Ano, y=total))+
  geom_line(size=.8, colour = '#8DA0CB')+
  geom_point()+
  geom_text(aes(label=total, vjust = 1))+
  scale_y_continuous(breaks = seq(0, 59000, by=4000), labels = number_format(big.mark ='.'))+
  scale_x_continuous(limits =c(2006,2017),
                     breaks = c(2006, 2008, 2010, 2012, 2014, 2016))+
  labs(title = "Gráfico 5 - Distribuição dos vínculos formais de emprego (2006-2017)",
       y = "Em mil",
       x = "", 
       colour = "Setor de Atividade", 
       caption = "Fonte: RAIS/MTr. Elaboração própria.")+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(linetype = "dashed"))+
  theme(axis.title = element_text(size=12))+
  theme(axis.title = element_text(size=9, face = "bold"))+
  theme(axis.text.x = element_text(size=10))+
  theme(plot.title=element_text(face="bold", size=13))

#Percentual de crescimento 
(53.260-28.135)/28.135*100


x11()

ggplot(rsetor_piv, aes(x=Ano, y=value, colour = fct_reorder2(setor, Ano, value), group=setor))+
  geom_line(size=.8)+
   scale_y_continuous(labels = number_format(accuracy = 0.01, big.mark ='.'), 
                      limits =c(0,23000), 
                      breaks = seq(0, 23000, by=2000))+
  scale_x_continuous(limits =c(2006,2017),
                     breaks = c(2006, 2008, 2010, 2012, 2014, 2016))+
  labs(title = "Gráfico 6 - Distribuição dos vínculos formais por atividade economica (2006-2017)",
       y = "Em mil",
       x = " ", 
       colour = "Setor de Atividade", 
       caption = "Fonte: RAIS/MTr. Elaboração própria.")+
  theme_minimal()+
  theme(axis.title = element_text(size=12))+
  theme(axis.title = element_text(size=9, face = "bold"))+
  theme(axis.text.x = element_text(size=10))+
  theme(plot.title=element_text(face="bold", size=13))



ano<-c(2014, 2015, 2016, 2017, 2018)
valor<-c(455.88, 722.28, 581.93, 551.96, 871.35)

df<-data.frame(ano, valor)

df
ggplot(df, aes(x=ano, y=valor))+
  geom_bar(stat = 'identity', fill = "#8DA0CB")+
  geom_text(aes(label=valor, vjust = 0))+
  labs(x=" ", y="US$ Milhares de milhares", title="Gráfico 3 - Valor das exportações de Imperatriz", 
       caption = "Fonte: Secex/Ministério da Indústria, Comércio Exterior e Serviços. Elaboração própria")+
theme(axis.title = element_text(size=10, face = "bold"))+
  theme(axis.text.x = element_text(size=8, face = "bold"))+
  theme(plot.title=element_text(face="bold", size=13))+
  theme_minimal()


# Matriz de correlacao 

pibreal<-evopib$real

raiscorr<-rsetor %>% 
  select(Ano, total) %>% 
  arrange(Ano) %>% 
  head(11) %>% 
  select(total)


emprescorr<-head((empresas1),11)
emprescorr<-emprescorr %>% 
  select(valor)

datacorr<-data.frame(c(raiscorr, emprescorr))

datacorr$pib<-pibreal

names(datacorr)<-c("Empregos", "Empresas", "Pib")

correlacao<-round(cor(datacorr),2)

ggcorrplot(correlacao, hc.order = TRUE, type = "lower",
           lab = TRUE, method = "circle")+
labs(title = "Gráfico 7  - Correlação entre emprego formal, 
         número de empresas e PIB em Imperatriz",
       caption = "Elaboração própria")+
scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                     midpoint = 0, limit = c(-1,1), space = "Lab", 
                     name="Correlação de Person")




  
?ggcorrplot

library(reshape2)
melted_cormat <- melt(correlacao)
head(melted_cormat)

get_lower_tri<-function(correlacao){
  correlacao[upper.tri(correlacao)] <- NA
  return(correlacao)
}

get_upper_tri <- function(correlacao){
  correlacao[lower.tri(correlacao)]<- NA
  return(correlacao)
}

upper_tri <- get_upper_tri(correlacao)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlaçao de\nPerson") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(correlacao){
  dd <- as.dist((1-correlacao)/2)
  hc <- hclust(dd)
  cormat <-correlacao[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(correlacao)
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  labs(x=' ', y= ' ')+
  scale_fill_gradient2(low = "grey", high = "blue", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlaçao\n de Person") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  labs(title = "Gráfico 7  - Correlação entre emprego formal, número de empresas e PIB em Imperatriz", 
       caption = "Elaboração própria")+
  geom_text(aes(Var2, Var1, label = value), colour = "white", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



###

plot<-ggplot(datacorr, aes(x=Pib, y=Empregos))+
  labs(title = "Gráfico 8 - Regressão Linear entre empregos e PIB em Imperatriz", 
       x="Evoluçãodo PIB real", y="Empregos Formais", caption = "Elaboração própria")+
      geom_point(size=2, colour='black', alpha=0.5)+
  geom_point(size=1, colour='green', alpha=0.2)+
  geom_point(size=2, colour='black', alpha=0.5)+
  geom_point(size=1, colour='green', alpha=0.2)+
  scale_y_continuous(breaks = seq(0, 60000, by=10000), labels = number_format(big.mark ='.'))+
  scale_x_continuous(labels = number_format(big.mark ='.', decimal.mark = ','))+
  geom_smooth(method = 'lm', colour='blue')+
  theme_minimal()+
  annotate("text", x = 7000000, y = 47000, label = "Nova tendência \nna crise?")

