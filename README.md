# "Projeto Inclusão Escolar"

  Não há dúvidas de que uma das ferramentas mais importantes na educação é a produção de conhecimento de forma acessível e sistematizada, principalmente. Essa característica de rigor sistemático possibilita a geração de conhecimento e descrição estatística, por exemplo, da população educacional de uma cidade. Nessa direção a Lei No 13.005 de junho de 2014, de forma orientadora, descreve metas e estratégias para o desenvolvimento do Plano Nacional de Educação (PNE). Entretanto, há grande dificuldade de promover as ações estratégicas fomentadas no PNE por parte dos profissionais da Educação. Portanto, neste estudo questiona-se: quais variáveis que impedem a implantação das estratégias? Assim, este projeto tem o objetivo primordial de identificar variáveis de impacto ao sucesso de implantação das estratégias da Meta 4 do PNE. 
  
  Para isso foram identificadas diversas variáveis estruturais da escola, dos profissionais e atitudinais. Foi aplicado um questionário diretamente aos professores e auxiliares que trabalham com as crianças público da educação inclusiva de uma cidade da Região Metropolitana de Belo Horizonte-MG. Como resultado obteve-se um amplo diagnóstico da situação da educação inclusiva do município. Este tipo de pesquisa contribui e impacta as ações das políticas públicas da própria região investigada, pois devido às dimensões e contradições sociais do território brasileiro, onde contextos de desenvolvimento impactam na vida dos cidadãos, o diagnóstico específico da cidade mostra-se de alta relevância.

```r
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(likert)
library(lsr)
```

Importação dos dados salvos em Excel

```r
df_dados <- data.frame(read_excel("dados.xlsx"))
class(df_dados)
```

Todos os dados importados continham anormalidades que foram tratadas caso a caso em R, tais como: 
-Exclusão de colunas vazias 
-Exclusão de valores como "x"
-Na coluna NOME onde começar com x,y,z,1,2,3,4,. foi removido primeiro caracter
-Na coluna TIPO onde havia 1,4 foi removido primeiro caracter e substituído 2 por II
-Na coluna HORARIO onde havia 2,3,4 foi removido primeiro caracter
-Na coluna DISPONIBILIDADE onde havia 1,2,3,4 foi removido primeiro caracter
-Convertido todos os valores para formato minusculo
-Substituído "duvida" por "nao sei"

```r
df_dados$...7 <- NULL
df_dados$...18 <- NULL
df_dados[df_dados == "x"] <- NA
df_dados[df_dados == "X"] <- NA
df_dados$Nome <- str_replace(df_dados$Nome, "[12345xyz.]", "") 
df_dados$Tipo <- str_replace(df_dados$Tipo, "[14]","")
df_dados$Tipo <- str_replace(df_dados$Tipo, "[14]","")
df_dados$Tipo <- str_replace(df_dados$Tipo, "[2]","II")
df_dados$Horario <- str_replace(df_dados$Horario, "[234]","")
df_dados$Disponibilidade <- str_replace(df_dados$Disponibilidade, "[1234]","")
df_dados <- df_dados %>% 
  mutate_if(is.character, tolower)
df_dados[df_dados == "dúvida"] <- "nao sei"
df_dados[df_dados == "duvida"] <- "nao sei"
```

Utilizado função para remoção dos acentos das strings

```r
RemoveAcentos <- function(textoComAcentos) {
  
 
  if(!is.character(textoComAcentos)){
    on.exit()
  }
  
  
  letrasComAcentos <- "áéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ´`^~¨"
  
  
  letrasSemAcentos <- "aeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC     "
  
  textoSemAcentos <- chartr(
    old = letrasComAcentos,
    new = letrasSemAcentos,
    x = textoComAcentos
  ) 
  
  return(textoSemAcentos)
}

df_dados <- df_dados %>% 
  mutate_if(is.character, RemoveAcentos)
```

Associando colunas para o tipo fator

```r
df_dados$Inclusão <- as.factor(df_dados$Inclusão)
df_dados$AEE <- as.factor(df_dados$AEE)
df_dados$Plano.Pedagogico <- as.factor(df_dados$Plano.Pedagogico)
df_dados$Estrutura <- as.factor(df_dados$Estrutura)
df_dados$Diretriz <- as.factor(df_dados$Diretriz)
df_dados$Função <- as.factor(df_dados$Função)

df_dados$Acessibilidade <- as.factor(df_dados$Acessibilidade)
df_dados$Trabalho.diferenciado <- as.factor(df_dados$Trabalho.diferenciado)
df_dados$Conhecimento.Lei <- as.factor(df_dados$Conhecimento.Lei)
df_dados$Leitura.PNE <- as.factor(df_dados$Leitura.PNE)
df_dados$Atitude <- as.factor(df_dados$Atitude)
df_dados$Funções.AEE <- as.factor(df_dados$Funções.AEE)
df_dados[df_dados == "acompanhamento familiar e psico"] <- "acompanhamento familiar, psico"
``` 

Subdividindo coluna INDISPENSAVEL em 6

```r
df_dados2 <- df_dados %>% 
  separate(
    col = Indispensavel, 
    into = c("indisp_1", "indisp_2", "indisp_3", "indisp_4", "indisp_5","indisp_6"), 
    sep = "\\,", 
    extra = "drop"
  )
```

Corrigindo repetições psico e estrutura familiar

```r
df_dados2[df_dados2 == " acompanhamento psico"] <- "psico"
df_dados2[df_dados2 == "acompanhamento psico"] <- "psico"

df_dados2[df_dados2 == " estutura"] <- "estrutura"
df_dados2[df_dados2 == " acompanhamento familar"] <- "acompanhamento familiar"
df_dados2[df_dados2 == " quantidade de profissionais"] <- "disponibilidade de profissional"
```

Fatorizando as novas colunas INDISP

```r
df_dados2$indisp_1 <- as.factor(df_dados2$indisp_1)
df_dados2$indisp_2 <- as.factor(df_dados2$indisp_2)
df_dados2$indisp_3 <- as.factor(df_dados2$indisp_3)
df_dados2$indisp_4 <- as.factor(df_dados2$indisp_4)
df_dados2$indisp_5 <- as.factor(df_dados2$indisp_5)
df_dados2$indisp_6 <- as.factor(df_dados2$indisp_6)
```

Subdividindo as colunas TIPO, HORARIO, DISPONIBILIDADE e fatorizando-as

```r
df_dados2 <- df_dados2 %>% 
  separate(
    col = Tipo, 
    into = c("Tipo_1","Tipo_2","Tipo_3","Tipo_4","Tipo_5"), 
    sep = "\\,", 
    extra = "drop"
  )

df_dados2$Tipo_1 <- as.factor(df_dados2$Tipo_1)
df_dados2$Tipo_2 <- as.factor(df_dados2$Tipo_2)
df_dados2$Tipo_3 <- as.factor(df_dados2$Tipo_3)
df_dados2$Tipo_4 <- as.factor(df_dados2$Tipo_4)
df_dados2$Tipo_5 <- as.factor(df_dados2$Tipo_5)

df_dados2 <- df_dados2 %>% 
  separate(
    col = Horario, 
    into = c("Horario_1","Horario_2","Horario_3"), 
    sep = "\\,", 
    extra = "drop"
  )

df_dados2$Horario_1 <- as.factor(df_dados2$Horario_1)
df_dados2$Horario_2 <- as.factor(df_dados2$Horario_2)
df_dados2$Horario_3 <- as.factor(df_dados2$Horario_3)

df_dados2 <- df_dados2 %>% 
  separate(
    col = Disponibilidade, 
    into = c("Disp_1","Disp_2","Disp_3"), 
    sep = "\\,", 
    extra = "drop"
  )

#Fatorizando novas colunas DISPONIBILIDADE
df_dados2$Disp_1 <- as.factor(df_dados2$Disp_1)
df_dados2$Disp_2 <- as.factor(df_dados2$Disp_2)
df_dados2$Disp_3 <- as.factor(df_dados2$Disp_3)
```

Combinando e contando as repetições em INDISPENSAVEL

```r
didatica <- df_dados2 %>% 
     filter_all(any_vars(str_detect(., pattern = "didatica"))) %>% 
     count(., sort = TRUE)

qualificacao <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "qualificacao"))) %>% 
  count(., sort = TRUE)

estrutura <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "estrutura"))) %>% 
  count(., sort = TRUE)

material <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "material"))) %>% 
  count(., sort = TRUE)

psico <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "psico"))) %>% 
  count(., sort = TRUE)

familiar <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "familiar"))) %>% 
  count(., sort = TRUE)

auxiliar <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "auxiliar"))) %>% 
  count(., sort = TRUE)

sala <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "sala"))) %>% 
  count(., sort = TRUE)

respeito <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "respeito"))) %>% 
  count(., sort = TRUE)

amor <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "amor"))) %>% 
  count(., sort = TRUE)

investimento <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "investimento"))) %>% 
  count(., sort = TRUE)

integral <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "integral"))) %>% 
  count(., sort = TRUE)

prioritario <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "prioritario"))) %>% 
  count(., sort = TRUE)

vontade <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "vontade"))) %>% 
  count(., sort = TRUE)

disp_profissional <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "disponibilidade"))) %>% 
  count(., sort = TRUE)

efetividade <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "efetividade"))) %>% 
  count(., sort = TRUE)

horario_diferenciado <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "horario"))) %>% 
  count(., sort = TRUE)

lei <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "lei"))) %>% 
  count(., sort = TRUE)

normas <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "norma"))) %>% 
  count(., sort = TRUE)

planejamento <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "planejamento"))) %>% 
  count(., sort = TRUE)

interdisciplinar <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "interdisciplinar"))) %>% 
  count(., sort = TRUE)

multidisciplinar <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "multidisciplinar"))) %>% 
  count(., sort = TRUE)

semed <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "semed"))) %>% 
  count(., sort = TRUE)

divulgacao <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "divulgacao"))) %>% 
  count(., sort = TRUE)

financeiro <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "financeiro"))) %>% 
  count(., sort = TRUE)

menos <- df_dados2 %>% 
  filter_all(any_vars(str_detect(., pattern = "menos"))) %>% 
  count(., sort = TRUE)

```

Criando o dataframe com as variáveis indispensáveis e sua porcentagem de peso

```r
Df <- rbind(didatica, qualificacao, estrutura, material, psico, familiar, auxiliar, sala, respeito, amor, investimento, integral, prioritario, vontade, disp_profissional, efetividade, horario_diferenciado, lei, normas, planejamento, interdisciplinar, multidisciplinar,semed, divulgacao, financeiro, menos )
Df<-Df %>%
  mutate(prop = n / sum(n), prop = scales::percent(prop))

  Df$variaveis <- c("Didatica", "Qualificacao", "Estrutura", "Material", "Acompanhamento Psicológico", "Acompanhamento Familiar", "Auxiliar de Turma", "Sala Especial", "Respeito", "Amor", "Investimento do Governo", "Atendimento Integral", "Atendimento Prioritario", "Vontade","Disponibilidade de Profissional", "Efetividade","Horario Diferenciado","Melhoria da Lei","Normas","Planejamento Estudantil","Trabalho Interdisciplinar","Trabalho Multidisciplinar","Atitude do Semed","Divulgacao","Recurso Financeiro","Menos Alunos") 

glimpse(Df)
```

Plotagem das variáveis indispensáveis

```r
plot_final<-Df %>%
  arrange_(~ desc(n)) %>%
  group_by_(~ n) %>%
  head(n=7)
  
  plot_outros<-subset(Df, n<=13)%>%
  arrange_(~ desc(n)) %>%
  group_by_(~ n)
  
  ggplot(plot_final, aes(x = reorder(variaveis, -n),y=n, fill = as.factor(n))) + 
    geom_histogram(stat = "identity", alpha=0.7, binwidth =0.5) +
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2")+
    labs(fill = "Votos", x="", y="",title="Soluções Indispensáveis para a Melhoria da Inclusão nas Escolas")+
    theme(plot.title = element_text(hjust = 0.5))
``` 

![alt text](https://github.com/CESARMALMSTEEN/R/blob/master/1.jpeg) 
  
```r
 ggplot(plot_outros, aes(x = reorder(variaveis, +n),y=n, fill = as.factor(n))) +
   geom_histogram(stat = "identity") +
   labs(fill = "Votos", x="Atributos", title="Outras soluções indispensáveis")+
   scale_y_continuous(name = "Quantidade", breaks = seq(0, 18, 2),expand = c(0, 1))+
   scale_fill_brewer(palette = 1)+
   theme_light()+
   coord_flip() 
```

![alt text](https://github.com/CESARMALMSTEEN/R/blob/master/2.jpeg)

Avaliando e combinando os histogramas das variáveis INFANTIL e FUNDAMENTAL

```r
 #INFANTIL
   prof_infantil <- filter_all(df_dados2, any_vars(str_detect(., pattern = "infantil")))
   prof_infantil_plot <- subset(prof_infantil, select = c(Inclusão, AEE, Plano.Pedagogico, Estrutura, Diretriz))
   
   g5 <- plot(likert(prof_infantil_plot),
   centered = TRUE, wrap=40, ordered = TRUE)+
   labs(title="Professores da Rede Infantil")+
   theme(plot.title = element_text(hjust = 0.5))
   
  #FUNDAMENTAL
   prof_fundamental <- filter_all(df_dados2, any_vars(str_detect(., pattern = "fundamental")))
   prof_fundamental_plot <- subset(prof_fundamental, select = c(Inclusão, AEE, Plano.Pedagogico, Estrutura, Diretriz))
   g4 <- plot(likert(prof_fundamental_plot),
   centered = TRUE, wrap=40, ordered = TRUE)+
   labs(title="Professores da Rede Fundamental")+
   theme(plot.title = element_text(hjust = 0.5))
   
   grid.arrange(g4,g5, ncol=2)
  
```

Análise do questionamento comparando Conhecimento de lei, funções do AEE e Leitura PNE

```r
prof_infantil <- filter_all(df_dados2, any_vars(str_detect(., pattern = "infantil")))
   
   lei<-prof_infantil %>%
     group_by(Conhecimento.Lei) %>%
     count()
   
   pne<-prof_infantil %>%
     group_by(Leitura.PNE) %>%
     count()
   
  aee<- prof_infantil %>%
     group_by(Funções.AEE) %>%
     count()
   
   prof_infantil_conhecimento <-data.frame (cbind(lei,pne,aee),
   Professor = c("Professor Infantil"))  
   
   View(prof_infantil_conhecimento)
   
   prof_fundamental <- filter_all(df_dados2, any_vars(str_detect(., pattern = "fundamental")))
   View(prof_fundamental)
   
   lei<-prof_fundamental %>%
     group_by(Conhecimento.Lei) %>%
     count()
   
   pne<-prof_fundamental %>%
     group_by(Leitura.PNE) %>%
     count()
   
   aee<- prof_fundamental %>%
     group_by(Funções.AEE) %>%
     count()
   
   prof_fundamental_conhecimento <-data.frame(cbind(lei,pne,aee),
   Professor = c("Professor Fundamental"))
   
   prof_final <-plyr::rbind.fill(prof_fundamental_conhecimento,prof_infantil_conhecimento)
     
   View(prof_final)
   
   par(mfrow = c(1,2), oma = c(4,1,1,1))
   
   g1 <- ggplot(prof_final, aes(x = reorder(Conhecimento.Lei, -n), y=n)) +
     geom_col(aes(fill = Professor), position = "dodge")+
     scale_y_continuous(name = "Votos", breaks = seq(0, 170, 20),expand = c(0, 1))+
     labs(title="Conhecimento de Lei", x="")+
      theme(legend.position=c(.8, .8),plot.title = element_text(hjust = 0.5))
   
   g2 <- ggplot(prof_final, aes(x = reorder(Leitura.PNE, -n), y=n1)) +
     geom_col(aes(fill = Professor), position = "dodge")+
     scale_y_continuous(breaks = seq(0, 170, 20),expand = c(0, 1))+
     labs(title="Leitura do PNE", x="", y="")+ theme(legend.position=c(.8, .8),plot.title = element_text(hjust = 0.5))
   
   g3 <- ggplot(prof_final, aes(x = reorder(Funções.AEE, -n), y=n2)) +
     geom_col(aes(fill = Professor), position = "dodge")+
     scale_y_continuous(breaks = seq(0, 170, 20),expand = c(0, 1))+
     labs(title="Conhecimento das Funções do AEE", x="", y="")+ 
     theme(legend.position=c(.8, .8),plot.title = element_text(hjust = 0.5))
     
   grid.arrange(g1, g2, g3, ncol=3)
   
```
![alt text](https://github.com/CESARMALMSTEEN/R/blob/master/3.jpeg)





 
