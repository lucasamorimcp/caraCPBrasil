
#Pacotes utilizados

library(readxl)
library(geobr)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(stringi)
library(ggplot2)
library(scales)
library(patchwork)
library(gridExtra)
library(grid)
library(ggraph)
library(quanteda.textplots)
library(wordcloud2)
library(tidyverse)
library(tokenizers)
library(quanteda)
library(tm)
library(SnowballC)
library(stm)
library(seededlda)
library(igraph)
library(MatchIt)
library(broom)
library(tidygraph)
library(ggraph)
library(ggrepel)
library(purrr)
library(stringr)
library(svglite)
library(rlang)

#Baixando dataframe

Data <- read_excel("Data.xlsx")

#Transformando os valores "NA" em valores NA

Data[Data == "NA"] <- NA

#Colocando as variaveis numericas em formato numerico

Data$Nota <- as.numeric(Data$Nota)
Data$Ano_Obtencao <- as.numeric(Data$Ano_Obtencao)
Data$Cit <- as.numeric(Data$Cit)
Data$Cit5 <- as.numeric(Data$Cit5)
Data$h <- as.numeric(Data$h)
Data$h5 <- as.numeric(Data$h5)
Data$i10 <- as.numeric(Data$i10)
Data$i105 <- as.numeric(Data$i105)

###MAPAS DOCENTES POR ESTADO

#Criando subsets por tipo de programa

Data_CP  <- Data[Data$Tipo == "CIÊNCIA POLÍTICA", ]
Data_ADM <- Data[Data$Tipo == "ADMINISTRAÇÃO PÚBLICA", ]
Data_PP  <- Data[Data$Tipo == "POLÍTICAS PÚBLICAS", ]
Data_S   <- Data[Data$Tipo == "SOCIOLOGIA", ]

#Criando objeto com mapa do Brasil

estados <- read_state(year = 2020, simplified = TRUE)

#Populacao por estado em milhoes

populacao <- tibble::tribble(
  ~name_state,           ~pop_milhoes,
  "Acre",                 0.9,
  "Alagoas",              3.3,
  "Amapá",                0.9,
  "Amazônas",             4.2,
  "Bahia",               14.9,
  "Ceará",                9.0,
  "Distrito Federal",     3.1,
  "Espírito Santo",       4.1,
  "Goiás",                7.3,
  "Maranhão",             7.1,
  "Mato Grosso",          3.7,
  "Mato Grosso Do Sul",   2.8,
  "Minas Gerais",        20.7,
  "Pará",                 9.0,
  "Paraíba",              4.0,
  "Paraná",              11.6,
  "Pernambuco",          10.0,
  "Piauí",                3.3,
  "Rio De Janeiro",      17.2,
  "Rio Grande Do Norte",  3.6,
  "Rio Grande Do Sul",   11.3,
  "Rondônia",             1.8,
  "Roraima",              0.7,
  "Santa Catarina",       7.3,
  "São Paulo",           46.0,
  "Sergipe",              2.4,
  "Tocantins",            1.6
)

#Criando funcao para contar numero de docentes por estado para usar em todos os df
conta_por_estado <- function(df) {
  df %>% group_by(Estado) %>% summarise(n_docentes = n(), .groups = "drop")
}
join_com_mapa <- function(tab_estado) {
  estados %>%
    left_join(tab_estado, by = c("name_state" = "Estado")) %>%
    left_join(populacao, by = "name_state") %>%
    mutate(
      n_docentes = ifelse(is.na(n_docentes), 0, n_docentes),
      dens_doc = n_docentes / (pop_milhoes * 1e6) * 1e6
    )
}

#Rodando funcao por df
doc_total <- conta_por_estado(Data)
doc_CP    <- conta_por_estado(Data_CP)
doc_PP    <- conta_por_estado(Data_PP)
doc_ADM   <- conta_por_estado(Data_ADM)
doc_S     <- conta_por_estado(Data_S)

join_com_mapa(doc_total) %>%
  st_drop_geometry() %>%                   
  select(name_state, n_docentes, pop_milhoes, dens_doc) %>%
  arrange(desc(dens_doc))

#Criando funcao para criar um mapa para cada dataframe
plot_mapa <- function(tab_estado, titulo) {
  dados_plot <- join_com_mapa(tab_estado)
  ggplot(dados_plot) +
    geom_sf(aes(fill = dens_doc), color = "grey60", size = 0.3) +
    scale_fill_gradient(
      name = "Docentes por milhão de hab.",
      low = "grey90",
      high = "black",
      na.value = "white"
    ) +
    coord_sf() +
    theme_minimal(base_size = 11) +
    labs(title = titulo) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

#Criando mapas
g_brasil <- plot_mapa(doc_total, "") #GRAFICO 1

#GRAFICO 2
g_cp  <- plot_mapa(doc_CP,  "PPGs de Ciência Política")
g_pp  <- plot_mapa(doc_PP,  "PPGs de Políticas Públicas")
g_adm <- plot_mapa(doc_ADM, "PPGs de Administração Pública")
g_soc <- plot_mapa(doc_S,   "PPGs de Ciências Sociais")

#Colocando mapas por area em um grid 2x2
grid_2x2 <- (g_cp + g_pp) / (g_adm + g_soc)

#Exportando para formato PNG
ggsave("mapa_brasil_todos.png", plot = g_brasil, width = 8, height = 6, dpi = 300)
ggsave("mapas_2x2_categorias.png", plot = grid_2x2, width = 14, height = 10, dpi = 300)

###PROPORCAO DE MULHERES
  
#Determinando categorias que vao aparecer no grafico
tipos_completos <- c("CIÊNCIA POLÍTICA", "POLÍTICAS PÚBLICAS",
                       "ADMINISTRAÇÃO PÚBLICA", "SOCIOLOGIA", "HISTÓRIA", "TOTAL")

#Contagem de docentes por tipo de programa e sexo
sexo_tipo <- Data %>%
  count(Tipo, Sexo) %>%
  group_by(Tipo) %>%
  mutate(prop = n / sum(n),
         pct  = round(100 * prop, 1)) %>%
  ungroup()

#Proporcao total
sexo_total <- Data %>%
  count(Sexo) %>%
  mutate(
    Tipo = "TOTAL",
    prop = n / sum(n),
    pct = round(100 * prop, 1)
  )

#Combinando tipos com total
sexo_tipo <- bind_rows(sexo_tipo, sexo_total)

#Completando caso tenham valores ausentes
sexo_tipo <- sexo_tipo %>%
  complete(Tipo = tipos_completos, Sexo = c("Feminino", "Masculino"), fill = list(n = 0, prop = 0, pct = 0))

#Ajustando nomes
sexo_tipo$Tipo[sexo_tipo$Tipo == "HISTÓRIA"] <- "CPDOC"
sexo_tipo$Tipo[sexo_tipo$Tipo == "SOCIOLOGIA"] <- "CIÊNCIAS SOCIAIS"

#Selecionando apenas feminino
fem_share <- sexo_tipo %>%
  filter(Sexo == "Feminino") %>%
  arrange(desc(prop))

#Media geral como linha de referencia no grafico
linha_media <- mean(fem_share$prop[fem_share$Tipo != "TOTAL"], na.rm = TRUE)

#Definindo as cores dos graficos antes para ficar tudo igual
cor_ref <- "grey40" 
cor_seg <- "grey75"  
cor_pt  <- "black"   

#definindo o tema do grafico a priori pra poder usar depois
theme_lucas <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 11)
    )
}

#Grafico proporcao de mulheres por tipo de programa (GRAFICO 3)
p_fem <- ggplot(fem_share, aes(x = prop, y = reorder(Tipo, prop))) +
  geom_vline(xintercept = linha_media, linetype = "dashed", color = cor_ref) +
  geom_segment(aes(x = 0, xend = prop, yend = Tipo), color = cor_seg, linewidth = 1.2) +
  geom_point(size = 3.5, color = cor_pt) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            nudge_x = 0.02, hjust = 0, size = 3.3, color = cor_pt) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = NULL, y = NULL, title = "") +
  theme_lucas()

ggsave("grafico_proporcao_feminina.png", plot = p_fem, width = 8, height = 5, dpi = 300, bg = "white")

###SEXO DO ORIENTADOR
  
#Criando variaveis numericas de sexo para rodar os modelos
Data$sexo_num <- ifelse(Data$Sexo == "Masculino", 1,
                          ifelse(Data$Sexo == "Feminino", 0, NA))

Data$sexo_o_num <- ifelse(Data$Sexo_O == "Masculino", 1,
                          ifelse(Data$Sexo_O == "Feminino", 0, NA))

#Retirando valores omissos
Data_reg <- Data[!is.na(Data$sexo_num) & !is.na(Data$sexo_o_num), ]

#Modelo de regressao simples
modelo_sexo <- lm(sexo_num ~ sexo_o_num, data = Data_reg)
summary(modelo_sexo)

#Rodando valores preditos
newdata <- data.frame(sexo_o_num = c(0, 1))
pred <- predict(modelo_sexo, newdata = newdata, se.fit = TRUE)

#Tirando outputs do modelos
newdata$fit <- pred$fit
newdata$low <- pred$fit - 1.96 * pred$se.fit
newdata$high <- pred$fit + 1.96 * pred$se.fit
newdata$label <- c("Feminino", "Masculino")

newdata

#Plotando modelo de valores preditos de probabilidade de ser orientado por masculino por sexo (GRAFICO 4)
pred_sex <- ggplot(newdata, aes(x = label, y = fit)) +
  geom_col(fill = cor_seg, color = cor_ref, width = 0.7) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, color = cor_pt) +
  geom_text(aes(label = paste0(round(fit * 100, 1), "%")),
            vjust = -2, size = 4, color = cor_pt) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "",
    subtitle = "",
    x = "Sexo do docente",
    y = "Probabilidade de ter tido\norientador do sexo Masculino"
  ) +
  theme_lucas(13) +
  theme(axis.text.x = element_text(face = "bold"))

ggsave("preditos_linear.png", plot = pred_sex, dpi = 320, width = 7, height = 5, bg = "white")

###PERCENTUAL DE MULHERES DOCENTES AO LONGO DOS ANOS

#Garantindo que nao tenham valores ausentes em Ano_Obtencao e transformando sexo em uma dummy
Data_clean_Ano <- Data %>%
  mutate(
    Ano = suppressWarnings(as.integer(Ano_Obtencao)),
    sexo_num = case_when(
      Sexo == "Masculino" ~ 1,
      Sexo == "Feminino"  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(Ano))

#Proporcao de mulheres por Ano
df_year <- Data_clean_Ano %>%
  group_by(Ano) %>%
  summarise(
    n_total = n(),
    n_mulheres = sum(sexo_num == 0, na.rm = TRUE),
    prop_mulheres = n_mulheres / n_total,
    .groups = "drop"
  )

#GRAFICO 5

scale_factor <- max(df_year$n_total, na.rm = TRUE)

mulheres_prop <- ggplot() +
  geom_col(data = df_year, aes(x = Ano, y = n_total),
           fill = cor_seg, color = cor_ref, width = 0.9) +
  geom_line(data = df_year, aes(x = Ano, y = prop_mulheres * scale_factor),
            linewidth = 1, color = cor_pt) +
  geom_point(data = df_year, aes(x = Ano, y = prop_mulheres * scale_factor),
             size = 2, color = cor_pt) +
  geom_smooth(
    data = df_year,
    aes(x = Ano, y = prop_mulheres * scale_factor),
    method = "lm", se = FALSE, linewidth = 1, linetype = "dashed", color = "darkred"
  ) +
  scale_y_continuous(
    name = "Frequência de docentes por ano de formação",
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Proporção de mulheres",
                        labels = scales::percent_format(accuracy = 1))
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Ano de obtenção do doutorado", title = "") +
  theme_lucas()

ggsave("grafico_paper.png", plot = mulheres_prop, width = 7, height = 4.5, dpi = 320, bg = "white")

#PRINCIPAIS FORMACOES

table(Data$Formacao)

#Criando pares fixos de algumas formacoes importantes com nomes compostos
pares_fixos <- c("Administração Pública", "Ciência Política","Ciências Sociais",
                 "Administração de Empresas","Engenharia de Produção",
                 "Relações Internacionais","Serviço Social","Sociologia Política")

formacoes <- Data$Formacao

#Adicionando "_" entre os pares fixos
for (p in pares_fixos) {
  formacoes <- str_replace_all(formacoes, fixed(p), str_replace_all(p, " ", "_"))
}

Data$Formacao <- formacoes

#Criando um corpus usando a variavel de formacao
corpus_formacao <- corpus(Data, text_field = "Formacao")

#tokenizando e criando df
toks_formacao <- tokens(corpus_formacao, remove_punct = TRUE, remove_numbers=TRUE)
toks_formacao <- tokens_wordstem(toks_formacao)
toks_formacao <- tokens_select(toks_formacao,  stopwords("pt"), selection = "remove")
dfm_formacao <- dfm(toks_formacao)
dfm_formacao

#retirando termos que aparecem em menos de 2,5%
dfm_trimmed_formacao <- dfm_trim(dfm_formacao, min_docfreq = 0.025, docfreq_type = "prop")

#Arrumando os conceitos que havia editado para melhor visualizacao
map <- c(
  "ciência_política"      = "Ciência Política",
  "administração_pública" = "Administração Pública",
  "história"              = "História",
  "educação"              = "Educação",
  "política"              = "Política",
  "sociologia"            = "Sociologia",
  "ciências_sociai"      = "Ciências Sociais",
  "desenvolvimento"       = "Desenvolvimento",
  "economia"              = "Economia",
  "social"                = "Social",
  "governo"               = "Governo",
  "ciência"               = "Ciência",
  "direito"               = "Direito",
  "administração"         = "Administração",
  "antropologia"          = "Antropologia"
)

dfm_pretty_formacao <- dfm_trimmed_formacao |>
  dfm_replace(
    pattern = names(map),
    replacement = unname(map)
  ) |>
  dfm_compress()

#Gerando nuvem de palavras (GRAFICO 6)
textplot_wordcloud(dfm_pretty_formacao, col="black")

png("nuvem_formacao.png", width = 2000, height = 2000, res = 300)

textplot_wordcloud(
  dfm_pretty_formacao,
  color = "black",
  max_words = 100,
  random_order = FALSE
)

dev.off()

#ANALISE DE REDES DE ORIENTACAO

#Limpeza para analise de redes
clean_str <- function(x){
  x |> as.character() |> str_trim() |> str_squish()
}

#Limpando variaveis nome, orientador e instituicao de formacao
df <- Data |>
  transmute(
    Nome        = clean_str(Nome),
    Orientador  = clean_str(Orientador),
    Instituicao = clean_str(Instituicao_Formacao)
  ) |>
  mutate(across(everything(), ~na_if(., "")))

#Criando pares de aresta entre essas 3 variaveis
edges <- bind_rows(
  df |> filter(!is.na(Nome),       !is.na(Orientador))  |> transmute(a = Nome,       b = Orientador),
  df |> filter(!is.na(Nome),       !is.na(Instituicao)) |> transmute(a = Nome,       b = Instituicao),
  df |> filter(!is.na(Orientador), !is.na(Instituicao)) |> transmute(a = Orientador, b = Instituicao)
) |>
  filter(a != b)

#criando nos a partir do numero de vezes que os individuos ou instituicoes aparecem
node_counts <- bind_rows(
  edges |> select(name = a),
  edges |> select(name = b)
) |>
  count(name, name = "count") |>
  arrange(desc(count))

#remover nos com count <= 2 (ou seja, aqueles que nunca orientaram ninguem)
node_counts_f <- node_counts |> filter(count > 2)

#manter apenas arestas cujos dois nos estao no conjunto filtrado
keep <- node_counts_f$name
edges_f <- edges |> filter(a %in% keep, b %in% keep)

#rotular exatamente os 115 com mais contagens (aqueles que orientaram pelo menos 2 outros professores)
top115_names <- node_counts_f |>
  slice_max(order_by = count, n = 115, with_ties = FALSE) |>
  pull(name)

node_counts_f <- node_counts_f |>
  mutate(show_label = name %in% top115_names)

#dividindo por 2 porque cada um aparece duplicado por serem combinacoes com 2 variaveis
node_counts_f$count <- node_counts_f$count/2

n_lines <- nrow(edges_f)

#Grafico - Redes de principais orientadores (GRAFICO 7)
g <- graph_from_data_frame(d = edges_f, directed = FALSE, vertices = node_counts_f)

set.seed(123)
redes_g <- ggraph(g, layout = "fr") +
  geom_edge_link(color = "grey55", alpha = 0.2) +
  geom_node_point(aes(size = count, color = count)) +
  geom_node_text(
    aes(label = ifelse(show_label, name, "")),
    repel = TRUE,
    max.overlaps = Inf,
    size = 3
  ) +
  scale_size_continuous(name = "Aparições (arestas)") +
  scale_color_gradient(name = "Aparições", low = "grey80", high = "black") +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#Salvando imagem
ggsave(
  filename = "rede_pessoas_instituicoes.png",
  plot = redes_g,
  width = 19,   
  height = 10, 
  dpi = 300
)

#Como SVG pra melhorar a visualizacao
ggsave(
  filename = "rede_pessoas_instituicoes.svg",
  plot = redes_g,
  width = 19,
  height = 10
)

###PRINCIPAIS AREAS DE ATUACAO

#Funcao para remover acentos e padronizar textos
remove_acentos <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII") 
  x <- tolower(x)                           
  x <- str_squish(x)                        
  return(x)
}

#Juntando areas de interesse no resumo com areas de atuacao
Data$texto <- paste(Data$Lattes_Resumo, Data$Area, sep = " ")

#Remove acentos antes de criar o corpus
Data$texto <- remove_acentos(Data$texto)

#Cria o corpus
corpus_texto <- corpus(Data, text_field = "texto")

#Tokenização e limpeza
toks_texto <- tokens(corpus_texto, remove_punct = TRUE, remove_numbers = TRUE)
toks_texto <- tokens_wordstem(toks_texto, language = "portuguese")
toks_texto <- tokens_select(toks_texto, stopwords("pt"), selection = "remove")

#Cria dfm
dfm_texto <- dfm(toks_texto)

#Trim (remove termos com freq < 5%)
dfm_trimmed_texto <- dfm_trim(dfm_texto, min_docfreq = 0.05, docfreq_type = "prop")

#Extrai apenas termos mantidos
kept_feats <- featnames(dfm_trimmed_texto)
toks_trimmed <- tokens_select(toks_texto, pattern = kept_feats, selection = "keep", padding = FALSE)

#Cria coluna de texto processado no df principal
Data$texto_trimmed <- vapply(
  toks_trimmed,
  function(x) if (length(x)) paste(x, collapse = " ") else "",
  FUN.VALUE = character(1)
)

#criando objetos com stopwords em PT
stop_pt <- stopwords("pt")

#Processando para analise do STM
temp <- textProcessor(
  documents = Data$texto_trimmed,
  metadata = Data,
  language = "portuguese",    
  customstopwords = stop_pt,   
  stem = TRUE                  
)

#colocando no formato para analise STM
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

#rodando modelo STM com 10 topicos
model.stm <- stm(out$documents, out$vocab, K = 10,
                 prevalence = ~ Sigla + s(Ano_Obtencao),
                 data = out$meta)

#olhando topicos
labelTopics(model.stm)  

#Plotando topicos mais influentes
plot(model.stm, n=5, labeltype="frex")
png("figura4_topicos_stm.png", width = 3000, height = 2200, res = 300)
plot(model.stm, n = 5, labeltype = "frex")
dev.off()

#Olhando topico por topico para classificar
findThoughts(model.stm, texts=out$meta$texto, topics=1, n=3) #Direito
findThoughts(model.stm, texts=out$meta$texto, topics=2, n=3) #Comportaento e Instituições
findThoughts(model.stm, texts=out$meta$texto, topics=3, n=3) #Saúde e Meio Ambiente
findThoughts(model.stm, texts=out$meta$texto, topics=4, n=3) #Administração e Políticas Públicas
findThoughts(model.stm, texts=out$meta$texto, topics=5, n=3) #História do Pensamento Político
findThoughts(model.stm, texts=out$meta$texto, topics=6, n=3) #Ciências Sociais
findThoughts(model.stm, texts=out$meta$texto, topics=7, n=3) #Educação
findThoughts(model.stm, texts=out$meta$texto, topics=8, n=3) #Economia Política
findThoughts(model.stm, texts=out$meta$texto, topics=9, n=3) #Teoria Democrática e Participação
findThoughts(model.stm, texts=out$meta$texto, topics=10, n=3) #Relações Internacionais

#Pegando valores relativos a cada individuo com a proporcao do texto associada a cada topico
theta <- model.stm$theta
K <- ncol(theta)
topic_cols <- paste0("topic_", 1:K)
colnames(theta) <- topic_cols

topics_df <- bind_cols(out$meta, as.data.frame(theta)) %>%
  mutate(
    topic_dom_id   = max.col(select(., starts_with("topic_")), ties.method = "first"),
    topic_dom_prop = apply(select(., starts_with("topic_")), 1, max)
  )

#rotulo dos nomes de cada topico
topic_labels <- c(
  "Direito",
  "Comportamento e Instituições",
  "Saúde e Meio Ambiente",
  "Administração e Gestão Pública",
  "História do Pensamento Político",
  "Ciências Sociais",
  "Educação",
  "Economia Política",
  "Teoria Democrática e Participação",
  "Relações Internacionais"
)

#rotulando
topics_df$topic_dom_label <- factor(
  topics_df$topic_dom_id,
  levels = seq_along(topic_labels),
  labels = topic_labels
)

#Juntando percentual de topicos as bases originais
Data <- Data %>%
  left_join(
    topics_df %>%
      select(Nome, all_of(topic_cols), topic_dom_id, topic_dom_prop, topic_dom_label),
    by = "Nome"
  )

#Conferindo resultado
Data %>%
  select(Nome, topic_dom_label, topic_dom_prop) %>%
  head()

#Calculando a media por topico e por ppg
topic_cols <- paste0("topic_", 1:10)

heat_df <- Data %>%
  select(Sigla, all_of(topic_cols)) %>%
  group_by(Sigla) %>%
  summarise(across(all_of(topic_cols), ~ mean(.x, na.rm = TRUE) * 100)) %>%
  pivot_longer(
    cols = all_of(topic_cols),
    names_to = "topico",
    values_to = "Percentual"
  ) %>%
  mutate(Tópico = factor(topic_labels[as.numeric(gsub("topic_", "", topico))],
                         levels = topic_labels))

heat_df_max <- heat_df %>%
  group_by(Sigla) %>%
  slice_max(order_by = Percentual, n = 1) %>%
  ungroup()

#Grafico de calor de topico (GRAFICO 8)
ggplot(heat_df, aes(x = Tópico, y = Sigla, fill = Percentual)) +
  geom_tile(color = "white") +
  geom_text(aes(label = number(Percentual, accuracy = 0.1, decimal.mark = ",")), size = 3) +
  scale_fill_gradient(low = "white", high = "darkred", name = "% média por tópico") +
  labs(
    title = "",
    x = "Tópico",
    y = "PPG"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave("heatmap_politica.png", width = 10, height = 12, dpi = 300)

#Analises de regressao

#Recodificando variaveis de estado e pais para rodar os modelos
Data <- Data %>%
  mutate(
    Regiao = case_when(
      Estado %in% c("Paraná", "Santa Catarina", "Rio Grande Do Sul") ~ "Sul",
      Estado %in% c("São Paulo", "Minas Gerais", "Rio De Janeiro", "Espírito Santo") ~ "Sudeste",
      Estado %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso Do Sul") ~ "Centro-Oeste",
      Estado %in% c("Bahia", "Sergipe", "Alagoas", "Pernambuco", "Paraíba",
                    "Rio Grande Do Norte", "Ceará", "Piauí", "Maranhão") ~ "Nordeste",
      Estado %in% c("Pará", "Amapá", "Roraima", "Amazonas", "Acre", "Rondônia", "Tocantins") ~ "Norte",
      TRUE ~ NA_character_
    )
  )

Data <- Data %>%
  mutate(
    Pais = case_when(
      Pais == "Brasil" ~ "Brasil",
      Pais == "Estados Unidos" ~ "Estados Unidos",
      Pais %in% c("Argentina", "Canadá", "Cuba", "Perú") ~ NA_character_,
      TRUE ~ "Europa"
    )
  )

#Colocando Ciência Política e região sudeste como categorias de referência
Data <- Data %>%
  mutate(
    Tipo = fct_relevel(as_factor(Tipo), "CIÊNCIA POLÍTICA"),
    Regiao = fct_relevel(as_factor(Regiao), "Sudeste")
  )

#Modelos por topicos (TABELA 1)
topic1_model <- lm(topic_1 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic1_model)

topic2_model <- lm(topic_2 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic2_model)

topic3_model <- lm(topic_3 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic3_model)

topic4_model <- lm(topic_4 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic4_model)

topic5_model <- lm(topic_5 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic5_model)

topic6_model <- lm(topic_6 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic6_model)

topic7_model <- lm(topic_7 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic7_model)

topic8_model <- lm(topic_8 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic8_model)

topic9_model <- lm(topic_9 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic9_model)

topic10_model <- lm(topic_10 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao,
                   data = Data)
summary(topic10_model)

###ANALISE DE IMPACTO

#Normalizando variaveis por tempo de atuacao no campo
Data$CitAno <- Data$Cit / (2025 - Data$Ano_Obtencao)
Data$Cit5Ano <- Data$Cit5 / (2025 - Data$Ano_Obtencao)
Data$i10Ano <- Data$i10 / (2025 - Data$Ano_Obtencao)
Data$i105Ano <- Data$i105 / (2025 - Data$Ano_Obtencao)

#Modelos de regressao Cit, h-index e i10-index (TABELA 2)
Cit_model <- lm(CitAno ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                  topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                  topic_8 + topic_9, data = Data)
summary(Cit_model)

Cit5_model <- lm(Cit5Ano ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                  topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                  topic_8 + topic_9, data = Data)
summary(Cit5_model)

h_model <- lm(h ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                   topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                   topic_8 + topic_9, data = Data)
summary(h_model)

h5_model <- lm(h5 ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                topic_8 + topic_9, data = Data)
summary(h5_model)

i10_model <- lm(i10Ano ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                 topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                 topic_8 + topic_9, data = Data)
summary(i10_model)

i105_model <- lm(i105Ano ~ Tipo + Regiao + Nota + Sexo + Pais + Sexo_O + Ano_Obtencao +
                  topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                  topic_8 + topic_9, data = Data)
summary(i105_model)

###ADENSANDO O IMPACTO DO SEXO

#Filtrando so os que tem o sexo (nesse nosso caso temos todos)
Data_sexo <- Data %>%
  filter(!is.na(Sexo))

#Variaveis que vao ser usadas
covars <- c("Tipo","Regiao","Nota","Pais","Sexo_O","Ano_Obtencao", paste0("topic_", 1:9))
vars   <- c("Sexo", covars)

#Verificando os NAs para nao ter problema com matching
sapply(Data_sexo[vars], function(x) sum(!is.finite(x)), simplify = TRUE) 
sapply(Data_sexo[vars], function(x) sum(is.na(x)), simplify = TRUE)

#Definindo as classes das variaveis para o matching
Data_sexo <- Data_sexo %>%
  mutate(
    Sexo = factor(Sexo), 
    Tipo = factor(Tipo),
    Regiao = factor(Regiao),
    Pais = factor(Pais),
    Sexo_O = factor(Sexo_O),
    Ano_Obtencao = suppressWarnings(as.numeric(Ano_Obtencao))
  )

#Agora removendo as linhas com NAs
Data_sexo <- Data_sexo %>%
  filter(!if_any(all_of(vars), ~ is.na(.))) %>%
  filter(if_all(all_of(c("Nota","Ano_Obtencao", paste0("topic_",1:9))),
                ~ is.finite(as.numeric(.))))

Data_sexo$Sexo

Data_sexo$Sexo_mat <- dplyr::recode(Data_sexo$Sexo,
                                    "Masculino" = 0,
                                    "Feminino"  = 1)

#Rodando o matching
matching <- matchit(
  Sexo_mat ~ Tipo + Regiao + Nota + Pais + Sexo_O + Ano_Obtencao + topic_dom_id,
  data = Data_sexo,
  method = "nearest", ratio = 1
)
summary(matching)

plot(matching, type = "jitter")

matching_data <- match.data(matching)

Cit_model_sexo <- lm(CitAno ~ Sexo, data = matching_data)
summary(Cit_model_sexo)

Cit5_model_sexo <- lm(Cit5Ano ~ Sexo, data = matching_data)
summary(Cit5_model_sexo)

h_model_sexo <- lm(h ~ Sexo, data = matching_data)
summary(h_model_sexo)

h5_model_sexo <- lm(h5 ~ Sexo, data = matching_data)
summary(h5_model_sexo)

i10_model_sexo <- lm(i10Ano ~ Sexo, data = matching_data)
summary(i10_model_sexo)

i105_model_sexo <- lm(i105 ~ Sexo, data = matching_data)
summary(i105_model_sexo)

#Grafico com resultados do matching (Grafico 9)

#Criando um tema fixo para o meu grafico
theme_lucas <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 11)
    )
}

#Paleta de cores do grafico
cor_ref <- "grey40"
cor_seg <- "grey75"
cor_pt1 <- "black"
cor_pt2 <- "grey30"

#Funcao para rodar o modelo e padronizar

fit_effect <- function(y, data, weights = NULL) {
  #padroniza a variavel dependente
  data <- data |> mutate(y_std = scale(.data[[y]]))
  #inclui a formula do modelo
  f <- as.formula("y_std ~ Sexo_mat")
  m <- if (is.null(weights)) lm(f, data = data) else lm(f, data = data, weights = weights)
  #extrai coeficiente e erro do termo
  b  <- coef(m)["Sexo_mat"]
  se <- sqrt(vcov(m)["Sexo_mat", "Sexo_mat"])
  tibble(outcome = y, estimate = as.numeric(b), se = as.numeric(se))
}

#Variaveis dependentes
outcomes <- c("CitAno", "Cit5Ano", "h", "h5", "i10Ano", "i105")

#Checagem para aviso caso trave por falta de variavel
missing_vars <- setdiff(outcomes, names(Data_sexo))
if (length(missing_vars) > 0) stop("Faltam variáveis: ", paste(missing_vars, collapse = ", "))

#garante Sexo_mat no matching_data (travou algumas vezes sem esse ajuste que rodei)
if (!"Sexo_mat" %in% names(matching_data)) {
  if ("Sexo" %in% names(matching_data)) {
    matching_data <- matching_data |>
      mutate(Sexo_mat = dplyr::recode(Sexo, "Masculino" = 0, "Feminino" = 1) |> as.numeric())
  } else stop("matching_data não tem 'Sexo_mat' nem 'Sexo'.")
}

#estimacoes com e sem matching
est_unmatched <- map_dfr(outcomes, ~ fit_effect(.x, data = Data_sexo)) |>
  mutate(method = "Sem matching")

est_matched <- map_dfr(outcomes, ~ fit_effect(.x, data = matching_data, weights = matching_data$weights)) |>
  mutate(method = "Com matching")

#junta o resultado e constroi os instervalos de confianca
est_all <- bind_rows(est_unmatched, est_matched) |>
  mutate(
    ci_low  = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    outcome = factor(
      outcome,
      levels = c("CitAno","Cit5Ano","i10Ano","i105","h","h5"),
      labels = c("Citações/ano",
                 "Citações últimos 5 anos/ano",
                 "i10/ano",
                 "i10 últimos 5 anos",
                 "h-index",
                 "h-index últimos 5 anos"))
  )

#ajusta o deslocamento visual dos pontos
delta <- 0.14
est_all <- est_all |>
  group_by(outcome) |>
  mutate(
    y_base = as.numeric(outcome),
    y_plot = ifelse(method == "Sem matching", y_base + delta, y_base - delta)
  ) |>
  ungroup()

#coloca o dumbbell
seg_df <- est_all |>
  select(outcome, method, estimate) |>
  pivot_wider(names_from = method, values_from = estimate) |>
  rename(est_sem = `Sem matching`, est_com = `Com matching`)

#ajuste visual do grafico
shape_values <- c("Sem matching" = 21, "Com matching" = 24)
fill_values  <- c("Sem matching" = cor_pt1, "Com matching" = cor_pt2)
color_values <- c("Sem matching" = cor_pt1, "Com matching" = cor_pt1)

#GRAFICO 9
p <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = cor_ref) +
  geom_segment(
    data = seg_df |> mutate(y_base = as.numeric(outcome)),
    aes(x = est_sem, xend = est_com, y = y_base, yend = y_base),
    color = cor_seg, linewidth = 1
  ) +
  geom_errorbarh(
    data = est_all,
    aes(y = y_plot, xmin = ci_low, xmax = ci_high),
    height = 0.12, color = "grey50", alpha = 0.6, linewidth = 0.6
  ) +
  geom_point(
    data = est_all,
    aes(x = estimate, y = y_plot, shape = method, fill = method, color = method),
    size = 2.8, stroke = 0.5
  ) +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = fill_values) +
  scale_color_manual(values = color_values) +
  scale_y_continuous(
    breaks = sort(unique(est_all$y_base)),
    labels = levels(est_all$outcome)
  ) +
  labs(
    x = "Efeito padronizado de ser mulher (em desvios-padrão)",
    y = NULL
  ) +
  theme_lucas()

print(p)

#salvando em PNG e SVG
out_dir <- "Graficos_Efeitos_Sexo"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "efeitos_sexo_dumbbell_padronizado.png"),
       plot = p, width = 8.5, height = 5.2, dpi = 300, bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  ggsave(file.path(out_dir, "efeitos_sexo_dumbbell_padronizado.svg"),
         plot = p, width = 8.5, height = 5.2, device = "svg", bg = "white")
}

#####APENDICE METODOLOGICO#####

###LOCALIZACAO

#Conta por estado
conta_por_estado_abs <- function(df) {
  df %>%
    group_by(Estado) %>%
    summarise(n_docentes_abs = n(), .groups = "drop")
}

#juntar com o mapa
join_com_mapa_abs <- function(tab_estado_abs) {
  estados %>%
    left_join(tab_estado_abs, by = c("name_state" = "Estado")) %>%
    mutate(n_docentes_abs = ifelse(is.na(n_docentes_abs), 0L, n_docentes_abs))
}

#plotar o mapa
plot_mapa_abs <- function(tab_estado_abs, titulo_abs) {
  dados_plot_abs <- join_com_mapa_abs(tab_estado_abs)
  ggplot(dados_plot_abs) +
    geom_sf(aes(fill = n_docentes_abs), color = "grey60", size = 0.3) +
    scale_fill_gradient(
      name = "Docentes (absoluto)",
      low = "grey90",
      high = "black",
      na.value = "white"
    ) +
    coord_sf() +
    theme_minimal(base_size = 11) +
    labs(title = titulo_abs) +
    theme(
      legend.position = "right",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

#Contagem por estado
doc_total_abs <- conta_por_estado_abs(Data)
doc_CP_abs    <- conta_por_estado_abs(Data_CP)
doc_PP_abs    <- conta_por_estado_abs(Data_PP)
doc_ADM_abs   <- conta_por_estado_abs(Data_ADM)
doc_S_abs     <- conta_por_estado_abs(Data_S)

#Plotando os mapas absolutos e por estado
g_brasil_abs <- plot_mapa_abs(doc_total_abs, "")

g_cp_abs  <- plot_mapa_abs(doc_CP_abs,  "PPGs de Ciência Política")
g_pp_abs  <- plot_mapa_abs(doc_PP_abs,  "PPGs de Políticas Públicas")
g_adm_abs <- plot_mapa_abs(doc_ADM_abs, "PPGs de Administração Pública")
g_soc_abs <- plot_mapa_abs(doc_S_abs,   "PPGs de Ciências Sociais")

#Grid 2x2 do mapa por tipo de programa
grid_2x2_abs <- (g_cp_abs + g_pp_abs) / (g_adm_abs + g_soc_abs)

#Salvando como imagem
ggsave("mapa_brasil_todos_absoluto.png", plot = g_brasil_abs, width = 8, height = 6, dpi = 300)
ggsave("mapas_2x2_categorias_absoluto.png", plot = grid_2x2_abs, width = 14, height = 10, dpi = 300)


###SEXO POR PROGRAMA

#Agregando por programa
df_prog <- Data %>%
  filter(!is.na(Sigla),
         !is.na(Sexo),
         Sexo %in% c("Feminino","Masculino")) %>%
  group_by(Sigla) %>%
  summarise(
    n        = n(),
    n_fem    = sum(Sexo == "Feminino"),
    n_masc   = sum(Sexo == "Masculino"),
    pct_fem  = n_fem / n,
    pct_masc = n_masc / n,
    .groups = "drop"
  )

#Ordenando para o grafico do menor para o maior)
df_lolli <- df_prog %>%
  filter(n >= 1) %>%
  arrange(pct_fem) %>%
  mutate(Sigla = factor(Sigla, levels = Sigla))

#Grafico percentual de mulheres por prograMA
ggplot(df_lolli, aes(x = pct_fem, y = Sigla)) +
  geom_segment(aes(x = 0, xend = pct_fem, y = Sigla, yend = Sigla)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Percentual de mulheres",
    y = NULL
  ) +
  theme_minimal(base_size = 13)

#Salvando
ggsave("percentual_mulheres_programas.png",
       width = 8,   
       height = 10,
       dpi = 300)

###REDES PPGs

#Minimo de arestas e numero maximo de nos (isso aqui nao afeta pq ta alto, mas e pra poder mudar se quiser mudar criterio)
edge_min_default <- 0.02
top_n_labels_default <- 115
set.seed(123)

#Nome dos topicos/areas de atuacao
topic_labels <- c(
  "Direito",
  "Comportamento e Instituições",
  "Saúde e Meio Ambiente",
  "Administração e Gestão Pública",
  "História do Pensamento Político",
  "Ciências Sociais",
  "Educação",
  "Economia Política",
  "Teoria Democrática e Participação",
  "Relações Internacionais"
)

#salvar tudo em uma mesma pasta no meu diretorio
out_dir <- "Graficos_Redes_PPGs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#Help que adicionei para salvar em SVG
#Duas formas de salvar em SVG porque tava travando por algum motivo
#Se uma nao for ele faz a outra
save_svg_safe <- function(filename, plot, width = 19, height = 10) {
  if (requireNamespace("svglite", quietly = TRUE)) {
    ok <- try({
      ggplot2::ggsave(filename = filename,
                      plot = plot, width = width, height = height,
                      device = svglite::svglite)
    }, silent = TRUE)
    if (!inherits(ok, "try-error")) return(TRUE)
    message("Aviso: falhou salvar via svglite -> tentando dispositivo base 'svg()'.")
  }
  ok2 <- try({
    ggplot2::ggsave(filename = filename,
                    plot = plot, width = width, height = height,
                    device = "svg")
  }, silent = TRUE)
  if (!inherits(ok2, "try-error")) return(TRUE)
  message("ERRO ao salvar SVG (tente instalar o pacote 'svglite'): ", filename)
  return(FALSE)
}

#Funcao principal que gera rede por sigla
plot_rede_por_sigla <- function(df,
                                sigla,
                                edge_min         = edge_min_default,
                                top_n_labels     = top_n_labels_default,
                                topic_labels_vec = topic_labels) {
  
  #Cria subsets por sigla
  df_sigla <- df %>% dplyr::filter(Sigla == sigla)
  if (nrow(df_sigla) == 0) stop("Sem linhas para sigla: ", sigla)
  
  #Cria uma coluna por topico
  topic_cols <- names(df_sigla) %>% grep("^topic_\\d+$", ., value = TRUE)
  if (length(topic_cols) == 0) stop("Sem colunas topic_* para sigla: ", sigla)
  
  K <- length(topic_cols)
  
  #garante rotulos suficientes pros topicos
  if (length(topic_labels_vec) < K) {
    faltam <- K - length(topic_labels_vec)
    topic_labels_vec <- c(
      topic_labels_vec,
      paste0("Tópico ", (length(topic_labels_vec) + 1):(length(topic_labels_vec) + faltam))
    )
  }
  names(topic_labels_vec) <- paste0("topic_", seq_len(K))
  
  #mantem nome e arestas de acordo com aquele criterio que eu criei antes, ai so mudar se for mudar o criterio, o meu aqui coloquei alto pra pegar todos
  edges_long <- df_sigla %>%
    dplyr::select(Nome, dplyr::all_of(topic_cols)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(topic_cols),
      names_to = "topic",
      values_to = "weight"
    ) %>%
    dplyr::mutate(
      topic_id = as.integer(stringr::str_extract(topic, "\\d+")),
      to_node  = paste0("topic_", topic_id)
    ) %>%
    dplyr::filter(!is.na(weight), weight >= edge_min)
  
  if (nrow(edges_long) == 0) stop("Sem arestas >= edge_min para sigla: ", sigla)
  
  #Tamanho dos nos considerando quantas vezes aparece em topic_dom_id que criei la em cima
  has_dom <- "topic_dom_id" %in% names(df_sigla)
  if (has_dom) {
    topic_sizes <- tibble(topic_id = seq_len(K)) %>%
      dplyr::left_join(
        df_sigla %>%
          dplyr::transmute(topic_id = as.integer(.data$topic_dom_id)) %>%
          dplyr::filter(!is.na(topic_id)) %>%
          dplyr::count(topic_id, name = "dom_count"),
        by = "topic_id"
      ) %>%
      dplyr::mutate(
        dom_count = dplyr::if_else(is.na(dom_count), 0L, dom_count),
        name = paste0("topic_", topic_id)
      ) %>%
      dplyr::select(name, dom_count)
  } else {
    topic_sizes <- tibble(
      name = paste0("topic_", seq_len(K)),
      dom_count = 0L
    )
  }
  
  #Constroi agora tibbles de nos
  
  
  name_nodes <- df_sigla %>%
    dplyr::distinct(Nome) %>%
    dplyr::transmute(name = Nome)
  
  nodes <- dplyr::bind_rows(
    name_nodes %>% dplyr::mutate(group = "Nome"),
    topic_sizes %>% dplyr::mutate(group = "Tópico")
  ) %>%
    dplyr::mutate(
      node_size = dplyr::if_else(group == "Tópico", as.numeric(dom_count), 1),
      type      = group == "Tópico"
    ) %>%
    dplyr::select(name, group, node_size, type)
  
  edges <- edges_long %>%
    dplyr::transmute(from = Nome, to = to_node, weight)
  
  #Agora converte pro grafico com os pesos
  g_tbl <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  g_ig  <- igraph::as.igraph(g_tbl)
  
  node_strength <- igraph::strength(
    g_ig,
    vids    = igraph::V(g_ig),
    weights = igraph::E(g_ig)$weight
  )
  
  #Aqui define a saliencia do no, ou seja, a popularidade do topico ali no programa
  g_tbl <- g_tbl %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(
      salience = dplyr::if_else(group == "Nome", pmax(node_strength, 0), pmax(node_size, 0))
    )
  
  #Seleciona top nos para rotulagem, aqui todos, mas pode ser mudado depois nessa parte
  node_df <- g_tbl %>% tidygraph::activate(nodes) %>% tibble::as_tibble()
  top_names <- node_df %>%
    dplyr::arrange(dplyr::desc(salience)) %>%
    dplyr::slice_head(n = min(top_n_labels, nrow(node_df))) %>%
    dplyr::pull(name)
  
  g_tbl <- g_tbl %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(show_label = name %in% top_names)
  
  #Topicos legiveis com base no nome
  topic_map <- topic_labels_vec
  g_tbl <- g_tbl %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(node_label = dplyr::recode(name, !!!topic_map, .default = name))
  
  #Agora finalmente o grafico
  p <- ggraph::ggraph(g_tbl, layout = "fr", weights = weight) +
    ggraph::geom_edge_link(
      ggplot2::aes(width = weight, colour = weight, alpha = weight),
      show.legend = TRUE
    ) +
    ggraph::scale_edge_width(name = "Peso da aresta", range = c(0.1, 2.8)) +
    ggraph::scale_edge_colour_gradient(name = "Peso da aresta", low = "grey85", high = "black") +
    ggraph::scale_edge_alpha(range = c(0.15, 0.8), guide = "none") +
    ggraph::geom_node_point(ggplot2::aes(size = node_size, color = salience)) +
    ggplot2::scale_color_gradient(name = "Salience", low = "grey80", high = "black") +
    ggraph::geom_node_text(
      ggplot2::aes(label = ifelse(show_label, node_label, "")),
      repel = TRUE, max.overlaps = Inf, size = 3
    ) +
    ggplot2::scale_size_continuous(name = "Tamanho do nó") +
    ggplot2::labs(
      title    = paste0("Rede Nome × Tópico — ", sigla),
      subtitle = paste0("Arestas ponderadas por participação temática (edge_min = ", edge_min, ")")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid = ggplot2::element_blank()
    )
  
  #Depois salvar em PNG e SVG, se o computador nao for bom pode travar no SVG,
  #Ai melhor apagar e rodar so PNG
  sigla_sanit <- sigla %>%
    stringr::str_replace_all("[^A-Za-z0-9_-]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
  
  out_png <- file.path(out_dir, paste0("rede_nome_topico_forca_", sigla_sanit, ".png"))
  out_svg <- file.path(out_dir, paste0("rede_nome_topico_forca_", sigla_sanit, ".svg"))
  
  #PNG
  ggplot2::ggsave(filename = out_png, plot = p, width = 19, height = 10, dpi = 300)
  
  #SVG, coloquei pra avisar se falhar (porque ja falou antes)
  svg_ok <- save_svg_safe(filename = out_svg, plot = p, width = 19, height = 10)
  
  if (isTRUE(svg_ok)) {
    message("OK: ", sigla, " -> ", out_png, " | ", out_svg)
  } else {
    message("OK (PNG) / FALHA (SVG): ", sigla, " -> ", out_png)
  }
  
  invisible(list(sigla = sigla, ok_png = TRUE, ok_svg = isTRUE(svg_ok),
                 files = c(png = out_png, svg = if (svg_ok) out_svg else NA)))
}

#Loop pra rodar em todas as siglas
siglas_unicas <- Data %>%
  dplyr::filter(!is.na(Sigla)) %>%
  dplyr::distinct(Sigla) %>%
  dplyr::arrange(Sigla) %>%
  dplyr::pull(Sigla)

log_result <- lapply(siglas_unicas, function(s) {
  tryCatch(
    plot_rede_por_sigla(Data, s),
    error = function(e) {
      message("ERRO: ", s, " -> ", conditionMessage(e))
      return(list(sigla = s, ok_png = FALSE, ok_svg = FALSE, error = conditionMessage(e)))
    }
  )
})

#Resumo do processamento pra informar o pesquisador
log_df <- do.call(rbind, lapply(log_result, function(x) {
  if (is.null(x)) return(data.frame(sigla = NA, ok_png = FALSE, ok_svg = FALSE, error = "retornou NULL"))
  data.frame(
    sigla  = x$sigla,
    ok_png = isTRUE(x$ok_png),
    ok_svg = isTRUE(x$ok_svg),
    error  = ifelse(isTRUE(x$ok_png) || isTRUE(x$ok_svg), NA, x$error)
  )
}))
print(log_df)

###FIGURAS DE MAIOR IMPACTO VIOLINOS

#Criando variavel de geracao
Data <- Data %>%
  mutate(
    Geracao = case_when(
      Ano_Obtencao <= 1990 ~ "Até 1990",
      Ano_Obtencao >= 1991 & Ano_Obtencao <= 2000 ~ "1991-2000",
      Ano_Obtencao >= 2001 & Ano_Obtencao <= 2010 ~ "2001-2010",
      Ano_Obtencao >= 2011 & Ano_Obtencao <= 2020 ~ "2011-2020",
      Ano_Obtencao >= 2021 & Ano_Obtencao <= 2025 ~ "2021-2025",
      TRUE ~ NA_character_
    )
  )

Data$Geracao <- factor(
  Data$Geracao,
  levels = c("Até 1990", "1991-2000", "2001-2010", "2011-2020", "2021-2025")
)

#Diretorio de saida para as figuras
root_dir <- "Violinos"
if (!dir.exists(root_dir)) dir.create(root_dir, recursive = TRUE)

out_root <- file.path(root_dir, "UNIFICADOS")
if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)

#Mantendo ordem de geracao fixa para nao ficar trocado por peso no grafico
Data$Geracao <- factor(
  Data$Geracao,
  levels = c("Até 1990", "1991-2000", "2001-2010", "2011-2020", "2021-2025")
)

#Variaveis dependentes e estrados que vao dividir os violinos
y_vars   <- c("CitAno", "Cit5Ano", "h", "h5", "i10Ano", "i105Ano")
groupers <- c("Regiao", "Geracao", "Pais", "topic_dom_label")
sexes    <- c("Masculino", "Feminino")

#Numero de categorias para eu medir o tamanho do grafico depois
n_cats_expected <- c(
  Regiao = 5L,
  Geracao = 5L,
  Pais = 3L,
  topic_dom_label = 10L
)

#Colocando os graficos juntos com 3 em cima e o maior embaixo
per_cat_width   <- 2.4
default_height  <- 6

top_total    <- sum(unname(n_cats_expected[c("Regiao", "Geracao", "Pais")])) 
bottom_total <- unname(n_cats_expected["topic_dom_label"])                  

unified_width  <- per_cat_width * max(top_total, bottom_total)  

#Aqui coloquei isso pra dobrar a altura pq tava muito achatado
height_scale   <- 1.5
unified_height <- (default_height * 1.6) * height_scale         

#Rotulo dos eixos
y_lab_pretty <- function(y_var) dplyr::case_when(
  y_var == "CitAno"  ~ "Citações por ano",
  y_var == "Cit5Ano" ~ "Citações/ano (últimos 5 anos)",
  y_var == "h"       ~ "Índice h",
  y_var == "h5"      ~ "Índice h (5 anos)",
  y_var == "i10Ano"  ~ "i10 por ano",
  y_var == "i105Ano" ~ "i10/ano (últimos 5 anos)",
  TRUE ~ y_var
)

x_lab_pretty <- function(group_var) dplyr::case_when(
  group_var == "Regiao"          ~ "Região",
  group_var == "Geracao"         ~ "Geração",
  group_var == "Pais"            ~ "País do doutorado",
  group_var == "topic_dom_label" ~ "Tópico dominante",
  TRUE ~ group_var
)

#Denindo numeracao dos graficos
base_num <- c(CitAno = 15L, Cit5Ano = 17L, h = 19L, h5 = 21L, i10Ano = 23L, i105Ano = 25L)
graph_number <- function(y_var, sex) {
  base <- base_num[[y_var]]
  if (isTRUE(sex == "Feminino")) base + 1L else base
}

#Funcao para construir um painel por estrato de sexo
make_violin_panel <- function(df, y_var, group_var, y_limits) {
  stopifnot(all(c("Nome", "Sexo", y_var, group_var) %in% names(df)))
  
  df_plot <- df %>%
    dplyr::select(Nome, Sexo, !!rlang::sym(y_var), !!rlang::sym(group_var)) %>%
    dplyr::rename(y = !!rlang::sym(y_var), group = !!rlang::sym(group_var)) %>%
    dplyr::filter(!is.na(y), !is.na(group), y >= 0)
  
  #Aqui eu ordeno os grupos, se for Geracao, usa a ordem fixa definida no fator, porque fica estranho mudar
  #caso contrário, ordena por mediana do y - sempre a esquerda vai mostrar o grupo com maior mediana
  if (group_var == "Geracao") {
    df_plot <- df_plot %>%
      dplyr::mutate(group = factor(group, levels = levels(Data$Geracao)))
  } else {
    df_plot <- df_plot %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(med = median(y, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(group = forcats::fct_reorder(as.factor(group), med, .desc = TRUE))
  }
  
  #Aqui seleciona o top 5 pra aparecerer o nome por grupo
  set.seed(123)
  df_top5 <- df_plot %>%
    dplyr::group_by(group) %>%
    dplyr::slice_max(order_by = y, n = 5, with_ties = FALSE) %>%
    dplyr::ungroup()
  
  #Depois de tudo isso, finalmente o grafico
  p <- ggplot(df_plot, aes(x = group, y = y)) +
    geom_violin(aes(group = group), scale = "width", trim = TRUE, alpha = 0.7) +
    geom_jitter(width = 0.08, height = 0, alpha = 0.18, size = 1) +
    geom_text_repel(
      data = df_top5,
      aes(label = Nome),
      size = 3,
      min.segment.length = 0,
      max.overlaps = Inf,
      box.padding = 0.5,
      point.padding = 0.2,
      seed = 123
    ) +
    scale_y_continuous(
      trans = "log1p",
      limits = y_limits,
      labels = number_format(accuracy = 1, big.mark = ".", decimal.mark = ",")
    ) +
    labs(x = x_lab_pretty(group_var), y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.text.x  = element_text(angle = 20, hjust = 1, vjust = 1),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank()
    )
  
  if (group_var == "topic_dom_label") {
    p <- p + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  }
  
  return(p)
}

#Agora depois de tudo configurando o loop para rodar cada VD X SEXO X ESTRATO
walk(sexes, function(sex_val) {
  #subconjunto por sexo
  df_sex <- Data %>% filter(Sexo == sex_val)
  if (nrow(df_sex) == 0) {
    message("Sem casos para Sexo = ", sex_val, ". Pulando.")
    return(invisible(NULL))
  }
  
  #isso aqui coloca as saidas numa pasta especifica
  out_dir_sex <- file.path(out_root, sex_val)
  if (!dir.exists(out_dir_sex)) dir.create(out_dir_sex, recursive = TRUE)
  
  walk(y_vars, function(yv) {
    yy <- df_sex[[yv]]
    yy <- yy[is.finite(yy) & !is.na(yy) & yy >= 0]
    if (length(yy) == 0) {
      message("Sem valores em ", yv, " para ", sex_val, ". Pulando.")
      return(invisible(NULL))
    }
    y_limits <- c(0, max(yy, na.rm = TRUE))
    
    #monta os 4 paineis pra sairem juntos
    panels <- map(groupers, ~ make_violin_panel(df_sex, yv, .x, y_limits))
    
    #especificamente 3 em cima e o ultimo embaixo ocupando essa configuracao
    design_mat <- "
    ABC
    DDD
    "
    
    #A largura que nem coloquei ali em cima
    top_widths  <- as.numeric(n_cats_expected[groupers[1:3]])                 
    row_heights <- c(sum(top_widths), as.numeric(n_cats_expected[groupers[4]]))
    
    #ai fica assim unificado
    unified <- panels[[1]] + panels[[2]] + panels[[3]] + panels[[4]] +
      plot_layout(
        design  = design_mat,
        widths  = top_widths,
        heights = row_heights,
        guides  = "collect"
      )
    
    unified <- unified & labs(y = paste0(y_lab_pretty(yv), " (escala log)"))
    
    #Numera o grafico e nomeia o arquivo
    gnum <- graph_number(yv, sex_val)
    base_fname <- sprintf("G%02d_%s_%s_unificado", gnum, yv, sex_val)
    
    #Ai exporta pra PNG
    ggsave(
      filename = file.path(out_dir_sex, paste0(base_fname, ".png")),
      plot = unified, width = unified_width, height = unified_height, dpi = 300,
      limitsize = FALSE
    )
    
    #PDF
    ggsave(
      filename = file.path(out_dir_sex, paste0(base_fname, ".pdf")),
      plot = unified, width = unified_width, height = unified_height, device = cairo_pdf,
      limitsize = FALSE
    )
    
    #E SVG, se seu computado travar mt melhor deixar so PNG
    ggsave(
      filename = file.path(out_dir_sex, paste0(base_fname, ".svg")),
      plot = unified, width = unified_width, height = unified_height, device = "svg",
      limitsize = FALSE
    )
    
    message("Salvo: ",
            file.path(out_dir_sex, paste0(base_fname, ".png")),
            " | ",
            file.path(out_dir_sex, paste0(base_fname, ".pdf")),
            " | ",
            file.path(out_dir_sex, paste0(base_fname, ".svg")))
  })
})

###ONDE AS MULHERES SAO PREJUDICADAS?

#Extrai a vase adicionando uma variavel se foi ou nao pareado
md_all <- match.data(matching, drop.unmatched = FALSE) %>%
  mutate(
    matched = as.integer(weights > 0),   
    matched_fct = factor(matched, labels = c("Unmatched","Matched"))
  )

table(md_all$matched, md_all$Sexo)

#Aqui eu inverti pq fico 1 pareado e 0 nao pareado, mas eu quero os nao pareados entao inverti
md_all$matched <- ifelse(md_all$matched == 0,1,0)

#Modelo dos determinantes de nao ser pareado
Privilegio <- lm(matched ~ Tipo + Regiao + Nota + Pais + Sexo_O + Ano_Obtencao +
                  topic_1 + topic_2 + topic_3 + topic_4 + topic_5 + topic_6 + topic_7 +
                  topic_8 + topic_9, data = md_all)
summary(Privilegio)
