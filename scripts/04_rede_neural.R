# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023: Rede Neural para Predição de HAS (foco didático)
# Rode 00_config.R antes (ele cria dados, dados_treino e dados_teste)
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_config.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────
# dados, dados_treino e dados_teste preparados pelo 00_config.R

# ── 2. Recipe: pré-processamento ──────────────────────────────────────────────
# PASSO 1: step_dummy()
#   Converte fatores em variáveis indicadoras (0/1).
#   Ex.: sexo vira "sexo_Feminino" (0 = Masculino, 1 = Feminino):
#   o 1º nível do fator ("Masculino") vira a referência e não gera dummy.
#   Com one_hot = FALSE usamos k-1 dummies (evita multicolinearidade perfeita).
#
# PASSO 2: step_normalize()
#   Centraliza (média 0) e escala (dp 1) as variáveis numéricas.
#   ESSENCIAL para redes neurais: sem isso variáveis em escalas grandes
#   (ex.: colesterol ~200) dominam os pesos sobre variáveis pequenas (ex.: whr ~0,5).

receita <- recipe(has_pas ~ ., data = dados_treino) %>%
  step_relevel(diabetes, ref_level = "Não") %>%
  step_relevel(fumante,  ref_level = "Nunca fumou") %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# ── 3. Especificação do modelo ────────────────────────────────────────────────
# mlp = Multi-Layer Perceptron: a arquitetura mais simples de rede neural.
#
# Arquitetura adotada (didática):
#   Entrada → [camada oculta: 4 neurônios] → Saída (P(HAS = "Sim"))
#
# hidden_units : neurônios na camada oculta
#                (4 é suficiente aqui; em produção, tunaria via cross-validation)
# penalty      : regularização L2 (weight decay): penaliza pesos grandes,
#                reduzindo overfitting
# epochs       : quantas vezes o algoritmo percorre todo o treino

espec_nn <- mlp(
  hidden_units = 4,
  penalty      = 0.01,
  epochs       = 300
) %>%
  set_engine("nnet") %>%
  set_mode("classification")

# ── 4. Workflow: une receita + modelo ─────────────────────────────────────────
# O workflow garante que a MESMA receita usada no treino seja aplicada
# automaticamente ao fazer predict(): sem risco de vazamento de dados.

wf_nn <- workflow() %>%
  add_recipe(receita) %>%
  add_model(espec_nn)

# ── 5. Treinar ────────────────────────────────────────────────────────────────
set.seed(42)
modelo_nn <- wf_nn %>% fit(data = dados_treino)

# ── 6. Avaliar no conjunto de teste ───────────────────────────────────────────
# metrics() retorna acurácia e kappa: úteis como primeira olhada, mas a
# acurácia é enganosa com classes desbalanceadas (ver ROC na seção 11).
predict(modelo_nn, dados_teste) %>%
  bind_cols(dados_teste %>% select(has_pas)) %>%
  metrics(truth = has_pas, estimate = .pred_class)

# ── 7. Visualização pedagógica da rede ────────────────────────────────────────
# extract_fit_engine() extrai o objeto nativo do nnet, que o plotnet entende.

nn_nativo <- extract_fit_engine(modelo_nn)

# Extrair os nomes reais das variáveis após o pré-processamento da receita.
# (step_dummy cria sufixos como "sexo_Feminino"; queremos rótulos legíveis)
nomes_entrada <- extract_mold(modelo_nn)$predictors |> names()

# Mapeamento técnico → PT-BR
# Inclui tanto os labels em inglês (nhanesA original) quanto PT-BR (após fct_recode),
# pois o resultado depende de quando setup.R foi rodado na sessão atual.
legenda <- c(
  # ── contínuas ───────────────────────────────
  "idade"                  = "Idade",
  "whr"                    = "Razão cintura/estatura",
  "renda"                  = "Renda",
  "colesterol"             = "Colesterol",
  # ── sexo (PT-BR / inglês) ───────────────────
  "sexo_Feminino"          = "Sexo\n(feminino)",
  "sexo_Masculino"         = "Sexo\n(masculino)",
  "sexo_Female"            = "Sexo\n(feminino)",
  "sexo_Male"              = "Sexo\n(masculino)",
  # ── HAS pela PAS ────────────────────────────
  "has_pas_Sim"            = "Hipertensão\n(sim)",
  "has_pas_Não"            = "Hipertensão\n(não)",
  # ── diabetes ────────────────────────────────
  "diabetes_Sim"           = "Diabetes\n(sim)",
  "diabetes_Não"           = "Diabetes\n(não)",
  # ── fumante (SMQ020+SMQ040: 3 categorias, ref = Nunca fumou) ──────
  "fumante_Ex.fumante"     = "Ex-fumante",
  "fumante_Fumante.atual"  = "Fumante\n(atual)",
  # ── raça/etnia ──────────────────────────────
  "raca_Branco"            = "Raça\n(branco)",
  "raca_Demais"            = "Raça\n(demais)",
  # ── sedentarismo ────────────────────────────
  "sedentarismo"           = "Sedentarismo\n(min/dia)"
)

# Aplica o mapeamento; mantém o nome original como fallback se não encontrado
rotulos_entrada <- legenda[nomes_entrada]
rotulos_entrada[is.na(rotulos_entrada)] <- nomes_entrada[is.na(rotulos_entrada)]

# O que cada elemento do gráfico representa:
#
#   NÓDULOS (círculos)
#   ├─ Coluna da esquerda (I1, I2…): neurônios de ENTRADA
#   │    Cada um corresponde a um preditor após a receita (dummy + normalização)
#   │    O nó "B1" é o bias da camada oculta (análogo ao intercepto da regressão)
#   ├─ Coluna do centro (H1…H4): neurônios da CAMADA OCULTA
#   │    Combinam as entradas via função de ativação (sigmoide/logística)
#   │    O nó "B2" é o bias da camada de saída
#   └─ Coluna da direita (O1): neurônio de SAÍDA
#        Emite P(HAS = "Sim") entre 0 e 1
#
#   CONEXÕES (linhas)
#   ├─ Azul   : peso POSITIVO → ativa / aumenta a probabilidade de saída
#   ├─ Vermelho: peso NEGATIVO → inibe / reduz a probabilidade de saída
#   └─ Espessura: proporcional à MAGNITUDE do peso (quanto mais grossa, mais influente)

p_net <- as.ggplot(function() {
  plotnet(
    nn_nativo,
    x_names    = rotulos_entrada,  # rótulos PT-BR para os nós de entrada
    y_names    = "Hipertensão",        # rótulo PT-BR para o nó de saída
    circle_cex = 5,                # tamanho dos nós
    cex_val    = 0.85,             # tamanho do texto dos rótulos
    bord_col   = "black",          # borda dos nós
    nid        = TRUE,             # NID: habilita cores e espessura por peso
                                   # (FALSE desabilitaria cores E espessura: tudo cinza)
    rel_rsc    = c(1, 6),          # [espessura mínima, máxima] das conexões
    var_labs   = TRUE,             # exibir nomes dos preditores
    pos_col    = "#2166ac",        # azul para pesos positivos
    neg_col    = "#d73027",        # vermelho para pesos negativos
    alpha_val  = 0.8               # transparência das linhas
  )
  title(
    main    = "",
    sub     = "Azul = peso positivo | Vermelho = peso negativo | Espessura = magnitude do peso",
    cex.sub = 0.8,
    col.sub = "gray40"
  )
})
ggsave("output/04_rede_neural/mlp_estrutura.png", plot = p_net,
       width = 12, height = 7, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# DICAS DE LEITURA DO GRÁFICO
# ──────────────────────────────────────────────────────────────────────────────
# 1. Rastreie caminhos: um preditor com linhas grossas em AZUL chegando
#    a neurônios ocultos que também têm linhas grossas em AZUL para a saída
#    provavelmente contribui POSITIVAMENTE para o risco de HAS.
#
# 2. Pesos grandes (linhas grossas) indicam que aquela conexão "importa mais"
#    para o modelo: mas, ao contrário de coeficientes de regressão,
#    não têm interpretação direta de direção na camada oculta.
#
# 3. Para interpretar direção dos efeitos, prefira SHAP values ou PDP plots
#    (disponíveis via {DALEXtra} ou {pdp}): o gráfico da rede mostra
#    ESTRUTURA, não causalidade.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# 8. FUNÇÃO DE ATIVAÇÃO: SIGMOIDE
# ══════════════════════════════════════════════════════════════════════════════
# Cada neurônio oculto aplica esta função à sua soma ponderada de entradas.
# Ela "comprime" qualquer valor real para (0, 1), como um dimmer:
# entradas muito negativas → saída ≈ 0 (neurônio inibido)
# entradas muito positivas → saída ≈ 1 (neurônio ativado)

x_vals   <- seq(-6, 6, by = 0.05)
sig_vals <- 1 / (1 + exp(-x_vals))

png("output/04_rede_neural/conceito_sigmoide.png", width = 900, height = 700, res = 120)
plot(
  x_vals, sig_vals,
  type = "l", lwd  = 2.5, col  = "#2166ac",
  xlab = "Entrada líquida  z  (soma ponderada das conexões)",
  ylab = "Saída do neurônio  \u03c3(z)",
  main = "Função de Ativação: Sigmoide (Logística)",
  las  = 1, bty  = "l", ylim = c(-0.05, 1.1)
)
abline(h = c(0, 0.5, 1), lty = 2, col = "gray70")
abline(v = 0,             lty = 2, col = "gray70")

# Ponto de inflexão: z = 0 → σ = 0,5
points(0, 0.5, pch = 19, col = "#d73027", cex = 1.8)
text(0.3, 0.50, "z = 0  \u2192  \u03c3 = 0,5\n(limiar de decisão)", adj = 0, cex = 0.8)

# Anotações de saturação
text(-5.2, 0.08, "inibido\n(\u03c3 \u2248 0)", cex = 0.8, col = "gray40")
text( 4.5, 0.93, "ativado\n(\u03c3 \u2248 1)", cex = 0.8, col = "gray40")

# Fórmula
mtext(expression(sigma(z) == frac(1, 1 + e^{-z})),
      side = 3, line = 0.2, cex = 0.9, col = "#2166ac")
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# 9. IMPORTÂNCIA DAS VARIÁVEIS: MÉTODO OLDEN
# ══════════════════════════════════════════════════════════════════════════════
# olden() percorre todos os caminhos da rede (entrada → oculta → saída)
# multiplicando os pesos em cadeia e somando as contribuições por variável.
# Preserva o SINAL: diferente de Garson(), que usa valores absolutos:
#   • Barra positiva : variável aumenta a probabilidade de HAS
#   • Barra negativa : variável reduz a probabilidade de HAS
#
# RESSALVA: o Olden lê a ESTRUTURA interna (pesos). Com preditores
# correlacionados, a rede "divide/compensa" pesos entre eles e o sinal pode
# sair errado (ex.: aqui o sedentarismo aparece "protetor", o que não se
# sustenta epidemiologicamente). Para importância confiável, ver permutação
# na seção 10.

# Rótulos sem "\n" para caber melhor no eixo horizontal do gráfico de barras
rotulos_olden <- unname(gsub("\n", " ", rotulos_entrada))

# olden(bar_plot = TRUE) usa scale_fill_gradient2 com mid = "white":
# valores de importância próximos de zero ficam brancos e somem no fundo branco.
# Solução: extrair os valores brutos e montar o ggplot com cores explícitas.
imp_raw <- olden(nn_nativo, bar_plot = FALSE)   # retorna matriz: linhas = preditores
imp_df  <- data.frame(
  variavel    = rotulos_olden,
  importancia = as.numeric(imp_raw[, 1])        # 1 coluna para classificação binária
)

p_olden <- ggplot(imp_df,
                  aes(x     = reorder(variavel, importancia),
                      y     = importancia,
                      fill  = importancia > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "gray40") +
  scale_fill_manual(values = c("TRUE" = "#2166ac", "FALSE" = "#d73027")) +
  coord_flip() +
  labs(
    title   = "Importância das Variáveis",
    x       = NULL,
    y       = "Importância relativa"
    ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_olden)
ggsave("output/04_rede_neural/mlp_importancia.png", plot = p_olden, width = 8, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 10. IMPORTÂNCIA POR PERMUTAÇÃO: MÉTODO AGNÓSTICO DE MODELO
# ══════════════════════════════════════════════════════════════════════════════
# Diferente do Olden (que lê os pesos internos), este é um EXPERIMENTO:
# embaralhamos UMA variável por vez nos dados de teste e medimos quanto o
# AUC cai. Queda grande = o modelo depende dela.
# Por ser agnóstica de modelo, é a única métrica de importância comparável
# ENTRE os scripts 02-04 (Gini ≠ ganho ≠ Olden).
# NOTA: a permutação atua nas variáveis BRUTAS (sexo, raca...), não nas
# dummies da receita: por isso o mapa de rótulos aqui usa os nomes brutos.

pred_prob <- function(object, newdata) {
  predict(object, newdata, type = "prob")$.pred_Sim
}

auc_evento_sim <- function(truth, estimate) {
  roc_auc_vec(truth, estimate, event_level = "second")
}

rotulos_perm <- c(
  "idade"        = "Idade",
  "whr"          = "Razão cintura/estatura",
  "renda"        = "Renda",
  "colesterol"   = "Colesterol",
  "sedentarismo" = "Sedentarismo (min/dia)",
  "sexo"         = "Sexo",
  "raca"         = "Raça/Etnia",
  "diabetes"     = "Diabetes",
  "fumante"      = "Tabagismo"
)

set.seed(42)
imp_perm <- vip::vi_permute(
  modelo_nn,
  train        = dados_teste,
  target       = "has_pas",
  metric       = auc_evento_sim,
  smaller_is_better = FALSE,    # AUC maior = melhor (obrigatório p/ métrica customizada)
  pred_wrapper = pred_prob,
  nsim         = 10
) %>%
  mutate(Variable = dplyr::recode(Variable, !!!rotulos_perm))  # sem tradução = nome bruto (exercicio.R)

p_perm <- ggplot(imp_perm, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_col(fill = "#4dac26", color = "white") +
  labs(
    title    = "Importância por permutação: Rede Neural",
    subtitle = "Queda média de AUC ao embaralhar cada variável\n(10 repetições, dados de teste)",
    x        = "Queda de AUC", y = NULL
  ) +
  theme_minimal(base_size = 13)
print(p_perm)
ggsave("output/04_rede_neural/mlp_importancia_permutacao.png", plot = p_perm,
       width = 7, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 11. CURVA ROC + AUC
# ══════════════════════════════════════════════════════════════════════════════
# A curva ROC mostra o trade-off sensibilidade × especificidade em TODOS
# os limiares de decisão possíveis.
# Este bloco de avaliação é idêntico ao do script 01 (repetição proposital:
# cada script é autocontido e a métrica comum permite comparar os modelos).
#   AUC = 0,5 → modelo aleatório (inútil)
#   AUC = 1,0 → modelo perfeito
#   AUC > 0,7 → discriminação aceitável

probs_teste <- predict(modelo_nn, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% select(has_pas))

probs_roc <- probs_teste %>%
  rename(prob_evento = .pred_Sim)   # probabilidade do evento: HAS = "Sim"

# AUC numérico: evento = "Sim" (2º nível do fator, daí event_level = "second")
auc_nn <- roc_auc(probs_roc, truth = has_pas, prob_evento, event_level = "second")
cat("AUC da rede neural:", round(auc_nn$.estimate, 3), "\n")

# Gráfico
roc_df <- roc_curve(probs_roc, truth = has_pas, prob_evento, event_level = "second")
ponto_otimo <- roc_df %>%
  mutate(youden = sensitivity + specificity - 1) %>%
  slice_max(youden, n = 1)

cat(sprintf(
  "Limiar ótimo (Youden): %.3f  |  Sensibilidade: %.3f  |  Especificidade: %.3f\n",
  ponto_otimo$.threshold, ponto_otimo$sensitivity, ponto_otimo$specificity
))

p_roc <- roc_df %>%
  autoplot() +
  geom_point(
    data  = ponto_otimo,
    aes(x = 1 - specificity, y = sensitivity),
    color = "#d73027", size = 3.5
  ) +
  annotate(
    "text",
    x     = 1 - ponto_otimo$specificity + 0.03,
    y     = ponto_otimo$sensitivity - 0.04,
    label = sprintf("Limiar ótimo\n(Youden = %s)", fmt_br(ponto_otimo$youden, 2)),
    hjust = 0, size = 3.2, color = "#d73027"
  ) +
  annotate(
    "text",
    x = 0.72, y = 0.18,
    label = paste0("AUC = ", fmt_br(auc_nn$.estimate)),
    size = 4.5, fontface = "bold", color = "#2166ac", hjust = 0
  ) +
  labs(
    title    = "Curva ROC: Rede Neural (MLP)",
    subtitle = "Análise 1 (9 preditores): NHANES 2021-2023",
    x        = "1 − Especificidade  (taxa de falsos positivos)",
    y        = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption  = "Diagonal pontilhada = modelo aleatório (AUC = 0,5)  |  Ponto vermelho = limiar ótimo (Youden)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc)
ggsave("output/04_rede_neural/mlp_roc.png", plot = p_roc, width = 7, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 12. COMPARAÇÃO DE PROFUNDIDADE: 1 vs 2 CAMADAS OCULTAS
# ══════════════════════════════════════════════════════════════════════════════
# O engine "nnet" suporta apenas 1 camada oculta.
# Para múltiplas camadas usamos "brulee" (torch), que aceita hidden_units
# como vetor: c(4, 4) = duas camadas ocultas de 4 neurônios cada.

# brulee carregado em 00_config.R

# ── Especificações ─────────────────────────────────────────────────────────────
# Mesma receita (receita) e mesmo split (dados_treino / dados_teste) das seções anteriores.
# Mantemos penalty e epochs iguais para isolar o efeito da profundidade.

espec_1c <- mlp(hidden_units = 4,penalty = 0.01, epochs = 300) %>%
  set_engine("brulee") %>%
  set_mode("classification")

# hidden_units como vetor → brulee cria uma camada para cada elemento
espec_2c <- mlp(hidden_units = c(4, 4), penalty = 0.01, epochs = 300) %>%
  set_engine("brulee") %>%
  set_mode("classification")

# ── Treinar ────────────────────────────────────────────────────────────────────
set.seed(42)
modelo_1c <- workflow() %>%
  add_recipe(receita) %>% add_model(espec_1c) %>% 
  fit(data = dados_treino)

set.seed(42)
modelo_2c <- workflow() %>%
  add_recipe(receita) %>% add_model(espec_2c) %>% 
  fit(data = dados_treino)

# ── AUC de cada arquitetura ───────────────────────────────────────────────────
get_probs <- function(modelo, dados) {
  predict(modelo, dados, type = "prob") %>%
    bind_cols(dados %>% select(has_pas)) %>%
    rename(prob_evento = .pred_Sim)
}

probs_1c <- get_probs(modelo_1c, dados_teste)
probs_2c <- get_probs(modelo_2c, dados_teste)

auc_1c <- roc_auc(probs_1c, truth = has_pas, prob_evento, event_level = "second")$.estimate
auc_2c <- roc_auc(probs_2c, truth = has_pas, prob_evento, event_level = "second")$.estimate
cat("AUC (1 camada oculta):", round(auc_1c, 3), "\n")
cat("AUC (2 camadas ocultas):", round(auc_2c, 3), "\n")

# ── Curvas ROC sobrepostas ─────────────────────────────────────────────────────
# Construir uma curva por modelo, marcar o AUC na legenda e sobrepor no mesmo eixo.
# Se as curvas estiverem próximas → profundidade adicional não agrega neste dataset.
# Esse resultado é esperado para dados tabulares com poucos preditores:
# profundidade ajuda em imagens/texto (muitos parâmetros livres), não aqui.

roc_comparacao <- bind_rows(
  roc_curve(probs_1c, truth = has_pas, prob_evento, event_level = "second") %>%
    mutate(modelo = paste0("1 camada   (AUC = ", round(auc_1c, 3), ")")),
  roc_curve(probs_2c, truth = has_pas, prob_evento, event_level = "second") %>%
    mutate(modelo = paste0("2 camadas  (AUC = ", round(auc_2c, 3), ")"))
)

p_comp <- ggplot(roc_comparacao,
                 aes(x = 1 - specificity, y = sensitivity, color = modelo)) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = c("#2166ac", "#d73027")) +
  labs(
    title   = "1 vs 2 Camadas Ocultas: Curvas ROC",
    x       = "1 \u2212 Especificidade  (taxa de falsos positivos)",
    y       = "Sensibilidade  (taxa de verdadeiros positivos)",
    color   = NULL,
    caption = "Engine: brulee/torch  |  Mesma receita e hiperparâmetros  |  Apenas profundidade varia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = c(0.72, 0.15),
    legend.background = element_rect(fill = "white", color = "gray80", linewidth = 0.3),
    plot.caption      = element_text(color = "gray40", size = 9)
  )
print(p_comp)
ggsave("output/04_rede_neural/mlp_comparacao_camadas.png", plot = p_comp,
       width = 8, height = 6, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# COMPARAÇÃO COM REGRESSÃO LOGÍSTICA (baseline)
# ══════════════════════════════════════════════════════════════════════════════
if (exists("auc_lr")) {
  cat(sprintf(
    "\nAUC  Regressão Logística : %.3f (baseline)\nAUC  Rede Neural           : %.3f\nGanho                     : %+.3f\n",
    auc_lr$.estimate, auc_nn$.estimate, auc_nn$.estimate - auc_lr$.estimate
  ))
} else {
  cat("\n[Comparação] Rode 01_regressao.R antes para ver o ganho sobre o baseline.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════