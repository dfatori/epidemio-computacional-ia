# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — Rede Neural para Predição de Infarto (foco didático)
# Rode setup.R antes para ter nhanes_cardio disponível
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
# Pacotes carregados em 00_setup.R

# ── 1. Dados ─────────────────────────────────────────────────────────────────
# dados, dados_treino e dados_teste preparados pelo 00_setup.R

# ── 2. Recipe: pré-processamento ──────────────────────────────────────────────
# PASSO 1 — step_dummy():
#   Converte fatores em variáveis indicadoras (0/1).
#   Ex.: sexo vira "sexo_Masculino" (0 = Feminino, 1 = Masculino).
#   Com one_hot = FALSE usamos k-1 dummies (evita multicolinearidade perfeita).
#
# PASSO 2 — step_normalize():
#   Centraliza (média 0) e escala (dp 1) as variáveis numéricas.
#   ESSENCIAL para redes neurais: sem isso variáveis em escalas grandes
#   (ex.: colesterol ~200) dominam os pesos sobre variáveis pequenas (ex.: imc ~28).

receita <- recipe(infarto ~ ., data = dados_treino) %>%
  step_relevel(diabetes, ref_level = "Não") %>%
  step_relevel(fumante,  ref_level = "Não fuma") %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  step_normalize(all_numeric_predictors())

# ── 3. Especificação do modelo ────────────────────────────────────────────────
# mlp = Multi-Layer Perceptron — a arquitetura mais simples de rede neural.
#
# Arquitetura adotada (didática):
#   Entrada → [camada oculta: 4 neurônios] → Saída (P(infarto = "Sim"))
#
# hidden_units : neurônios na camada oculta
#                (4 é suficiente aqui; em produção, tunaria via cross-validation)
# penalty      : regularização L2 (weight decay) — penaliza pesos grandes,
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
# automaticamente ao fazer predict() — sem risco de vazamento de dados.

wf_nn <- workflow() %>%
  add_recipe(receita) %>%
  add_model(espec_nn)

# ── 5. Treinar ────────────────────────────────────────────────────────────────
set.seed(42)
modelo_nn <- wf_nn %>% fit(data = dados_treino)

# ── 6. Avaliar no conjunto de teste ───────────────────────────────────────────
predict(modelo_nn, dados_teste) %>%
  bind_cols(dados_teste %>% select(infarto)) %>%
  metrics(truth = infarto, estimate = .pred_class)

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
  # ── contínua ────────────────────────────────
  "idade"                  = "Idade",
  # ── sexo (PT-BR / inglês) ───────────────────
  "sexo_Feminino"          = "Sexo\n(feminino)",
  "sexo_Masculino"         = "Sexo\n(masculino)",
  "sexo_Female"            = "Sexo\n(feminino)",
  "sexo_Male"              = "Sexo\n(masculino)",
  # ── hipertensão ─────────────────────────────
  "hipertens_Sim"          = "Hipertensão\n(sim)",
  "hipertens_Não"          = "Hipertensão\n(não)",
  "hipertens_Yes"          = "Hipertensão\n(sim)",
  "hipertens_No"           = "Hipertensão\n(não)",
  # ── diabetes ────────────────────────────────
  "diabetes_Sim"           = "Diabetes\n(sim)",
  "diabetes_Não"           = "Diabetes\n(não)",
  "diabetes_Limítrofe"     = "Diabetes\n(limítrofe)",
  "diabetes_Yes"           = "Diabetes\n(sim)",
  "diabetes_No"            = "Diabetes\n(não)",
  "diabetes_Borderline"    = "Diabetes\n(limítrofe)",
  # ── fumante (SMQ040: status atual) ────────────────────
  "fumante_Todo dia"       = "Fumante\n(todo dia)",
  "fumante_Às vezes"       = "Fumante\n(às vezes)",
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
#        Emite P(infarto = "Sim") entre 0 e 1
#
#   CONEXÕES (linhas)
#   ├─ Azul   : peso POSITIVO → ativa / aumenta a probabilidade de saída
#   ├─ Vermelho: peso NEGATIVO → inibe / reduz a probabilidade de saída
#   └─ Espessura: proporcional à MAGNITUDE do peso (quanto mais grossa, mais influente)

p_net <- as.ggplot(function() {
  plotnet(
    nn_nativo,
    x_names    = rotulos_entrada,  # rótulos PT-BR para os nós de entrada
    y_names    = "Infarto",        # rótulo PT-BR para o nó de saída
    circle_cex = 5,                # tamanho dos nós
    cex_val    = 0.85,             # tamanho do texto dos rótulos
    bord_col   = "black",          # borda dos nós
    nid        = TRUE,             # NID: habilita cores e espessura por peso
                                   # (FALSE desabilitaria cores E espessura — tudo cinza)
    rel_rsc    = c(1, 6),          # [espessura mínima, máxima] das conexões
    var_labs   = TRUE,             # exibir nomes dos preditores
    pos_col    = "#2166ac",        # azul para pesos positivos
    neg_col    = "#d73027",        # vermelho para pesos negativos
    alpha_val  = 0.8               # transparência das linhas
  )
  title(
    main    = "",
    sub     = "Azul = peso positivo  |  Vermelho = peso negativo  |  Espessura = magnitude do peso",
    cex.sub = 0.8,
    col.sub = "gray40"
  )
})
ggsave("output/neural_network_estrutura.png", plot = p_net,
       width = 12, height = 7, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# DICAS DE LEITURA DO GRÁFICO
# ──────────────────────────────────────────────────────────────────────────────
# 1. Rastreie caminhos: um preditor com linhas grossas em AZUL chegando
#    a neurônios ocultos que também têm linhas grossas em AZUL para a saída
#    provavelmente contribui POSITIVAMENTE para o risco de infarto.
#
# 2. Pesos grandes (linhas grossas) indicam que aquela conexão "importa mais"
#    para o modelo — mas, ao contrário de coeficientes de regressão,
#    não têm interpretação direta de direção na camada oculta.
#
# 3. Para interpretar direção dos efeitos, prefira SHAP values ou PDP plots
#    (disponíveis via {DALEXtra} ou {pdp}) — o gráfico da rede mostra
#    ESTRUTURA, não causalidade.
# ══════════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════════
# 8. FUNÇÃO DE ATIVAÇÃO — SIGMOIDE
# ══════════════════════════════════════════════════════════════════════════════
# Cada neurônio oculto aplica esta função à sua soma ponderada de entradas.
# Ela "comprime" qualquer valor real para (0, 1) — funciona como um dimmer:
# entradas muito negativas → saída ≈ 0 (neurônio inibido)
# entradas muito positivas → saída ≈ 1 (neurônio ativado)

x_vals   <- seq(-6, 6, by = 0.05)
sig_vals <- 1 / (1 + exp(-x_vals))

png("output/neural_network_sigmoide.png", width = 900, height = 700, res = 120)
plot(
  x_vals, sig_vals,
  type = "l", lwd  = 2.5, col  = "#2166ac",
  xlab = "Entrada líquida  z  (soma ponderada das conexões)",
  ylab = "Saída do neurônio  \u03c3(z)",
  main = "Função de Ativação — Sigmoide (Logística)",
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
# 9. IMPORTÂNCIA DAS VARIÁVEIS — MÉTODO OLDEN
# ══════════════════════════════════════════════════════════════════════════════
# olden() percorre todos os caminhos da rede (entrada → oculta → saída)
# multiplicando os pesos em cadeia e somando as contribuições por variável.
# Preserva o SINAL — diferente de Garson(), que usa valores absolutos:
#   • Barra positiva : variável aumenta a probabilidade de infarto
#   • Barra negativa : variável reduz a probabilidade de infarto

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
    title   = "Importância das Variáveis — Método Olden",
    x       = NULL,
    y       = "Importância relativa (preserva sinal)",
    caption = paste(
      "Azul → variável aumenta o risco de infarto",
      "Vermelho → variável reduz o risco de infarto",
      sep = "  |  "
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_olden)
ggsave("output/neural_network_importancia.png", plot = p_olden, width = 8, height = 5, dpi = 150, bg = "white")

# ══════════════════════════════════════════════════════════════════════════════
# 10. CURVA ROC + AUC
# ══════════════════════════════════════════════════════════════════════════════
# Acurácia é enganosa com classes desbalanceadas.
# A curva ROC mostra o trade-off sensibilidade × especificidade em TODOS
# os limiares de decisão possíveis.
#   AUC = 0,5 → modelo aleatório (inútil)
#   AUC = 1,0 → modelo perfeito
#   AUC > 0,7 → discriminação aceitável

probs_teste <- predict(modelo_nn, dados_teste, type = "prob") %>%
  bind_cols(dados_teste %>% select(infarto))

# Detectar automaticamente o nome da coluna do evento positivo
# (primeiro nível do fator: "Sim" em PT-BR ou "Yes" em inglês)
col_evento <- paste0(".pred_", levels(dados_teste$infarto)[1])

probs_roc <- probs_teste %>%
  rename(prob_evento = all_of(col_evento))

# AUC numérico
auc_nn <- roc_auc(probs_roc, truth = infarto, prob_evento)
cat("AUC da rede neural:", round(auc_nn$.estimate, 3), "\n")

# Gráfico
roc_df <- roc_curve(probs_roc, truth = infarto, prob_evento)
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
    label = sprintf("Limiar ótimo\n(Youden = %.2f)", max(roc_df$sensitivity + roc_df$specificity - 1, na.rm = TRUE)),
    hjust = 0, size = 3.2, color = "#d73027"
  ) +
  labs(
    title   = "Curva ROC — Rede Neural (NHANES 2021-2023)",
    x       = "1 − Especificidade  (taxa de falsos positivos)",
    y       = "Sensibilidade  (taxa de verdadeiros positivos)",
    caption = paste0(
      "AUC = ", round(auc_nn$.estimate, 3),
      "  |  Diagonal pontilhada = modelo aleatório (AUC = 0,5)",
      "  |  Ponto vermelho = limiar ótimo (Youden)"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(color = "gray40", size = 9))
print(p_roc)
ggsave("output/neural_network_roc.png", plot = p_roc, width = 7, height = 6, dpi = 150, bg = "transparent")

# ══════════════════════════════════════════════════════════════════════════════
# 11. COMPARAÇÃO DE PROFUNDIDADE: 1 vs 2 CAMADAS OCULTAS
# ══════════════════════════════════════════════════════════════════════════════
# O engine "nnet" suporta apenas 1 camada oculta.
# Para múltiplas camadas usamos "brulee" (torch), que aceita hidden_units
# como vetor: c(4, 4) = duas camadas ocultas de 4 neurônios cada.

# brulee carregado em 00_setup.R

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
  add_recipe(receita) %>% add_model(espec_1c) %>% fit(data = dados_treino)

set.seed(42)
modelo_2c <- workflow() %>%
  add_recipe(receita) %>% add_model(espec_2c) %>% fit(data = dados_treino)

# ── AUC de cada arquitetura ───────────────────────────────────────────────────
get_probs <- function(modelo, dados) {
  predict(modelo, dados, type = "prob") %>%
    bind_cols(dados %>% select(infarto)) %>%
    rename(prob_evento = all_of(paste0(".pred_", levels(dados$infarto)[1])))
}

probs_1c <- get_probs(modelo_1c, dados_teste)
probs_2c <- get_probs(modelo_2c, dados_teste)

auc_1c <- roc_auc(probs_1c, truth = infarto, prob_evento)$.estimate
auc_2c <- roc_auc(probs_2c, truth = infarto, prob_evento)$.estimate
cat("AUC — 1 camada oculta :", round(auc_1c, 3), "\n")
cat("AUC — 2 camadas ocultas:", round(auc_2c, 3), "\n")

# ── Curvas ROC sobrepostas ─────────────────────────────────────────────────────
# Construir uma curva por modelo, marcar o AUC na legenda e sobrepor no mesmo eixo.
# Se as curvas estiverem próximas → profundidade adicional não agrega neste dataset.
# Esse resultado é esperado para dados tabulares com poucos preditores:
# profundidade ajuda em imagens/texto (muitos parâmetros livres), não aqui.

roc_comparacao <- bind_rows(
  roc_curve(probs_1c, truth = infarto, prob_evento) %>%
    mutate(modelo = paste0("1 camada   (AUC = ", round(auc_1c, 3), ")")),
  roc_curve(probs_2c, truth = infarto, prob_evento) %>%
    mutate(modelo = paste0("2 camadas  (AUC = ", round(auc_2c, 3), ")"))
)

p_comp <- ggplot(roc_comparacao,
                 aes(x = 1 - specificity, y = sensitivity, color = modelo)) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  scale_color_manual(values = c("#2166ac", "#d73027")) +
  labs(
    title   = "1 vs 2 Camadas Ocultas — Curvas ROC (NHANES 2021-2023)",
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
ggsave("output/neural_network_comparacao_camadas.png", plot = p_comp,
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
  cat("\n[Comparação] Rode 01_logistic_regression.R antes para ver o ganho sobre o baseline.\n")
}

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════