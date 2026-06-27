# ══════════════════════════════════════════════════════════════════════════════
# NHANES 2021-2023 — DAG: Determinantes de Infarto do Miocárdio
# Pacotes: dagitty + ggdag
# ══════════════════════════════════════════════════════════════════════════════

# ── 0. Pacotes ────────────────────────────────────────────────────────────────
library(dagitty)
library(ggdag)
library(ggplot2)

# ── 1. Definir o DAG ──────────────────────────────────────────────────────────
# Hierarquia causal:
#   Nível 1 — Determinantes sociais distais : raca, renda, escolaridade, est_civil
#   Nível 2 — Biossociais (não modificáveis): idade, sexo
#   Nível 3 — Comportamentais / intermediários: fumante, imc
#   Nível 4 — Fatores de risco clínicos     : hipertens, diabetes, colesterol
#   Desfecho                                : infarto
#
# ATENÇÃO — mediadores:
#   hipertens, diabetes e colesterol são mediadores parciais de fumante e imc.
#   Controlá-los na análise principal gera viés de supercontrole.

dag <- dagitty('dag {
  raca         [pos="0,2"]
  renda        [pos="1,2"]
  escolaridade [pos="2,2"]
  est_civil    [pos="3,2"]
  idade        [pos="0,1"]
  sexo         [pos="1,1"]
  fumante      [exposure, pos="2,1"]
  imc          [pos="3,1"]
  hipertens    [pos="1,0"]
  diabetes     [pos="2,0"]
  colesterol   [pos="3,0"]
  infarto      [outcome, pos="4,0"]

  raca         -> renda
  raca         -> infarto
  renda        -> escolaridade
  renda        -> hipertens
  renda        -> diabetes
  renda        -> infarto
  escolaridade -> fumante
  escolaridade -> imc
  est_civil    -> fumante
  est_civil    -> imc

  idade -> fumante
  idade -> imc
  idade -> hipertens
  idade -> diabetes
  idade -> colesterol
  idade -> infarto

  sexo  -> fumante
  sexo  -> imc
  sexo  -> hipertens
  sexo  -> colesterol
  sexo  -> infarto

  fumante -> hipertens
  fumante -> colesterol
  fumante -> diabetes
  fumante -> infarto

  imc -> hipertens
  imc -> diabetes
  imc -> colesterol
  imc -> infarto

  hipertens  -> infarto
  diabetes   -> infarto
  colesterol -> infarto
}')

# ── 2. Verificar implicações de independência condicional ─────────────────────
# dagitty deriva as independências que o DAG implica — útil para testar nos dados
impliedConditionalIndependencies(dag)

# ── 3. Conjunto mínimo de ajuste: efeito de fumante → infarto ─────────────────
# Quais variáveis controlar para estimar o efeito causal total de fumante?
# NÃO inclui hipertens/diabetes/colesterol (mediadores — bloqueariam o caminho)
adjustmentSets(dag,
               exposure = "fumante",
               outcome  = "infarto",
               type     = "minimal")

# Conjunto de ajuste para efeito direto (controlando mediadores):
adjustmentSets(dag,
               exposure = "fumante",
               outcome  = "infarto",
               effect   = "direct")

# ── 4. Visualização base (ggdag) ──────────────────────────────────────────────
p1 <- ggdag(dag, text = TRUE, use_labels = "name") +
  theme_dag() +
  labs(title = "DAG — Determinantes de Infarto (NHANES 2021-2023)")

ggsave("output/dag_geral.png", plot = p1, width = 12, height = 8, dpi = 150)

# ── 5. Destacar caminhos de backdoor (fumante → infarto) ─────────────────────
p2 <- ggdag_adjustment_set(dag,
                     exposure = "fumante",
                     outcome  = "infarto",
                     type     = "minimal") +
  theme_dag() +
  labs(title    = "Conjunto mínimo de ajuste: fumante → infarto",
       subtitle = "Variáveis em azul devem ser controladas")

ggsave("output/dag_ajuste_fumante.png", plot = p2, width = 12, height = 8, dpi = 150)

# ── 6. Visualizar status causal dos nós ───────────────────────────────────────
# Mostra exposição, desfecho, confundidores, mediadores
p3 <- ggdag_status(dag) +
  theme_dag() +
  labs(title = "Status causal de cada variável")

ggsave("output/dag_status.png", plot = p3, width = 12, height = 8, dpi = 150)

# ── 7. Colisores e confundidores (via tidy_dagitty) ──────────────────────────
# tidy_dagitty converte o DAG para tibble com metadados por no
# node_collider() anota quais nos sao colisores em algum caminho
# node_status()   anota exposure / outcome / latent

tdag <- dag %>%
  tidy_dagitty() %>%
  node_collider() %>%   # adiciona coluna: colliders (TRUE/FALSE)
  node_status()         # adiciona coluna: status (exposure/outcome/NA)

p4 <- tdag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = colliders), size = 14) +
  geom_dag_text(size = 3) +
  scale_color_manual(
    name   = "Colisor",
    values = c("TRUE" = "#e74c3c", "FALSE" = "#95a5a6"),
    labels = c("TRUE" = "Sim", "FALSE" = "Nao")
  ) +
  theme_dag() +
  labs(title    = "Colisores no DAG",
       subtitle = "Vermelho = no colisor (duas setas entrando)")

ggsave("output/dag_colisores.png", plot = p4, width = 12, height = 8, dpi = 150)

# ── 8. Confundidores identificados pelo dagitty ───────────────────────────────
# Um confundidor abre um caminho de backdoor entre exposicao e desfecho.
# dagitty::adjustmentSets() ja retorna exatamente esse conjunto.
# Aqui visualizamos destacando esses nos no grafico.

conjunto_ajuste <- adjustmentSets(dag,
                                  exposure = "fumante",
                                  outcome  = "infarto",
                                  type     = "minimal")

confundidores <- unlist(conjunto_ajuste)

p5 <- tdag %>%
  mutate(confundidor = name %in% confundidores) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = confundidor), size = 14) +
  geom_dag_text(size = 3) +
  scale_color_manual(
    name   = "Confundidor",
    values = c("TRUE" = "#2980b9", "FALSE" = "#95a5a6"),
    labels = c("TRUE" = "Sim", "FALSE" = "Nao")
  ) +
  theme_dag() +
  labs(title    = "Confundidores: fumante -> infarto",
       subtitle = "Azul = deve ser controlado para fechar backdoor paths")

ggsave("output/dag_confundidores.png", plot = p5, width = 12, height = 8, dpi = 150)

# ── 9. Caminhos de backdoor explícitos ───────────────────────────────────────
paths(dag, from = "fumante", to = "infarto", directed = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# fim do script ────────────────────────────────────────────────────────────────
# ══════════════════════════════════════════════════════════════════════════════