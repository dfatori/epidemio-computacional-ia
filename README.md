# Epidemiologia Computacional e Inteligência Artificial

Material de aula com pipeline completo de machine learning aplicado a dados populacionais reais — da limpeza dos dados à comparação de modelos preditivos — usando o ciclo **NHANES 2021–2023** como base.

---

## Objetivo

Prever a ocorrência de **infarto do miocárdio** (e, como contraste, o **colesterol total**) a partir de variáveis sociodemográficas e clínicas disponíveis no NHANES, comparando cinco abordagens de modelagem de forma didática e progressiva.

---

## Dados

Os dados são baixados automaticamente pelo pacote [`nhanesA`](https://cran.r-project.org/package=nhanesA) (NHANES, ciclo L — 2021–2023) e salvos localmente em `data/` para reutilização.

| Tabela NHANES | Conteúdo |
|---|---|
| `DEMO_L` | Idade, sexo, raça/etnia, renda |
| `BMX_L` | IMC, circunferência da cintura |
| `BPQ_L` | Hipertensão diagnosticada |
| `DIQ_L` | Diabetes diagnosticada |
| `TCHOL_L` | Colesterol total sérico |
| `SMQ_L` | Tabagismo |
| `MCQ_L` | Infarto do miocárdio autorrelatado (**desfecho**) |
| `PAQ_L` | Atividade física |

> A pasta `data/` está no `.gitignore` — os dados são recriados localmente ao rodar `00_setup.R`.

---

## Estrutura do Repositório

```
.
├── scripts/
│   ├── workflow.R                  # Ponto de entrada: executa todo o pipeline
│   ├── 00_setup.R                  # Download, limpeza e preparo dos dados
│   ├── 01_logistic_regression.R    # Regressão Logística
│   ├── 02_random_forest.R          # Árvore de Decisão + Random Forest
│   ├── 03_gradient_boosting.R      # Gradient Boosting (gbm / XGBoost)
│   ├── 04_neural_network.R         # Rede Neural (MLP via brulee)
│   ├── 05_linear_regression.R      # Regressão Linear (desfecho: colesterol)
│   └── leftovers/
│       ├── dag.R                   # Visualização do DAG causal
│       ├── causal_inference.R      # Inferência causal: supercontrole vs DAG vs Causal Forest
│       └── leftover.r              # Trechos experimentais
├── data/                           # Dados NHANES em cache (.rds) — ignorado pelo git
└── output/                         # Tabelas e gráficos gerados — ignorado pelo git
```

---

## Pipeline

Execute o pipeline completo a partir da raiz do projeto:

```r
source("scripts/workflow.R")
```

Ou rode cada etapa individualmente na ordem abaixo:

| # | Script | Modelo | Desfecho |
|---|---|---|---|
| 00 | `00_setup.R` | — | Download e limpeza dos dados |
| 01 | `01_logistic_regression.R` | Regressão Logística | Infarto (binário) |
| 02 | `02_random_forest.R` | Árvore de Decisão + Random Forest | Infarto (binário) |
| 03 | `03_gradient_boosting.R` | Gradient Boosting (stumps iterativos) | Infarto (binário) |
| 04 | `04_neural_network.R` | Rede Neural MLP (`brulee`) | Infarto (binário) |
| 05 | `05_linear_regression.R` | Regressão Linear (OLS) | Colesterol total (mg/dL) |

---

## Conteúdo Didático por Script

- **`01` — Regressão Logística**: função logística, log-odds, odds ratio, curva ROC.
- **`02` — Árvore + Random Forest**: critério de Gini, matrix de custo para desbalanceamento, importância de variáveis.
- **`03` — Gradient Boosting**: conceito de *aprendiz fraco* (stump), learning rate, detecção do número ótimo de árvores via CV.
- **`04` — Rede Neural**: pré-processamento com `step_dummy` + `step_normalize`, arquitetura MLP, visualização de pesos.
- **`05` — Regressão Linear**: mínimos quadrados ordinários (OLS), resíduos, diagnóstico.

### Extras (`leftovers/`)
- **`dag.R`**: construção e visualização do DAG (directed acyclic graph) para o efeito causal do tabagismo sobre infarto.
- **`causal_inference.R`**: demonstração dos erros de supercontrole, ajuste correto pelo DAG e estimação por *Causal Forest* (`grf`).

---

## Dependências

```r
# Instala automaticamente via 00_setup.R
nhanesA, dplyr, forcats, gtsummary, tidymodels,
vip, rpart.plot, gbm, NeuralNetTools, ggplotify, brulee
```

> Para a rede neural (`brulee`), pode ser necessário instalar o backend PyTorch:
> ```r
> torch::install_torch()
> ```
