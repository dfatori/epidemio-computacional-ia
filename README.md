# Epidemiologia Computacional e Inteligência Artificial

Material complementar da aula "**Epidemiologia Computacional e Inteligência Artificial**", ministrada por [Daniel Fatori](https://scholar.google.com.br/citations?user=xIM4B1gAAAAJ&hl=pt-BR&hl=pt-BR) no contexto da **Prova didática do Concurso Público** para provimento de Professor Doutor no Departamento de Medicina Preventiva da Faculdade de Medicina da Universidade de São Paulo.

Este repositório contém análises didáticas de **aprendizado de máquina** aplicadas aos dados epidemiológicos do estudo **NHANES (anos 2021–2023)**.

---


## Objetivo do repositório

Apresentar conceitos básicos de aprendizado de máquina aplicados a área da epidemiologia. Os scripts e dados estão organizados de forma a permitir que o aluno possa baixar o conjunto completo de arquivos disponíveis do repositório e reproduzir as análises.

## Objetivo da análise

Desenvolver um modelo preditivo de **hipertensão arterial sistêmica** (pressão sistólica ≥ 140 mmHg) a partir de variáveis sociodemográficas e clínicas utilizando dados do estudo epidemiológicos NHANES de 2021-2023.

---

## Dados

Dados baixados pelo pacote [`nhanesA`](https://cran.r-project.org/package=nhanesA) (NHANES, ciclo L — 2021–2023) e salvos localmente em `data/`.

| Tabela NHANES | Conteúdo |
|---|---|
| `DEMO_L` | Idade, sexo, raça/etnia, renda |
| `BMX_L` | Antropometria: cintura e estatura (para calcular razão cintura/estatura) |
| `BPXO_L` | Pressão arterial oscilométrica (média das 3 medidas) — PAS ≥ 140 mmHg = **desfecho** |
| `DIQ_L` | Diabetes diagnosticada |
| `TCHOL_L` | Colesterol total sérico |
| `SMQ_L` | Tabagismo (SMQ020 + SMQ040 → 3 categorias) |
| `PAQ_L` | Atividade física: sedentarismo (min/dia sentado) |

---

## Estrutura do Repositório

```
.
├── scripts/
│   ├── rodar.R                     # Executa o fluxo de trabalho (00–04)
│   ├── 00_config.R                 # Baixa dados, limpeza e preparo
│   ├── 01_regressao.R              # Regressão Logística
│   ├── 02_random_forest.R          # Árvore de Decisão + Random Forest
│   ├── 03_gradient_boosting.R      # Gradient Boosting
│   ├── 04_rede_neural.R            # Rede Neural
│   ├── exercicio.R                 # Banco ampliado (22 preditores)
├── data/                           # Dados NHANES (.rds)
└── output/                         # Tabelas e gráficos
```

---

# Análise 1: Fluxo de trabalho

| # | Script | Modelo | Desfecho |
|---|---|---|---|
| 00 | `00_config.R` | — | Download e limpeza dos dados |
| 01 | `01_regressao.R` | Regressão Logística | HAS (PAS ≥ 140 mmHg) |
| 02 | `02_random_forest.R` | Árvore de Decisão + Random Forest | HAS (PAS ≥ 140 mmHg) |
| 03 | `03_gradient_boosting.R` | Gradient Boosting (stumps iterativos) | HAS (PAS ≥ 140 mmHg) |
| 04 | `04_rede_neural.R` | Rede Neural MLP (`nnet` + `brulee`) | HAS (PAS ≥ 140 mmHg) |

---

## Tabela descritiva dos dados

[Abrir tabela descritiva — banco baseline](output/00_config/tabela_descritiva.png)

---

# Análise 2: Exercício para alunos(as)

`exercicio.R` expande o banco de 9 para 22 preditores. Rode-o e compare o desempenho dos modelos.

Variáveis adicionais:

| Variável | Descrição | Tabela NHANES |
|---|---|---|
| `escolaridade` | Graduação vs. sem graduação | `DEMO_L` |
| `est_civil` | Casado/união estável vs. demais | `DEMO_L` |
| `imc` | Índice de massa corporal (kg/m²) | `BMX_L` |
| `freq_cardiaca` | Frequência cardíaca de repouso (bpm) | `BPXO_L` |
| `hba1c` | Hemoglobina glicada (%) | `GHB_L` |
| `hdl` | HDL colesterol (mg/dL) | `HDL_L` |
| `pcr` | Proteína C-reativa, alta sensibilidade (mg/L) | `HSCRP_L` |
| `ac_urico` | Ácido úrico sérico (mg/dL) | `BIOPRO_L` |
| `creatinina` | Creatinina sérica (mg/dL) | `BIOPRO_L` |
| `alcool` | Doses de álcool por dia (últimos 12 meses) | `ALQ_L` |
| `sono_horas` | Horas de sono por noite | `SLQ_L` |
| `saude_geral` | Saúde autorrelatada (boa vs. ruim) | `HUQ_L` |
| `phq9` | Score de depressão PHQ-9 (0–27) | `DPQ_L` |

---

## Tabela descritiva dos dados

[Abrir tabela descritiva — variáveis adicionais do banco ampliado](output/exer cicio/tabela_descritiva_ampliada.png)

---