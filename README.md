# Epidemiologia Computacional e Inteligência Artificial

Material complementar da aula "**Epidemiologia Computacional e Inteligência Artificial**", ministrada por [Daniel Fatori](https://scholar.google.com.br/citations?user=xIM4B1gAAAAJ&hl=pt-BR&hl=pt-BR) no contexto da **Prova didática do Concurso Público** para provimento de Professor Doutor no Departamento de Medicina Preventiva da Faculdade de Medicina da Universidade de São Paulo.

Este repositório contém análises didáticas de **aprendizado de máquina** aplicadas aos dados epidemiológicos do estudo **NHANES (anos 2021–2023)**.

---

## Objetivo do repositório

Apresentar conceitos básicos de aprendizado de máquina aplicados a área da epidemiologia. Os scripts e dados estão organizados de forma a permitir que o aluno possa baixar o conjunto completo de arquivos disponíveis do repositório e reproduzir as análises.

## Objetivo das análises

Desenvolver modelos preditivos de **hipertensão arterial sistêmica** (pressão sistólica ≥ 140 mmHg) a partir de variáveis sociodemográficas e clínicas utilizando dados do estudo epidemiológico NHANES de 2021-2023.

---

# Estrutura do Repositório

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

# Análise 1

Modelo preditivo de hipertensão em adultos com 9 preditores sociodemográficos e clínicos (idade, sexo, raça, razão cintura/estatura, renda, colesterol, diabetes, tabagismo, sedentarismo) do NHANES 2021–2023 (N=4.687).

## Fluxo de trabalho

| # | Script | Modelo |
|---|---|---|
| 00 | `00_config.R` | — |
| 01 | `01_regressao.R` | Regressão Logística |
| 02 | `02_random_forest.R` | Árvore de Decisão + Random Forest |
| 03 | `03_gradient_boosting.R` | Gradient Boosting |
| 04 | `04_rede_neural.R` | Rede Neural MLP |

## Variáveis preditoras

[Tabela descritiva dos dados](output/00_config/tabela_descritiva.png)

---

# Exercício

Modelo preditivo de hipertensão em adultos com 22 preditores sociodemográficos, clínicos, laboratoriais e comportamentais (além das variáveis da análise 1: escolaridade, estado civil, IMC, frequência cardíaca, HbA1c, HDL, PCR, ácido úrico, creatinina, álcool, sono, saúde autorrelatada, PHQ-9) do NHANES 2021–2023 (N=3.944).

---

## Variáveis preditoras

[Tabela descritiva dos dados ampliados](output/exercicio/tabela_descritiva_ampliada.png)

---

## Passo a passo

**Antes de começar**

1. Instale o **R** (https://cran.r-project.org) e o **RStudio** (https://posit.co/downloads);
2. Baixe este repositório: no GitHub, botão verde **"Code" → "Download ZIP"**, e descompacte em uma pasta do seu computador;
3. Dê **duplo-clique no arquivo `aula_concurso_epidemio.Rproj`**. Isso abre o RStudio já na pasta certa.

**Rodando o exercício**

4. No RStudio, abra o arquivo `scripts/exercicio.R` (painel de arquivos, canto inferior direito).
5. Rode o script inteiro com **Ctrl + Shift + Enter**. Este script prepara o banco ampliado e roda os 4 modelos, terminando com a tabela de AUCs no console. Na primeira vez pode demorar vários minutos, já que instala pacotes e baixa os dados (precisa de internet).
6. Compare a tabela final com os resultados da **Análise 1**, já salvos na pasta [`output/`](output/) do repositório: quanto cada modelo ganhou com as 13 variáveis extras? Valeu a pena? Será que mais variáveis são necessárias para um bom modelo?

---