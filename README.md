# Replicating the California fast-food minimum wage study

Nitin Lodha. STAT 5200 Applied Econometrics at Penn. Fall 2026.

This is my replication project for the course. I replicate this paper:

> Clemens, Jeffrey, Olivia Edwards, Jonathan Meer and Joshua D. Nguyen. "Employment Effects of
> California's Fast Food Minimum Wage: Evidence from the QCEW and QWI." NBER Working Paper 34033.
> First posted July 2025 and revised October 2026. https://www.nber.org/papers/w34033

## What the paper asks

In April 2024 California raised the minimum wage at big fast-food chains from $16 to $20 an hour
(law AB 1228). The paper asks what this did to fast-food jobs. It compares California with other
states using two public data sets: the QCEW from BLS and the QWI from the Census.

I chose this paper because my course project asks the same question with the same QCEW data. It is
also very new.

I rebuilt every number myself in R starting from the raw public files. The authors share their code
online. I only looked at it to understand their definitions and to check my answers.

## The short version

- I match all 336 published numbers on the data the authors used.
- My two-year QCEW numbers are a little off on today's data. BLS revised its 2025 numbers after the
  authors downloaded them. When I run my code on their data file it matches exactly.
- My three extensions show that the answer depends a lot on what you compare California with.

## Folders

```
code/              R scripts (00_run_all.R runs everything in order)
data/raw/          data exactly as downloaded
data/clean/        state panels built from the raw data
results/tables/    every table as a CSV with the published number next to mine
results/figures/   every figure as a PDF
presentation/      my slides and the speaker script
```

The code reads from `data` and writes to `results`. It never changes the files in `data/raw`. All
paths use the `here` package so the code works on any computer as long as you start R in this folder.

## Data

| Source | What I use | Where it comes from |
|---|---|---|
| QCEW | Monthly jobs and quarterly wages by state from 2015 to 2025 for all private jobs (10), retail (44-45), accommodation and food (72), full-service restaurants (722511) and fast food (722513) | BLS open data API `https://data.bls.gov/cew/data/api/YEAR/QTR/industry/CODE.csv` |
| QWI | Jobs at the start of each quarter (Emp) by state for the same industries from release R2026Q2 (the one the paper uses) | `https://lehd.ces.census.gov/data/qwi/R2026Q2/` |
| CPI-U | Turns weekly wages into real wages | FRED series CPIAUCNS |

`01_download_data.R` keeps only the rows I need so the raw data is about 14 MB. The download date is
in `data/raw/download_log.txt`. I keep the raw files in the repo on purpose. BLS and Census revise
their numbers over time and these files give my exact results even after the next revision.

## How to run it

You need R with the `here` and `quadprog` packages. From this folder run

```
Rscript code/00_run_all.R
```

It runs scripts 01 to 10 in order. The log goes to `results/run_log.txt` and package versions go to
`results/session_info.txt`. The download steps skip files that are already here.

| Script | What it does |
|---|---|
| `01_download_data.R` | downloads the raw data and skips files already here |
| `02_clean_qcew.R` | builds the state by month QCEW panel. Higher wage industries are all private jobs minus food (72) and retail (44-45) |
| `03_clean_qwi.R` | builds the state by quarter QWI panel from 2015Q1 to 2025Q3 |
| `04_qcew_tables.R` | Tables 1 to 4 and A1 |
| `05_qwi_tables.R` | Tables 5 to 8 and A2 to A4 |
| `06_vintage_check.R` | runs my code on the authors' own QCEW file to explain the two-year gaps |
| `07_figures.R` | Figures 1, 2, A2, A4 and A5 plus the placebo pictures |
| `08_medians_elasticities.R` | medians, jobs lost, wage effect and elasticities from Sections 4.1 and 4.2 |
| `09_extensions.R` | my three extensions |
| `10_scorecard.R` | counts how many published numbers I match |
| `functions.R` | shared settings and helper functions |
| `published.R` | every published number typed in from the paper |

The first time `06_vintage_check.R` runs it downloads the authors' replication package (about 86 MB)
into `authors_package/`. That folder is not in the repo.

## How well it matches

| Tables | Numbers | Match | Largest gap |
|---|---|---|---|
| Tables 1 and 2: QCEW after 1 year | 64 | 64 | 0.005 |
| Tables 3 and 4: QCEW after 2 years on today's data | 64 | 20 | 0.233 |
| Tables 3 and 4: QCEW after 2 years on the authors' data | 64 | 64 | 0.005 |
| Tables 5 and 6: QWI after 1 year | 64 | 64 | 0.005 |
| Tables 7 and 8: QWI after 2 years | 64 | 64 | 0.005 |
| Tables A3 and A4: QWI with a 2023Q4 base | 64 | 64 | 0.005 |
| Table A1: QCEW job counts | 8 | 8 | 0 |
| Table A2: QWI job counts | 8 | 8 | 0 |

Each pair of tables has 30 numbers in Panel A (I leave out the base column since it is always 100) and
18 in Panel B. The placebo test adds 16 more (8 estimates and 8 p-values). A number counts as a match
when it rounds to the published value. Job counts have to match exactly. The lists of placebo states
that beat California match in every table too.

Section 4.2 lines up as well. After one year the real weekly wage in California fast food grew 8.4
percent more than in the rest of the US (the paper says about 8). After two years the gap is 6.3
percent (the paper says just over 6). The median one-year QCEW effect is -3.3 percent or about 18,900
jobs. That gives an elasticity of -0.39 next to the paper's -0.40. The one-year QWI elasticity is -0.16
next to -0.17. The paper does not say exactly which wage number it divides by so I think these tiny
gaps are rounding.

### What does not match and why

**Tables 3 and 4 (QCEW after two years).** BLS revised its 2025 data after the authors downloaded it.
My data and theirs are the same in every state and month from 2015 to 2024. But 400 of the 612 state by
month fast-food numbers for 2025 changed. For example California had 554,166 fast-food jobs in
September 2025 in the authors' file and has 553,092 today. When I run my code on the authors' data
every number in Tables 3 and 4 matches. On today's data the two-year effects are a bit bigger. The
simple DD is -2.97 instead of -2.74.

**QWI.** Census updates the QWI every quarter too. Using release R2026Q2 like the paper gives an exact
match. The newer R2026Q3 release would move the numbers a little.

## Choices that matter

- Every series is set to 100 in September 2023 when the law was signed. The paper does not use April
  2024 because firms could react as soon as they knew the law was coming.
- Alaska and Michigan are left out everywhere because their QWI data have gaps.
- A state can be a control only if it has data in every period. For the QCEW that means every
  September from 2015 to 2025. For the QWI it means every quarter from 2015Q1 to 2025Q3.
- The pre-trend comes only from the two years before the law (September 2021 to September 2023).
- Placebo p-values are one-sided: p = (b + 1) / (m + 1). Here b is the number of placebo states with a
  more negative estimate than California and m is the number of placebo states.

## My extensions

My project asks how much of the debate over AB 1228 comes down to the choice of comparison group.
These three extensions test that. They all use the QCEW and the paper's placebo test
(`09_extensions.R`).

1. **Full-service restaurants as the control.** The law does not cover sit-down restaurants. Yet
   California's full-service restaurants also fell 2.4 percent compared with other states after one
   year (placebo p = 0.08). Using them as the control inside California like Sosinskiy and Reich do
   gives -0.46 after one year (p = 0.39) and +0.35 after two. The authors argue the law
   also hit full-service restaurants through prices and the job market so they are not a clean control.
2. **Starting month.** The one-year effect is -2.12 (p = 0.12) when I measure from December 2023
   like Dube does. It is -1.24 (p = 0.22) when I measure from March 2024 just before the $20 wage
   started. So a lot of the drop happened between the signing of the law and the start of the new wage.
3. **Synthetic control.** I built a synthetic California as a weighted mix of other states. I find
   the weights with quadratic programming so they add up to one and are never negative.
   Fit on September 2021 to September 2023 it gives -3.28 after one year. Fit on January 2015 to
   September 2023 it gives -2.75. Both have placebo p = 0.04. But the weights change completely. The
   first fit leans on Hawaii and Nevada and the second on Florida and New York. That is the fragility
   the authors warn about.

## Presentation

`presentation/replication_presentation.pptx` is my 10-minute recorded talk. It has 18 slides on the
course template. They cover the research question, data, method and results. Then they go through
the replication itself, my extensions, lessons learned and how the paper shapes my project. Every table and chart is a
real PowerPoint chart or table filled from `results/tables` and not a screenshot. Each slide's notes
hold that part of the script. `presentation/speaker_script.pdf` has the full script with timings that
add up to 10 minutes.

## Software

R 4.6.1 with `here` and `quadprog`. Everything else is base R. Exact versions are in
`results/session_info.txt`.
