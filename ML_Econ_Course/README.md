# Machine Learning, Big Data & AI in Economics — Course Repo

This repository contains a **Quarto** project for the MS-level course *Machine Learning, Big Data & AI in Economics* (School of Economics, QAU, Islamabad). It is designed for mixed math/coding backgrounds with **dual-track labs** in **R** and **Python**.

## Quick Start

1. **Install [Quarto](https://quarto.org/)** (required for rendering).
2. **Python env (Conda):**
   ```bash
   cd envs
   conda env create -f environment.yml
   conda activate ml-econ
   python -m ipykernel install --user --name=ml-econ --display-name "Python (ml-econ)"
   ```
3. **R packages (optional first-run script):**
   ```r
   source("envs/renv_setup.R")
   # If you prefer renv:
   # install.packages("renv"); renv::init(); renv::snapshot()
   ```
4. **Render syllabus (HTML/PDF):**
   ```bash
   quarto render syllabus/syllabus.qmd
   ```
5. **Open starter notebooks:**
   - `notebooks/python/week01_intro.qmd` (Jupyter/Python)
   - `notebooks/R/week01_intro.qmd` (R/knitr)

## Repo Layout

```
ML_Econ_Course/
├─ _quarto.yml                 # Quarto project config (site-style)
├─ syllabus/
│  └─ syllabus.qmd            # Printable course syllabus
├─ envs/
│  ├─ environment.yml         # Conda env for Python
│  └─ renv_setup.R            # R package installation helper
├─ notebooks/
│  ├─ python/week01_intro.qmd # Python starter notebook
│  └─ R/week01_intro.qmd      # R starter notebook
├─ src/
│  ├─ python/utils.py         # Python helpers (placeholders)
│  └─ R/utils.R               # R helpers (placeholders)
├─ data/
│  ├─ raw/.gitkeep
│  └─ processed/.gitkeep
├─ models/.gitkeep
├─ reports/.gitkeep
├─ figures/.gitkeep
└─ templates/
   ├─ rubric_labs.md
   ├─ rubric_capstone.md
   ├─ model_card.md
   ├─ data_card.md
   └─ policy_memo_template.md
```

## Notes
- The **syllabus** encodes the 15-week plan, resources, assessment, and capstone.
- Templates include **rubrics**, **model card**, **data card**, and a **policy memo** scaffold.
- For Spark/Dask demos (Week 13), see comments in `envs/environment.yml` to enable extras.