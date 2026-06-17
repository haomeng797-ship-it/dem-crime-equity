# Data sources and terms

This folder holds third-party source data, kept here so the analysis can be reproduced. Each
dataset is the property of its original providers and is subject to their own terms of use. Please
cite the providers below as the data source, not this repository.

| File | Dataset | Provider | Link |
|---|---|---|---|
| `raw/SDI_2.0.csv`, `raw/SDI_2.0_variables_list.csv` | State Democracy Index 2.0 (2000–2023) | Grumbach & Bitton, UC Berkeley Democracy Policy Lab | https://democracypolicylab.berkeley.edu/state-democracy-index/ |
| `raw/incarceration_trends_state.csv`, `raw/Vera_codebook_03-2026.pdf` | Incarceration Trends (state file) | Vera Institute of Justice | https://github.com/vera-institute/incarceration-trends |
| `raw/ufl_turnout_1980_2022.csv`, `raw/ufl_turnout_doc.txt` | Voter Turnout 1980–2022 (v1.2) | M. McDonald, UF Election Lab | https://election.lab.ufl.edu/voter-turnout/ |

The merged analysis file `state_dem_incarceration.{rds,csv}` is derived from these inputs by
`R/01_load_merge.R`.
