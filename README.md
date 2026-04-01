# 🌍 cities-heat-resilient-infrastructure

Repository for generating and running heat-resilient infrastructure scenarios
(street trees, cool roofs, shade structures, and baseline workflows).

## ⚙️ Environment Setup (Reproducible Across EC2 Instances)

This repo uses:
- `chri.yml` for the Conda environment (R + geospatial system libraries)
- `install_packages.R` for R package installation

### 🐍 1. Create the Conda environment

```bash
conda env create -f chri.yml
conda activate chri
```

If the environment already exists:

```bash
conda env update -f chri.yml
conda activate chri
```

### 🟢 2. Install R packages

With the chri conda environment active, run:
```bash
Rscript install_packages.R
```
This may take some time!

## 🐍 Python Usage

Python is not currently required for the active pipeline.

## 🔐 Authentication

On EC2 instances with appropriate IAM roles, AWS and GEE may not require manual
login.

### ☁️ AWS

```bash
export AWS_PROFILE=cities-data-dev
aws sso login
aws s3 ls
```

### 🌎 Google Earth Engine / GCP

```bash
gcloud auth application-default login
gcloud config set project citiesindicators
gcloud auth application-default set-quota-project citiesindicators
```

Also ensure your account has Earth Engine access.

## 🚀 Running Scenarios

Main CLI entrypoint:
- `run-scenarios.R`

Example:

```bash
Rscript run-scenarios.R \
  --plan 'BRA-Campinas@accelerator_area|aoi_path=DEFAULT|copy_baseline=urban_extent:
            baseline:baseline[g],
            trees:pedestrian-achievable-90pctl[gdcp],
            cool-roofs:all-buildings[dcp];
          ZAF-Cape_Town@business_district|aoi_path=DEFAULT|copy_baseline=false:
            cool-roofs:all-buildings[dcp]'
```

To automatically terminate the EC2 instance after all groups finish add
`EC2_TERMINATE_ON_COMPLETE=true`:

```bash
EC2_TERMINATE_ON_COMPLETE=true Rscript run-scenarios.R --plan ....

```
The plan syntax is: 
```bash
{city_id}@{aoi_id}|aoi_path={s3_object_url}|copy_baseline={baseline_aoi_id}:
  {infrastructure_id}:{scenario_id}[flags]
```
Flags inside `[ ]`:
- `g`: generate scenario data (only flag that can be used for baseline)
- `d`: download data
- `c`: run CTCM
- `p`: process and upload outputs

Currently available infrastructures and scenarios are:

| infra_id | scenario_id | Definition |
| ----- | ----- | ----- |
| baseline | baseline\* | Baseline layers |
| trees | pedestrian-achievable-90pctl\* | Street trees achievable |
| cool-roofs | all-buildings | Cool roofs on all buildings |
| | large-buildings | Cool roofs on large buildings |
| cool-roofs_trees | all-buildings_pedestrian-achievable-90pctl | Combination cool roofs and street trees achievable |


\* Current behavior if baseline is flagged to generate is to process existing baseline data
to the necessary CCL layers and calculate metrics. This **does not** run the baseline
CTCM.
\* Running the pedestrian-achievable-90pctl tree scenario requires that the [Opportunity Layers](https://github.com/wri/cities-OpenUrban?tab=readme-ov-file#-generate-opportunity-layers) have been generated for a city. Generation of the scenario requires a geojson file that is created during this process.

Notes:

* `aoi_path` can be provided as an S3 object URL. If not specified, the default is:
`city_projects/{CITY}/{AOI}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson` 
in the `wri-cities-tcm` bucket where `{CITY}` is the city name and `{AOI}` is the AOI name
specified in the script invocation.
* Scenario runs **require** baseline data in the target AOI folder. Copy data from
  an existing baseline run by setting `copy_baseline` per city block inside `--plan` using `|copy_baseline=...`.
  Use `false` to disable, `true` (maps to `urban_extent`), or an AOI name such as `business_district`. If 
  omitted, `copy_baseline` is disabled.
  
## Resume tree scenario generation

The tree scenario generation may take a long time to run and may occasionally fail. The code works by iterating over a grid and you can resume the scenario generation from the point where it failed. Simply include `TREE_RESUME_AOI_GRID_ID=xx` before the script invocation where `xx` is the ID of the gridcell where the failure occured.

```bash
EC2_TERMINATE_ON_COMPLETE=true TREE_RESUME_AOI_GRID_ID=640 Rscript run-scenarios.R --plan 'BRA-Teresina@accelerator_area_big|aoi_path=DEFAULT|copy_baseline=false:
            trees:pedestrian-achievable-90pctl[gdcp],
            cool-roofs:all-buildings[gdcp]'

```


## 🖥 Running Long-Running Jobs on EC2

Use `screen` or `nohup` so jobs survive SSH disconnects.

### 🧩 `screen` (recommended)

```bash
screen -S chri
Rscript run-scenarios.R --plan '...'
# Detach: Ctrl+A then D
screen -r chri
```

### 🧾 `nohup`

```bash
nohup Rscript run-scenarios.R --plan '...' > run.log 2>&1 &
tail -f run.log
```
