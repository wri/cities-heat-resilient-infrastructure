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

```bash
Rscript install_packages.R
```

## 🐍 Python Usage

Python is not required for the active `run-scenarios.R` pipeline.

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
  --plan 'BRA-Campinas@accelerator_area|aoi=DEFAULT:
            trees:pedestrian-achievable-90pctl[gdcu],
            cool-roofs:all-buildings[dcu];
          ZAF-Cape_Town@business_district|aoi=default:
            cool-roofs:all-buildings[dcu]' \
  --copy_baseline false
```

* aoi can be provided as an S3 object URL. If not specified, the default is:
`https://wri-cities-tcm.s3.us-east-1.amazonaws.com/city_projects/{CITY}/{AOI}/scenarios/baseline/baseline/aoi__baseline__baseline.geojson`
* `copy_baseline` takes the name of an AOI that already has baseline data (most commonly urban_extent). 
  For example, `--copy_baseline urban_extent` copies baseline data for tiles that 
  intersect the target AOI into that AOI’s folder. Scenario runs **require** 
  baseline data to exist in the target AOI folder.
  
Flags inside `[ ]`:
- `g`: generate scenario data
- `d`: download data
- `c`: run CTCM
- `u`: upload outputs


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

