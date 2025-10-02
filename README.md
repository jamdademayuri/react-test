# PFB Data Analysis in RStudio

An interactive Shiny application for analyzing **Portable Format for Biomedical Data (PFB)** stored in **AVRO files**.  
This tool enables researchers to:

- Flatten and explore AVRO datasets  
- Perform **percentage distributions**  
- Generate **Kaplan–Meier survival curves (OS & EFS)**  
- Build **Table 1 summary statistics**  
- Export results as **Excel (.xlsx)** and **plots (.png)**  

The app runs inside an RStudio environment using Docker.

---

## Getting Started

### 1. Prerequisites

-   Install [Docker](https://www.docker.com/) on your system

### 2. Setup

#### Clone the Repository

``` bash
git clone https://github.com/chicagopcdc/pcdc-analysis-notebook.git
cd pcdc-analysis-notebook
```

#### Build the Docker Image

``` bash
docker build -t my-rstudio .
```

#### Run the Container

``` bash
docker run -d --name my-rstudio-container -p 8787:8787 -e DISABLE_AUTH=true my-rstudio
```

### 3. Access the RStudio Environment

Once the container is running, open your browser and go to:

    http://localhost:8787

No login credentials are required (`DISABLE_AUTH=true`).

Inside RStudio, you will find **4 pre-loaded files** under
`/home/rstudio`.\
The entry point for the Shiny app is:

    app.R

Run `app.R` to launch the application.

------------------------------------------------------------------------


## Workflow

### Step 1: Upload AVRO (PFB format)
- In the UI, upload your **PFB AVRO file**.  
- Supported format: **Portable Format for Biomedical Data (PFB)**.

### Step 2: Select Entities
- Core entities automatically included:  
  - `subject`  
  - `person`  
  - `timing`  
  - `survival_characteristic`  
- You can select **additional entities** to join with the dataset.

### Step 3: Data Preparation
- The selected entities will be **flattened** into a single table.  
- Progress is displayed in the sidebar.

### Step 4: Analyses
- **Percentage Distribution**  
  - Select a column and view percentage breakdowns.  
  - Download `.xlsx` summary and `.png` plot.

- **Survival Curves**  
  - Overall Survival (OS) using `timing + survival_characteristic`.  
  - Event-Free Survival (EFS) using `subject + timing (Initial Diagnosis)`.  
  - Download plots as `.png`.

- **Summary Table (Table 1)**  
  - Without grouping → analyze all subjects together.  
  - With grouping → upload an **Excel/TXT/CSV file** containing Subject IDs.  
    - Group 1 = uploaded IDs  
    - Group 2 = remaining subjects  
  - Download results as `.xlsx`.

---

## Container Management

Stop the container:
```bash
docker stop my-rstudio-container
```

Restart later (no rebuild required):
```bash
docker start my-rstudio-container
```