# ![](www/LipidAnalyst.png){width="49"} LipidAnalyst

**LipidAnalyst** is a Shiny-based, end-to-end lipidomics analysis platform designed for researchers without programming expertise. It enables streamlined data preprocessing, statistical analysis, visualization, and network modeling within an interactive interface.

## 🔬 Features

-    📊 **Data preprocessing**

    -   Feature filtering

    -   Missing value imputation

    -   Merging lipid duplicates or isoforms

    -   Normalization (internal standard, class-based, etc.)

-    🧪 **Statistical analysis**

    -   Student t-test, Welch’s t-test, ANOVA

    -    Volcano plots

-    📉 **Multivariate analysis**

    -    PCA

    -    PLS-DA / OPLS-DA

-    🧠 **Network analysis**

    -    Correlation networks

    -    DSPC (debiased sparse partial correlation)

-    🤖 **Machine learning**

    -    Random Forest classification with Feature importance ranking

-    🔥 **Novel visualization**

    -    Differential Mean Lipid Heatmaps\
        (organized by carbon chain length × unsaturation)

## 🖥️ Run locally with Docker

### 1. Build image

```         
docker build -t lipidanalyst:latest .
```

### 2. Run container

```         
docker run --rm -p 3838:3838 lipidanalyst:latest
```

### 3. Open in browser

```         
http://localhost:3838
```

## 📦 Reproducibility

This project uses:

-    `renv` for R package management

-    `Docker` for full environment reproducibility

To restore the R environment manually:

```         
renv::restore()
```

## 📊 Example Data

LipidAnalyst includes embedded example data within the application for demonstration purposes.\
These data are preloaded in the app interface and allow users to explore functionalities without uploading external files.

For privacy and data size considerations, raw datasets are not distributed separately.

Users are encouraged to upload their own lipidomics datasets for analysis.

## 📄 License

MIT License

## 👤 Author

Lily (Xinyi) Liu
