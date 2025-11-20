# DE Venn Explorer

**Version 2.0 (Enhanced)**  
**DE Venn Explorer** is an interactive **R Shiny application** for visualizing and comparing **differential gene expression (DE)** overlaps across multiple datasets.  
It supports 2–5 DE gene sets, flexible statistical filters, and multiple visualization types (Venn, Euler, UpSet, Sankey) to help you explore shared and unique genes from RNA‑seq or other expression analyses.

Developed by **Dinuka Adasooriya**, Yonsei University College of Dentistry, Seoul, Korea.

---

## Access

- **Live app (Posit Cloud)** – run DE Venn Explorer in your browser:  
  https://dinuka-de-venn-explorer.share.connect.posit.cloud

- **Source code and issues (GitHub):**  
  https://github.com/Dinuka0001/DE-Venn-Explorer
---

## Key Features

### General

- Analyze **2–5 datasets** in a single session.
- Two data input modes:
  1. **Upload DE result files** (CSV, TSV, TXT, XLSX)  
  2. **Paste pre-filtered significant gene IDs/names** (one per line, per dataset)
- Automatic detection of key columns for uploaded files:
  - Gene ID column (required)
  - Gene name column (optional)
  - Adjusted p-value (`padj`) column
  - Log2 fold-change (`log2FC`) column
- Flexible statistical filtering (file upload mode):
  - Adjusted p-value cutoff
  - Optional absolute Log2FC cutoff
- Increased upload size limit – up to **50 MB total**.

### New/Enhanced in v2.0

- **Gene direction filter**
  - Restrict overlaps to:
    - All significant genes  
    - Upregulated genes only (Log2FC > 0)  
    - Downregulated genes only (Log2FC < 0)  
  - Works together with `padj` and |Log2FC| filtering.

- **New visualization types**
  - **Euler diagrams** (`eulerr`)  
    - Area-proportional Euler diagrams as an alternative to classical Venns.
  - **UpSet plots** (`UpSetR`)  
    - For high-dimensional overlaps and when more than 3–4 sets are used.
  - **Sankey diagrams** (`networkD3`)  
    - Visualize “flow” of genes across datasets / directions with customizable colors.

- **Improved UI and workflow**
  - Sidebar reorganized into:
    - Data input (file vs paste)
    - Significance & log2FC filters
    - Diagram type & appearance
  - Diagram-specific options shown only when relevant (e.g., Sankey color settings).
  - Clear notifications for:
    - Missing/invalid columns
    - Skipped files
    - Unsupported export actions (e.g., Sankey download).

- **Enhanced exports and reporting**
  - Venn / Euler / UpSet diagrams exportable as:
    - **PNG**
    - **SVG** (where supported)
  - Structured representation of overlaps reused for:
    - Plots
    - Summary tables
    - Downloadable gene lists.

---

## Input Data

### Option 1 – Upload DE Result Files

**Supported formats**

- Text: `.csv`, `.tsv`, `.txt`  
- Excel: `.xlsx`  
  - Legacy `.xls` files are not directly supported by the current version of the app; please convert `.xls` files to `.xlsx` or `.csv`.

**Automatic handling**

- Separator detection for text files:
  - Comma, tab, semicolon.
- Sheet selection for Excel files with multiple sheets.
- Column mapping:
  - Choose **Gene ID** column (required).
  - Optionally choose **Gene name** column.
  - Choose **adjusted p-value (`padj`)** column.
  - Choose **log2 fold-change (`log2FC`)** column (for LFC-based filters and direction filters).

**Filtering**

- Set a **padj cutoff** (e.g. 0.05).
- Optionally turn on **|log2FC| cutoff** and provide a threshold (e.g. 1).
- Optionally choose a **direction filter**:
  - All significant genes
  - Upregulated only (log2FC > 0)
  - Downregulated only (log2FC < 0)

The app constructs a list of significant genes per dataset from these criteria and uses them for all downstream analyses.

---

### Option 2 – Paste Gene Lists

If you already have pre-filtered significant genes:

- Select the **“Paste gene IDs/names”** mode.
- For each dataset:
  - Provide a dataset label.
  - Paste gene IDs or names, **one per line
The app uses these pasted lists directly for:

- Venn diagram generation  
- Overlap summaries  
- Downloadable intersection gene lists  

> Note: This mode bypasses internal DE filtering (`padj` / `log2FC`) and assumes the pasted lists are already filtered.

---

## Outputs

### 1. Venn Diagrams

#### 2–3 datasets

- Rendered with **ggvenn**
- Option to display:
  - Counts only
  - Counts + percentages

#### 4–5 datasets

- Rendered with **VennDiagram**
- Counts shown for all intersection regions where possible

**Customization options**

- Set labels and label order  
- Fill colors for each set  
- Label font size  
- Count font size  
- Optional title text  

**Export formats**

- PNG  
- SVG  

---

### 2. Overlap Summary and Gene Tables

**Numeric summary for each dataset:**

- Total number of significant genes per set  
- Size of each intersection  
- Summary downloadable as **TXT**

**For each intersection** (e.g., *A ∩ B*, *A ∩ B ∩ C*):

- Interactive gene table (**DT**)
- Sorting, filtering, and searching within the table

**Export options:**

- Copy to clipboard  
- CSV  
- Excel  

---

## Required R Packages

```r
# Core dependencies
shiny
shinyjs
colourpicker
VennDiagram
ggvenn
dplyr
DT
shinyWidgets
readxl
openxlsx
tools
grid

#To install them in R:
pkgs <- c(
  "shiny", "shinyjs", "colourpicker", "VennDiagram", "ggvenn",
  "dplyr", "DT", "shinyWidgets", "readxl", "openxlsx"
)

to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install) > 0) {
  install.packages(to_install)
}
