# DE Venn Explorer
**Version 1.1**  
An interactive **R Shiny application** for visualizing and comparing **differential gene expression (DE)** overlaps across multiple datasets.  
Developed by **Dinuka Adasooriya**, Yonsei University College of Dentistry, Seoul, Korea.

---

## 🔗 Access

- **GitHub repository** (source code, issues, and development):  
  https://github.com/Dinuka0001/DE_Venn_Explorer.git

- **Online app (Posit Cloud)** – run DE Venn Explorer directly in your browser:  
  https://dinuka-de-venn-explorer.share.connect.posit.cloud

---

## ✨ Features

- Analyze **2–5 datasets** in one session
- Two data input modes:
  - **Upload DE result files** (CSV, TSV, TXT, XLSX, XLS)
  - **Paste pre-filtered significant gene IDs/names** (one per line, per dataset)
- Automatic detection of (for file uploads):
  - Gene ID column  
  - Gene name column (optional)  
  - Adjusted p-value (padj) column  
  - Log2 fold-change (log2FC) column  
- Apply statistical filters (file upload mode):
  - Adjusted p-value cutoff  
  - |Log2FC| cutoff (optional toggle)  
- Robust file handling:
  - Automatic separator detection for text files (comma, tab, semicolon)
  - Excel sheet selection for multi-sheet XLSX/XLS files
  - Helpful guidance for problematic **.xls** files (recommend converting to .xlsx or .csv)
  - Increased upload size limit (**up to 50 MB** total)
- Generates:
  - **2–3 set Venn diagrams** (ggvenn)
  - **4–5 set Venn diagrams** (VennDiagram)
- Fully editable Venn diagrams:
  - Set labels  
  - Colors  
  - Font sizes for labels and counts  
  - Optional percentage display for 2–3 set diagrams
- Summary and gene-level views:
  - Overlap summary:  
    - Number of genes per set  
    - Intersection counts  
    - Downloadable summary (TXT)
  - Interactive gene tables for each overlap with export (copy, CSV, Excel)
- Download:
  - Venn diagram (PNG / SVG)
  - Overlap gene lists (CSV)
- Additional information:
  - File details, column mappings, and summary statistics
  - “About” tab with app and package information

---

## 📂 Input Data

### Option 1 – Upload DE Result Files

Supported formats:
- **Text**: `.csv`, `.tsv`, `.txt`  
- **Excel**: `.xlsx`, `.xls`

Automatic handling:
- Separator detection (comma, tab, semicolon) for text files  
- Sheet selection dialog for Excel files  
- Column mapping interface:
  - Choose gene ID column (required)  
  - Optionally choose gene name column  
  - Choose adjusted p-value (`padj`) column  
  - Choose log2 fold-change (`log2FC`) column

Filtering:
- Specify **padj cutoff** (e.g. 0.05)  
- Optionally enable **|log2FC| cutoff** and provide threshold (e.g. 1)  

The app then extracts the list of **significant genes** for each dataset based on these criteria and uses them for overlap analysis and Venn diagram generation.

### Option 2 – Paste Gene Lists

If you already have pre-filtered lists of significant genes:

- Select the **“Paste gene lists”** mode
- For each dataset:
  - Paste one gene ID or gene symbol **per line**
  - Optionally provide a **dataset label**
- The app uses these pasted lists directly for:
  - Venn diagram generation  
  - Overlap summaries  
  - Downloadable intersection gene lists

---

## 📊 Outputs

### Venn Diagrams

- **2–3 datasets**:
  - Drawn using **ggvenn**
  - Option to display **counts only** or **counts + percentages**
- **4–5 datasets**:
  - Drawn using **VennDiagram**
  - Counts displayed for all intersection regions where possible

Customization options:
- Set labels and label order  
- Fill colors for each set  
- Label font size  
- Count font size  
- Optional title text

You can download Venn diagrams as:
- **PNG**
- **SVG**

### Overlap Summary & Gene Tables

- Numeric summary for each dataset:
  - Total number of significant genes per set  
  - Size of each intersection
- Summary downloadable as **TXT**
- For each intersection (e.g. A ∩ B, A ∩ B ∩ C):
  - Interactive gene table (DT)
  - Sorting and searching within the table
  - Export gene lists as:
    - Copy to clipboard  
    - CSV  
    - Excel

---

## 📦 Required R Packages

```r
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

## To install them in R:
```r
pkgs <- c(
  "shiny", "shinyjs", "colourpicker", "VennDiagram", "ggvenn",
  "dplyr", "DT", "shinyWidgets", "readxl", "openxlsx"
)

to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(to_install) > 0) {
  install.packages(to_install)
}


## How to Run Locally
# 1. Install required packages (see above)
# 2. Install or clone the app

# From GitHub (if using remotes or devtools):
# remotes::install_github("Dinuka0001/DE_Venn_Explorer")

# Or clone manually and set working directory to the app folder, then:
shiny::runApp("path/to/DE_Venn_Explorer")
