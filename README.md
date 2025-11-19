# DE Venn Explorer
**Version 2.0**  
An interactive **R Shiny application** for visualizing and comparing **differential gene expression (DE)** overlaps across multiple datasets.  
Developed by **Dinuka Adasooriya**, Yonsei University College of Dentistry, Seoul, Korea.

---

## ✨ Features

- Analyze **2–5 datasets** in one session
- Two data input modes:
  - **Upload DE result files** (CSV, TSV, TXT, XLSX, XLS)
  - **Paste pre-filtered significant gene IDs/names** (one per line, per dataset)
- Automatic detection of (for file uploads):
  - gene ID column  
  - gene name column (optional)  
  - adjusted p-value (padj) column  
  - log2 fold-change (log2FC) column  
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

## 📦 Required Packages

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
