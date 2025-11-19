# DE_Venn_Explorer
**Version 1.0**  
An interactive **R Shiny application** for visualizing and comparing **differential gene expression (DE)** overlaps across multiple datasets.  
Developed by **Dinuka Adasooriya**, Yonsei University College of Dentistry, Seoul, Korea.

---

## ✨ Features

- Upload **2–5 DE result files** (CSV, TSV, TXT, XLSX, XLS)
- Automatic detection of:
  - gene ID column  
  - gene name column  
  - padj column  
  - log2FC column  
- Apply statistical filters:
  - Adjusted p-value cutoff  
  - |Log2FC| cutoff  
- Generates:
  - **2–3 set Venn diagrams** (ggvenn)
  - **4–5 set Venn diagrams** (VennDiagram)
- Fully editable:
  - Set colors  
  - Labels  
  - Font sizes
- Download:
  - Venn diagram (PNG / SVG)
  - Overlap gene lists (CSV)
- Interactive gene tables with export (copy, CSV, Excel)
- File details, column mappings, and summary statistics

---

## 📦 Required Packages
The app automatically installs missing packages:

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

