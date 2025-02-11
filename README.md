# MODFLOW Cell-by-Cell Budget Extractor
## Overview
This program reads a MODFLOW binary Cell-by-Cell (CBB) compact budget file and extracts accumulated flow values for a specified budget term (e.g., "STORAGE") over a range of stress periods. The output is written to a structured text file for further analysis.

## Features
* Reads and processes MODFLOW CBB files in compact budget format.
* Extracts net flow values for a specified stress period range.
* Supports multiple budget terms (e.g., "STORAGE", "WELLS", "RECHARGE", "STREAM LEAKAGE").
* Outputs results in a structured table format, with row, column, layer indices, by **stress period**.

---

## Usage
### Included Windows Executables
Two version of the executable are included in the [Bin](./Bin/) folder:
* `MF_CBC_Extractor.exe`
* `MF_CBC_Extractor_dbl.exe`
The second executable is compiled with all real variables in double precision. If your version of MODFLOW was also compiled in double precision, then you should use this executable. If you're not sure what precision your MODFLOW version was compiled with, then try the single-precision version first (MF_CBC_Extractor.exe). If it crashes or reports wacky numbers on screen, then your MODFLOW version was likely compiled with double precision.

### Command-line Arguments
The program requires the following arguments:

```bash
MF_CBC_Extractor [CBB Filename] [Start SP] [End SP] [Target Label]
```
* `[CBB Filename]` The name of the MODFLOW CBB file.
* `[Start SP]` The first stress period to accumulate from.
* `[End SP]` The last stress period to accumulate (not inclusive).
* `[Target Label]` The budget term to extract (e.g., "STORAGE").
### Example Usage
Extracting STORAGE changes from stress period 2 to 10:
```bash
MF_CBC_Extractor model.cbb 2 10 STORAGE
```

---

## Output File Format
The program writes an output file named: `MF_CBC_<Target Label>.out`
For example, if extracting "STORAGE", the output will be: **MF_CBC_STORAGE.out**
## File Structure
Each row represents a model grid cell and includes:

1. **LAYER** - Layer index
2. **ROW** - Row index
2. **COLUMN** - Column index
4. **SP_X** - Storage change for stress period X
5. **TOTAL** - Sum of storage change over all specified stress periods
**Example Output Format**
```
LAYER      ROW    COLUMN              SP_2          SP_3          SP_4         TOTAL        
    1        1         1         -0.000123     -0.000256     -0.000367     -0.000746
    1        1         2         -0.000145     -0.000278     -0.000390     -0.000813
...
  SUM        -         -         -5.231412     -4.892345     -6.210987    -16.334744
```
* The **SUM** row at the bottom represents the total storage change summed across all cells for each stress period.

### MODFLOW Budget Interpretation
* A positive value means the cell gained (e.g., for STORAGE: water stored).
* A negative value means the cell lost (e.g., for STORAGE: water removed).

---

## Assumptions & Limitations
* The CBB file must be in compact budget format.
* Single-precision is assumed in the CBB file.
* The output file structure assumes MODFLOW structured grids (NROW, NCOL, NLAY).
* No auxiliary variables are handled.
* Only one budget term is extracted per run. To extract multiple terms, run the program separately for each term.
* Stress periods are assumed to be sequential in the CBB file.
