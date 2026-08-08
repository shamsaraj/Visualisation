# Visualisation

R scripts for plotting ROC curves and computing enrichment factors from
virtual screening / docking results (actives vs. decoys).

## Requirements

```r
install.packages(c("ROCR", "enrichvs"))
```

## Input format

Both scripts expect `actives.csv` and `decoys.csv`: semicolon-separated,
with a header row and one score column per scoring function/target.

## Scripts

### `roc_curve_multi_target.R`

Overlays ROC curves for several targets on a single plot and reports the
AUC for each. Edit the `targets` list at the top of the file to point at
your own `actives.csv` / `decoys.csv` pairs, then source the script in R.
Produces `cross.pdf`.

### `roc_curve_enrichment.R`

For a single target, loops over score columns (defined in `header`),
computing AUC and enrichment factors (EF at 100%/20%/10%/2%/1%/0.2%/0.1%)
via `enrichvs`, and plotting a per-column ROC curve via `ROCR`. Edit
`working_dir`, `actives_file`, `decoys_file`, and `header` at the top of
the file, then source it in R. Appends results to `output.txt` and writes
one PDF per score column.
