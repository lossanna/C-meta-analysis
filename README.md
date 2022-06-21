# C-meta-analysis
Systematic review and meta-analysis about the effects of carbon addition on fast-growing, exotic invasive plants in grasslands.

## Directory contents
data/
scripts/
RMarkdown
output_figs/  
README.md  
.gitignore  
C-meta-analysis.Rproj  

## Files to accompany publication
Raw data
- Systematic review: `data/raw/Table-S1_review.csv` with column descriptions in `data/raw/Table-S1_metadata.txt`
- Meta-analysis: `data/raw/Table-S2_meta-analysis.csv` with column descriptions in `data/raw/Table-S2_metadata.txt`

R Markdown documents
- Main figures in publication: `RMarkdown/pub_main-figs.html`
- Systematic review analysis: `RMarkdown/pub_systematic-review.html`
- Meta-analysis: `RMarkdown/pub_data-cleaning.html`
- Meta-analysis, exotic plants:
	+ `RMarkdown/pub_ex-1mod.html`
	+ `RMarkdown/pub_ex-outliers.html`
	+ `RMarkdown/pub_ex-modsel.html`
- Meta-analysis, native plants:
	+ `RMarkdown/pub_nt-1mod.html`
	+ `RMarkdown/pub_nt-outliers.html`
	+ `RMarkdown/pub_nt-modsel.html`