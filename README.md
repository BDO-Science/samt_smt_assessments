# samt_smt_assessments
Test code for creating and deploying SaMT and SMT assessments on SacPAS


## Smelt files

- **smelt_vars.R**: This is the file you will edit weekly with the narrative, and which actions are active. There are instructions in this file for what you need to do each week. This file filters datasets to recent data and generates variables called in the quarto doc.
- **smt-assessment-v1.qmd**: This file creates the assessment page html. Once you add any new data files and update certain fields in smelt_vars.R you should just need to render this doc. You should not need to make edits unless you want to make changes to the layout. 
- **smelt_data_extraction.R**: This file updates the data to create datasets for delta smelt and longfin smelt based off SacPAS and any data files you added. You should not need to interact with this file regularly. You will need to edit the salvage data link once salvage of ds or lfs starts (notes in the script)
- **smelt_functions.R**: A few functions to help with formatting and extracting data from spreadsheets. You should not need to interact with this file unless the data formats change.

