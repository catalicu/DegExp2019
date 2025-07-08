This README.md file was updated on 07/02/2025. 

## A. Paper associated with this archive 
Title: 'Negative biodiversity effects on broad but not on narrow functions in pitcher plant microbial communities ' 
Citation: To be updated upon acceptance 

Brief abstract: The relationship between Biodiversity and Ecosystem Function (BEF) addresses how communities transform their environment. BEF relationships can have positive, neutral, or negative slopes, yet it remains unclear what conditions result in a particular slope. A popular classification in microbial ecology distinguishes broad from narrow functions and we ask whether this distinction improves predictions of BEF relationships. Specifically, we evaluate whether the relationships between broad functions and diversity can be predicted based on (1) the combined slopes from underlying narrow functions, (2) the phylogenetic breadth of associated species and (3) their ecological dominance. We assembled bacterial communities from pitcher plant fluid, using a dilution-to-extinction approach to create a gradient in biodiversity. Darlingtonia californica are carnivorous plants that depend on their bacterial community to degrade insects that supplement their nitrogen requirements. We found a negative BEF relationship between bacterial richness and degradation, while narrow functions had positive and neutral BEF slopes. The narrow functions did not predict the BEF relationship for the broader function. We identified three species statistically associated with degradation: Clostridium sp. had a positive association, while Herbinix sp. and Dyadobacter sp. had negative associations. 

## B. Author information: 
Names, institutions of all authors: To be updated upon acceptance. 

## C. Contact information
Name: To be updated upon acceptance   
Address:To be updated upon acceptance   
Email:To be updated upon acceptance 

# DATA & CODE FILE OVERVIEW
This data repository consist of data files, code scripts, and this README document, with the following data and code filenames and variables.  

## Contents and narrative
This manuscript shows broad functions can have negative BEF relationships while narrow functions can have a variety of slopes, with positive and neutral BEF slopes in our findings. 

A central figure in our manuscript shows the negative relationships between bacterial richness and degradation:  
-script for the figure  
-script for the stats  

Other files and figures reflect the remaining figures and statistics included in the manuscript:  

## Controls and supplementary materials 

## Data file descriptions
Data files are stored in the input_data folder: 
1. TAXtable_FunExp12021-03-24.txt: Taxonomy table with ASV sequence, Phylum, Class, Order, Family, Genus. It lists the full dataset with 1375 ASVs.  
2. ASVtable_FunExp12021-03-24.txt: Sample vs ASV table showing read abundances. It contains 97 samples and 1375 ASVs.  
3. METAtable_RunExp1230-03-24_sampleNamesFIX.txt: Table listing each sample's Leaf age code, Treatment code, and Replicate code. It contains 97 samples.  
4. DegExp_WeightChange_data_edits.txt: table providing the data on weight change for prey (flies) before and after the degradation assay for each sample. 
5. Degradation_16Sgene_qPCR_calculations.csv: output of qPCR estimating the 16S rRNA copy number across samples. 

Data files in the output_files folder are the result of calculations or reshaping of data from the input data folder. See the workflow below for the specific relationships:  
6. MetaDiv_table_DegExp2025-04-01.txt: diversity indexes (richness, shannon, evenness) for each sample as calculated with script XXX.  
7. glmm_funct_rich2025-07-07.txt: results of GLMs evaluating BEF between bacterial richness and each narrow function. Output of script 2 (Fig2_BEF_estimates_narrow.R).  
8. glmm_functvs2025-07-07.txt: results of GLMs evaluating narrow function - broad function relationships. Output of script 3 (Fig3_Function_correlogram.R).  

## Scripts
These scripts process data to generate the figures and statistical tests for the main text:  
1. Fig1_BEF_main. 
2. Fig2_BEF_estimates_narrow.R. 
3. Fig3_Functions_correlogram.R. 
4. Fig4_Function_ASV_heatmap.R. 
5. Fig5_NMDS. 
6. Fig6_Field_samples.R. 
These scripts process data to generate supplementary materials:   
FigS1_Dilution_diversity. 
FigS7_qPCR abund.R. 

# SOFTWARE VERSIONS
The software and package versions can be found in the [session information](https://github.com/catalicu/DegExp2019/blob/main/sessionInfo_degexp07072025.txt) file.  

# REFERENCES
See full manuscritp for list of references.  