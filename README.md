## README

Data for the paper Dynamics of Eastern equine encephalitis virus during the 2019 outbreak in the Northeast United States 
https://www.medrxiv.org/content/10.1101/2023.03.06.23286851v1

- genomic_data: the alignment and associated metadata. This contains newly sequenced samples, and publicly available samples from genbank.
- phylogenetic_data: raw and processed output files from BEAST analyses, and XMLs. Also includes "recorded_outbreaks.csv", used to generate starting points for skygrid covariates prior to 2003

"combined_data_nomosquito" is all of the spatial level data combined but with mosquito specific data removed until approvals come through

"recorded_outbreaks.csv" is mostly from Tricia Corrin, Rachel Ackford, Mariola Mascarenhas, Judy Greig, and Lisa A. Waddell.
Eastern Equine Encephalitis Virus: A Scoping Review of the Global Evidence.
Vector-Borne and Zoonotic Diseases.May 2021.305-320.http://doi.org/10.1089/vbz.2020.2671
but with some supplementation from references detailed in the CSV. Used to generate Skygrid covariate starting values for before 2003.

Shapefiles for maps were obtained from gadm.org

Post-2003 case data has also been removed as it was obtained from ArboNET - available on request from https://wwwn.cdc.gov/arbonet/maps/ADB_Diseases_Map/index.html

### Data for figures:
Figure one:
A) Arbonet data
B) [state]_melanura_ir.csv
C) Arbonet data
D) Arbonet data

Figure two:
A) study_dataset.csv
B) study_dataset.csv
C) study_dataset.csv
D) combined_DTA_all_states.mcc
E) DTA_all_states.jumps.csv

Figure three:
A) new_introductions.tree
B) introduction_times.csv
C) location_outbreaks.csv

Figure four:
introduction_times.csv
new_introductions.trees
location_outbreaks.csv
subtree_[XX].csv 

Figure five:
A) Arbonet data and [state]_mosquito.csv
B) combined_data.csv
C)
D)

Figure S1:
tempest_export.tsv

Figure S2:
A and B) Arbonet data and [state]_melanura_ir.csv
C) mosquito_abundance_by_week_CT_all_mosq.csv
D) combined_data.csv

Figure S3:
All of the skygrid log_files

Figure S4:
Celis-Murillo, A., Malorodova, M., and Nakash, E., 2022, North American Bird Banding Program Dataset 1960-2022 retrieved 2022-07-14: U.S. Geological Survey data release, https://doi.org/10.5066/P9BSM38F.
 
Figure S5:
Arbonet data

Figure S6: 
Model output data




