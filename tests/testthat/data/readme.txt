SCB Statistik för Painkiller


get_scb_data.R 

- downloads data from SCB and saves data in JSON files
- each data source has an input and output json file, the latter with a _data ending
- combines the data to a single dataset ../scb.Rds
- alternatively the saved *_data.json data files can be used to generate data