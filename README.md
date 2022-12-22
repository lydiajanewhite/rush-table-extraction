# rush-table-extraction
extracting and summarising species abundance data for response diversity meta analysis database

for handful of studies (1042, 1334, 179, 452, 809) pivoting data from wide to long format resulted in NAs, so convert these to zeros in import scripts, after verifying in original papers that they should be zeros. 

manually edited sd NAs for study 2629 lady beetle - checked against original data set in paper 


data originating from scatter plots do not have error (SE or SD), data from tables where samples are all zeros, have SE/SD of NA, data with n = 1 have SE of NA. 

scatterplot data (study 1970) already in processed form (i.e. effect size plotted against effect size) but study 2877 needs formatting 

to do:
1. but sometimes SD of 0 recorded as zero in table not NA - check with Ian. 
2. check all studies which give no error! and verify that these have been recorded as NAs. 
and for error type have NA. 
4. format LRR data 

