### Load required packages to connect to a MySQL DB
pacman::p_load(DBI,readxl,here,tidyverse,dplyr,xlsx,install=TRUE, update = getOption("pac_update"))
### Update credentials to allow access to KIDMS DB
username<-read.table("C:/Users/CMUIRURI/OneDrive - Kemri Wellcome Trust/Desktop/usernamesql.txt")
mysqlconnection = dbConnect(RMySQL::MySQL(),
                            dbname=username[1,1],
                            host=username[2,1],
                            port=as.numeric(username[3,1]),
                            user=username[4,1],
                            password=username[5,1])

### Run query to return Haematology and Biochemistry results for VAC078
## used left join + union + right join as SQL lacks a dplyr::full_join capability
result2 = dbGetQuery(mysqlconnection, "SELECT rh.serial_study_id,rh.date_collect,rh.wbc,rh.rbc,rh.hgb,rh.hct,rh.mcv,rh.plt,rh.ne,rh.ly,rh.mo_1,rh.eo_1,rh.ba_1,rh.ne_1,rh.ly_1,rcc.ALT,rcc.T_Bilirubin,rcc.Plasma_Creatinine FROM kidms.report_haematology rh 
                     LEFT JOIN kidms.report_clinical_chemistry rcc ON rh.serial_study_id=rcc.serial_study_id AND rh.date_collect=rcc.date_collect WHERE rh.fk_study = 266 UNION 
                     SELECT rh.serial_study_id,rh.date_collect,rh.wbc,rh.rbc,rh.hgb,rh.hct,rh.mcv,rh.plt,rh.ne,rh.ly,rh.mo_1,rh.eo_1,rh.ba_1,rh.ne_1,rh.ly_1,rcc.ALT,rcc.T_Bilirubin,rcc.Plasma_Creatinine FROM kidms.report_haematology rh 
                     RIGHT JOIN kidms.report_clinical_chemistry rcc ON rh.serial_study_id=rcc.serial_study_id AND rh.date_collect=rcc.date_collect WHERE rh.fk_study = 266")
## make my data workable with the dataset retrieved from eSource
mydata<-result2|>
  setNames(c("Subject","sampledate","WBC Count","RBC Count","Haemoglobin","Haematocrit","MCV","Platelet Count","Neutrophils (%)","Lymphocytes (%)","Monocytes","Eosinophils","Basophils","Neutrophils","Lymphocytes","ALT","Total Bilirubin","Creatinine"))|>
  tidyr::pivot_longer(cols = -c("Subject","sampledate"), names_to = "Test", values_to = "Result")
## store kidms dataset in the folder where the shiny app runs for easier pulling
write.csv(mydata,here("study data/kidmsresults.csv"))
