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


# pending<-inner_join(yster <- read_excel(here("study data/myqdata.xlsx"), sheet = 5),
#                     bind_rows(read_excel(here("study data/myqdata.xlsx"), sheet = 1),
#                               read_excel(here("study data/myqdata.xlsx"), sheet = 2),
#                               read_excel(here("study data/myqdata.xlsx"), sheet = 3),
#                               read_excel(here("study data/myqdata.xlsx"), sheet = 4)),
#                     by = c("Subject ID", "Visit", "Module", "Field Name", "Query Text"))
# 
# write.xlsx(pending|>filter(`Entered By` == "Martha Ndichu"), here("urgentqueries1.xlsx"), append = TRUE, sheetName = "Martha")
# write.xlsx(pending|>filter(`Entered By` == "Joseph Weya"), here("urgentqueries1.xlsx"), append = TRUE, sheetName = "Joseph")
# write.xlsx(pending|>filter(`Entered By` == "Titus Buluku"), here("urgentqueries1.xlsx"), append = TRUE, sheetName = "Titus")
# write.xlsx(pending|>filter(`Entered By` == "Kevin Njogu"), here("urgentqueries1.xlsx"), append = TRUE, sheetName = "Kelvin")

# pending2<-inner_join(yster <- read_excel(here("study data/myqdata.xlsx"), sheet = 5),
#                                                   read_excel(here("study data/myqdata.xlsx"), sheet = 6),
#                                         by = c("Subject ID", "Visit", "Module", "Field Name", "Query Text"))
# 
# newdata <- data %>%
#   select(column1, column2, column3, column4, column5, column6, column7, column8) %>%
#   mutate(year = years(column1), month = months(column1)) %>%
#   group_by(column1, year, month,column3) %>%
#   summarize(count=count(column4), sums = sum(column5)) %>%
#   mutate(year.month = paste0(month, ":", year))
# 
# plotrix::radial.plot(newdata$sum, rp.type = "p", lwd = 3, line.col = "blue", labels = newdata$year.month, clockwise = TRUE, start = 1.5)
# plotrix::radial.plot(newdata$count, rp.type = "p", lwd = 3, line.col = "red", labels = newdata$year.month, clockwise = TRUE, start = 1.5,, add = TRUE)