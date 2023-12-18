pacman::p_load(tidyverse,shinyauthr,stringi,stringr,tinytex, Hmisc,ggiraph,finalfit, openxlsx,webshot, janitor, dplyr, table1, knitr, xlsx, here, psych, boot, ggconsort,readxl, lubridate, ggplot2,flextable, kableExtra, formattable, scales, data.table,writexl, scales,shinyWidgets, shiny,DT,shinythemes,install = T, update = getOption("pac_update"))

## work your data
###++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
###### Functions########
convert_to_factors <- function(data) {
  mutate(data,
         across(where(~!is.Date(.)),
                ~factor(.x, levels = unique(.x)%>% sort(), exclude = NULL)))
}


fill_blanks<- function(data){
  mutate_if(data,is.character,~na_if(., ''))
}


minpositive = function(x) min(x[x >= 0], na.rm = TRUE)

#####
# consortdataset 

vacdata <- suppressWarnings(fread(here("study data/Study Vaccination Listing.csv")))|>
  mutate(`Date of vaccination`=as.Date(`Date of vaccination`, "%d-%b-%Y"))|>fill_blanks()
screenID <- left_join(left_join(
  left_join(
    # Screening dataset
    screenIoD<-screenD<-suppressWarnings(fread(here("study data/Eligibility Assessment Listing.csv")))|>fill_blanks()|>
      setNames(c("Site ID","Subject","VISIT","Date eligibility assessed","Does subject meet all inclusion criteria?","age_eligible","sign_consent","comply_protocol","residence_remain","Does subject meet any exclusion criteria?","prev_mal_vac","other_mal_study","allergy","anaphylaxis","congenital_def","anemia","blood_trans","immunoglobulin","malnourished","chronic_illnes","hiv_3_4","vac_1mo","other_trial","disorder","other","Describe","Does the Subject meet all eligibility criteria?","First Data Time","Last Data Time")),
    ##filter_per_vac
    # Vaccinations dataset
    newfupdata<-vacdata|>
      select(2,3)|>
      pivot_wider(names_from = "VISIT", values_from = "VISIT")|>
      select(1,4,6,2,3,5), by="Subject",relationship = "many-to-many"),
  # Follow up status dataset
  fupdata<- suppressWarnings(fread(here("study data/participants.csv")))|>fill_blanks()|>
    select(screenId, group,currentStatus,Site,13:16)|>
    #mutate(screenId = as.character(screenId))|>
    dplyr::rename(Subject = "screenId"), by="Subject",relationship = "many-to-many"),
  # Get randomization number
  rnddata<- suppressWarnings(fread(here("study data/Visit Completed v_s SDV.csv")))|>fill_blanks()|>
    filter(VISITNAME=="Screening")|>
    distinct(`Subject Id`, .keep_all = TRUE)|>
    select(2,3)|>
    setnames(c("Subject","Rand"))|>
    mutate_if(is.character,~na_if(., '')),by="Subject")

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
## define your consort cohorts using ggconsort's cohort_start()
study_cohorts <- screenID |>
  cohort_start("Screened")|>
  cohort_define(
    screened = .full |> filter(VISIT != 1),
    prevc_mal_vac = screened |> filter(prev_mal_vac == "Yes"&is.na(Rand)),
    anti1 = anti_join(screened, prevc_mal_vac, by = "Subject"),
    congenital_def = anti_join(anti1, prevc_mal_vac, by = "Subject") |> filter(congenital_def == "Yes"&is.na(Rand)),
    anti2 = anti_join(anti1, congenital_def, by = "Subject"),
    malnourished = anti_join(anti2, congenital_def, by = "Subject") |> filter(malnourished == "Yes"&is.na(Rand)),
    anti3= anti_join(anti2, malnourished, by = "Subject"),
    chronic_illness = anti_join(anti3, malnourished, by = "Subject") |> filter(chronic_illnes == "Yes"&is.na(Rand)),
    anti4 = anti_join(anti3, chronic_illness, by = "Subject"),
    vac_1mo = anti_join(anti4, chronic_illness, by = "Subject") |> filter(vac_1mo == "Yes"| grepl('vaccine', Describe)&is.na(Rand)),
    anti5 = anti_join(anti4, vac_1mo, by = "Subject"),
    disorder = anti_join(anti5, vac_1mo, by = "Subject") |> filter(disorder == "Yes"| grepl('HIV', Describe)&is.na(Rand)),
    anti6 = anti_join(anti5, disorder, by = "Subject"),
    Describe = anti_join(anti6, disorder, by = "Subject") |> filter(other=="Closure of enrolment"&is.na(Rand)),
    anti7 = anti_join(anti6, Describe, by = "Subject"),
    Described = anti_join(anti7, Describe, by = "Subject") |> filter(other=="Consent withdrawal"&is.na(Rand)),
    Randomized = anti_join(anti7, Described, by = "Subject"),
    excluded = anti_join(screened, Randomized, by = "Subject"),
    vacc1 = screened |> filter(`Dose 1` == "Dose 1"),
    vacgrpA = vacc1 |> filter(Subject <= 785100331),
    vacgrpB = vacc1 |> filter(Subject > 785100331),
    enr = vacc1,
    groupAenr = vacgrpA,
    groupBenr = vacgrpB,
    recvac1 = vacc1,
    vacc2 = screened |> filter(`Dose 2` == "Dose 2"),
    vac2grpA = vacc2 |> filter(Subject <= 785100331),
    vac2grpB = vacc2 |> filter(Subject > 785100331),
    vacc3 = screened |> filter(`Dose 3` == "Dose 3"),
    vac3grpA = vacc3 |> filter(Subject <= 785100331),
    vac3grpB = vacc3 |> filter(Subject > 785100331),
    vacc4 = screened |> filter(`Booster` == "Booster"),
    vac4grpA = vacc4 |> filter(Subject <= 785100331),
    vac4grpB = vacc4 |> filter(Subject > 785100331),
    vacc5 = screened %>% filter(`2B0 Vaccination` == "2B0 Vaccination"),
    vac5grpA = vacc5 %>% filter(Subject <= 785100331),
    vac5grpB = vacc5 %>% filter(Subject > 785100331),
    rand_no_enr = anti_join(Randomized, vacc1, by = "Subject"),
    on_active_fu = screened|> filter(currentStatus == "On Active Follow up"),
    activeA = on_active_fu |> filter(Subject <= 785100331),
    activeB = on_active_fu |> filter(Subject > 785100331),
    pi_discretion = screened|> filter(currentStatus == "Terminated due to PI discretion"),
    consent_withdrawal = screened|> filter(currentStatus == "Consent withdrawal"),
    blood_volume = screened|> filter(currentStatus == "Blood volume Issues"),
    vacerror = screened|> filter(currentStatus == "Terminated due to dosing error"),
    ltfup = screened|> filter(currentStatus == "Loss to follow up"),
    ltfA = ltfup |> filter(Subject <= 785100331),
    ltfB = ltfup |> filter(Subject > 785100331),
    saedeath = screened|> filter(currentStatus == "SAE resulting to death"),
    withdrawal_db = rbind(consent_withdrawal,blood_volume,vacerror,pi_discretion,saedeath),
    withdrawalA = withdrawal_db |> filter(Subject <= 785100331),
    withdrawalB = withdrawal_db |> filter(Subject > 785100331),
    consentv6_yes = screened|>filter(consentV6_0Accept=="Yes"),
    v6A = consentv6_yes %>% filter(Subject <= 785100331),
    v6B = consentv6_yes %>% filter(Subject > 785100331),
    consentv6_no = screened|>filter(consentV6_0Accept=="No"),
    consentv6_yet = screened|>filter(is.na(consentV6_0Accept)& currentStatus=="On Active Follow up"),
    consentv7_yes = screened|>filter(consentV7_0Accept=="Yes"),
    v7A = consentv7_yes %>% filter(Subject <= 785100331),
    v7B = consentv7_yes %>% filter(Subject > 785100331),
    consentv7_no = screened|>filter(consentV7_0Accept=="No"),
    consentv7_yet = screened|>filter(is.na(consentV7_0Accept) & currentStatus=="On Active Follow up")
  )|>
  cohort_label(
    screened = "screened",
    prevc_mal_vac = "Previous Malaria Vaccination",
    congenital_def = " Has Major congenital defects",
    malnourished = "Malnourished and requiring admission",
    chronic_illness = "Clinically significant Acute illnesses",
    vac_1mo = "Received a Vaccine within 30 days",
    disorder = "Significant Disease Disorder",
    Describe = "Closure of enrolment",
    Described = "Consent withdrawal pre-randomization",
    Randomized = "Randomized",
    excluded = "Excluded",
    vacc1 = "Enrolled",
    enr = "Enrolled participants",
    groupAenr = "Group A enrolled",
    groupBenr = "Group B enrolled",
    recvac1 = "Received Vacc1",
    vacgrpA = "Vac1 Group A",
    vacgrpB = "Vac1 Group B",
    vac2grpA = "Vac2 Group A",
    vac2grpB = "Vac2 Group B",
    vac3grpA = "Vac3 Group A",
    vac3grpB = "Vac3 Group B",
    vac4grpA = "Booster Group A",
    vac4grpB = "Booster Group B",
    vacc2 = "Received Vacc2",
    vacc3 = "Received Vacc3",
    vacc4 = "Received Booster",
    vacc5 = "Received 2nd Booster",
    vac5grpA = "2B0 Group A",
    vac5grpB = "2B0 Group B",
    rand_no_enr = "Randomized but not enrolled",
    on_active_fu = "On follow up",
    activeA = "Group A active",
    activeB = "Group B active",
    withdrawal_db = "Withdrawals",
    ltfA = "Group A LTF",
    ltfB = "Group B LTF",
    withdrawalA = "Group A Withdrawals",
    withdrawalB = "Group B Withdrawals",
    pi_discretion = "Moved outside study area",
    consent_withdrawal = "Known Consent withdrawals",
    blood_volume = "Issues with Blood volume",
    vacerror = "Dosing error termination",
    ltfup = "Loss to follow up",
    saedeath = "SAE resulting to death",
    consentv6_yes = "Consent V6 Accept",
    v6A = "V6 Group A",
    v6B = "V6 Group B",
    consentv6_no = "Consent V6 Decline",
    consentv6_yet = "Consent V6 Pending",
    consentv7_yes = "Consent V7 Accept",
    v7A = "V7 Group A",
    v7B = "V7 Group B",
    consentv7_no = "Consent V7 Decline",
    consentv7_yet = "Consent V7 Pending"
  )######## Get the list of the summaries obtained #########
#summary(study_cohorts)
########################## use ggplot2 to generate the consort diagram #############
study_consort <- study_cohorts |>
  consort_box_add("full", 0,70, cohort_count_adorn(study_cohorts, .full)) |> ### Adds a box
  consort_box_add("exclusions", 20, 64, glue::glue(
    '{cohort_count_adorn(study_cohorts, excluded)}<br> 
  . {cohort_count_adorn(study_cohorts, prevc_mal_vac)}<br> 
  . {cohort_count_adorn(study_cohorts, congenital_def)}<br> 
  . {cohort_count_adorn(study_cohorts, chronic_illness)}<br> 
  . {cohort_count_adorn(study_cohorts, vac_1mo)}<br> 
  . {cohort_count_adorn(study_cohorts, disorder)}<br> 
  . {cohort_count_adorn(study_cohorts, Described)}<br> 
  . {cohort_count_adorn(study_cohorts, Describe)}<br> 
  . {cohort_count_adorn(study_cohorts, malnourished)}'))|>
  consort_box_add("Randomized", 0, 59, cohort_count_adorn(study_cohorts, Randomized))|>
  consort_box_add("on_active_fu", 50, 42, glue::glue(
    '<b>{cohort_count_adorn(study_cohorts, recvac1)}</b><br> 
  . {cohort_count_adorn(study_cohorts, vacgrpA)}<br> 
  . {cohort_count_adorn(study_cohorts, vacgrpB)}<br> 
  <b>{cohort_count_adorn(study_cohorts, vacc2)}</b><br> 
  . {cohort_count_adorn(study_cohorts, vac2grpA)}<br> 
  . {cohort_count_adorn(study_cohorts, vac2grpB)}<br> 
  <b>{cohort_count_adorn(study_cohorts, vacc3)}</b><br>
  . {cohort_count_adorn(study_cohorts, vac3grpA)}<br> 
  . {cohort_count_adorn(study_cohorts, vac3grpB)}<br> 
  <b>{cohort_count_adorn(study_cohorts, vacc4)}</b><br>
  . {cohort_count_adorn(study_cohorts, vac4grpA)}<br> 
  . {cohort_count_adorn(study_cohorts, vac4grpB)}<br> 
  <b>{cohort_count_adorn(study_cohorts, vacc5)}</b><br>
  . {cohort_count_adorn(study_cohorts, vac5grpA)}<br> 
  . {cohort_count_adorn(study_cohorts, vac5grpB)}'))|>
  consort_box_add("vacc1", 0, 54, glue::glue(
    '{cohort_count_adorn(study_cohorts, vacc1)}<br>
  {cohort_count_adorn(study_cohorts, on_active_fu)}<br>
  {cohort_count_adorn(study_cohorts, ltfup)}<br>
  {cohort_count_adorn(study_cohorts, withdrawal_db)}<br> 
  . {cohort_count_adorn(study_cohorts, consent_withdrawal)}<br> 
  . {cohort_count_adorn(study_cohorts, pi_discretion)}<br> 
  . {cohort_count_adorn(study_cohorts, blood_volume)}  
  . {cohort_count_adorn(study_cohorts, vacerror)}<br>
  . {cohort_count_adorn(study_cohorts, saedeath)}'))|>
  consort_box_add("enr", -50, 42, glue::glue(
    '<b>{cohort_count_adorn(study_cohorts, enr)}</b><br>
  .{cohort_count_adorn(study_cohorts, groupAenr)}<br>
  .{cohort_count_adorn(study_cohorts, groupBenr)}<br>
  <b>{cohort_count_adorn(study_cohorts, on_active_fu)}</b><br> 
  . {cohort_count_adorn(study_cohorts, activeA)}<br> 
  . {cohort_count_adorn(study_cohorts, activeB)}<br> 
  <b>{cohort_count_adorn(study_cohorts, withdrawal_db)}</b><br> 
  . {cohort_count_adorn(study_cohorts, withdrawalA)}<br> 
  . {cohort_count_adorn(study_cohorts, withdrawalB)}<br>
  <b>{cohort_count_adorn(study_cohorts, ltfup)}</b><br> 
  . {cohort_count_adorn(study_cohorts, ltfA)}<br> 
  . {cohort_count_adorn(study_cohorts, ltfB)}'))|>
  consort_box_add("reconsent", -2, 42, glue::glue(
    '<b>{cohort_count_adorn(study_cohorts, consentv7_yes)}</b><br>
  .{cohort_count_adorn(study_cohorts, v7A)}<br>
  .{cohort_count_adorn(study_cohorts, v7B)}<br>
    {cohort_count_adorn(study_cohorts, consentv7_no)}<br> 
    {cohort_count_adorn(study_cohorts, consentv7_yet)}<br>
    <b>{cohort_count_adorn(study_cohorts, consentv6_yes)}</b><br>
  .{cohort_count_adorn(study_cohorts, v6A)}<br>
  .{cohort_count_adorn(study_cohorts, v6B)}'))|>
  consort_box_add("rand_no_enr", 20, 56, cohort_count_adorn(study_cohorts, rand_no_enr))|>
  consort_line_add(start_x = -50 , start_y =44 , end_x =50 ,end_y =44 )|>
  consort_arrow_add(
    end = "exclusions", end_side = "left", start_x = 0, start_y = 64)|>
  consort_arrow_add(
    end = "rand_no_enr", end_side = "left", start_x = 0, start_y = 56)|>
  consort_arrow_add(start = "vacc1" , start_side ="bottom" , end_x =0 ,end_y =44)|>
  consort_arrow_add("full", "bottom", "Randomized", "top")|>
  consort_arrow_add(start_x = -50 , start_y =44 , end = "enr", end_side = "top")|>
  consort_arrow_add(start_x = 50 , start_y =44 , end = "on_active_fu",end_side = "top")|>
  consort_arrow_add(start_x = -2 , start_y =44 , end = "reconsent",end_side = "top")|>
  consort_arrow_add("Randomized", "bottom", "vacc1", "top")##### Adds an arrow

### End of study
requiredEos<-bind_rows(
  wqe<-study_cohorts|>
    cohort_pull(excluded)|>
    mutate(currentStatus="Excluded before rand"),
  rand_fail<-study_cohorts|>
    cohort_pull(rand_no_enr)|>
    mutate(currentStatus="Randomized not enrolled"),
  truendofstudy<-bind_rows(losttofup<-study_cohorts|>
                             cohort_pull(ltfup),
                           vwithdrawae<-study_cohorts|>
                             cohort_pull(withdrawal_db)))|>
  mutate(time=case_when(is.na(Site)~"Before Enr",TRUE~"After Enr"))|>
  select(2,34,25,35,36,43)|>
  arrange(group,other,Subject)

sumaryreq<-requiredEos|>
  group_by(currentStatus,time)|>
  summarise(freq=n(), .groups = 'drop')|>
  pivot_wider(names_from = time,values_from = freq)|>
  arrange(`Before Enr`,desc(`After Enr`))|>
  adorn_totals(c('row','col'))|>
  flextable()|>autofit()

completedEos<-suppressWarnings(fread(here("study data/End Of Study Listing.csv")))|>fill_blanks()|>
  filter(!is.na(`Protocol completion or discontinuation date`))

pending_completion<-anti_join(requiredEos,completedEos,by="Subject")|>
  arrange(group,currentStatus,Subject)
completed_error<-anti_join(completedEos,requiredEos,by="Subject")

#############+++++++++++++++++++++++++++++++++++++++++++++++++++++++###
#############+Scheduler table
###### unscheduled visits
dateofvisit<- suppressWarnings(fread(here("study data/Date Of Visit Listing.csv"),colClasses = c("text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text")))|>fill_blanks()|>
  mutate(`Visit date`=as.Date(`Visit date`,"%d-%b-%Y"),
         `First Data Time`=as.Date(`First Data Time`,"%d-%b-%Y %H:%M"),
         `Last Data Time`=as.Date(`Last Data Time`,"%d-%b-%Y %H:%M"))
all_unscheduled<-dateofvisit|>
  filter(grepl('Unscheduled', VISIT))##grep() for 'Pattern Matching and/or Replacement' 

unscheduled_this<-all_unscheduled|>
  filter(`Visit date`>= today()-7 & `Visit date`< today())
### Scheduled visits
scheduler<-suppressWarnings(fread(here("study data/group_schedule_village.csv"),
                                  colClasses = c("numeric","text","date","date","date",
                                                 "date","date","text","text","text","text")))|>fill_blanks()
scheduler$visitCode<-factor(scheduler$visitCode, levels = c("Day 14", "Day 28", "Day 42","Day 56", "Day 70",
                                                            "Day 84", "Day 140", "Day 196", "Day 236", 
                                                            "Day 292", "Day 348", "Day 404", "Day B0", 
                                                            "Day B14", "Day B28", "Day B84", "Day B140", 
                                                            "Day B168", "Day B224", "Day B280", "Day B336","Reconsent",
                                                            "Day B365/2B0","Day 2B14","Day 2B28","Day 2B180",
                                                            "Day 3B0","Day 3B14","Day 3B28","Day 3B180","Day 3B365"))
scheduler$windowOpen<-as.Date(scheduler$windowOpen, "%d %m %Y")
scheduler$scheduledDate<-as.Date(scheduler$scheduledDate, "%d %m %Y")
scheduler$rescheduledDate<-as.Date(scheduler$rescheduledDate, "%d %m %Y")
scheduler$exactDate<-as.Date(scheduler$exactDate, "%d %m %Y")
scheduler$windowClose<-as.Date(scheduler$windowClose, "%d %m %Y")

sumreacto<-scheduler|>
  distinct(screenID, .keep_all = TRUE)|>
  group_by(Site,group)|>
  dplyr::summarize(count=n(), .groups='keep')|>
  pivot_wider(names_from = group, values_from = count)|>
  adorn_totals(c("row","col"))|>
  flextable()|>hline()|>autofit()

scheduler$Site[scheduler$Site == "Chasimba"] = "Pingilikani"

reschedule<-scheduler|>
  filter(!is.na(rescheduledDate))

actualer<- scheduler|>
  filter(!is.na(exactDate))
actual<-actualer|>
  select(screenID, visitCode)|>
  group_by(visitCode)|>
  dplyr::summarize('actual'=n(), .groups='keep')|>
  select(visitCode,'actual')


actual_percentages <- actual|>
  mutate('Actual.vs.enrolled'= scales::percent((`actual`/603),accuracy=1))|>
  select(visitCode, 'Actual.vs.enrolled')

misseds<- scheduler|>
  filter(windowClose<today() & is.na(exactDate) & scheduledDate<today() )

missed<-misseds|>
  select(screenID, visitCode)|>
  group_by(visitCode)|>
  dplyr::summarize('Missed'=n(), .groups='keep')|>
  select(visitCode,'Missed')


LFUer<- scheduler|>
  filter(scheduledDate<=today(),is.na(exactDate), windowClose>=today())

LFU<-LFUer|>
  select(screenID, visitCode)|>
  group_by(visitCode)|>
  dplyr::summarize('Due for visit'=n(), .groups='keep')|>
  select(visitCode,'Due for visit')

all_scheduleder<- scheduler|>
  filter(scheduledDate<=today() | !is.na(exactDate))
all_scheduled<-all_scheduleder|>
  select(screenID, visitCode)|>
  group_by(visitCode)|>
  dplyr::summarize('scheduled'=n(), .groups='keep')|>
  select(visitCode,'scheduled')


tabler<-left_join(all_scheduled,actual, by = "visitCode",relationship = "many-to-many")
schedulertablet<-left_join(tabler,LFU, by = "visitCode",relationship = "many-to-many")
schedulertabler<-left_join(schedulertablet,missed, by = "visitCode",relationship = "many-to-many")
schedulertable<-left_join(schedulertabler,actual_percentages, by = "visitCode",relationship = "many-to-many")|>
  adorn_totals(c("row"))|>
  mutate('Success rate'= scales::percent(`actual`/`scheduled`, accuracy = 0.1))

scheduler12<-scheduler|>
  mutate(scheduleDate=coalesce(rescheduledDate,scheduledDate))|>
  select(1:3,12,6:11)|>
  mutate(scheduledvis=case_when(as.Date(scheduleDate)<=today()~"Scheduled", TRUE~NA),
         actualvis=case_when(!is.na(exactDate)~"Actual", TRUE~NA),
         duevis=case_when(as.Date(scheduleDate)<=today()&is.na(exactDate)&as.Date(windowClose)>today()~"Due visit", TRUE~NA),
         misvis=case_when(is.na(exactDate)&as.Date(windowClose)<=today()~"Missed", TRUE~NA))|>
  select(1,2,9,11:14)|>
  filter(!is.na(scheduledvis))|>
  pivot_longer(cols=c("scheduledvis","actualvis","duevis","misvis"),values_to ="status",names_repair = "minimal")|>
  filter(!is.na(status))|>
  mutate(status=factor(status, levels = c("Scheduled","Actual","Due visit","Missed")))

####====================QUERIES and incomplete records=====================================####
queries_nowe<-suppressWarnings(fread(paste0(here(),"/study data/QueryReports.csv")))|>fill_blanks()|>as.data.frame()
queries_now<-queries_nowe|>
  dplyr::rename(type="Query Type",
                status="Query Status")
queries<-queries_now|>
  group_by(status, type)|>
  summarise(count=n(), .groups = 'drop')|>
  pivot_wider(names_from=type, values_from=count)|>
  arrange(factor(status, levels = c('Resolved', 'Data Deleted', 'CDM Override', 'Answered', 'Open Query')))|>
  adorn_totals(c("row","col"))



in_completed1<- suppressWarnings(fread(here("study data/Visit Completed v_s SDV_2.csv"),
                                       colClasses = c("text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text")))|>fill_blanks()|>
  # mutate(VISDAT=as.Date(VISDAT,"%d %m %Y"),
  #        `Entered Date-Time`=as.Date(`Entered Date-Time`, "%d %m %Y %H:%M"))
  mutate(VISDAT=as.Date(VISDAT,"%d %b %y"),
         `Entered Date-Time`=as.Date(`Entered Date-Time`,"%d %b %y" ))#"%d %m %Y %H:%M"
in_completed<-in_completed1|>
  dplyr::rename(status="EntryStatus")|>
  filter(status == "Incomplete")|>
  select(2,5:7,9,10)

trends1<-left_join(queries_now|>
                     filter(status=="Open Query")|>dplyr::rename(Module=`Module Name`),dateofvisit|>
                     dplyr::rename("Subject ID"=Subject, Visit=VISIT)|>
                     select(2,3,7),by=c("Subject ID","Visit"),relationship = "many-to-many")|>
  select(4,5,21,6,7,14)|>
  mutate(`Visit date`=as.Date(`Visit date`, format="%Y-%m-%d"))|>left_join(dertas<-in_completed1|>select(2,6,7,10)|>
                                                                             dplyr::rename(`Subject ID`=`Subject Id`, Visit=VISITNAME, Module=FORMNAME),by=c("Subject ID","Visit","Module"),relationship = "many-to-one")#|>
# distinct(`Subject ID`, VISIT,`Visit date`, Module,`Rule Description`,`Field Name`,`Query Text`,`Entered By`, .keep_all = TRUE)


trends<-trends1|>
  group_by(Module)|>
  summarise(count=n())|>
  arrange(desc(count))

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++####
###+ Sign off status
###+ 1. esource
esarce<-in_completed1|>
  select(2,5:8,11,14)|>
  filter(`EntryStatus`!="Not Started",`eSource - Investigator Sign Off Status`=="Pending")|>
  mutate(VISDAT=as.Date(VISDAT, format="%d-%b-%Y"))

###+ 2. ECRF
ecfe<-esarce|>
  filter(`Investigator Sign Off Status`=="Pending")


####+++++++++++++end sign off+++++++++++++####

###+Deviations+##############

visit_out_win <- scheduler |>
  filter(exactDate-windowClose>0)
summary_vis_out<- visit_out_win|>
  group_by(visitCode)|>
  summarise(local=n())|>
  adorn_totals()

data_natakaga<- queries_nowe |>
  dplyr::rename(visitCode = `Visit`,
                screenID = `Subject ID`)

data_nataka <- data_natakaga|>
  select(`screenID`, `visitCode`,`Module Name`,`Field Name`, `Query Text`, `Query Status`)|>
  filter(`Field Name` == "Visit date" | `Field Name` == "Date of vaccination",
         `visitCode` != "Screening",
         !str_detect(`visitCode`, "Unscheduled|Pre-vacc"),
         str_detect(`Query Text`, "Difference between")|
           str_detect(`Query Text`, "Visit Date is missing."))|>
  distinct(`screenID`,`visitCode`, .keep_all = TRUE)|>
  mutate(
    visitCode = case_when(
      visitCode == "Dose 2" ~ "Day 28",
      visitCode == "Dose 3" ~ "Day 56",
      visitCode == "Pre-Booster" ~ "Day B0",
      visitCode == "B 14" ~ "Day B14",
      visitCode == "B 28" ~ "Day B28",
      visitCode == "B 84" ~ "Day B84",
      visitCode == "B 140" ~ "Day B140",
      visitCode == "B 168" ~ "Day B168",
      visitCode == "B 224" ~ "Day B224",
      visitCode == "B 280" ~ "Day B280",
      visitCode == "B 336" ~ "Day B336",
      visitCode == "B1Y 0" ~ "Day B365/2B0",
      TRUE ~ visitCode
    ),screenID=as.numeric(screenID)
  )


summary_of <- data_nataka |>
  group_by(visitCode) |>
  summarise(eSource=n()) |>
  arrange(factor(visitCode, levels = c("Day 14", "Day 28", "Day 42", "Day 56", "Day 70", "Day 84", "Day 140"
                                       ,"Day 196","Day 236","Day 292","Day 348","Day 404","Day B0","Day B14","Day B28","Day B84","Day B140","Day B168","Day B224","Day B280", "Day B336", "Day B365/2B0")))


combined_l_e <- left_join(summary_of, summary_vis_out, by = "visitCode",relationship = "many-to-many")|>
  adorn_totals()

yf<-flextable(combined_l_e)
op_en <- data_nataka |>
  filter(`Query Status` == "Open Query")

###++++++++++++++++++++++end deviations++++++++++++++++++++++++++++++++###

###+Lab results+####
bloodcoll<- suppressWarnings(fread(here("study data/Blood Sample Collection Listing.csv"),colClasses = c("text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text")))|>
  select(2:5,9,10,15,16)|>
  setNames(c("Subject","Visit","hemblood","hemdate","bioblood","biodate","dbs","dbsdate"))|>fill_blanks()|>
  mutate(hemdate=as.Date(hemdate,"%d-%b-%Y"),biodate=as.Date(biodate,"%d-%b-%Y"),dbsdate=as.Date(dbsdate,"%d-%b-%Y"))
## Samples collected in week and/or month
## Overall
extract1<-bloodcoll|>
  filter(hemblood=="Yes"|bioblood=="Yes"|dbs=="Yes")
## DBS
dbsex1<-extract1|>
  filter(dbs=="Yes")
## Hem
hemex1<-extract1|>
  filter(hemblood=="Yes")
## Bio
bioex1<-extract1|>
  filter(bioblood=="Yes")

## Monthly
extract2<-bloodcoll|>
  filter(!is.na(hemdate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(hemdate))<30|
           !is.na(biodate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(biodate))<30|
           !is.na(dbsdate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(dbsdate))<30)
## DBS2
dbsex2<-extract2|>
  filter(dbs=="Yes")
## Hem2
hemex2<-extract2|>
  filter(hemblood=="Yes")
## Bio2
bioex2<-extract2|>
  filter(bioblood=="Yes")

## Weekly
extract3<-bloodcoll|>
  filter(!is.na(hemdate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(hemdate))<7|
           !is.na(biodate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(biodate))<7|
           !is.na(dbsdate)&as.numeric(as.Date(format(Sys.time(),"%Y-%m-%d"))-as.Date(dbsdate))<7)
## DBS3
dbsex3<-extract3|>
  filter(dbs=="Yes")
## Hem3
hemex3<-extract3|>
  filter(hemblood=="Yes")
## Bio3
bioex3<-extract3|>
  filter(bioblood=="Yes")

### Plasmodium results
malrslt<- suppressWarnings(fread(here("study data/Blood Sample For Plasmodium Species Results Listing.csv")))|>fill_blanks()|>
  select(2,3,6,7,9,10,12,13,15,16,18,19)|>
  setNames(c("Subject","Visit","dbsrslt","result","count_trophozoites_falciparum","count_game_falciparum","count_trophozoites_malariae","count_game_malariae","count_trophozoites_ovale","count_game_ovale","count_trophozoites_vivax","count_game_vivax"))|>
  filter(dbsrslt=="Blood film for Plasmodium species")
### Hematology and Biochem
hemboirsltew<- suppressWarnings(fread(here("study data/Haematology And Biochemistry Listing.csv"),
                                      colClasses = c("numeric","text","text","text","text","text","text","numeric","text","numeric","numeric","text","text","text","date","date")))|>fill_blanks()
hemboirslt<-hemboirsltew|>
  select(2,3,6,8,9)|>
  setNames(c("Subject","Visit","test","result","no_res_reason"))|>
  mutate(result=coalesce(as.character(result),no_res_reason))|>select(-5)|>
  distinct(Subject,Visit,test, .keep_all = TRUE)|>
  #dcast(Subject+Visit~test)|>
  pivot_wider(id_cols = c("Subject","Visit"),names_from = "test",values_from = "result")|>
  select(1:4,6:16,5,17,18)
## Overall
##plas rslts entered
entereddbo<-inner_join(malrslt,ex2<-extract1|>
                         filter(dbs=="Yes"),by=c("Subject","Visit"))|>
  filter(!is.na(result))
##plas rslts not entered
notentereddbo<-anti_join(ex2,entereddbo,by=c("Subject","Visit"))

## Monthly
entereddbm<-inner_join(malrslt,ex3<-extract2|>
                         filter(dbs=="Yes"),by=c("Subject","Visit"))|>
  filter(!is.na(result))
##plas rslts not entered
notentereddbm<-anti_join(ex3,entereddbm,by=c("Subject","Visit"))
## weekly
entereddbw<-inner_join(malrslt,ex4<-extract3|>
                         filter(dbs=="Yes"),by=c("Subject","Visit"))|>
  filter(!is.na(result))
##plas rslts not entered
notentereddbw<-anti_join(ex4,entereddbw,by=c("Subject","Visit"))

#hem results
enteredhemo<-inner_join(hemboirslt|>
                          filter(!is.na(`Platelet Count`)&!is.na(Haemoglobin)&!is.na(Haematocrit)&!is.na(`RBC Count`)&!is.na(MCV)&!is.na(`WBC Count`)&!is.na(Neutrophils)&!is.na(Lymphocytes)&!is.na(Monocytes)&!is.na(Eosinophils)&!is.na(Basophils)&!is.na(`Neutrophils (%)`)&!is.na(`Lymphocytes (%)`))|>
                          select(1:15),ex5<-extract1|>
                          filter(hemblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##hem rslts not entered
notenteredhemo<-anti_join(ex5,enteredhemo,by=c("Subject","Visit"))

## Monthly
enteredhemm<-inner_join(hemboirslt|>
                          filter(!is.na(`Platelet Count`)&!is.na(Haemoglobin)&!is.na(Haematocrit)&!is.na(`RBC Count`)&!is.na(MCV)&!is.na(`WBC Count`)&!is.na(Neutrophils)&!is.na(Lymphocytes)&!is.na(Monocytes)&!is.na(Eosinophils)&!is.na(Basophils)&!is.na(`Neutrophils (%)`)&!is.na(`Lymphocytes (%)`))|>
                          select(1:15),ex6<-extract2|>
                          filter(hemblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##hem rslts not entered
notenteredhemm<-anti_join(ex6,enteredhemm,by=c("Subject","Visit"))
## weekly
enteredhemw<-inner_join(hemboirslt|>
                          filter(!is.na(`Platelet Count`)&!is.na(Haemoglobin)&!is.na(Haematocrit)&!is.na(`RBC Count`)&!is.na(MCV)&!is.na(`WBC Count`)&!is.na(Neutrophils)&!is.na(Lymphocytes)&!is.na(Monocytes)&!is.na(Eosinophils)&!is.na(Basophils)&!is.na(`Neutrophils (%)`)&!is.na(`Lymphocytes (%)`))|>
                          select(1:15),ex7<-extract3|>
                          filter(hemblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##hem rslts not entered
notenteredhemw<-anti_join(ex7,enteredhemw,by=c("Subject","Visit"))

#Bio results
enteredbioo<-inner_join(hemboirslt|>
                          filter(!is.na(`Total Bilirubin`)&!is.na(Creatinine)&!is.na(ALT))|>
                          select(1,2,16:18),ex51<-extract1|>
                          filter(bioblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##Bio rslts not entered
notenteredbioo<-anti_join(ex51,enteredbioo,by=c("Subject","Visit"))

## Monthly
enteredbiom<-inner_join(hemboirslt|>
                          filter(!is.na(`Total Bilirubin`)&!is.na(Creatinine)&!is.na(ALT))|>
                          select(1,2,16:18),ex61<-extract2|>
                          filter(bioblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##Bio rslts not entered
notenteredbiom<-anti_join(ex61,enteredbiom,by=c("Subject","Visit"))
## weekly
enteredbiow<-inner_join(hemboirslt|>
                          filter(!is.na(`Total Bilirubin`)&!is.na(Creatinine)&!is.na(ALT))|>
                          select(1,2,16:18),ex71<-extract3|>
                          filter(bioblood=="Yes")|>mutate(Subject=as.character(Subject)),by=c("Subject","Visit"))
##Bio rslts not entered
notenteredbiow<-anti_join(ex71,enteredbiow,by=c("Subject","Visit"))

#### Create table
results_tab<-as.data.frame(rbind(
  overall_collected<-list("Expected overall",nrow(extract1|>filter(dbs=="Yes")),nrow(extract1|>filter(hemblood=="Yes")),nrow(extract1|>filter(bioblood=="Yes"))),
  # overall_entered<-list("Entered overall",nrow(entereddbo),nrow(enteredhemo),nrow(enteredbioo)),
  overall_pending<-list("Pending overall",nrow(notentereddbo),nrow(notenteredhemo),nrow(notenteredbioo)),
  monthly<-list("Expected this month",nrow(extract2|>filter(dbs=="Yes")),nrow(extract2|>filter(hemblood=="Yes")),nrow(extract2|>filter(bioblood=="Yes"))),
  entered_month<-list("Entered this month",nrow(entereddbm),nrow(enteredhemm),nrow(enteredbiom)),
  pending_month<-list("Pending this month",nrow(notentereddbm),nrow(notenteredhemm),nrow(notenteredbiom)),
  weekly<-list("Expected this week",nrow(extract3|>filter(dbs=="Yes")),nrow(extract3|>filter(hemblood=="Yes")),nrow(extract3|>filter(bioblood=="Yes"))),
  entered_week<-list("Entered this week",nrow(entereddbw),nrow(enteredhemw),nrow(enteredbiow)),
  pending_week<-list("Pending this week",nrow(notentereddbw),nrow(notenteredhemw),nrow(notenteredbiow))))|>
  setNames(c("col","Plasmodium species","Haematology","Biochem"))|>
  mutate(col=as.character(col), `Plasmodium species`=as.numeric(`Plasmodium species`),Haematology=as.numeric(Haematology),Biochem=as.numeric(Biochem))|>
  adorn_totals(c("col"))|>
  flextable()|>vline()|>hline()|>vline_left()|>autofit()
allsamp<-left_join(hemboirsltew|>
                     filter(is.na(`Reason for No Result`))|>mutate(Subject=as.numeric(Subject)),bloodcoll|>select(1,2,4,6)|>rename(VISIT=Visit)|>mutate(`sampledate`=coalesce(hemdate,biodate)),by=c("Subject","VISIT"),relationship = "many-to-many")|>
  filter(!is.na(`sampledate`))|>
  select(2,3,19,6,8)|>
  setnames(c("Subject","VISIT","sampledate","Test","Result"))

# kidms data import into my environment
mydata<-suppressWarnings(fread(here("study data/kidmsresults.csv")))|>select(-1)

entered_vbaya<-anti_join(allsamp,mydata|>mutate(Subject=as.numeric(Subject),sampledate=as.Date(sampledate),Result=as.numeric(Result)), by=c("Subject","sampledate","Test","Result"))
distinkt<-entered_vbaya|>
  distinct(Subject,VISIT)|>arrange(Subject)

##Out of Range results
hembioqry23<-left_join(noresdat<-hemboirsltew|>
                         select(-c(1,4,5,7,16))|>
                         mutate(`First Data Time`=as.Date(`First Data Time`, "%d-%b-%Y %H:%M")),dobnew<-fread(here("study data/Demographics Listing.csv"))|>fill_blanks()|>
                         select(2,4)|>mutate(Subject=as.character(Subject),`Date of Birth`=as.Date(`Date of Birth`, "%d %m %Y")), by="Subject",relationship =
                         "many-to-many")|>
  mutate(ageinmonths=interval(`Date of Birth`,`First Data Time`)%/% months(1))|>
  distinct(Subject,VISIT,`Test name`, .keep_all = TRUE)|>
  select(-c(6,7,12))|>
  mutate_if(is.character,~na_if(., ''))|>
  setnames(c("Subject","Visit","Test","Result","Reason","Range","Significance","Code","sampledate","Age"))|>
  mutate(Monocytesqry=case_when(Test=="Monocytes"&between(Age,1,5)&!between(Result,0.29,1.9)&is.na(Range)&is.na(Reason)~"Review Monocytes value and/ or range",
                                Test=="Monocytes"&between(Age,6,11)&!between(Result,0.3,1.86)&is.na(Range)&is.na(Reason)~"Review Monocytes value and/ or range",
                                Test=="Monocytes"&between(Age,12,59)&!between(Result,0.21,1.42)&is.na(Range)&is.na(Reason)~"Review Monocytes value and/ or range",
                                Test=="Monocytes"&between(Age,60,215)&!between(Result,0.15,0.58)&is.na(Range)&is.na(Reason)~"Review Monocytes value and/ or range",
                                TRUE~NA_character_),
         Eosinophilsqry=case_when(Test=="Eosinophils"&between(Age,1,5)&!between(Result,0.07,0.71)&is.na(Range)&is.na(Reason)~"Review Eosinophils value and/ or range",
                                  Test=="Eosinophils"&between(Age,6,11)&!between(Result,0.06,1.11)&is.na(Range)&is.na(Reason)~"Review Eosinophils value and/ or range",
                                  Test=="Eosinophils"&between(Age,12,59)&!between(Result,0.07,1.07)&is.na(Range)&is.na(Reason)~"Review Eosinophils value and/ or range",
                                  Test=="Eosinophils"&between(Age,60,215)&!between(Result,0.04,0.50)&is.na(Range)&is.na(Reason)~"Review Eosinophils value and/ or range",
                                  TRUE~NA_character_),
         Plateletsqry=case_when(Test=="Platelet Count"&between(Age,1,5)&!between(Result,52,752)&is.na(Range)&is.na(Reason)~"Review Platelet Count and/ or range",
                                Test=="Platelet Count"&between(Age,6,11)&!between(Result,89,789)&is.na(Range)&is.na(Reason)~"Review Platelet Count and/ or range",
                                Test=="Platelet Count"&between(Age,12,59)&!between(Result,139,730)&is.na(Range)&is.na(Reason)~"Review Platelet Count and/ or range",
                                Test=="Platelet Count"&between(Age,60,215)&!between(Result,159,451)&is.na(Range)&is.na(Reason)~"Review Platelet Count and/ or range",
                                TRUE~NA_character_),
         Creatinineqry=case_when(Test=="Creatinine"&between(Age,1,5)&!between(Result,26,43)&is.na(Range)&is.na(Reason)~"Review Creatinine and/ or range",
                                 Test=="Creatinine"&between(Age,6,11)&!between(Result,25,45)&is.na(Range)&is.na(Reason)~"Review Creatinine and/ or range",
                                 Test=="Creatinine"&between(Age,12,59)&!between(Result,25,48)&is.na(Range)&is.na(Reason)~"Review Creatinine and/ or range",
                                 TRUE~NA_character_),
         ALTqry=case_when(Test=="ALT"&between(Age,1,5)&!between(Result,7.18,34.83)&is.na(Range)&is.na(Reason)~"Review ALT and/ or range",
                          Test=="ALT"&between(Age,6,11)&!between(Result,9.98,33.03)&is.na(Range)&is.na(Reason)~"Review ALT and/ or range",
                          Test=="ALT"&between(Age,12,215)&!between(Result,11,35)&is.na(Range)&is.na(Reason)~"Review ALT and/ or range",
                          TRUE~NA_character_),
         bilirubinqry=case_when(Test=="Total Bilirubin"&between(Age,1,215)&!between(Result,2,9)&is.na(Range)&is.na(Reason)~"Review Bilirubin and/ or range",
                                TRUE~NA_character_),
         WBCqry=case_when(Test=="WBC Count"&between(Age,1,5)&!between(Result,5.1,14.9)&is.na(Range)&is.na(Reason)~"Review WBC Count and/ or range",
                          Test=="WBC Count"&between(Age,6,11)&!between(Result,6,17.2)&is.na(Range)&is.na(Reason)~"Review WBC Count and/ or range",
                          Test=="WBC Count"&between(Age,12,59)&!between(Result,5.1,16.3)&is.na(Range)&is.na(Reason)~"Review WBC Count and/ or range",
                          Test=="WBC Count"&between(Age,60,215)&!between(Result,3.8,8.8)&is.na(Range)&is.na(Reason)~"Review WBC Count and/ or range",
                          TRUE~NA_character_),
         haemoglobinqry=case_when(Test=="Haemoglobin"&between(Age,1,5)&!between(Result,8.1,14)&is.na(Range)&is.na(Reason)~"Review Haemoglobin and/ or range",
                                  Test=="Haemoglobin"&between(Age,6,11)&!between(Result,7.2,11.5)&is.na(Range)&is.na(Reason)~"Review Haemoglobin and/ or range",
                                  Test=="Haemoglobin"&between(Age,12,59)&!between(Result,7.6,12.2)&is.na(Range)&is.na(Reason)~"Review Haemoglobin and/ or range",
                                  Test=="Haemoglobin"&between(Age,60,215)&!between(Result,10.4,14.7)&is.na(Range)&is.na(Reason)~"Review Haemoglobin and/ or range",
                                  TRUE~NA_character_),
         haematocritqry=case_when(Test=="Haematocrit"&between(Age,1,5)&!between(Result,24.6,42.1)&is.na(Range)&is.na(Reason)~"Review Haematocrit and/ or range",
                                  Test=="Haematocrit"&between(Age,6,11)&!between(Result,23.7,35.8)&is.na(Range)&is.na(Reason)~"Review Haematocrit and/ or range",
                                  Test=="Haematocrit"&between(Age,12,59)&!between(Result,25.7,37.3)&is.na(Range)&is.na(Reason)~"Review Haematocrit and/ or range",
                                  Test=="Haematocrit"&between(Age,60,215)&!between(Result,32.3,44.4)&is.na(Range)&is.na(Reason)~"Review Haematocrit and/ or range",
                                  TRUE~NA_character_),
         RBCqry=case_when(Test=="RBC Count"&between(Age,1,5)&!between(Result,2.85,5.34)&is.na(Range)&is.na(Reason)~"Review RBC Count and/ or range",
                          Test=="RBC Count"&between(Age,6,11)&!between(Result,3.80,5.78)&is.na(Range)&is.na(Reason)~"Review RBC Count and/ or range",
                          Test=="RBC Count"&between(Age,12,59)&!between(Result,3.84,5.92)&is.na(Range)&is.na(Reason)~"Review RBC Count and/ or range",
                          Test=="RBC Count"&between(Age,60,215)&!between(Result,3.93,5.91)&is.na(Range)&is.na(Reason)~"Review RBC Count and/ or range",
                          TRUE~NA_character_),
         MCVqry=case_when(Test=="MCV"&between(Age,1,5)&!between(Result,57,100)&is.na(Range)&is.na(Reason)~"Review MCV and/ or range",
                          Test=="MCV"&between(Age,6,11)&!between(Result,52,78)&is.na(Range)&is.na(Reason)~"Review MCV and/ or range",
                          Test=="MCV"&between(Age,12,59)&!between(Result,52,82)&is.na(Range)&is.na(Reason)~"Review MCV and/ or range",
                          Test=="MCV"&between(Age,60,215)&!between(Result,64,90)&is.na(Range)&is.na(Reason)~"Review MCV and/ or range",
                          TRUE~NA_character_),
         basophilsqry=case_when(Test=="Basophils"&between(Age,1,5)&!between(Result,0.01,0.13)&is.na(Range)&is.na(Reason)~"Review Basophils and/ or range",
                                Test=="Basophils"&between(Age,6,11)&!between(Result,0.01,0.3)&is.na(Range)&is.na(Reason)~"Review Basophils and/ or range",
                                Test=="Basophils"&between(Age,12,59)&!between(Result,0.01,0.25)&is.na(Range)&is.na(Reason)~"Review Basophils and/ or range",
                                Test=="Basophils"&between(Age,60,215)&!between(Result,0.02,0.1)&is.na(Range)&is.na(Reason)~"Review Basophils and/ or range",
                                TRUE~NA_character_),
         neutrophilsqry=case_when(Test=="Neutrophils"&between(Age,1,5)&!between(Result,0.57,4.03)&is.na(Range)&is.na(Reason)~"Review Neutrophils and/ or range",
                                  Test=="Neutrophils"&between(Age,6,11)&!between(Result,0.98,4.38)&is.na(Range)&is.na(Reason)~"Review Neutrophils and/ or range",
                                  Test=="Neutrophils"&between(Age,12,59)&!between(Result,1.12,5.27)&is.na(Range)&is.na(Reason)~"Review Neutrophils and/ or range",
                                  Test=="Neutrophils"&between(Age,60,215)&!between(Result,1.03,5.04)&is.na(Range)&is.na(Reason)~"Review Neutrophils and/ or range",
                                  TRUE~NA_character_),
         lymphocytesqry=case_when(Test=="Lymphocytes"&between(Age,1,5)&!between(Result,3.12,9.44)&is.na(Range)&is.na(Reason)~"Review Lymphocytes and/ or range",
                                  Test=="Lymphocytes"&between(Age,6,11)&!between(Result,3.39,11.00)&is.na(Range)&is.na(Reason)~"Review Lymphocytes and/ or range",
                                  Test=="Lymphocytes"&between(Age,12,59)&!between(Result,2.5,10.39)&is.na(Range)&is.na(Reason)~"Review Lymphocytes and/ or range",
                                  Test=="Lymphocytes"&between(Age,60,215)&!between(Result,1.5,4.09)&is.na(Range)&is.na(Reason)~"Review Lymphocytes and/ or range",
                                  TRUE~NA_character_),
         neutrophils_qry=case_when(Test=="Neutrophils (%)"&between(Age,1,5)&!between(Result,9.1,35.1)&is.na(Range)&is.na(Reason)~"Review Neutrophils (%) and/ or range",
                                  Test=="Neutrophils (%)"&between(Age,6,11)&!between(Result,10.2,38.5)&is.na(Range)&is.na(Reason)~"Review Neutrophils (%) and/ or range",
                                  Test=="Neutrophils (%)"&between(Age,12,59)&!between(Result,12.8,51.8)&is.na(Range)&is.na(Reason)~"Review Neutrophils (%) and/ or range",
                                  Test=="Neutrophils (%)"&between(Age,60,215)&!between(Result,24.4,60.8)&is.na(Range)&is.na(Reason)~"Review Neutrophils (%) and/ or range",
                                  TRUE~NA_character_),
         lymphocytes_qry=case_when(Test=="Lymphocytes (%)"&between(Age,1,5)&!between(Result,44.2,78.0)&is.na(Range)&is.na(Reason)~"Review Lymphocytes (%) and/ or range",
                                  Test=="Lymphocytes (%)"&between(Age,6,11)&!between(Result,44.2,77.6)&is.na(Range)&is.na(Reason)~"Review Lymphocytes (%) and/ or range",
                                  Test=="Lymphocytes (%)"&between(Age,12,59)&!between(Result,36.9,74.8)&is.na(Range)&is.na(Reason)~"Review Lymphocytes (%) and/ or range",
                                  Test=="Lymphocytes (%)"&between(Age,60,215)&!between(Result,26.4,65.5)&is.na(Range)&is.na(Reason)~"Review Lymphocytes (%) and/ or range",
                                  TRUE~NA_character_),
         significanceqry=case_when(!is.na(Range)&is.na(Significance)~"Complete Clinical significance",TRUE~NA_character_),
         codeqry=case_when(Significance=="Clinically Significant"&is.na(Code)~"Complete Event code",TRUE~NA_character_))|>
  select(1:5,9:28)|>
  filter(Visit!="Screening",!is.na(Monocytesqry)|!is.na(Eosinophilsqry)|!is.na(Plateletsqry)|!is.na(Creatinineqry)|!is.na(ALTqry)|!is.na(bilirubinqry)|!is.na(WBCqry)|!is.na(haemoglobinqry)|!is.na(haematocritqry)|!is.na(RBCqry)|!is.na(MCVqry)|!is.na(basophilsqry)|!is.na(neutrophilsqry)|!is.na(lymphocytesqry)|!is.na(neutrophils_qry)|!is.na(lymphocytes_qry)|!is.na(significanceqry)|!is.na(codeqry))|>
  mutate(`query text`=coalesce(Monocytesqry,Eosinophilsqry,Plateletsqry,Creatinineqry,ALTqry,bilirubinqry,WBCqry,haemoglobinqry,haematocritqry,RBCqry,MCVqry,basophilsqry,neutrophilsqry,lymphocytesqry,neutrophils_qry,lymphocytes_qry,significanceqry,codeqry))|>
  select(1:4,7,26,6)|>
  arrange(Subject,sampledate,Visit)|>
  rename(`age at test(months)`=Age)


###+end lab results+#################################

###+++++ Adverse Events++++######
localae<-suppressWarnings(fread(paste0(here(),"/study data/Local Solicited Adverse Event Listing.csv")))|>fill_blanks()|>
  filter(!is.na(Outcome))|>
  dplyr::rename(post_dose="Local solicited adverse event observed post dose")|>
  mutate(post_dose=factor(post_dose, levels = c(1,2,3,"Booster"), labels = c("Dose 1","Dose 2","Dose 3","Booster")))
solicited<-suppressWarnings(fread(paste0(here(),"/study data/Systemic Solicited Adverse Event Listing.csv")))|>fill_blanks()|>
  filter(!is.na(Outcome))|>
  dplyr::rename(post_dose="Systemic solicited adverse event observed post dose")|>
  mutate(post_dose=factor(post_dose, levels = c(1,2,3,"Booster"), labels = c("Dose 1","Dose 2","Dose 3","Booster")))
unsolicited<-suppressWarnings(fread(paste0(here(),"/study data/Unsolicited Adverse Event Listing.csv")))|>fill_blanks()|>
  filter(!is.na(Outcome))

localae2<-localae[!is.na(localae$`Date of onset`),]
overduelsae<-localae2|>
  mutate(datedif=today()-as.Date(`Date of onset`,"%d-%b-%Y"))|>
  filter(Outcome=="Recovering / Resolving")
solicited2<-solicited[!is.na(solicited$`Date of onset`),]
overduessae<-solicited2|>
  mutate(datedif=today()-as.Date(`Date of onset`,"%d-%b-%Y"))|>
  filter(Outcome=="Recovering / Resolving")
unsolicited_2<-unsolicited[!is.na(unsolicited$`Date of onset`),]
## Get name of clinician logging AE

unsolicited2<-left_join(unsolicited_2|>
                          mutate(datedif=today()-as.Date(`Date of onset`,"%d-%b-%Y"),`First Data Time`=as.Date(`First Data Time`,format="%d-%b-%Y %H:%M")),left_join(dateofvisit|>
                                                                                                                                                                       dplyr::rename(VISDAT=`Visit date`),
                                                                                                                                                                     in_completed1|>
                                                                                                                                                                       dplyr::rename(Subject="Subject Id", VISIT=VISITNAME)|>mutate(VISDAT=as.Date(VISDAT, "%Y-%m-%d"))|>filter(!VISIT%in%c("Day 14","Day 42","Day 70","Day 140","Day 196","Day 292","Day 348","Day 404","B 14","B 84","B 140","B 224","B 280","B 336")),by=c("Subject","VISIT","VISDAT"),relationship = "many-to-many")|>
                          filter(EntryStatus!="Not Started")|>
                          select(2:3,18,23:26)|>
                          mutate(`First Data Time`=as.Date(`First Data Time`,format="%d-%m-%Y %H:%M")),by=c("Subject","First Data Time"),relationship = "many-to-many")|>
  arrange(Subject)|>
  distinct(Subject,`Unsolicited adverse event code`,.keep_all = TRUE)|>
  mutate(`Date of onset`=as.Date(`Date of onset`,"%d-%b-%Y"),`Entered By`=case_when(`Adverse Event Preferred Term Code` %in%c(10040644,10040641,10040642,10061273,10022016,10043348,10062061,10039906)~"Chronic",TRUE~`Entered By`))|>
  select(2:15,22,23,25,27,29,31,33,34,36,37,41)

grade3<-unsolicited2|>
  filter(`Severity grade`=="Grade 3")|>
  select(1,24,4,5,6,7,9,10,13,25)|>
  filter(!is.na(`Date of onset`))|>
  dplyr::rename(VISIT=VISIT.y)


overduesssae<-unsolicited2|>
  filter(Outcome!="Recovered / Resolved without sequelae"&Outcome!="Fatal"&Outcome!="Lost to follow-up / Unknown")|>
  select(1,24,4,5,6,7,9,10,13,25)|>
  filter(!is.na(`Date of onset`))|>
  dplyr::rename(VISIT=VISIT.y)

get_name<-overduesssae

kelvina<-get_name|>
  filter(grepl("Kevin",`Entered By`))|>
  select(-`Entered By`)
josepha<-get_name|>
  filter(grepl("Joseph",`Entered By`))|>
  select(-`Entered By`)
sharona<-get_name|>
  filter(grepl("Sharon",`Entered By`)|is.na(`VISIT`))|>
  select(-`Entered By`)
seriana<-get_name|>
  filter(grepl("Seriana",`Entered By`))|>
  select(-`Entered By`)
marthaa<-get_name|>
  filter(grepl("Martha",`Entered By`)|grepl("Omar",`Entered By`)|grepl("Grace",`Entered By`)|grepl("Boni",`Entered By`)|grepl("Adam",`Entered By`)|grepl("Anthony",`Entered By`)|grepl("Kennedy",`Entered By`)|grepl("Oscar",`Entered By`))|>
  select(-`Entered By`)
chronica<-get_name|>
  filter(grepl("Chronic",`Entered By`)&!is.na(`VISIT`))|>
  select(-`Entered By`)

###====local AEs ====###
lsae<-localae|>filter(!is.na(post_dose))|>
  group_by(`Outcome`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against Outcome")
lsae2<-localae|>filter(!is.na(post_dose))|>
  group_by(`Local solicited adverse event term`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against AE term")

###==== Systemic AEs =====###
ssae<-solicited|>filter(!is.na(post_dose))|>
  group_by(`Outcome`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against Outcome")

ssae2<-solicited|>filter(!is.na(post_dose))|>
  group_by(`Systemic solicited adverse event term`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against AE term")

###==== Unsolicited AEs =====###
sumunsol<-unsolicited|>filter(`Did the subject experience any unsolicited adverse event?`=="Yes")|>
  group_by(`Outcome`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  select(1,4,3,2)|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against Outcome")


sumunsol2<-unsolicited|>filter(`Did the subject experience any unsolicited adverse event?`=="Yes")|>
  mutate(`Adverse Event High Level Term`=case_when(is.na(`Adverse Event High Level Term`)~"Other infections", TRUE~`Adverse Event High Level Term`))|>
  group_by(`Adverse Event High Level Term`,`Severity grade`)|>
  summarise(count=n(), .groups='drop')|>
  pivot_wider(names_from = `Severity grade`, values_from = count)|>
  select(1,4,2,3)|>
  arrange(desc(`Grade 2`))|>
  adorn_totals(c("row","col"))|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Severity grade against AE term")

#### Serious AEs
g3summ1<-unsolicited2|>
  filter(`Severity grade`=="Grade 3")|>
  group_by(`Outcome`)|>
  summarise(count=n(), .groups='drop')|>
  arrange(desc(count))|>
  adorn_totals()|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Count based on SAE Outcome")

g3summ2<-unsolicited2|>
  filter(`Severity grade`=="Grade 3")|>
  group_by(`Adverse Event High Level Term`)|>
  summarise(count=n(), .groups='drop')|>
  arrange(desc(count))|>
  adorn_totals()|>
  flextable()|>
  hline(border = NULL, part = "body")|>
  autofit(add_w = 0.02, add_h = 0.1, part = c("body", "header"), unit = "in")|>
  set_caption(caption = "Count based on SAE term")

## AE closure summary
sumari<-left_join(get_open<-get_name|>
                    mutate(`Entered By`=case_when(`Entered By`%in%c("Joseph Ochieng Weya","Chronic","Seriana Nyange","Kevin Njogu","Martha Ndichu")~`Entered By`, TRUE~"Pending Link"))|>
                    group_by(`Entered By`)|>
                    summarise(open=n()),left_join(get_open<-get_name|>
                                                    mutate(`Entered By`=case_when(`Entered By`%in%c("Joseph Ochieng Weya","Chronic","Seriana Nyange","Kevin Njogu","Martha Ndichu")~`Entered By`, TRUE~"Pending Link"),
                                                           datedif=today()-as.Date(`Date of onset`))|>
                                                    filter(datedif>7)|>
                                                    group_by(`Entered By`)|>
                                                    summarise("open>7days"=n()),
                                                  asda<-left_join(unsolicited|>
                                                                    mutate(`First Data Time`=as.Date(`First Data Time`,format="%d-%b-%Y %H:%M"),
                                                                           `Last Data Time`=as.Date(`Last Data Time`,format="%d-%b-%Y %H:%M"),
                                                                           diffe_r=as.numeric(today()-`Last Data Time`)),
                                                                  left_join(dateofvisit|> dplyr::rename(VISDAT=`Visit date`),
                                                                            in_completed1|>
                                                                              dplyr::rename(Subject="Subject Id", VISIT=VISITNAME)|>mutate(VISDAT=as.Date(VISDAT, "%Y-%m-%d")),by=c("Subject","VISIT","VISDAT"),relationship = "many-to-many")|>
                                                                    filter(EntryStatus!="Not Started")|>
                                                                    select(2:3,18,23:26)|>
                                                                    mutate(`First Data Time`=as.Date(`First Data Time`,format="%Y-%m-%d")),by=c("Subject","First Data Time"),relationship = "many-to-many")|>
                                                    arrange(Subject)|>
                                                    distinct(Subject,`Unsolicited adverse event code`,.keep_all = TRUE)|>
                                                    mutate(`Date of onset`=as.Date(`Date of onset`,"%d-%b-%Y"),
                                                           `Entered By`=case_when(`Adverse Event Preferred Term Code` %in%c(10040644,10040641,10040642,10061273,10022016,10043348,10062061)~"Chronic",
                                                                                  TRUE~`Entered By`))|>
                                                    filter(!is.na(`Date of resolution`)&diffe_r<=7)|>
                                                    group_by(`Entered By`)|>
                                                    summarise(closed=n()),by="Entered By",relationship = "many-to-many"),by="Entered By",relationship = "many-to-many")|>arrange(factor(`Entered By`, levels=c("Joseph Ochieng Weya","Kevin Njogu","Martha Ndichu","Seriana Nyange","Pending Link","Chronic")))|>adorn_totals(c("row"))|>flextable()|>autofit()



###++++++++++++++++++ end adverse events ++++++++++++++##########

###+++++++++++ Malaria Cases ++++++++##########
malaria_cases<- left_join(medications<-suppressWarnings(fread(here("study data/Prior Or Concomitant Medications Listing.csv"),colClasses = c("text","numeric","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text")))|>fill_blanks()|>
                            dplyr::rename("Visit date"="First Data Time")|>
                            mutate(`Visit date`=as.Date(`Visit date`, format="%d-%b-%Y %H:%M")),
                          dateofvisit|>
                            select(2,3,7)|>dplyr::rename(timepoint=VISIT),by=c("Subject","Visit date"),relationship = "many-to-many")
cases<-malaria_cases|>
  dplyr::rename(drug_name="Generic Name / Active Ingredients")|>
  filter(grepl('lumef', drug_name)|grepl('Lumef', drug_name))

mal_cases<-cases|>
  filter(Indication=="Malaria")|>
  mutate(`Start date`=as.Date(`Start date`,format="%d/%m/%Y"),
         `Stop date`=as.Date(`Stop date`, format="%d/%m/%Y"))

ongoinmal<-cases|>
  filter(Ongoing=="Yes")
withvacdate1<-left_join(mal_cases,updatedcases<-vacdata|>
                          select(2,3,5)|>
                          pivot_wider(id_cols=Subject,names_from = "VISIT",values_from = "Date of vaccination")|>
                          select(1,4,6,2,3,5),by="Subject",relationship = "many-to-many")|>
  mutate("Dose 1"=as.numeric(`Visit date`-as.Date(`Dose 1`,"%d %m %Y")),"Dose 2"=as.numeric(`Visit date`-as.Date(`Dose 2`,"%d %m %Y")),
         "Dose 3"=as.numeric(`Visit date`-as.Date(`Dose 3`,"%d %m %Y")),"Booster"=as.numeric(`Visit date`-as.Date(`Booster`,"%d %m %Y")),
         "2B0"=as.numeric(`Visit date`-as.Date(`2B0 Vaccination`,"%d %m %Y")))|>
  rowwise()|>
  mutate(dosevisit=
           case_when(
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 1`~"Dose 1",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 2`~"Dose 2",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 3`~"Dose 3",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Booster`~"Booster",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`2B0`~"2B0"))

plasmod1<- suppressWarnings(fread(paste0(here(),"/study data/Blood Sample For Plasmodium Species Results Listing.csv"),
                                  colClasses = c("text","text","text","text","date",
                                                 "text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","text","date","date")))|>fill_blanks()

malcasecrfdbspos<-plasmod1|>
  filter(VISIT=="Malaria case report",`Type of blood sample collection`=="Blood film for Plasmodium species",!is.na(`Result of blood film for Plasmodium species`) & `Result of blood film for Plasmodium species`!="Negative for Plasmodium species")|>
  mutate(`First Data Time`=as.Date(`First Data Time`, format="%Y-%m-%d"))

malcasecrfrdtpos<-plasmod1|>
  filter(VISIT=="Malaria case report",`Type of blood sample collection`=="A P. falciparum rapid diagnosis test (RDT)",`Result of RDT`=="Positive for Plasmodium falciparum")

falsepos<-inner_join(malcasecrfdbspos,malcasecrfrdtpos,by=c("Subject","VISIT","Episode Number","Date of blood sample collection"))

plasample<- suppressWarnings(fread(paste0(here(),"/study data/Blood Sample Collection Listing.csv")))|>
  select(2,3,15,16,26)|>fill_blanks()

withvacdate<-withvacdate1|>
  group_by(dosevisit)|>
  summarise(count=n())|>
  mutate(percentage=(round(as.numeric(count/sum(count)*100),0)))|>
  adorn_totals("row")|>
  mutate(percentage=paste0(percentage,"%"))|>
  flextable()|>hline()|>align(align = "center", part = "body")|>
  set_caption(caption = "Malaria Cases Based on Malaria treatment: With the most recent Vaccination")

newsumm1<-left_join(malcasecrfdbspos,updatedcases<-vacdata|>mutate(Subject=as.character(Subject))|>
                      select(2,3,5)|>
                      pivot_wider(id_cols=Subject,names_from = "VISIT",values_from = "Date of vaccination")|>
                      select(1,4,6,2,3,5),by="Subject",relationship = "many-to-many")|>
  mutate("Dose 1"=as.numeric(as.Date(`Date of blood sample collection`,"%d-%b-%Y")-as.Date(`Dose 1`,"%d %m %Y")),"Dose 2"=as.numeric(as.Date(`Date of blood sample collection`,"%d-%b-%Y")-as.Date(`Dose 2`,"%d %m %Y")),
         "Dose 3"=as.numeric(as.Date(`Date of blood sample collection`,"%d-%b-%Y")-as.Date(`Dose 3`,"%d %m %Y")),"Booster"=as.numeric(as.Date(`Date of blood sample collection`,"%d-%b-%Y")-as.Date(`Booster`,"%d %m %Y")),
         "2B0"=as.numeric(as.Date(`Date of blood sample collection`,"%d-%b-%Y")-as.Date(`2B0 Vaccination`,"%d %m %Y")))|>
  rowwise()|>
  mutate(dosevisit=
           case_when(
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 1`~"Dose 1",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 2`~"Dose 2",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Dose 3`~"Dose 3",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`Booster`~"Booster",
             minpositive(c(`Dose 1`,`Dose 2`,`Dose 3`,`Booster`,`2B0`))==`2B0`~"2B0"))|>
  arrange(Subject,`Date of blood sample collection`)|>
  group_by(Subject)|>
  mutate(case_number2=paste0("MLR-",row_number()))|>ungroup()|>
  filter(!is.na(dosevisit))|>
  group_by(Subject)|>
  mutate(case_number=paste0("MLR-",row_number()))|>ungroup()

newsumm<-newsumm1|>
  group_by(dosevisit)|>
  summarise(count=n())|>
  mutate(percentage=(round(as.numeric(count/sum(count)*100),1)))|>
  adorn_totals("row")|>
  mutate(percentage=paste0(percentage,"%"))|>
  flextable()|>hline()|>align(align = "center", part = "body")|>
  set_caption(caption = "Malaria Cases as per positive plasmodium species results; with the most recent Vaccination")

newsumm2<-newsumm1|>
  group_by(case_number)|>
  summarise(count=n())|>
  mutate(percentage=(round(as.numeric(count/sum(count)*100),1)))|>
  adorn_totals("row")|>
  mutate(percentage=paste0(percentage,"%"))|>
  flextable()|>hline()|>align(align = "center", part = "body")|>
  set_caption(caption = "Malaria Cases as per positive plasmodium species results; as per the case number")
###+++++++++ end Malaria cases ++++++++++++######

###+ Medications ++++#####
cleanmeds<-left_join(medications|>
                       mutate(`Start date`=as.Date(`Start date`, "%d/%m/%Y"),
                              `Stop date`=as.Date(`Stop date`, "%d/%m/%Y"),
                              `Last Data Time`=as.Date(`Last Data Time`, "%d-%b-%Y %H:%M")),
                     durta<-left_join(dateofvisit|>
                                        dplyr::rename(VISDAT=`Visit date`),
                                      in_completed1|>
                                        dplyr::rename(Subject="Subject Id", VISIT=VISITNAME)|>mutate(VISDAT=as.Date(VISDAT, "%Y-%m-%d"))|>filter(VISIT%nin%c("Day 14","Day 42","Day 70","Day 140","Day 196","Day 292","Day 348","Day 404","B 14","B 84","B 140","B 224","B 280","B 336")),by=c("Subject","VISIT","VISDAT"),relationship = "many-to-many")|>
                       filter(EntryStatus!="Not Started")|>
                       select(2:3,18,23:26)|>
                       mutate(`Visit date`=as.Date(`First Data Time`,format="%d-%m-%Y %H:%M")),
                     by=c("Subject","Visit date"),relationship = "many-to-many")|>
  arrange(Subject)|>
  distinct(Subject,`Event code / Episode number`,`Generic Name / Active Ingredients`,.keep_all = TRUE)|>
  select(2,21,22,5:11,17,20,26)|>
  setnames(c("Subject","Visit","Date data entered","Indication","Episode number","Generic Name","Brand name","Start date","Ongoing","Stop date","Frequency","Last Data Time","Entered By"))|>
  filter(!is.na(Indication)&!is.na(`Episode number`))|>
  mutate("Entered By"=case_when(is.na(`Entered By`)~"Name missing",TRUE~`Entered By`))


## summarize meds as per closed and open
summa1<-cleanmeds|>
  mutate(closed=case_when(!is.na(`Stop date`)~"Closed", TRUE~"Open"))|>
  group_by(closed)|>
  dplyr::summarize(count=n())|>
  adorn_totals()|>flextable()|>autofit()
## open meds summaries
openau<-cleanmeds|>
  filter(is.na(`Stop date`))|>
  mutate(`other staff`=`Entered By`,`Entered By`=case_when(`Entered By` %nin% c("Joseph Ochieng Weya","Kevin Njogu","Martha Ndichu","Name missing","Sharon Nyaringa Omenda","Seriana Nyange")~"Other staff", TRUE~`Entered By`))

## summary of open meds
opensum1<-left_join(openau|>
                      group_by(`Entered By`)|>
                      dplyr::summarize(open=n())|>ungroup(),
                    openau|>
                      mutate(diff3=as.numeric(today()-`Start date`))|>filter(diff3>14)|>
                      group_by(`Entered By`)|>
                      dplyr::summarize("open>14days"=n())|>ungroup(), by="Entered By")|>
  arrange(desc(open))|>
  flextable()|>autofit()

# get the names for each tabPanel
josmed<-openau|>filter(`Entered By`=="Joseph Ochieng Weya")|>select(1:11)
kevmed<-openau|>filter(`Entered By`=="Kevin Njogu")|>select(1:11)
martmed<-openau|>filter(`Entered By`=="Martha Ndichu")|>select(1:11)
mismed<-openau|>filter(`Entered By`=="Name missing")|>select(1:11)
othermed<-openau|>filter(`Entered By`=="Other staff")|>select(1:11,14)
sharmed<-openau|>filter(`Entered By`=="Sharon Nyaringa Omenda")|>select(1:11)
sermed<-openau|>filter(`Entered By`=="Seriana Nyange")|>select(1:11)


###+end meds ++++####

###++++++ Weekly Summary ++++++#####
### Visits in the last week
scnpwk <- scheduler |>
  filter(exactDate!=today() & exactDate>today()-8)
scnpwkd<-scnpwk|>
  mutate(weekday=weekdays(exactDate))

if(weekdays(today())== "Monday"){
  scnpwkd$weekday<-factor(scnpwkd$weekday,levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday' ,'Friday','Saturday','Sunday'))
}else if(weekdays(today())== "Tuesday"){
  scnpwkd$weekday<-factor(scnpwkd$weekday,levels = c('Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday','Monday'))
}else if(weekdays(today())== "Wednesday"){
  scnpwkd$weekday<-factor(scnpwkd$weekday,levels = c('Wednesday', 'Thursday','Friday','Saturday','Sunday','Monday', 'Tuesday'))
}else if(weekdays(today())== "Thursday"){
  scnpwkd$weekday<-factor(scnpwkd$weekday,levels = c('Thursday','Friday','Saturday','Sunday','Monday', 'Tuesday', 'Wednesday'))
}else if(weekdays(today())== "Friday"){
  scnpwkd$weekday<-factor(scnpwkd$weekday,levels = c('Friday','Saturday','Sunday','Monday', 'Tuesday', 'Wednesday', 'Thursday'))
}

if(nrow(scnpwkd)>0){
  table4 <- table1(~ weekday | Site*visitCode, data=scnpwkd, topclass = "Rtable1-zebra", overall=F,caption = "Visits conducted in the previous week")|>
    t1kable()
}else{
  table4<-paste0("There were no scheduled visits conducted this week")
}

### Schedule for the next week
scnwkdayt1 <- scheduler |>
  filter(scheduledDate>=today() & scheduledDate<= today()+6, is.na(exactDate))|>
  mutate(weekday=weekdays(scheduledDate))
scnwkdayt2 <- scheduler |>
  filter(rescheduledDate>=today() & rescheduledDate<= today()+6, is.na(exactDate))|>
  mutate(weekday=weekdays(rescheduledDate))

scnwkdayt11 <- scheduler |>
  filter(scheduledDate== today()+7 & is.na(exactDate))|>
  mutate(weekday=weekdays(scheduledDate))
scnwkdayt21 <- scheduler |>
  filter(rescheduledDate== today()+7 & is.na(exactDate))|>
  mutate(weekday=weekdays(rescheduledDate))

scnwkdayt<-rbind(scnwkdayt1,scnwkdayt2)
scnwkdayt1n<-rbind(scnwkdayt11,scnwkdayt21)

scnwkdaytnew<-rbind(scnwkdayt,scnwkdayt1n)|>
  mutate(targetDate=coalesce(scheduledDate,rescheduledDate))|>
  select(1,2,13,10,11,12)|>
  dplyr::rename(scheduledDate=targetDate)|>
  mutate(scheduledDate=ymd(scheduledDate))|>
  arrange(scheduledDate,Site,group,visitCode,screenID)

if(weekdays(today())== "Monday"){
  scnwkdayt$weekday<-factor(scnwkdayt$weekday,levels = c('Monday','Tuesday', 'Wednesday', 'Thursday' ,'Friday','Saturday','Sunday'))
}else if(weekdays(today())== "Tuesday"){
  scnwkdayt$weekday<-factor(scnwkdayt$weekday,levels = c('Tuesday','Wednesday', 'Thursday', 'Friday','Saturday','Sunday','Monday'))
}else if(weekdays(today())== "Wednesday"){
  scnwkdayt$weekday<-factor(scnwkdayt$weekday,levels = c('Wednesday','Thursday','Friday','Saturday','Sunday','Monday', 'Tuesday'))
}else if(weekdays(today())== "Thursday"){
  scnwkdayt$weekday<-factor(scnwkdayt$weekday,levels = c('Thursday','Friday','Saturday','Sunday','Monday', 'Tuesday', 'Wednesday'))
}else if(weekdays(today())== "Friday"){
  scnwkdayt$weekday<-factor(scnwkdayt$weekday,levels = c('Friday','Saturday','Sunday','Monday', 'Tuesday', 'Wednesday', 'Thursday'))
}
if(nrow(scnwkdayt)>0){
  table3<-table1(~ weekday | Site*visitCode, data=scnwkdayt, topclass = "Rtable1-zebra", overall=F,caption = "Scheduled visits for the week")|>
    t1kable()
}else{
  table3<-paste0("There are no scheduled visits for the coming week")
}

###++++++ end weekly summary +++++####

###++++++ reconsent summary +++++####

fupdata2<-fupdata|>
  filter(currentStatus=="On Active Follow up")|>
  mutate(consentV6_0Accept=if_else(is.na(consentV6_0Accept), "Pending",consentV6_0Accept),
         consentV7_0Accept=if_else(is.na(consentV7_0Accept), "Pending",consentV7_0Accept))

###+Version 6.0
v6.0_summ<-fupdata2|> group_by(consentV6_0Accept)|> summarise(count=n())
cons<-fupdata2|>
  filter(consentV6_0Accept=="Yes")|>
  select(1,2,4:6)
refs<-fupdata2|>
  filter(consentV6_0Accept=="No")|>
  select(1,2,4:6)
pens<-fupdata2|>
  filter(consentV6_0Accept=="Pending")|>
  select(1:4)

###+Version 7.0
v7.0_summ<-fupdata2|> group_by(consentV7_0Accept)|> summarise(count=n())
cons7<-fupdata2|>
  filter(consentV7_0Accept=="Yes")|>
  select(1,2,4,7,8)
refs7<-fupdata2|>
  filter(consentV7_0Accept=="No")|>
  select(1,2,4,7,8)
pens7<-fupdata2|>
  filter(consentV7_0Accept=="Pending")|>
  select(1:4)


###++++++ end reconsent +++++###


###+ Dosing summary +###

dose_sum<- suppressWarnings(fread(here("study data/Dosing Summary.csv"),colClasses = c("text","numeric","text","text","numeric","date","text")))|>fill_blanks()

dsum1<-left_join(dose_sum|>
  mutate(VISIT= factor(VISIT, levels = c("Dose 1","Dose 2","Dose 3","Booster","Booster 2")))|>
  group_by(VISIT)|>
  summarise(Dosed = n()),

vacdata|>
  mutate(VISIT = if_else(VISIT=="2B0 Vaccination","Booster 2", VISIT),
         VISIT= factor(VISIT, levels = c("Dose 1","Dose 2","Dose 3","Booster","Booster 2")))|>
  group_by(VISIT)|>
  summarise(Vaccinated = n()))

notvax<- anti_join(dose_sum|>rename(Subject="Participant Id")|>mutate(VISIT= factor(VISIT, levels = c("Dose 1","Dose 2","Dose 3","Booster","Booster 2"))),derf<-vacdata|>
                     mutate(VISIT = case_when(VISIT=="2B0 Vaccination"~"Booster 2",TRUE ~ VISIT),
                            VISIT= factor(VISIT, levels = c("Dose 1","Dose 2","Dose 3","Booster","Booster 2"))), by=c("Subject", "VISIT"))

###+



######Users
user_base <- as_tibble(suppressWarnings(fread(here("study data/users.csv"))))

###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
ui <- fluidPage(
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),div(id="show-page-content",
                                        navbarPage("VAC078",
                                                   tabPanel("Consort",plotOutput("consort"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                            downloadButton("downloadData1", "download"),br(),
                                                            textOutput("consnarative"),
                                                            strong(h4("1. Summary of active participants and their catchment area")),
                                                            tableOutput("activepersite"),
                                                            strong(h4("2. Summary of enrolled participants and their catchment area")),
                                                            tableOutput("enrolpersite")
                                                   ),
                                                   tabPanel("Visit progress",
                                                            tabsetPanel(
                                                              tabPanel("Tabular",br(),uiOutput('schedulertablete')),
                                                              tabPanel("Graphical",girafeOutput('schedulergraph')),
                                                              tabPanel("Missed visits",DTOutput('misseds'))
                                                            )),
                                                   navbarMenu("Consenting",
                                                              tabPanel("Version 6.0",
                                                                       h3("Overall summary"),br(),uiOutput("v6.0_summ"),br(),
                                                                       tabsetPanel(
                                                                         tabPanel("Consented",downloadButton("downcons", "download"), DTOutput('cons')),
                                                                         tabPanel("Declined Consent",downloadButton("downrefs", "download"), DTOutput('refs')),
                                                                         tabPanel("Pending Consenting",downloadButton("downpens", "download"),DTOutput('pens'))
                                                                       )),
                                                              tabPanel("Version 7.0",
                                                                       h3("Overall summary"),br(),uiOutput("v7.0_summ"),br(),
                                                                       tabsetPanel(
                                                                         tabPanel("Consented",downloadButton("downcons7", "download"), DTOutput('cons7')),
                                                                         tabPanel("Declined Consent",downloadButton("downrefs7", "download"), DTOutput('refs7')),
                                                                         tabPanel("Pending Consenting",downloadButton("downpens7", "download"), DTOutput('pens7'))
                                                                       ))),
                                                   tabPanel("Weekly visits",
                                                            tabsetPanel(
                                                              tabPanel("Summary", br(),
                                                                       if(nrow(scnpwkd)>0){
                                                                         tableOutput('kable1')
                                                                       }else{
                                                                         textOutput('kable1one')
                                                                       },br(),if(nrow(scnwkdayt)>0){
                                                                         tableOutput('kable2')
                                                                       }else{
                                                                         textOutput('kable2one')
                                                                       }),
                                                              tabPanel("Due for visit",br(),DTOutput('LFUer')),
                                                              tabPanel("Scheduled visits for the week",downloadButton("downloadDatavis", "download"),br(),DTOutput('scnwkdaytnews')),
                                                              tabPanel("Expected Immunology bloods for the week",br(),DTOutput('immunology')))),
                                                   tabPanel("Queries",
                                                            tabsetPanel(
                                                              tabPanel("Summary", tableOutput('queriestab'),
                                                                       textOutput('querytxt1'),
                                                                       tableOutput('queriestab2'),
                                                                       p("The current query trends in order of the most common Open queries.")),
                                                              tabPanel("Open queries",downloadButton("downloadDataqu", "download"),
                                                                       DTOutput('openq')),
                                                              tabPanel("Incomplete records",
                                                                       DTOutput('incomprec')))),
                                                   tabPanel("E-Sign off",
                                                            tabsetPanel(
                                                              tabPanel("eSource pending", textOutput('signtxt1'),br(),DTOutput('esourcesign')),
                                                              tabPanel("ECRF pending", textOutput('signtxt2'),br(), DTOutput('ecrfsign')))),
                                                   tabPanel("Visit deviations",
                                                            tabsetPanel(
                                                              tabPanel("Summary of deviations",
                                                                       uiOutput("devflex")),
                                                              tabPanel("Deviations pending upload", DTOutput('querypend')))),
                                                   navbarMenu("Lab",
                                                              tabPanel("Summary",
                                                                       uiOutput("resflex1"),br(),h3("Samples Collected in the previous week"),
                                                                       tabsetPanel(
                                                                         tabPanel("Plasmodium",
                                                                                  DTOutput('plasres')),
                                                                         tabPanel("Haematology", DTOutput('haemres')),
                                                                         tabPanel("Biochemistry",DTOutput('biores'))
                                                                       )),
                                                              tabPanel("Results pending entry on eSource",
                                                                       tabsetPanel(
                                                                         tabPanel("Plasmodium",
                                                                                  DTOutput('plasnores')),
                                                                         tabPanel("Haematology",
                                                                                  DTOutput('haemnores')),
                                                                         tabPanel("Biochemistry",
                                                                                  DTOutput('bionores'))
                                                                       )),
                                                              tabPanel("Out of range results", downloadButton("downloadDataoor", "download"),
                                                                       DTOutput('hembioqry23')),
                                                              tabPanel("KIDMS vs eSource", downloadButton("downloadkdmsdat", "download"),
                                                                       DTOutput('entered_vbaya'))),
                                                   navbarMenu("Adverse Events",
                                                              tabPanel("Summary of AEs",
                                                                       tabsetPanel(
                                                                         tabPanel('Local solicited',br(), uiOutput('locflex1'),br(), uiOutput('locflex2')),
                                                                         tabPanel('Systemic solicited',br(), uiOutput('sysflex1'),br(), uiOutput('sysflex2')),
                                                                         tabPanel('Unsolicited',br(), uiOutput('unsolflex1'),br(), uiOutput('unsolflex2')),
                                                                         tabPanel('SAEs',br(),uiOutput('saeflex1'),br(), uiOutput('saeflex2'),br(),a(href="https://kemriwellcometrust-my.sharepoint.com/personal/somenda_kemri-wellcome_org/Documents/VAC078%20Study%20Coordination/VAC078%20Filing%20%26%20QC%20drive/Participant%20File%20Trackers/SAE%20reporting%20summary.xlsx?web=1", "For summary on required SAE submissions, follow this link👈"))
                                                                       )),
                                                              tabPanel("AEs open for +7days",h4("Last week AE closure summary"), uiOutput("sumari"),
                                                                       tabsetPanel(
                                                                         tabPanel('Kelvin',downloadButton("downloadData3", "download"), DTOutput('kelvina')),
                                                                         tabPanel('Joseph',downloadButton("downloadData4", "download"), DTOutput('josepha')),
                                                                         tabPanel('Martha',downloadButton("downloadData5", "download"), DTOutput('marthaa')),
                                                                         tabPanel('Seriana',downloadButton("downloadData8", "download"), DTOutput('seriana')),
                                                                         tabPanel('Chronic', downloadButton("downloadData6", "download"),DTOutput('chronica')),
                                                                         tabPanel('Pending link with clinic visit',downloadButton("downloadData7", "download"), DTOutput('sharona')),
                                                                         tabPanel('All Grade 3',downloadButton("downloadDatag3","download"), DTOutput('grade3'))
                                                                       ))),
                                                   navbarMenu("Medicine",
                                                              tabPanel("summary of Medications", uiOutput('summa1'),br(),uiOutput('opensum1')),
                                                              tabPanel("Open meds",
                                                                       tabsetPanel(
                                                                         tabPanel('Joseph',downloadButton("downloadmed", "download"), DTOutput('josmed')),
                                                                         tabPanel('Kelvin',downloadButton("downloadmed1", "download"), DTOutput('kevmed')),
                                                                         tabPanel('Martha',downloadButton("downloadmed2", "download"), DTOutput('martmed')),
                                                                         tabPanel('Sharon',downloadButton("downloadmed3", "download"), DTOutput('sharmed')),
                                                                         tabPanel('Seriana',downloadButton("downloadmed4", "download"), DTOutput('sermed')),
                                                                         tabPanel('Other staff',downloadButton("downloadmed5", "download"), DTOutput('othermed')),
                                                                         tabPanel('Missing',downloadButton("downloadmed6", "download"), DTOutput('mismed'))
                                                                       ))),
                                                   tabPanel("Malaria Cases",
                                                            tabsetPanel(
                                                              tabPanel("Summary of Malaria cases",br(),uiOutput('maltab1'),br(),uiOutput('maltab2'),br(),uiOutput('maltab3')),
                                                              tabPanel("Open malaria meds",downloadButton("downloadmaldata","download"),br(),DTOutput('ongoinmal')))),
                                                   tabPanel("End of Study",
                                                            tabsetPanel(
                                                              tabPanel('Required EOS forms', br(),uiOutput('sumaryreq'),br(),
                                                                       downloadButton("downloadData", "download"),br(),
                                                                       DTOutput('requiredEos')),
                                                              tabPanel("Forms pending upload",
                                                                       downloadButton("downloadData2", "download"), DTOutput('pending_completion'))
                                                            ))#,
                                                   # tabPanel("SAE's", imageOutput("saesa", width = "500px", height = "300px"))    
                                        ))|>shinyjs::hidden())
server <- function(input, output){
  
  shiny::observe({
    shiny::req(credentials()$user_auth)
    shinyjs::show(id="show-page-content")
  })
  
  output$consort<-renderPlot({study_consort |>
      ggplot() +
      geom_consort() +
      theme_consort(margin_h = 8, margin_v = 11)}, height = 670, width = 680 )
  output$consnarative<-renderText({
    paste0("This summary is at ",format(Sys.time(), "%B %d, %Y"))
  })
  output$activepersite<-renderTable({
    actpart<-fupdata|>
      filter(currentStatus=="On Active Follow up")|>
      tabyl(Site,group)|>
      adorn_totals(c("row","col"))|>
      adorn_percentages("col") |>
      adorn_pct_formatting(rounding = "half up", digits = 0) |>
      adorn_ns()
  })
  output$enrolpersite<-renderTable({
    enrpart<-fupdata|>
      tabyl(Site,group)|>
      adorn_totals(c("row","col"))|>
      adorn_percentages("col") |>
      adorn_pct_formatting(rounding = "half up", digits = 0) |>
      adorn_ns()
  })
  
  output$schedulertablete<-renderUI(
    schedulertable|>flextable()|>autofit()|>htmltools_value())
  output$schedulergraph<-renderGirafe({
    interractive<-ggplot(scheduler12,aes(x=`visitCode`,fill=`status`, data_id=`status`))+
      geom_bar_interactive(aes(tooltip = sprintf("%s: %.0f", fill, after_stat(count))),position = "dodge")+
      facet_wrap(~group, ncol = 1)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position = 'none')
    girafe(ggobj=interractive)
  })
  output$v6.0_summ<-renderUI(
    v6.0_summ|>flextable()|>autofit()|>htmltools_value())
  output$cons<-renderDT(
    cons|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  output$refs<-renderDT(
    refs|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  output$pens<-renderDT(
    pens|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  
  
  output$downcons <- downloadHandler(
    filename = function() {
      "Version6 consented.csv"
    },
    content = function(file) {
      write.csv(cons, file, row.names = TRUE)
    }
  )
  output$downrefs <- downloadHandler(
    filename = function() {
      "Version6 Refusals.csv"
    },
    content = function(file) {
      write.csv(refs, file, row.names = TRUE)
    }
  )
  output$downpens <- downloadHandler(
    filename = function() {
      "Version6 Pending.csv"
    },
    content = function(file) {
      write.csv(pens, file, row.names = TRUE)
    }
  )

  output$v7.0_summ<-renderUI(
    v7.0_summ|>flextable()|>autofit()|>htmltools_value())
  output$cons7<-renderDT(
    cons7|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  output$refs7<-renderDT(
    refs7|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  output$pens7<-renderDT(
    pens7|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 50,
      autoWidth=TRUE))
  output$downcons7 <- downloadHandler(
    filename = function() {
      "Version7 consented.csv"
    },
    content = function(file) {
      write.csv(cons7, file, row.names = TRUE)
    }
  )
  output$downrefs7 <- downloadHandler(
    filename = function() {
      "Version7 Refusals.csv"
    },
    content = function(file) {
      write.csv(refs7, file, row.names = TRUE)
    }
  )
  output$downpens7 <- downloadHandler(
    filename = function() {
      "Version7 Pending.csv"
    },
    content = function(file) {
      write.csv(pens7, file, row.names = TRUE)
    }
  )
  
  output$queriestab<-renderTable({
    queries
  })
  output$querytxt1<-renderText({
    paste0(nrow(queries_now), " queries have been generated for the Kilifi site since we started the study: Where ",nrow(queries_now|> filter(type=="Automatic"))," are Automatic queries and ",nrow(queries_now|> filter(type=="Manual"))," are Manual queries. ",nrow(queries_now|> filter(status=="Open Query"))," queries are open as of today.")
  })
  output$queriestab2<-renderTable({
    trends
  })
  output$openq<-renderDT(
    trends1|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$incomprec<-renderDT(
    anti_join(in_completed,tibble::tibble("Subject Id"=c(785100021,785100152,785100163,785100167,785100203,785100304,785100313,785100382,785100383,785100435,785100450,785100499,785100541,785100548,785100551,785100185,785100201,785100503),VISITNAME=c("Unscheduled - 3","Unscheduled - 2","Unscheduled - 13","Dose 1 (Pre-vaccination)","Dose 1 (Pre-vaccination)","Dose 3 (Pre-vaccination)","Unscheduled - 16","Unscheduled - 19","Unscheduled - 4","Unscheduled - 9","Unscheduled - 7","Unscheduled - 3","Unscheduled - 3","Unscheduled - 6","Unscheduled - 2","Unscheduled - 5","Unscheduled - 9","Unscheduled - 14")),by=c("Subject Id","VISITNAME"))|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$signtxt1<-renderText({
    paste0(nrow(esarce), " records are pending sign off on eSource as of today.")
  })
  output$signtxt2<-renderText({
    paste0(nrow(ecfe), " records are pending sign off on the ECRF as of today.")
  })
  output$esourcesign<-renderDT(
    esarce|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$ecrfsign<-renderDT(
    ecfe|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$devflex<-renderUI(
    yf|>autofit()|>htmltools_value())
  output$querypend<-renderDT(
    op_en|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$resflex1<-renderUI(
    results_tab|>htmltools_value())
  output$plasres<-renderDT(
    extract3|>filter(dbs=="Yes")|>select(1,2,7,8)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$haemres<-renderDT(
    extract3|>filter(hemblood=="Yes")|>select(1:4)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$biores<-renderDT(
    extract3|>filter(bioblood=="Yes")|>select(1,2,5,6)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$plasnores<-renderDT(
    notentereddbo|>select(1,2,7,8)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$haemnores<-renderDT(
    notenteredhemo|>select(1:4)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$bionores<-renderDT(
    notenteredbioo|>select(1,2,5,6)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$entered_vbaya<-renderDT(
    entered_vbaya|>convert_to_factors()|>arrange(Subject,sampledate),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$hembioqry23<-renderDT(
    hembioqry23|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  
  output$immunology<-renderDT(
    scheduler|>mutate(targetDate=coalesce(rescheduledDate,scheduledDate),
                      dayt=as.numeric(as.Date(targetDate)-today()))|>
      filter(visitCode%in%c("Day B365/B1Y0/2B0","Day 2B28","Day 2B180","Day 3B0","Day 3B28","Day 3B180","Day 3B365"),
             group=="Group A",
             dayt<=8,
             is.na(exactDate))|>
      select(1,2,12,10,11)|>
      arrange(targetDate,screenID),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE)
  )
  
  output$locflex1<-renderUI(
    lsae|>htmltools_value())
  output$sysflex1<-renderUI(
    ssae|>htmltools_value())
  output$unsolflex1<-renderUI(
    sumunsol|>htmltools_value())
  output$locflex2<-renderUI(
    lsae2|>htmltools_value())
  output$sysflex2<-renderUI(
    ssae2|>htmltools_value())
  output$unsolflex2<-renderUI(
    sumunsol2|>htmltools_value())
  output$saeflex1<-renderUI(
    g3summ1|>htmltools_value())
  output$saeflex2<-renderUI(
    g3summ2|>htmltools_value())
  output$summa1<-renderUI(
    summa1|>htmltools_value())
  output$opensum1<-renderUI(
    opensum1|>htmltools_value())
  output$josmed<-renderDT(
    josmed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed <- downloadHandler(
    filename = function() {
      "Joseph meds.csv"
    },
    content = function(file) {
      write.csv(josmed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$kevmed<-renderDT(
    kevmed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed1 <- downloadHandler(
    filename = function() {
      "Kelvin meds.csv"
    },
    content = function(file) {
      write.csv(kevmed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$martmed<-renderDT(
    martmed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed2 <- downloadHandler(
    filename = function() {
      "Martha meds.csv"
    },
    content = function(file) {
      write.csv(martmed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$mismed<-renderDT(
    mismed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed6 <- downloadHandler(
    filename = function() {
      "Name missing meds.csv"
    },
    content = function(file) {
      write.csv(mismed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$othermed<-renderDT(
    othermed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed5 <- downloadHandler(
    filename = function() {
      "Other staff meds.csv"
    },
    content = function(file) {
      write.csv(othermed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$sharmed<-renderDT(
    sharmed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed3 <- downloadHandler(
    filename = function() {
      "Sharon meds.csv"
    },
    content = function(file) {
      write.csv(sharmed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$sermed<-renderDT(
    sermed|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadmed4 <- downloadHandler(
    filename = function() {
      "Seriana meds.csv"
    },
    content = function(file) {
      write.csv(sermed|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$kelvina<-renderDT(
    kelvina|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$josepha<-renderDT(
    josepha|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$sharona<-renderDT(
    sharona|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$marthaa<-renderDT(
    marthaa|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$seriana<-renderDT(
    seriana|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$chronica<-renderDT(
    chronica|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$grade3<-renderDT(
    grade3|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$sumaryreq<-renderUI(
    sumaryreq|>htmltools_value())
  output$requiredEos<-renderDT(
    requiredEos|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$pending_completion<-renderDT(
    pending_completion|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$sumari<-renderUI(
    sumari|>htmltools_value())
  output$maltab1<-renderUI(
    withvacdate|>htmltools_value())
  output$maltab2<-renderUI(
    newsumm|>htmltools_value())
  output$maltab3<-renderUI(
    newsumm2|>htmltools_value())
  output$ongoinmal<-renderDT(
    ongoinmal|>select(2:13)|>arrange(Subject)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadkdmsdat <- downloadHandler(
    filename = function() {
      "Mismatched results.csv"
    },
    content = function(file) {
      write.csv(entered_vbaya|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$downloadmaldata <- downloadHandler(
    filename = function() {
      "ongoing malaria meds.csv"
    },
    content = function(file) {
      write.csv(ongoinmal|>select(2:13)|>arrange(Subject), file, row.names = TRUE)
    }
  )
  output$kable1 <- function() {
    table4|>
      kable_styling(position = "center",full_width = FALSE)
  }
  output$kable1one<-renderText(table4)
  output$kable2one<-renderText(table3)
  output$kable2 <- function() {
    table3|>
      kable_styling(position = "center",full_width = FALSE)
  }
  output$misseds<-renderDT(
    misseds|>select(1,2,4,10,11)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$LFUer<-renderDT(
    LFUer|>select(1,2,4,8:11)|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$scnwkdaytnews<-renderDT(
    scnwkdaytnew|>convert_to_factors(),
    filter = list(position="top",clear=TRUE),
    options = list(
      pageLength = 100,
      autoWidth=TRUE))
  output$downloadDatavis <- downloadHandler(
    filename = function() {
      "Scheduled visits for next week.csv"
    },
    content = function(file) {
      write.csv(scnwkdaytnew, file, row.names = TRUE)
    }
  )
  output$downloadDataoor <- downloadHandler(
    filename = function() {
      "out of range results.csv"
    },
    content = function(file) {
      write.csv(hembioqry23, file, row.names = TRUE)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "kelvin_AEs.csv"
    },
    content = function(file) {
      write.csv(kelvina, file, row.names = TRUE)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "joseph_AEs.csv"
    },
    content = function(file) {
      write.csv(josepha, file, row.names = TRUE)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "martha_AEs.csv"
    },
    content = function(file) {
      write.csv(marthaa, file, row.names = TRUE)
    }
  )
  output$downloadDataqu <- downloadHandler(
    filename = function() {
      "open Queries.csv"
    },
    content = function(file) {
      write.csv(trends1, file, row.names = TRUE)
    }
  )
  output$downloadData8 <- downloadHandler(
    filename = function() {
      "seriana_AEs.csv"
    },
    content = function(file) {
      write.csv(seriana, file, row.names = TRUE)
    }
  )
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "chronic_AEs.csv"
    },
    content = function(file) {
      write.csv(chronica, file, row.names = TRUE)
    }
  )
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "pendinglinkage_AEs.csv"
    },
    content = function(file) {
      write.csv(sharona, file, row.names = TRUE)
    }
  )
  output$downloadDatag3 <- downloadHandler(
    filename = function() {
      "allgrade3s.csv"
    },
    content = function(file) {
      write.csv(grade3, file, row.names = TRUE)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() {
      "requiredEOS.csv"
    },
    content = function(file) {
      write.csv(requiredEos, file, row.names = TRUE)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "pendingEOS.csv"
    },
    content = function(file) {
      write.csv(pending_completion, file, row.names = TRUE)
    }
  )
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "consort.png"
    },
    content = function(file) {
      ggsave(file,study_consort|>
               ggplot() +
               geom_consort() +
               theme_consort(margin_h = 10, margin_v = 8.5),height = 9, width = 12, dpi=300)
    }
  )
  output$saesa<-renderImage({
    filename<-normalizePath(file.path(here(), paste('SAE',input$saesa,".png",sep='')))
    list(src=filename, width="278%",height="224%")
    
  },deleteFile = FALSE)
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
}
shinyApp(ui = ui, server = server)
