################################################################################################
#
# Melanoma Population Attributable Fractions (PAFs)
#
# Primary analysis with NORDCAN population as reference
#
#
# PAF by country################################################################################################

setwd("")



# Acral melanoma rates

areas 



melanoma_PAF2_nordcan<- mela_inc %>%
  left_join(nordcan_ref, by = c("age" , "sex")) %>%
  dplyr::filter(age_cat != "0-30") %>%
  droplevels() %>%
  ungroup() %>%
  group_by(sex, age) %>% # uncertainty calculated within within each age group for both sexes
  # dplyr::mutate(af.uv = case_when(inc_rate != 0 ~ (inc_rate - ref_inc) / inc_rate,
  #                                 inc_rate == 0 ~ 0)) %>%
  #simply subtracting acral rate
  # dplyr::mutate(af.uv = case_when(inc_rate != 0 ~ (inc_rate-rate_acral - ref_inc) / inc_rate,
  #                                 inc_rate == 0 ~ 0)) %>%
  #Prop method
  dplyr::mutate(af.uv = case_when(inc_rate != 0 ~ (inc_rate*prop_acral - ref_inc) / inc_rate,
                                  inc_rate == 0 ~ 0)) %>%
  dplyr::select(-prop_acral)%>%
  dplyr::mutate(af.uv = case_when(af.uv < 0 ~ 0,
                                  TRUE ~ af.uv)) %>%
  dplyr::mutate(varaf = var(af.uv))%>%
  dplyr::mutate(laf.uv = case_when(af.uv > 0 ~ log(1 - af.uv),
                                   TRUE ~ 0)) %>%
  group_by(sex, age)%>% # uncertainty calculated within within each age group for both sexes
  dplyr::mutate(
    af.uv.low = 1 - exp(laf.uv + 1.96 * sqrt(varaf)),
    af.uv.upp = 1 - exp(laf.uv - 1.96 * sqrt(varaf))) %>%
  group_by(country_code, sex, age) %>%
  dplyr::mutate(cases.prev = cases * af.uv) %>%
  dplyr::mutate(cases.prev.low = cases * af.uv.low) %>%
  dplyr::mutate(cases.prev.upp = cases * af.uv.upp) %>%
  ungroup()%>%
  dplyr::select(-varaf,-laf.uv)%>%
  dplyr::mutate(af.uv = case_when(is.na(af.uv) ~ 0,
                                      af.uv.low!=0~ af.uv))%>%
  dplyr::mutate(af.uv.low = case_when(is.na(af.uv.low) ~ 0,
                                      af.uv.low!=0~ af.uv.low))%>%
  dplyr::mutate(af.uv.upp = case_when(is.na(af.uv.upp) ~ 0,
                                      af.uv.low!=0~ af.uv.upp))%>%
  #Making sure the zero PAF values are not included in the sum
  dplyr::mutate(af.uv.upp = case_when(af.uv==0 | (af.uv==0 & af.uv.upp>0) ~ 0,
                                      af.uv!=0~ af.uv.upp))%>%
  dplyr::mutate(af.uv.low = case_when(af.uv==0 | (af.uv==0 & af.uv.low<0)~ 0,
                                      af.uv!=0~ af.uv.low))%>%
  dplyr::mutate(cases.prev.upp = case_when(cases.prev==0 | (cases.prev==0 & cases.prev.upp>0) ~ 0,
                                      cases.prev!=0~ cases.prev.upp))%>%
  dplyr::mutate(cases.prev.low = case_when(cases.prev==0 | (cases.prev==0 & cases.prev.low<0) ~ 0,
                                      cases.prev!=0~ cases.prev.low))%>%
  #Filtering so all cases where it overlaps zero are zero so they are excluded from the final sum
  dplyr::mutate(af.uv = case_when(af.uv.low>=0 ~ af.uv,
                                      af.uv.low<0~ 0))%>%
  dplyr::mutate(af.uv.upp = case_when(af.uv.low>=0 ~ af.uv.upp,
                af.uv.low<0~ 0))%>%
  dplyr::mutate(af.uv.low = case_when(af.uv.low>=0 ~ af.uv.low,
                af.uv.low<0~ 0))%>%
  dplyr::mutate(cases.prev = case_when(cases.prev.low>=0 ~ cases.prev,
                cases.prev.low<0~ 0))%>%
  dplyr::mutate(cases.prev.upp = case_when(cases.prev.low>=0 ~ cases.prev.upp,
                cases.prev.low<0~ 0))%>%
  dplyr::mutate(cases.prev.low = case_when(cases.prev.low>=0 ~ cases.prev.low,
                cases.prev.low<0~ 0))%>%
  dplyr::select(-ref_inc,-inc_rate) 
  
  
melanoma_PAF_nordcan<-melanoma_PAF2_nordcan%>%
  dplyr::mutate(sex = 3) %>%
  group_by(country_code,sex, age) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  ungroup() %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  distinct() %>%
  ungroup()%>%

  full_join(melanoma_PAF2_nordcan)



melanoma_PAF_overall_by_sex_nordcan <- melanoma_PAF_nordcan %>%
  dplyr::select(-age) %>%
  group_by(age_cat, country_code, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  ungroup() %>%
  distinct() %>%
  dplyr::mutate(af.uv = case_when(is.na(af.uv) ~ 0,
                                  af.uv.low!=0~ af.uv))%>%
  dplyr::mutate(af.uv.low = case_when(is.na(af.uv.low) ~ 0,
                                      af.uv.low!=0~ af.uv.low))%>%
  dplyr::mutate(af.uv.upp = case_when(is.na(af.uv.upp) ~ 0,
                                      af.uv.low!=0~ af.uv.upp))


melanoma_PAF_overall_nordcan<-melanoma_PAF_nordcan %>% 
  dplyr::select( -age) %>%
  mutate(age_cat="Overall")%>%
  group_by(country_code,sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  ungroup() %>%
  distinct() %>%

  dplyr::mutate(af.uv = case_when(is.na(af.uv) ~ 0,
                                  af.uv.low!=0~ af.uv))%>%
  dplyr::mutate(af.uv.low = case_when(is.na(af.uv.low) ~ 0,
                                      af.uv.low!=0~ af.uv.low))%>%
  dplyr::mutate(af.uv.upp = case_when(is.na(af.uv.upp) ~ 0,
                                      af.uv.low!=0~ af.uv.upp))


melanoma_PAF_overall_by_sex2_nordcan <- melanoma_PAF_overall_by_sex_nordcan %>%
  full_join(melanoma_PAF_overall_nordcan)%>%
  distinct() %>%
  group_by(country_code, age_cat, sex)%>%
  dplyr::mutate(rate.prev=sum(cases.prev/py*10**5)) %>% 
  dplyr::mutate(rate.prev.low=sum(cases.prev.low/py*10**5)) %>%
  dplyr::mutate(rate.prev.upp=sum(cases.prev.upp/py*10**5)) %>%
  ungroup() %>%
  dplyr::select(country_code, 
         country_label,
         cancer_code,
         cancer_label,
         age_cat,
         sex,
         cases,
         cases.prev.low,
         cases.prev,
         cases.prev.upp,
         af.uv.low,
         af.uv,
         af.uv.upp, 
         rate.prev, rate.prev.low, rate.prev.upp) %>%
  dplyr::mutate(across(8:10, round,0)) %>%
  dplyr::mutate(across(11:13, round, 3) * 100) %>%
  dplyr::mutate(af.uv = case_when(is.na(af.uv) ~ 0,
                                  af.uv.low!=0~ af.uv))%>%
  dplyr::mutate(af.uv.low = case_when(is.na(af.uv.low) ~ 0,
                                      af.uv.low!=0~ af.uv.low))%>%
  dplyr::mutate(af.uv.upp = case_when(is.na(af.uv.upp) ~ 0,
                                      af.uv.low!=0~ af.uv.upp))

age_check_1_nordcan<- melanoma_PAF_overall_by_sex2_nordcan%>%
  filter(age_cat=="30-49")%>%
  dplyr::select(country_label)%>%
  distinct() #155

age_check_2_nordcan<- melanoma_PAF_overall_by_sex2_nordcan%>%
  filter(age_cat=="50-69")%>%
  dplyr::select(country_label)%>%
  distinct() #155

age_check_3_nordcan <- melanoma_PAF_overall_by_sex2_nordcan%>%
  filter(age_cat=="70-99")%>%
  dplyr::select(country_label)%>%
  distinct() #155




PAF_uv_region_final_by_sexes_nordcan <- melanoma_PAF_nordcan %>%
  dplyr::select( -age) %>%
  left_join(HDI_Region_Mapping2)%>%
  dplyr::select(-country_label,-country_code)%>%
  left_join(areas)%>%
  group_by(area, age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  ungroup() %>%
  distinct() 

melanoma_PAF_Region_overall_nordcan<-PAF_uv_region_final_by_sexes_nordcan %>%
  ungroup()%>%
  mutate(age_cat="Overall")%>%
  group_by(area, age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases))%>%
  dplyr::mutate(py=sum(py)) %>%
  ungroup()%>%
  distinct()

PAF_uv_region2_nordcan <- PAF_uv_region_final_by_sexes_nordcan %>%
  distinct() %>%
  full_join(melanoma_PAF_Region_overall_nordcan)%>%
  group_by(area, age_cat, sex)%>%
  dplyr::mutate(rate.prev=sum(cases.prev/py*10**5)) %>%  
  dplyr::mutate(rate.prev.low=sum(cases.prev.low/py*10**5)) %>%   
  dplyr::mutate(rate.prev.upp=sum(cases.prev.upp/py*10**5)) %>%
  ungroup() %>%
  arrange(area,age_cat,sex)%>%
  dplyr::rename("country_code"="area")%>%
  dplyr::select(country_code, 
         country_label,
         cancer_code,
         cancer_label,
         age_cat,
         sex,
         cases,
         cases.prev.low,
         cases.prev,
         cases.prev.upp,
         af.uv.low,
         af.uv,
         af.uv.upp,
         rate.prev, rate.prev.low, rate.prev.upp) %>%
  dplyr::mutate(across(8:10, round,0)) %>%
  dplyr::mutate(across(11:13, round, 3) * 100) %>%
  dplyr::mutate(across(14:16, round, 1)) %>%
  as.data.frame()



#Total risk factor cases overall



PAF_uv_total_final_by_sexes_nordcan <- melanoma_PAF_nordcan %>%
  dplyr::select( -age) %>%
  dplyr::select(-country_label,-country_code)%>%
  group_by(age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  ungroup() %>%
  distinct() %>%
  mutate(country_label="All countries")

melanoma_PAF_Total_overall_nordcan<-PAF_uv_total_final_by_sexes_nordcan %>%
  ungroup()%>%
  mutate(age_cat="Overall")%>%
  group_by(age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  ungroup()%>%
  distinct()


PAF_uv_total2_nordcan <- PAF_uv_total_final_by_sexes_nordcan %>% #Error to fix
  ungroup() %>%
  distinct() %>%
  full_join(melanoma_PAF_Total_overall_nordcan) %>%
  dplyr::mutate( country_code=1000) %>%
  arrange(age_cat,sex) %>%
  group_by(age_cat, sex) %>%
  dplyr::mutate(rate.prev=sum(cases.prev/py*10**5)) %>%   dplyr::mutate(rate.prev.low=sum(cases.prev.low/py*10**5)) %>%   dplyr::mutate(rate.prev.upp=sum(cases.prev.upp/py*10**5)) %>%
  ungroup()%>%
  dplyr::select(country_code, 
         country_label,
         cancer_code,
         cancer_label,
         age_cat,
         sex,
         cases,
         cases.prev.low,
         cases.prev,
         cases.prev.upp,
         af.uv.low,
         af.uv,
         af.uv.upp,
         rate.prev, rate.prev.low, rate.prev.upp) %>%
  dplyr::mutate(across(8:10, round,0)) %>%
  dplyr::mutate(across(11:13, round, 3) * 100) %>%
  dplyr::mutate(across(14:16, round, 1)) %>%
  as.data.frame()



PAF_uv_total2_nordcan

#Analysis by HDI group


PAF_uv_hdi_final_by_sexes_nordcan <- melanoma_PAF_nordcan %>%
  dplyr::select(-age) %>%
  left_join(HDI) %>%
  dplyr::select(-country_code,-country_label) %>%
  dplyr::rename("country_label" = "hdi_group") %>%
  group_by(country_label, age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases)) %>%
  dplyr::mutate(py=sum(py)) %>%
  distinct() %>%
  dplyr::mutate(af.uv = as.numeric(af.uv)) %>%
  ungroup() %>%
  distinct()


melanoma_PAF_HDI_overall_nordcan<-PAF_uv_hdi_final_by_sexes_nordcan %>%
  ungroup()%>%
  mutate(age_cat="Overall")%>%
  group_by(country_label,age_cat, sex) %>%
  dplyr::mutate(cases = sum(cases)) %>%
  dplyr::mutate(cases.prev = sum(cases.prev)) %>%
  dplyr::mutate(cases.prev.low = sum(cases.prev.low)) %>%
  dplyr::mutate(cases.prev.upp = sum(cases.prev.upp)) %>%
  dplyr::mutate(af.uv.low = sum(cases.prev.low) / sum(cases)) %>%
  dplyr::mutate(af.uv = sum(cases.prev) / sum(cases)) %>%
  dplyr::mutate(af.uv.upp = sum(cases.prev.upp) / sum(cases))%>%
  dplyr::mutate(py=sum(py)) %>%
  ungroup()%>%
  distinct()


PAF_uv_HDI2_nordcan <-  PAF_uv_hdi_final_by_sexes_nordcan %>%
  distinct() %>%
  ungroup() %>%
  full_join(melanoma_PAF_HDI_overall_nordcan)%>%
  
  mutate(country_code=country_label+30)%>% #coding for the tables formatting
  mutate(country_code=as.double(country_code))%>%
  mutate(country_label=as.character(country_label))%>%
  arrange(country_code,age_cat,sex)%>%
  group_by(country_code, age_cat, sex)%>%
  dplyr::mutate(rate.prev=sum(cases.prev/py*10**5)) %>%   dplyr::mutate(rate.prev.low=sum(cases.prev.low/py*10**5)) %>%   dplyr::mutate(rate.prev.upp=sum(cases.prev.upp/py*10**5)) %>%
  ungroup()%>%
  dplyr::select(country_code,
         country_label,
         cancer_code,
         cancer_label,
         age_cat,
         sex,
         cases,
         cases.prev.low,
         cases.prev,
         cases.prev.upp,
         af.uv.low,
         af.uv,
         af.uv.upp,
         rate.prev, rate.prev.low, rate.prev.upp) %>%
  dplyr::mutate(across(8:10, round,0)) %>%
  dplyr::mutate(across(11:13, round, 3) * 100)  %>%
  dplyr::mutate(across(14:16, round, 1)) %>%
  as.data.frame()


  
  
#Computing ASRs and adding them to the data frames:

PAF_uv_region2_nordcan_asrs <-  melanoma_PAF_nordcan %>%
  left_join(HDI_Region_Mapping2)%>%
  dplyr::select(-country_label,-country_code)%>%
  left_join(areas)%>%
  distinct() %>%
    ungroup() %>%
  arrange(area,age_cat,sex)%>%
  dplyr::rename("country_code"="area")
 



PAF_uv_region2_nordcan_asr<-core.csu_asr(PAF_uv_region2_nordcan_asrs,
        var_age = "age",
        var_cases = "cases.prev",
        var_py ="py",
        group_by=c("country_code", "country_label","sex", "cancer_code", "cancer_label"),
        var_age_group=NULL,
        missing_age = c(1:6),
        db_rate = 100000,
        first_age = 7,
        last_age = 18,
        pop_base = "SEGI",
        crude_rate = FALSE,
        var_st_err = "st_err",
        correction_info = FALSE,
        var_asr = "asr",
        age_dropped = TRUE)
        

PAF_uv_region2_nordcan_asr <- PAF_uv_region2_nordcan_asr %>%
  mutate(age_cat = "Overall") %>%
  dplyr::select(-cases.prev, -py) %>%
  mutate(asr.low = round(asr - 1.96 * st_err, 1),
         asr.upp = round(asr + 1.96 * st_err, 1),
         asr = round(asr, 1)) %>%
  dplyr::select(-st_err)


PAF_uv_region2_nordcan<-PAF_uv_region2_nordcan%>%
left_join(PAF_uv_region2_nordcan_asr, 
          by=c("country_code", "country_label","sex", "cancer_code", "cancer_label", "age_cat"))



PAF_uv_total2_nordcan_asrs <-  melanoma_PAF_nordcan %>%
  dplyr::select(-country_label,-country_code)%>%
  mutate(country_label="All countries",country_code=1000)
 
 


PAF_uv_total2_nordcan_asr<-core.csu_asr(PAF_uv_total2_nordcan_asrs,
        var_age = "age",
        var_cases = "cases.prev",
        var_py ="py",
        group_by=c("country_code","country_label","sex" , "cancer_code" , "cancer_label"),
        var_age_group=NULL,
        missing_age = NULL,
        db_rate = 100000,
        first_age = 7,
        last_age = 18,
        pop_base = "SEGI",
        crude_rate = FALSE,
        var_st_err = "st_err",
        correction_info = FALSE,
        var_asr = "asr",
        age_dropped = TRUE)
        
PAF_uv_total2_nordcan_asr<-PAF_uv_total2_nordcan_asr%>%
        mutate(age_cat="Overall")%>%
        dplyr::select(-cases.prev, -py)%>%
  mutate(asr.low = round(asr - 1.96 * st_err, 1),
         asr.upp = round(asr + 1.96 * st_err, 1),
         asr = round(asr, 1)) %>%
        dplyr::select(-st_err)


PAF_uv_total2_nordcan<-PAF_uv_total2_nordcan%>%
left_join(PAF_uv_total2_nordcan_asr, by=c("country_code" , "country_label","sex" , "cancer_code" , "cancer_label" , "age_cat"))

#HDI

PAF_uv_HDI2_nordcan_asrs <-  melanoma_PAF_nordcan %>%
  left_join(HDI) %>%
  dplyr::select(-country_code,-country_label) %>%
  dplyr::rename("country_label" = "hdi_group") %>%
    mutate(country_code=as.numeric(country_label)+30,
    country_label=as.character(country_label)) #coding for the tables formatting
 



PAF_uv_HDI2_nordcan_asr<-core.csu_asr(PAF_uv_HDI2_nordcan_asrs,
        var_age = "age",
        var_cases = "cases.prev",
        var_py ="py",
        group_by=c("country_code" , "country_label","sex" , "cancer_code" , "cancer_label"),
        var_age_group=NULL,
        missing_age = NULL,
        db_rate = 100000,
        first_age = 7,
        last_age = 18,
        pop_base = "SEGI",
        crude_rate = FALSE,
        var_st_err = "st_err",
        correction_info = FALSE,
        var_asr = "asr",
        age_dropped = TRUE)
        
PAF_uv_HDI2_nordcan_asr<-PAF_uv_HDI2_nordcan_asr%>%
        mutate(age_cat="Overall")%>%
        dplyr::select(-cases.prev, -py)%>%
  mutate(asr.low = round(asr - 1.96 * st_err, 1),
         asr.upp = round(asr + 1.96 * st_err, 1),
         asr = round(asr, 1)) %>%
        mutate(across(contains("asr"), round, 1))%>%
        dplyr::select(-st_err)


PAF_uv_HDI2_nordcan<-PAF_uv_HDI2_nordcan%>%
left_join(PAF_uv_HDI2_nordcan_asr, by=c("country_code" , "country_label","sex" , "cancer_code" , "cancer_label" , "age_cat"))




#By country

melanoma_PAF_nordcan 


melanoma_PAF_overall_by_sex2_nordcan_asr<-core.csu_asr(melanoma_PAF_nordcan ,
        var_age = "age",
        var_cases = "cases.prev",
        var_py ="py",
        group_by=c("country_code" , "country_label","sex", 
        "cancer_code" , "cancer_label"),
        var_age_group=NULL,
        missing_age = NULL,
        db_rate = 100000,
        first_age = 7,
        last_age = 18,
        pop_base = "SEGI",
        crude_rate = FALSE,
        var_st_err = "st_err",
        correction_info = FALSE,
        var_asr = "asr",
        age_dropped = TRUE)
        
melanoma_PAF_overall_by_sex2_nordcan_asr<-melanoma_PAF_overall_by_sex2_nordcan_asr%>%
        mutate(age_cat="Overall")%>%
        dplyr::select(-cases.prev, -py)%>%
  mutate(asr.low = round(asr - 1.96 * st_err, 1),
         asr.upp = round(asr + 1.96 * st_err, 1),
         asr = round(asr, 1)) %>%
        mutate(across(contains("asr"), round, 1))%>%
        dplyr::select(-st_err)


melanoma_PAF_overall_by_sex2_nordcan<-melanoma_PAF_overall_by_sex2_nordcan%>%
left_join(melanoma_PAF_overall_by_sex2_nordcan_asr, by=c("country_code" , "country_label","cancer_code","cancer_label","sex",  "age_cat"))


#Final tables...

PAF_uv_region2_nordcan
PAF_uv_total2_nordcan
PAF_uv_HDI2_nordcan
melanoma_PAF_overall_by_sex2_nordcan


#combined table

table_UV_PAF_nordcan <- PAF_uv_region2_nordcan %>%
  full_join(PAF_uv_HDI2_nordcan) %>%
  full_join(PAF_uv_total2_nordcan) %>%
  as.data.frame()%>%
  arrange(sex, age_cat, country_code)

table_UV_PAF2_nordcan<-table_UV_PAF_nordcan%>%
  mutate("Preventable Cases" = paste0(format(cases.prev, big.mark = ","), " (", format(cases.prev.low, big.mark = ","), " , ", format(cases.prev.upp, big.mark = ","), ")"),
         "Population Attributable Fraction (%)" = paste0(af.uv, " (", af.uv.low, " , ", af.uv.upp, ")"),
         rate.prev = paste0(rate.prev, " (", rate.prev.low, " , ", rate.prev.upp, ")"),
         rate.prev.asr = paste0(asr, " (", asr.low, " , ", asr.upp, ")"),
  ) %>%
  dplyr::rename("Country" = "country_label") %>%
  dplyr::rename("Age Group" = "age_cat") %>%
  dplyr::select("sex",
         "country_code",
    "Country",
    "Age Group",
    "Preventable Cases",
   "Population Attributable Fraction (%)",
  rate.prev,
  rate.prev.asr
  )

melanoma_PAF_overall_by_sex3_nordcan<-melanoma_PAF_overall_by_sex2_nordcan%>%
  arrange(sex, age_cat, country_code)%>%
 mutate("Preventable Cases" = paste0(format(cases.prev, big.mark = ","), " (", format(cases.prev.low, big.mark = ","), " , ", format(cases.prev.upp, big.mark = ","), ")"),
         "Population Attributable Fraction (%)" = paste0(af.uv, " (", af.uv.low, " , ", af.uv.upp, ")"),
         rate.prev = paste0(rate.prev, " (", rate.prev.low, " , ", rate.prev.upp, ")"),
         rate.prev.asr = paste0(asr, " (", asr.low, " , ", asr.upp, ")"),
  ) %>%
  dplyr::rename("Country" = "country_label") %>%
  dplyr::rename("Age Group" = "age_cat") %>%
  dplyr::select("sex",
         "country_code",
    "Country",
    "Age Group",
    "Preventable Cases",
   "Population Attributable Fraction (%)",
  rate.prev,
  rate.prev.asr
  )

#printing final tables

melanoma_PAF_overall_by_sex3_nordcan
table_UV_PAF2_nordcan



#writing the tables

regcheck<-PAF_uv_region2_nordcan%>%dplyr::select(country_label, country_code,sex, age_cat)%>%distinct()

# Numbers for discussion section

#Checking population percentage in each HDI group
pop_size<-globocan%>%
  filter(sex!=0)%>%
  dplyr::select(country_code, country_label, py)%>%
  ungroup()%>%
  distinct()%>%
  mutate(total=sum(py))%>%
  left_join(HDI)%>%
  dplyr::select(-country_label, -country_code)%>%
  group_by(hdi_group)%>%
  mutate(py_by_hdi = sum(py))%>%
  ungroup()%>%
  dplyr::select(-py)%>%
  distinct()%>%
  mutate(popprop_hdi=py_by_hdi/total)


#Checking incidence percentage in each HDI group
inc_size <- globocan%>%
  filter(sex!=0)%>%
  dplyr::select(country_code, country_label, cases)%>%
  ungroup()%>%
  distinct()%>%
  mutate(total=sum(cases))%>%
  left_join(HDI)%>%
  dplyr::select(-country_label, -country_code)%>%
  group_by(hdi_group)%>%
  mutate(inc_by_hdi = sum(cases))%>%
  ungroup()%>%
  dplyr::select(-cases)%>%
  distinct()%>%
  mutate(inc_hdi=inc_by_hdi/total)


# checking how much incidence our study covers

mela_inc_countries<-mela_inc%>%dplyr::select(country_code, country_label)%>%distinct()


#Checking population percentage in all included countries
pop_size_countries <- globocan%>%
  filter(sex!=0)%>%
  dplyr::select(country_code, country_label, py)%>%
  ungroup()%>%
  distinct()%>%
  mutate(total=sum(py))%>%
  filter(country_code%in%mela_inc_countries$country_code)%>%
  dplyr::select(-country_code,-country_label)%>%
  mutate(py = sum(py))%>%
  ungroup()%>%
  distinct()%>%
  mutate(popprop=py/total)


#Checking incidence percentage in all countries
inc_size_countries <- globocan%>%
  filter(sex!=0)%>%
  dplyr::select(country_code, country_label, cases)%>%
  ungroup()%>%
  distinct()%>%
  mutate(total=sum(cases))%>%
  filter(country_code%in%mela_inc_countries$country_code)%>%
  dplyr::select(-country_code,-country_label)%>%
  mutate(inc = sum(cases))%>%
  ungroup()%>%
  dplyr::select(-cases)%>%
  distinct()%>%
  mutate(inc_prop=inc/total)




