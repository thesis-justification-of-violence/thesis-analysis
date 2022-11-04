###################### DOCUMENTO DE CALCULO PARA MODELOS OLS##################################
# Documentos asociados: 
# - analysis_mult_ols_elsocjv.Rmd

# 1. Cargar datos ----------------

load(url("https://github.com/thesis-justification-of-violence/thesis-analysis/raw/main/input/data/procjv_analysis_nona.RData"))

# 2. Recode datos

## 2.1 Justificación estudiantes tiren piedras

sjmisc::frq(procjv_analysis_nona$jv_est_2019_factor) # Ver frecuencuas

procjv_analysis_nona$jv_est_2019_factor <- car::Recode(procjv_analysis_nona$jv_est_2019_factor,
                                                       "
                                                       'Nunca se justifica' = 1;
                                                       'Pocas veces se justifica' = 2;
                                                       'Algunas veces se justifica' = 3;
                                                       'Muchas veces se justifica' = 4;
                                                       'Siempre se justifica' = 5
                                                       ") # Recodificar
sjmisc::frq(procjv_analysis_nona$jv_est_2019_factor) # Ver frecuenicas post rec

## 2.1 Justificación carabineros repriman marchas

sjmisc::frq(procjv_analysis_nona$jv_carab1_2019_factor) # Ver frecuencuas

procjv_analysis_nona$jv_carab1_2019_factor <- car::Recode(procjv_analysis_nona$jv_carab1_2019_factor,
                                                       "
                                                       'Nunca se justifica' = 1;
                                                       'Pocas veces se justifica' = 2;
                                                       'Algunas veces se justifica' = 3;
                                                       'Muchas veces se justifica' = 4;
                                                       'Siempre se justifica' = 5
                                                       ") # Recodificar
sjmisc::frq(procjv_analysis_nona$jv_carab1_2019_factor) # Ver frecuenicas post rec

## 2.1 Justificación carabineros desalojen tomas

sjmisc::frq(procjv_analysis_nona$jv_carab2_2019_factor) # Ver frecuencuas

procjv_analysis_nona$jv_carab2_2019_factor <- car::Recode(procjv_analysis_nona$jv_carab2_2019_factor,
                                                       "
                                                       'Nunca se justifica' = 1;
                                                       'Pocas veces se justifica' = 2;
                                                       'Algunas veces se justifica' = 3;
                                                       'Muchas veces se justifica' = 4;
                                                       'Siempre se justifica' = 5
                                                       ") # Recodificar
sjmisc::frq(procjv_analysis_nona$jv_carab2_2019_factor) # Ver frecuenicas post rec

# 3. Calculas modelos -------------

# 3.1  Justificación estudiantes tiren piedras ----------------------------------------------

jv_est_2019.modelmulpub1 <- lm(jv_est_2019_factor ~ 
                                  sj_ceo_rec + 
                                  sj_obrero_rec + 
                                  sj_entrevistado, 
                                data = procjv_analysis_nona)

jv_est_2019.modelmulpub2<- lm(jv_est_2019_factor ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor, 
                               data = procjv_analysis_nona)

jv_est_2019.modelmulpub3<- lm(jv_est_2019_factor ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor + 
                                 sdo_indice + 
                                 rwa_indice, 
                               data = procjv_analysis_nona)

jv_est_2019.modelmulpub4<- lm(jv_est_2019_factor ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor + 
                                 sdo_indice + 
                                 rwa_indice + 
                                 trato_salud_factor + 
                                 trato_carab_factor, 
                               data = procjv_analysis_nona)

jv_est_2019.modelmulpub5<- lm(jv_est_2019_factor ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor + 
                                 sdo_indice + 
                                 rwa_indice + 
                                 trato_salud_factor + 
                                 trato_carab_factor + 
                                 sexo_factor*sj_ceo_rec + 
                                 ingreso_satisfact_factor*sj_ceo_rec, 
                               data = procjv_analysis_nona)

# 3.2 Justificación carabineros repriman marchas -----------------------

jv_carab1_2019.modelmulpub1 <- lm(jv_carab1_2019_factor ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado, 
                                   data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub2<- lm(jv_carab1_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor, 
                                  data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub3<- lm(jv_carab1_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor + 
                                    sdo_indice + 
                                    rwa_indice, 
                                  data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub4<- lm(jv_carab1_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor + 
                                    sdo_indice + 
                                    rwa_indice + 
                                    trato_salud_factor + 
                                    trato_carab_factor, 
                                  data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub5<- lm(jv_carab1_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor + 
                                    sdo_indice + 
                                    rwa_indice + 
                                    trato_salud_factor + 
                                    trato_carab_factor + 
                                    educ_rec_factor*sj_obrero_rec, 
                                  data = procjv_analysis_nona)

# 3.3 Justificación carabineros desalojen tomas -----------------------------

jv_carab2_2019.modelmulpub1 <- lm(jv_carab2_2019_factor ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado_rec, 
                                   data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub2<- lm(jv_carab2_2019_factor ~ 
                                    sj_ceo_rec +
                                    sj_obrero_rec + 
                                    sj_entrevistado_rec +
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor +
                                    indigena_rec_factor,
                                  data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub3<- lm(jv_carab2_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado_rec + 
                                    ingreso +
                                    sexo_factor + 
                                    ingreso_satisfact_factor +
                                    educ_rec_factor + 
                                    indigena_rec_factor + 
                                    sdo_indice + 
                                    rwa_indice, 
                                  data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub4<- lm(jv_carab2_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec +
                                    sj_entrevistado_rec + 
                                    ingreso +
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor +
                                    indigena_rec_factor +
                                    sdo_indice + 
                                    rwa_indice + 
                                    trato_salud_factor +
                                    trato_carab_factor, 
                                  data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub5<- lm(jv_carab2_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado_rec + 
                                    ingreso +
                                    sexo_factor + 
                                    ingreso_satisfact_factor +
                                    educ_rec_factor + 
                                    indigena_rec_factor + 
                                    sdo_indice + 
                                    rwa_indice +
                                    trato_salud_factor + 
                                    trato_carab_factor + 
                                    ingreso*sj_obrero_rec + 
                                    ingreso_satisfact_factor*sj_obrero_rec,
                                  data = procjv_analysis_nona)
