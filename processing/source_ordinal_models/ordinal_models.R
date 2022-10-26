###################### DOCUMENTO DE CALCULO PARA MODELOS ORDINALES ##################################
# Documentos asociados: 
# - analysis_mult_elsocjv.Rmd
# - analysis_mult_brant.Rmd
# - analysis_mult_plots.Rmd
# - analysis_mult_r2.Rmd

# 1. Cargar datos ----------------

load(url("https://github.com/thesis-justification-of-violence/thesis-analysis/raw/main/input/data/procjv_analysis_nona.RData"))

# 2. Calculas modelos -------------

# 2.1  Justificación estudiantes tiren piedras ----------------------------------------------

jv_est_2019.modelmulpub1 <- clm(jv_est_2019_factor ~ 
                                  sj_ceo_rec + 
                                  sj_obrero_rec + 
                                  sj_entrevistado, 
                                data = procjv_analysis_nona)

jv_est_2019.modelmulpub2<- clm(jv_est_2019_factor ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor, 
                               data = procjv_analysis_nona)

jv_est_2019.modelmulpub3<- clm(jv_est_2019_factor ~ 
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

jv_est_2019.modelmulpub4<- clm(jv_est_2019_factor ~ 
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

jv_est_2019.modelmulpub5<- clm(jv_est_2019_factor ~ 
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

# 2.2 Justificación carabineros repriman marchas -----------------------

jv_carab1_2019.modelmulpub1 <- clm(jv_carab1_2019_factor ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado, 
                                   data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub2<- clm(jv_carab1_2019_factor ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor, 
                                  data = procjv_analysis_nona)

jv_carab1_2019.modelmulpub3<- clm(jv_carab1_2019_factor ~ 
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

jv_carab1_2019.modelmulpub4<- clm(jv_carab1_2019_factor ~ 
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

jv_carab1_2019.modelmulpub5<- clm(jv_carab1_2019_factor ~ 
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

# 2.3 Justificación carabineros desalojen tomas -----------------------------

jv_carab2_2019.modelmulpub1 <- clm(jv_carab2_2019_factor ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado_rec, 
                                   data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub2<- clm(jv_carab2_2019_factor ~ 
                                    sj_ceo_rec +
                                    sj_obrero_rec + 
                                    sj_entrevistado_rec +
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor +
                                    indigena_rec_factor,
                                  data = procjv_analysis_nona)

jv_carab2_2019.modelmulpub3<- clm(jv_carab2_2019_factor ~ 
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

jv_carab2_2019.modelmulpub4<- clm(jv_carab2_2019_factor ~ 
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

jv_carab2_2019.modelmulpub5<- clm(jv_carab2_2019_factor ~ 
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
