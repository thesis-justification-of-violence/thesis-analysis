###################### DOCUMENTO DE CALCULO PARA MODELOS BINARIOS ##################################
# Documentos asociados: 
# - analysis_mult_binary_5vs1234.Rmd


# 1. Cargar datos ----------------

load(url("https://github.com/thesis-justification-of-violence/thesis-analysis/raw/main/input/data/procjv_analysis_nona.RData"))

procjv_analysis_nona$jv_est_2019_bin[procjv_analysis_nona$jv_est_2019_factor == "Nunca se justifica"] <- 0
procjv_analysis_nona$jv_est_2019_bin[procjv_analysis_nona$jv_est_2019_factor == "Pocas veces se justifica"] <- 0
procjv_analysis_nona$jv_est_2019_bin[procjv_analysis_nona$jv_est_2019_factor == "Algunas veces se justifica"] <- 0
procjv_analysis_nona$jv_est_2019_bin[procjv_analysis_nona$jv_est_2019_factor == "Muchas veces se justifica"] <- 0
procjv_analysis_nona$jv_est_2019_bin[procjv_analysis_nona$jv_est_2019_factor == "Siempre se justifica"] <- 1

# procjv_analysis_nona$jv_est_2019_bin <- factor(procjv_analysis_nona$jv_est_2019_bin, levels = c(0,1), labels = c("No se justifica", "Se justifica"))

procjv_analysis_nona$jv_carab1_2019_bin[procjv_analysis_nona$jv_carab1_2019_factor == "Nunca se justifica"] <- 0
procjv_analysis_nona$jv_carab1_2019_bin[procjv_analysis_nona$jv_carab1_2019_factor == "Pocas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab1_2019_bin[procjv_analysis_nona$jv_carab1_2019_factor == "Algunas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab1_2019_bin[procjv_analysis_nona$jv_carab1_2019_factor == "Muchas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab1_2019_bin[procjv_analysis_nona$jv_carab1_2019_factor == "Siempre se justifica"] <- 1

# procjv_analysis_nona$jv_carab1_2019_bin <- factor(procjv_analysis_nona$jv_carab1_2019_bin, levels = c(0,1), labels = c("No se justifica", "Se justifica"))

procjv_analysis_nona$jv_carab2_2019_bin[procjv_analysis_nona$jv_carab2_2019_factor == "Nunca se justifica"] <- 0
procjv_analysis_nona$jv_carab2_2019_bin[procjv_analysis_nona$jv_carab2_2019_factor == "Pocas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab2_2019_bin[procjv_analysis_nona$jv_carab2_2019_factor == "Algunas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab2_2019_bin[procjv_analysis_nona$jv_carab2_2019_factor == "Muchas veces se justifica"] <- 0
procjv_analysis_nona$jv_carab2_2019_bin[procjv_analysis_nona$jv_carab2_2019_factor == "Siempre se justifica"] <- 1

# procjv_analysis_nona$jv_carab2_2019_bin <- factor(procjv_analysis_nona$jv_carab2_2019_bin, levels = c(0,1), labels = c("No se justifica", "Se justifica"))


# procjv_analysis_nona$jv_carab2_2019_bin <- factor(procjv_analysis_nona$jv_carab2_2019_bin, levels = c(0,1), labels = c("No se justifica", "Se justifica"))

# procjv_analysis_nona$jv_carab2_2019_bin <- factor(procjv_analysis_nona$jv_carab2_2019_bin, levels = c(0,1), labels = c("No se justifica", "Se justifica"))

# 2. Calculas modelos -------------

# 2.1 Justificación estudiantes tiren piedras --------------

jv_est_2019.modelmulpub1 <- glm(jv_est_2019_bin ~ 
                                  sj_ceo_rec + 
                                  sj_obrero_rec + 
                                  sj_entrevistado, 
                                data = procjv_analysis_nona, family = "binomial")

jv_est_2019.modelmulpub2<- glm(jv_est_2019_bin ~ 
                                 sj_ceo_rec + 
                                 sj_obrero_rec + 
                                 sj_entrevistado + 
                                 ingreso + 
                                 sexo_factor + 
                                 ingreso_satisfact_factor + 
                                 educ_rec_factor + 
                                 indigena_rec_factor, 
                               data = procjv_analysis_nona, family = "binomial")

jv_est_2019.modelmulpub3<- glm(jv_est_2019_bin ~ 
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
                               data = procjv_analysis_nona, family = "binomial")

jv_est_2019.modelmulpub4<- glm(jv_est_2019_bin ~ 
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
                               data = procjv_analysis_nona, family = "binomial")

jv_est_2019.modelmulpub5<- glm(jv_est_2019_bin ~ 
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
                               data = procjv_analysis_nona, family = "binomial")

# 2.2 Justificación carabineros repriman marchas --------------

jv_carab1_2019.modelmulpub1 <- glm(jv_carab1_2019_bin ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado, 
                                   data = procjv_analysis_nona, family = "binomial")

jv_carab1_2019.modelmulpub2<- glm(jv_carab1_2019_bin ~ 
                                    sj_ceo_rec + 
                                    sj_obrero_rec + 
                                    sj_entrevistado + 
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor + 
                                    indigena_rec_factor, 
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab1_2019.modelmulpub3<- glm(jv_carab1_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab1_2019.modelmulpub4<- glm(jv_carab1_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab1_2019.modelmulpub5<- glm(jv_carab1_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

# 2.3 Justificación carabineros desalojen tomas ---------------

jv_carab2_2019.modelmulpub1 <- glm(jv_carab2_2019_bin ~ 
                                     sj_ceo_rec + 
                                     sj_obrero_rec + 
                                     sj_entrevistado_rec, 
                                   data = procjv_analysis_nona, family = "binomial")

jv_carab2_2019.modelmulpub2<- glm(jv_carab2_2019_bin ~ 
                                    sj_ceo_rec +
                                    sj_obrero_rec + 
                                    sj_entrevistado_rec +
                                    ingreso + 
                                    sexo_factor + 
                                    ingreso_satisfact_factor + 
                                    educ_rec_factor +
                                    indigena_rec_factor,
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab2_2019.modelmulpub3<- glm(jv_carab2_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab2_2019.modelmulpub4<- glm(jv_carab2_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab2_2019.modelmulpub5<- glm(jv_carab2_2019_bin ~ 
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
                                  data = procjv_analysis_nona, family = "binomial")

jv_carab2_2019.modelmulpub6<- glm(jv_carab2_2019_bin ~ 
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
                                    ingreso_satisfact_factor*sj_obrero_rec +
                                    pos_pol_rec_factor +
                                    frec_marcha_factor + 
                                    conf_carab_factor, 
                                  data = procjv_analysis_nona, family = "binomial")
