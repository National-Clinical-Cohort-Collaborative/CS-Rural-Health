@transform_pandas(
  Output(
    rid = "ri.foundry.main.dataset.3a4b73d2-a553-448a-be81-13f65d475948"
  ), 
  summary2_for_analysis = Input(
    rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
  )
) 
SELECT 
  distinct data_partner_id 
FROM 
  summary2_for_analysis @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.940daaa6-4f42-4a36-95f1-946b45da9d1b"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    )
  ) 
SELECT 
  DISTINCT(person_id), 
  MAX(death_date) AS death_date 
FROM 
  death 
GROUP BY 
  person_id @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.4c1567cc-00a9-4228-a837-dcfad186bb90"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    )
  ) 
SELECT 
  DISTINCT (person_id), 
  death_date 
FROM 
  (
    SELECT 
      d.person_id, 
      d.death_date 
    FROM 
      death d 
      INNER JOIN (
        SELECT 
          person_id, 
          Max(death_date) AS maxdate 
        FROM 
          death 
        where 
          death_date IS NULL 
          OR death_date BETWEEN '2020-01-01' 
          AND '2021-06-01' 
        GROUP BY 
          person_id
      ) d1 ON d1.person_id = d.person_id 
      AND d.death_date = d1.maxdate
  ) X @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.b8514664-febd-4ff6-a660-5602a279a673"
    ), 
    death_check = Input(
      rid = "ri.foundry.main.dataset.940daaa6-4f42-4a36-95f1-946b45da9d1b"
    )
  ) 
SELECT 
  person_id, 
  CASE WHEN death_date < '2020-01-01' THEN NULL WHEN death_date >= current_date() THEN NULL ELSE death_date END AS death_date 
FROM 
  death_check @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.20a9830f-f161-414e-8ade-8399115d14e4"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    )
  ) 
SELECT 
  person_id, 
  count(person_id) as person_count 
FROM 
  death 
GROUP BY 
  person_id 
HAVING 
  count(person_id) > 1 @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.ac3be23a-03ce-4225-8b4c-0dfc6ddf01b7"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    ), 
    death_qc = Input(
      rid = "ri.foundry.main.dataset.20a9830f-f161-414e-8ade-8399115d14e4"
    )
  ) 
SELECT 
  * 
FROM 
  death 
where 
  person_id IN (
    SELECT 
      distinct(person_id) 
    FROM 
      death_qc
  ) 
ORDER BY 
  person_id @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.17410b49-b729-479e-858e-4dd10d851798"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  * 
FROM 
  summary2_for_analysis 
WHERE 
  in_death_table = 1 @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.0272d167-f133-43df-8d5b-87c2c1ce1b2d"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  * 
FROM 
  summary2_for_analysis 
WHERE 
  in_death_table = 0 @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"
    ), 
    complete_patient_table_with_derived_scores = Input(
      rid = "ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    ), 
    death_final = Input(
      rid = "ri.foundry.main.dataset.b8514664-febd-4ff6-a660-5602a279a673"
    ), 
    inpatient_summar = Input(
      rid = "ri.foundry.main.dataset.0e900617-fef1-4205-82d9-a088a0d9c066"
    ), 
    summary_table = Input(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    ), 
    supplemental_oxygen_after_covid_diagnosis = Input(
      rid = "ri.foundry.main.dataset.ac8f295f-628d-48ef-9df7-d13bd13c25a6"
    )
  ) 
SELECT 
  s.*, 
  DATEDIFF(
    d.death_date, s.date_of_earliest_covid_diagnosis
  ) AS ttdeath, 
  oxygen_start_date, 
  DATEDIFF(
    s.visit_start_date, '2020-04-01'
  ) AS days_to_hospitalization 
FROM 
  summary_table s 
  LEFT JOIN complete_patient_table_with_derived_scores c on c.person_id = s.person_id --INNER JOIN inpatient_summar i on i.person_id = s.person_id
  LEFT JOIN death_final d on d.person_id = s.person_id 
  LEFT JOIN supplemental_oxygen_after_covid_diagnosis so ON so.person_id = s.person_id 
WHERE 
  c.visit_concept_name = 'Inpatient Visit' @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.b1df49c2-e04c-4fd3-a052-cac09549ecdc"
    ), 
    dta_residuals = Input(
      rid = "ri.foundry.main.dataset.50cb3522-2016-4884-af8b-01d080a61674"
    ), 
    dta_residuals_original = Input(
      rid = "ri.foundry.main.dataset.3ca80ba2-c251-4504-9f5f-e89e4492b6f9"
    )
  ) (
    SELECT 
      AVG(rs) AS mean_residuals, 
      gender AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      gender, 
      four_category_ruca
  ) 
UNION 
  (
    SELECT 
      AVG(rs) AS mean_residuals, 
      age_Group AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      age_Group, 
      four_category_ruca
  ) 
UNION 
  (
    SELECT 
      AVG(rs) AS mean_residuals, 
      Race AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      Race, 
      four_category_ruca
  ) 
UNION 
  (
    SELECT 
      AVG(rs) AS mean_residuals, 
      Ethnicity AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      Ethnicity, 
      four_category_ruca
  ) 
UNION 
  (
    SELECT 
      AVG(rs) AS mean_residuals, 
      Race AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      Race, 
      four_category_ruca
  ) 
UNION 
  (
    SELECT 
      AVG(rs) AS mean_residuals, 
      BMI_Group AS Category, 
      four_category_ruca 
    FROM 
      dta_residuals 
    GROUP BY 
      BMI_Group, 
      four_category_ruca
  ) @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.181e4fa7-b412-42b4-8da5-122e954f8a60"
    ), 
    inpatient_summary_for_analysis = Input(
      rid = "ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"
    )
  ) 
SELECT 
  person_id, 
  COUNT(person_id) AS count_person 
FROM 
  inpatient_summary_for_analysis 
GROUP BY 
  person_id 
HAVING 
  count(person_id) > 1 @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.b2620bfb-86fd-4a4e-ae0b-8cb524ce7be7"
    ), 
    death_hospice_1 = Input(
      rid = "ri.foundry.main.dataset.6aca817e-d197-4271-b31c-1e42420d7ce6"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    pers_RUCA2 = Input(
      rid = "ri.foundry.main.dataset.017db5c4-76f7-4063-a75e-d94106e7d769"
    )
  ) 
SELECT 
  RUCA1, 
  count(person_id) AS caseload, 
  SUM(death) AS death_count 
FROM 
  (
    SELECT 
      distinct p.person_id, 
      CASE WHEN d.person_id IS NOT NULL THEN 1 ELSE 0 END AS death, 
      RUCA1 
    FROM 
      pers_RUCA2 p 
      JOIN pers pe ON pe.person_id = p.person_id 
      LEFT JOIN death_hospice_1 d ON d.person_id = p.person_id 
    WHERE 
      p.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
      AND '2021-03-31' 
      AND RUCA1 IS NOT NULL 
      AND pe.data_partner_id NOT IN (REDACTED)
  ) X 
GROUP BY 
  RUCA1 @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    ), 
    SPO2_labs_first = Input(
      rid = "ri.foundry.main.dataset.4e0df5f5-75cb-4735-98d4-69809826afd2"
    ), 
    closest_bmi_to_covid_diagnosis_from_observation = Input(
      rid = "ri.foundry.main.dataset.5711686f-9d1e-4016-8e2e-e75b13e4b202"
    ), 
    combined_alc_smok_sub_hyper_cov_pos = Input(
      rid = "ri.foundry.main.dataset.90f4e5c8-66d2-4777-b64f-31bb9d795b54"
    ), 
    complete_patient_table_with_derived_scores = Input(
      rid = "ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"
    ), 
    covid_medications = Input(
      rid = "ri.foundry.main.dataset.3116094b-57e5-44ba-908c-4494c1883c56"
    ), 
    death_hospice = Input(
      rid = "ri.foundry.main.dataset.6aca817e-d197-4271-b31c-1e42420d7ce6"
    ), 
    inpatient_summary_for_analysis = Input(
      rid = "ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"
    ), 
    manifest = Input(
      rid = "ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    pers_percent_by_rurality = Input(
      rid = "ri.foundry.main.dataset.65d56879-6c09-447c-b01a-08725a7b5fe1"
    ), 
    pers_with_covid_peak_reference = Input(
      rid = "ri.foundry.main.dataset.cef3672e-405f-473d-af51-913f341c2fcb"
    ), 
    selected_critical_visits = Input(
      rid = "ri.foundry.main.dataset.a8a5404a-760c-4f8e-bee2-76ccc7b5b781"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) 
SELECT 
  DISTINCT i.person_id, 
  datediff(
    death.probable_death_date, i.date_of_earliest_covid_diagnosis
  ) AS ttdeath, 
  four_category_ruca, 
  three_category_ruca, 
  CASE WHEN i.age_at_visit_start_in_years_int BETWEEN 0 
  AND 120 THEN i.age_at_visit_start_in_years_int ELSE null END AS age, 
  CASE WHEN four_category_ruca = 'urban' THEN '0' ELSE '1' END AS i_rural, 
  i.gender_concept_name AS gender, 
  CASE WHEN i.age_at_visit_start_in_years_int < 18 THEN '0-17' WHEN i.age_at_visit_start_in_years_int BETWEEN 18 
  AND 29 THEN '18-29' WHEN i.age_at_visit_start_in_years_int BETWEEN 30 
  AND 39 THEN '30-39' WHEN i.age_at_visit_start_in_years_int BETWEEN 40 
  AND 49 THEN '40-49' WHEN i.age_at_visit_start_in_years_int BETWEEN 50 
  AND 59 THEN '50-59' WHEN i.age_at_visit_start_in_years_int BETWEEN 60 
  AND 69 THEN '60-69' WHEN i.age_at_visit_start_in_years_int BETWEEN 70 
  AND 79 THEN '70-79' WHEN i.age_at_visit_start_in_years_int BETWEEN 80 
  and 120 THEN '80-120' ELSE 'Unknown/Missing' END AS age_Group, 
  CASE WHEN i.age_at_visit_start_in_years_int < 18 THEN '0-17' WHEN i.age_at_visit_start_in_years_int BETWEEN 18 
  AND 29 THEN '18-29' WHEN i.age_at_visit_start_in_years_int BETWEEN 30 
  AND 49 THEN '30-49' WHEN i.age_at_visit_start_in_years_int BETWEEN 50 
  AND 64 THEN '50-64' WHEN i.age_at_visit_start_in_years_int BETWEEN 65 
  AND 120 THEN '>65' ELSE 'Unknown/Missing' END AS age_Category, 
  CASE WHEN i.Race = 'Native Hawaiian or Other Pacific Islander' THEN 'Asian or NHPI' WHEN i.Race = 'Asian' THEN 'Asian or NHPI' ELSE i.Race END AS Race, 
  i.Ethnicity, 
  CASE WHEN i.BMI BETWEEN 10 
  AND 18.5 THEN '<18.5' WHEN i.BMI >= 18.5 
  AND i.BMI < 25 THEN '18.5-24.9' WHEN i.BMI >= 25 
  AND i.BMI < 30 THEN '25-29.9' WHEN i.BMI BETWEEN 30 
  AND 75 THEN '>30' WHEN bmi.bmi_from_observation IN (
    '<18.5', '18.5-24.9', '25-29.9', '>30'
  ) THEN bmi.bmi_from_observation WHEN i.BMI > 10 THEN 'Unknown/Missing' WHEN i.BMI < 75 THEN 'Unknown/Missing' ELSE 'Unknown/Missing' END AS BMI_Group, 
  i.BMI, 
  CASE WHEN i.Q_Score < 1 THEN '<1.0' WHEN i.Q_Score BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN i.Q_Score > 2 THEN '>2.0' ELSE 'Unknown/Missing' END AS Q_Score_Categories, 
  i.Q_Score, 
  CASE WHEN (
    diabetes = 1 
    OR dmcx = 1
  ) THEN 1 ELSE 0 END AS Diabetes, 
  MI, 
  CHF, 
  PVD, 
  stroke, 
  dementia, 
  pulmonary, 
  rheumatic, 
  multiple, 
  CASE WHEN (
    liver_mild = 1 
    OR liversevere = 1
  ) THEN 1 ELSE 0 END AS Liver, 
  paralysis, 
  renal, 
  cancer, 
  mets, 
  hiv, 
  i.Severity_Type, 
  CASE WHEN rECMO = 1 THEN 1 WHEN c.ECMO IS NOT NULL THEN 1 ELSE 0 END AS ECMO, 
  MACE, 
  CASE WHEN i.Mechnical_Ventilation = 1 THEN 1 WHEN c.Invasive_Ventilation IS NOT NULL THEN 1 ELSE 0 END AS Mechnical_Ventilation, 
  c.Invasive_Ventilation, 
  CASE WHEN death.person_id IS NOT NULL THEN 1 WHEN i.in_death_table = 1 THEN 1 ELSE 0 END AS in_death_table, 
  i.length_of_stay, 
  CASE WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2020-03-31' THEN 'Q1 2020' WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-04-01' 
  AND '2020-06-30' THEN 'Q2 2020' WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-07-01' 
  AND '2020-09-30' THEN 'Q3 2020' WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-10-01' 
  AND '2020-12-31' THEN 'Q4 2020' WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2021-01-01' 
  AND '2021-03-31' THEN 'Q1 2021' ELSE 'Unknown/Missing' END AS quarter_of_diagnosis, 
  i.data_partner_id, 
  CAST(i.CCI_INDEX AS DOUBLE), 
  CASE WHEN CCI_INDEX < 1 THEN '<1.0' WHEN CCI_INDEX BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN CCI_INDEX > 2 THEN '>2.0' END AS CCI_Categories, 
  CASE WHEN i.oxygen_start_date IS NOT NULL THEN 1 WHEN i.Mechnical_Ventilation = 1 THEN 1 WHEN c.Invasive_Ventilation IS NOT NULL THEN 1 ELSE 0 END AS supplemental_oxygen, 
  i.smoking_status, 
  a.smoking, 
  a.hypertension, 
  a.alcoholism, 
  a.opioid, 
  a.substance_abuse, 
  s.readmission, 
  CASE WHEN i.ECMO IS NOT NULL THEN 1 WHEN rECMO = 1 THEN 1 WHEN i.MACE = 1 THEN 1 ELSE 0 END AS ecmo_mace_invasive_vent_aki, 
  p.rural_percentage, 
  oxygen_saturation_lab_value, 
  CASE WHEN meds.Dexamethasone = 1 THEN 1 ELSE 0 END AS Dexamethasone, 
  CASE WHEN meds.Hydrocortisone = 1 THEN 1 ELSE 0 END AS Hydrocortisone, 
  CASE WHEN meds.Prednisone_Methylprednisolone = 1 THEN 1 ELSE 0 END AS Prednisone_Methylprednisolone, 
  CASE WHEN meds.Prednisolone = 1 THEN 1 ELSE 0 END AS Prednisolone, 
  CASE WHEN meds.Hydroxychloroquine = 1 THEN 1 WHEN meds.Chloroquine = 1 THEN 1 ELSE 0 END AS Chloroquine_Hydroxychloroquine 
FROM 
  inpatient_summary_for_analysis i 
  LEFT JOIN combined_alc_smok_sub_hyper_cov_pos a ON a.person_id = i.person_id 
  LEFT JOIN complete_patient_table_with_derived_scores c ON c.person_id = i.person_id 
  LEFT JOIN selected_critical_visits s ON s.person_id = i.person_id 
  LEFT JOIN SPO2_labs_first sp ON sp.person_id = i.person_id 
  LEFT JOIN pers_percent_by_rurality p on p.data_partner_id = i.data_partner_id 
  LEFT JOIN closest_bmi_to_covid_diagnosis_from_observation bmi on bmi.person_id = i.person_id 
  LEFT JOIN covid_medications meds ON meds.person_id = i.person_id 
  LEFT JOIN death_hospice death ON death.person_id = i.person_id 
WHERE 
  i.date_of_earliest_covid_diagnosis <= '2021-03-31' --MISSING or sparsely populated death data
  AND i.data_partner_id NOT IN (REDACTED) --MISSING or sparssely populated BMI
  AND i.data_partner_id NOT IN (REDACTED) --DATE SHIFTING OVER >= 90 days
  AND i.data_partner_id NOT IN (REDACTED) @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) 
SELECT 
  *, 
  DATEDIFF(
    death_date, date_of_earliest_covid_diagnosis
  ) AS ttdeath, 
  CASE WHEN CCI_INDEX < 1 THEN '<1.0' WHEN CCI_INDEX BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN CCI_INDEX > 2 THEN '>2.0' END AS CCI_Categories 
FROM 
  summary_table_for_analysis 
WHERE 
  date_of_earliest_covid_diagnosis <= '2021-03-31' 
  /* Sites [REDACTED] shift dates <= 90 days. Sites [REDACTED] have unreliable mortality information */
  AND i.data_partner_id NOT IN [REDACTED] 
  /*  Filter out additional sites with no BMI or BMI percentages under 0.10 percentage for all patients */
  AND i.data_partner_id NOT IN [REDACTED] @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.58239702-c61d-4026-9e36-2e75ccd2da01"
    ), 
    Covid_positive_persons_LDS = Input(
      rid = "ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"
    ), 
    SPO2_labs_first = Input(
      rid = "ri.foundry.main.dataset.4e0df5f5-75cb-4735-98d4-69809826afd2"
    ), 
    closest_bmi_to_covid_diagnosis_from_observation = Input(
      rid = "ri.foundry.main.dataset.5711686f-9d1e-4016-8e2e-e75b13e4b202"
    ), 
    combined_alc_smok_sub_hyper_cov_pos = Input(
      rid = "ri.foundry.main.dataset.90f4e5c8-66d2-4777-b64f-31bb9d795b54"
    ), 
    complete_patient_table_with_derived_scores = Input(
      rid = "ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"
    ), 
    covid_positive_pts_with_preexisting_comorbidities = Input(
      rid = "ri.foundry.main.dataset.30b4b356-f911-49bc-9f87-c4b38cc3c51f"
    ), 
    death_final = Input(
      rid = "ri.foundry.main.dataset.b8514664-febd-4ff6-a660-5602a279a673"
    ), 
    death_hospice = Input(
      rid = "ri.foundry.main.dataset.6aca817e-d197-4271-b31c-1e42420d7ce6"
    ), 
    inpatient_summary_for_analysis = Input(
      rid = "ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"
    ), 
    manifest = Input(
      rid = "ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    pers_percent_by_rurality = Input(
      rid = "ri.foundry.main.dataset.65d56879-6c09-447c-b01a-08725a7b5fe1"
    ), 
    pers_with_covid_peak_reference = Input(
      rid = "ri.foundry.main.dataset.cef3672e-405f-473d-af51-913f341c2fcb"
    ), 
    ruca_categories_wo_nulls = Input(
      rid = "ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17"
    ), 
    selected_critical_visits = Input(
      rid = "ri.foundry.main.dataset.a8a5404a-760c-4f8e-bee2-76ccc7b5b781"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) 
SELECT 
  DISTINCT i.person_id, 
  ttdeath, 
  ruc.four_category_ruca, 
  ruc.three_category_ruca, 
  CASE WHEN c.age_at_visit_start_in_years_int BETWEEN 0 
  AND 120 THEN c.age_at_visit_start_in_years_int ELSE null END AS age, 
  CASE WHEN ruc.four_category_ruca = 'urban' THEN '0' ELSE '1' END AS i_rural, 
  c.gender_concept_name AS gender, 
  CASE WHEN c.age_at_visit_start_in_years_int < 18 THEN '0-17' WHEN c.age_at_visit_start_in_years_int BETWEEN 18 
  AND 29 THEN '18-29' WHEN c.age_at_visit_start_in_years_int BETWEEN 30 
  AND 39 THEN '30-39' WHEN c.age_at_visit_start_in_years_int BETWEEN 40 
  AND 49 THEN '40-49' WHEN c.age_at_visit_start_in_years_int BETWEEN 50 
  AND 59 THEN '50-59' WHEN c.age_at_visit_start_in_years_int BETWEEN 60 
  AND 69 THEN '60-69' WHEN c.age_at_visit_start_in_years_int BETWEEN 70 
  AND 79 THEN '70-79' WHEN c.age_at_visit_start_in_years_int BETWEEN 80 
  and 120 THEN '80-120' ELSE 'Unknown/Missing' END AS age_Group, 
  CASE WHEN c.age_at_visit_start_in_years_int < 18 THEN '0-17' WHEN c.age_at_visit_start_in_years_int BETWEEN 18 
  AND 29 THEN '18-29' WHEN c.age_at_visit_start_in_years_int BETWEEN 30 
  AND 49 THEN '30-49' WHEN c.age_at_visit_start_in_years_int BETWEEN 50 
  AND 64 THEN '50-64' WHEN c.age_at_visit_start_in_years_int BETWEEN 65 
  AND 120 THEN '>65' ELSE 'Unknown/Missing' END AS age_Category, 
  CASE WHEN c.Race = 'Native Hawaiian or Other Pacific Islander' THEN 'Asian or NHPI' WHEN c.Race = 'Asian' THEN 'Asian or NHPI' ELSE c.Race END AS Race, 
  c.Ethnicity, 
  CASE WHEN c.BMI BETWEEN 10 
  AND 18.5 THEN '<18.5' WHEN c.BMI >= 18.5 
  AND i.BMI < 25 THEN '18.5-24.9' WHEN c.BMI >= 25 
  AND i.BMI < 30 THEN '25-29.9' WHEN c.BMI BETWEEN 30 
  AND 75 THEN '>30' WHEN bmi.bmi_from_observation IN (
    '<18.5', '18.5-24.9', '25-29.9', '>30'
  ) THEN bmi.bmi_from_observation WHEN c.BMI > 10 THEN 'Unknown/Missing' WHEN c.BMI < 75 THEN 'Unknown/Missing' ELSE 'Unknown/Missing' END AS BMI_Group, 
  i.BMI, 
  CASE WHEN i.Q_Score < 1 THEN '<1.0' WHEN i.Q_Score BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN i.Q_Score > 2 THEN '>2.0' ELSE 'Unknown/Missing' END AS Q_Score_Categories, 
  i.Q_Score, 
  CASE WHEN (
    pre.diabetes = 1 
    OR pre.dmcx = 1
  ) THEN 1 ELSE 0 END AS Diabetes, 
  pre.MI, 
  pre.CHF, 
  pre.PVD, 
  pre.stroke, 
  pre.dementia, 
  pre.pulmonary, 
  pre.rheumatic, 
  CASE WHEN pre.liver_mild = 1 THEN 1 WHEN pre.liversevere = 1 THEN 1 ELSE 0 END as liver, 
  pre.multiple, 
  pre.paralysis, 
  pre.renal, 
  pre.cancer, 
  pre.mets, 
  pre.hiv, 
  c.Severity_Type, 
  CASE WHEN rECMO = 1 THEN 1 WHEN c.ECMO IS NOT NULL THEN 1 ELSE 0 END AS ECMO, 
  i.MACE, 
  CASE WHEN i.Mechnical_Ventilation = 1 THEN 1 WHEN c.Invasive_Ventilation IS NOT NULL THEN 1 ELSE 0 END AS Mechnical_Ventilation, 
  c.Invasive_Ventilation, 
  CASE WHEN death.person_id IS NOT NULL THEN 1 WHEN i.in_death_table = 1 THEN 1 ELSE 0 END AS in_death_table, 
  CASE WHEN cov.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2020-03-31' THEN 'Q1 2020' WHEN cov.date_of_earliest_covid_diagnosis BETWEEN '2020-04-01' 
  AND '2020-06-30' THEN 'Q2 2020' WHEN cov.date_of_earliest_covid_diagnosis BETWEEN '2020-07-01' 
  AND '2020-09-30' THEN 'Q3 2020' WHEN cov.date_of_earliest_covid_diagnosis BETWEEN '2020-10-01' 
  AND '2020-12-31' THEN 'Q4 2020' WHEN cov.date_of_earliest_covid_diagnosis BETWEEN '2021-01-01' 
  AND '2021-03-31' THEN 'Q1 2021' ELSE 'Unknown/Missing' END AS quarter_of_diagnosis, 
  pers.data_partner_id, 
  CAST(pre.CCI_INDEX AS DOUBLE) AS CCI_INDEX, 
  CASE WHEN pre.CCI_INDEX < 1 THEN '<1.0' WHEN pre.CCI_INDEX BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN pre.CCI_INDEX > 2 THEN '>2.0' END AS CCI_Categories, 
  c.smoking_status, 
  a.smoking, 
  a.hypertension, 
  a.alcoholism, 
  a.opioid, 
  a.substance_abuse, 
  s.readmission, 
  CASE WHEN c.ECMO IS NOT NULL THEN 1 WHEN rECMO = 1 THEN 1 WHEN i.MACE = 1 THEN 1 ELSE 0 END AS ecmo_mace_invasive_vent_aki, 
  p.rural_percentage, 
  oxygen_saturation_lab_value, 
  CASE WHEN ip.person_id is NOT NULL THEN 1 ELSE 0 END as hospitalized, 
  CASE WHEN days_to_hospitalization < 0 THEN 365 WHEN days_to_hospitalization > 364 THEN 365 ELSE days_to_hospitalization END AS time, 
  CASE WHEN days_to_hospitalization BETWEEN 0 
  AND 365 THEN 1 WHEN days_to_hospitalization < 0 THEN NULL ELSE 0 END AS status 
FROM 
  summary_table_for_analysis i 
  INNER JOIN pers ON pers.person_id = i.person_id 
  LEFT JOIN covid_positive_pts_with_preexisting_comorbidities pre ON pre.person_id = i.person_id 
  LEFT JOIN inpatient_summary_for_analysis ip on ip.person_id = i.person_id 
  LEFT JOIN combined_alc_smok_sub_hyper_cov_pos a ON a.person_id = i.person_id 
  LEFT JOIN complete_patient_table_with_derived_scores c ON c.person_id = i.person_id 
  LEFT JOIN selected_critical_visits s ON s.person_id = i.person_id 
  LEFT JOIN SPO2_labs_first sp ON sp.person_id = i.person_id 
  LEFT JOIN pers_percent_by_rurality p on p.data_partner_id = pers.data_partner_id 
  LEFT JOIN closest_bmi_to_covid_diagnosis_from_observation bmi on bmi.person_id = i.person_id 
  LEFT JOIN ruca_categories_wo_nulls ruc ON ruc.person_id = i.person_id 
  LEFT JOIN Covid_positive_persons_LDS cov ON cov.person_id = i.person_id 
  LEFT JOIN death_hospice death ON death.person_id = i.person_id 
WHERE 
  cov.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2021-03-31' --MISSING or sparsely populated death data
  AND pers.data_partner_id NOT IN [REDACTED] --MISSING or sparsely populated BMI
  AND pers.data_partner_id NOT IN [REDACTED] --DATE SHIFTING >= 90 days
  AND pers.data_partner_id NOT IN [REDACTED] @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    ), 
    Covid_positive_persons_LDS = Input(
      rid = "ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"
    ), 
    bmi = Input(
      rid = "ri.foundry.main.dataset.db1da349-c960-4a32-b5bc-74339e07f9c3"
    ), 
    complete_patient_table_with_derived_scores = Input(
      rid = "ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"
    ), 
    covid_positive_pts_with_preexisting_comorbidities = Input(
      rid = "ri.foundry.main.dataset.30b4b356-f911-49bc-9f87-c4b38cc3c51f"
    ), 
    covid_positive_with_ecmo_or_vent = Input(
      rid = "ri.foundry.main.dataset.2d6ccd42-b673-4052-bc67-a7d81bb7eb43"
    ), 
    death = Input(
      rid = "ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"
    ), 
    death_final = Input(
      rid = "ri.foundry.main.dataset.b8514664-febd-4ff6-a660-5602a279a673"
    ), 
    manual_bmi = Input(
      rid = "ri.foundry.main.dataset.f4b8b494-00c7-4d00-99ae-aabd692ac480"
    ), 
    ruca_categories_wo_nulls = Input(
      rid = "ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17"
    ), 
    selected_critical_visits = Input(
      rid = "ri.foundry.main.dataset.a8a5404a-760c-4f8e-bee2-76ccc7b5b781"
    ), 
    visit_occurrence = Input(
      rid = "ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7"
    )
  ) 
SELECT 
  DISTINCT r.person_id, 
  r.data_partner_id, 
  r.visit_concept_id, 
  r.visit_start_date, 
  r.visit_concept_name, 
  r.visit_occurrence_id, 
  r.AKI_in_hospital, 
  r.ECMO, 
  r.Invasive_Ventilation, 
  r.positive_covid_test, 
  r.negative_covid_test, 
  r.Suspected_COVID, 
  -- Filter through three methods of obtaining death data, 
  -- preferentially choosing in the death table
  CASE WHEN r.in_death_table IS NOT NULL THEN 1 WHEN t.person_id IS NOT NULL THEN 1 WHEN d.person_id IS NOT NULL THEN 1 WHEN r.Severity_Type = 'Dead_w_COVID' THEN 1 ELSE 0 END AS in_death_table, 
  r.age_at_visit_start_in_years_int, 
  r.length_of_stay, 
  r.Race, 
  r.Ethnicity, 
  r.gender_concept_name, 
  r.smoking_status, 
  r.blood_type, 
  r.covid_status_name, 
  r.Severity_Type, 
  r.InpatientOrED, 
  r.Q_Score, 
  r.Testcount, 
  -- Filter through methods of obtaining BMI, preferentially
  -- using the BMI code workbook
  CASE WHEN r.BMI IS NOT NULL THEN r.BMI WHEN b.bmi_calculated IS NOT NULL THEN b.bmi_calculated WHEN b.bmi_from_site IS NOT NULL THEN b.bmi_from_site WHEN man.manual_bmi IS NOT NULL THEN man.manual_bmi WHEN (
    r.Height IS NOT NULL 
    and r.Weight IS NOT NULL
  ) THEN CAST(
    (
      r.Height / (r.Weight * r.Weight)
    ) AS DOUBLE
  ) ELSE NULL END AS BMI, 
  r.four_category_ruca, 
  r.three_category_ruca, 
  cp.MI, 
  cp.CHF, 
  cp.PVD, 
  cp.stroke, 
  cp.dementia, 
  cp.pulmonary, 
  cp.rheumatic, 
  cp.PUD, 
  liver_mild, 
  cp.liversevere, 
  cp.diabetes, 
  cp.dmcx, 
  cp.paralysis, 
  cp.renal, 
  cp.cancer, 
  cp.mets, 
  cp.hiv, 
  cp.multiple, 
  cp.CCI_INDEX, 
  s.readmission, 
  cv.ECMO as rECMO, 
  cv.MACE, 
  cv.Mechnical_Ventilation, 
  r.date_of_earliest_covid_diagnosis, 
  d.death_date -- This sub-query is redundant at this point, but I'm too
  -- lazy to remove it and improve query efficiency
FROM 
  (
    SELECT 
      r.person_id, 
      r.in_death_table, 
      r.four_category_ruca, 
      r.seven_category_ruca, 
      r.three_category_ruca, 
      ce.date_of_earliest_covid_diagnosis, 
      c.data_partner_id, 
      c.visit_concept_id, 
      c.visit_start_date, 
      c.visit_concept_name, 
      c.visit_occurrence_id, 
      c.AKI_in_hospital, 
      c.ECMO, 
      c.Invasive_Ventilation, 
      c.positive_covid_test, 
      c.negative_covid_test, 
      c.Suspected_COVID, 
      c.age_at_visit_start_in_years_int, 
      c.length_of_stay, 
      c.Race, 
      c.Ethnicity, 
      c.gender_concept_name, 
      c.smoking_status, 
      c.blood_type, 
      c.covid_status_name, 
      c.Severity_Type, 
      c.InpatientOrED, 
      c.Q_Score, 
      c.BMI, 
      c.Testcount, 
      c.Height, 
      c.Weight 
    FROM 
      ruca_categories_wo_nulls r 
      INNER JOIN Covid_positive_persons_LDS ce on ce.person_id = r.person_id 
      LEFT JOIN complete_patient_table_with_derived_scores c on c.person_id = r.person_id
  ) r 
  LEFT JOIN covid_positive_pts_with_preexisting_comorbidities cp on cp.person_id = r.person_id 
  LEFT JOIN selected_critical_visits s on s.person_id = r.person_id 
  LEFT JOIN covid_positive_with_ecmo_or_vent cv on cv.person_id = r.person_id 
  LEFT JOIN death_final d on d.person_id = r.person_id 
  LEFT JOIN bmi b on b.person_id = r.person_id 
  LEFT JOIN manual_bmi man on man.person_id = r.person_id -- This join brings in a few more patients with a discharge
  -- status of hospice, etc.
  LEFT JOIN (
    SELECT 
      r.person_id 
    FROM 
      ruca_categories_wo_nulls r 
      JOIN visit_occurrence v on v.person_id = r.person_id 
      JOIN Covid_positive_persons_LDS ce on ce.person_id = r.person_id 
      and ce.person_id = v.person_id 
    WHERE 
      v.visit_start_date >= ce.date_of_earliest_covid_diagnosis 
      AND (
        lower(v.discharge_to_concept_name) like '%hospice%' 
        OR lower(v.discharge_to_concept_name) like '%deceased%' 
        OR lower(v.discharge_to_concept_name) like '%died%' 
        OR lower(v.discharge_to_concept_name) like '%death%'
      )
  ) t on t.person_id = r.person_id @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    summary_table = Input(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    )
  ) 
SELECT 
  s.person_id, 
  four_category_ruca, 
  three_category_ruca, 
  CASE WHEN age_at_visit_start_in_years_int BETWEEN 0 
  AND 120 THEN age_at_visit_start_in_years_int ELSE null END AS age, 
  CASE WHEN four_category_ruca = 'urban' THEN '0' ELSE '1' END AS i_rural, 
  s.gender_concept_name AS gender, 
  CASE WHEN age_at_visit_start_in_years_int < 18 THEN '0-17' WHEN age_at_visit_start_in_years_int BETWEEN 18 
  AND 29 THEN '18-29' WHEN age_at_visit_start_in_years_int BETWEEN 30 
  AND 39 THEN '30-39' WHEN age_at_visit_start_in_years_int BETWEEN 40 
  AND 49 THEN '40-49' WHEN age_at_visit_start_in_years_int BETWEEN 50 
  AND 59 THEN '50-59' WHEN age_at_visit_start_in_years_int BETWEEN 60 
  AND 69 THEN '60-69' WHEN age_at_visit_start_in_years_int BETWEEN 70 
  AND 79 THEN '70-79' WHEN age_at_visit_start_in_years_int > 80 THEN '80-120' ELSE 'Unknown/Missing' END AS age_Group, 
  CASE WHEN s.Race = 'Native Hawaiian or Other Pacific Islander' THEN 'Other' ELSE s.Race END AS Race, 
  Ethnicity, 
  CASE WHEN BMI BETWEEN 10 
  AND 18.5 THEN '<18.5' WHEN BMI >= 18.5 
  AND BMI < 25 THEN '18.5-24.9' WHEN BMI >= 25 
  AND BMI < 30 THEN '25-29.9' WHEN BMI BETWEEN 30 
  AND 75 THEN '>30' WHEN BMI < 10 THEN 'Unknown/Missing' WHEN BMI > 75 THEN 'Unknown/Missing' ELSE 'Unknown/Missing' END AS BMI_Group, 
  BMI, 
  CASE WHEN Q_Score < 1 THEN '<1.0' WHEN Q_Score BETWEEN 1 
  AND 2 THEN '1.0-2.0' WHEN Q_Score > 2 THEN '>2.0' ELSE 'Unknown/Missing' END AS Q_Score_Categories, 
  Q_Score, 
  CASE WHEN diabetes = 1 THEN 1 WHEN dmcx = 1 THEN 1 ELSE 0 END AS Diabetes, 
  MI, 
  CHF, 
  PVD, 
  stroke, 
  dementia, 
  pulmonary, 
  rheumatic, 
  CASE WHEN liver_mild = 1 THEN 1 WHEN liversevere = 1 THEN 1 ELSE 0 END AS Liver, 
  paralysis, 
  renal, 
  cancer, 
  mets, 
  hiv, 
  Severity_Type, 
  rECMO AS ECMO, 
  MACE, 
  Mechnical_Ventilation, 
  in_death_table, 
  CASE WHEN (
    death_date BETWEEN '2020-01-01' 
    AND '2021-04-13'
  ) THEN death_date ELSE NULL END AS death_date, 
  CASE WHEN date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2020-03-31' THEN 'Q1 2020' WHEN date_of_earliest_covid_diagnosis BETWEEN '2020-04-01' 
  AND '2020-06-30' THEN 'Q2 2020' WHEN date_of_earliest_covid_diagnosis BETWEEN '2020-07-01' 
  AND '2020-09-30' THEN 'Q3 2020' WHEN date_of_earliest_covid_diagnosis BETWEEN '2020-10-01' 
  AND '2020-12-31' THEN 'Q4 2020' WHEN date_of_earliest_covid_diagnosis BETWEEN '2021-01-01' 
  AND '2021-03-31' THEN 'Q1 2021' ELSE 'Unknown/Missing' END AS quarter_of_diagnosis, 
  CASE WHEN date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2021-04-13' THEN date_of_earliest_covid_diagnosis ELSE NULL END AS date_of_earliest_covid_diagnosis, 
  CASE WHEN (
    (
      DATEDIFF(
        visit_start_date, date_of_earliest_covid_diagnosis
      ) BETWEEN 0 
      AND 30
    ) 
    AND visit_concept_name = 'Inpatient Visit'
  ) THEN 1 ELSE 0 END AS admission_status, 
  CAST(CCI_INDEX AS DOUBLE) 
FROM 
  summary_table s 
  INNER JOIN pers p on p.person_id = s.person_id 
WHERE 
  date_of_earliest_covid_diagnosis <= '2021-03-31' 
  /* Sites [REDACTED] shift dates <= 90 days. Sites [REDACTED] have unreliable mortality information */
  AND p.data_partner_id NOT IN [REDACTED] 
  /*  Filter out additional sites with no BMI or BMI percentages under 0.10 percentage for all patients */
  AND p.data_partner_id NOT IN [REDACTED] @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.6cdffeac-119e-42d4-9cc2-1d8e5acb4ecb"
    ), 
    summary_table = Input(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    )
  ) 
SELECT 
  s.*, 
  CASE WHEN date_of_earliest_covid_diagnosis between '2020-01-01' 
  AND '2020-01-31' THEN '1 January 2020' WHEN date_of_earliest_covid_diagnosis between '2020-02-01' 
  AND '2020-02-29' THEN '2 February 2020' WHEN date_of_earliest_covid_diagnosis between '2020-03-01' 
  AND '2020-03-31' THEN '3 March 2020' WHEN date_of_earliest_covid_diagnosis between '2020-04-01' 
  AND '2020-04-30' THEN '4 April 2020' WHEN date_of_earliest_covid_diagnosis between '2020-05-01' 
  AND '2020-05-31' THEN '5 May 2020' WHEN date_of_earliest_covid_diagnosis between '2020-06-01' 
  AND '2020-06-30' THEN '6 June 2020' WHEN date_of_earliest_covid_diagnosis between '2020-07-01' 
  AND '2020-07-31' THEN '7 July 2020' WHEN date_of_earliest_covid_diagnosis between '2020-08-01' 
  AND '2020-08-31' THEN '8 August 2020' WHEN date_of_earliest_covid_diagnosis between '2020-09-01' 
  AND '2020-09-30' THEN '9 September 2020' WHEN date_of_earliest_covid_diagnosis between '2020-10-01' 
  AND '2020-10-31' THEN '10 October 2020' WHEN date_of_earliest_covid_diagnosis between '2020-11-01' 
  AND '2020-11-30' THEN '11 November 2020' WHEN date_of_earliest_covid_diagnosis between '2020-12-01' 
  AND '2020-12-31' THEN '12 December 2020' WHEN date_of_earliest_covid_diagnosis between '2021-01-01' 
  AND '2021-01-31' THEN '13 January 2021' WHEN date_of_earliest_covid_diagnosis between '2021-02-01' 
  AND '2021-02-28' THEN '14 February 2021' WHEN date_of_earliest_covid_diagnosis between '2021-03-01' 
  AND '2021-03-31' THEN '15 March 2021' WHEN date_of_earliest_covid_diagnosis between '2021-04-01' 
  AND '2021-04-30' THEN '16 April 2021' ELSE NULL END AS COVID_diagnosis_month_year, 
  CASE WHEN death_date between '2020-01-01' 
  AND '2020-01-31' THEN '1 January 2020' WHEN death_date between '2020-02-01' 
  AND '2020-02-29' THEN '2 February 2020' WHEN death_date between '2020-03-01' 
  AND '2020-03-31' THEN '3 March 2020' WHEN death_date between '2020-04-01' 
  AND '2020-04-30' THEN '4 April 2020' WHEN death_date between '2020-05-01' 
  AND '2020-05-31' THEN '5 May 2020' WHEN death_date between '2020-06-01' 
  AND '2020-06-30' THEN '6 June 2020' WHEN death_date between '2020-07-01' 
  AND '2020-07-31' THEN '7 July 2020' WHEN death_date between '2020-08-01' 
  AND '2020-08-31' THEN '8 August 2020' WHEN death_date between '2020-09-01' 
  AND '2020-09-30' THEN '9 September 2020' WHEN death_date between '2020-10-01' 
  AND '2020-10-31' THEN '10 October 2020' WHEN death_date between '2020-11-01' 
  AND '2020-11-30' THEN '11 November 2020' WHEN death_date between '2020-12-01' 
  AND '2020-12-31' THEN '12 December 2020' WHEN death_date between '2021-01-01' 
  AND '2021-01-31' THEN '13 January 2021' WHEN death_date between '2021-02-01' 
  AND '2021-02-28' THEN '14 February 2021' WHEN death_date between '2021-03-01' 
  AND '2021-03-31' THEN '15 March 2021' WHEN death_date between '2021-04-01' 
  AND '2021-04-30' THEN '16 April 2021' ELSE NULL END AS COVID_death_month_year, 
  CASE WHEN death_date is not null THEN DATEDIFF(
    death_date, date_of_earliest_covid_diagnosis
  ) ELSE DATEDIFF(
    '2021-04-01', date_of_earliest_covid_diagnosis
  ) END AS time_to_death, 
  CASE WHEN in_death_table IS NOT NULL THEN 1 ELSE 0 END AS death_status 
FROM 
  summary_table s @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.ecb6b1db-103d-4ed0-b69d-fb45b5c13cd2"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  ttdeath, 
  four_category_ruca, 
  three_category_ruca, 
  age, 
  i_rural, 
  gender, 
  Race, 
  Ethnicity, 
  BMI_Group, 
  BMI, 
  Q_Score_Categories, 
  Q_Score, 
  Diabetes, 
  MI, 
  CHF, 
  PVD, 
  stroke, 
  dementia, 
  pulmonary, 
  rheumatic, 
  Liver, 
  paralysis, 
  renal, 
  cancer, 
  mets, 
  hiv, 
  Severity_Type, 
  ECMO, 
  MACE, 
  Mechnical_Ventilation, 
  in_death_table, 
  length_of_stay, 
  quarter_of_diagnosis, 
  data_partner_id, 
  CCI_INDEX, 
  CCI_Categories, 
  CASE WHEN ttdeath BETWEEN -1000 
  AND -1 THEN null WHEN ttdeath BETWEEN 1 
  AND 29 THEN ttdeath WHEN ttdeath = 0 THEN 1 ELSE 30 END AS time, 
  CASE WHEN ttdeath < 30 THEN 1 ELSE 0 END AS status, 
  CASE WHEN four_category_ruca = 'urban' THEN 'urban' ELSE 'rural' END AS irural, 
  CASE WHEN three_category_ruca = 'urban' THEN 'Urban' WHEN three_category_ruca = 'urban-adjacent rural' THEN 'Urban-Adjacent Rural' WHEN three_category_ruca = 'nonurban-adjacent rural' THEN 'Nonurban-Adjacent Rural' ELSE NULL END AS rural_categories, 
  CASE WHEN age_Group = '0-17' THEN '<18' WHEN age_Group = '80-120' THEN '80+' ELSE age_Group END AS age_Group, 
  CASE WHEN age_Category = '0-17' THEN '<18' WHEN age_Category = '>65' THEN '>=65' ELSE age_Category END AS age_Category 
FROM 
  summary2_for_analysis @transform_pandas(
    Output(
      rid = "ri.foundry.main.dataset.e9517e65-dadc-4823-9f97-1dce2d903ba1"
    ), 
    inpatient_summary_for_analysis = Input(
      rid = "ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"
    ), 
    qc = Input(
      rid = "ri.foundry.main.dataset.181e4fa7-b412-42b4-8da5-122e954f8a60"
    )
  ) 
SELECT 
  i.* 
FROM 
  inpatient_summary_for_analysis i 
  INNER JOIN qc q on q.person_id = i.person_id @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.8eff961c-b4ae-4162-a805-3ac49e676eb6"
    ), 
    inpatient_summar = Input(
      rid = "ri.foundry.main.dataset.0e900617-fef1-4205-82d9-a088a0d9c066"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) 
SELECT 
  s.*, 
  CASE WHEN i.person_ID is not null THEN 1 ELSE 0 END AS admission 
FROM 
  summary_table_for_analysis s 
  LEFT JOIN inpatient_summar i on i.person_id = s.person_ID 
WHERE 
  DATEDIFF(
    i.macrovisit_start_date, i.date_of_earliest_covid_diagnosis
  ) <= 30 @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.86a94e64-d454-4d8c-a909-dfd0ed4f129a"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  BMI_Group, 
  COUNT(in_death_table) as bmi_deaths 
FROM 
  summary2_for_analysis 
WHERE 
  in_death_table = 1 
GROUP BY 
  BMI_Group @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.42abcb14-36d6-4c0d-8f5a-b1a920558e5e"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  BMI_Group, 
  count(person_id) AS BMI_counts 
FROM 
  summary2_for_analysis 
GROUP BY 
  BMI_Group @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.74b11939-217e-457c-84ce-9b58df241c7f"
    ), 
    death_hospice_1 = Input(
      rid = "ri.foundry.main.dataset.6aca817e-d197-4271-b31c-1e42420d7ce6"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    pers_RUCA2 = Input(
      rid = "ri.foundry.main.dataset.017db5c4-76f7-4063-a75e-d94106e7d769"
    )
  ) 
SELECT 
  COUNT(distinct data_partner_id) 
FROM 
  pers_RUCA2 p 
  JOIN pers pe ON pe.person_id = p.person_id 
WHERE 
  p.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' 
  AND '2021-03-31' 
  AND RUCA1 IS NOT NULL 
  AND pe.data_partner_id NOT IN [REDACTED] @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.5c249c91-048e-4f88-995b-bc98bd55320d"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) 
SELECT 
  COUNT(*) Q_Count, 
  Q_Score 
FROM 
  summary_table_for_analysis 
GROUP BY 
  Q_Score @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.b2dbf8d4-c371-43bd-877b-caa720f5c2a8"
    ), 
    summary_table = Input(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    )
  ) 
SELECT 
  COUNT(*) Q_Count, 
  Q_Score 
FROM 
  summary_table 
GROUP BY 
  Q_Score 
ORDER BY 
  Q_Score @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.7e70a5d5-62d8-4447-abc3-35654b239faa"
    ), 
    complete_patient_table_with_derived_scores = Input(
      rid = "ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"
    )
  ) 
SELECT 
  COUNT(person_id) AS category_count, 
  covid_status_name 
FROM 
  complete_patient_table_with_derived_scores 
GROUP BY 
  covid_status_name @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.4b39e974-5c51-4d13-a937-3ff6574cfc39"
    ), 
    inpatient_summar = Input(
      rid = "ri.foundry.main.dataset.0e900617-fef1-4205-82d9-a088a0d9c066"
    ), 
    pers = Input(
      rid = "ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"
    ), 
    summary_all_for_analysis = Input(
      rid = "ri.foundry.main.dataset.246fccb0-8f84-4ed9-a296-405e603a75e3"
    ), 
    summary_table = Input(
      rid = "ri.foundry.main.dataset.5e4cc200-0e81-4428-823c-6f2e56347bd2"
    ), 
    summary_table_for_analysis = Input(
      rid = "ri.foundry.main.dataset.46b4856c-26c0-4f80-a469-ccf41839f3b5"
    )
  ) (
    SELECT 
      count(distinct p.data_partner_id) as data_partner_count 
    FROM 
      inpatient_summar s 
      INNER JOIN pers p on p.person_id = s.person_id
  ) 
UNION 
  (
    SELECT 
      count(distinct p.data_partner_id) as data_partner_count 
    FROM 
      summary_table_for_analysis s 
      INNER JOIN pers p on p.person_id = s.person_id
  ) 
UNION 
  (
    SELECT 
      count(distinct p.data_partner_id) as data_partner_count 
    FROM 
      summary_all_for_analysis s 
      INNER JOIN pers p on p.person_id = s.person_id
  ) @transform_pandas(
    Output(
      rid = "ri.vector.main.execute.2d4fedb6-412f-4f26-a7b7-73ad2c2870b9"
    ), 
    summary2_for_analysis = Input(
      rid = "ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565"
    )
  ) 
SELECT 
  count(distinct data_partner_id) as data_partner_count 
FROM 
  summary2_for_analysis
