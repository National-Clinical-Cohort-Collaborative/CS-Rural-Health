

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1d3520eb-889b-4db2-8ecb-8cda3b389091"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'urban'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'large rural'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'small rural'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'isolated'
GROUP BY four_category_ruca)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2f1330cb-cfa2-4eaf-ae83-252cbdb665ce"),
    mortality_summary_all=Input(rid="ri.foundry.main.dataset.50c3398b-7574-408d-8514-ff4854e863b5")
)
SELECT *, CASE WHEN age < 18 THEN '0-17' WHEN age BETWEEN 18 AND 25 THEN '18-25' WHEN age BETWEEN 26 AND 45 THEN '26-45' WHEN age BETWEEN 46 AND 65 THEN '46-65' WHEN age BETWEEN 66 AND 80 THEN '66-80' WHEN age >80 THEN '80-120' ELSE 'Unknown' END AS age_Group
FROM mortality_summary_all

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.534f1f87-603a-4071-bc1f-4b24487ce0dc"),
    age_group_summary_all=Input(rid="ri.foundry.main.dataset.2f1330cb-cfa2-4eaf-ae83-252cbdb665ce")
)
SELECT age_Group, four_category_ruca, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality, COUNT(person_id) AS pers_count
FROM age_group_summary_all 
GROUP BY age_Group, mortality, four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a45f06c0-1c1c-4902-be31-9fe4caf907cc"),
    summary_Table=Input(rid="ri.foundry.main.dataset.5dd50df8-79ce-461c-9397-0a429352f52a")
)
SELECT age_at_visit_start_in_years_int,
Race,
Ethnicity, 
gender_concept_name, 
smoking_status,
InpatientOrED,
AKI_in_hospital,
Q_Score,
CAST(BMI AS INT),
CASE WHEN in_death_table = TRUE THEN in_death_table ELSE FALSE END AS in_death_table,
CAST(ACE AS INT),
CAST(ARB AS INT),
CAST(MI AS INT),
CAST(CHF AS INT),
CAST(PVD AS INT),
CAST(stroke AS INT),
CAST(dementia AS INT),
CAST(pulmonary AS INT),
CAST(rheumatic AS INT),
CAST(PUD AS INT),
CAST(liver_mild AS INT),
CAST(liversevere AS INT),
CAST(diabetes AS INT),
CAST(dmcx AS INT),
CAST(paralysis AS INT),
CAST(renal AS INT),
CAST(cancer AS INT),
CAST(mets AS INT),
CAST(hiv AS INT),
CAST(RUCA1 AS STRING),
CAST(RUCA2 AS STRING),
CAST(RUCC_2013 AS STRING)
FROM summary_Table

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.05106a8c-adc1-43ac-8457-c07249426bbb"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    summary_with_geolocation=Input(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a")
)
SELECT data_partner_id, count(s.person_id) AS data_partner_person_count
FROM summary_with_geolocation s
INNER JOIN pers p on p.person_id = s.person_id 
GROUP BY data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cebc4a77-85dd-4ea7-8d77-06ec4f8c45b8"),
    summary_table_all_with_secondary_categories=Input(rid="ri.foundry.main.dataset.3c32e414-54c6-4669-8ad7-6aae022f6afd")
)
SELECT Ethnicity, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM summary_table_all_with_secondary_categories
GROUP BY Ethnicity, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.vector.main.execute.8d6ec166-48a5-4511-aa8f-cdfe40a7db7d"),
    age_group_summary_all_mortality=Input(rid="ri.foundry.main.dataset.534f1f87-603a-4071-bc1f-4b24487ce0dc"),
    inpatient_summary_age_group=Input(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685"),
    mortality_age_ruca_category=Input(rid="ri.foundry.main.dataset.73c5d6b7-5fdf-4542-9d14-5256498ca347"),
    mortality_summary_age_ruca_category=Input(rid="ri.foundry.main.dataset.23ee5e78-fd72-4ab3-bef5-4647e2572ee5")
)

SELECT *
FROM mortality_age_ruca_category
WHERE four_category_ruca = 'urban'
AND mortality = 'in death table'
ORDER BY age_Group

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.45af6786-b215-4d20-8d90-fceb7eb962aa"),
    summary_table_all_with_secondary_categories=Input(rid="ri.foundry.main.dataset.3c32e414-54c6-4669-8ad7-6aae022f6afd")
)
SELECT gender_concept_name, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM summary_table_all_with_secondary_categories
GROUP BY gender_concept_name, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5a83346c-a13a-4c5e-976d-4163c670cb5e"),
    inpatient_summary_gender=Input(rid="ri.foundry.main.dataset.ebcf94dd-ef40-404a-ad04-d1763a341789")
)
SELECT gender_concept_name, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM inpatient_summary_gender
GROUP BY gender_concept_name, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.91bbcb63-f4c7-4ec8-996f-0be7ff20261d"),
    inpatient_summary_gender=Input(rid="ri.foundry.main.dataset.ebcf94dd-ef40-404a-ad04-d1763a341789")
)
SELECT Race, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM inpatient_summary_gender
GROUP BY Race, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0e900617-fef1-4205-82d9-a088a0d9c066"),
    Covid_positive_persons_LDS=Input(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    microvisit_to_macrovisit_lds=Input(rid="ri.foundry.main.dataset.5af2c604-51e0-4afa-b1ae-1e5fa2f4b905"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
select * from
 (
SELECT 
distinct
  c.person_id , c.date_of_earliest_covid_diagnosis , m.macrovisit_id , m.macrovisit_start_date , m.macrovisit_end_date ,
  p.year_of_birth , p.month_of_birth , p.day_of_birth , 
  ( 
       ( year(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) - p.year_of_birth )
         - ( case 
              when (month(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) < coalesce(p.month_of_birth,1)) then 1
              when (month(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) = coalesce(p.month_of_birth,1) and day(coalesce(c.date_of_earliest_covid_diagnosis , current_date))<coalesce                     (p.day_of_birth,1)) then 1
              else 0
             end
           )   
  ) as age ,
  case when m1.visit_concept_id = 581379 then 'Y' else 'N' end as ICU_Flag , 
  case when ( (DATEDIFF(m.macrovisit_end_date , m.macrovisit_start_date) > 0)) then (DATEDIFF(m.macrovisit_end_date , m.macrovisit_start_date))  else 1 end as LOS ,
  d.death_date as date_of_death
FROM 
Covid_positive_persons_LDS c
inner join microvisit_to_macrovisit_lds m on m.person_id = c.person_id  -- and m.macrovisit_id > 0 
     and 
         (
          (c.date_of_earliest_covid_diagnosis between date_sub(m.macrovisit_start_date, 14) and m.macrovisit_start_date )   
              -- earliest covid date within 14 days from visit start date
              or
          (c.date_of_earliest_covid_diagnosis between m.macrovisit_start_date and m.macrovisit_end_date )        
               -- earliest covid date between visit start and end date
         )
left outer join microvisit_to_macrovisit_lds m1 on m1.macrovisit_id = m.macrovisit_id and m1.visit_concept_id = 581379  -- check for ICU visit
inner join pers p on p.person_id = c.person_id 
left outer join death d on d.person_id = c.person_id
)q

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f01b7649-18c1-4c8e-9fad-505aac09fc08"),
    inpatient_summary2=Input(rid="ri.foundry.main.dataset.5e41c567-dd6f-4cc5-a944-80268e1f9670")
)
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'urban'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'large rural'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'small rural'
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS category_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'isolated'
GROUP BY four_category_ruca)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5e41c567-dd6f-4cc5-a944-80268e1f9670"),
    inpatient_summar=Input(rid="ri.foundry.main.dataset.0e900617-fef1-4205-82d9-a088a0d9c066"),
    ruca_categories=Input(rid="ri.foundry.main.dataset.a906b039-4043-481c-b5d2-e82f350b19d5")
)
SELECT i.*, r.four_category_ruca, r.in_death_table
FROM inpatient_summar i
JOIN ruca_categories r on r.person_id = i.person_id
WHERE r.four_category_ruca IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.43589468-c14d-44a4-b4d2-58a7d44dcd2a"),
    inpatient_summary1=Input(rid="ri.foundry.main.dataset.f01b7649-18c1-4c8e-9fad-505aac09fc08"),
    inpatient_summary2=Input(rid="ri.foundry.main.dataset.5e41c567-dd6f-4cc5-a944-80268e1f9670")
)
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM inpatient_summary2)) AS percent_category 
FROM inpatient_summary2 R
INNER JOIN inpatient_summary1 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'urban')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM inpatient_summary2)) AS percent_category 
FROM inpatient_summary2 R
INNER JOIN inpatient_summary1 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'large rural')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM inpatient_summary2)) AS percent_category 
FROM inpatient_summary2 R
INNER JOIN inpatient_summary1 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'small rural')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM inpatient_summary2)) AS percent_category 
FROM inpatient_summary2 R
INNER JOIN inpatient_summary1 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'isolated')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f63b2210-76ca-43ee-9520-1272ce05d4b8"),
    inpatient_summary2=Input(rid="ri.foundry.main.dataset.5e41c567-dd6f-4cc5-a944-80268e1f9670")
)
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'urban'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'large rural'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'small rural'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM inpatient_summary2
WHERE four_category_ruca = 'isolated'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ae8f1282-4a26-4f33-a3f5-9eaf737dd241"),
    inpatient_summary1=Input(rid="ri.foundry.main.dataset.f01b7649-18c1-4c8e-9fad-505aac09fc08"),
    inpatient_summary4=Input(rid="ri.foundry.main.dataset.f63b2210-76ca-43ee-9520-1272ce05d4b8")
)
SELECT s2.four_category_ruca, s2.death_count/s3.category_count AS mortality_rate
FROM inpatient_summary4 s2
JOIN inpatient_summary1 s3 ON s3.four_category_ruca = s2.four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685"),
    inpatient_summary2=Input(rid="ri.foundry.main.dataset.5e41c567-dd6f-4cc5-a944-80268e1f9670")
)
SELECT *, CASE WHEN age < 18 THEN '0-17' WHEN age BETWEEN 18 AND 25 THEN '18-25' WHEN age BETWEEN 26 AND 45 THEN '26-45' WHEN age BETWEEN 46 AND 65 THEN '46-65' WHEN age BETWEEN 66 AND 80 THEN '66-80' WHEN age >80 THEN '80-120' ELSE 'Unknown' END AS age_Group
FROM inpatient_summary2

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8cf31a0a-a1c1-48a5-aab0-5f895e50368c"),
    inpatient_summary_gender=Input(rid="ri.foundry.main.dataset.ebcf94dd-ef40-404a-ad04-d1763a341789")
)
SELECT Ethnicity, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM inpatient_summary_gender
GROUP BY Ethnicity, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ebcf94dd-ef40-404a-ad04-d1763a341789"),
    complete_patient_table_with_derived_scores=Input(rid="ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"),
    inpatient_summary_age_group=Input(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685")
)
SELECT i.*, c.Race, c.Ethnicity, c.BMI, c.gender_concept_name, c.Q_Score
FROM inpatient_summary_age_group i
INNER JOIN complete_patient_table_with_derived_scores c on c.person_id = i.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.47609b22-adf5-44b8-bcf5-3624ade4005c"),
    inpatient_summary_age_group=Input(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685"),
    lat_long_ref=Input(rid="ri.foundry.main.dataset.b40d340b-68e0-4f16-9e8c-4074f9d69f65"),
    pers_zip_codes=Input(rid="ri.foundry.main.dataset.10278bcf-1fd8-4030-b7b3-12ea5cd744aa")
)
SELECT i.*, l.Latitude, l.Longitude, l.geopoint
FROM inpatient_summary_age_group i
INNER JOIN pers_zip_codes p on p.person_id = i.person_id
INNER JOIN lat_long_ref l on l.Zip = p.zip

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ce8c767b-3c15-44e6-a1c3-fbab8a4a1c4c"),
    summary_with_geolocation=Input(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a")
)
SELECT CAST(Latitude AS DOUBLE) as lat, CAST(Longitude AS DOUBLE) as long, CAST(case_count AS INT), CAST(case_count AS STRING) as cases, ROW_NUMBER() OVER(ORDER BY case_count DESC) AS group FROM 
(SELECT CAST(Latitude AS STRING), CAST(Longitude AS STRING), COUNT(*) AS case_count
FROM summary_with_geolocation
GROUP BY Latitude, Longitude) X

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c1b03bb3-cb19-4254-a7bd-003a79790248"),
    summary_with_geolocation=Input(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a")
)
SELECT CAST(Latitude AS DOUBLE) as lat, CAST(Longitude AS DOUBLE) as long, CAST(case_count AS INT), CAST(case_count AS STRING) as cases, ROW_NUMBER() OVER(ORDER BY case_count DESC) AS group FROM 
(SELECT CAST(Latitude AS STRING), CAST(Longitude AS STRING), COUNT(*) AS case_count
FROM summary_with_geolocation
WHERE three_category_ruca = 'urban'
GROUP BY Latitude, Longitude) X

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d94caaba-ac4b-419f-a26e-af25419fb7fe"),
    summary_with_geolocation=Input(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a")
)
SELECT CAST(Latitude AS DOUBLE) as lat, CAST(Longitude AS DOUBLE) as long, CAST(case_count AS INT), CAST(case_count AS STRING) as cases, ROW_NUMBER() OVER(ORDER BY case_count DESC) AS group FROM 
(SELECT CAST(Latitude AS STRING), CAST(Longitude AS STRING), COUNT(*) AS case_count
FROM summary_with_geolocation
WHERE three_category_ruca = 'nonurban-adjacent rural'
GROUP BY Latitude, Longitude) X

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.32ed912f-cfad-4767-9dd6-1019b06fdfc6"),
    summary_with_geolocation=Input(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a")
)
SELECT CAST(Latitude AS DOUBLE) as lat, CAST(Longitude AS DOUBLE) as long, CAST(case_count AS INT), CAST(case_count AS STRING) as cases, ROW_NUMBER() OVER(ORDER BY case_count DESC) AS group FROM 
(SELECT CAST(Latitude AS STRING), CAST(Longitude AS STRING), COUNT(*) AS case_count
FROM summary_with_geolocation
WHERE three_category_ruca = 'urban-adjacent rural'
GROUP BY Latitude, Longitude) X

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.73c5d6b7-5fdf-4542-9d14-5256498ca347"),
    inpatient_summary_age_group=Input(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685")
)
SELECT age_Group, four_category_ruca, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality, COUNT(person_id) AS pers_count
FROM inpatient_summary_age_group 
GROUP BY age_Group, mortality, four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.23ee5e78-fd72-4ab3-bef5-4647e2572ee5"),
    inpatient_summary_age_group=Input(rid="ri.foundry.main.dataset.944d2e1f-1cce-4b06-93dc-f1f5730a9685")
)
(SELECT COUNT(person_id) AS pers_count, age_Group, four_category_ruca, 'in death table' AS mortality
FROM inpatient_summary_age_group i
WHERE in_death_table is not null
GROUP BY four_category_ruca, age_Group
ORDER BY four_category_ruca, age_Group)
UNION
(SELECT COUNT(person_id) AS pers_count, age_Group, four_category_ruca, 'NOT in death table' AS mortality
FROM inpatient_summary_age_group i
WHERE in_death_table is null
GROUP BY four_category_ruca, age_Group
ORDER BY four_category_ruca, age_Group)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.50c3398b-7574-408d-8514-ff4854e863b5"),
    Covid_positive_persons_LDS=Input(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
SELECT r.*,  ( 
       ( year(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) - p.year_of_birth )
         - ( case 
              when (month(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) < coalesce(p.month_of_birth,1)) then 1
              when (month(coalesce(c.date_of_earliest_covid_diagnosis , current_date)) = coalesce(p.month_of_birth,1) and day(coalesce(c.date_of_earliest_covid_diagnosis , current_date))<coalesce                     (p.day_of_birth,1)) then 1
              else 0
             end
           )   
  ) as age 
FROM ruca_categories_wo_nulls r
INNER JOIN pers p ON p.person_id = r.person_id 
INNER JOIN Covid_positive_persons_LDS c ON c.person_id = p.person_id AND c.person_id = r.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0e8c0c75-c81f-4d81-abef-b1fe6d830dbf"),
    summary_table_all_with_secondary_categories=Input(rid="ri.foundry.main.dataset.3c32e414-54c6-4669-8ad7-6aae022f6afd")
)
SELECT Race, four_category_ruca, COUNT(person_id) AS gender_count, CASE WHEN in_death_table IS NOT NULL THEN 'in death table' ELSE 'not in death table' END AS mortality
FROM summary_table_all_with_secondary_categories
GROUP BY Race, four_category_ruca, mortality
ORDER BY four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a906b039-4043-481c-b5d2-e82f350b19d5"),
    summary_Table=Input(rid="ri.foundry.main.dataset.5dd50df8-79ce-461c-9397-0a429352f52a")
)
SELECT person_id, 
CASE 
    WHEN RUCA2 IN (1, 1.1, 2, 2.1, 3) THEN 'urban'
    WHEN RUCA2 IN (4, 4.1, 5, 5.1, 6) THEN 'large rural'
    WHEN RUCA2 IN (7, 7.1, 7.2, 8, 8.1, 8.2, 9) THEN 'small rural'
    WHEN RUCA2 IN (10, 10.1, 10.2, 10.3) THEN 'isolated'
    ELSE NULL
END AS four_category_ruca,
CASE
    WHEN RUCA2 IN ('1.0', '1.1') THEN 'urban core'
    WHEN RUCA2 IN ('2.0', '2.1', '3.0', '4.1', '5.1', '7.1', '8.1', '10.1') THEN 'other urban'
    WHEN RUCA2 IN ('4.0', '4.2') THEN 'large rural core'
    WHEN RUCA2 IN ('5.0', '5.2', '6.0', '6.1') THEN 'other large rural'
    WHEN RUCA2 IN ('7.0', '7.2', '7.3', '7.4') THEN 'small rural core'
    WHEN RUCA2 IN ('8.0', '8.2', '8.3', '8.4', '9.0', '9.1', '9.2') THEN 'other small rural'
    WHEN RUCA2 IN ('10.0', '10.2', '10.3', '10.4', '10.5', '10.6') THEN 'isolated rural'
    ELSE NULL
END AS seven_category_ruca,
CASE
    WHEN RUCA1 IN (1, 2, 3) THEN 'urban'
    WHEN RUCA1 IN (4, 5, 7, 8) THEN 'urban-adjacent rural'
    WHEN RUCA1 IN (6, 9, 10) THEN 'nonurban-adjacent rural'
    ELSE NULL
END AS three_category_ruca,
in_death_table
FROM summary_Table

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17"),
    ruca_categories=Input(rid="ri.foundry.main.dataset.a906b039-4043-481c-b5d2-e82f350b19d5")
)
SELECT *
FROM ruca_categories
WHERE four_category_ruca IS NOT NULL
AND seven_category_ruca IS NOT NULL
AND three_category_ruca IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.12ed1806-42ed-47c5-af3c-d0bfa15745a3"),
    Summary3=Input(rid="ri.foundry.main.dataset.1d3520eb-889b-4db2-8ecb-8cda3b389091"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM ruca_categories_wo_nulls)) AS percent_category 
FROM ruca_categories_wo_nulls R
INNER JOIN Summary3 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'urban')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM ruca_categories_wo_nulls)) AS percent_category 
FROM ruca_categories_wo_nulls R
INNER JOIN Summary3 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'large rural')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM ruca_categories_wo_nulls)) AS percent_category 
FROM ruca_categories_wo_nulls R
INNER JOIN Summary3 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'small rural')
UNION
(SELECT R.four_category_ruca, (S.category_count / (SELECT COUNT(*) FROM ruca_categories_wo_nulls)) AS percent_category 
FROM ruca_categories_wo_nulls R
INNER JOIN Summary3 S ON S.four_category_ruca = R.four_category_ruca
WHERE R.four_category_ruca = 'isolated')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1840920d-9cd2-4f8b-a093-2febe1f82dd5"),
    Summary3=Input(rid="ri.foundry.main.dataset.1d3520eb-889b-4db2-8ecb-8cda3b389091"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'urban'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'large rural'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'small rural'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)
UNION
(SELECT four_category_ruca, COUNT(*) AS death_count 
FROM ruca_categories_wo_nulls
WHERE four_category_ruca = 'isolated'
AND in_death_table IS NOT NULL
GROUP BY four_category_ruca)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.222d26c9-e529-4294-9669-538d7c00dc9a"),
    Summary3=Input(rid="ri.foundry.main.dataset.1d3520eb-889b-4db2-8ecb-8cda3b389091"),
    summary2=Input(rid="ri.foundry.main.dataset.1840920d-9cd2-4f8b-a093-2febe1f82dd5")
)
SELECT s2.four_category_ruca, s2.death_count/s3.category_count AS mortality_rate
FROM summary2 s2
JOIN Summary3 s3 ON s3.four_category_ruca = s2.four_category_ruca

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3c32e414-54c6-4669-8ad7-6aae022f6afd"),
    complete_patient_table_with_derived_scores=Input(rid="ri.foundry.main.dataset.d467585c-53b3-4f10-b09e-8e86a4796a1a"),
    mortality_summary_all=Input(rid="ri.foundry.main.dataset.50c3398b-7574-408d-8514-ff4854e863b5")
)
SELECT m.*, c.Race, c.Ethnicity, c.BMI, c.gender_concept_name, c.Q_Score
FROM mortality_summary_all m
INNER JOIN complete_patient_table_with_derived_scores c on c.person_id = m.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.23b2e999-3f7c-4268-bd8e-cf189f3e4c9a"),
    lat_long_ref=Input(rid="ri.foundry.main.dataset.b40d340b-68e0-4f16-9e8c-4074f9d69f65"),
    pers_zip_codes=Input(rid="ri.foundry.main.dataset.10278bcf-1fd8-4030-b7b3-12ea5cd744aa"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
SELECT i.*, l.Latitude, l.Longitude, l.geopoint
FROM ruca_categories_wo_nulls i
INNER JOIN pers_zip_codes p on p.person_id = i.person_id
INNER JOIN lat_long_ref l on l.Zip = p.zip

@transform_pandas(
    Output(rid="ri.vector.main.execute.35ce1b7f-aaf9-48ac-b777-2cb42c2b12ee"),
    summary_Table=Input(rid="ri.foundry.main.dataset.5dd50df8-79ce-461c-9397-0a429352f52a")
)
SELECT RUCA2, COUNT(*) as count_per_secondary_RUCA
FROM summary_Table
GROUP BY RUCA2

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.59f662fa-de2c-448d-b2cf-d61765329024"),
    Covid_positive_persons_LDS=Input(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"),
    ruca_categories_wo_nulls=Input(rid="ri.foundry.main.dataset.ddc3e93d-f9e2-4782-863b-6a052954ab17")
)
SELECT COUNT(DISTINCT R.person_id) / COUNT(DISTINCT C.person_id) AS percentage_with_zip
FROM Covid_positive_persons_LDS C
LEFT JOIN ruca_categories_wo_nulls R on R.person_id = C.person_id

