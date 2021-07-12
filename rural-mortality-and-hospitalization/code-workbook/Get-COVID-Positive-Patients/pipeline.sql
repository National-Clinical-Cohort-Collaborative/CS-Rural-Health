

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ce08ffdd-1b19-49de-aaa6-376022b45d51"),
    Covid_positive_persons_LDS=Input(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    selected_critical_visits=Input(rid="ri.foundry.main.dataset.a8a5404a-760c-4f8e-bee2-76ccc7b5b781")
)
SELECT p.person_id, s.visit_start_date as date_of_earliest_covid_diagnosis
FROM pers p
INNER JOIN selected_critical_visits s on s.person_id = p.person_id
LEFT JOIN Covid_positive_persons_LDS c on c.person_id = p.person_id
WHERE c.person_id IS NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad"),
    Covid_positive_lab_records=Input(rid="ri.foundry.main.dataset.cec65557-1d6c-4680-8751-4e9445eb73e0"),
    covid_diagnosis_records=Input(rid="ri.foundry.main.dataset.71d5711a-3e54-4556-914e-a740f0857dce"),
    pcr_ag_measurement_concepts=Input(rid="ri.foundry.main.dataset.2e8b1b56-9dd5-4efb-bc9c-8fedc5d7f560")
)
SELECT DISTINCT person_id, MIN(date_of_earliest_covid_diagnosis) AS date_of_earliest_covid_diagnosis
FROM
(SELECT person_id, measurement_date as date_of_earliest_covid_diagnosis, visit_occurrence_id as covid_visit_occurrence_id
FROM Covid_positive_lab_records
UNION
SELECT person_id, condition_start_date as date_of_earliest_covid_diagnosis, visit_occurrence_id as covid_visit_occurrence_id
FROM covid_diagnosis_records) A
GROUP BY person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fb77d755-fc2c-464f-8b15-3c7b0cdfd71e"),
    Covid_positive_lab_records=Input(rid="ri.foundry.main.dataset.cec65557-1d6c-4680-8751-4e9445eb73e0"),
    covid_diagnosis_records=Input(rid="ri.foundry.main.dataset.71d5711a-3e54-4556-914e-a740f0857dce"),
    pcr_ag_measurement_concepts=Input(rid="ri.foundry.main.dataset.2e8b1b56-9dd5-4efb-bc9c-8fedc5d7f560")
)
SELECT DISTINCT person_id, MIN(date_of_earliest_covid_diagnosis) AS date_of_earliest_covid_diagnosis, diagnosis_type
FROM
(SELECT person_id, measurement_date as date_of_earliest_covid_diagnosis, visit_occurrence_id as covid_visit_occurrence_id,
CASE WHEN concept_id IS NOT NULL THEN 'pcr_ag' ELSE 'antibody' END AS diagnosis_type
FROM Covid_positive_lab_records c
LEFT JOIN pcr_ag_measurement_concepts p ON p.concept_id = c.measurement_concept_id
UNION
SELECT person_id, condition_start_date as date_of_earliest_covid_diagnosis, visit_occurrence_id as covid_visit_occurrence_id, 'diagnosis' AS diagnosis_type
FROM covid_diagnosis_records) A
GROUP BY person_id, diagnosis_type

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b3caec56-f557-43b7-b337-bf7675e52e2c"),
    covid_19_negative_patients=Input(rid="ri.foundry.main.dataset.cf76a45e-c5bd-4839-84c3-c880b9d16633"),
    covid_negative_with_index=Input(rid="ri.foundry.main.dataset.1798f145-ca97-494c-a785-01f730ca9b71"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
select
distinct
q.*,
case
    when ( q.age < 18 ) then ('0-17')
    when ( q.age between 18 and 29 ) then ('18-29')
    when ( q.age between 30 and 49 ) then ('30-49')
    when ( q.age between 50 and 64 ) then ('50-64')
    when ( q.age >= 65 ) then ('65+')
    else ('NA')
end as age_group 
FROM
(
SELECT 
distinct
a.person_id , a.data_partner_id , a.measurement_date as date_of_latest_measurement_value
, p.year_of_birth , p.month_of_birth , p.day_of_birth , upper(p.gender_concept_name) as sex , trim(upper(p.race_concept_name)) as race , upper(p.ethnicity_concept_name) as ethnicity 
, ( 
  ( year(a.measurement_date) - p.year_of_birth )
   - ( case 
        when (month(a.measurement_date) < coalesce(p.month_of_birth,1)) then 1
        when (month(a.measurement_date) = coalesce(p.month_of_birth,1) and day(a.measurement_date)<coalesce(p.day_of_birth,1)) then 1
        else 0
       end
   )   
) 
as age
FROM covid_19_negative_patients a
left outer join pers p on p.person_id = a.person_id 
where
a.measurement_date = (select max(b.measurement_date) from covid_19_negative_patients b where b.person_id = a.person_id)
)q
INNER JOIN covid_negative_with_index c on c.person_id = q.person_id 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1798f145-ca97-494c-a785-01f730ca9b71"),
    COVID_controls=Input(rid="ri.foundry.main.dataset.ce08ffdd-1b19-49de-aaa6-376022b45d51")
)
-- Copy for public datasets folder
SELECT *
FROM COVID_controls

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.00e3584d-579d-42dd-8aca-8e11570a5551"),
    Covid_positive_persons_LDS=Input(rid="ri.foundry.main.dataset.a917b9e7-b891-4ac5-93dc-de1a3b93c7ad")
)
--This is a copy for the public datasets folder
SELECT *
FROM Covid_positive_persons_LDS

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.359c5f57-70d4-4e6f-bd7e-f2b67cd8f57d"),
    covid_positive_with_index=Input(rid="ri.foundry.main.dataset.00e3584d-579d-42dd-8aca-8e11570a5551"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT concat(year1, ', ', month1) AS year_month, month_year_count
FROM 
(SELECT month(date_of_earliest_covid_diagnosis) as month1, year(date_of_earliest_covid_diagnosis) as year1, COUNT(*) as month_year_count
FROM covid_positive_with_index c
INNER JOIN pers p on p.person_id = c.person_id
--WHERE p.data_partner_id NOT IN [redacted]
GROUP BY month(date_of_earliest_covid_diagnosis), year(date_of_earliest_covid_diagnosis)
ORDER BY year(date_of_earliest_covid_diagnosis) ASC, month(date_of_earliest_covid_diagnosis) ASC
) X

