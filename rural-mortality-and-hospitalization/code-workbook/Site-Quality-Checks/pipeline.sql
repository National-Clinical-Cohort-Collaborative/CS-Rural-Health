

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8ebbbafe-5a6c-44be-981d-0e885e7f6c4f"),
    condition_occurrence_check=Input(rid="ri.foundry.main.dataset.88642db3-8a8e-457b-badf-0027c1812f0f"),
    death_check=Input(rid="ri.foundry.main.dataset.f6ee3ab1-bdee-433e-a0e2-7d97e821d919"),
    drug_era_check=Input(rid="ri.foundry.main.dataset.c39a9826-f425-4ebb-9823-bec6df8d279f"),
    drug_exposure_check=Input(rid="ri.foundry.main.dataset.d05f10f0-8e4a-49f7-a5bf-0c6f4bb9498f"),
    measurement_check=Input(rid="ri.foundry.main.dataset.7d00a39a-c2aa-4102-aa78-c1b864a13594"),
    observation_check=Input(rid="ri.foundry.main.dataset.795c52eb-40b4-417e-abe0-1443c5117ea2"),
    person_check=Input(rid="ri.foundry.main.dataset.b3b5bc89-6f75-420f-af39-e76bc135f582"),
    procedure_occurence_check=Input(rid="ri.foundry.main.dataset.46f6925f-9a0a-4f5e-9a97-17e36bef8923"),
    visit_occurrence_check=Input(rid="ri.foundry.main.dataset.723a6a4d-2500-4c63-b4d5-64b11b5a9e50")
)
SELECT p.cdm_name, SUM(person_counts) AS person_count, SUM(death_counts) AS death_count, (SUM(death_counts)/ SUM(person_counts)) AS death_percentage, (SUM(measurement_countss) / SUM(person_counts)) AS measurement_percentage, (SUM(visit_occurrence_counts)/ SUM(person_counts)) AS visit_occurrence_percentage, (SUM(observation_counts) / SUM(person_counts)) AS observation_percentage, (SUM(procedure_counts) / SUM(person_counts)) AS procedure_percentage, (SUM(drug_era_counts) / SUM(person_counts)) AS drug_era_percentage, (SUM(condition_counts) / SUM(person_counts)) AS condition_occurrence_percentage, (SUM(drug_exposure_counts) / SUM(person_counts)) AS drug_exposure_percentage
FROM person_check p
LEFT JOIN measurement_check m ON m.data_partner_id = p.data_partner_id
LEFT JOIN visit_occurrence_check v ON v.data_partner_id = p.data_partner_id
LEFT JOIN death_check d ON d.data_partner_id = p.data_partner_id
LEFT JOIN observation_check o ON o.data_partner_id = p.data_partner_id
LEFT JOIN procedure_occurence_check po ON po.data_partner_id = p.data_partner_id
LEFT JOIN drug_era_check de on de.data_partner_id = p.data_partner_id
LEFT JOIN condition_occurrence_check co ON co.data_partner_id = p.data_partner_id
LEFT JOIN drug_exposure_check dec ON dec.data_partner_id = p.data_partner_id
GROUP BY p.cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.88642db3-8a8e-457b-badf-0027c1812f0f"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT m.data_partner_id, cdm_name, COUNT(condition_occurrence_id) as condition_counts
FROM manifest m
JOIN condition_occurrence c on c.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.24d76af2-1c70-4423-9f66-eaafc7b7d60c"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    site_diff_to_mean=Input(rid="ri.foundry.main.dataset.3355cee5-e6fd-4b8c-8e00-356e97be35a1"),
    site_quality_check=Input(rid="ri.foundry.main.dataset.cf76d24a-73b1-45fe-b1f8-4a4b098d2f50")
)
SELECT 
s.data_partner_id,
sq.person_counts AS patient_population,
CASE WHEN s.death_percentage_diff_to_mean < 0.32 THEN 0 WHEN s.death_percentage_diff_to_mean IS NULL THEN 0 ELSE 1 END AS death_percentage_grade,
CASE WHEN s.measurement_diff_to_mean < 0.32 THEN 0 WHEN s.measurement_diff_to_mean IS NULL THEN 0 ELSE 1 END AS measurement_grade,
CASE WHEN s.visit_occurrence_diff_to_mean < 0.32 THEN 0 WHEN s.visit_occurrence_diff_to_mean IS NULL THEN 0 ELSE 1 END AS visit_occurrence_grade,
CASE WHEN s.observation_diff_to_mean < 0.32 THEN 0 WHEN s.observation_diff_to_mean IS NULL THEN 0 ELSE 1 END AS observation_grade,
CASE WHEN s.procedure_diff_to_mean < 0.32 THEN 0 WHEN s.procedure_diff_to_mean IS NULL THEN 0 ELSE 1 END AS procedure_grade,
CASE WHEN s.drug_era_diff_to_mean < 0.32 THEN 0 WHEN s.drug_era_diff_to_mean IS NULL THEN 0 ELSE 1 END AS drug_era_grade,
CASE WHEN s.condition_occurrence_diff_to_mean < 0.32 THEN 0 WHEN s.condition_occurrence_diff_to_mean IS NULL THEN 0 ELSE 1 END AS condition_occurrencee_grade,
CASE WHEN s.drug_exposure_diff_to_mean < 0.32 THEN 0 WHEN s.drug_exposure_diff_to_mean IS NULL THEN 0 ELSE 1 END AS drug_exposure_grade,
m.shift_date_yn, m.max_num_shift_days
FROM site_diff_to_mean s
INNER JOIN site_quality_check sq on sq.data_partner_id = s.data_partner_id
INNER JOIN manifest m on m.data_partner_id = s.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f6ee3ab1-bdee-433e-a0e2-7d97e821d919"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT m.data_partner_id, cdm_name, COUNT(distinct person_id) as death_counts
FROM manifest m
JOIN death d on d.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c39a9826-f425-4ebb-9823-bec6df8d279f"),
    drug_era=Input(rid="ri.foundry.main.dataset.bc6b481a-7b75-470a-addc-66d241e7d7c7"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT m.data_partner_id, cdm_name, COUNT(drug_era_id) as drug_era_counts
FROM manifest m
JOIN drug_era d on d.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d05f10f0-8e4a-49f7-a5bf-0c6f4bb9498f"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT m.data_partner_id, cdm_name, COUNT(drug_exposure_id) as drug_exposure_counts
FROM manifest m
JOIN drug_exposure d on d.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0980ad6e-7a6a-43a5-b592-c007784c597e"),
    manifest_with_qc=Input(rid="ri.foundry.main.dataset.479a9430-26a6-4251-b3ca-6acfc4c1079f")
)
select data_partner_id, stack(9, 'death_percentage_grade', death_percentage_grade, 'measurement_grade', measurement_grade, 'visit_occurrence_grade', visit_occurrence_grade, 'observation_grade', observation_grade, 'procedure_grade', procedure_grade, 'drug_era_grade', drug_era_grade, 'condition_occurrencee_grade', condition_occurrencee_grade, 'drug_exposure_grade', drug_exposure_grade, 'date_shift_grade', date_shift_grade ) as (domain, grade)
FROM manifest_with_qc

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.479a9430-26a6-4251-b3ca-6acfc4c1079f"),
    final_grades=Input(rid="ri.foundry.main.dataset.44780bed-1635-4ef8-bfe0-21987ddcb0de"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT f.*, cdm_name, run_date, contribution_date, 
CASE 
    WHEN abs(f.date_shift) > 30 THEN 0
    WHEN abs(f.date_shift) <= 30 THEN 1
    WHEN lower(m.shift_date_yn) = 'n' THEN 1
    ELSE 1
END AS date_shift_grade
FROM final_grades f
INNER JOIN manifest m ON m.data_partner_id = f.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7d00a39a-c2aa-4102-aa78-c1b864a13594"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
SELECT m.data_partner_id, cdm_name, measurement_countss
FROM manifest m
LEFT JOIN 
(SELECT COUNT(measurement_id) as measurement_countss, data_partner_id
FROM measurement 
GROUP BY data_partner_id
) me on me.data_partner_id = m.data_partner_id
--GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.795c52eb-40b4-417e-abe0-1443c5117ea2"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    observation=Input(rid="ri.foundry.main.dataset.b998b475-b229-471c-800e-9421491409f3")
)
SELECT m.data_partner_id, cdm_name, COUNT(observation_id) as observation_counts
FROM manifest m
JOIN observation o on o.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b3b5bc89-6f75-420f-af39-e76bc135f582"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT m.data_partner_id, cdm_name, COUNT(person_id) as person_counts
FROM manifest m
JOIN pers p on p.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.46f6925f-9a0a-4f5e-9a97-17e36bef8923"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    procedure_occurrence=Input(rid="ri.foundry.main.dataset.f6f0b5e0-a105-403a-a98f-0ee1c78137dc")
)
SELECT m.data_partner_id, cdm_name, COUNT(procedure_occurrence_id) as procedure_counts
FROM manifest m
JOIN procedure_occurrence p on p.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3355cee5-e6fd-4b8c-8e00-356e97be35a1"),
    site_quality_check=Input(rid="ri.foundry.main.dataset.cf76d24a-73b1-45fe-b1f8-4a4b098d2f50")
)
SELECT data_partner_id, 
person_counts/(SELECT AVG(person_counts) FROM site_quality_check) AS person_diff_to_mean,
death_counts/(SELECT AVG(death_counts) FROM site_quality_check) AS death_diff_to_mean,
death_percentage/(SELECT AVG(death_percentage) FROM site_quality_check) AS death_percentage_diff_to_mean,
measurement_percentage/(SELECT AVG(measurement_percentage) FROM site_quality_check) AS measurement_diff_to_mean,
visit_occurrence_percentage/(SELECT AVG(visit_occurrence_percentage) FROM site_quality_check) AS visit_occurrence_diff_to_mean,
observation_percentage/(SELECT AVG(observation_percentage) FROM site_quality_check) AS observation_diff_to_mean,
procedure_percentage/(SELECT AVG(procedure_percentage) FROM site_quality_check) AS procedure_diff_to_mean,
drug_era_percentage/(SELECT AVG(drug_era_percentage) FROM site_quality_check) AS drug_era_diff_to_mean,
condition_occurrence_percentage/(SELECT AVG(condition_occurrence_percentage) FROM site_quality_check) AS condition_occurrence_diff_to_mean,
drug_exposure_percentage/(SELECT AVG(drug_exposure_percentage) FROM site_quality_check) AS drug_exposure_diff_to_mean
FROM site_quality_check

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cf76d24a-73b1-45fe-b1f8-4a4b098d2f50"),
    condition_occurrence_check=Input(rid="ri.foundry.main.dataset.88642db3-8a8e-457b-badf-0027c1812f0f"),
    death_check=Input(rid="ri.foundry.main.dataset.f6ee3ab1-bdee-433e-a0e2-7d97e821d919"),
    drug_era_check=Input(rid="ri.foundry.main.dataset.c39a9826-f425-4ebb-9823-bec6df8d279f"),
    drug_exposure_check=Input(rid="ri.foundry.main.dataset.d05f10f0-8e4a-49f7-a5bf-0c6f4bb9498f"),
    measurement_check=Input(rid="ri.foundry.main.dataset.7d00a39a-c2aa-4102-aa78-c1b864a13594"),
    observation_check=Input(rid="ri.foundry.main.dataset.795c52eb-40b4-417e-abe0-1443c5117ea2"),
    person_check=Input(rid="ri.foundry.main.dataset.b3b5bc89-6f75-420f-af39-e76bc135f582"),
    procedure_occurence_check=Input(rid="ri.foundry.main.dataset.46f6925f-9a0a-4f5e-9a97-17e36bef8923"),
    visit_occurrence_check=Input(rid="ri.foundry.main.dataset.723a6a4d-2500-4c63-b4d5-64b11b5a9e50")
)
SELECT p.data_partner_id, CAST(person_counts AS DOUBLE) AS person_counts, CAST(death_counts AS DOUBLE) AS death_counts, (death_counts / person_counts) AS death_percentage, (measurement_countss / person_counts) AS measurement_percentage, (visit_occurrence_counts/ person_counts) AS visit_occurrence_percentage, (observation_counts / person_counts) AS observation_percentage, (procedure_counts / person_counts) AS procedure_percentage, (drug_era_counts / person_counts) AS drug_era_percentage, (condition_counts / person_counts) AS condition_occurrence_percentage, (drug_exposure_counts / person_counts) AS drug_exposure_percentage
FROM person_check p
LEFT JOIN measurement_check m ON m.data_partner_id = p.data_partner_id
LEFT JOIN visit_occurrence_check v ON v.data_partner_id = p.data_partner_id
LEFT JOIN death_check d ON d.data_partner_id = p.data_partner_id
LEFT JOIN observation_check o ON o.data_partner_id = p.data_partner_id
LEFT JOIN procedure_occurence_check po ON po.data_partner_id = p.data_partner_id
LEFT JOIN drug_era_check de on de.data_partner_id = p.data_partner_id
LEFT JOIN condition_occurrence_check co ON co.data_partner_id = p.data_partner_id
LEFT JOIN drug_exposure_check dec ON dec.data_partner_id = p.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.723a6a4d-2500-4c63-b4d5-64b11b5a9e50"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
SELECT m.data_partner_id, cdm_name, COUNT(visit_occurrence_id) as visit_occurrence_counts
FROM manifest m
JOIN visit_occurrence v on v.data_partner_id = m.data_partner_id
GROUP BY m.data_partner_id, cdm_name

