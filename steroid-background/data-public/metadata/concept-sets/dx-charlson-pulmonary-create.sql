SELECT
  st.target_concept_id    concept_id
  ,dc.concept_id          concept_id_from_icd
  ,dc.icd_code
  ,dc.icd_description
  ,iif(dc.icd_code like 'J45%', 1, 0)   dx_asthma
FROM cdw_outpost.lexis.dim_dx_charlson dc
  left  join cdw_omop_1.v6.concept_source_to_target st on dc.concept_id = st.source_concept_id
WHERE
  dc.pulmonary = 1
  and  
  dc.vocabulary_id = 'ICD10CM'
ORDER BY icd_code
