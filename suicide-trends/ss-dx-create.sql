SELECT TOP (1000) 
  concept_id
  ,concat('[', concept_code, ']')   as icd_code
  ,''                               as category
  ,'TRUE'                           as include_in_analysis
  ,concept_name                     as icd_description
  ,domain_id
  ,vocabulary_id
  ,concept_class_id
FROM [cdw_omop_1].[v6].[concept]
WHERE
  (
    vocabulary_id  = 'ICD10CM'
    --or
    --(
    --  vocabulary_id  = 'SNOMED'
    --)
  )
  and
  --domain_id = 'Condition'
  --and
  (
    concept_name like '%suicid%' -- grabs 'suicide' and 'suicidal'
    --and
    --concept_name like '%self%inf%'
  )