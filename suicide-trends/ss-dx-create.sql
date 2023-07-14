-- use cdw_omop_1;
WITH concept_icd10cm as (
SELECT
  concept_id    as icd_concept_id
  ,concept_code as icd_code
  ,concept_name as icd_description
FROM v6.concept
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
)

SELECT
  ct.concept_id                     as target_concept_id
  ,ct.domain_id                     as target_domain_id
  ,'TRUE'                           as include_in_analysis
  ,''                               as category
  ,cs.icd_code
  ,cs.icd_description
  ,ct.concept_name                  as target_description
  ,cs.icd_concept_id
  ,ct.vocabulary_id                 as target_vocabulary
FROM concept_icd10cm cs -- for *c*oncept *s*ource (icd10cm)
  left  join v6.concept_source_to_target cst on cs.icd_concept_id = cst.source_concept_id
  left  join v6.concept ct                   on cst.target_concept_id = ct.concept_id -- for *c*oncept *t*arget (snomed)
  
--WHERE 
--  c1.concept_id in (SELECT 



  --[cdw_omop_1].[v6].[concept]