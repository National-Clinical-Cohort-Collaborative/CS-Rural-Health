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
    concept_code = 'R45.851'
    or
    left(concept_code, 6)  = 'T14.91' -- Suicide attempt
    or
    --left(concept_code, 3) in ('T36', 'T37', 'T38', 'T39')
    --or
    --left(concept_code, 2) in ('T4', 'T5')
    --or
    --left(concept_code, 3) in ('T60', 'T61', 'T62', 'T63', 'T64', 'T65')
    --or
    left(concept_code, 7)  = 'T71.122' -- Asphyxiation due to smothering under pillow, intentional self-harm
    or
    left(concept_code, 2) in ('X6', 'X7')
    or
    left(concept_code, 3) in ('X80', 'X81', 'X82', 'X83', 'X84')
    --or
    --concept_name like '%suicid%' -- grabs 'suicide' and 'suicidal'
    --and
    --concept_name like '%self%inf%'
  )
)

SELECT
  ct.concept_id                     as target_concept_id
  ,ct.domain_id                     as target_domain_id
  ,case
    when cs.icd_description like '%nonsuicid%'         then 'FALSE'   -- List before `'%suicid%'`
    when cs.icd_description like '%accidental%'        then 'FALSE'   
    when cs.icd_description like '%unintentional%'     then 'FALSE'   
    when cs.icd_description like '%assault%'           then 'FALSE'   
    when cs.icd_description like 'terrorism%'          then 'FALSE'   
    when cs.icd_description like 'personal history%'   then 'FALSE'   
    when cs.icd_description like '%suicid%'            then 'TRUE'
    when cs.icd_description like '%self-harm%'         then 'TRUE'
    else                                                    'FALSE'
  end                               as include_in_analysis        -- Analysis, starting from scratch
  ,'TRUE'                           as include_replicate_2017     -- To best replicate 2017 article
  ,''                               as category
  ,cs.icd_code
  ,cs.icd_description
  ,ct.concept_name                  as target_description
  ,cs.icd_concept_id
  ,ct.vocabulary_id                 as target_vocabulary
FROM concept_icd10cm cs -- for *c*oncept *s*ource (icd10cm)
  left  join v6.concept_source_to_target cst on cs.icd_concept_id = cst.source_concept_id
  left  join v6.concept ct                   on cst.target_concept_id = ct.concept_id -- for *c*oncept *t*arget (snomed)
WHERE 
  ct.concept_id is not null
ORDER BY icd_code

