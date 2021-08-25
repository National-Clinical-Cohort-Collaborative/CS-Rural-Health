use cdw_omop_1

--this produces the inhaled-corticosteroid concept set
SELECT
  --csm.codeset
  c.concept_id
  ,c.concept_name
  ,c.standard_concept
  ,'' as standard_concept_caption
  ,c.invalid_reason
  ,'' as invalid_reason_caption
  ,c.concept_code
  ,c.domain_id
  ,c.vocabulary_id
  ,c.concept_class_id
FROM  v6.concept c
WHERE 
  domain_id = 'drug'
  and
  vocabulary_id != 'RXNorm Extension'
  and 
  (
    concept_name like '%fluticasone%'
    or
    concept_name like '%Ciclesonide%'
    or
    concept_name like '%Mometasone%'
    or
    concept_name like '%Budesonide%'
    or
    concept_name like '%Beclomethasone%'
   )
  --and 
  --standard_concept !=  'C'

--this produces the nasal spray concept set

SELECT
  --csm.codeset
  c.concept_id
  ,c.concept_name
  ,c.standard_concept
  ,'' as standard_concept_caption
  ,c.invalid_reason
  ,'' as invalid_reason_caption
  ,c.concept_code
  ,c.domain_id
  ,c.vocabulary_id
  ,c.concept_class_id
FROM  v6.concept c
WHERE 
  domain_id = 'drug'
  and
  vocabulary_id != 'RXNorm Extension'
  and 
  (
    concept_name like '%Budesonide%'
    or
    concept_name like '%Fluticasone%'
    or
    concept_name like '%Ciclesonide%'
    or
    concept_name like '%Triamcinolone%' 
    or
    concept_name like '%Mometasone%'
    or
    concept_name like '%Beclometasone%' 
  )

