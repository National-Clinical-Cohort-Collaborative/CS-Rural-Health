-- use cdw_omop_1;

with dx_core as (
  SELECT
    concept_id
  FROM v6.concept
  WHERE
    concept_id in (  -- ancestor concept name
      1572413,     -- ICD10CM for "Intracranial injury"
      10655        -- "Crushing injury of skull"
    )
)
,downstream as (
  SELECT
    cr.concept_id_2                              as concept_id
    ,cr.concept_id_1                             as dx_core_concept_id
  FROM [v6].[concept_relationship]  cr
    left  join v6.concept c1 on cr.concept_id_1 = c1.concept_id
    left  join v6.concept c2 on cr.concept_id_2 = c2.concept_id
  WHERE
    cr.invalid_reason is null
    and
    cr.relationship_id in ('Maps to', 'Subsumes') -- not 'Mapped from'
    and
    -- cr.concept_id_1 != cr.concept_id_2
    -- and
    c1.concept_id in (SELECT dc.concept_id FROM dx_core dc)
)
,descendant as (
  SELECT
    ca.descendant_concept_id                      as concept_id
    ,ca.ancestor_concept_id                       as dx_core_concept_id
  FROM v6.concept_ancestor ca
    inner join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
    inner join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
  WHERE
    -- d.standard_concept = 'S'
    -- and
    -- d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
    -- and
    ca.ancestor_concept_id in (SELECT dc.concept_id FROM dx_core dc)
)
,stack as (
  SELECT concept_id, dx_core_concept_id FROM downstream
  UNION
  SELECT concept_id, dx_core_concept_id FROM descendant
)
,collapsed as (
  SELECT
    s.concept_id
      ,string_agg(s.dx_core_concept_id, ';')         as dx_core_concept_ids
      ,string_agg(i.concept_name, ';')               as dx_core_concept_names
      ,count(distinct s.dx_core_concept_id)          as dx_core_count
  FROM stack s
    inner join v6.concept i on s.dx_core_concept_id = i.concept_id
  GROUP BY s.concept_id
)
-- ,cte as (
SELECT
  co.concept_id
  -- ,case
  --   when c.concept_name like '%drug implant%'   then 'systemic'
  --   when c.concept_name like '%inject%'         then 'systemic'
  --   when c.concept_name like '%oral%'           then 'systemic'
  --   when c.concept_name like '%inhal%'          then 'inhaled'
  --   when c.concept_name like '%nasal%'          then 'nasal'
  --   when c.concept_name like '%cream%'          then 'other'
  --   when c.concept_name like '%enema%'          then 'other'
  -- end                        as guess
  ,co.dx_core_concept_ids
  ,co.dx_core_concept_names
  ,co.dx_core_count
  ,c.concept_name
  ,c.vocabulary_id
  ,c.concept_class_id
  ,c.standard_concept
FROM collapsed co
  inner join v6.concept c on co.concept_id = c.concept_id
ORDER BY dx_core_concept_names, c.concept_name
-- )

-- SELECT
--   concept_id
--   ,concept_name
-- FROM cte
-- WHERE guess is null
-- ORDER BY concept_name
