
with ingredient as (
      SELECT
        concept_id
      FROM v6.concept
      WHERE
        concept_id in (  -- ancestor concept name
          905151           -- 'alclometasone'                      -- probably ignore b/c mostly topical
          ,1115572         -- 'beclomethasone'
          ,92048           -- 'betamethasone'
          ,939259          -- 'budesonide'
          ,902938          -- 'ciclesonide'
          ,19050907        -- 'cloprednol'
          ,1000632         -- 'clotrimazole'                       -- probably ignore b/c mostly topical
          ,1507705         -- 'cortisone'
          ,19061907        -- 'cortivazol'
          ,19086888        -- 'deflazacort'
          ,1518254         -- 'dexamethasone'
          ,19111234        -- 'fluprednisolone'
          ,1149380         -- 'fluticasone'
          ,975125          -- 'hydrocortisone'
          ,21602737        -- 'hydrocortisone; systemic'
          ,19009116        -- 'meprednisone'
          ,1506270         -- 'methylprednisolone'
          ,905233          -- 'mometasone'
          ,19027186        -- 'paramethasone'
          ,1550557         -- 'prednisolone'
          ,1551099         -- 'prednisone'
          ,19011127        -- 'prednylidene'
          ,903963          -- 'triamcinolone'
        )
    )
,downstream as (
  SELECT
    --top 1000
    --concept_id_1
    --,
    concept_id_2           as concept_id
    --,c1.concept_name
    --,c2.concept_name
    --,c2.*
    ----,*
  FROM [v6].[concept_relationship]  cr
    left  join v6.concept c1 on cr.concept_id_1 = c1.concept_id
    left  join v6.concept c2 on cr.concept_id_2 = c2.concept_id
  WHERE
    cr.invalid_reason is null
    and
    cr.relationship_id = 'Mapped from'
    and
    --cr.concept_id_1 = cr.concept_id_2
    --and
    c1.concept_class_id = 'ingredient'
    and
    c2.vocabulary_id != 'RxNorm Extension'
    and
    c1.concept_id in (SELECT i.concept_id FROM ingredient i)
    --and
    --c2.concept_name like '%dexameth%'
)
,descendant as (
  SELECT
    ca.descendant_concept_id                      as concept_id
    -- ,group_concat(a.concept_name, ';')            as ancestor_names
    -- ,group_concat(ca.ancestor_concept_id, ';')    as ancestor_concept_ids
    -- -- ,ca.min_levels_of_separation
    -- -- ,ca.max_levels_of_separation
    -- -- ,a.concept_id   as ancestor_id
    -- -- ,d.concept_id   as descendent_id
  FROM v6.concept_ancestor ca
    inner join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
    inner join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
  WHERE
    -- d.standard_concept = 'S'
    -- and
    d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
    and
    ca.ancestor_concept_id in (SELECT concept_id FROM ingredient)
  GROUP BY ca.descendant_concept_id
)

SELECT
  *
FROM v6.concept_ancestor ca
  left  join v6.concept c2 on ca.descendant_concept_id = c2.concept_id
WHERE
  ca.ancestor_concept_id in (SELECT do.concept_id FROM downstream do)
  and
  -- ca.descendant_concept_id not in (SELECT d.concept_id FROM descendant d)
  -- and
  c2.vocabulary_id != 'RxNorm Extension'
ORDER BY ca.ancestor_concept_id, c2.concept_name
