
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
    cr.concept_id_2                              as concept_id
    ,cr.concept_id_1                             as ingredient_concept_id
    -- ,string_agg(c2.concept_name, ';')            as ingredient_names
    -- ,string_agg(cr.concept_id_1, ';')            as ingredient_concept_ids
    -- ,group_concat(c2.concept_name, ';')          as ingredient_names
    -- ,group_concat(cr.concept_id_1, ';')          as ingredient_concept_ids
  FROM [v6].[concept_relationship]  cr
    left  join v6.concept c1 on cr.concept_id_1 = c1.concept_id
    left  join v6.concept c2 on cr.concept_id_2 = c2.concept_id
  WHERE
    cr.invalid_reason is null
    and
    cr.relationship_id = 'Mapped from'
    and
    -- cr.concept_id_1 != cr.concept_id_2
    -- and
    c1.concept_class_id = 'ingredient'
    and
    c2.vocabulary_id != 'RxNorm Extension'
    and
    c1.concept_id in (SELECT i.concept_id FROM ingredient i)
    --and
    --c2.concept_name like '%dexameth%'
  -- GROUP BY cr.concept_id_2
)
,descendant as (
  SELECT
    ca.descendant_concept_id                      as concept_id
    ,ca.ancestor_concept_id                       as ingredient_concept_id
    -- ,string_agg(a.concept_name, ';')              as ingredient_names          -- about 100 duplicates
    -- ,string_agg(ca.ancestor_concept_id, ';')      as ingredient_concept_ids
    -- ,group_concat(a.concept_name, ';')            as ingredient_names
    -- ,group_concat(ca.ancestor_concept_id, ';')    as ingredient_concept_ids
  FROM v6.concept_ancestor ca
    inner join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
    inner join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
  WHERE
    -- d.standard_concept = 'S'
    -- and
    d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
    and
    ca.ancestor_concept_id in (SELECT i.concept_id FROM ingredient i)
  -- GROUP BY ca.descendant_concept_id
)
,stack as (
  SELECT
    concept_id
    ,ingredient_concept_id
  FROM downstream
  UNION
  SELECT
    concept_id
    ,ingredient_concept_id
  FROM descendant
)
,collapsed as (
  SELECT
    s.concept_id
      ,string_agg(s.ingredient_concept_id, ';')      as ingredient_concept_ids -- about 100 duplicates
      ,string_agg(i.concept_name, ';')               as ingredient_names
      -- ,group_concat(s.ingredient_concept_id, ';')   as ingredient_concept_ids
      -- ,group_concat(i.concept_name, ';')            as ingredient_names
      ,count(distinct s.ingredient_concept_id)       as ingredient_count
  FROM stack s
    inner join v6.concept i on s.ingredient_concept_id = i.concept_id
  GROUP BY s.concept_id
)
SELECT
  co.concept_id
  ,co.ingredient_concept_ids
  ,co.ingredient_names
  ,co.ingredient_count
  ,c.concept_name
  ,c.vocabulary_id
  ,c.concept_class_id
  ,c.standard_concept
FROM collapsed co
  inner join v6.concept c on co.concept_id = c.concept_id
ORDER BY ingredient_names
-- SELECT
--   *
-- FROM v6.concept_ancestor ca
--   left  join v6.concept c2 on ca.descendant_concept_id = c2.concept_id
-- WHERE
--   ca.ancestor_concept_id in (SELECT do.concept_id FROM downstream do)
--   and
--   -- ca.descendant_concept_id not in (SELECT d.concept_id FROM descendant d)
--   -- and
--   c2.vocabulary_id != 'RxNorm Extension'
-- ORDER BY ca.ancestor_concept_id, c2.concept_name
