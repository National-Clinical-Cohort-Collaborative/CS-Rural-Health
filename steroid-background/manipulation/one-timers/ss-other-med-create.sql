
with ingredient as (
  SELECT
    concept_id
  FROM v6.concept
  WHERE
    concept_id in (  -- ancestor concept name
      905151           -- 'alclometasone'                      -- probably ignore b/c mostly topical
      ,1115572         -- 'beclomethasone'
      ,92048           -- 'betamethasone'

    )
)
,downstream as (
  SELECT
    cr.concept_id_2                              as concept_id
    ,cr.concept_id_1                             as ingredient_concept_id
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
)
,descendant as (
  SELECT
    ca.descendant_concept_id                      as concept_id
    ,ca.ancestor_concept_id                       as ingredient_concept_id
  FROM v6.concept_ancestor ca
    inner join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
    inner join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
  WHERE
    -- d.standard_concept = 'S'
    -- and
    d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
    and
    ca.ancestor_concept_id in (SELECT i.concept_id FROM ingredient i)
)
,stack as (
  SELECT concept_id, ingredient_concept_id FROM downstream
  UNION
  SELECT concept_id, ingredient_concept_id FROM descendant
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
-- ,cte as (
SELECT
  co.concept_id
  ,case
    when c.concept_name like '%drug implant%'   then 'systemic'
    when c.concept_name like '%inject%'         then 'systemic'
    when c.concept_name like '%oral%'           then 'systemic'
    when c.concept_name like '%pill%'           then 'systemic'
    when c.concept_name like '%syringe%'        then 'systemic'
    when c.concept_name like '% systemic%'      then 'systemic'
    when c.concept_name like '%tablet%'         then 'systemic'

    when c.concept_name like '%inhal%'          then 'inhaled'

    when c.concept_name like '%nasal%'          then 'nasal'

    when c.concept_name like '%cream%'          then 'other'
    when c.concept_name like '%enema%'          then 'other'
    when c.concept_name like '%itch%'           then 'other'
    when c.concept_name like '%ointment%'       then 'other'
    when c.concept_name like '%ophthal%'        then 'other'
    when c.concept_name like '%otic%'           then 'other'
    when c.concept_name like '% pad%'           then 'other'
    when c.concept_name like '% paste%'         then 'other'
    when c.concept_name like '%rectal%'         then 'other'
    when c.concept_name like '%shampoo%'        then 'other'
    when c.concept_name like '% soap%'          then 'other'
    when c.concept_name like '%spray%'          then 'other'
    when c.concept_name like '%tape%'           then 'other'
    when c.concept_name like '%toothpaste%'     then 'other'
    when c.concept_name like '%topical%'        then 'other'
    when c.concept_name like '%vaginal%'        then 'other'
  end                        as guess
  ,co.ingredient_concept_ids
  ,co.ingredient_names
  ,co.ingredient_count
  ,c.concept_name
  ,c.vocabulary_id
  ,c.concept_class_id
  ,c.standard_concept
FROM collapsed co
  inner join v6.concept c on co.concept_id = c.concept_id
ORDER BY ingredient_names, c.concept_name
-- )

-- SELECT
--   concept_id
--   ,concept_name
-- FROM cte
-- WHERE guess is null
-- ORDER BY concept_name
