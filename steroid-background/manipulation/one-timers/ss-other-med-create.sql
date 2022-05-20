-- produces  concept-sets/rx-asthma-tx-nonsteroid-classification.csv

with ingredient as (
  SELECT
    concept_id
  FROM v6.concept
  WHERE
    concept_id in (  -- ancestor concept name
      1154343        -- saba: albuterol
      ,1192218       -- saba: levalbuterol

      ,1593467       -- biologics: dupilumab
      ,792993        -- biologics: benralizumab
      ,35606631      -- biologics: mepolizumab
      ,35603983      -- biologics: reslizumab
      ,1110942       -- biologics: omalizumab

      --,19034275      -- saba: bambuterol we're ignoring this because it won't be used clinically
      --,45774639      -- biologics: vedolizumab  not used for asthma; used primary for GI
      --,936429        -- biologics: efalizumab   not used for asthma; withdrawn from the market in 2009 related to lvier disease
      --,1110942       -- biologics: ibalizumab   not used for asthma; used primary for HIV;
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
    when co.ingredient_names like '%dupilumab%'      then 'biologic'
    when co.ingredient_names like '%benralizumab%'   then 'biologic'
    when co.ingredient_names like '%mepolizumab%'    then 'biologic'
    when co.ingredient_names like '%reslizumab%'     then 'biologic'
    when co.ingredient_names like '%omalizumab%'     then 'biologic'

    when co.ingredient_names like '%albuterol%'      then 'saba'  -- stands for short acting beta agonist
    when co.ingredient_names like '%levalbuterol%'   then 'saba'
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
