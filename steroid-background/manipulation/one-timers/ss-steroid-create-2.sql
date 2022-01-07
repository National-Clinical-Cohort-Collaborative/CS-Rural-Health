-- with cte as (    -- use cdw_omop_1
    SELECT
      ca.descendant_concept_id
      ,case
        when d.concept_name like '%drug implant%'   then 'systemic'
        when d.concept_name like '%inject%'         then 'systemic'
        when d.concept_name like '%oral%'           then 'systemic'
        when d.concept_name like '%pill%'           then 'systemic'
        when d.concept_name like '%syringe%'        then 'systemic'
        when d.concept_name like '%tablet%'         then 'systemic'

        when d.concept_name like '%inhal%'          then 'inhaled'

        when d.concept_name like '%nasal%'          then 'nasal'

        when d.concept_name like '%cream%'          then 'other'
        when d.concept_name like '%enema%'          then 'other'
        when d.concept_name like '%itch%'           then 'other'
        when d.concept_name like '%ointment%'       then 'other'
        when d.concept_name like '%ophthal%'        then 'other'
        when d.concept_name like '%otic%'           then 'other'
        when d.concept_name like '% pad%'           then 'other'
        when d.concept_name like '% paste%'         then 'other'
        when d.concept_name like '%rectal%'         then 'other'
        when d.concept_name like '%shampoo%'        then 'other'
        when d.concept_name like '%tape%'           then 'other'
        when d.concept_name like '%toothpaste%'     then 'other'
        when d.concept_name like '%topical%'        then 'other'
        when d.concept_name like '%vaginal%'        then 'other'
      end                        as guess
      ,d.concept_name            as descendent_name
      ,a.concept_name            as ancestor_name
      ,d.vocabulary_id
      ,d.standard_concept
      ,a.concept_class_id
      ,ca.ancestor_concept_id
      ,ca.min_levels_of_separation
      ,ca.max_levels_of_separation
      -- ,a.concept_id   as ancestor_id
      -- ,d.concept_id   as descendent_id
    FROM v6.concept_ancestor ca
      left join v6.concept a on ca.ancestor_concept_id = a.concept_id -- stands for ancestor
      left join v6.concept d on ca.descendant_concept_id = d.concept_id -- stands for descendant
    WHERE
      -- [descendant_concept_id] = 19121836
      -- and
      d.vocabulary_id != 'RxNorm Extension'  --d.vocabulary_id = 'RxNorm'
      -- and
      -- d.standard_concept = 'S'
      and
      a.concept_name in (          -- ancestor_concept_id
        'alclometasone'            -- 905151 --probably ignore b/c mostly topical
        ,'beclomethasone'          -- 1115572
        ,'betamethasone'           -- 92048
        ,'budesonide'              -- 939259
        ,'ciclesonide'             -- 902938
        ,'cloprednol'              -- 19050907
        ,'clotrimazole'            -- 1000632 --probably ignore b/c mostly topical
        ,'cortisone'               -- 1507705
        ,'cortivazol'              -- 19061907
        ,'deflazacort'             -- 19086888
        ,'dexamethasone'           -- 1518254
        ,'fluprednisolone'         -- 19111234
        ,'fluticasone'             -- 1149380
        ,'hydrocortisone'          -- 975125
        ,'hydrocortisone; systemic'-- 21602737
        ,'meprednisone'            -- 19009116
        ,'methylprednisolone'      -- 1506270
        ,'mometasone'              -- 905233
        ,'paramethasone'           -- 19027186
        ,'prednisolone'            -- 1550557
        ,'prednisone'              -- 1551099
        ,'prednylidene'            -- 19011127
        ,'triamcinolone'           -- 903963
      )
    ORDER BY
      a.concept_name
      ,d.concept_name
-- )

-- select * FROM CTE
-- WHERE
--   guess is null
-- ORDER BY
--   ancestor_name
--   ,descendent_name
