Exceprts from steroid_pull
==================

https://unite.nih.gov/workspace/vector/view/ri.vector.main.workbook.69dc0fbc-b028-4c9f-a633-abc957c5284a?branch=master

Global Code
------------------

*none*

`person_tx`
------------------

```sql
--set val upper_bound=90; -- A patient has to have the drug within the past 90 days

with pt_steroid_class as (
  SELECT
    distinct
    d.person_id 
    ,1                                  as place_holder
    ,case
      when s.concept_id is not null then s.steroid_class
      when n.concept_id is not null then n.guess
      else                               'class not found'
    end                                                                 as tx_class
    ,cast(if(s.ingredient_names = 'dexamethasone', 1, 0) as boolean)    as dexamethasone
  FROM drug_exposure as d
    inner join Covid_positive_persons_LDS              as c on d.person_id = c.person_id 
    left  join rx_asthma_tx_steroid_classification     as s on d.drug_concept_id = s.concept_id -- stands for *s*teroid
    left  join rx_asthma_tx_nonsteroid_classification  as n on d.drug_concept_id = n.concept_id -- stands for *n*on steroid
  WHERE
    (
        (
            d.drug_exposure_end_date is not null
            and
            datediff(d.drug_exposure_end_date, c.date_of_earliest_covid_diagnosis) between 1 and 30
        )
        or
        (
            d.drug_exposure_end_date is null
            and
            datediff(d.drug_exposure_start_date, c.date_of_earliest_covid_diagnosis) between 1 and 365
        )
    )
    and
    (
        s.concept_id is not null
        or
        n.concept_id is not null
    )
    -- datediff(c.date_of_earliest_covid_diagnosis, d.drug_exposure_end_date) between 1 and ${upper_bound}
  --LIMIT 10000
)
,pt as( -- Collapse from [one row per person per steroid class] to [one row per person]
  SELECT
    p.person_id
    ,cast(biologic  as boolean)    as tx_nonsteroid_biologic
    ,cast(systemic  as boolean)    as tx_steroid_systemic
    ,cast(inhaled   as boolean)    as tx_steroid_inhaled
    ,cast(nasal     as boolean)    as tx_steroid_nasal
    ,cast(saba      as boolean)    as tx_nonsteroid_saba
    ,cast(unused    as boolean)    as tx_steroid_unused
    ,p.dexamethasone
  FROM pt_steroid_class p
  PIVOT(max(place_holder) 
    FOR tx_class in ('biologic', 'systemic', 'inhaled', 'nasal', 'saba', 'unused'))
)

SELECT
  pc.person_id
  ,case
    when tx_nonsteroid_biologic   = true then 'biologic'
    when tx_steroid_systemic      = true then 'steroid_systemic'
    when tx_steroid_inhaled       = true then 'steroid_inhaled'
    when tx_steroid_nasal         = true then 'steroid_nasal'
    when tx_nonsteroid_saba       = true then 'saba'
    when tx_steroid_unused        = true then 'no_tx_documented'  --'steroid_unused'
    else                                      'no_tx_documented'
  end                                                       as tx_v1
  ,case
    when tx_nonsteroid_biologic   = true and tx_steroid_systemic      = true then 'both'
    when tx_nonsteroid_biologic   = true then 'biologic'
    when tx_steroid_systemic      = true then 'steroid_systemic'
    when tx_steroid_inhaled       = true then 'steroid_inhaled'
    when tx_steroid_nasal         = true then 'steroid_nasal'
    when tx_nonsteroid_saba       = true then 'saba'
    when tx_steroid_unused        = true then 'no_tx_documented'  --'steroid_unused'
    else                                      'no_tx_documented'
  end                                                       as tx_v2
  ,case
    when tx_nonsteroid_biologic   = true then 'biologic'
    when tx_steroid_systemic      = true then 'steroid_systemic'
    when tx_steroid_inhaled       = true then 'steroid_inhaled_nasal'
    when tx_steroid_nasal         = true then 'steroid_inhaled_nasal'
    -- when tx_nonsteroid_saba       = true then 'saba'
    when tx_steroid_unused        = true then 'no_tx_documented'  --'steroid_unused'
    else                                      'no_tx_documented'
  end                                                       as tx_v3
  ,coalesce(tx_nonsteroid_biologic, false)                  as tx_nonsteroid_biologic
  ,coalesce(tx_steroid_systemic   , false)                  as tx_steroid_systemic
  ,coalesce(tx_steroid_inhaled    , false)                  as tx_steroid_inhaled
  ,coalesce(tx_steroid_nasal      , false)                  as tx_steroid_nasal
  ,coalesce(tx_nonsteroid_saba    , false)                  as tx_nonsteroid_saba
  ,coalesce(tx_steroid_unused     , false)                  as tx_steroid_unused
  ,coalesce(dexamethasone         , false)                  as dexamethasone  
FROM Covid_positive_persons_LDS as pc                                        -- stands for patient covid
  left  join pt                 as ps on pc.person_id = ps.person_id         -- stands for patient steroids
```

`person_asthma`
------------------

```sql
with dx as (
    SELECT
        distinct 
        dx.person_id
        ,case 
            when dx.condition_concept_id in (37116845, 45769350, 45769350, 45768965, 4145356) then 1 -- severe persistent asthma
            when dx.condition_source_value like '%J45.5%'                                     then 1 -- severe persistent asthma
            when dx.condition_concept_id in (45769351, 46273487, 45768964, 4142738          ) then 2 -- moderate persistent asthma
            when dx.condition_source_value like '%J45.4%'                                     then 2 -- moderate persistent asthma
            when dx.condition_concept_id in (45769352, 46270082, 45768963, 4143828          ) then 3 -- mild persistent asthma
            when dx.condition_source_value like '%J45.3%'                                     then 3 -- mild persistent asthma
            when dx.condition_concept_id in (45769438,  4138760,  4146581, 4146581          ) then 4 -- mild intermittent asthma
            when dx.condition_source_value like '%J45.2%'                                     then 4 -- mild intermittent asthma
            when dx.condition_source_value like '%J45.90%'                                    then 5 -- asthma - unspecified
            when dx.condition_source_value like '%J45.99%'                                    then 6 -- asthma - other
            else                                                                                   7
        end as asthma_cdc_category
    FROM condition_occurrence dx
        -- inner join codeset_asthma_cdc ca on p.condition_concept_id = ca.concept_id 
    WHERE dx.person_id in (SELECT distinct p.person_id FROM Covid_positive_persons_LDS p)
)

SELECT 
    d.person_id
    ,case
        when min(d.asthma_cdc_category) = 1 then 'severe persistent' 
        when min(d.asthma_cdc_category) = 2 then 'moderate persistent' 
        when min(d.asthma_cdc_category) = 3 then 'mild persistent' 
        when min(d.asthma_cdc_category) = 4 then 'mild intermittent' 
        when min(d.asthma_cdc_category) = 5 then 'unspecified' 
        when min(d.asthma_cdc_category) = 6 then 'other' 
        else                                     'none'
    end                                                                  as asthma_v1
    ,case
        when min(d.asthma_cdc_category) = 1 then 'severe'          -- 'severe persistent'              
        when min(d.asthma_cdc_category) = 2 then 'moderate mild'   -- 'moderate persistent'                
        when min(d.asthma_cdc_category) = 3 then 'moderate mild'   -- 'mild persistent'            
        when min(d.asthma_cdc_category) = 4 then 'moderate mild'   -- 'mild intermittent'              
        when min(d.asthma_cdc_category) = 5 then 'unspecified'     -- 'unspecified'        
        when min(d.asthma_cdc_category) = 6 then 'other'           -- 'other'  
        else                                     'none'            -- 'none'
    end                                                                  as asthma_v2
FROM Covid_positive_persons_LDS c 
    left  join dx d on 
        d.person_id = c.person_id
        -- and
        -- datediff(d.drug_exposure_start_date, c.date_of_earliest_covid_diagnosis) between 1 and 365
GROUP BY  d.person_id
```

`patient_covid_dx`
------------------

```sql
--filter for patients who had at least 1 visit documented between 1 and 90 days prior to first COVID dx.
with pt_visit_range as (
    SELECT
        p.person_id
        ,p.date_of_earliest_covid_diagnosis    as date_first_covid_dx
        ,min(v.visit_start_date)               as first_visit_date_in_3_mos
        ,max(v.visit_start_date)               as last_visit_start_date_in_3_months
    FROM Covid_positive_persons_LDS p
        inner join visit_occurrence v on p.person_id = v.person_id
    WHERE 
        datediff(p.date_of_earliest_covid_diagnosis, v.visit_start_date) between 1 and 365
    GROUP BY 
        p.person_id
        ,p.date_of_earliest_covid_diagnosis
)
SELECT
  pvr.person_id
  ,pvr.date_first_covid_dx
  ,pvr.first_visit_date_in_3_mos
  ,pvr.last_visit_start_date_in_3_months
--   ,c.covid_event_type
  ,coalesce(p.hypertension      ,0) as hypertension
  ,coalesce(p.upper_gi_bleed    ,0) as upper_gi_bleed
  ,coalesce(p.MI                ,0) as MI
  ,coalesce(p.CHF               ,0) as CHF
  ,coalesce(p.PVD               ,0) as PVD
  ,coalesce(p.stroke            ,0) as stroke
  ,coalesce(p.dementia          ,0) as dementia
  ,coalesce(p.pulmonary         ,0) as pulmonary
  ,coalesce(p.rheumatic         ,0) as rheumatic
  ,coalesce(p.PUD               ,0) as PUD
  ,coalesce(p.liver_mild        ,0) as liver_mild
  ,coalesce(p.liversevere       ,0) as liversevere
  ,coalesce(p.diabetes          ,0) as diabetes
  ,coalesce(p.dmcx              ,0) as dmcx
  ,coalesce(p.paralysis         ,0) as paralysis
  ,coalesce(p.renal             ,0) as renal
  ,coalesce(p.cancer            ,0) as cancer
  ,coalesce(p.mets              ,0) as mets
  ,coalesce(p.hiv               ,0) as hiv
  ,coalesce(p.multiple          ,0) as multiple
--   ,st.visit_concept_id
--   ,st.visit_start_date
--   ,st.visit_concept_name
--   ,st.visit_occurrence_id
--   ,st.AKI_in_hospital                           as AKI_in_hospital
--   ,st.ECMO                                      as ECMO
--   ,st.Invasive_Ventilation                      as Invasive_Ventilation
  ,coalesce(st.in_death_table, false)              as in_death_table
  ,st.age_at_visit_start_in_years_int              as age_at_visit_start_in_years_int
  ,case 
     when st.InpatientOrED = 1 then st.length_of_stay
     else                           null
  end                                             as length_of_stay
  ,st.Race                                        as Race
  ,st.Ethnicity                                   as Ethnicity
  ,cast(case
      when st.gender_concept_name = 'FEMALE' then 0
      when st.gender_concept_name = 'MALE'   then 1      
      else                                        null
  end as boolean)                                 as gender_male
  ,cast(case
    when st.smoking_status = 'Current or Former' then true
    when st.smoking_status = 'Non smoker'        then false
    else                                               null -- no null/missing smoking values as of Feb 2022
  end as boolean)                                 as smoking_ever
--   ,st.smoking_status                              as smoking_status
  ,st.Severity_Type                               as Severity_Type
  ,cast(if(st.Severity_Type = 'Dead_w_COVID', 1, 0) as boolean) as covid_fatality  
  ,coalesce(st.InpatientOrED, false)              as InpatientOrED
--   ,coalesce(st.Q_Score, 0)                        as Q_Score
  ,st.BMI                                         as bmi
  ,st.Height                                      as height
  ,st.Weight                                      as weight
--   ,coalesce(a.asthma_v1, 'none')                  as asthma_v1
--   ,coalesce(a.asthma_v2, 'none')                  as asthma_v2
  ,coalesce(d.tx_v1, 'no_tx_documented')          as tx_v1
  ,coalesce(d.tx_v2, 'no_tx_documented')          as tx_v2
  ,coalesce(d.tx_v3, 'no_tx_documented')          as tx_v3
--   ,d.steroid_systemic                             as steroid_systemic
--   ,d.steroid_inhaled                              as steroid_inhaled
--   ,d.steroid_nasal                                as steroid_nasal
  ,d.tx_nonsteroid_biologic                       as tx_nonsteroid_biologic
  ,d.tx_steroid_systemic                          as tx_steroid_systemic
  ,d.tx_steroid_inhaled                           as tx_steroid_inhaled
  ,d.tx_steroid_nasal                             as tx_steroid_nasal
  ,d.tx_nonsteroid_saba                           as tx_nonsteroid_saba
  ,d.tx_steroid_unused                            as tx_steroid_unused
  ,d.dexamethasone                                as dexamethasone
  ,st.data_partner_id                             as data_partner_id
  ,cast(row_number() over (partition by pvr.person_id order by pvr.date_first_covid_dx) as integer) covid_date_index_within_person
FROM pt_visit_range                                     pvr
  left  join complete_patient_table_with_derived_scores  st on pvr.person_id = st.person_id
  left  join person_tx                                    d on pvr.person_id =  d.person_id 
--   left  join person_asthma                                a on pvr.person_id =  a.person_id
  left  join preexisting_comorbidities_both               p on pvr.person_id =  p.person_id

```

`patient`
------------------

```sql
SELECT
    /*+ REPARTITION(3) */ 
    pd.person_id
    -- ,covid_event_type
    ,pd.date_first_covid_dx
    ,date_add(cast(date_trunc('month', pd.date_first_covid_dx) as date), 14) as month_first_covid_dx
    ,case
        when pd.date_first_covid_dx <= '2019-12-31' then 'too early'
        when pd.date_first_covid_dx <= '2020-03-30' then '2020Q1'
        when pd.date_first_covid_dx <= '2020-06-30' then '2020Q2'
        when pd.date_first_covid_dx <= '2020-09-30' then '2020Q3'
        when pd.date_first_covid_dx <= '2020-12-31' then '2020Q4'
        when pd.date_first_covid_dx <= '2021-03-30' then '2021Q1'
        when pd.date_first_covid_dx <= '2021-06-30' then '2021Q2'
        when pd.date_first_covid_dx <= '2021-09-30' then '2021Q3'
        when pd.date_first_covid_dx <= '2021-12-31' then '2021Q4'
        when pd.date_first_covid_dx <= '2022-03-30' then '2022Q1'
        when pd.date_first_covid_dx <= '2022-06-30' then '2022Q2'
        when pd.date_first_covid_dx <= '2022-09-30' then '2022Q3'
        when pd.date_first_covid_dx <= '2022-12-31' then '2022Q4'
        else                                             'error'
    end                                                           as quarter_first_covid_dx
    ,case
        when pd.date_first_covid_dx <= '2019-12-31' then 'too early'
        when pd.date_first_covid_dx <= '2020-06-30' then '2020H1'
        when pd.date_first_covid_dx <= '2020-12-31' then '2020H2'
        when pd.date_first_covid_dx <= '2021-06-30' then '2021H1'
        when pd.date_first_covid_dx <= '2021-12-31' then '2021H2'
        when pd.date_first_covid_dx <= '2022-06-30' then '2022H1'
        when pd.date_first_covid_dx <= '2022-12-31' then '2022H2'
        else                                             'error'
    end                                                           as period_first_covid_dx
    ,pd.first_visit_date_in_3_mos
    ,pd.last_visit_start_date_in_3_months
    -- ,pd.hypertension
    ,pd.upper_gi_bleed
    -- ,pd.MI
    -- ,pd.CHF
    -- ,pd.PVD
    -- ,pd.stroke
    ,pd.dementia
    ,pd.pulmonary
    ,pd.rheumatic
    ,pd.PUD
    ,pd.liver_mild
    -- ,pd.liversevere
    -- ,pd.diabetes
    -- ,pd.dmcx
    -- ,pd.paralysis
    -- ,pd.renal
    -- ,pd.cancer
    -- ,pd.mets
    -- ,pd.hiv
    ,pd.multiple
    ,pd.in_death_table
    ,pd.length_of_stay
    ,coalesce(     pd.hypertension      , 0) 
        + coalesce(pd.upper_gi_bleed    , 0)
        + coalesce(pd.MI                , 0)
        + coalesce(pd.CHF               , 0)
        + coalesce(pd.PVD               , 0)
        + coalesce(pd.stroke            , 0)
        + coalesce(pd.dementia          , 0)
        + coalesce(pd.pulmonary         , 0)
        + coalesce(pd.rheumatic         , 0)
        + coalesce(pd.PUD               , 0)
        + coalesce(pd.liver_mild        , 0)
        + coalesce(pd.liversevere       , 0)
        + coalesce(pd.diabetes          , 0)
        + coalesce(pd.dmcx              , 0)
        + coalesce(pd.paralysis         , 0)
        + coalesce(pd.renal             , 0)
        + coalesce(pd.cancer            , 0)
        + coalesce(pd.mets              , 0)
        + coalesce(pd.hiv               , 0)              as count_comorbidities
    ,pd.age_at_visit_start_in_years_int                   as age
    ,round(log10(pd.age_at_visit_start_in_years_int) , 4) as age_log10
    ,case
        when pd.age_at_visit_start_in_years_int is null then 'Unknown'
        when pd.age_at_visit_start_in_years_int < 0     then 'Unknown'
        when pd.age_at_visit_start_in_years_int < 19    then '0-18'
        when pd.age_at_visit_start_in_years_int < 51    then '19-50'
        when pd.age_at_visit_start_in_years_int < 76    then '51-75'
        else                                                 '76+'
    end                                  as age_cut5
    ,case
        when pd.Severity_Type = 'Dead_w_COVID' then TRUE
        when pd.Severity_Type = 'Severe'       then TRUE
        else                                        FALSE
    end                                                    as severe_dead
    ,case
        when pd.Severity_Type = 'Dead_w_COVID' then TRUE
        else                                        FALSE
    end                                                    as dead_w_covid
    ,pd.gender_male
    ,pd.smoking_ever
    ,pd.Severity_Type
    ,pd.covid_fatality
    ,pd.InpatientOrED                       as inpatient_ed
    ,pd.bmi
    ,case -- https://www.medicalnewstoday.com/articles/323622#BMI-in-children-and-teens
        when pd.bmi is null then 'missing'
        when pd.bmi < 18.5  then 'underweight'
        when pd.bmi < 25    then 'healthy'
        when pd.bmi < 30    then 'overweight'
        else                     'obese'
    end                                             as bmi_cut5
    ,pd.height
    ,pd.weight
    ,coalesce(a.asthma_v1, 'none')                  as asthma_v1
    ,coalesce(a.asthma_v2, 'none')                  as asthma_v2
    ,pd.tx_v1
    ,pd.tx_v2
    ,pd.tx_v3
    ,pd.dexamethasone
    ,pd.tx_nonsteroid_biologic
    ,pd.tx_steroid_systemic
    ,pd.tx_steroid_inhaled
    ,pd.tx_steroid_nasal
    ,pd.tx_nonsteroid_saba
    ,pd.tx_steroid_unused
    ,case 
        when pd.Ethnicity = 'Hispanic or Latino'                        then 'Hispanic or Latino'
        when pd.Race = 'White'                                          then 'White'
        when pd.Race = 'Black or African American'                      then 'Black/African American'
        when pd.Race = 'Asian'                                          then 'Asian'
        when pd.Race = 'Native Hawaiian or Other Pacific Islander'      then 'Native Hawaiian or Other Pacific Islander'
        when pd.Race in ('Other', 'Missing/Unknown')                    then 'Other/Unknown'
        else                                                                 'Other/Unknown'
    end                                                  as race_v1
    ,case 
        when pd.Ethnicity = 'Hispanic or Latino'                        then 'Latino'
        when pd.Race = 'White'                                          then 'White'
        when pd.Race = 'Black or African American'                      then 'Black'
        when pd.Race = 'Asian'                                          then 'Other/Unknown'
        when pd.Race = 'Native Hawaiian or Other Pacific Islander'      then 'Other/Unknown'
        when pd.Race in ('Other', 'Missing/Unknown')                    then 'Other/Unknown'
        else                                                                 'Other/Unknown'
    end                                                  as race_v2
    ,pd.data_partner_id
    ,pi.propensity_slice_id                              as partner_slice_id
FROM patient_covid_dx pd
  left  join person_asthma       a on pd.person_id =  a.person_id
  left  join partner_include    pi on pd.data_partner_id = pi.data_partner_id
WHERE 
    pd.covid_date_index_within_person = 1
    and
    a.asthma_v1 != 'other'
    and
    pd.date_first_covid_dx between '2020-07-01' and '2021-12-31'
    and
    pd.gender_male is not null -- one missing value
    and
    (
        pd.hypertension = 0 and
        -- pd.upper_gi_bleed = 0 and
        pd.MI = 0 and
        pd.CHF = 0 and
        pd.PVD = 0 and
        pd.stroke = 0 and
        -- pd.dementia = 0 and
        -- pd.pulmonary = 0 and
        -- pd.rheumatic = 0 and
        -- pd.PUD = 0 and   -- peptic ulcer
        -- pd.liver_mild = 0 and
        pd.liversevere = 0 and
        pd.diabetes = 0 and    -- diabetes w/o chronic complications
        pd.dmcx = 0 and        -- diabetes w/  chronic complications
        pd.paralysis = 0 and
        pd.renal = 0 and
        pd.cancer = 0 and
        pd.mets = 0 and   -- metastatic?
        pd.hiv = 0
    )
```
