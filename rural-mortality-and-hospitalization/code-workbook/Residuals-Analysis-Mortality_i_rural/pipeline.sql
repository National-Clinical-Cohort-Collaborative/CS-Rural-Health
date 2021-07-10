

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.eb2276ff-d83f-4e94-85bd-e8cea8b9b339"),
    covid_19_positive_patients_raw=Input(rid="ri.foundry.main.dataset.7daabdb1-b5f9-4858-aa1d-fc78f3ce6f14"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
with s_ip_ever as (SELECT distinct person_id
        FROM visit_occurrence
        where visit_concept_name like '%Inpatient%'
)
select distinct data_partner_id, count(distinct person_id) as npats ,avg(ever_ip) as ip_rate
    from (
        SELECT cov.person_id, data_partner_id, case when ever_ip is null then 0 else 1 end as ever_ip
        FROM (select distinct person_id,data_partner_id from covid_19_positive_patients_raw )cov
        left join (select person_id, 1 as ever_ip from s_ip_ever) inpatient
        on cov.person_id = inpatient.person_id
    )a
group by data_partner_id
--where ip_rate>0
order by ip_rate desc

@transform_pandas(
    Output(rid="ri.vector.main.execute.22f0cc55-4cdd-499a-8282-293e42178bea"),
    covid_19_positive_patients_raw=Input(rid="ri.foundry.main.dataset.7daabdb1-b5f9-4858-aa1d-fc78f3ce6f14"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
with s_ip_ever as (SELECT distinct person_id
        FROM visit_occurrence
        where visit_concept_name like '%Inpatient%'
)
select distinct data_partner_id, count(distinct person_id) as npats ,avg(ever_ip) as ip_rate
    from (
        SELECT cov.person_id, data_partner_id, case when ever_ip is null then 0 else 1 end as ever_ip
        FROM (select distinct person_id,data_partner_id from covid_19_positive_patients_raw )cov
        left join (select person_id, 1 as ever_ip from s_ip_ever) inpatient
        on cov.person_id = inpatient.person_id
    )a
group by data_partner_id
--where ip_rate>0
order by ip_rate desc

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.87cf0aae-9a08-46c5-8272-65bb40bf1824"),
    s1_summary2_for_analysis=Input(rid="ri.foundry.main.dataset.3e6ac662-c355-49a8-b908-84c70f95b3b2")
)
SELECT distinct person_id,ttdeath,  in_death_table, four_category_ruca as ruca_cat,
        CASE
            WHEN four_category_ruca = 'urban' THEN '0'
            ELSE '1'
        END AS i_rural,
        gender,
        CASE
            WHEN age < 18 THEN '0-17'
            WHEN age BETWEEN 18 AND 29 THEN '18-29'
            WHEN age BETWEEN 30 AND 39 THEN '30-39'
            WHEN age BETWEEN 40 AND 49 THEN '40-49'
            WHEN age BETWEEN 50 AND 59 THEN '50-59'
            WHEN age BETWEEN 60 AND 69 THEN '60-69'
            WHEN age BETWEEN 70 AND 79 THEN '70-79'
            WHEN age >80 THEN '80-120'
            ELSE 'Unk_Age'
       END AS age_Group,
       age,
       CASE
            WHEN Race = 'White' then 'White'
            When Race = 'Black or African American' then 'Black'
            WHEN Race = 'Missing/Unknown' THEN 'Unk_Race'
            ELSE 'Other'
        END AS Race,
        case
            when Ethnicity='Hispanic or Latino' then 'Hispanic'
            when Ethnicity='Not Hispanic or Latino' then 'Not Hispanic'
            else 'Unk_Ethnicity'
        end as Ethnicity,
        CASE
            WHEN BMI < 18.5 THEN '<18.5'
            WHEN BMI >= 18.5 AND BMI < 25 THEN '18.5-24.9'
            WHEN BMI >= 25 AND BMI < 30 THEN '25-29.9'
            WHEN BMI > 30 THEN '30+'
            ELSE 'Unk_BMI'
        END AS BMI_Group,
        BMI,
        CASE
            WHEN Q_Score < 1 THEN '<1.0'
            WHEN Q_Score BETWEEN 1 AND 2 THEN '1.0-2.0'
            WHEN Q_Score > 2 THEN '2.0+'
            ELSE 'Unk_Qscore'
        END AS Q_cat,
        Q_Score,
        Diabetes, MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic, Liver, paralysis, renal, cancer, mets, hiv,
        case
            when Diabetes =1 then 1
			when MI =1 then 1
			when CHF =1 then 1
			when PVD =1 then 1
			when stroke =1 then 1
			when dementia =1 then 1
			when pulmonary =1 then 1
            when rheumatic =1 then 1
			when Liver =1 then 1
			when paralysis =1 then 1
			when renal =1 then 1
			when cancer =1 then 1
			when mets =1 then 1
			when hiv =1 then 1
            else 0
        end as any_disease,
        Severity_Type, ECMO, MACE, Mechnical_Ventilation,
        covid_peak, length_of_stay as los,
         CASE
            WHEN quarter_of_diagnosis = 'Q1 2020' THEN '2020q1'
            WHEN quarter_of_diagnosis = 'Q2 2020' THEN '2020q2'
            WHEN quarter_of_diagnosis = 'Q3 2020' THEN '2020q3'
            WHEN quarter_of_diagnosis = 'Q4 2020' THEN '2020q4'
            WHEN quarter_of_diagnosis = 'Q1 2021' THEN '2021q1'
            ELSE 'Missing'
        END AS  quarter_dx,
        data_partner_id
FROM s1_summary2_for_analysis

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.64f9d685-6967-4af5-9656-9d3044b0493e"),
    s1_dp_cov_ip_rate=Input(rid="ri.foundry.main.dataset.eb2276ff-d83f-4e94-85bd-e8cea8b9b339"),
    s1_forSan=Input(rid="ri.foundry.main.dataset.87cf0aae-9a08-46c5-8272-65bb40bf1824")
)
SELECT row_number() over(order by person_id)  as ipat--cov.person_id 722,141
    ,ttdeath,in_death_table
    ,ruca_cat,i_rural
    ,gender,age_Group,age,Race,Ethnicity
    ,BMI_Group,BMI
    ,Q_cat,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv,any_disease
    ,Severity_Type,ECMO,MACE,Mechnical_Ventilation
    ,covid_peak,los,quarter_dx
    ,cast(cov.data_partner_id as varchar(4)) as data_partner_id
    ,npats
    ,ip_rate
FROM s1_forSan cov
    inner join s1_dp_cov_ip_rate dp_ip
    on cov.data_partner_id = dp_ip.data_partner_id
        and age_Group <> 'Unknown'
        and gender <> 'Other'
        and quarter_dx <> 'Missing'
        and age between 18 and 79

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3e6ac662-c355-49a8-b908-84c70f95b3b2"),
    inpatient_summary_for_analysis=Input(rid="ri.foundry.main.dataset.b1b12bb9-d01f-4ee7-8505-ec93207c7d0c"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    pers_with_covid_peak_reference=Input(rid="ri.foundry.main.dataset.cef3672e-405f-473d-af51-913f341c2fcb")
)
SELECT i.person_id, ttdeath, four_category_ruca,
        CASE
            WHEN age_at_visit_start_in_years_int BETWEEN 0 AND 120 THEN age_at_visit_start_in_years_int
            ELSE null
        END AS age,
        CASE
            WHEN four_category_ruca = 'urban' THEN '0'
            ELSE '1'
        END AS i_rural,
       i.gender_concept_name AS gender,
       CASE
            WHEN age_at_visit_start_in_years_int < 18 THEN '0-17'
            WHEN age_at_visit_start_in_years_int BETWEEN 18 AND 29 THEN '18-29'
            WHEN age_at_visit_start_in_years_int BETWEEN 30 AND 39 THEN '30-39'
            WHEN age_at_visit_start_in_years_int BETWEEN 40 AND 49 THEN '40-49'
            WHEN age_at_visit_start_in_years_int BETWEEN 50 AND 59 THEN '50-59'
            WHEN age_at_visit_start_in_years_int BETWEEN 60 AND 69 THEN '60-69'
            WHEN age_at_visit_start_in_years_int BETWEEN 70 AND 79 THEN '70-79'
            WHEN age_at_visit_start_in_years_int BETWEEN 80 and 120 THEN '80-120'
            ELSE 'Unknown/Missing'
       END AS age_Group,
       CASE
            WHEN Race = 'Native Hawaiian or Other Pacific Islander' THEN 'Other'
            ELSE Race
        END AS Race,
       Ethnicity,
        CASE
            WHEN BMI BETWEEN 10 AND 18.5 THEN '<18.5'
            WHEN BMI >= 18.5 AND BMI < 25 THEN '18.5-24.9'
            WHEN BMI >= 25 AND BMI < 30 THEN '25-29.9'
            WHEN BMI BETWEEN 30 AND 75 THEN '>30'
            WHEN BMI > 10 THEN 'Unknown/Missing'
            WHEN BMI < 75 THEN 'Unknown/Missing'
            ELSE 'Unknown/Missing'
       END AS BMI_Group, BMI,
       CASE
              WHEN Q_Score < 1 THEN '<1.0'
              WHEN Q_Score BETWEEN 1 AND 2 THEN '1.0-2.0'
              WHEN Q_Score > 2 THEN '>2.0'
              ELSE 'Unknown/Missing'
        END AS Q_Score_Categories, Q_Score,
        CASE
            WHEN diabetes = 1 THEN 1
            WHEN dmcx = 1 THEN 1
            ELSE 0
        END AS Diabetes,
        MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic,
        CASE
            WHEN liver_mild = 1 THEN 1
            WHEN liversevere = 1 THEN 1
            ELSE 0
        END AS Liver,
        paralysis, renal, cancer, mets, hiv, Severity_Type, rECMO AS ECMO, MACE, Mechnical_Ventilation, CASE WHEN in_death_table IS NOT NULL THEN 1 ELSE 0 END AS in_death_table, covid_peak, length_of_stay,
        CASE
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' AND '2020-03-31' THEN 'Q1 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-04-01' AND '2020-06-30' THEN 'Q2 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-07-01' AND '2020-09-30' THEN 'Q3 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-10-01' AND '2020-12-31' THEN 'Q4 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2021-01-01' AND '2021-03-31' THEN 'Q1 2021'
            ELSE 'Unknown/Missing'
        END AS quarter_of_diagnosis,
p1.data_partner_id,
CAST(i.CCI_INDEX AS DOUBLE),
       CASE
              WHEN CCI_INDEX < 1 THEN '<1.0'
              WHEN CCI_INDEX BETWEEN 1 AND 2 THEN '1.0-2.0'
              WHEN CCI_INDEX > 2 THEN '>2.0'
        END AS CCI_Categories
    FROM   inpatient_summary_for_analysis i
    LEFT JOIN pers_with_covid_peak_reference p on p.person_id = i.person_id
    JOIN pers p1 ON p1.person_id = i.person_id
    JOIN manifest m on m.data_partner_id = p1.data_partner_id
    WHERE i.date_of_earliest_covid_diagnosis <= '2021-03-31'

@transform_pandas(
    Output(rid="ri.vector.main.execute.7c311da2-6cdf-4937-8033-e323191775a9"),
    s_pers_with_covid_peak_reference=Input(rid="ri.foundry.main.dataset.cef3672e-405f-473d-af51-913f341c2fcb"),
    s_visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
with s_ip_ever as (SELECT distinct person_id, data_partner_id
        FROM s_visit_occurrence
        where visit_concept_name like '%Inpatient%'
)
select distinct data_partner_id, count(distinct person_id) as npats ,avg(ever_ip) as ip_rate
    from (
        SELECT person_id, data_partner_id , case when ever_ip is null then 0 else 1 end as ever_ip
        FROM (select distinct cov.person_id,data_partner_id,ever_ip
                from s_pers_with_covid_peak_reference cov
                    left join (select person_id, data_partner_id, 1 as ever_ip from s_ip_ever) inpatient
                        on cov.person_id = inpatient.person_id
        )b
    )a
group by data_partner_id
--where ip_rate>0
order by ip_rate desc

@transform_pandas(
    Output(rid="ri.vector.main.execute.2d8b74d7-eacf-430e-ad66-5d4ef6285ad5"),
    s_inpatient_summary_for_analysis=Input(rid="ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576")
)
with s_dp_rural as
    (select distinct person_id, data_partner_id
        , CASE
            WHEN four_category_ruca = 'urban' THEN 0
            ELSE 1
        END AS i_rural
    from s_inpatient_summary_for_analysis
    )
select data_partner_id, sum(i_rural) as npats, avg(i_rural) as r_rural
from s_dp_rural
group by data_partner_id
/*SELECT distinct data_partner_id, npats , avg(i_rural) as r_rural
    FROM unnamed_15
    group by data_partner_id, npats)
    */

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b4fe8042-f99d-4db1-afcc-2863e9896760"),
    s_interact_NOTinvars=Input(rid="ri.foundry.main.dataset.7dc6d77a-edbf-4c00-834d-ac149c707e21"),
    s_interact_invars=Input(rid="ri.foundry.main.dataset.56725eae-2284-4cb3-a920-ac17601fcc18")
)
with comb as (SELECT 1 as invar, varname, i_rural,x,npats,rs2_m, npats*rs2_m as prod
                FROM s_interact_invars
            union
            SELECT 0 as invar, varname, i_rural, x, npats, rs2_m, npats*rs2_m as prod
                FROM s_interact_NOTinvars)
SELECT invar, varname, i_rural, x, npats, rs2_m,prod, xsum_npats,xsum_prod,rsum_npats,rsum_prod
FROM (SELECT  invar, varname, i_rural, x, npats, rs2_m,prod, xsum_npats,xsum_prod--,rsum_npats,rsum_prod
    FROM comb
        left join (SELECT varname as cx_varname, x as  cx_x , sum(npats) as xsum_npats , sum(prod) as xsum_prod
            FROM comb
            group by varname, x) ccx
        on varname = cx_varname and x= cx_x) comb_ccx
    join (SELECT cr_varname, cr_i_rural,x as cr_x, rsum_npats,rsum_prod
        FROM comb
        left join (SELECT varname as cr_varname, i_rural as  cr_i_rural , sum(npats) as rsum_npats , sum(prod) as rsum_prod
            FROM comb
            group by varname, i_rural) ccr
        on varname = cr_varname and i_rural = cr_i_rural) comb_ccr
    on varname = cr_varname and x= cr_x and i_rural = cr_i_rural


/*    left join (as SELECT varname as cr_varname, i_rural as  cr_i_rural , sum(npats) as rsum_npats , sum(prod) as rsum_prod
            FROM comb
            group by varname, i_rural) ccr
        on varname = cr_varname and i_rural = cr_i_rural
*/

/*, ccr as SELECT varname as cr_varname, i_rural as  cr_i_rural , sum(npats) as nr_subtotal , sum(prod) as prodr
    FROM comb
    group by varname, i_rural*/
/*select  invar, varname, i_rural, x,  npats, subtotal, rs2_m
        , npats/subtotal as ppats , rs2_m*npats/subtotal as rs2_m_adj
from comb
    inner join comb_collapse
    on varname = cl_varname and x = cl_x
order by invar, varname, i_rural , x
*/

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e513da2c-7b4b-4c0e-b0be-fcf26b9a57c7"),
    s_interact_NOTinvars=Input(rid="ri.foundry.main.dataset.7dc6d77a-edbf-4c00-834d-ac149c707e21"),
    s_interact_invars=Input(rid="ri.foundry.main.dataset.56725eae-2284-4cb3-a920-ac17601fcc18")
)
with comb as (SELECT 1 as invar, varname, i_rural,x,npats,rs2_m
                FROM s_interact_invars
            union
            SELECT 0 as invar, varname, i_rural, x, npats, rs2_m
                FROM s_interact_NOTinvars)
    , comb_collapse as (SELECT varname as cl_varname, x as  cl_x , sum(npats) as subtotal
                FROM s_interact_invars group by varname, x
            union
            SELECT varname as cl_varname, x as cl_x , sum(npats) as subtotal
                FROM s_interact_NOTinvars group by varname, x )
select  invar, varname, i_rural, x,  npats, subtotal, rs2_m
        , npats/subtotal as ppats , rs2_m*npats/subtotal as rs2_m_adj
from comb
    inner join comb_collapse
    on varname = cl_varname and x = cl_x
order by invar, varname, i_rural , x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e6ed064f-09a8-491e-b4b3-508c624dbf93"),
    s2_dp_cov_ip_rate=Input(rid="ri.vector.main.execute.7c311da2-6cdf-4937-8033-e323191775a9"),
    s2_r_rural=Input(rid="ri.vector.main.execute.2d8b74d7-eacf-430e-ad66-5d4ef6285ad5"),
    s_summary2_for_analysis=Input(rid="ri.foundry.main.dataset.e5fda101-be78-46cf-a144-b5190183655d")
)
SELECT distinct s1.*
    , ip_rate, rr.r_rural
FROM  s_summary2_for_analysis s1
    inner join s2_dp_cov_ip_rate ip1
        on s1.data_partner_id= ip1.data_partner_id
    inner join s2_r_rural rr
        on ip1.data_partner_id= rr.data_partner_id

/
@transform_pandas(
    Output(rid="ri.vector.main.execute.602ad173-fc4f-45be-932e-95ce0b030d37"),
    unnamed_15=Input(rid="ri.foundry.main.dataset.9f3a78c9-5e00-4877-bc61-b011f963feb1")
)
--select data_partner_id,  n_rural /npats as r_rural
--from (
    SELECT distinct data_partner_id, npats , avg(i_rural) as r_rural
    FROM unnamed_15
    group by data_partner_id, npats
--)a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3ea07984-9c9e-4365-92e5-efa838846c4b"),
    s1_pre_analysis=Input(rid="ri.foundry.main.dataset.64f9d685-6967-4af5-9656-9d3044b0493e")
)
SELECT in_death_table,cast(i_rural as integer) as i_rural,gender,age,Race,Ethnicity,BMI,BMI_Group,Q_cat,Q_Score,any_disease,quarter_dx,data_partner_id
FROM s1_pre_analysis

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e5fda101-be78-46cf-a144-b5190183655d"),
    closest_bmi_to_covid_diagnosis_from_observation=Input(rid="ri.foundry.main.dataset.5711686f-9d1e-4016-8e2e-e75b13e4b202"),
    s_inpatient_summary_for_analysis=Input(rid="ri.foundry.main.dataset.147442b8-a019-49c1-95e6-16d755ffa576"),
    s_manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f"),
    s_pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    s_pers_with_covid_peak_reference=Input(rid="ri.foundry.main.dataset.cef3672e-405f-473d-af51-913f341c2fcb")
)
SELECT p.person_id,ttdeath, four_category_ruca, three_category_ruca,
        CASE
            WHEN age_at_visit_start_in_years_int BETWEEN 0 AND 120 THEN age_at_visit_start_in_years_int
            ELSE null
        END AS age,
        CASE
            WHEN four_category_ruca = 'urban' THEN '0'
            ELSE '1'
        END AS i_rural,
       i.gender_concept_name AS gender,
       CASE
            WHEN age_at_visit_start_in_years_int < 18 THEN '0-17'
            WHEN age_at_visit_start_in_years_int BETWEEN 18 AND 29 THEN '18-29'
            WHEN age_at_visit_start_in_years_int BETWEEN 30 AND 39 THEN '30-39'
            WHEN age_at_visit_start_in_years_int BETWEEN 40 AND 49 THEN '40-49'
            WHEN age_at_visit_start_in_years_int BETWEEN 50 AND 59 THEN '50-59'
            WHEN age_at_visit_start_in_years_int BETWEEN 60 AND 69 THEN '60-69'
            WHEN age_at_visit_start_in_years_int BETWEEN 70 AND 79 THEN '70-79'
            WHEN age_at_visit_start_in_years_int BETWEEN 80 and 120 THEN '80-120'
            ELSE 'Unknown/Missing'
       END AS age_Group,
       CASE
            WHEN Race = 'Native Hawaiian or Other Pacific Islander' THEN 'Other'
            ELSE Race
        END AS Race,
       Ethnicity,
        CASE
            WHEN BMI BETWEEN 10 AND 18.5 THEN '<18.5'
            WHEN BMI >= 18.5 AND BMI < 25 THEN '18.5-24.9'
            WHEN BMI >= 25 AND BMI < 30 THEN '25-29.9'
            WHEN BMI BETWEEN 30 AND 75 THEN '>30'
            WHEN bmi.bmi_from_observation IN ('<18.5', '18.5-24.9', '25-29.9', '>30') THEN bmi.bmi_from_observation
            ELSE 'Unknown/Missing'
       END AS BMI_Group, BMI,
       CASE
              WHEN Q_Score < 1 THEN '<1.0'
              WHEN Q_Score BETWEEN 1 AND 2 THEN '1.0-2.0'
              WHEN Q_Score > 2 THEN '>2.0'
              ELSE 'Unknown/Missing'
        END AS Q_Score_Categories, Q_Score,
        CASE
            WHEN diabetes = 1 THEN 1
            WHEN dmcx = 1 THEN 1
            ELSE 0
        END AS Diabetes,
        MI, CHF, PVD, stroke, dementia, pulmonary, rheumatic,
        CASE
            WHEN liver_mild = 1 THEN 1
            WHEN liversevere = 1 THEN 1
            ELSE 0
        END AS Liver,
        paralysis, renal, cancer, mets, hiv, Severity_Type, rECMO AS ECMO, MACE, Mechnical_Ventilation, in_death_table, covid_peak, length_of_stay,
        CASE
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-01-01' AND '2020-03-31' THEN 'Q1 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-04-01' AND '2020-06-30' THEN 'Q2 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-07-01' AND '2020-09-30' THEN 'Q3 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2020-10-01' AND '2020-12-31' THEN 'Q4 2020'
            WHEN i.date_of_earliest_covid_diagnosis BETWEEN '2021-01-01' AND '2021-03-31' THEN 'Q1 2021'
            ELSE 'Unknown/Missing'
        END AS quarter_of_diagnosis,
p1.data_partner_id,
CAST(i.CCI_INDEX AS DOUBLE),
       CASE
              WHEN CCI_INDEX < 1 THEN '<1.0'
              WHEN CCI_INDEX BETWEEN 1 AND 2 THEN '1.0-2.0'
              WHEN CCI_INDEX > 2 THEN '>2.0'
        END AS CCI_Categories
FROM   s_inpatient_summary_for_analysis i
LEFT JOIN s_pers_with_covid_peak_reference p on p.person_id = i.person_id
JOIN s_pers p1 ON p1.person_id = i.person_id
JOIN s_manifest m on m.data_partner_id = p1.data_partner_id
LEFT JOIN (select  person_id, bmi_from_observation from closest_bmi_to_covid_diagnosis_from_observation ) bmi on p.person_id = bmi.person_id
WHERE i.date_of_earliest_covid_diagnosis <= '2021-03-31'

@transform_pandas(
    Output(rid="ri.vector.main.execute.34326a1b-1d19-4050-be65-b4eabd39d31a"),
    s1_pre_analysis=Input(rid="ri.foundry.main.dataset.64f9d685-6967-4af5-9656-9d3044b0493e")
)
SELECT distinct in_death_table
FROM s1_pre_analysis

@transform_pandas(
    Output(rid="ri.vector.main.execute.ce756a0e-bf36-4cea-a77d-c3f99e901fb4"),
    s_r_set1=Input(rid="ri.foundry.main.dataset.e6f4b601-1ad2-47d5-a169-9db890c57d74")
)
SELECT *
FROM s_r_set1
where p_value <.05

@transform_pandas(
    Output(rid="ri.vector.main.execute.7beee699-6818-4047-98b1-c699585fbbf9"),
    s_r_set2=Input(rid="ri.foundry.main.dataset.47dbb795-485f-4cee-a908-800cc5a72fd0")
)
SELECT *
FROM s_r_set2
where p_value<.05

@transform_pandas(
    Output(rid="ri.vector.main.execute.941a1df7-55d3-4895-be13-6bf9efbdd745"),
    s_r_set2_1=Input(rid="ri.foundry.main.dataset.d8e0aa68-5b3e-4c77-9129-f621d4b2a534")
)
SELECT *
FROM s_r_set2_1
where p_value<.05

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b6d7f323-96b4-44c0-95e5-1fbe3a72c440"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
SELECT *
FROM summary2_for_analysis
where

data_partner_id NOT IN [redacted]
AND data_partner_id NOT IN [redacted]

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9f3a78c9-5e00-4877-bc61-b011f963feb1"),
    s1_dp_cov_ip_rate=Input(rid="ri.foundry.main.dataset.eb2276ff-d83f-4e94-85bd-e8cea8b9b339"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
SELECT ip1.data_partner_id, ip1.i_rural, np.npats
FROM  summary2_for_analysis ip1
inner join s1_dp_cov_ip_rate np
on ip1.data_partner_id = np.data_partner_id

/* Sites 808 and 325 shift dates <= 90 days. Sites 399, 787, 664, 353, 134,
   and 294 have unreliable mortality information */
and ip1.data_partner_id NOT IN [redacted]
/*  Filter out additional sites with no BMI or BMI percentages under 0.10 percentage for all patients */
AND ip1.data_partner_id NOT IN [redacted]

@transform_pandas(
    Output(rid="ri.vector.main.execute.a6cfe7d7-06af-40c0-b1e7-0960201b0739"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
SELECT distinct  data_partner_id, sum(i_rural) as n_rural, avg(i_rural) as mean_rural
FROM unnamed_8
group by data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291"),
    s_ip_cov_summ2=Input(rid="ri.foundry.main.dataset.e6ed064f-09a8-491e-b4b3-508c624dbf93")
)
SELECT in_death_table, four_category_ruca,three_category_ruca,cast(i_rural as int) as i_rural
    , age,gender,age_Group,Race,Ethnicity,BMI_Group,quarter_of_diagnosis,CCI_INDEX
    , CCI_Categories,data_partner_id,covid_peak
    , BMI,Q_Score_Categories,Q_Score,Diabetes,MI,CHF,PVD,stroke,dementia,pulmonary,rheumatic,Liver,paralysis,renal,cancer,mets,hiv
    , ttdeath,Severity_Type,ECMO,MACE,Mechnical_Ventilation,length_of_stay
    , ip_rate, r_rural
FROM  s_ip_cov_summ2
where
    data_partner_id NOT IN [redacted]
AND data_partner_id NOT IN [redacted]

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b0e5bdf3-bfd9-4235-a1f8-b8f0df3ab537"),
    s_manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)
SELECT *
FROM s_manifest
where cdm_name ='PCORNET' and run_date > '2021-04-01'

--and cdm_version like '%6%'
/*729, 739, 793*/

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8187b4c4-b7d0-4a9a-b4ea-90df63fbe5ed"),
    s_pers=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66"),
    unnamed_18=Input(rid="ri.foundry.main.dataset.b0e5bdf3-bfd9-4235-a1f8-b8f0df3ab537")
)
select distinct data_partner_id, run_date, cdm_version count(*) as npats
from (SELECT distinct d.*, person_id
    FROM s_pers p
        join unnamed_18 d
        on p.data_partner_id = d.data_partner_id and cdm_version like '%6%'
    )a
group by data_partner_id, run_date, cdm_version

@transform_pandas(
    Output(rid="ri.vector.main.execute.c3aad9cf-7a66-486e-a0d5-59383bbb0c16"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
SELECT distinct  data_partner_id, ip_rate
FROM unnamed_8

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9214db27-bb67-4c30-9248-1c81d9387d1e"),
    care_site=Input(rid="ri.foundry.main.dataset.7d9c16b0-6f3f-40d3-ae68-b772eb03f22e")
)
SELECT *
FROM care_site
where data_partner_id in [redacted]

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.53734ea0-08e0-4e96-aaf4-266dcd10de87"),
    s2_md_i_rural_adj=Input(rid="ri.foundry.main.dataset.c051eb4b-f3c5-455b-9ffc-f9d9f6c04d2c"),
    s2_md_i_rural_dp_fe=Input(rid="ri.foundry.main.dataset.65e1e4f5-d12a-4412-84ed-ddc4dd8ce852"),
    s2_md_i_rural_dp_psc=Input(rid="ri.vector.main.execute.9afdb1ab-cb11-4519-b010-59399eac97c1"),
    s2_md_i_rural_dp_psc_drop_ukBMIcat=Input(rid="ri.foundry.main.dataset.0b179ae9-db1f-49a9-a3e7-f3178013099b"),
    s2_md_i_rural_dp_re=Input(rid="ri.foundry.main.dataset.6607434d-0510-4559-acee-d4034250793f"),
    s2_md_i_rural_dp_re_lmer=Input(rid="ri.foundry.main.dataset.d30878f7-dc49-479c-ba4a-0a932357858d"),
    s2_md_i_rural_dp_re_slopes=Input(rid="ri.foundry.main.dataset.fb64454f-65d9-4ad3-80ba-571fe687a292"),
    s2_md_i_rural_only=Input(rid="ri.foundry.main.dataset.44ac363d-91de-43f3-be0f-fb1e8c59d876")
)
SELECT '1. glm(in_death_table ~ i_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'i_rural'
union SELECT '2. glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis))' as md, label, estimate,ci,p_value FROM s2_md_i_rural_adj where label = 'i_rural'
union SELECT '3. glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + factor(data_partner_id))' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_fe where label = 'i_rural'
union SELECT '4. glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id))' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_re where label = 'i_rural'
union SELECT '5. glmer(formula = in_death_table ~ 1 + i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_re_lmer where label = 'i_rural'
union SELECT '6. glm(in_death_table ~ i_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + factor(age_Group) + CCI_INDEX  + factor(quarter_of_diagnosis) + (1|data_partner_id) + (0 + r_rural|data_partner_id))' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_re_slopes where label = 'i_rural'
union SELECT '7. s2_md_i_rural_dp_psc' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_psc where label = 'i_rural'
union SELECT '8. s2_md_i_rural_dp_psc_drop_ukBMIcat' as md, label, estimate,ci,p_value FROM s2_md_i_rural_dp_psc_drop_ukBMIcat --where label = 'i_rural'
--order by md



@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bb0c206e-7fec-4dd6-a36d-329b60e4573f"),
    s2_md_i_rural_set1=Input(rid="ri.foundry.main.dataset.205b617b-0aa0-489c-adf2-7274f8032969"),
    s2_md_i_rural_set1_intact_i_rural=Input(rid="ri.foundry.main.dataset.cbad3e16-51da-4366-9d5c-351af9beaada"),
    s2_md_i_rural_set2=Input(rid="ri.foundry.main.dataset.a4e83696-0314-41d6-afcf-2c88d182a422"),
    s2_md_i_rural_set3=Input(rid="ri.foundry.main.dataset.c081501f-2750-4b54-a237-584db81dd481")
)
SELECT 's2_md_i_rural_set1' as md_set, md,label, estimate,ci,p_value FROM s2_md_i_rural_set1 where label = 'i_rural'
union SELECT 's2_md_i_rural_set1_intact_i_rural' as md_set, md,label, estimate,ci,p_value FROM s2_md_i_rural_set1_intact_i_rural where label = 'i_rural'
union SELECT 's2_md_i_rural_set2' as md_set, md, label, estimate,ci,p_value FROM s2_md_i_rural_set2 where label = 'i_rural'
union SELECT 's2_md_i_rural_set3' as md_set, md, label, estimate,ci,p_value FROM s2_md_i_rural_set3 where label = 'i_rural'

@transform_pandas(
    Output(rid="ri.vector.main.execute.9eb9fed3-a19d-45ec-b659-becc1451cf2c"),
    s2_dt_r_rural_rural=Input(rid="ri.foundry.main.dataset.3035f7ef-e9ec-439a-9519-6dcee27977b5")
)
SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '12. glm(in_death_table ~ r_rural+ factor(gender) + factor(Race) + factor(Ethnicity) + factor(BMI_Group) + age + CCI_INDEX  + factor(quarter_of_diagnosis)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '13. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'
union SELECT '11. glm(in_death_table ~ r_rural)' as md, label, estimate,ci,p_value FROM s2_md_i_rural_only where label = 'r_rural'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3a5b5387-1de7-44f1-bcd4-835182e2e51f"),
    unnamed_17=Input(rid="ri.foundry.main.dataset.e7d68db1-0cf9-46b3-86cc-5f485ed7d291")
)
SELECT distinct data_partner_id
FROM unnamed_17

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a"),
    s1_dp_cov_ip_rate=Input(rid="ri.foundry.main.dataset.eb2276ff-d83f-4e94-85bd-e8cea8b9b339"),
    s_r_rural=Input(rid="ri.vector.main.execute.602ad173-fc4f-45be-932e-95ce0b030d37"),
    summary2_for_analysis=Input(rid="ri.foundry.main.dataset.5bf96de6-ef2a-43da-87da-729188343565")
)
SELECT s1.*, ip_rate, rr.r_rural
FROM  summary2_for_analysis s1
    inner join s1_dp_cov_ip_rate ip1
        on s1.data_partner_id= ip1.data_partner_id
    inner join s_r_rural rr
        on ip1.data_partner_id= rr.data_partner_id


AND ip1.data_partner_id NOT IN [redacted]
AND ip1.data_partner_id NOT IN [redacted]

@transform_pandas(
    Output(rid="ri.vector.main.execute.e8576487-b8c4-4b37-a613-d82b1da52393"),
    unnamed_8=Input(rid="ri.foundry.main.dataset.6269482e-6137-4c64-97bc-9264db84b63a")
)
SELECT  ttdeath , four_category_ruca , three_category_ruca , age , i_rural , gender , age_Group , Race , Ethnicity , BMI_Group , BMI , Q_Score_Categories , Q_Score , Diabetes , MI , CHF , PVD , stroke , dementia , pulmonary , rheumatic , Liver , paralysis , renal , cancer , mets , hiv , Severity_Type , ECMO , MACE , Mechnical_Ventilation , in_death_table , covid_peak , length_of_stay , quarter_of_diagnosis , data_partner_id , CCI_INDEX , CCI_Categories , ip_rate
FROM unnamed_8
