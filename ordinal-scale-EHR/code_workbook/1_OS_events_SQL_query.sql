/* 
   Author: Maryam Khodaverdi 
   Date: 05/21
   Desc: get all OS events within 28 days of covid dx
*/

with covid_positive_with_index as (

	select p.*
	from Covid_positive_persons p
	where !(covid_event_type = 'Antibody Test' AND YEAR(date_of_earliest_covid_diagnosis) < 2021 )

), Covid_pos_hospitalization as (
	
	select distinct
    		f.person_id
    		, f.date_of_earliest_covid_diagnosis 
    		, s.visit_start_date as hosp_start_date
    		, case when s.visit_end_date < visit_start_date then visit_start_date
        	   when s.visit_end_date is null then visit_start_date
        	   else s.visit_end_date
    		 end as hosp_end_date
	from visit_occurrence s
    		inner join covid_positive_with_index f     ON f.person_id=s.person_id
	where 1=1
	--string search 
    	--and (lower(visit_concept_name) like '%inpatient%'
    	--or lower(visit_concept_name) like '%intensive%care%')
    	
	and visit_concept_id in ('8920','8717','262','581379','9201', '32037') 
    	and (datediff(visit_start_date , date_of_earliest_covid_diagnosis) between 0 and 28
             or datediff(visit_end_date , date_of_earliest_covid_diagnosis) between 0 and 28)

), Covid_pos_oxygen as (

    select *
    from (
    	select distinct 
        	pts.person_id
        	, pro.procedure_occurrence_id as oxy_occurrence_id
        	, pts.date_of_earliest_covid_diagnosis
        	, pro.procedure_concept_name as oxy_concept_name
        	, pro.procedure_date as oxy_start_date
        	, pro.procedure_date as oxy_end_date
    	from  covid_positive_with_index pts               
        	inner join procedure_occurrence pro      ON pts.person_id=pro.person_id
    	where 1=1
		-- string search
        	--and (lower(procedure_concept_name) like 'oxygen%therapy%'
        	--or lower(procedure_concept_name) like '%oxygen%administration%nasal%cannula%'
        	--or lower(procedure_concept_name) like '%continuous%airway%ventilation%cpap%initiation%'
        	--or lower(procedure_concept_name) like '%continuous%negative%pressure%ventilation%cnp%initiation%'
        	--or lower(procedure_concept_name) like '%assistance%respiratory%ventilation%'
        	--or lower(procedure_concept_name) like '%respiratory%ventilation%single%nonmechanical%'
        	--or lower(procedure_concept_name) like '%removal%tracheostomy%device%via%natural%artificial%endoscopic%' )
        
		and procedure_concept_id in (1781162, 2788020, 2788017, 2788023, 1781160, 1781161, 
				2788027, 2788028, 2788026, 2787824, 4239130, 2788022, 2788021, 
				2314036, 2743417, 2788019, 2787823, 2314035, 2788025, 2788016, 
				2788018, 2788035, 2788024, 4155151, 2788035)
        	and datediff(procedure_date , date_of_earliest_covid_diagnosis) between 0 and 28
	) union 
	select *
	from (
    		select distinct 
        		pts.person_id
        		, med.drug_exposure_id as oxy_occurrence_id
        		, pts.date_of_earliest_covid_diagnosis
        		, med.drug_concept_name as oxy_concept_name
        		, med.drug_exposure_start_date as oxy_start_date
        		, case when med.drug_exposure_end_date < drug_exposure_start_date then drug_exposure_start_date
            		   when med.drug_exposure_end_date is null then drug_exposure_start_date
            		   else med.drug_exposure_end_date
        		 end as oxy_end_date
    		from covid_positive_with_index pts               
        		inner join drug_exposure med    ON pts.person_id=med.person_id
    		where 1=1
        		--and lower(drug_concept_name) like '%oxygen%'
        		and drug_concept_id in (19025274)
        		and (datediff(drug_exposure_start_date , date_of_earliest_covid_diagnosis) between 0 and 28
            		     or datediff(drug_exposure_end_date , date_of_earliest_covid_diagnosis) between 0 and 28)
	) union 
	select *
	from (
    		select distinct 
        		pts.person_id
        		, mea.measurement_id as oxy_occurrence_id
        		, pts.date_of_earliest_covid_diagnosis
        		, mea.measurement_concept_name as oxy_concept_name
        		, mea.measurement_date as oxy_start_date
        			, mea.measurement_date as oxy_end_date
    		from covid_positive_with_index pts               
        		inner join measurement mea    ON pts.person_id=mea.person_id
    		where 1=1
			--string search
        		--and (lower(measurement_concept_name) like '%inhaled%oxygen%concentration%' 
        		--or lower(measurement_concept_name) like '%inhaled%oxygen%flow%rate%')
        		and measurement_concept_id in (3005629, 3020716, 4141684, 3574723, 3574685, 3574702, 
							3438660, 3014080)
        		and datediff(measurement_date , date_of_earliest_covid_diagnosis) between 0 and 28
	) union 
	select *
	from (
    		select distinct 
        		pts.person_id
        		, dev.device_exposure_id as oxy_occurrence_id
        		, pts.date_of_earliest_covid_diagnosis
        		, dev.device_concept_name as oxy_concept_name
        		, dev.device_exposure_start_date as oxy_start_date
        		, dev.device_exposure_start_date as oxy_end_date
    		from covid_positive_with_index pts               
        		inner join device_exposure dev    ON pts.person_id=dev.person_id
    		where 1=1
			-- string search
        		--and (lower(concept_name) like 'cannula%nasal'
        		--or lower(concept_name) like '%concentration mask%')
        		and device_concept_id in (2614925, 2614930)
        		and datediff(device_exposure_start_date , date_of_earliest_covid_diagnosis) between 0 and 28
	) union 
	select *
	from (
    		select distinct 
        		pts.person_id
        		, obs.observation_id as oxy_occurrence_id
        		, pts.date_of_earliest_covid_diagnosis
        		, obs.observation_concept_name as oxy_concept_name
        		, obs.observation_date as oxy_start_date
        		, obs.observation_date as oxy_end_date
    		from covid_positive_with_index pts               
        		inner join observation obs    ON pts.person_id=obs.person_id
    		where 1=1
			--string search
        		--and (lower(concept_name) like 'delivered oxygen%'
        		--or lower(concept_name) like'oxygen gas%delivery%'
        		--or lower(concept_name) like 'oxygen delivery%')
        		and observation_concept_id in (1004397, 3327346)
        		and datediff(observation_date , date_of_earliest_covid_diagnosis) between 0 and 28
	)

), Covid_pos_ventilator as (
	
	select distinct 
    		pts.person_id
    		, pro.procedure_occurrence_id as vent_occurrence_id
    		, pts.date_of_earliest_covid_diagnosis
    		, pro.procedure_concept_name as vent_concept_name
    		, pro.procedure_date as vent_start_date
    		, pro.procedure_concept_id
	from  covid_positive_with_index pts               
    		inner join procedure_occurrence pro      ON pts.person_id=pro.person_id
	where 1=1
		--string search
    		--and (lower(procedure_concept_name) like '%transtracheal%introduction%needle%wire%dilator%tube%oxygen%therapy%'
    		--    or lower(procedure_concept_name) like '%insertion%endotracheal%airway%trachea%'
    		--    or lower(procedure_concept_name) like '%change%endotracheal%airway%trachea%external%approach%'
    		--    or lower(procedure_concept_name) like '%insertion%airway%artificial%opening%'
    		--    or lower(procedure_concept_name) like '%artificial respiration%'
    		--    or lower(procedure_concept_name) like 'respiratory%ventilation%'
    		--    or lower(procedure_concept_name) like '%initiation%pressure%preset%ventilators%breathing%'
    		--    or lower(procedure_concept_name) like '%provision%mechanical%ventilator%'
    		--    or lower(procedure_concept_name) like '%high%frequency%positive%pressure%ventilation%'
    		--    or lower(procedure_concept_name) like 'intubation%endotracheal%'
    		--    or lower(procedure_concept_name) like 'intubation'
    		--    or lower(procedure_concept_name) like '%tracheostomy%')
    		--and lower(procedure_concept_name) not like 'glossectomy%tracheostomy%'
    		--and lower(procedure_concept_name) not like 'surgical%tracheostomy%'
    		--and lower(procedure_concept_name) not like 'removal%tracheostomy%' 
    		--and lower(procedure_concept_name) not like '%tracheostomy incision'
    		
		and procedure_concept_id in (2788038, 2745444, 2106469, 2314001, 2314000, 4230167, 
					2788037, 2745447, 2788036, 2106562, 2741580, 2741578, 
					2741589, 2741588, 44808555, 4202832, 2788035, 2106567, 
					2106642, 2741582, 2106564, 4337047, 2745483 , 4336916, 
					2745515, 2748632, 2106565, 2106563, 2741675, 2748633, 
					4331311, 2106667, 2745491, 4143728, 2745507, 2748582, 
					4055376, 2742789, 2745440, 4065590, 2742790, 2745499, 
					2314002, 42738853, 42738852, 45887795, 765576, 4080957, 
					40487536, 37116689)
    		and datediff(procedure_date , date_of_earliest_covid_diagnosis) between 0 and 28
	
	union
 
	select distinct 
    		pts.person_id
    		, con.condition_occurrence_id as vent_occurrence_id
    		, pts.date_of_earliest_covid_diagnosis
    		, con.condition_concept_name as vent_concept_name
    		, con.condition_start_date as vent_start_date
    		, con.condition_concept_id as procedure_concept_id
	from covid_positive_with_index pts               
    		inner join condition_occurrence con    ON pts.person_id=con.person_id
	where 1=1
    		and condition_concept_id in (46273390, 4325601, 40481547, 45552902, 4031379, 45566484, 
					45567282, 45542507, 35208098, 45596288, 45552148, 1576312)
    		and datediff(condition_start_date , date_of_earliest_covid_diagnosis) between 0 and 28

 	
	union 

	select distinct 
    		pts.person_id
    		, obs.observation_id as vent_occurrence_id
    		, pts.date_of_earliest_covid_diagnosis
    		, obs.observation_concept_name as vent_concept_name
    		, obs.observation_date as vent_start_date
    		, obs.observation_concept_id as procedure_concept_id
	from covid_positive_with_index pts               
    		inner join observation obs    ON pts.person_id=obs.person_id
	where 1=1
		--string search
    		--and observation_concept_name like 'Endotracheal tube present'
    		and observation_concept_id in (4168966, 4353715, 3007397, 38001053, 38001054, 2108680, 
						4108138)
    		and datediff(observation_date , date_of_earliest_covid_diagnosis) between 0 and 28

), Covid_pos_ecmo as (

	select distinct 
    		pts.person_id
    		, pro.procedure_occurrence_id as ecmo_occurrence_id
    		, pts.date_of_earliest_covid_diagnosis
    		, pro.procedure_concept_name as ecmo_concept_name
    		, pro.procedure_concept_id as ecmo_concept_id
    		, pro.procedure_date as ecmo_start_date
    		, pro.procedure_date as ecmo_end_date
    
	from  covid_positive_with_index pts               
    		inner join procedure_occurrence pro      ON pts.person_id=pro.person_id
	where 1=1
		--string search
    		--and (lower(procedure_concept_name) like 'extracorporeal%oxygenation%membrane%'
    		--or lower(procedure_concept_name) like 'extracorporeal%supersaturated%oxygenation%'
    		--or lower(procedure_concept_name) like 'extracorporeal%systemic assistance%')

    		and procedure_concept_id in (46257397,46257685,46257467,46257513,46257680,46257512,
					46257466,46257439,46257468,46257398,42739551,46257684,
					46257544,46257682,46257683,46257730,46257469,46257399,
					46257400,46257729,46257585,46257543,46257510,46257441,
					46257440,46257438,46257511,2108293,4197460,44811012,
					40379806,3525733,2788034,2002247,3118570,3396547,
					3245375,44515635,37206601,37206602,37206603,4338595,
					4052536,1531630,1531631,1531632,2787820,2787821,
					46257586,2893766,2834015,2813710,2800859,2867784,
					2805870)
    		and datediff(procedure_date , date_of_earliest_covid_diagnosis) between 0 and 28
	
	union
 
	select distinct 
    		pts.person_id
    		, mea.drug_concept_id as ecmo_occurrence_id
    		, pts.date_of_earliest_covid_diagnosis
    		, mea.drug_concept_name as ecmo_concept_name
    		, mea.drug_concept_id as ecmo_concept_id
    		, mea.drug_exposure_start_date as ecmo_start_date
    		, case when mea.drug_exposure_end_date < mea.drug_exposure_start_date then mea.drug_exposure_start_date
        	    when mea.drug_exposure_end_date is null then mea.drug_exposure_start_date
          	    else mea.drug_exposure_end_date
    		 end as ecmo_end_date
	from covid_positive_with_index pts               
    		inner join drug_exposure mea    on pts.person_id=mea.person_id
    		inner join Meds_concept con     on con.drug_concept_id=mea.drug_concept_id  --file OS_ECMO_JHarper_MKhodaverdi.csv uploaded
	where 
		datediff(drug_exposure_start_date , date_of_earliest_covid_diagnosis) between 0 and 28
    		or datediff(drug_exposure_end_date , date_of_earliest_covid_diagnosis) between 0 and 28

)

select distinct
	pts.person_id
        , pts.date_of_earliest_covid_diagnosis
	, hosp_start_date
	, hosp_end_date
	, oxy_start_date
	, oxy_end_date
	, ecmo_start_date
	, ecmo_end_date 
	, death_date

from covid_positive_with_index pts 
	left join death dtb                        on pts.person_id= dtb.person_id
        left join Covid_pos_hospitalization hosp   on pts.person_id= hosp.person_id
        left join Covid_pos_oxygen oxy             on pts.person_id= oxy.person_id
        left join Covid_pos_ventilator vent        on pts.person_id= vent.person_id
        left join Covid_pos_ecmo ecmo              on pts.person_id= ecmo.person_id


