SELECT
  --TOP (1000)
  [concept_id]
  ,[concept_name]
  ,[domain_id]
  ,[vocabulary_id]
  ,[concept_class_id]
  ,[standard_concept]
  ,[concept_code]
  ,[valid_start_date]
  ,[valid_end_date]
  ,[invalid_reason]
FROM [cdw_omop_1].[v6].[concept]
WHERE
  domain_id = 'Drug'
  and
  vocabulary_id = 'RxNorm'
  and
  invalid_reason is null
  -- and
  -- standard_concept = 'S'
  and
  (
    concept_name like '%prednisone%' or
    concept_name like '%prednisolone%' or -- 741 vs 646
    concept_name like '%dexamethasone%' or -- 565 vs 26
    concept_name like '%hydrocortisone%' or  -- 1,675 vs 650 (in oral) & 398 (in systemic)

    -- concept_name like '%acetate%' or
    -- concept_name like '%acetonide%' or
    -- concept_name like '%acid%' or
    -- concept_name like '%acis%' or
    -- concept_name like '%actuat%' or
    concept_name like '%advair%' or --include
    concept_name like '%airduo%' or --include
    -- concept_name like '%alfason%' or
    concept_name like '%aricin%' or --include
    concept_name like '%aristocort%' or --include
    concept_name like '%armonair%' or --include
    concept_name like '%arnuity%' or --include
    concept_name like '%asmanex%' or --include
    -- concept_name like '%astellas%' or
    concept_name like '%beclomethasone%' or --include
    -- concept_name like '%box%' or
    concept_name like '%budesonide%' or --include
    -- concept_name like '%chloramphenicol%' or
    -- concept_name like '%chloroxylenol%' or
    concept_name like '%ciclesonide%' or --include
    -- concept_name like '%clioquinol%' or
    concept_name like '%clotrimazole%' or --include
    concept_name like '%cortef%' or --include
    -- concept_name like '%cream%' or
    -- concept_name like '%crelo%' or
    concept_name like '%deltasone%' or --include
    concept_name like '%depo-medrol%' or --include
    concept_name like '%dexamethasone%' or --include
    -- concept_name like '%diacetate%' or
    concept_name like '%dipropionate%' or --include
    concept_name like '%dulera%' or --include
    concept_name like '%flovent%' or --include
    concept_name like '%fluticasone%' or --include
    -- concept_name like '%formoterol%' or
    -- concept_name like '%forte%' or
    -- concept_name like '%fumarate%' or
    concept_name like '%furoate%' or --include
    -- concept_name like '%gentamicin%' or
    concept_name like '%hexacetonide%' or --include
    -- concept_name like '%hydrochloride%' or
    concept_name like '%hydrocortison%' or --include
    concept_name like '%hydrocortisone%' or --include
    concept_name like '%hydrocortone%' or --include
    -- concept_name like '%hydroson%' or
    concept_name like '%hydventia%' or --include
    concept_name like '%hysone%' or --include
    concept_name like '%kenalog%' or --include
    -- concept_name like '%labs%' or
    concept_name like '%ledercort%' or --include
    -- concept_name like '%lidocaine%' or
    concept_name like '%locoid%' or --include
    concept_name like '%medrol%' or --include
    concept_name like '%meprednisone%' or --include
    concept_name like '%methylprednisolone%' or --include
    concept_name like '%millipred%' or --include
    concept_name like '%mometasone%' or --include
    -- concept_name like '%nasal%' or
    concept_name like '%orapred%' or --include
    concept_name like '%orasone%' or --include
    -- concept_name like '%phoenix%' or
    -- concept_name like '%phosphate%' or
    concept_name like '%plenadren%' or --include
    concept_name like '% pred %' or --include  --keep the surrounding spaces
    concept_name like '%prednisolone%' or --include
    concept_name like '%prednisone%' or --include
    concept_name like '%predsol%' or --include
    -- concept_name like '%propionate%' or
    concept_name like '%pulmicort%' or --include
    -- concept_name like '%salmeterol%' or
    -- concept_name like '%sodium%' or
    concept_name like '%solu-medrol%' or --include
    concept_name like '%sterapred%' or --include
    -- concept_name like '%systral%' or
    concept_name like '%thiostrepton%' or
    concept_name like '%triamcinolone%' or --include
    concept_name like '%triderm%' or --include
    -- concept_name like '%umeclidinium%' or
    -- concept_name like '%vetalog%' or
    -- concept_name like '%vilanterol%' or
    concept_name like '%wixela%' --include

  )
ORDER BY concept_name
