

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cec65557-1d6c-4680-8751-4e9445eb73e0"),
    covid_lab_records=Input(rid="ri.foundry.main.dataset.dfed4165-b1f4-4df1-989c-265b37538e8a"),
    positive_value_concepts=Input(rid="ri.foundry.main.dataset.f0507b1f-6040-490b-9773-e76eafc08dd3")
)
def Covid_positive_lab_records(covid_lab_records, positive_value_concepts):

    domain_col = "value_as_concept_id"

    df = covid_lab_records.join(
        positive_value_concepts.select("concept_id"), 
        covid_lab_records[domain_col] == positive_value_concepts["concept_id"],
        how="inner"
    ).drop(positive_value_concepts.concept_id)
    
    return df

#################################################
## Global imports and functions included below ##
#################################################

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cf76a45e-c5bd-4839-84c3-c880b9d16633"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
def covid_19_negative_patients(measurement):

    df_measurement = measurement

    covid_measurement_concept_ids = [

        '586524', '723478', '706157', '706155', '715262',
        '723477', '36661378', '586523', '715272', '723470',
        '706166', '706173', '586525', '706156', '706154',
        '715261', '36661371', '706167', '723463', '723469',
        '706168', '706159', '36661370', '723471', '723468',
        '706172', '723467', '723464', '586528', '586529',
        '706160', '706174', '723465', '706161', '706165',
        '723472', '706171', '723466', '706175', '586518',
        '757685', '706163', '706170', '715260', '586519', 
        '586520', '586516', '723476', '706158', '706169',
        '586526', '757678', '36661377', '757677', '37310257',
        '706180', '706181', '586521', '36661372', '706178',
        '36661374', '706177', '723479', '36661373', '723459',
        '706176', '757686', '723473', '723474', '723475', 
        '586522', '37310258', '757680', '586515', '757679',
        '706179', '36659631', '586527', '723480',
        '757680', '757679', '757678', '757677', '723459', '715262', '715261', '715260', '706181',
        '706180', '706179', '706178', '706177', '706176', '706175', '706174', '706173', '706172',
        '706171', '706170', '706169', '706168', '706167', '706166', '706165', '706163', '706161',
        '706160', '706159', '706158', '706157', '706156', '706155', '706154', '586526', '586523',
        '586522', '586521', '586520', '586519', '586518', '586517', '586516', '586515'

    ]

    covid_measurement_value_as_concept_ids = ['45878583', '45880296', '9189', '9190']

    persons_with_no_covid_measurement = df_measurement.where(

        (df_measurement.measurement_concept_id.isin(covid_measurement_concept_ids))

        & (df_measurement.value_as_concept_id.isin(covid_measurement_value_as_concept_ids))

    ).selectExpr("person_id", "data_partner_id", "measurement_date" , "measurement_concept_id", "measurement_concept_name as concept" , "measurement_source_value" , "value_as_concept_id" , "value_source_value" , "value_as_concept_name" ).distinct().withColumn("covid_diagnosis", F.lit(1))

    return persons_with_no_covid_measurement

#################################################
## Global imports and functions included below ##
#################################################
from pyspark.sql import functions as F
import pandas as pd
import numpy as np
import math
import statistics
import scipy.stats

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8f3aa58b-b21e-4ca2-afad-50d66e62b736"),
    concept_relationship=Input(rid="ri.foundry.main.dataset.0469a283-692e-4654-bb2e-26922aff9d71"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
def covid_antibody_concepts(concept_set_members, concept_relationship):

    concept_set_name = "Atlas #818 [N3C] CovidAntibody retry"
    # Treat empty string as null
    if len(concept_set_name) == 0:
        concept_set_name = None

    concept_set_id = ""
    # Treat empty string as null
    if len(concept_set_id) == 0:
        concept_set_id = None

    use_most_recent_version = True
    version = ""
    # Treat empty string as null
    if len(version) == 0:
        version = None

    if (concept_set_name and concept_set_id):
        raise ValueError("Only one of 'concept_set_name' or 'concept_set_id' should be filled in")
    elif (concept_set_name is None and concept_set_id is None):
        raise ValueError("Please enter a value for one of 'concept_set_name' or 'concept_set_id'")
    elif concept_set_name:
        df = concept_set_members.filter(concept_set_members.concept_set_name == concept_set_name)
    elif concept_set_id:
        df = concept_set_members.filter(concept_set_members.codeset_id == concept_set_id)
    else:
        raise ValueError("One of concept_set_name or concept_set_id must be filled in")

    if (version is not None and use_most_recent_version):
        raise ValueError("Only one of 'version' or 'use most recent version' should be selected")
    elif (concept_set_id is None and version is None and not use_most_recent_version):
        raise ValueError("Please enter a version or set 'use most recent version' to True")
    if (concept_set_id is not None and (use_most_recent_version or version)):
        raise ValueError("If entering a concept_set_id you should not specify a version or select 'use most recent version'")
    elif version:
        df = df.filter(df.version == int(version))
    elif use_most_recent_version:
        df = df.filter(df.is_most_recent_version == True)

    # Use the concept relationship table to look up the standard codes that the 
    # concepts in the original Concept Set map to
    df = df.join(
        concept_relationship.select("concept_id_1", "concept_id_2", "relationship_id"), 
        (df.concept_id == concept_relationship.concept_id_1) & 
        (concept_relationship.relationship_id == "Maps to"),
        how="inner"
    ).drop(concept_relationship.concept_id_1).drop(concept_relationship.relationship_id)
    

    df =  df.withColumnRenamed("concept_id", "original_concept_id") \
            .withColumnRenamed("concept_id_2", "concept_id")

    return df

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.11b940c1-4006-40cc-b0d5-5c48c2738fca"),
    concept_relationship=Input(rid="ri.foundry.main.dataset.0469a283-692e-4654-bb2e-26922aff9d71"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
def covid_diagnosis_concepts(concept_set_members, concept_relationship):

    concept_set_name = "N3C Covid Diagnosis"
    # Treat empty string as null
    if len(concept_set_name) == 0:
        concept_set_name = None

    concept_set_id = ""
    # Treat empty string as null
    if len(concept_set_id) == 0:
        concept_set_id = None

    use_most_recent_version = True
    version = ""
    # Treat empty string as null
    if len(version) == 0:
        version = None

    if (concept_set_name and concept_set_id):
        raise ValueError("Only one of 'concept_set_name' or 'concept_set_id' should be filled in")
    elif (concept_set_name is None and concept_set_id is None):
        raise ValueError("Please enter a value for one of 'concept_set_name' or 'concept_set_id'")
    elif concept_set_name:
        df = concept_set_members.filter(concept_set_members.concept_set_name == concept_set_name)
    elif concept_set_id:
        df = concept_set_members.filter(concept_set_members.codeset_id == concept_set_id)
    else:
        raise ValueError("One of concept_set_name or concept_set_id must be filled in")

    if (version is not None and use_most_recent_version):
        raise ValueError("Only one of 'version' or 'use most recent version' should be selected")
    elif (concept_set_id is None and version is None and not use_most_recent_version):
        raise ValueError("Please enter a version or set 'use most recent version' to True")
    if (concept_set_id is not None and (use_most_recent_version or version)):
        raise ValueError("If entering a concept_set_id you should not specify a version or select 'use most recent version'")
    elif version:
        df = df.filter(df.version == int(version))
    elif use_most_recent_version:
        df = df.filter(df.is_most_recent_version == True)

    # Use the concept relationship table to look up the standard codes that the 
    # concepts in the original Concept Set map to
    df = df.join(
        concept_relationship.select("concept_id_1", "concept_id_2", "relationship_id"), 
        (df.concept_id == concept_relationship.concept_id_1) & 
        (concept_relationship.relationship_id == "Maps to"),
        how="inner"
    ).drop(concept_relationship.concept_id_1).drop(concept_relationship.relationship_id)
    

    df =  df.withColumnRenamed("concept_id", "original_concept_id") \
            .withColumnRenamed("concept_id_2", "concept_id")

    return df

    

#################################################
## Global imports and functions included below ##
#################################################

#################################################
## Global imports and functions included below ##
#################################################

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.71d5711a-3e54-4556-914e-a740f0857dce"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86"),
    covid_diagnosis_concepts=Input(rid="ri.foundry.main.dataset.11b940c1-4006-40cc-b0d5-5c48c2738fca")
)
def covid_diagnosis_records(condition_occurrence, covid_diagnosis_concepts):

    domain_col = "condition_concept_id"

    df = condition_occurrence.join(
        covid_diagnosis_concepts.select("concept_id"), 
        condition_occurrence[domain_col] == covid_diagnosis_concepts["concept_id"],
        how="inner"
    ).drop(covid_diagnosis_concepts.concept_id)
    
    return df

#################################################
## Global imports and functions included below ##
#################################################

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dfed4165-b1f4-4df1-989c-265b37538e8a"),
    covid_tests_concepts=Input(rid="ri.vector.main.execute.4422a39a-d930-46ca-831b-5a3e727805ac"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
def covid_lab_records(measurement, covid_tests_concepts):

    domain_col = "measurement_concept_id"

    df = measurement.join(
        covid_tests_concepts.select("concept_id"), 
        measurement[domain_col] == covid_tests_concepts["concept_id"],
        how="inner"
    ).drop(covid_tests_concepts.concept_id)
    
    return df

#################################################
## Global imports and functions included below ##
#################################################

@transform_pandas(
    Output(rid="ri.vector.main.execute.4422a39a-d930-46ca-831b-5a3e727805ac"),
    covid_antibody_concepts=Input(rid="ri.foundry.main.dataset.8f3aa58b-b21e-4ca2-afad-50d66e62b736"),
    pcr_ag_measurement_concepts=Input(rid="ri.foundry.main.dataset.2e8b1b56-9dd5-4efb-bc9c-8fedc5d7f560")
)
def covid_tests_concepts(pcr_ag_measurement_concepts, covid_antibody_concepts):
    return pcr_ag_measurement_concepts.union(covid_antibody_concepts)
    

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2e8b1b56-9dd5-4efb-bc9c-8fedc5d7f560"),
    concept_relationship=Input(rid="ri.foundry.main.dataset.0469a283-692e-4654-bb2e-26922aff9d71"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
def pcr_ag_measurement_concepts(concept_set_members, concept_relationship):

    concept_set_name = "ATLAS SARS-CoV-2 rt-PCR and AG"
    # Treat empty string as null
    if len(concept_set_name) == 0:
        concept_set_name = None

    concept_set_id = ""
    # Treat empty string as null
    if len(concept_set_id) == 0:
        concept_set_id = None

    use_most_recent_version = True
    version = ""
    # Treat empty string as null
    if len(version) == 0:
        version = None

    if (concept_set_name and concept_set_id):
        raise ValueError("Only one of 'concept_set_name' or 'concept_set_id' should be filled in")
    elif (concept_set_name is None and concept_set_id is None):
        raise ValueError("Please enter a value for one of 'concept_set_name' or 'concept_set_id'")
    elif concept_set_name:
        df = concept_set_members.filter(concept_set_members.concept_set_name == concept_set_name)
    elif concept_set_id:
        df = concept_set_members.filter(concept_set_members.codeset_id == concept_set_id)
    else:
        raise ValueError("One of concept_set_name or concept_set_id must be filled in")

    if (version is not None and use_most_recent_version):
        raise ValueError("Only one of 'version' or 'use most recent version' should be selected")
    elif (concept_set_id is None and version is None and not use_most_recent_version):
        raise ValueError("Please enter a version or set 'use most recent version' to True")
    if (concept_set_id is not None and (use_most_recent_version or version)):
        raise ValueError("If entering a concept_set_id you should not specify a version or select 'use most recent version'")
    elif version:
        df = df.filter(df.version == int(version))
    elif use_most_recent_version:
        df = df.filter(df.is_most_recent_version == True)

    # Use the concept relationship table to look up the standard codes that the 
    # concepts in the original Concept Set map to
    df = df.join(
        concept_relationship.select("concept_id_1", "concept_id_2", "relationship_id"), 
        (df.concept_id == concept_relationship.concept_id_1) & 
        (concept_relationship.relationship_id == "Maps to"),
        how="inner"
    ).drop(concept_relationship.concept_id_1).drop(concept_relationship.relationship_id)
    

    df =  df.withColumnRenamed("concept_id", "original_concept_id") \
            .withColumnRenamed("concept_id_2", "concept_id")

    return df

    

#################################################
## Global imports and functions included below ##
#################################################

#################################################
## Global imports and functions included below ##
#################################################

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f0507b1f-6040-490b-9773-e76eafc08dd3"),
    concept_relationship=Input(rid="ri.foundry.main.dataset.0469a283-692e-4654-bb2e-26922aff9d71"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
def positive_value_concepts(concept_set_members, concept_relationship):

    concept_set_name = "ResultPos"
    # Treat empty string as null
    if len(concept_set_name) == 0:
        concept_set_name = None

    concept_set_id = ""
    # Treat empty string as null
    if len(concept_set_id) == 0:
        concept_set_id = None

    use_most_recent_version = True
    version = ""
    # Treat empty string as null
    if len(version) == 0:
        version = None

    if (concept_set_name and concept_set_id):
        raise ValueError("Only one of 'concept_set_name' or 'concept_set_id' should be filled in")
    elif (concept_set_name is None and concept_set_id is None):
        raise ValueError("Please enter a value for one of 'concept_set_name' or 'concept_set_id'")
    elif concept_set_name:
        df = concept_set_members.filter(concept_set_members.concept_set_name == concept_set_name)
    elif concept_set_id:
        df = concept_set_members.filter(concept_set_members.codeset_id == concept_set_id)
    else:
        raise ValueError("One of concept_set_name or concept_set_id must be filled in")

    if (version is not None and use_most_recent_version):
        raise ValueError("Only one of 'version' or 'use most recent version' should be selected")
    elif (concept_set_id is None and version is None and not use_most_recent_version):
        raise ValueError("Please enter a version or set 'use most recent version' to True")
    if (concept_set_id is not None and (use_most_recent_version or version)):
        raise ValueError("If entering a concept_set_id you should not specify a version or select 'use most recent version'")
    elif version:
        df = df.filter(df.version == int(version))
    elif use_most_recent_version:
        df = df.filter(df.is_most_recent_version == True)

    # Use the concept relationship table to look up the standard codes that the 
    # concepts in the original Concept Set map to
    df = df.join(
        concept_relationship.select("concept_id_1", "concept_id_2", "relationship_id"), 
        (df.concept_id == concept_relationship.concept_id_1) & 
        (concept_relationship.relationship_id == "Maps to"),
        how="inner"
    ).drop(concept_relationship.concept_id_1).drop(concept_relationship.relationship_id)
    

    df =  df.withColumnRenamed("concept_id", "original_concept_id") \
            .withColumnRenamed("concept_id_2", "concept_id")

    return df

    

#################################################
## Global imports and functions included below ##
#################################################

#################################################
## Global imports and functions included below ##
#################################################

