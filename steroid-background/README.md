Research of background steroids among the [N3C](https://ncats.nih.gov/n3c) population.
========================

Investigators
-------------------

Current & former members of this group include
David E Bard, Nelson Agudelo Higuita, Suzanne M McCahan, Annemarie Brescia, Geneva A Marshall, Jerrod Anzalone, Timothy M. VanWagoner, William H Beasley, with input from the [N3C Rural Health Domain Team](https://covid.cd2h.org/rural-health).

Methods Used
-------------------

OMOP Codesets, Propensity Scoring, Multiple Imputation, SparkSQL, R

Codesets
-------------------

### Steroids - Nasal

* nasal-spray

### Steroids - Inhaled

* inhaled-corticosteroid

### Steroids - Systemic

The analyses's eventual systemic collection of system steroids is the union of the following five codesets.

* oral-dexamethasone
  * [CSV Reviewed by SMEs](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/steroid-background/concept-sets/input/reviewed/oral-dexamethasone.csv)
  * [Enclave](https://unite.nih.gov/workspace/hubble/objects/ri.phonograph2-objects.main.object.c70c3e88-75c6-4b37-8e83-50aecd80fba4,ri.phonograph2-objects.main.object.35500a7e-6347-402a-9f68-1b45b4773989)
* oral-hydrocortisone
* systemic-hydrocortisone
* systemic-prednisolone
* systemic-prednosone-methyprednisolone

### Asthma

Converted [from the CDC's ICDs](https://www.cdc.gov/asthma/data-analysis-guidance/ICD-9-CM-ICD-10-CM.htm) to Snomeds.
