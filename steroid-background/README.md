Research of background steroids among the [N3C](https://ncats.nih.gov/n3c) population.
========================

Investigators
-------------------

Current & former members of this group include
David E Bard, Nelson Agudelo Higuita, Suzanne M McCahan, Annemarie Brescia, Geneva A Marshall, Jerrod Anzalone, Timothy M. VanWagoner, William H Beasley, with input from the [N3C Rural Health Domain Team](https://covid.cd2h.org/rural-health).

Resources
------------------

* "CTR N3C" Google Drive: `Manuscripts/steroid-background`
* N3C Enclave [RP-504BA5](https://unite.nih.gov/workspace/compass/view/ri.compass.main.folder.f90211ae-6dc2-4eba-b3ae-919cc78a9aaf) *COVID-19 Treatements Associated with Lower Mortality*: [`projects/steroid-background`](https://unite.nih.gov/workspace/compass/view/ri.compass.main.folder.4d227459-f9bb-4ea2-b4b3-ef437369665e)

Methods Used
-------------------

OMOP Codesets, Propensity Scoring, Multiple Imputation, SparkSQL, R

Codesets
-------------------

### Steroids - Nasal

* nasal-spray
  * [CSV Reviewed by SMEs](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/steroid-background/concept-sets/input/reviewed/nasal-spray.csv)
  * [Enclave](https://unite.nih.gov/workspace/hubble/objects/ri.phonograph2-objects.main.object.ea6c820d-e34a-4029-9756-6c712ead4ec8?tab=overview)

### Steroids - Inhaled

* inhaled-corticosteroid
  * [CSV Reviewed by SMEs](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/steroid-background/concept-sets/input/reviewed/inhaled-corticosteroid.csv) 
  * [Enclave](https://unite.nih.gov/workspace/hubble/objects/ri.phonograph2-objects.main.object.cd4cc48f-9037-468b-bd8f-368a0c07068b?tab=overview)

### Steroids - Systemic

The analyses's eventual systemic collection of system steroids is the union of the following five codesets.

* oral-dexamethasone
  * [CSV Reviewed by SMEs](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/steroid-background/concept-sets/input/reviewed/oral-dexamethasone.csv)
  * [Enclave](https://unite.nih.gov/workspace/hubble/objects/ri.phonograph2-objects.main.object.c70c3e88-75c6-4b37-8e83-50aecd80fba4,ri.phonograph2-objects.main.object.35500a7e-6347-402a-9f68-1b45b4773989)
* oral-hydrocortisone
  * [CSV Reviewed by SMEs](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/steroid-background/concept-sets/input/reviewed/oral-hydrocortisone.csv)
  * [Enclave](https://unite.nih.gov/workspace/hubble/objects/ri.phonograph2-objects.main.object.02f9c5e9-9038-47e4-a511-2e8e356daec1?tab=overview)
* systemic-hydrocortisone
* systemic-prednisolone
* systemic-prednosone-methyprednisolone

### Asthma

Converted [from the CDC's ICDs](https://www.cdc.gov/asthma/data-analysis-guidance/ICD-9-CM-ICD-10-CM.htm) to Snomeds.
