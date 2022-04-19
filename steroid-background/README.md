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

Both codesets are stored in [`steroid-background/data-public/metadata/concept-sets`](data-public/metadata/concept-sets):
    Treatments involving steroids: [`.../rx-asthma-tx-steroid-classification.csv`](data-public/metadata/concept-sets/rx-asthma-tx-steroid-classification.csv)
    Treatments involving non steroids: [`.../rx-asthma-tx-nonsteroid-classification.csv`](data-public/metadata/concept-sets/rx-asthma-tx-nonsteroid-classification.csv)


### Asthma

Converted [from the CDC's ICDs](https://www.cdc.gov/asthma/data-analysis-guidance/ICD-9-CM-ICD-10-CM.htm) to Snomeds.
