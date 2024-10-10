## The Impact of COVID-19 Treatments on Patient Outcomes: A Probabilistic View 

## Authors
[Bradley S. Price, PhD](https://business.wvu.edu/faculty-and-staff/directory/profile?pid=273); [Maryam Khodaverdi, MS](https://directory.hsc.wvu.edu/Profile/61365);  [Hamidreza Moradi, PhD](https://umc.edu/facultyprofile/moradi_hamidreza/); [H. Timothy Bunnell, PhD](https://www.nemoursresearch.org/snap/user/5); [J. Zachary Porterfield, MD PhD](https://med.uky.edu/users/jpo284); [Michael T. Vest, MD](https://doctors.christianacare.org/provider/Michael+T.+Vest/844978); [William B. Hillegass, PhD](https://www.umc.edu/FacultyProfile/Hillegass_William_B/); [Susan L. Santangelo, ScD](https://mmcri.org/?page_id=9111); [Wes D. Kimble, MPA](https://directory.hsc.wvu.edu/Profile/39623); [Alfred J. Anzalone, MS](https://www.unmc.edu/bmi/current-students/student-bios/anzalone-jerrod-bio.html); [Jeremy Harper, MS](https://owlhealthworks.com/);  [Brian Hendricks, PhD](https://directory.hsc.wvu.edu/Profile/52462);  [Sally L. Hodder, MD](https://directory.hsc.wvu.edu/Profile/41751)

## Abstract
Background: Machine learning approaches paired with large-scale electronic health record (EHR) databases, such as the National COVID Cohort Collaborative (N3C), provide researchers with clinical data from multiple participating sites. These data can be used to study the impact of treatment across large cohorts, which can then be used to inform practitioners and randomized clinical trials. In this study, we assess broad classes of therapeutics and identify specific agents and combinations of agents associated with the greatest likelihood of clinical improvement for hospitalized COVID-19 patients.
Methods: This study includes 222,690 COVID-19 patients hospitalized from April 2020 to March 2022. We assess COVID-19 disease severity among persons hospitalized using an ordinal scale measured across the pre-Delta, Delta, and Omicron variant-dominant periods over 28 days. Elastic-net-penalized multinomial regression provides probabilistic estimates to evaluate the effectiveness of treatment classes (e.g. Antivirals, Antibiotics, spike protein Monoclonal Antibodies) while considering the impact of Charlson Co-morbidity Index (CCI), and patient demographics. 
Results: Generally, as CCI or disease severity increased, the likelihood of hospital discharge decreased. Mechanically ventilated patients had better outcomes during the Omicron wave, regardless of therapeutic regimen. Spike protein Monoclonal Antibody therapy in combination with Steroids and/or Anticoagulants, was associated with the highest estimated likelihood of hospital discharge for most patients. 
Conclusion: Machine learning approaches defined therapeutic combinations most highly associated with clinical improvement among hospitalized COVID-19 patients with varying disease severity and comorbidities. These insights may help confirm and possibly extend the knowledge from randomized trials and inform the design of future randomized studies.


## Repository Usage
This repository is broken down into two sections: 

[Results of modeling](https://github.com/National-Clinical-Cohort-Collaborative/CS-Rural-Health/tree/main/medication-groups/model-results)

[Results of bootstrapping](https://github.com/National-Clinical-Cohort-Collaborative/CS-Rural-Health/tree/main/medication-groups/bootstrap-results)

## National COVID Cohort Collaborative (N3C)
N3C is a public resource maintained by NCATS to support COVID-19 research. Investigators can request access to the Enclave [here](https://ncats.nih.gov/n3c/about/applying-for-access).


## License
[MIT](https://choosealicense.com/licenses/mit/)
