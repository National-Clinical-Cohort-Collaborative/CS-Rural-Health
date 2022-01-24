# Code Workbooks
A code workbook is the fundamental coding pipeline used by the platform underlying N3C, Palantir's Foundry. Per their [documentation](https://unite.nih.gov/workspace/documentation/product/code-workbook/overview):

>Code Workbook is an application that allows users to analyze and transform data using an intuitive graphical interface.

This platform contains individual code transforms in SQL, Python, and R with an Apache Spark backend. This folder describes the primary code used in this project, including: 

 - [OS events query](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/ordinal-scale-EHR/code_workbook/1_OS_events_query.sql)
 - [Daily and Weekly OS level calculation](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/ordinal-scale-EHR/code_workbook/2_Daily%26Weekly_OS_level_calculation_method1.sql)
 - [Daily and Weekly OS level formatting](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/ordinal-scale-EHR/code_workbook/2_Daily%26%20Weekly_OS_level_formatting_method1.r)
 - [Weekly OS level calculation alternative approach](https://github.com/National-COVID-Cohort-Collaborative/CS-Rural-Health/blob/main/ordinal-scale-EHR/code_workbook/3_Weekly_OS_level_calculation_method2.sql)

## Primary Contributors
Base COVID-19 positive cohort called in this project involves shared logic used across all N3C project as described in the [N3C Cohort Paper](https://www.medrxiv.org/content/10.1101/2021.01.12.21249511v3.full-text). Additional coding was done primarily by 
[Maryam Khodaverdi](https://directory.hsc.wvu.edu/Profile/61365).

