Comparative Effectiveness of Different Dosing Schedules of Pneumococcal Conjugate Vaccines:
Target Trial Emulation
=============

<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): `Population-Level Estimation`
- Study type: `Clinical Application`
- Tags: **-**
- Study lead: Fan Bu and Kayoko Shioda
- Study lead forums tag: **[fanbu](https://forums.ohdsi.org/u/fanbu)**
- Study start date: **Feb 01, 2025**
- Study end date: **-**
- Protocol: **[protocol](https://ohdsi-studies.github.io/PcvDosingSchedule/Protocol)**
- Publications: **-**
- Results explorer: **-**

This study aims to evaluate the comparative effectiveness of different PCV13 dosing schedules among children under five years of age using observational healthcare records over a network of data sources. 
We will use a trial emulation method, clone censor weight analysis, to estimate the relative risk of clinical outcomes of interest between different PCV13 dosing schedules. 

This study is particularly relevant and time-sensitive as the World Health Organization (WHO) Strategic Advisory Group of Experts on Immunization (SAGE) who will provide recommendation in **early 2025** about switching from the existing 2+1 or 3+0 PCV dosing schedules to the 1+1 schedule. 
The WHO SAGE group have been gathering data to investigate the potential impact of this change, and results of this large-scale real-world study will be able to provide supporting evidence.

Requirements
============

- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, or Microsoft APS.
- R version >= 4.3
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 50 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java.

2. `git clone` this repository to your local machine or server.

3. Open the study package in RStudio. Use the following code to install all the dependencies. You may need several attempts of running `renv::restore()`. In some cases, you may need to manually install the dependencies (please refer to those listed in the `DESCRIPTION` file). 

	```r
	install.packages("renv")
	renv::activate()
	renv::restore()
	```

4. In RStudio, select 'Build' then 'Install and Restart' to install the `PCVDosingSchedule` package.

5. To execute the study, go to `extras/CodeToRunTemplate.R`, customize the first block of code given your data server crendentials and local environment (don't forget to update your database metadata and local output directory!), and run the code block with the `execute()` function.
