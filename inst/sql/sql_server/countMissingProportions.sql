SELECT 
    'subject_id' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN subject_id IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN subject_id IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'birth_date' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN birth_date IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN birth_date IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'date_dose_1' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN date_dose_1 IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN date_dose_1 IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'date_dose_2' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN date_dose_2 IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN date_dose_2 IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'date_dose_3' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN date_dose_3 IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN date_dose_3 IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'date_dose_4' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN date_dose_4 IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN date_dose_4 IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'sex' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN sex IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN sex IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'race' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN race IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN race IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'ethnicity' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN ethnicity IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN ethnicity IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates

UNION ALL

SELECT 
    'state' AS column_name,
    COUNT(*) AS total_count,
    SUM(CASE WHEN state IS NULL THEN 1 ELSE 0 END) AS missing_count,
    SUM(CASE WHEN state IS NULL THEN 1 ELSE 0 END) * 1.0 / COUNT(*) AS missing_proportion

FROM 
    @cohort_database_schema.dosette_cohort_covariates;
