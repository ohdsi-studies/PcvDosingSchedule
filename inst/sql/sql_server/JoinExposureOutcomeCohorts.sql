DROP TABLE IF EXISTS @cohort_database_schema.dosette_exposure_outcome_cohort;

CREATE TABLE @cohort_database_schema.dosette_exposure_outcome_cohort (
    pid BIGINT,
    sex VARCHAR(50),               
    race VARCHAR(50),                 
    ethnicity VARCHAR(50),           
    state VARCHAR(100),
    dob DATE,                  
    date_pcv1 DATE,                 
    date_pcv2 DATE,
    date_pcv3 DATE,
    date_pcv4 DATE,
    outcome INT,
    date_outcome DATE,
    date_admin_censor DATE
);


INSERT INTO @cohort_database_schema.dosette_exposure_outcome_cohort 
SELECT 
    dcc.subject_id AS pid,
    dcc.sex,
    dcc.race,
    dcc.ethnicity,
    dcc.state,
    dcc.birth_date AS dob,
    dcc.date_dose_1 AS date_pcv1,
    dcc.date_dose_2 AS date_pcv2,
    dcc.date_dose_3 AS date_pcv3,
    dcc.date_dose_4 AS date_pcv4,
    -- compute 1/0 indicator of outcome occurrence
    CASE
        WHEN oc.cohort_start_date IS NOT NULL AND 
             oc.cohort_start_date < DATEADD(YEAR, 5, dcc.birth_date) 
        THEN 1
        ELSE 0
    END AS outcome,
    -- Compute outcome_date based on the LEFT JOIN logic
    CASE 
        WHEN oc.cohort_start_date IS NOT NULL AND 
             oc.cohort_start_date > DATEADD(YEAR, 5, dcc.birth_date) 
        THEN NULL
        ELSE oc.cohort_start_date 
    END AS date_outcome,
    -- Process admin censor again, making sure it's <= 5 years after birth
    CASE WHEN dcc.date_admin_censor IS NOT NULL AND
              dcc.date_admin_censor > DATEADD(YEAR, 5, dcc.birth_date)
         THEN DATEADD(YEAR, 5, dcc.birth_date)
         ELSE dcc.date_admin_censor
    END AS date_admin_censor
FROM 
    @cohort_database_schema.@exposure_cohort_table dcc
LEFT JOIN 
    (SELECT subject_id, cohort_start_date 
     FROM @cohort_database_schema.@outcome_cohort_table 
     WHERE cohort_definition_id = @outcome_id) oc 
ON dcc.subject_id = oc.subject_id;