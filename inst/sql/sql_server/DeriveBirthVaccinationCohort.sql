-- Drop the table if it already exists
DROP TABLE IF EXISTS @cohort_database_schema.dosette_birth_vaccine_cohort;

-- Create the new table and insert the results of the join into it
CREATE TABLE @cohort_database_schema.dosette_birth_vaccine_cohort (
  subject_id int NOT NULL,
  birth_date date,                   
  date_dose_1 date,                  
  date_dose_2 date,
  date_dose_3 date,
  date_dose_4 date
);

INSERT INTO @cohort_database_schema.dosette_birth_vaccine_cohort (
  subject_id, 
  birth_date, 
  date_dose_1, 
  date_dose_2, 
  date_dose_3,
  date_dose_4
  )
SELECT 
    t1.subject_id,
    t1.cohort_start_date AS birth_date,
    MAX(CASE WHEN dose_num = 1 THEN t2.cohort_start_date END) AS date_dose_1,
    MAX(CASE WHEN dose_num = 2 THEN t2.cohort_start_date END) AS date_dose_2,
    MAX(CASE WHEN dose_num = 3 THEN t2.cohort_start_date END) AS date_dose_3,
    MAX(CASE WHEN dose_num = 4 THEN t2.cohort_start_date END) AS date_dose_4
FROM 
    (SELECT subject_id, cohort_start_date 
     FROM @cohort_database_schema.@cohort_table
     WHERE cohort_definition_id = 1) t1
LEFT JOIN 
    (SELECT subject_id, cohort_start_date, 
            ROW_NUMBER() OVER (PARTITION BY subject_id ORDER BY cohort_start_date) AS dose_num 
     FROM @cohort_database_schema.@cohort_table 
     WHERE cohort_definition_id = 2) t2
ON t1.subject_id = t2.subject_id
GROUP BY 
    t1.subject_id, t1.cohort_start_date
ORDER BY 
    t1.subject_id;
