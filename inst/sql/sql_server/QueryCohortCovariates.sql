-- Drop the table if it already exists
DROP TABLE IF EXISTS @cohort_database_schema.@result_cohort_table;

-- Create the new table with explicit data types
CREATE TABLE @cohort_database_schema.@result_cohort_table (
    subject_id INT NOT NULL,                     -- Assuming subject_id is of type INT
    birth_date DATE,                    -- Assuming birth_date is of type DATE
    date_dose_1 DATE,                   -- Assuming vaccination dates are of type DATE
    date_dose_2 DATE,
    date_dose_3 DATE,
    date_dose_4 DATE,
    gender VARCHAR(50),                 -- Assuming gender is a string, adjust length as needed
    race VARCHAR(50),                   -- Assuming race is a string, adjust length as needed
    ethnicity VARCHAR(50),              -- Assuming ethnicity is a string, adjust length as needed
    location VARCHAR(100)               -- Assuming state/location is a string, adjust length as needed
);

-- Insert the results into the created table
INSERT INTO @cohort_database_schema.@result_cohort_table (subject_id, birth_date, date_dose_1, date_dose_2, date_dose_3, date_dose_4, gender, race, ethnicity, location)
SELECT 
    dbvc.subject_id,                    -- subject_id from dosette_birth_vaccine_cohort
    dbvc.birth_date,                    -- birth_date from dosette_birth_vaccine_cohort
    dbvc.date_dose_1,                   -- date_dose_1 from dosette_birth_vaccine_cohort
    dbvc.date_dose_2,                   -- date_dose_2 from dosette_birth_vaccine_cohort
    dbvc.date_dose_3,                   -- date_dose_3 from dosette_birth_vaccine_cohort
    dbvc.date_dose_4,                   -- date_dose_4 from dosette_birth_vaccine_cohort
    gender.concept_name AS gender,      -- gender concept_name from concept table
    race.concept_name AS race,          -- race concept_name from concept table
    ethnicity.concept_name AS ethnicity, -- ethnicity concept_name from concept table
    loc.state AS location                -- state from location table
FROM 
    @cohort_database_schema.dosette_birth_vaccine_cohort dbvc
LEFT JOIN 
    @cdm_database_schema.person p ON dbvc.subject_id = p.person_id           -- Join on person_id
LEFT JOIN 
    @cdm_database_schema.concept gender ON p.gender_concept_id = gender.concept_id    -- Join with concept for gender
LEFT JOIN 
    @cdm_database_schema.concept race ON p.race_concept_id = race.concept_id          -- Join with concept for race
LEFT JOIN 
    @cdm_database_schema.concept ethnicity ON p.ethnicity_concept_id = ethnicity.concept_id -- Join with concept for ethnicity
LEFT JOIN 
    @cdm_database_schema.location loc ON p.location_id = loc.location_id;            -- Join with location table for state