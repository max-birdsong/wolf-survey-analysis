/* =============================================================================
   Wolf Survey – Data Cleaning Pipeline
   -----------------------------------------------------------------------------
   Context
   - Dataset: Longitudinal wildlife attitudes survey (wolves), random cross-section
     across years; not panel (IDNOs can recur across years/groups).
   - Goal: Create a de-duplicated, standardized staging table suitable for analysis
     and weighting by removing duplicate rows, normalizing key text fields, and
     dropping helper columns used during cleaning.

   Assumptions
   - MySQL 8.0+ (CTEs and window functions available).
   - Duplicates are defined as rows sharing the same (IDNO, year, `group`).
   - Keep the first occurrence within each duplicate set (ROW_NUMBER() = 1).
   - `survey_data` is the immutable raw table; staging tables are safe to create.

   Deliverables
   - survey_staging: raw copy of survey_data for safe manipulation.
   - survey_staging2: de-duplicated and standardized working table.

   Skills Used
   - Data engineering workflow: raw → staging → cleaned staging
   - Window functions: ROW_NUMBER() OVER (PARTITION BY …)
   - Common Table Expressions (CTEs) for duplicate inspection
   - De-duplication strategy design & implementation
   - Text normalization (TRIM, harmonizing categorical labels)
   - NULL handling & record hygiene (ID integrity checks)
   - Schema definition for curated staging (explicit column types)
   - Reproducible, documented SQL with guardrails and sanity checks
   ============================================================================ */

select*
from survey_data;

-- 1. Remove Duplicates

CREATE TABLE survey_staging
like survey_data;

INSERT survey_staging
select*
from survey_data;

Select*,
row_number() over(
partition by IDNO, `year`, `group`)
from survey_staging;

WITH duplicate_cte as
(
Select*,
row_number() over(
partition by IDNO, `year`, `group`) as row_num
from survey_staging
)
select*
From duplicate_cte
where row_num > 1;

CREATE TABLE `survey_staging2` (
  `IDNO` int DEFAULT NULL,
  `Q1a` int DEFAULT NULL,
  `Q1b` int DEFAULT NULL,
  `Q1c` int DEFAULT NULL,
  `Q1d` int DEFAULT NULL,
  `Q1e` int DEFAULT NULL,
  `Q1f` int DEFAULT NULL,
  `Q2a` int DEFAULT NULL,
  `Q2b` int DEFAULT NULL,
  `Q2c` int DEFAULT NULL,
  `Q2d` int DEFAULT NULL,
  `Q2e` int DEFAULT NULL,
  `Q3` int DEFAULT NULL,
  `Q4` int DEFAULT NULL,
  `Q5` int DEFAULT NULL,
  `Q6` int DEFAULT NULL,
  `Q7a` int DEFAULT NULL,
  `Q7b` text,
  `Q8a` int DEFAULT NULL,
  `Q8b` int DEFAULT NULL,
  `Q9` int DEFAULT NULL,
  `Q10` int DEFAULT NULL,
  `Q11a` int DEFAULT NULL,
  `Q11b` text,
  `Q12` int DEFAULT NULL,
  `Q13` int DEFAULT NULL,
  `Q14` int DEFAULT NULL,
  `Q15a` int DEFAULT NULL,
  `Q15b` int DEFAULT NULL,
  `Q15c` int DEFAULT NULL,
  `Q15d` int DEFAULT NULL,
  `Q15e` int DEFAULT NULL,
  `Q16a` int DEFAULT NULL,
  `Q16b` int DEFAULT NULL,
  `Q16c` int DEFAULT NULL,
  `Q16d` int DEFAULT NULL,
  `Q17` int DEFAULT NULL,
  `Q17a1` int DEFAULT NULL,
  `Q17a2` int DEFAULT NULL,
  `Q17a3` int DEFAULT NULL,
  `Q17a4` int DEFAULT NULL,
  `Q17a5` int DEFAULT NULL,
  `Q17a6` int DEFAULT NULL,
  `Q17a7` int DEFAULT NULL,
  `Q17a8` int DEFAULT NULL,
  `Q17b` int DEFAULT NULL,
  `Q18` int DEFAULT NULL,
  `Q19` int DEFAULT NULL,
  `Q20` int DEFAULT NULL,
  `Q21` int DEFAULT NULL,
  `Q22a` int DEFAULT NULL,
  `Q22b` int DEFAULT NULL,
  `Q22c` int DEFAULT NULL,
  `Q22d` int DEFAULT NULL,
  `Q22e` int DEFAULT NULL,
  `Q22f` int DEFAULT NULL,
  `Q23a` int DEFAULT NULL,
  `Q23b` int DEFAULT NULL,
  `Q23c` int DEFAULT NULL,
  `Q24` int DEFAULT NULL,
  `Q25` int DEFAULT NULL,
  `Q26a` int DEFAULT NULL,
  `Q26b` text,
  `Q27a` int DEFAULT NULL,
  `Q27b` int DEFAULT NULL,
  `Q27c` int DEFAULT NULL,
  `Q27d` int DEFAULT NULL,
  `Q27e` int DEFAULT NULL,
  `Q27f` int DEFAULT NULL,
  `Q27g` int DEFAULT NULL,
  `Q27h` int DEFAULT NULL,
  `Q27i` int DEFAULT NULL,
  `Q27j` int DEFAULT NULL,
  `Q27k` int DEFAULT NULL,
  `Q27l` int DEFAULT NULL,
  `Q27m` int DEFAULT NULL,
  `Q28` int DEFAULT NULL,
  `Q29` int DEFAULT NULL,
  `Q30` int DEFAULT NULL,
  `Q31` int DEFAULT NULL,
  `Q32` int DEFAULT NULL,
  `Q33` int DEFAULT NULL,
  `Q34` int DEFAULT NULL,
  `Q35a` int DEFAULT NULL,
  `Q35b` text,
  `Q36` int DEFAULT NULL,
  `Q37` int DEFAULT NULL,
  `FinalWt` double DEFAULT NULL,
  `group` text,
  `year` int DEFAULT NULL,
  `utilitarian_score` double DEFAULT NULL,
  `hunting_score` double DEFAULT NULL,
  `mutualism_score` double DEFAULT NULL,
  `caring_score` double DEFAULT NULL,
  `UT_WVO` double DEFAULT NULL,
  `MUT_WVO` double DEFAULT NULL,
  `classification` text,
  `strata` int DEFAULT NULL,
  `reverse_Q27d` int DEFAULT NULL,
  `reverse_Q27i` int DEFAULT NULL,
  `row_num` INT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

INSERT INTO survey_staging2
Select*,
row_number() over(
partition by IDNO, `year`, `group`)
from survey_staging;

Delete
from survey_staging2
where row_num> 1;

-- 2. Standardizing data

select*
from survey_staging2
order by `year` desc;

update survey_staging2
set `group`= trim(`group`);

update survey_staging2
set classification ='Mutualist'
where classification like 'Mutual%';

-- 3. Null values

Select*
from survey_staging2
where IDNO is NULL;

Delete
from survey_staging2
where IDNO is Null;

-- 4. remove columns

Alter Table survey_staging2
Drop column row_num;

select*
from survey_staging2
order by `year` desc;
