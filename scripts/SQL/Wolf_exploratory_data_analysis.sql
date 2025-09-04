-- ================================================
-- Wolf Survey Exploratory Data Analysis (EDA)
-- ================================================

select*
from survey_staging2;

-- 0. Response counts by year & group
SELECT year, `group`, COUNT(*) AS n_responses
FROM survey_staging2
GROUP BY year, `group`
ORDER BY year DESC, `group`;

-- 1. Tolerance toward wolves (Q3) with window stats
WITH year_stats AS (
    SELECT year, 
           AVG(Q3) AS avg_tolerance,
           MIN(Q3) AS min_tol,
           MAX(Q3) AS max_tol,
           STD(Q3) AS std_tol
    FROM survey_staging2
    GROUP BY year
)
SELECT year,
       avg_tolerance,
       min_tol,
       max_tol,
       std_tol,
       RANK() OVER (ORDER BY avg_tolerance DESC) AS rank_across_years
FROM year_stats
ORDER BY year;

-- 2. Tolerance toward wolf hunting (Q6) by group
SELECT year, `group`,
       AVG(Q6) AS avg_tolerance_hunt,
       STD(Q6) AS std_tolerance_hunt,
       COUNT(*) AS n
FROM survey_staging2
GROUP BY year, `group`
HAVING COUNT(*) > 50  -- only show groups with decent sample size
ORDER BY year DESC, avg_tolerance_hunt DESC;

-- 3. Mutualists vs. Utilitarians: CASE WHEN style
SELECT year,
       SUM(CASE WHEN classification = 'Mutualist' THEN 1 ELSE 0 END) AS n_mutualists,
       SUM(CASE WHEN classification = 'Utilitarian' THEN 1 ELSE 0 END) AS n_utilitarians,
       ROUND(100.0 * SUM(CASE WHEN classification = 'Mutualist' THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_mutualist
FROM survey_staging2
WHERE year <> 12 # 2012 survey exluded
GROUP BY year
ORDER BY year DESC;

-- 4. Pivot-style summary of trust in management (Q13)
SELECT year,
       AVG(CASE WHEN `group` = 'Land' THEN Q13 END) AS trust_landowners,
       AVG(CASE WHEN `group` = 'Deer' THEN Q13 END) AS trust_deer_hunters,
       AVG(CASE WHEN `group` = 'GenPop' THEN Q13 END) AS trust_general,
       AVG(CASE WHEN `group` = 'Wolf' THEN Q13 END) AS trust_wolf_hunters
FROM survey_staging2
WHERE year <> 12 # 2012 survey exluded
GROUP BY year
ORDER BY year DESC;

-- 5. Weighted orientation scores: windowed comparison
WITH score_summary AS (
    SELECT year,
           AVG(mutualism_score) AS avg_mutualism,
           AVG(utilitarian_score) AS avg_utilitarian,
           AVG(hunting_score) AS avg_hunting
    FROM survey_staging2
    WHERE year <> 12 # 2012 survey exluded
    GROUP BY year
)
SELECT year,
       avg_mutualism,
       avg_utilitarian,
       avg_hunting,
       RANK() OVER (ORDER BY avg_mutualism DESC) AS rank_mutualism
FROM score_summary;

-- 6. Cross-tab of support for livestock conflict hunting (Q14)
SELECT year,
       classification,
       ROUND(AVG(Q14),2) AS avg_support_conflict
FROM survey_staging2
GROUP BY year, classification
ORDER BY year DESC, avg_support_conflict DESC;

-- 7. Temp table example: identify top-5 most tolerant groups in latest year
CREATE TEMPORARY TABLE top_groups AS
SELECT `group`, AVG(Q3) AS avg_tol
FROM survey_staging2
WHERE year = (SELECT MAX(year) FROM survey_staging2)
GROUP BY `group`
ORDER BY avg_tol DESC
LIMIT 5;

SELECT * FROM top_groups;

-- ================================================
-- End of Wolf Survey EDA
-- ================================================