WITH BUDG AS (
SELECT 
    PERIOD_NAME, 
    ENTITY, 
    DEPARTMENT,
    SPLIT_PART(BUDGET_NAME,' ', 1) AS "BUDGET_FY",
    SPLIT_PART(BUDGET_NAME,' ', 2) AS "BUDGET_COUNTRY",
    CASE
        WHEN SPLIT_PART(BUDGET_NAME,' ', 3) = 'PROJ' THEN SUM(BUDGET_AMOUNT)
    END AS "PROJ_BUDGET_AMOUNT",
    CASE
        WHEN SPLIT_PART(BUDGET_NAME,' ', 3) = 'BUDGET' THEN SUM(BUDGET_AMOUNT)
    END AS "BUDGET_AMOUNT"   
    FROM "ANALYTICS"."GL"."fct_budgets"
    GROUP BY 
        PERIOD_NAME, 
        ENTITY, 
        DEPARTMENT,
        SPLIT_PART(BUDGET_NAME,' ', 1),
        SPLIT_PART(BUDGET_NAME,' ', 2),
        SPLIT_PART(BUDGET_NAME,' ', 3)
),
BUDG2 AS (
SELECT PERIOD_NAME, ENTITY, DEPARTMENT, BUDGET_FY, BUDGET_AMOUNT
FROM BUDG
WHERE BUDGET_AMOUNT IS NOT NULL 
),
BUDG3 AS (
SELECT DEPARTMENT, ENTITY, DEPARTMENT || '_' || ENTITY AS "DEP_ENT", BUDGET_AMOUNT, TO_DATE(PERIOD_NAME, 'MON-yy') as "MONTH"
FROM BUDG2)
SELECT *
FROM BUDG3
WHERE DEP_ENT IN (
    '160_155',
    '170_155',
    '200_155',
    '200_310',
    '210_155',
    '210_165',
    '210_310',
    '220_155',
    '220_310',
    '240_155',
    '250_155',
    '250_165',
    '250_310',
    '260_155'
) AND MONTH >= '2016-01-01'