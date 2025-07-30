select * from fact_sales;

-- PREGUNTA 1 ---------------------------------------------------------------------------------------------

WITH DISTRIBUTOR_TOTALS AS (
    SELECT
        dd.STATE,
        dd.DISTRIBUTOR_NAME,
        SUM(fs.SELL_PRICE) AS TOTAL_SALES,
        SUM(fs.UNITS_SOLD) AS TOTAL_UNITS
    FROM
        FACT_SALES fs
    JOIN
        DIM_DISTRIBUTOR dd ON fs.DIST_DIM_ID = dd.DIST_DIM_ID
    GROUP BY
        dd.STATE,
        dd.DISTRIBUTOR_NAME
),
RANKED_DISTRIBUTORS AS (
    SELECT
        STATE,
        DISTRIBUTOR_NAME,
        TOTAL_SALES,
        TOTAL_UNITS,
        RANK() OVER (PARTITION BY STATE ORDER BY TOTAL_SALES DESC) AS DISTRIBUTOR_RANK
    FROM
        DISTRIBUTOR_TOTALS
)
SELECT
    STATE,
    DISTRIBUTOR_NAME,
    TOTAL_SALES,
    TOTAL_UNITS,
    DISTRIBUTOR_RANK
FROM
    RANKED_DISTRIBUTORS
WHERE
    DISTRIBUTOR_RANK <= 3
ORDER BY
    STATE,
    DISTRIBUTOR_RANK;



-- PREGUNTA 2 ---------------------------------------------------------------------------------------------

WITH BRAND_YEAR_SALES AS (
    SELECT
        dt.YEAR,
        dc.MAKE,
        SUM(fs.SELL_PRICE) AS TOTAL_BRAND_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dc.MAKE
)
SELECT
    YEAR,
    MAKE,
    TOTAL_BRAND_SALES,
    ROUND(RATIO_TO_REPORT(TOTAL_BRAND_SALES) OVER (PARTITION BY YEAR) * 100, 2) AS PERCENT_OF_YEAR_SALES
FROM
    BRAND_YEAR_SALES
ORDER BY
    YEAR,
    PERCENT_OF_YEAR_SALES DESC;



-- PREGUNTA 3 ---------------------------------------------------------------------------------------------

WITH MONTHLY_SALES AS (
    SELECT
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME,
        SUM(fs.SELL_PRICE) AS TOTAL_MONTH_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME
),
SALES_WITH_LAG AS (
    SELECT
        YEAR,
        MONTH_NAME,
        MONTH_NUMBER,
        TOTAL_MONTH_SALES,
        LAG(TOTAL_MONTH_SALES, 1) OVER (ORDER BY YEAR, MONTH_NUMBER) AS PREVIOUS_MONTH_SALES
    FROM
        MONTHLY_SALES
)
SELECT
    YEAR,
    MONTH_NAME,
    MONTH_NUMBER,
    TOTAL_MONTH_SALES,
    PREVIOUS_MONTH_SALES,
    CASE
        WHEN PREVIOUS_MONTH_SALES IS NOT NULL AND PREVIOUS_MONTH_SALES != 0
        THEN ROUND(((TOTAL_MONTH_SALES - PREVIOUS_MONTH_SALES) / PREVIOUS_MONTH_SALES) * 100, 2)
        ELSE NULL
    END AS PERCENTAGE_CHANGE
FROM
    SALES_WITH_LAG
ORDER BY
    YEAR,
    MONTH_NUMBER; 


-- PREGUNTA 4 ---------------------------------------------------------------------------------------------


WITH MONTHLY_SALES AS (
    SELECT
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME,
        SUM(fs.SELL_PRICE) AS TOTAL_MONTH_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME
)
SELECT
    YEAR,
    MONTH_NAME,
    MONTH_NUMBER,
    TOTAL_MONTH_SALES,
    ROUND(
        AVG(TOTAL_MONTH_SALES) OVER (ORDER BY YEAR, MONTH_NUMBER ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
    , 2) AS CENTERED_3_MONTH_MOVING_AVG
FROM
    MONTHLY_SALES
ORDER BY
    YEAR,
    MONTH_NUMBER;


-- PREGUNTA 5 ---------------------------------------------------------------------------------------------

WITH MONTHLY_SALES AS (
    SELECT
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME,
        SUM(fs.SELL_PRICE) AS TOTAL_MONTH_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME
),
SALES_YOY_COMPARISON AS (
    SELECT
        YEAR,
        MONTH_NAME,
        MONTH_NUMBER,
        TOTAL_MONTH_SALES,
        LAG(TOTAL_MONTH_SALES, 1) OVER (PARTITION BY MONTH_NUMBER ORDER BY YEAR) AS PREVIOUS_YEAR_SALES
    FROM
        MONTHLY_SALES
)
SELECT
    YEAR,
    MONTH_NAME,
    TOTAL_MONTH_SALES,
    PREVIOUS_YEAR_SALES,
    ABS(TOTAL_MONTH_SALES - PREVIOUS_YEAR_SALES) AS ABSOLUTE_DIFFERENCE
FROM
    SALES_YOY_COMPARISON
ORDER BY
    YEAR,
    MONTH_NUMBER;

-- PREGUNTA 6 ---------------------------------------------------------------------------------------------

WITH MONTHLY_SALES AS (
    SELECT
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME,
        SUM(fs.SELL_PRICE) AS TOTAL_MONTH_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME
)
SELECT
    YEAR,
    MONTH_NAME,
    TOTAL_MONTH_SALES,
    SUM(TOTAL_MONTH_SALES) OVER (
        PARTITION BY YEAR
        ORDER BY MONTH_NUMBER
        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
    ) AS CUMULATIVE_SALES_BY_YEAR
FROM
    MONTHLY_SALES
ORDER BY
    YEAR,
    MONTH_NUMBER;

-- PREGUNTA 7 ---------------------------------------------------------------------------------------------

SELECT
    dco.COUNTRY,
    SUM(fs.UNITS_SOLD) AS TOTAL_UNITS_SOLD,
    ROUND(AVG(dc.RETAIL_PRICE), 2) AS AVERAGE_RETAIL_PRICE
FROM
    FACT_SALES fs
JOIN
    DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
JOIN
    DIM_CAR_ORIGIN dco ON fs.ORIGIN_DIM_ID = dco.ORIGIN_ID
GROUP BY
    dco.COUNTRY
ORDER BY
    TOTAL_UNITS_SOLD DESC;


-- PREGUNTA 8 ---------------------------------------------------------------------------------------------

SELECT DISTINCT MARKET_CATEGORY FROM DIM_CAR WHERE MARKET_CATEGORY IS NOT NULL;

WITH SALES_DATA_TO_PIVOT AS (
    SELECT
        dc.TARGET_MARKET,
        dc.MARKET_CATEGORY,
        fs.UNITS_SOLD,
        fs.SELL_PRICE
    FROM
        FACT_SALES fs
    JOIN
        DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
)
SELECT *
FROM SALES_DATA_TO_PIVOT
PIVOT (
    SUM(UNITS_SOLD) AS UNITS,
    SUM(SELL_PRICE) AS SALES
    FOR MARKET_CATEGORY IN (

        'High-Performance' AS High_Performance,
        'Hatchback'        AS Hatchback,
        'Crossover'        AS Crossover,
        'Hybrid'           AS Hybrid,
        'Diesel'           AS Diesel,
        'Performance'      AS Performance,
        'Exotic'           AS Exotic,
        'Sedan'            AS Sedan,
        'Other'            AS Other,
        'Electric'         AS Electric,
        'Luxury'           AS Luxury
    )
)
ORDER BY
    TARGET_MARKET;


-- PREGUNTA 9 ---------------------------------------------------------------------------------------------

WITH SALES_BY_CAT_AGE AS (
    SELECT
        dc.MARKET_CATEGORY,
        dcu.AGE_BUCKET,
        SUM(fs.SELL_PRICE) AS TOTAL_SALES
    FROM
        FACT_SALES fs
    JOIN
        DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
    JOIN
        DIM_CUSTOMER_DATA dcu ON fs.CUST_DIM_ID = dcu.CUST_DIM_ID
    WHERE
        dc.MARKET_CATEGORY IS NOT NULL
    GROUP BY
        dc.MARKET_CATEGORY,
        dcu.AGE_BUCKET
)
SELECT
    MARKET_CATEGORY,
    AGE_BUCKET,
    TOTAL_SALES,
    ROUND(
        RATIO_TO_REPORT(TOTAL_SALES) OVER (PARTITION BY MARKET_CATEGORY) * 100, 2
    ) AS PERCENT_OF_CATEGORY_SALES
FROM
    SALES_BY_CAT_AGE
ORDER BY
    MARKET_CATEGORY,
    AGE_BUCKET;


-- PREGUNTA 10 ---------------------------------------------------------------------------------------------

WITH LUXURY_EXOTIC_SALES AS (
    SELECT
        fs.TIME_DIM_ID,
        fs.DISCOUNT_AMOUNT,
        dc.RETAIL_PRICE
    FROM
        FACT_SALES fs
    JOIN
        DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
    WHERE
        dc.MARKET_CATEGORY IN ('Exotic', 'Luxury')
),
MONTHLY_AVG_DISCOUNT AS (
    SELECT
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME,
        (SUM(les.DISCOUNT_AMOUNT) / SUM(les.RETAIL_PRICE)) * 100 AS AVG_DISCOUNT_PERCENT
    FROM
        LUXURY_EXOTIC_SALES les
    JOIN
        DIM_TIME dt ON les.TIME_DIM_ID = dt.TIME_ID
    GROUP BY
        dt.YEAR,
        dt.MONTH_NUMBER,
        dt.MONTH_NAME
),
DISCOUNT_TREND_DATA AS (
    SELECT
        YEAR,
        MONTH_NUMBER,
        MONTH_NAME,
        AVG_DISCOUNT_PERCENT,
        LAG(AVG_DISCOUNT_PERCENT, 1) OVER (ORDER BY YEAR, MONTH_NUMBER) AS PREVIOUS_MONTH_AVG_DISCOUNT
    FROM
        MONTHLY_AVG_DISCOUNT
)
SELECT
    YEAR,
    MONTH_NAME,
    ROUND(AVG_DISCOUNT_PERCENT, 2) AS AVG_DISCOUNT_PERCENT,
    ROUND(PREVIOUS_MONTH_AVG_DISCOUNT, 2) AS PREVIOUS_MONTH_AVG_DISCOUNT,
    CASE
        WHEN PREVIOUS_MONTH_AVG_DISCOUNT IS NOT NULL AND PREVIOUS_MONTH_AVG_DISCOUNT != 0
        THEN ROUND(((AVG_DISCOUNT_PERCENT - PREVIOUS_MONTH_AVG_DISCOUNT) / PREVIOUS_MONTH_AVG_DISCOUNT) * 100, 2)
        ELSE NULL
    END AS DISCOUNT_TREND_PERCENT_CHANGE
FROM
    DISCOUNT_TREND_DATA
ORDER BY
    YEAR,
    MONTH_NUMBER;


-- PREGUNTA 11 ---------------------------------------------------------------------------------------------




WITH MODEL_YEARLY_UNITS AS (
    SELECT
        dc.MAKE,
        dc.MODEL,
        dc.YEAR AS CAR_MODEL_YEAR,
        dt.YEAR AS SALE_YEAR,
        fs.UNITS_SOLD
    FROM
        FACT_SALES fs
    JOIN
        DIM_CAR dc ON fs.CAR_DIM_ID = dc.CAR_DIM_ID
    JOIN
        DIM_TIME dt ON fs.TIME_DIM_ID = dt.TIME_ID
    WHERE
        dt.YEAR IN (2022, 2023)
),
SALES_BY_YEAR AS (
    SELECT * FROM (
        SELECT MAKE, MODEL, CAR_MODEL_YEAR, SALE_YEAR, UNITS_SOLD
        FROM MODEL_YEARLY_UNITS
    )
    PIVOT (
        SUM(UNITS_SOLD)
        FOR SALE_YEAR IN (2022 AS UNITS_2022, 2023 AS UNITS_2023)
    )
),
MODEL_GROWTH AS (
    SELECT
        MAKE,
        MODEL,
        CAR_MODEL_YEAR,
        NVL(UNITS_2022, 0) AS UNITS_2022, 
        NVL(UNITS_2023, 0) AS UNITS_2023,
        CASE
            WHEN NVL(UNITS_2022, 0) > 0
            THEN ((NVL(UNITS_2023, 0) - NVL(UNITS_2022, 0)) / NVL(UNITS_2022, 0)) * 100
            ELSE NULL 
        END AS PERCENT_GROWTH
    FROM
        SALES_BY_YEAR
)
SELECT
    MAKE,
    MODEL,
    CAR_MODEL_YEAR,
    UNITS_2023,
    ROUND(PERCENT_GROWTH, 2) AS PERCENT_GROWTH,
    NTILE(5) OVER (ORDER BY PERCENT_GROWTH DESC NULLS LAST) AS GROWTH_PERCENTILE
FROM
    MODEL_GROWTH
WHERE
    UNITS_2022 > 0
ORDER BY
    GROWTH_PERCENTILE,
    PERCENT_GROWTH DESC;