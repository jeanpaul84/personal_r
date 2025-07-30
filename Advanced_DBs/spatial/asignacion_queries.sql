-- Pagina 1

-- -- Parte 3


SELECT P.NAME,
       P.ID, 
       C.NOMBRE AS name_city,
       P.GEOMETRY AS province_geometry,
       C.SHAPE as city_point
FROM CIUDADES C, PROVINCIAS P
WHERE 
    P.ID = 'CRC' AND
    SDO_CONTAINS(p.geometry, c.shape) = 'TRUE';

-- Parte 5

SELECT
    P.NAME,
    COUNT(*) as city_count
FROM 
    CIUDADES C,
    PROVINCIAS P
WHERE 
    SDO_CONTAINS(p.geometry, c.shape) = 'TRUE'
GROUP BY 
    P.NAME;


-- Pagina 2

-- -- Parte 8

SELECT s.*,
       c.nombre
FROM   ciudades c, sismos s
WHERE
    SDO_WITHIN_DISTANCE(
          c.shape,
          s.shape,
          'distance=20 unit=KM ellipsoidal=true'
       ) = 'TRUE' AND
       c.nombre = 'Paraíso';


-- Pagina 3

-- Parte 9
SELECT p.NAME,
       p.ID, 
       s.shape as earthquake_point,
       P.GEOMETRY AS province_geometry
FROM provincias p, sismos s
WHERE 
    p.id = 'CRC' AND
    SDO_CONTAINS(p.geometry, s.shape) = 'TRUE';


-- Parte 10
SELECT p.NAME,
       p.ID, 
       s.shape as earthquake_point,
       P.GEOMETRY AS province_geometry,
       s.magnitud
FROM provincias p, sismos s
WHERE 
    p.id = 'CRC' AND
    SDO_CONTAINS(p.geometry, s.shape) = 'TRUE' AND
    s.magnitud >= 
                CASE
                    WHEN :P5_MAGNITUD = 'Y'  
                    THEN 5                
                    ELSE 0                 
                  END;

-- Pagina 4
-- Parte 11
SELECT s.id,
       s.shape AS earthquake_point
FROM   sismos s
WHERE  NOT EXISTS (
         SELECT 1
         FROM   provincias p
         WHERE  SDO_CONTAINS(p.geometry, s.shape) = 'TRUE'
       );

-- Parte 12

WITH costa_pacifica AS (
  SELECT
    SDO_AGGR_UNION(geometry) AS geom
  FROM   provincias
  WHERE  name IN ('Guanacaste','Puntarenas')
)
SELECT
    s.id,
    s.magnitud,
    s.fecha,
    s.shape AS earthquake_point
  FROM   sismos s
  JOIN   costa_pacifica cp
    ON SDO_WITHIN_DISTANCE(
         s.shape,
         cp.geom,
         'distance=10 unit=KM'
       ) = 'TRUE';

select * from provincias;


WITH costa_pacifica AS (
  SELECT
    SDO_AGGR_UNION(
      MDSYS.SDOAGGRTYPE(geometry, 0.005)
    ) AS geom
  FROM   provincias
  WHERE  name IN ('Guanacaste','Puntarenas')
),
sismos_costa AS (
  SELECT
    s.id,
    s.magnitud,
    s.fecha,
    s.shape AS earthquake_point
  FROM   sismos s
  JOIN   costa_pacifica cp
    ON SDO_WITHIN_DISTANCE(
         s.shape,
         cp.geom,
         'distance=10 unit=KM'
       ) = 'TRUE'
)
SELECT * FROM sismos_costa;



WITH linea_costera_pacifica AS (
  SELECT
    SDO_AGGR_UNION(
      MDSYS.SDOAGGRTYPE(geometry, 0.005)
    ) AS geom
  FROM   provincias
  WHERE  name IN ('Guanacaste','Puntarenas')
)
SELECT
  s.*
FROM
  sismos s,
  linea_costera_pacifica lcp
WHERE
  SDO_WITHIN_DISTANCE(
    s.shape,
    lcp.geom,
    'distance=10 unit=KM ellipsoidal=true'
  ) = 'TRUE'
  
  AND NOT EXISTS (
    SELECT 1
    FROM provincias p_check
    WHERE SDO_ANYINTERACT(s.shape, p_check.geometry) = 'TRUE'
  );


-- Pagina 6

-- Parte 14

SELECT *
FROM sismos
WHERE magnitud >= 6;

-- Parte 15

SELECT 
    SDO_AGGR_CONVEXHULL(
        SDOAGGRTYPE(shape, 0.005)
    )
FROM 
    sismos
WHERE 
    magnitud >= 6;


-- Parte 16

WITH
    provincia_geom AS (
        SELECT geometry
        FROM provincias 
        WHERE name = 'Guanacaste'
    ),
    
    sismos_hull AS (
        SELECT 
            SDO_AGGR_CONVEXHULL(
                SDOAGGRTYPE(shape, 0.005)
            ) AS hull_shape
        FROM sismos
        WHERE magnitud >= 6
    )

SELECT
    SDO_GEOM.SDO_INTERSECTION(prov.geometry, hull.hull_shape, 0.005)
FROM
    provincia_geom prov,
    sismos_hull hull;

-- Parte 17

WITH
    provincia_geom AS (
        SELECT geometry
        FROM provincias 
        WHERE name = 'Guanacaste'
    ),
    
    sismos_hull AS (
        SELECT 
            SDO_AGGR_CONVEXHULL(
                SDOAGGRTYPE(shape, 0.005)
            ) AS hull_shape
        FROM sismos
        WHERE magnitud >= 6
    )


SELECT
    ROUND(
        SDO_GEOM.SDO_AREA(
            SDO_GEOM.SDO_INTERSECTION(prov.geometry, hull.hull_shape, 0.005),
            
            0.005,
            'unit=SQ_KM' 
        ), 
    2) AS area_interseccion_km2
FROM
    provincia_geom prov,
    sismos_hull hull;
