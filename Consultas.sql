--  En la BD se cargaron los dos archivos de Excel en formato y se revisaron los atributos de cada una
SELECT * FROM Prestadores;
SELECT * FROM Municipios;


-- Número total de Prestadores por departamento
SELECT Cod_Dep, depa_nombre As Departamento, count(codigo_habilitacion) As Total_Prestadores
FROM Prestadores
Group By Cod_Dep
Order by Total_Prestadores DESC;


-- Número total de prestadores por Municipio
SELECT  depa_nombre As Departamento, muni_nombre As Municipio, count(Cod_Muni) As Total_Prestadores
FROM Prestadores
Group By Cod_Muni
Order by Total_Prestadores DESC;


-- Total por tipo de prestador
SELECT clpr_nombre As Tipo_Prestador, COUNT(clpr_codigo) As Total
FROM Prestadores
Group By clpr_nombre;


-- Total según caracter del prestador
-- NOTA: este atributo no está disponible para todos los registros
SELECT caracter As Caracter, COUNT(caracter) As Total
FROM Prestadores
WHERE caracter IN ("DEPARTAMENTAL","NACIONAL","MUNICIPAL","DISTRITAL","INDIGENA")
Group By caracter;


-- Número total de radicaciones de Prestadores por año
SELECT DISTINCT(substring(fecha_radicacion,1,4)) As Año, count(substring(fecha_radicacion,1,4)) As Total
FROM Prestadores
Group By Año
Order by Año DESC;


-- Número total de vencimientos de Prestadores por año
SELECT DISTINCT(substring(fecha_vencimiento,1,4)) As Año, count(substring(fecha_vencimiento,1,4)) As Total
FROM Prestadores
Group By Año
Order by Año;


-- Total de Prestadores según su naturaleza y tipo de persona
SELECT DISTINCT clase_persona As Persona, naju_nombre As Tipo, count(naju_nombre) As Total
FROM Prestadores
Group By Persona, Tipo;


-- Número total de prestadores por Region
SELECT Municipios.Region As Region, count(Prestadores.Cod_Muni) As Total_Prestadores
FROM Municipios INNER JOIN Prestadores
       ON Municipios.Depmun = Prestadores.Cod_Muni
Group By Region
Order by Total_Prestadores;


-- Indice Indice prestadores según poblacion por departamento
-- NOTA:No hay prestadores en todos los municipios
SELECT Prestadores.Cod_Dep, Prestadores.depa_nombre as Departamento, COUNT(DISTINCT Prestadores.codigo_habilitacion) as Total_Prestadores, SUM(DISTINCT Municipios.Poblacion) As Poblacion, 
	((SUM(DISTINCT Municipios.Poblacion))/(COUNT(DISTINCT Prestadores.codigo_habilitacion))) as "Indice_Cobertura"
FROM Municipios INNER JOIN Prestadores
       ON Municipios.Depmun = Prestadores.Cod_Muni
Group By Departamento
Order by Departamento;
