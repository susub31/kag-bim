with cte_1 as
(
  SELECT Semana, Agencia_ID, Ruta_SAK, Cliente_ID, Producto_ID, COUNT(*) as [Count] FROM train GROUP BY Semana, Agencia_ID, Ruta_SAK, Cliente_ID, Producto_ID HAVING COUNT(*) > 1
)

select Semana, Cliente_ID, Producto_ID, Agencia_ID, Ruta_SAK, Canal_ID, Venta_uni_hoy, Dev_uni_proxima, Demanda_uni_equil from train t 
WHERE (SELECT COUNT(*) FROM cte_1 c WHERE t.Semana = c.Semana AND t.Agencia_ID = c.Agencia_ID AND t.Ruta_SAK = c.Ruta_SAK AND t.Cliente_ID = c.Cliente_ID AND t.Producto_ID = c.Producto_ID) > 0
ORDER BY Semana, Cliente_ID, Producto_ID, Agencia_ID, Ruta_SAK, Canal_ID