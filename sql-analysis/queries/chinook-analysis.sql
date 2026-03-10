-- ============================================
-- Chinook Music Store Analysis
-- Author: xipecast
-- Date: 2026
-- ============================================

-- ---- Revenue by Country ----
SELECT 
    c.Country,
    COUNT(DISTINCT i.InvoiceId) as total_orders,
    ROUND(SUM(i.Total), 2) as total_revenue
FROM Customer c
JOIN Invoice i ON c.CustomerId = i.CustomerId
GROUP BY c.Country
ORDER BY total_revenue DESC
LIMIT 10;

-- ---- Top Artists by Revenue ----
SELECT 
    ar.Name as Artist,
    COUNT(il.InvoiceLineId) as tracks_sold,
    ROUND(SUM(il.UnitPrice * il.Quantity), 2) as revenue
FROM Artist ar
JOIN Album al ON ar.ArtistId = al.ArtistId
JOIN Track t ON al.AlbumId = t.AlbumId
JOIN InvoiceLine il ON t.TrackId = il.TrackId
GROUP BY ar.Name
ORDER BY revenue DESC
LIMIT 10;

-- ---- Top Genres by Revenue ----
SELECT 
    g.Name as Genre,
    COUNT(il.InvoiceLineId) as tracks_sold,
    ROUND(SUM(il.UnitPrice * il.Quantity), 2) as revenue
FROM Genre g
JOIN Track t ON g.GenreId = t.GenreId
JOIN InvoiceLine il ON t.TrackId = il.TrackId
GROUP BY g.Name
ORDER BY revenue DESC
LIMIT 10;

-- ---- Employee Sales Performance ----
SELECT 
    e.FirstName || ' ' || e.LastName as Employee,
    e.Title,
    COUNT(DISTINCT i.InvoiceId) as total_sales,
    ROUND(SUM(i.Total), 2) as total_revenue
FROM Employee e
JOIN Customer c ON e.EmployeeId = c.SupportRepId
JOIN Invoice i ON c.CustomerId = i.CustomerId
GROUP BY e.EmployeeId
ORDER BY total_revenue DESC;

-- ---- Employee Revenue Per Sale ----
SELECT 
    e.FirstName || ' ' || e.LastName as Employee,
    COUNT(DISTINCT i.InvoiceId) as total_sales,
    ROUND(SUM(i.Total), 2) as total_revenue,
    ROUND(SUM(i.Total)/COUNT(DISTINCT i.InvoiceId), 2) as avg_per_sale
FROM Employee e
JOIN Customer c ON e.EmployeeId = c.SupportRepId
JOIN Invoice i ON c.CustomerId = i.CustomerId
GROUP BY e.EmployeeId
ORDER BY avg_per_sale DESC;

-- ---- Monthly Revenue Trend ----
SELECT 
    strftime('%Y', i.InvoiceDate) as Year,
    strftime('%m', i.InvoiceDate) as Month,
    ROUND(SUM(i.Total), 2) as monthly_revenue,
    COUNT(DISTINCT i.InvoiceId) as total_orders
FROM Invoice i
GROUP BY Year, Month
ORDER BY Year, Month;
