select DATE_TRUNC('MONTH', "date") AS MONTH, SUM("unit_sales") AS UNITS
from ANALYTICS.SALES."fct_costco_sales"
where "party_id" = '8316'
and "item_number" = '1561882'
and "date" >= '2022-08-01'
group by DATE_TRUNC('MONTH', "date")