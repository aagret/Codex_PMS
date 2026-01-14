# porfolios
SELECT positions.positiondate,
positions.bankname,
positions.clientnumber,
positions.basecurrency,
positions."position",
positions.assetprice,
positions.evaluation,
securities.name,
securities.ccy,
securities.assetclass,
securities.assettype,
securities.sector,
positions.evaluation / fx_cc.rate AS evaluation_cc,
positions.evaluation / fx_cc.rate * fx_chf.rate AS evaluation_chf
FROM positions
JOIN securities ON securities.isin = positions.isin
JOIN fxcrossrates fx_cc ON fx_cc.positioncurrency = positions.basecurrency AND fx_cc.quotecurrency = securities.ccy AND fx_cc.positiondate = positions.positiondate
JOIN fxcrossrates fx_chf ON fx_chf.positioncurrency = positions.basecurrency AND fx_chf.quotecurrency = 'CHF'::text AND fx_chf.positiondate = positions.positiondate
ORDER BY positions.positiondate DESC, positions.bankname, positions.clientnumber;

#total_client
SELECT portfolios.positiondate,
portfolios.bankname,
portfolios.clientnumber,
portfolios.basecurrency,
round(sum(portfolios.evaluation_chf)::numeric, 2) AS accountvalue_chf
FROM portfolios
GROUP BY portfolios.positiondate, portfolios.bankname, portfolios.clientnumber, portfolios.basecurrency
ORDER BY portfolios.positiondate DESC, portfolios.bankname, portfolios.clientnumber;


#total_client_assetclass
SELECT portfolios.positiondate,
portfolios.bankname,
portfolios.clientnumber,
portfolios.basecurrency,
portfolios.assetclass,
round(sum(portfolios.evaluation_chf)::numeric, 2) AS total_chf,
round(100::numeric * sum(portfolios.evaluation_chf::numeric) / sum(sum(portfolios.evaluation_chf::numeric)) OVER (PARTITION BY portfolios.positiondate, portfolios.clientnumber), 2) AS pct_of_client
FROM portfolios
GROUP BY portfolios.positiondate, portfolios.bankname, portfolios.clientnumber, portfolios.basecurrency, portfolios.assetclass
ORDER BY portfolios.positiondate DESC, portfolios.bankname, portfolios.clientnumber, (array_position(ARRAY['Cash'::text, 'Fixed Income'::text, 'Equity'::text, 'Alternative'::text, 'Commodity'::text], portfolios.assetclass));

#total_client_assetclass_type_sector
SELECT portfolios.positiondate,
portfolios.bankname,
portfolios.clientnumber,
portfolios.basecurrency,
portfolios.assetclass,
portfolios.assettype,
portfolios.sector,
round(sum(portfolios.evaluation_chf)::numeric, 2) AS total_chf,
round(100::numeric * sum(portfolios.evaluation_chf::numeric) / sum(sum(portfolios.evaluation_chf::numeric)) OVER (PARTITION BY portfolios.positiondate, portfolios.clientnumber), 2) AS pct_of_client
FROM portfolios
GROUP BY portfolios.positiondate, portfolios.bankname, portfolios.clientnumber, portfolios.basecurrency, portfolios.assetclass, portfolios.assettype, portfolios.sector
ORDER BY portfolios.positiondate DESC, portfolios.bankname, portfolios.clientnumber, portfolios.assetclass, (array_position(ARRAY['Cash'::text, 'Fixed Income'::text, 'Equity'::text, 'Alternative'::text, 'Commodity'::text], portfolios.assetclass));
