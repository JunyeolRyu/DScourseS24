#(a)
CREATE TABLE FL_insurance(
  "var1", "var2", "var3","var4", "var5", "var6", "var7", "var8", "var9", "var10", "var11", "var12", "var13", "var14", "var15", "var16", "var17", "var18");

.mode csv
.import FL_insurnace_sample.csv FL_insurance

#(b)
 sqlite> SELECT * FROM FL_insurance LIMIT 10;

#(c)
SELECT DISTINT var3 FROM FL_insurance


  
#(d)
SELECT AVG(var9 - var8) AS average_difference
FROM FL_insurance;
  
#(e)
SELECT var17, COUNT(*) AS frequency
FROM FL_insurance
GROUP BY var17
ORDER BY frequency DESC;
