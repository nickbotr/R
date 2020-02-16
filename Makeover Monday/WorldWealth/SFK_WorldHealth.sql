SELECT COUNTRY, REGION, WEALTH
FROM TEST_DB."PUBLIC".WORLD_WEALTH;

--Always use '/' instead of '\'. Compiler can't read.
PUT 'FILE://D:/Repos/R/Makeover Monday/WorldWealth/Modified_Data.csv' @TEDDY;