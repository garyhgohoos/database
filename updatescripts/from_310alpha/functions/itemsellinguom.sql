CREATE OR REPLACE FUNCTION itemSellingUOM(INTEGER) RETURNS TEXT AS '
DECLARE
  pItemid ALIAS FOR $1;

BEGIN
  RETURN itemUOMByType(pItemid, ''Selling'');
END;
' LANGUAGE 'plpgsql';
