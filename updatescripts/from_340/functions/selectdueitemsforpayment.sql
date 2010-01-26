CREATE OR REPLACE FUNCTION selectDueItemsForPayment(INTEGER, INTEGER) RETURNS INTEGER AS '
DECLARE
  pVendid ALIAS FOR $1;
  pBankaccntid ALIAS FOR $2;
  _currid INTEGER;
  _r RECORD;

BEGIN

  SELECT bankaccnt_curr_id INTO _currid
  FROM bankaccnt
  WHERE (bankaccnt_id=pBankaccntid);

  FOR _r IN SELECT apopen_id
              FROM apopen
             WHERE((apopen_duedate <= CURRENT_DATE)
               AND (apopen_open)
               AND (apopen_status = ''O'')
               AND (apopen_doctype IN (''V'', ''D''))
               AND (apopen_vend_id=pVendid)
               AND (apopen_curr_id=_currid) ) LOOP
    PERFORM selectPayment(_r.apopen_id, pBankaccntid);
  END LOOP;

  RETURN 1;

END;
' LANGUAGE 'plpgsql';