CREATE OR REPLACE FUNCTION hasPriv(TEXT) RETURNS BOOLEAN AS $$
DECLARE
  pPrivName	ALIAS FOR $1;
  _result       INTEGER;
  _returnVal	BOOLEAN;

BEGIN
  RAISE NOTICE 'hasPriv(TEXT) is deprecated. Use checkPrivilege(TEXT) instead.';
  SELECT priv_id INTO _result
    FROM priv, grppriv, usrgrp
   WHERE((usrgrp_grp_id=grppriv_grp_id)
     AND (grppriv_priv_id=priv_id)
     AND (priv_name=pPrivName)
     AND (usrgrp_username=CURRENT_USER));
  IF (FOUND) THEN
    RETURN true;
  END IF;

  SELECT COALESCE(usrpriv_id, 0) != 0 INTO _returnVal
  FROM priv LEFT OUTER JOIN
       usrpriv ON (priv_id=usrpriv_priv_id AND usrpriv_username = CURRENT_USER)
  WHERE (priv_name=pPrivName);
  IF (_returnVal IS NULL) THEN
    _returnVal := FALSE;
  END IF;

  RETURN _returnVal;
END;
$$ LANGUAGE 'plpgsql';
