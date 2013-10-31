CREATE OR REPLACE VIEW api.issuetoshipping
AS 
  SELECT
    'SO'::varchar AS order_type,
    CURRENT_DATE AS transaction_date,
    cohead_number AS order_number,
    coitem_linenumber AS line_number,
    (coitem_qtyord - coitem_qtyshipped + coitem_qtyreturned - qtyAtShipping(coitem_id)) AS quantity,
    'N/A'::varchar AS location,
    'N/A'::varchar AS lotserial,
    FALSE AS force
  FROM cohead JOIN coitem ON (coitem_cohead_id=cohead_id)
  WHERE (coitem_status NOT IN ('C','X'));

GRANT ALL ON TABLE api.issuetoshipping TO xtrole;
COMMENT ON VIEW api.issuetoshipping IS 'Issue to Shipping with distribution';

CREATE OR REPLACE FUNCTION api.IssueToShippingTrans(api.issuetoshipping) RETURNS INTEGER AS $$
-- Copyright (c) 1999-2013 by OpenMFG LLC, d/b/a xTuple. 
-- See www.xtuple.com/CPAL for the full text of the software license.
DECLARE
  pNEW ALIAS FOR $1;
  _sohead RECORD;
  _soitem RECORD;
  _period RECORD;
  _location RECORD;
  _itemloc RECORD;
  _forcetrans BOOLEAN := FALSE;
  _forceqty NUMERIC := 0.0;
  _itemlocSeries INTEGER := 0;
  _itemlocdistid INTEGER := 0;
  _lotitemlocdistid INTEGER := 0;
  _result INTEGER;

BEGIN

  -- edits
  -- order not found
  SELECT cohead_id, cohead_status, cohead_holdtype
    INTO _sohead
  FROM cohead
  WHERE (cohead_number=pNEW.order_number);
  IF (NOT FOUND) THEN
    RETURN -10;
  END IF;
  IF (_sohead.cohead_status = 'C') THEN
    RETURN -11;
  END IF;
  IF (_sohead.cohead_status = 'X') THEN
    RETURN -12;
  END IF;
  IF (_sohead.cohead_holdtype = 'C') THEN
    RETURN -50;
  END IF;
  IF (_sohead.cohead_holdtype = 'R') THEN
    RETURN -53;
  END IF;
  IF (_sohead.cohead_holdtype = 'P') THEN
    RETURN -57;
  END IF;

  -- order line not found
  SELECT coitem_id, coitem_status,
         (coitem_qtyord - coitem_qtyshipped + coitem_qtyreturned - qtyAtShipping(coitem_id)) AS balance,
         itemsite_id, itemsite_warehous_id, itemsite_loccntrl, itemsite_controlmethod,
         itemsite_qtyonhand, itemsite_costmethod
    INTO _soitem
  FROM coitem JOIN itemsite ON (itemsite_id=coitem_itemsite_id)
  WHERE (coitem_cohead_id=_sohead.cohead_id)
    AND (coitem_linenumber=pNEW.line_number);
  IF (NOT FOUND) THEN
    RETURN -20;
  END IF;
  IF (_soitem.coitem_status = 'C') THEN
    RETURN -21;
  END IF;
  IF (_soitem.coitem_status = 'X') THEN
    RETURN -22;
  END IF;

  -- item not found ???

  -- closed or frozen period
  SELECT * INTO _period
  FROM period
  WHERE ((pNEW.transaction_date) between period_start AND period_end);
  IF (NOT FOUND) THEN
    RETURN -40;
  END IF;
  IF (_period.period_closed) THEN
    RETURN -41;
  END IF;
  IF (_period.period_freeze) THEN
    RETURN -42;
  END IF;
  
  -- find itemloc
  IF (_soitem.itemsite_loccntrl) THEN
    -- location not found
    SELECT *
    INTO _location
    FROM location
    WHERE (location_warehous_id=_soitem.itemsite_warehous_id)
      AND (formatLocationName(location_id)=pNEW.location);
    IF (NOT FOUND) THEN
      RETURN -80;
    END IF;
    SELECT *
    INTO _itemloc
    FROM itemloc
    WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
      AND (formatLocationName(itemloc_location_id)=pNEW.location);
    IF (NOT FOUND) THEN
      IF (NOT pNEW.force) THEN
        RETURN -80;
      ELSE
        _forcetrans := TRUE;
      END IF;
    END IF;
  END IF;
  IF (_soitem.itemsite_controlmethod IN ('S','L')) THEN
    -- lotserial not found
    SELECT *
    INTO _itemloc
    FROM itemloc JOIN ls ON (ls_id=itemloc_ls_id)
    WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
      AND (UPPER(ls_number)=UPPER(pNEW.lotserial));
    IF (NOT FOUND) THEN
      IF (NOT pNEW.force) THEN
        RETURN -90;
      ELSE
        _forcetrans := TRUE;
      END IF;
    END IF;
  END IF;
  IF ( (_soitem.itemsite_loccntrl) AND (_soitem.itemsite_controlmethod IN ('S','L')) ) THEN
    -- location/lotserial not found
    SELECT *
    INTO _itemloc
    FROM itemloc JOIN ls ON (ls_id=itemloc_ls_id)
    WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
      AND (formatLocationName(itemloc_location_id)=pNEW.location)
      AND (UPPER(ls_number)=UPPER(pNEW.lotserial));
    IF (NOT FOUND) THEN
      IF (NOT pNEW.force) THEN
        RETURN -100;
      ELSE
        _forcetrans := TRUE;
      END IF;
    END IF;
  END IF;

  IF ( (_soitem.itemsite_loccntrl) OR (_soitem.itemsite_controlmethod IN ('S','L')) ) THEN
    -- location/lotserial quantity
    IF (COALESCE(_itemloc.itemloc_qty, 0.0) < pNEW.quantity) THEN
      IF (NOT pNEW.force) THEN
        RETURN -110;
      ELSE
        _forcetrans := TRUE;
        _forceqty := (pNEW.quantity - COALESCE(_itemloc.itemloc_qty, 0.0));
      END IF;
    ELSE
      _forceqty := pNEW.quantity;
    END IF;
  ELSEIF (_soitem.itemsite_costmethod = 'A') THEN
    -- itemsite quantity
    IF (COALESCE(_soitem.itemsite_qtyonhand, 0.0) < pNEW.quantity) THEN
      IF (NOT pNEW.force) THEN
        RETURN -110;
      ELSE
        _forcetrans := TRUE;
        _forceqty := (pNEW.quantity - _soitem.itemsite_qtyonhand);
      END IF;
    ELSE
      _forceqty := pNEW.quantity;
    END IF;
  END IF;

  -- force adjustment trans
  IF (_forcetrans) THEN
    SELECT invAdjustment(_soitem.itemsite_id, _forceqty,
                         (pNEW.order_number || '-' || pNEW.line_number), 'IssueToShippingForce')
           INTO _itemlocSeries;
    IF (_itemlocSeries < 0) THEN
      RAISE EXCEPTION 'invAdjustment failed, result=%', _itemlocSeries;
    END IF;

    IF ( (_soitem.itemsite_loccntrl) OR (_soitem.itemsite_controlmethod IN ('S','L')) ) THEN
      -- find itemlocdist_id
      SELECT itemlocdist_id INTO _itemlocdistid
      FROM itemlocdist
      WHERE (itemlocdist_series=_itemlocSeries)
        AND (itemlocdist_source_type='O');
      IF (NOT FOUND) THEN
        RAISE EXCEPTION 'cannot find adjustment itemlocdist, itemlocseries=%', _itemlocSeries;
      END IF;

      IF (_soitem.itemsite_controlmethod IN ('S','L')) THEN
        -- create lotserial
        SELECT createLotserial(_soitem.itemsite_id, pNEW.lotserial,
                               _itemlocSeries, 'I', NULL,
                               _itemlocdistid, _forceqty,
                               endOfTime(), endOfTime()) INTO _lotitemlocdistid;

        UPDATE itemlocdist SET itemlocdist_source_type='O'
        WHERE (itemlocdist_series=_itemlocSeries);

        DELETE FROM itemlocdist
        WHERE (itemlocdist_id=_itemlocdistid);

        _itemlocdistid := _lotitemlocdistid;
      END IF;

      -- distribute
      IF (_soitem.itemsite_loccntrl) THEN
        INSERT INTO itemlocdist(itemlocdist_itemlocdist_id,
                                itemlocdist_source_type,
                                itemlocdist_source_id,
                                itemlocdist_qty,
                                itemlocdist_expiration)
        SELECT _itemlocdistid,
               'L',
               location_id,
               _forceqty,
               endOfTime()
        FROM location
        WHERE (formatLocationName(location_id)=pNEW.location);
      ELSE
        INSERT INTO itemlocdist(itemlocdist_itemlocdist_id,
                                itemlocdist_source_type,
                                itemlocdist_source_id,
                                itemlocdist_qty,
                                itemlocdist_expiration)
        VALUES (_itemlocdistid,
               'L',
               -1,
               _forceqty,
               endOfTime());
      END IF;

      -- post distributions
      PERFORM distributeToLocations(_itemlocdistid);
    END IF;

    PERFORM postItemlocSeries(_itemlocSeries);

    -- find itemloc
    IF (_soitem.itemsite_loccntrl) THEN
      SELECT *
      INTO _itemloc
      FROM itemloc
      WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
        AND (formatLocationName(itemloc_location_id)=pNEW.location);
      IF (NOT FOUND) THEN
        RAISE EXCEPTION 'cannot find forced itemloc';
      END IF;
    END IF;
    IF (_soitem.itemsite_controlmethod IN ('S','L')) THEN
      SELECT *
      INTO _itemloc
      FROM itemloc JOIN ls ON (ls_id=itemloc_ls_id)
      WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
        AND (UPPER(ls_number)=UPPER(pNEW.lotserial));
      IF (NOT FOUND) THEN
        RAISE EXCEPTION 'cannot find forced itemloc';
      END IF;
    END IF;
    IF ( (_soitem.itemsite_loccntrl) AND (_soitem.itemsite_controlmethod IN ('S','L')) ) THEN
      SELECT *
      INTO _itemloc
      FROM itemloc JOIN ls ON (ls_id=itemloc_ls_id)
      WHERE (itemloc_itemsite_id=_soitem.itemsite_id)
        AND (formatLocationName(itemloc_location_id)=pNEW.location)
        AND (UPPER(ls_number)=UPPER(pNEW.lotserial));
      IF (NOT FOUND) THEN
        RAISE EXCEPTION 'cannot find forced itemloc';
      END IF;
    END IF;

  END IF;

  -- issue to shipping
  SELECT issueToShipping(pNEW.order_type, _soitem.coitem_id,
                         pNEW.quantity, 0, pNEW.transaction_date::TIMESTAMP)
         INTO _itemlocSeries;
  IF (_itemlocSeries < 0) THEN
    RAISE EXCEPTION 'issueToShipping failed, result=%', _itemlocSeries;
  END IF;

  IF ( (_soitem.itemsite_loccntrl) OR (_soitem.itemsite_controlmethod IN ('S','L')) ) THEN
    -- find itemlocdist_id
    SELECT itemlocdist_id INTO _itemlocdistid
    FROM itemlocdist
    WHERE (itemlocdist_series=_itemlocSeries)
      AND (itemlocdist_source_type='O');
    IF (NOT FOUND) THEN
      RAISE EXCEPTION 'cannot find itemlocdist, itemlocseries=%', _itemlocSeries;
    END IF;

    -- distribute
    INSERT INTO itemlocdist(itemlocdist_itemlocdist_id,
                            itemlocdist_source_type,
                            itemlocdist_source_id,
                            itemlocdist_qty,
                            itemlocdist_expiration)
    VALUES (_itemlocdistid,
            'I',
            _itemloc.itemloc_id,
            (pNEW.quantity * -1.0),
            endOfTime());

    -- post distributions
    PERFORM distributeToLocations(_itemlocdistid);
  END IF;

  PERFORM postItemlocSeries(_itemlocSeries);

  RETURN 0;
END;
$$ LANGUAGE 'plpgsql';


--Rules

CREATE OR REPLACE RULE "_INSERT" AS
    ON INSERT TO api.issuetoshipping DO INSTEAD  SELECT api.issuetoshippingtrans(new.*) AS issuetoshippingtrans;

CREATE OR REPLACE RULE "_UPDATE" AS 
    ON UPDATE TO api.issuetoshipping DO INSTEAD

  NOTHING;
           
CREATE OR REPLACE RULE "_DELETE" AS 
    ON DELETE TO api.issuetoshipping DO INSTEAD

  NOTHING;
