BEGIN;

  --Customer View

  DROP VIEW api.customer;
  CREATE OR REPLACE VIEW api.customer AS
 
  SELECT 
    cust_number::varchar(100) AS customer_number,
    custtype_code AS customer_type,
    cust_name AS customer_name,
    cust_active AS active,
    salesrep_number AS sales_rep,
    cust_commprcnt * 100 AS commission,
    cust_shipvia AS ship_via,
    shipform_name AS ship_form,
    shipchrg_name AS shipping_charges,
    cust_backorder AS accepts_backorders,
    cust_partialship AS accepts_partial_shipments,
    cust_ffshipto AS allow_free_form_shipto,
    cust_ffbillto AS allow_free_form_billto,
    warehous_code AS preferred_selling_whs,
    taxauth_code AS default_tax_authority,
    terms_code AS default_terms,
    CASE 
      WHEN cust_balmethod='B' THEN
       'Balance Forward'
      ELSE
       'Open Item'
      END AS balance_method,
    cust_discntprcnt AS default_discount,
    dc.curr_abbr AS default_currency,
    clc.curr_abbr AS credit_limit_currency,
    cust_creditlmt AS credit_limit,
    cust_creditrating AS credit_rating,
    CASE
      WHEN (cust_creditstatus = 'G') THEN
        'In Good Standing'
      WHEN (cust_creditstatus = 'W') THEN
        'On Credit Warning'
      ELSE
        'On Credit Hold'
    END AS credit_status,
    cust_autoupdatestatus AS credit_status_exceed_warn,
    cust_autoholdorders AS credit_status_exceed_hold,
    cust_usespos AS uses_purchase_orders,
    cust_blanketpos AS uses_blanket_pos,
    mc.cntct_number AS billing_contact_number,
    mc.cntct_honorific AS billing_contact_honorific,
    mc.cntct_first_name AS billing_contact_first,
    mc.cntct_last_name AS billing_contact_last,
    mc.cntct_title AS billing_contact_job_title,
    mc.cntct_phone AS billing_contact_voice,
    mc.cntct_phone2 AS billing_contact_alternate,
    mc.cntct_fax AS billing_contact_fax,
    mc.cntct_email AS billing_contact_email,
    mc.cntct_webaddr AS billing_contact_web,
    (''::TEXT) AS billing_contact_change,
    m.addr_number AS billing_contact_address_number,
    m.addr_line1 AS billing_contact_address1,
    m.addr_line2 AS billing_contact_address2,
    m.addr_line3 AS billing_contact_address3,
    m.addr_city AS billing_contact_city,
    m.addr_state AS billing_contact_state,
    m.addr_postalcode AS billing_contact_postalcode,
    m.addr_country AS billing_contact_country,
    (''::TEXT) AS billing_contact_address_change,
    cc.cntct_number AS correspond_contact_number,
    cc.cntct_honorific AS correspond_contact_honorific,
    cc.cntct_first_name AS correspond_contact_first,
    cc.cntct_last_name AS correspond_contact_last,
    cc.cntct_title AS correspond_contact_job_title,
    cc.cntct_phone AS correspond_contact_voice,
    cc.cntct_phone2 AS correspond_contact_alternate,
    cc.cntct_fax AS correspond_contact_fax,
    cc.cntct_email AS correspond_contact_email,
    cc.cntct_webaddr AS correspond_contact_web,
    (''::TEXT) AS correspond_contact_change,
    c.addr_number AS correspond_contact_address_number,
    c.addr_line1 AS correspond_contact_address1,
    c.addr_line2 AS correspond_contact_address2,
    c.addr_line3 AS correspond_contact_address3,
    c.addr_city AS correspond_contact_city,
    c.addr_state AS correspond_contact_state,
    c.addr_postalcode AS correspond_contact_postalcode,
    c.addr_country AS correspond_contact_country,
    (''::TEXT) AS correspond_contact_address_change,
    cust_comments AS notes,
    CASE 
      WHEN (cust_soediprofile_id IS NOT NULL) THEN
        sedi.ediprofile_name
      WHEN (cust_soemaildelivery) THEN
        'Custom Email'
      ELSE
        'No EDI'
    END AS so_edi_profile,
    cust_soediemail AS so_edi_email,
    cust_soedicc AS so_edi_cc,
    cust_soedisubject AS so_edi_subject,
    cust_soedifilename AS so_edi_filename,
    cust_soediemailbody AS so_edi_emailbody,
    CASE 
      WHEN (cust_ediprofile_id IS NOT NULL) THEN
        iedi.ediprofile_name
      WHEN (cust_emaildelivery) THEN
        'Custom Email'
      ELSE
        'No EDI'
    END AS invc_edi_profile,
    cust_ediemail AS invc_edi_email,
    cust_edicc AS invc_edi_cc,
    cust_edisubject AS invc_edi_subject,
    cust_edifilename AS invc_edi_filename,
    cust_ediemailbody AS invc_edi_emailbody           
  FROM
    custinfo
      LEFT OUTER JOIN shipchrg ON (cust_shipchrg_id=shipchrg_id)
      LEFT OUTER JOIN whsinfo ON (cust_preferred_warehous_id=warehous_id)
      LEFT OUTER JOIN ediprofile sedi ON (cust_ediprofile_id=sedi.ediprofile_id)
      LEFT OUTER JOIN ediprofile iedi ON (cust_ediprofile_id=iedi.ediprofile_id)
      LEFT OUTER JOIN cntct mc ON (cust_cntct_id=mc.cntct_id)
      LEFT OUTER JOIN addr m ON (mc.cntct_addr_id=m.addr_id)
      LEFT OUTER JOIN cntct cc ON (cust_corrcntct_id=cc.cntct_id)
      LEFT OUTER JOIN addr c ON (cc.cntct_addr_id=c.addr_id)
      LEFT OUTER JOIN taxauth ON (cust_taxauth_id=taxauth_id),
    custtype,salesrep,shipform,
    curr_symbol dc, curr_symbol clc, terms
  WHERE ((cust_custtype_id=custtype_id)
  AND (cust_salesrep_id=salesrep_id)
  AND (cust_shipform_id=shipform_id)
  AND (cust_curr_id=dc.curr_id)
  AND (cust_creditlmt_curr_id=clc.curr_id)
  AND (cust_terms_id=terms_id));

GRANT ALL ON TABLE api.customer TO openmfg;
COMMENT ON VIEW api.customer IS 'Customer';

--Rules

CREATE OR REPLACE RULE "_INSERT" AS
    ON INSERT TO api.customer DO INSTEAD

INSERT INTO custinfo
	(cust_active,
	cust_custtype_id,
        cust_salesrep_id,
        cust_commprcnt,
        cust_name,
        cust_creditlmt,
  	cust_creditrating,
  	cust_backorder,
  	cust_partialship,
  	cust_terms_id,
	cust_discntprcnt,
  	cust_balmethod,
  	cust_ffshipto,
  	cust_shipform_id,
  	cust_shipvia,
  	cust_blanketpos,
  	cust_shipchrg_id,
  	cust_creditstatus,
  	cust_comments,
        cust_ffbillto,
        cust_usespos,
        cust_number,
        cust_emaildelivery,
        cust_ediemail,
        cust_edisubject,
        cust_edifilename,
        cust_ediemailbody,
        cust_autoupdatestatus,
        cust_autoholdorders,
        cust_edicc,
        cust_ediprofile_id,
        cust_preferred_warehous_id,
        cust_curr_id,
        cust_creditlmt_curr_id,
        cust_cntct_id,
        cust_corrcntct_id,
        cust_taxauth_id,
        cust_soemaildelivery,
        cust_soediemail,
        cust_soedisubject,
        cust_soedifilename,
        cust_soediemailbody,
        cust_soedicc,
        cust_soediprofile_id )
        VALUES (
	COALESCE(NEW.active,true),
	COALESCE(getcusttypeid(NEW.customer_type),FetchMetricValue('DefaultCustType')),
        COALESCE(getSalesRepId(NEW.sales_rep),FetchMetricValue('DefaultSalesRep')),
        COALESCE(NEW.commission * .01,(
          SELECT salesrep_commission
          FROM salesrep
          WHERE (salesrep_id=getSalesRepId(NEW.sales_rep)))),
        COALESCE(NEW.customer_name,''),
        COALESCE(NEW.credit_limit,FetchMetricValue('SOCreditLimit')),	
        COALESCE(NEW.credit_rating,FetchMetricText('SOCreditRate')),
        COALESCE(NEW.accepts_backorders,FetchMetricBool('DefaultBackOrders'),false),
        COALESCE(NEW.accepts_partial_shipments,FetchMetricBool('DefaultPartialShipments'::text),false),
        COALESCE(getTermsId(NEW.default_terms),FetchMetricValue('DefaultTerms')),
        COALESCE(NEW.default_discount,0),
	CASE 
	  WHEN NEW.balance_method='Balance Forward' THEN
	    'B'
	  WHEN NEW.balance_method='Open Items' THEN
	    'O'
          ELSE
            COALESCE(FetchMetricText('DefaultBalanceMethod'),'B')
	END,
        COALESCE(NEW.allow_free_form_shipto,FetchMetricBool('DefaultFreeFormShiptos'),false),
        COALESCE(getShipFormId(NEW.ship_form),FetchMetricValue('DefaultShipFormId')),
        COALESCE(NEW.ship_via,FetchDefaultShipVia()),
        COALESCE(NEW.uses_blanket_pos,false),
        COALESCE(getShipChrgId(NEW.shipping_charges),-1),
        CASE
	  WHEN (NEW.credit_status = 'On Credit Warning') THEN
	   'W'
	  WHEN (NEW.credit_status = 'On Credit Hold') THEN
	   'H'
         ELSE
           'G'
	END,
        COALESCE(NEW.notes,''),
        COALESCE(NEW.allow_free_form_billto,false),
        COALESCE(NEW.uses_purchase_orders,false),
        UPPER(NEW.customer_number),
        CASE
	  WHEN (NEW.invc_edi_profile='Custom Email') THEN
	    true
	  ELSE
	    false
        END,
        COALESCE(NEW.invc_edi_email,''),
        COALESCE(NEW.invc_edi_subject,''),
        COALESCE(NEW.invc_edi_filename,''),
        COALESCE(NEW.invc_edi_emailbody,''),	
        COALESCE(NEW.credit_status_exceed_warn,false),
        COALESCE(NEW.credit_status_exceed_hold,false),
        COALESCE(NEW.invc_edi_cc,''),
        CASE WHEN (NEW.invc_edi_profile IN ('Custom Email','No EDI')) THEN
          NULL
        ELSE
          getEdiProfileId(NEW.invc_edi_profile)
        END,
        COALESCE(getWarehousId(NEW.preferred_selling_whs,'ACTIVE'),-1),
        COALESCE(getCurrId(NEW.default_currency),basecurrid()),
        COALESCE(getCurrID(NEW.credit_limit_currency),basecurrid()),
        saveCntct(
          getCntctId(NEW.billing_contact_number),
          NEW.billing_contact_number,
          saveAddr(
            getAddrId(NEW.billing_contact_address_number),
            NEW.billing_contact_address_number,
            NEW.billing_contact_address1,
            NEW.billing_contact_address2,
            NEW.billing_contact_address3,
            NEW.billing_contact_city,
            NEW.billing_contact_state,
            NEW.billing_contact_postalcode,
            NEW.billing_contact_country,
            NEW.billing_contact_address_change),
          NEW.billing_contact_first,
          NEW.billing_contact_last,
          NEW.billing_contact_honorific,
          NEW.billing_contact_voice,
          NEW.billing_contact_alternate,
          NEW.billing_contact_fax,
          NEW.billing_contact_email,
          NEW.billing_contact_web,
          NEW.billing_contact_job_title,
          NEW.billing_contact_change
          ),
        saveCntct(
          getCntctId(NEW.correspond_contact_number),
          NEW.correspond_contact_number,
          saveAddr(
            getAddrId(NEW.correspond_contact_address_number),
            NEW.correspond_contact_address_number,
            NEW.correspond_contact_address1,
            NEW.correspond_contact_address2,
            NEW.correspond_contact_address3,
            NEW.correspond_contact_city,
            NEW.correspond_contact_state,
            NEW.correspond_contact_postalcode,
            NEW.correspond_contact_country,
            NEW.correspond_contact_address_change),
          NEW.correspond_contact_first,
          NEW.correspond_contact_last,
          NEW.correspond_contact_honorific,
          NEW.correspond_contact_voice,
          NEW.correspond_contact_alternate,
          NEW.correspond_contact_fax,
          NEW.correspond_contact_email,
          NEW.correspond_contact_web,
          NEW.correspond_contact_job_title,
          NEW.correspond_contact_change
          ),
        getTaxAuthId(NEW.default_tax_authority),
        CASE
	  WHEN (NEW.so_edi_profile='Custom Email') THEN
	    true
	  ELSE
	    false
        END,
        COALESCE(NEW.so_edi_email,''),
        COALESCE(NEW.so_edi_subject,''),
        COALESCE(NEW.so_edi_filename,''),
        COALESCE(NEW.so_edi_emailbody,''),
        COALESCE(NEW.so_edi_cc,''),
        CASE WHEN (NEW.so_edi_profile IN ('Custom Email','No EDI')) THEN
          NULL
        ELSE
          getEdiProfileId(NEW.so_edi_profile)
        END
         );

CREATE OR REPLACE RULE "_UPDATE" AS
    ON UPDATE TO api.customer DO INSTEAD

UPDATE custinfo SET
	cust_active=NEW.active,
	cust_custtype_id=getCustTypeId(NEW.customer_type),
        cust_salesrep_id=getSalesRepId(NEW.sales_rep),
        cust_commprcnt=NEW.commission * .01,
        cust_name=NEW.customer_name,
        cust_creditlmt=NEW.credit_limit,
  	cust_creditrating=NEW.credit_rating,
  	cust_backorder=NEW.accepts_backorders,
  	cust_partialship=NEW.accepts_partial_shipments,
  	cust_terms_id=getTermsId(NEW.default_terms),
	cust_discntprcnt=NEW.default_discount,
  	cust_balmethod=
	  CASE 
	    WHEN NEW.balance_method='Balance Forward' THEN
	      'B'
	    WHEN NEW.balance_method='Open Items' THEN
	      'O'
            ELSE
              NULL
	  END,
  	cust_ffshipto=NEW.allow_free_form_shipto,
  	cust_shipform_id=getShipFormId(NEW.ship_form),
  	cust_shipvia=NEW.ship_via,
  	cust_blanketpos=NEW.uses_blanket_pos,
  	cust_shipchrg_id=COALESCE(getShipChrgId(NEW.shipping_charges),-1),
  	cust_creditstatus=
          CASE
	    WHEN (NEW.credit_status = 'On Credit Warning') THEN
	      'W'
	    WHEN (NEW.credit_status = 'On Credit Hold') THEN
	      'H'
            ELSE
              'G'
        END,
  	cust_comments=NEW.notes,
        cust_ffbillto=NEW.allow_free_form_billto,
        cust_usespos=NEW.uses_purchase_orders,
        cust_number=NEW.customer_number,
        cust_emaildelivery=
          CASE
	    WHEN (NEW.invc_edi_profile='Custom Email') THEN
	      true
	    ELSE
	      false
          END,
        cust_ediemail=NEW.invc_edi_email,
        cust_edisubject=NEW.invc_edi_subject,
        cust_edifilename=NEW.invc_edi_filename,
        cust_ediemailbody=NEW.invc_edi_emailbody,
        cust_autoupdatestatus=NEW.credit_status_exceed_warn,
        cust_autoholdorders=NEW.credit_status_exceed_hold,
        cust_edicc=NEW.invc_edi_cc,
        cust_ediprofile_id=
          CASE WHEN (NEW.invc_edi_profile IN ('Custom Email','No EDI')) THEN
            NULL
          ELSE
            getEdiProfileId(NEW.invc_edi_profile)
          END,
        cust_preferred_warehous_id=COALESCE(getWarehousId(NEW.preferred_selling_whs,'ACTIVE'),-1),
        cust_curr_id=getCurrId(NEW.default_currency),
        cust_creditlmt_curr_id=getCurrId(NEW.credit_limit_currency),
        cust_cntct_id=saveCntct(
          getCntctId(NEW.billing_contact_number),
          NEW.billing_contact_number,
          saveAddr(
            getAddrId(NEW.billing_contact_address_number),
            NEW.billing_contact_address_number,
            NEW.billing_contact_address1,
            NEW.billing_contact_address2,
            NEW.billing_contact_address3,
            NEW.billing_contact_city,
            NEW.billing_contact_state,
            NEW.billing_contact_postalcode,
            NEW.billing_contact_country,
            NEW.billing_contact_address_change),
          NEW.billing_contact_first,
          NEW.billing_contact_last,
          NEW.billing_contact_honorific,
          NEW.billing_contact_voice,
          NEW.billing_contact_alternate,
          NEW.billing_contact_fax,
          NEW.billing_contact_email,
          NEW.billing_contact_web,
          NEW.billing_contact_job_title,
          NEW.billing_contact_change
          ),
        cust_corrcntct_id=saveCntct(
          getCntctId(NEW.correspond_contact_number),
          NEW.correspond_contact_number,
          saveAddr(
            getAddrId(NEW.correspond_contact_address_number),
            NEW.correspond_contact_address_number,
            NEW.correspond_contact_address1,
            NEW.correspond_contact_address2,
            NEW.correspond_contact_address3,
            NEW.correspond_contact_city,
            NEW.correspond_contact_state,
            NEW.correspond_contact_postalcode,
            NEW.correspond_contact_country,
            NEW.correspond_contact_address_change),
          NEW.correspond_contact_first,
          NEW.correspond_contact_last,
          NEW.correspond_contact_honorific,
          NEW.correspond_contact_voice,
          NEW.correspond_contact_alternate,
          NEW.correspond_contact_fax,
          NEW.correspond_contact_email,
          NEW.correspond_contact_web,
          NEW.correspond_contact_job_title,
          NEW.correspond_contact_change
          ),
        cust_taxauth_id=getTaxAuthId(NEW.default_tax_authority),
        cust_soemaildelivery=
          CASE
	    WHEN (NEW.so_edi_profile='Custom Email') THEN
	      true
	    ELSE
	      false
          END,
        cust_soediemail=NEW.so_edi_email,
        cust_soedisubject=NEW.so_edi_subject,
        cust_soedifilename=NEW.so_edi_filename,
        cust_soediemailbody=NEW.so_edi_emailbody,
        cust_soedicc=NEW.so_edi_cc,
        cust_soediprofile_id=
          CASE WHEN (NEW.so_edi_profile IN ('Custom Email','No EDI')) THEN
           NULL
          ELSE
            getEdiProfileId(NEW.so_edi_profile)
          END
        WHERE cust_id=getCustId(OLD.customer_number);

CREATE OR REPLACE RULE "_DELETE" AS
    ON DELETE TO api.customer DO INSTEAD

SELECT deleteCustomer(getCustId(OLD.customer_number));

COMMIT;
