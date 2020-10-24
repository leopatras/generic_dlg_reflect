IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF T_customer
CONSTANT CUSTOM_ACTION = "Custom Action"

MAIN
  DEFINE a T_customers
  DEFINE sDA sDAdyn.T_SingleTableDA
  CALL utils.dbconnect()
  LET sDA.sqlAll = "SELECT * FROM CUSTOMER"
  LET sDA.initDA = FUNCTION initDA
  LET sDA.browseForm = "customers_singlerow"
  LET sDA.browseRecord = "scr"
  --LET sDA.filterInitially=TRUE
  CALL sDA.browseArray(reflect.Value.valueOf(a))
  --CALL da()
END MAIN

FUNCTION da()
  DEFINE a T_customers
  OPEN FORM f FROM "customers"
  DISPLAY FORM f
  DISPLAY ARRAY a TO scr.*
    BEFORE ROW
      DISPLAY "before row:", arr_curr()
    ON DELETE
      DISPLAY "on delete:", arr_curr()
    ON APPEND
      DISPLAY "on append:", arr_curr()
    ON UPDATE
      DISPLAY "on update:", arr_curr()
  END DISPLAY
END FUNCTION

PRIVATE FUNCTION initDA(sdi sDAdyn.I_SingleTableDA, d ui.Dialog) RETURNS()
  DISPLAY "init customer DA called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

FUNCTION (self T_customer) BeforeRow(d ui.Dialog, row INT) RETURNS()
  DISPLAY SFMT("customer BeforeRow:%1", row)
END FUNCTION

FUNCTION (self T_customer) AfterRow(d ui.Dialog, row INT) RETURNS()
  DISPLAY SFMT("customer AfterRow:%1", row)
END FUNCTION

FUNCTION (self T_customer) DeleteRow(d ui.Dialog, row INT) RETURNS()
  DISPLAY SFMT("customer DeleteRow:%1", row)
END FUNCTION

FUNCTION (self T_customer)
  onDAevent(
  d ui.Dialog, row INT, event STRING)
  RETURNS()
  DISPLAY SFMT("onDAevent ev:%1 row:%2", event, row)
  CASE event
    WHEN C_ON_ACTION || " " || CUSTOM_ACTION
      DISPLAY "custom action chosen"
  END CASE
END FUNCTION

FUNCTION (self T_customer) initINPUT(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
  DISPLAY "init customer INPUT called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

FUNCTION (self T_customer) BeforeField(d ui.Dialog, fieldName STRING) RETURNS()
  DISPLAY "customer BeforeField:", fieldName
END FUNCTION

--if this function is called the record member has the new value already (filled in by reflection)
FUNCTION (self T_customer)
  AfterField(
  d ui.Dialog, fieldName STRING, oldValue STRING)
  RETURNS STRING
  DISPLAY SFMT("AFTER FIELD field:%1 value:%2 oldValue:%3",
    fieldName, d.getFieldValue(fieldName), oldValue)
  CASE
    WHEN fieldName == "zipcode" AND LENGTH(self.zipcode) <> 5
      DISPLAY "zipcode:'", self.zipcode, "'"
      RETURN "Zipcode must have 5 digits"
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION (self T_customer)
  onINPUTevent(
  d ui.Dialog, ev STRING, fieldName STRING, value STRING)
  RETURNS(BOOLEAN, STRING)
  DISPLAY SFMT("onINPUTevent ev:%1 field:%2 value:%3", ev, fieldName, value)
  CASE ev
    WHEN C_ON_ACTION || " " || CUSTOM_ACTION
      DISPLAY "custom action chosen"
      RETURN TRUE, NULL
  END CASE
  RETURN FALSE, NULL
END FUNCTION

FUNCTION (self T_customer) insertOrUpdate(update BOOLEAN)
  IF update THEN
    UPDATE customer
      SET customer.* = self.*
      WHERE @customer_num = self.customer_num
  ELSE
    INSERT INTO customer VALUES self.*
  END IF
END FUNCTION
