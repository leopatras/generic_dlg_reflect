IMPORT reflect
IMPORT FGL utils
IMPORT FGL sql2array
IMPORT FGL sDAdyn
SCHEMA stores
TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF T_customer


DEFINE a T_customers
MAIN
  DEFINE customers sql2array.T_customers
  CALL utils.dbconnect()
  CALL sql2array.readIntoArray(
    reflect.Value.valueOf(a), "select * from customer")
  CALL sDAdyn.browseArray(
    reflect.Value.valueOf(a), "scr", "customers_singlerow" {customers},FUNCTION initDA)
  --CALL da()
END MAIN

FUNCTION da()
  OPEN FORM f FROM "customers"
  DISPLAY FORM f
  DISPLAY ARRAY a TO scr.*
    BEFORE ROW
      DISPLAY "before row:",arr_curr()
    ON DELETE
      DISPLAY "on delete:",arr_curr()
    ON APPEND
      DISPLAY "on append:",arr_curr()
    ON UPDATE
      DISPLAY "on update:",arr_curr()
  END DISPLAY
END FUNCTION

FUNCTION test( x T_customer)
END FUNCTION

PRIVATE FUNCTION initDA(d ui.Dialog) RETURNS()
  DISPLAY "init customer DA called"
  CALL d.addTrigger("ON ACTION custom_action")
END FUNCTION

FUNCTION (self T_customer) BeforeRow(d ui.Dialog,row INT) RETURNS()
  DISPLAY sfmt("customer BeforeRow:%1",row)
END FUNCTION

FUNCTION (self T_customer) AfterRow(d ui.Dialog,row INT) RETURNS()
  DISPLAY sfmt("customer AfterRow:%1",row)
END FUNCTION

FUNCTION (self T_customer) DeleteRow(d ui.Dialog,row INT) RETURNS()
  DISPLAY sfmt("customer DeleteRow:%1",row)
END FUNCTION

FUNCTION (self T_customer) onDAevent( d ui.Dialog, row INT, event STRING) RETURNS ()
  DISPLAY SFMT("onDAevent ev:%1 row:%2", event,row)
  CASE event
    WHEN C_ON_ACTION || " custom_action"
      DISPLAY "custom action chosen"
  END CASE
END FUNCTION

FUNCTION (self T_customer) initINPUT(d ui.Dialog) RETURNS()
  DISPLAY "init customer INPUT called"
  CALL d.addTrigger("ON ACTION custom_action")
END FUNCTION

FUNCTION (self T_customer) BeforeField(d ui.Dialog, fieldName STRING) RETURNS()
  DISPLAY "customer BeforeField:", fieldName
END FUNCTION

--if this function is called the record member has the new value already (filled in by reflection)
FUNCTION (self T_customer)
  AfterField( d ui.Dialog, fieldName STRING, oldValue STRING)
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
  onINPUTevent( d ui.Dialog, ev STRING, fieldName STRING, value STRING)
  RETURNS(BOOLEAN, STRING)
  DISPLAY SFMT("onINPUTevent ev:%1 field:%2 value:%3", ev, fieldName, value)
  CASE ev
    WHEN C_ON_ACTION || " custom_action"
      DISPLAY "custom action chosen"
      RETURN TRUE, NULL
  END CASE
  RETURN FALSE, NULL
END FUNCTION
