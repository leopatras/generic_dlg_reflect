IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
IMPORT FGL orders
SCHEMA stores
TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF T_customer
TYPE T_customersWithMethods RECORD
  c T_customers
END RECORD
CONSTANT SHOW_ORDERS = "Show Orders"
CONSTANT CUSTOM_ACTION = "Custom Action"

MAIN
  DEFINE c T_customers
  DEFINE d T_customersWithMethods = (c: c)
  DEFINE sDA sDAdyn.T_SingleTableDA =
    (sqlAll: "SELECT * FROM CUSTOMER",
      browseForm: "customers_singlerow",
      browseRecord: "scr",
      hasUpdate: TRUE,
      hasAppend: TRUE,
      hasDelete: TRUE,
      hasFilter: TRUE -- ,filterInitially:TRUE
      )
  IF FALSE THEN
    CALL checkInterfaces(c, d)
  END IF
  CALL utils.dbconnect()
  LET sDA.initDA = FUNCTION initDA
  LET sDA.delegateDA = reflect.Value.valueOf(d)
  CALL sDA.browseArray(reflect.Value.valueOf(c))
  --CALL da()
END MAIN

FUNCTION checkInterfaces(c T_customers, d T_customersWithMethods INOUT)
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iAR I_sDAdynAfterRow
  DEFINE iBR I_sDAdynBeforeRow
  DEFINE iDR I_sDAdynDeleteRow
  DEFINE iOE I_sDAdynOnDAEvent
  DEFINE iII I_sDAdynInitInput
  DEFINE iBF I_sDAdynBeforeField
  DEFINE iAF I_sDAdynAfterField
  DEFINE iIU I_sDAdynInsertUpdate
  DEFINE iOAD I_sDAdynOnActionInDA
  DEFINE iOAI I_sDAdynOnActionInInput
  RETURN
  LET iAR = c[1] --the compiler checks here if T_customer implements I_sDAdynAfterRow
  LET iBR = c[1] --the compiler checks here if T_customer implements I_sDAdynBeforeRow
  LET iDR = c[1] --etc. etc.
  LET iOE = c[1]
  LET iAF = c[1]
  LET iBF = c[1]
  LET iIU = c[1]
  LET iOAI = c[1]
  LET iOAD = c[1]
  LET iOAD = d
END FUNCTION

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
  CALL sdi.addOnAction(d, SHOW_ORDERS)
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

FUNCTION (self T_customer) OnActionInDA(actionName STRING, row INT) RETURNS()
  DISPLAY SFMT("OnActionInDA actionName:'%1',row:%2", actionName, row)
  CASE actionName
    WHEN SHOW_ORDERS
      CALL orders.showOrders(self.customer_num, self.fname, self.lname)
    OTHERWISE
      DISPLAY "actionName:'", actionName, "' not handled"
  END CASE
END FUNCTION

FUNCTION (self T_customersWithMethods)
  OnActionInDA(
  actionName STRING, row INT)
  RETURNS()
  DISPLAY SFMT("customerWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
END FUNCTION

FUNCTION (self T_customer)
  onDAevent(
  d ui.Dialog, row INT, event STRING)
  RETURNS()
  DISPLAY SFMT("onDAevent ev:%1 row:%2", event, row)
  CASE event
    WHEN C_ON_ACTION || " " || SHOW_ORDERS
      DISPLAY "show orders"
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
  RETURN NULL --NULL means: no error
END FUNCTION

FUNCTION (self T_customer) OnActionInINPUT(d ui.Dialog, actionName STRING)
  CASE actionName
    WHEN CUSTOM_ACTION
      DISPLAY "custom action chosen"
  END CASE
END FUNCTION

FUNCTION (self T_customer)
  onINPUTevent(
  d ui.Dialog, ev STRING, fieldName STRING, value STRING)
  RETURNS(BOOLEAN, STRING)
  DISPLAY SFMT("onINPUTevent ev:%1 field:%2 value:%3", ev, fieldName, value)
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
