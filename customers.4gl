&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
IMPORT FGL orders
SCHEMA stores
TYPE T_customer RECORD LIKE customer.* --TODO: add IMPLEMENTS
TYPE T_customers DYNAMIC ARRAY OF T_customer
--we can't have ARRAY's with methods...but
--we can define a RECORD...
TYPE T_customersWithMethods RECORD --TODO: add IMPLEMENTS
  c T_customers --..and have the data array as a member
END RECORD
CONSTANT SHOW_ORDERS = "show_orders"
CONSTANT CUSTOM_ACTION = "Custom Action"

MAIN
  DEFINE d T_customersWithMethods
  DEFINE opts sDAdyn.T_SingleTableDAOptions =
    (sqlAll: "SELECT * FROM CUSTOMER",
      browseForm: "customers_singlerow",
      browseRecord: "scr",
      hasUpdate: TRUE,
      hasAppend: TRUE,
      hasDelete: TRUE,
      hasFilter: TRUE -- ,filterInitially:TRUE
      )
  CALL utils.dbconnect()
  LET opts.delegateDA = reflect.Value.valueOf(d)
  LET opts.arrayValue = reflect.Value.valueOf(d.c)
  CALL sDAdyn.browseArray(opts)
END MAIN

FUNCTION checkInterfaces(c T_customer, d T_customersWithMethods INOUT)
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iAR I_sDAdynAfterRow
  DEFINE iBR I_sDAdynBeforeRow
  DEFINE iDR I_sDAdynDeleteRow
  DEFINE iOE I_sDAdynOnDAEvent
  DEFINE iII I_sDAdynInitInput
  DEFINE iIE I_sDAdynOnInputEvent
  DEFINE iBF I_sDAdynBeforeField
  DEFINE iAF I_sDAdynAfterField
  DEFINE iIU I_sDAdynInsertUpdate
  DEFINE iOAD I_sDAdynOnActionInDA
  DEFINE iOAI I_sDAdynOnActionInInput
  MYASSERT(FALSE)
  LET iAR =
    c --the compiler checks here if T_customer implements I_sDAdynAfterRow
  LET iBR =
    c --the compiler checks here if T_customer implements I_sDAdynBeforeRow
  LET iBR = d
  LET iDR = c --etc. etc.
  LET iOE = d
  LET iAF = c
  LET iBF = c
  LET iII = c
  LET iIE = c
  LET iIU = c
  LET iOAI = c
  LET iOAD = c
  LET iOAD = d
END FUNCTION

FUNCTION show_cust(customer_num LIKE customer.customer_num)
  DEFINE cust T_customer
  SELECT * INTO cust.* FROM customer WHERE @customer_num = customer_num
  OPEN WINDOW show_cust WITH FORM "customers_singlerow"
  DISPLAY cust.* TO scr.*
  MENU
    COMMAND "Exit"
      EXIT MENU
  END MENU
  CLOSE WINDOW show_cust
END FUNCTION

--method called by the browseArray function
--to initialize the DISPLAY ARRAY with a row bound action
FUNCTION (self T_customersWithMethods)
  initDA(
  sdi sDAdyn.I_SingleTableDA, d ui.Dialog)
  RETURNS()
  UNUSED(self)
  DISPLAY "init customer DA called"
  CALL sdi.addOnActionRowBound(d, SHOW_ORDERS)
  CALL d.setActionText(SHOW_ORDERS, "Show Orders")
END FUNCTION

PRIVATE FUNCTION count_orders(customer_num LIKE customer.customer_num)
  DEFINE n INT
  SELECT COUNT(*) INTO n FROM orders WHERE @customer_num == customer_num
  RETURN n
END FUNCTION

PRIVATE FUNCTION checkOrders(
  d ui.Dialog, customer_num LIKE customer.customer_num)
  CALL d.setActionActive(SHOW_ORDERS, count_orders(customer_num) > 0)
END FUNCTION

--method called by the browseArray function
--whenever a BEFORE ROW of a valid row (row>0 AND row<array.getLength()) is triggered
FUNCTION (self T_customer) BeforeRow(d ui.Dialog, row INT) RETURNS()
  DISPLAY SFMT("customer BeforeRow:%1", row)
  CALL checkOrders(d, self.customer_num)
END FUNCTION

--method called by the browseArray function
--on *each* BEFORE ROW of the DISPLAY ARRAY (row may be 0)
FUNCTION (self T_customersWithMethods) BeforeRow(d ui.Dialog, row INT) RETURNS()
  UNUSED(self)
  UNUSED(d)
  DISPLAY SFMT("customersWithMethods BeforeRow:%1", row)
END FUNCTION

--method called by the browseArray function
--whenever an AFTER ROW of a valid row (row>0 AND row<array.getLength()) is triggered
FUNCTION (self T_customer) AfterRow(d ui.Dialog, row INT) RETURNS()
  UNUSED(self)
  UNUSED(d)
  DISPLAY SFMT("customer AfterRow:%1", row)
END FUNCTION

--method called by the browseArray function
--whenever an ON ACTION of a valid row (row>0 AND row<array.getLength()) is triggered
--if the ACTION is rowbound, this method is sufficient for this action
FUNCTION (self T_customer) OnActionInDA(actionName STRING, row INT) RETURNS()
  DISPLAY SFMT("OnActionInDA actionName:'%1',row:%2", actionName, row)
  CASE actionName
    WHEN SHOW_ORDERS
      CALL orders.showOrders(self.customer_num, self.fname, self.lname)
    OTHERWISE
      DISPLAY "actionName:'", actionName, "' not handled"
  END CASE
END FUNCTION

--method called by the browseArray function
--whenever an ON ACTION is triggered
--the array can be empty in this method
FUNCTION (self T_customersWithMethods) OnActionInDA(actionName STRING, row INT)
  UNUSED(self)
  DISPLAY SFMT("customerWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
END FUNCTION

--method called by the browseArray function
--whenever another DISPLAY ARRAY event (BEFORE DISPLAY for example) is triggered
--the array can be empty in this method
FUNCTION (self T_customersWithMethods)
  OnDAevent(
  d ui.Dialog, row INT, event STRING)
  UNUSED(self)
  UNUSED(d)
  DISPLAY SFMT("OnDAevent ev:%1 row:%2", event, row)
END FUNCTION

--method called by the browseArray function
--whenever the current RECORD is about to be edited for APPEND or UPDATE in an INPUT statement
--here custon INPUT actions can be added
FUNCTION (self T_customer) initINPUT(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
  UNUSED(self)
  DISPLAY "init customer INPUT called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

--method called by the browseArray function
--whenever the current RECORD is being edited for APPEND or UPDATE in an INPUT statement
--is triggered in a BEFORE FIELD of the INPUT
FUNCTION (self T_customer) BeforeField(d ui.Dialog, fieldName STRING) RETURNS()
  UNUSED(self)
  UNUSED(d)
  DISPLAY "customer BeforeField:", fieldName
END FUNCTION

--method called by the browseArray function
--whenever the current RECORD is being edited for APPEND or UPDATE in an INPUT statement
--is triggered in an AFTER FIELD of the INPUT
--if this function is called the record field member has the *new* value already (filled in by reflection)
--if the field validation for a particular field isn't ok, RETURN an ERROR string
FUNCTION (self T_customer)
  AfterField(
  d ui.Dialog, fieldName STRING, oldValue STRING)
  RETURNS STRING
  UNUSED(self)
  DISPLAY SFMT("AFTER FIELD field:%1 value:%2 oldValue:%3",
    fieldName, d.getFieldValue(fieldName), oldValue)
  CASE
    WHEN fieldName == "zipcode" AND LENGTH(self.zipcode) <> 5
      DISPLAY "zipcode:'", self.zipcode, "'"
      RETURN "Zipcode must have 5 digits"
  END CASE
  RETURN NULL --NULL means: no error
END FUNCTION

--method called by the browseArray function
--in INPUT mode whenever a custom ON ACTION  is triggered
FUNCTION (self T_customer) OnActionInINPUT(d ui.Dialog, actionName STRING)
  UNUSED(self)
  UNUSED(d)
  CASE actionName
    WHEN CUSTOM_ACTION
      MESSAGE "custom action chosen"
      DISPLAY "custom action chosen"
  END CASE
END FUNCTION

FUNCTION (self T_customer)
  OnINPUTevent(
  d ui.Dialog, ev STRING, fieldName STRING, value STRING)
  RETURNS(BOOLEAN, STRING)
  UNUSED(self)
  UNUSED(d)
  DISPLAY SFMT("OnINPUTevent ev:%1 field:%2 value:%3", ev, fieldName, value)
  RETURN FALSE, NULL
END FUNCTION

--method called by the browseArray function
--after INPUT mode whenever the current INPUT data must be written to DB
--depending on the update flag one needs to invoke the suitable SQL statements here
FUNCTION (self T_customer) insertOrUpdate(update BOOLEAN)
  IF update THEN
    UPDATE customer
      SET customer.* = self.*
      WHERE @customer_num = self.customer_num
  ELSE
    INSERT INTO customer VALUES self.*
  END IF
END FUNCTION

FUNCTION (self T_customer) DeleteRow(d ui.Dialog, row INT) RETURNS()
  UNUSED(self)
  UNUSED(d)
  DISPLAY SFMT("customer DeleteRow:%1", row)
  DELETE FROM customer WHERE @customer_num = self.customer_num
END FUNCTION
