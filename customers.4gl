&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
IMPORT FGL orders
IMPORT FGL cols_customer -- columns of customer
SCHEMA stores
--the 'TM_' prefix indicates: this type has Methods
--vs the 'T_' prefix : this type doesn't have Methods
--{ Begin TM_customer
PUBLIC TYPE TM_customer RECORD --TODO: add IMPLEMENTS
  cust RECORD LIKE customer.*,
  flagNotUsed BOOLEAN --for ex:usage in dialog
END RECORD

--method called by the browseArray function
--whenever a BEFORE ROW of a valid row (row>0 AND row<array.getLength()) is triggered
FUNCTION (self TM_customer) BeforeRow(d ui.Dialog, row INT) RETURNS()
  DISPLAY SFMT("customer BeforeRow:%1", row)
  CALL checkOrders(d, self.cust.customer_num)
END FUNCTION

FUNCTION (self TM_customer) CanDeleteRow(row INT) RETURNS BOOLEAN
  UNUSED(row) --normally we never need the row param
  --we only allow to delete a customer without orders
  RETURN count_orders(self.cust.customer_num) == 0
END FUNCTION

PRIVATE FUNCTION count_orders(
  customer_num LIKE customer.customer_num)
  RETURNS INT
  DEFINE n INT
  SELECT COUNT(*) INTO n FROM orders WHERE @customer_num == customer_num
  RETURN n
END FUNCTION

PRIVATE FUNCTION checkOrders(
  d ui.Dialog, customer_num LIKE customer.customer_num)
  VAR numOrders = count_orders(customer_num)
  VAR active = numOrders > 0
  DISPLAY "numOrders:", numOrders, ",active:", active
  CALL d.setActionActive(SHOW_ORDERS, active)
  CASE numOrders
    WHEN 0
      CALL d.setActionText(SHOW_ORDERS, "No Orders")
    WHEN 1
      CALL d.setActionText(SHOW_ORDERS, "Show Order")
    OTHERWISE
      CALL d.setActionText(SHOW_ORDERS, SFMT("Show %1 Orders", numOrders))
  END CASE
END FUNCTION

CONSTANT CUSTOM_ACTION = "Custom Action"

--method called by the browseArray function
--whenever the current RECORD is about to be edited for APPEND or UPDATE in an INPUT statement
--here custon INPUT actions can be added
FUNCTION (self TM_customer)
  InitINPUT(
  sdi I_SingleTableDA, d ui.Dialog)
  RETURNS()
  UNUSED(self)
  DISPLAY "init customer INPUT called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

--method called by the browseArray function
--in INPUT mode whenever a custom ON ACTION  is triggered
FUNCTION (self TM_customer) OnActionInINPUT(d ui.Dialog, actionName STRING)
  UNUSED(self)
  UNUSED(d)
  CASE actionName
    WHEN CUSTOM_ACTION
      MESSAGE "custom action chosen"
      DISPLAY "custom action chosen"
  END CASE
END FUNCTION

--method called by the browseArray function
--whenever the current RECORD is being edited for APPEND or UPDATE in an INPUT statement
--The method is triggered in an AFTER FIELD of the INPUT
--if this function is called the record field member has the *new* value already (filled in by reflection)
--if the field validation for a particular field isn't ok, RETURN an ERROR string
FUNCTION (self TM_customer)
  AfterField(
  d ui.Dialog, fieldName STRING, oldValue STRING)
  RETURNS STRING
  UNUSED(self)
  DISPLAY SFMT("AFTER FIELD field:%1 value:%2 oldValue:%3",
    fieldName, d.getFieldValue(fieldName), oldValue)
  CASE
    WHEN fieldName == cols_customer.C_zipcode AND length(self.cust.zipcode) <> 5
      DISPLAY fieldName, ":'", self.cust.zipcode, "'"
      RETURN "Zipcode must have 5 digits"
  END CASE
  RETURN NULL --NULL means: no error
END FUNCTION

FUNCTION (self TM_customer) checkInterfaces()
  --dummy func to enable a compiler check until IMPLEMENTS is there
  DEFINE iBR I_BeforeRow
  DEFINE iII I_InitINPUT
  DEFINE iAF I_AfterField
  DEFINE iOA I_OnActionInINPUT
  MYASSERT(FALSE)
  LET iBR = self --the compiler checks if TM_customer implements I_BeforeRow
  LET iAF = self
  LET iII = self
  --LET iIU = self
  LET iOA = self
END FUNCTION

--} End TM_customer

--{ Begin TM_customers
--we can't have ARRAY's with methods...but
--we can define a RECORD...
TYPE TM_customers RECORD --TODO IMPLEMENTS I_BeforeRow,
  c_arr
    DYNAMIC ARRAY OF
    TM_customer --..and have the data array 'c_arr' as a member
END RECORD

CONSTANT SHOW_ORDERS = "show_orders"
--method called by the browseArray function
--to initialize the DISPLAY ARRAY with a row bound action
FUNCTION (self TM_customers) InitDA(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
  UNUSED(self)
  DISPLAY "init customer DA called"
  CALL sdi.addOnActionRowBound(d, SHOW_ORDERS)
  CALL d.setActionText(SHOW_ORDERS, "Show Orders")
END FUNCTION

--method called by the browseArray function
--whenever an ON ACTION is triggered
--the array can be empty in this method
FUNCTION (self TM_customers) OnActionInDA(actionName STRING, row INT)
  DEFINE curr RECORD LIKE customer.*
  UNUSED(self)
  DISPLAY SFMT("customerWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
  CASE actionName
    WHEN SHOW_ORDERS
      MYASSERT(row >= 0 AND row <= self.c_arr.getLength())
      LET curr.* = self.c_arr[row].cust.*
      CALL orders.showOrders(curr.customer_num, curr.fname, curr.lname)
  END CASE
END FUNCTION

FUNCTION (self TM_customers) checkInterfaces()
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iDA I_InitDA
  DEFINE iOA I_OnActionInDA
  MYASSERT(FALSE)
  LET iDA = self --the compiler checks if TM_customers implements I_InitDA
  LET iOA = self --the compiler checks if TM_customers implements I_OnActionInDA
END FUNCTION
--} End TM_customers

FUNCTION main()
  DEFINE d TM_customers
  DEFINE opts sDAdyn.T_SingleTableDAOptions =
    (sqlAll: "SELECT * FROM CUSTOMER AS cust",
      browseForm: "customers",
      --browseForm: "customers_singlerow",
      browseRecord: "scr",
      inputForm: "customers_singlerow",
      --filterForm: "customers_singlerow",
      autoPhantom:
         TRUE, --tolerates missing FormFields/TableColumns in the forms
      addClickableImages: FALSE,
      hasUpdate: TRUE,
      hasAppend: TRUE,
      hasDelete: TRUE,
      hasFilter: TRUE -- ,filterInitially:TRUE
      , addToolBar: TRUE)
  CALL utils.dbconnect()
  --we pass the reflect value of the TM_customers variable
  --the browse array function calls various methods of the
  --TM_customers while being active
  LET opts.delegateDA = reflect.Value.valueOf(d)
  --we must pass the reflect value of the array,
  --the browseArray function fills the array with SQL data
  --and calls implemented methods of the TM_customer type
  --for the current RECORD in the array
  LET opts.arrayValue = reflect.Value.valueOf(d.c_arr)
  CALL sDAdyn.browseArray(opts)
END FUNCTION
