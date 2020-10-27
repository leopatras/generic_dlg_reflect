&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
IMPORT FGL customers
SCHEMA stores
TYPE T_order RECORD LIKE orders.* --TODO: add IMPLEMENTS
TYPE T_orders DYNAMIC ARRAY OF T_order
TYPE T_ordersWithMethods RECORD --TODO: add IMPLEMENTS
  o T_orders,
  browseOrders BOOLEAN
END RECORD

CONSTANT SHOW_CUSTOMER = "show_customer"

MAIN
  CALL utils.dbconnect()
  CALL showOrders(-1, "", "")
END MAIN

FUNCTION showOrders(
  customer_num LIKE customer.customer_num,
  fname LIKE customer.fname,
  lname LIKE customer.lname)
  DEFINE mo T_ordersWithMethods
  DEFINE opts sDAdyn.T_SingleTableDAOptions =
    (browseForm: "orders", browseRecord: "scr")
  --setting the delegate ensures we get events for situations where the array is empty
  LET opts.delegateDA = reflect.Value.valueOf(mo)
  LET opts.arrayValue = reflect.Value.valueOf(mo.o)
  IF customer_num == -1 THEN
    LET opts.sqlAll = "SELECT * FROM ORDERS"
    LET opts.hasFilter = TRUE
    LET mo.browseOrders = TRUE
  ELSE
    LET opts.sqlAll =
      SFMT("SELECT * FROM ORDERS WHERE customer_num = %1", customer_num)
    LET opts.browseTitle =
      SFMT("Orders of Customer %1: %2 %3",
        customer_num, fname CLIPPED, lname CLIPPED)
    LET mo.browseOrders = FALSE
  END IF
  CALL sDAdyn.browseArray(opts)
END FUNCTION

FUNCTION checkInterfaces(d T_ordersWithMethods INOUT)
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iOAD I_sDAdynOnActionInDA
  DEFINE iDA I_sDAdynInitDA
  MYASSERT(FALSE)
  LET iOAD =
    d --the compiler checks here if T_ordersWithMethods implements I_sDAdynOnActionInDA
  LET iDA =
    d --the compiler checks here if T_ordersWithMethods implements I_sDAdynInitDA
END FUNCTION

FUNCTION (self T_ordersWithMethods)
  initDA(
  sdi sDAdyn.I_SingleTableDA, d ui.Dialog)
  RETURNS()
  DISPLAY "T_ordersWithMethods initDA :", self.browseOrders
  IF self.browseOrders THEN
    CALL sdi.addOnActionRowBound(d, SHOW_CUSTOMER)
    CALL d.setActionText(SHOW_CUSTOMER, "Show Customer")
  END IF
END FUNCTION

FUNCTION (self T_ordersWithMethods)
  OnActionInDA(
  actionName STRING, row INT)
  RETURNS()

  --custom action always triggers here
  DISPLAY SFMT("ordersWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
  IF row >= 1 AND row <= self.o.getLength() THEN
    --shows how to access the array
    DISPLAY "curr order:",
      self.o[row].order_num,
      "cust:",
      self.o[row].customer_num
    CASE actionName
      WHEN SHOW_CUSTOMER
        CALL customers.show_cust(self.o[row].customer_num)
    END CASE
  END IF
END FUNCTION
