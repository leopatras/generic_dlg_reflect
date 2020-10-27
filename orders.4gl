&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
TYPE T_order RECORD LIKE orders.* --TODO: add IMPLEMENTS
TYPE T_orders DYNAMIC ARRAY OF T_order
TYPE T_ordersWithMethods RECORD --TODO: add IMPLEMENTS
  o T_orders
END RECORD

CONSTANT CUSTOM_ACTION = "Custom Action"

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
  ELSE
    LET opts.sqlAll =
      SFMT("SELECT * FROM ORDERS WHERE customer_num = %1", customer_num)
    LET opts.browseTitle =
      SFMT("Orders of Customer %1: %2 %3",
        customer_num, fname CLIPPED, lname CLIPPED)
  END IF
  LET opts.initDA = FUNCTION initDA
  CALL sDAdyn.browseArray(opts)
END FUNCTION

PRIVATE FUNCTION initDA(sdi sDAdyn.I_SingleTableDA, d ui.Dialog) RETURNS()
  DISPLAY "init customer DA called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

FUNCTION checkInterfaces(c T_order, d T_ordersWithMethods INOUT)
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iOAD I_sDAdynOnActionInDA
  MYASSERT(FALSE)
  LET iOAD = c  --the compiler checks here if T_order implements I_sDAdynOnActionInDA
  LET iOAD = d  --the compiler checks here if T_ordersWithMethods implements I_sDAdynOnActionInDA
END FUNCTION

FUNCTION (self T_order) OnActionInDA(actionName STRING, row INT) RETURNS()
  UNUSED(self)
  --custom action doesn't trigger here when the array is empty
  DISPLAY SFMT("order OnActionInDA actionName:'%1',row:%2", actionName, row)
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
    DISPLAY "curr order:", self.o[row].order_num
  END IF
END FUNCTION
