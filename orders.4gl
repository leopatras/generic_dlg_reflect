&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
TYPE T_order RECORD LIKE orders.*
TYPE T_orders DYNAMIC ARRAY OF T_order
TYPE T_ordersWithMethods RECORD
    o T_orders
END RECORD
  
CONSTANT CUSTOM_ACTION = "Custom Action"

MAIN
  CALL utils.dbconnect()
  CALL showOrders(-1,"","")
END MAIN

FUNCTION showOrders( customer_num LIKE customer.customer_num, fname LIKE customer.fname, lname LIKE customer.lname)
  DEFINE o T_orders
  DEFINE mo T_ordersWithMethods = ( o: o)
  DEFINE sDA sDAdyn.T_SingleTableDA = ( browseForm:"orders",browseRecord:"scr")
  --setting the delegate ensures we get events for situations where the array is empty
  LET sDA.delegateDA = reflect.Value.valueOf(mo)
  IF customer_num == -1 THEN
    LET sDA.sqlAll = "SELECT * FROM ORDERS"
    LET sDA.hasFilter = TRUE
  ELSE
    LET sDA.sqlAll = sfmt("SELECT * FROM ORDERS WHERE customer_num = %1",customer_num)
    LET sDA.browseTitle=sfmt("Orders of Customer %1: %2 %3",customer_num,fname CLIPPED,lname CLIPPED)
  END IF
  LET sDA.initDA = FUNCTION initDA
  CALL sDA.browseArray(reflect.Value.valueOf(o))
END FUNCTION

PRIVATE FUNCTION initDA(sdi sDAdyn.I_SingleTableDA, d ui.Dialog) RETURNS()
  DISPLAY "init customer DA called"
  CALL sdi.addOnAction(d, CUSTOM_ACTION)
END FUNCTION

FUNCTION (self T_order) OnActionInDA( actionName STRING,row INT) RETURNS ()
  --custom action doesn't trigger here when the array is empty
  DISPLAY SFMT("order OnActionInDA actionName:'%1',row:%2", actionName,row)
END FUNCTION

FUNCTION (self T_ordersWithMethods) OnActionInDA( actionName STRING, row INT) RETURNS()
  --custom action always triggers here
  DISPLAY SFMT("ordersWithMethods OnActionInDA actionName:'%1',row:%2", actionName,row)
  IF row>=1 AND row<=self.o.getLength() THEN
    --shows how to access the array
    DISPLAY "curr order:",self.o[row].order_num
  END IF
END FUNCTION
