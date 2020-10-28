&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
IMPORT FGL customers
SCHEMA stores

--the 'TM_' prefix indicates: this type has Methods
--vs the 'T_' prefix : this type doesn't have Methods
--{ Begin TM_orders
TYPE TM_orders RECORD --TODO: add IMPLEMENTS
  o_arr DYNAMIC ARRAY OF RECORD LIKE orders.*,
  browseOrders BOOLEAN
END RECORD

CONSTANT SHOW_CUSTOMER = "show_customer"

FUNCTION (self TM_orders) InitDA(sdi I_SingleTableDA, d ui.Dialog)
  DISPLAY "TM_orders InitDA :", self.browseOrders
  IF self.browseOrders THEN
    CALL sdi.addOnActionRowBound(d, SHOW_CUSTOMER)
    CALL d.setActionText(SHOW_CUSTOMER, "Show Customer")
  END IF
END FUNCTION

FUNCTION (self TM_orders) OnActionInDA(actionName STRING, row INT)
  --custom action always triggers here
  DISPLAY SFMT("ordersWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
  IF row >= 1 AND row <= self.o_arr.getLength() THEN
    --shows how to access the array
    DISPLAY "curr order:",
      self.o_arr[row].order_num,
      "cust:",
      self.o_arr[row].customer_num
    CASE actionName
      WHEN SHOW_CUSTOMER
        CALL customers.show_cust(self.o_arr[row].customer_num)
    END CASE
  END IF
END FUNCTION

FUNCTION (self TM_orders) checkInterfaces()
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iDA I_InitDA
  DEFINE iOA I_OnActionInDA
  MYASSERT(FALSE)
  LET iDA = self --the compiler checks if TM_orders implements I_InitDA
  LET iOA = self --the compiler checks if TM_orders implements I_OnActionInDA
END FUNCTION
--} End TM_orders

FUNCTION showOrders(
  customer_num LIKE customer.customer_num,
  fname LIKE customer.fname,
  lname LIKE customer.lname)
  DEFINE mo TM_orders
  DEFINE opts sDAdyn.T_SingleTableDAOptions =
    (browseForm: "orders", browseRecord: "scr")
  --we pass the reflect value of the TM_orders variable
  --the browse array function calls various methods of the
  --TM_orders type while being active
  LET opts.delegateDA = reflect.Value.valueOf(mo)
  --we must pass the reflect value of the array,
  --the browseArray function fills the array with SQL data
  LET opts.arrayValue = reflect.Value.valueOf(mo.o_arr)
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

FUNCTION MAIN()
  CALL utils.dbconnect()
  CALL showOrders(-1, "", "")
END FUNCTION
