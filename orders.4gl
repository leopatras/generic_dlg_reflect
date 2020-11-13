#-- show orders in 2 modes: browse all orders 
#-- or show the orders for a specific customer
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL utils_customer
IMPORT FGL sDAdyn
IMPORT FGL customers
IMPORT FGL items
SCHEMA stores

--{ Begin TM_orders
TYPE TM_orders RECORD --TODO: add IMPLEMENTS
  o_arr DYNAMIC ARRAY OF RECORD LIKE orders.*,
  browseOrders BOOLEAN,
  custName STRING
END RECORD

CONSTANT SHOW_CUSTOMER = "show_customer"
CONSTANT SHOW_ITEMS = "show_items"

FUNCTION (self TM_orders) InitDA(sdi I_SingleTableDA, d ui.Dialog)
  IF self.browseOrders THEN
    CALL sdi.addOnActionRowBound(d, SHOW_CUSTOMER)
    CALL d.setActionText(SHOW_CUSTOMER, "Show Customer")
  END IF
  CALL sdi.addOnActionRowBound(d, SHOW_ITEMS)
  CALL d.setActionText(SHOW_ITEMS, "Show Items")
END FUNCTION

FUNCTION (self TM_orders) OnActionInDA(actionName STRING, row INT)
  --custom action always triggers here
  DISPLAY SFMT("ordersWithMethods OnActionInDA actionName:'%1',row:%2",
    actionName, row)
  IF row >= 1 AND row <= self.o_arr.getLength() THEN
    CASE actionName
      WHEN SHOW_CUSTOMER
        CALL utils_customer.showCustomer(self.o_arr[row].customer_num)
      WHEN SHOW_ITEMS
        CALL items.showItems(self.o_arr[row].order_num, self.custName)
    END CASE
  END IF
END FUNCTION

FUNCTION (self TM_orders) DeleteRow(d ui.Dialog, row INT)
  UNUSED(d)
  WHENEVER ERROR RAISE
  VAR num = self.o_arr[row].order_num
  DELETE FROM items WHERE @order_num = num
  DELETE FROM orders WHERE @order_num = num
  WHENEVER ERROR STOP
END FUNCTION

FUNCTION (self TM_orders) checkInterfaces()
  --surrounds the missing IMPLEMENTS with a compiler check
  DEFINE iDA I_InitDA
  DEFINE iOA I_OnActionInDA
  DEFINE iDR I_DeleteRow
  MYASSERT(FALSE)
  LET iDA = self --the compiler checks if TM_orders implements I_InitDA
  LET iOA = self --the compiler checks if TM_orders implements I_OnActionInDA
  LET iDR = self --the compiler checks if TM_orders implements I_DeleteRow
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
    LET opts.hasUpdate = TRUE
    LET opts.hasDelete = TRUE
    LET opts.autoPhantom = TRUE
    LET mo.browseOrders = TRUE
  ELSE
    LET opts.sqlAll =
      SFMT("SELECT * FROM ORDERS WHERE customer_num = %1", customer_num)
    LET mo.custName = SFMT("%1 %2", fname CLIPPED, lname CLIPPED)
    LET opts.browseTitle =
      SFMT("Orders of Customer %1: %2", customer_num, mo.custName)
    LET mo.browseOrders = FALSE
  END IF
  CALL sDAdyn.browseArray(opts)
END FUNCTION

FUNCTION main()
  CALL utils.dbconnect()
  --browse all orders
  CALL showOrders(-1, "", "")
END FUNCTION
