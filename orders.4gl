#-- show orders in 2 modes: browse all orders
#-- or show the orders for a specific customer
&include "myassert.inc"
&include "implements_interface.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL utils_customer
IMPORT FGL sDAdyn
IMPORT FGL items
SCHEMA stores

--{ Begin TM_orders
TYPE TM_orders RECORD --TODO: add IMPLEMENTS
  o_arr DYNAMIC ARRAY OF RECORD LIKE orders.*,
  browseOrders BOOLEAN,
  custName STRING
END RECORD

--dummy funcs to enable a compiler checks until IMPLEMENTS is there
IMPLEMENTS_INTERFACE(TM_orders, I_InitDA) --the compiler checks if TM_orders implements I_InitDA
IMPLEMENTS_INTERFACE(TM_orders, I_OnActionInDA) --the compiler checks if TM_orders implements I_OnActionInDA
IMPLEMENTS_INTERFACE(TM_orders, I_DeleteRow) --the compiler checks if TM_orders implements I_DeleteRow

CONSTANT SHOW_CUSTOMER = "show_customer"
CONSTANT SHOW_ITEMS = "show_items"

FUNCTION (self TM_orders) InitDA(sdi I_SingleTableDA, d ui.Dialog)
  IF self.browseOrders THEN
    CALL sdi.addOnActionRowBound(d, SHOW_CUSTOMER)
  END IF
  CALL sdi.addOnActionRowBound(d, SHOW_ITEMS)
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

--} End TM_orders

FUNCTION showOrders(
    customer_num LIKE customer.customer_num,
    fname LIKE customer.fname,
    lname LIKE customer.lname)
  DEFINE mo TM_orders
  DEFINE opts T_SingleTableDAOptions =
      (browseForm: "orders", browseRecord: "scr", addToolBar: TRUE)
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
    --LET opts.autoPhantom = TRUE
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
