&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores

FUNCTION showItems(
  order_num LIKE orders.order_num, custName STRING)
  DEFINE arr DYNAMIC ARRAY OF RECORD LIKE items.*
  DEFINE opts sDAdyn.T_SingleTableDAOptions 
    -- = (browseForm: "orders", browseRecord: "scr")
  --we must pass the reflect value of the array,
  --the browseArray function fills the array with SQL data
  LET opts.arrayValue = reflect.Value.valueOf(arr)
  LET opts.hasUpdate = TRUE
  LET opts.hasDelete = TRUE
  LET opts.addClickableImages = TRUE
  IF order_num == -1 THEN
    LET opts.sqlAll = "SELECT * FROM ITEMS"
    LET opts.hasFilter = TRUE
    LET opts.autoPhantom = TRUE
  ELSE
    LET opts.sqlAll =
      SFMT("SELECT * FROM ITEMS WHERE order_num = %1", order_num)
    LET opts.browseTitle =
      SFMT("Items of Order %1,Customer: %2", order_num, custName)
  END IF
  CALL sDAdyn.browseArray(opts)
END FUNCTION

FUNCTION main()
  CALL utils.dbconnect()
  CALL showItems(-1, "")
END FUNCTION
