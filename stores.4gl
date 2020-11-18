#--mounts every table module into one maintenance app
&include "myassert.inc"
IMPORT FGL utils
IMPORT FGL customers
IMPORT FGL orders
IMPORT FGL items
TYPE T_voidFunc FUNCTION() RETURNS()
TYPE T_FuncAndName RECORD
  main T_voidFunc,
  tableName STRING
END RECORD

FUNCTION main()
  CALL utils.dbconnect()
  VAR stores DYNAMIC ARRAY OF STRING
  VAR storesF DYNAMIC ARRAY OF T_FuncAndName
  -- customers module
  VAR idx INT
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION customers.main
  LET storesF[idx].tableName = "customers"
  --orders module
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION orders.main
  LET storesF[idx].tableName = "orders"
  --items module
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION items.main
  LET storesF[idx].tableName = "items"
  --fill in the tablenames from each module
  VAR i INT
  FOR i = 1 TO storesF.getLength()
    LET stores[i] = storesF[i].tableName
  END FOR
  OPEN FORM f FROM "stores"
  DISPLAY FORM f
  DISPLAY ARRAY stores TO stores.*
    ON ACTION accept ATTRIBUTE(ROWBOUND)
      CALL storesF[arr_curr()].main()
  END DISPLAY
END FUNCTION
