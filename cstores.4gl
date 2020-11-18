&include "myassert.inc"
IMPORT FGL utils
IMPORT FGL ccustomers
IMPORT FGL corders
IMPORT FGL citems
TYPE T_voidFunc FUNCTION() RETURNS()
TYPE T_FuncAndName RECORD
  main T_voidFunc,
  tableName STRING
END RECORD

FUNCTION main()
  CALL utils.dbconnect()
  VAR stores DYNAMIC ARRAY OF STRING
  VAR storesF DYNAMIC ARRAY OF T_FuncAndName
  --ccustomers module
  VAR idx INT
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION ccustomers.main
  LET storesF[idx].tableName = "customers"
  --corders module
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION corders.main
  LET storesF[idx].tableName = "orders"
  --citems module
  LET idx = storesF.getLength() + 1
  LET storesF[idx].main = FUNCTION citems.main
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
