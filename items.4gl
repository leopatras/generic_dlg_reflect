#-- we have the DeleteRow/InsertOrUpdate methods
#-- defined for the RECORD members
&include "myassert.inc"
&include "implements_interface.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
TYPE TM_Item RECORD LIKE items.*


IMPLEMENTS_INTERFACE(TM_Item, I_DeleteRow) --the compiler checks if TM_Item implements I_DeleteRow
FUNCTION (self TM_Item) DeleteRow(d ui.Dialog, row INT)
  UNUSED(d)
  UNUSED(row)
  WHENEVER ERROR RAISE
  --DISPLAY "DeleteRow:", util.JSON.stringify(self)
  DELETE FROM items
      WHERE @item_num = $self.item_num AND @order_num = $self.order_num
  WHENEVER ERROR STOP
END FUNCTION

IMPLEMENTS_INTERFACE(TM_Item, I_InsertOrUpdate) --the compiler checks if TM_Item implements I_InsertOrUpdate
FUNCTION (self TM_Item) InsertOrUpdate(update BOOLEAN)
  WHENEVER ERROR RAISE
  MYASSERT(update == TRUE)
  --DISPLAY "InsertOrUpdate:", util.JSON.stringify(self)
  UPDATE items
      SET items.* = self.*
      WHERE @item_num = $self.item_num AND @order_num = $self.order_num
  WHENEVER ERROR STOP
END FUNCTION

FUNCTION showItems(order_num LIKE orders.order_num, custName STRING)
  DEFINE arr DYNAMIC ARRAY OF TM_Item
  DEFINE opts T_SingleTableDAOptions
  LET opts.arrayValue = reflect.Value.valueOf(arr)
  LET opts.tabname = "items"
  LET opts.addToolBar = TRUE
  IF order_num == -1 THEN
    LET opts.sqlAll = "SELECT * FROM ITEMS"
    LET opts.hasFilter = TRUE
    LET opts.hasUpdate = TRUE
    LET opts.hasDelete = TRUE
    LET opts.addClickableImages = TRUE
  ELSE
    --show items for a specific order
    LET opts.sqlAll =
        SFMT("SELECT * FROM ITEMS WHERE order_num = %1", order_num)
    LET opts.browseTitle =
        SFMT("Items of Order %1,Customer: %2", order_num, custName)
  END IF
  CALL sDAdyn.browseArray(opts)
END FUNCTION

FUNCTION main()
  CALL utils.dbconnect()
  --browse all items
  CALL showItems(-1, "")
END FUNCTION
