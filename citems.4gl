#-- demonstrates classic DISPLAY ARRAY/INPUT/CONSTRUCT statements
#-- for the items table.
#-- its basically a copy/paste/replace of the corders module
&include "myassert.inc"
IMPORT util
IMPORT FGL utils
IMPORT FGL fgldialog
IMPORT FGL utils_customer
SCHEMA stores
PUBLIC TYPE T_item RECORD 
   it RECORD LIKE items.*,
   upd STRING,
   del STRING
END RECORD
TYPE T_items DYNAMIC ARRAY OF T_item

DEFINE m_order_num INT

FUNCTION fetchItems(items T_items, where STRING)
  DEFINE it RECORD LIKE items.*
  DEFINE n INT
  DEFINE rwhere STRING
  CALL items.clear()
  LET rwhere = IIF(m_order_num >= 0, SFMT("order_num=%1", m_order_num), where)
  VAR sql = SFMT("SELECT * FROM items WHERE %1", rwhere)
  PREPARE s1 FROM sql
  DECLARE cu1 CURSOR FOR s1
  FOREACH cu1 INTO it.*
    LET n = n + 1
    LET items[n].it.* = it.*
    LET items[n].upd = "fa-edit"
    LET items[n].del = "fa-trash-o"
  END FOREACH
END FUNCTION


FUNCTION updateItems(it RECORD LIKE items.*) RETURNS RECORD LIKE items.*
  DEFINE num LIKE items.item_num = it.item_num
  IF NOT int_flag THEN
    UPDATE items SET items.* = it.* WHERE @item_num = $num AND @order_num = it.order_num
  ELSE
    --re read to repair changes to the record, this avoids a save var
    SELECT * INTO it.* FROM items WHERE @item_num == $num AND @order_num = it.order_num
  END IF
  RETURN it
END FUNCTION

FUNCTION browseArray(options T_SingleTableDAOptions, items T_items)
  DEFINE ba TM_BrowseOrd
  LET ba.o = options
  CALL ba.browseArray(items)
END FUNCTION

TYPE TM_BrowseOrd RECORD
  o utils.T_SingleTableDAOptions,
  browseFormTextOrig STRING
END RECORD

FUNCTION (self TM_BrowseOrd) browseArray(items T_items)
  DEFINE filterActive BOOLEAN
  DEFINE where STRING
  VAR winId = utils.openDynamicWindow(self.o.browseForm)
  CALL utils.checkCloseScreen()
  LET self.o.browseRecord = "scr"
  CALL utils.checkToolBar(self.o)
  --CALL checkDATEjustify(trec, names, rec, self.o.qualifiedNames)
  --we need to store the original form text
  --because fgl_settitle changes the form text too
  LET self.browseFormTextOrig = utils.getFormTitle()
  IF self.o.browseTitle IS NULL THEN
    LET self.o.browseTitle = SFMT("%1: Browse", self.browseFormTextOrig)
  END IF
  IF self.o.hasFilter AND self.o.filterInitially THEN
    LET filterActive = TRUE
  END IF
  VAR done = FALSE
  VAR prevWhere = ""
  WHILE NOT done
    IF filterActive THEN
      LET where = self.getFilter()
      --DISPLAY "getFilter did return where:",where
      IF where == " 1=1" THEN
        --DISPLAY "filter wasn't actually set"
        LET filterActive = FALSE
      ELSE
        IF where IS NULL THEN --user cancelled the filter
          IF prevWhere IS NOT NULL THEN
            LET where = prevWhere
          ELSE
            LET filterActive = FALSE
            LET where = " 1=1"
          END IF
        END IF
      END IF
    ELSE
      LET where = "1=1"
    END IF

    IF where IS NOT NULL THEN
      CALL fetchItems(items, where)
      MESSAGE SFMT("Found %1 records", items.getLength())
    END IF
    IF filterActive AND items.getLength() == 0 THEN
      LET filterActive = utils.filterNoRecords(filterActive)
      CONTINUE WHILE
    END IF
    CALL self.setBrowseTitle(filterActive)
    DISPLAY ARRAY items TO scr.* ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE)
      BEFORE DISPLAY
        CALL DIALOG.setActionText("cancel", "Exit")
        CALL DIALOG.setActionActive("update", self.o.hasUpdate)
        CALL DIALOG.setActionHidden("update", NOT self.o.hasUpdate)
        CALL DIALOG.setActionActive("append", self.o.hasAppend)
        CALL DIALOG.setActionHidden("append", NOT self.o.hasAppend)
        CALL DIALOG.setActionActive("delete", self.o.hasDelete)
        CALL DIALOG.setActionHidden("delete", NOT self.o.hasDelete)
        CALL DIALOG.setActionActive("filter", self.o.hasFilter)
        CALL DIALOG.setActionHidden("filter", NOT self.o.hasFilter)
        CALL DIALOG.setActionActive("clear_filter", self.o.hasFilter)
        CALL DIALOG.setActionHidden("clear_filter", NOT self.o.hasFilter)
      ON ACTION cancel
        LET done = TRUE
        EXIT DISPLAY
      ON UPDATE
        CALL self.inputRow(items[arr_curr()].*, TRUE)
          RETURNING items[arr_curr()].*
      ON APPEND
        CALL self.inputRow(items[arr_curr()].*, FALSE)
          RETURNING items[arr_curr()].*
      ON DELETE
        IF utils.reallyDeleteRecords() THEN
          VAR num = items[arr_curr()].it.item_num
          VAR order_num = items[arr_curr()].it.order_num
          DELETE FROM items WHERE @item_num = $num AND @order_num = $order_num
        END IF
      ON ACTION filter
        LET prevWhere = IIF(filterActive, where, NULL)
        LET filterActive = TRUE
        EXIT DISPLAY
      ON ACTION clear_filter
        LET prevWhere = NULL
        LET filterActive = FALSE
        EXIT DISPLAY
    END DISPLAY
  END WHILE
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseOrd)
  inputRow(
  item T_item, update BOOLEAN)
  RETURNS T_item
  UNUSED(self)
  --VAR winId = openDynamicWindow("customers_singlerow")
  --CALL fgl_settitle(SFMT("Order: %1", IIF(update, "Update", "New")))
  LET int_flag = FALSE
  INPUT BY NAME item.it.* ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS = update)
  MYASSERT(update==TRUE)
  CALL updateItems(item.it.*) RETURNING item.it.*
  --CALL utils.closeDynamicWindow(winId)
  RETURN item
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseOrd) setBrowseTitle(filterActive BOOLEAN)
  IF NOT self.o.hasFilter THEN
    CALL fgl_settitle(self.o.browseTitle)
  ELSE
    CALL fgl_settitle(
      SFMT("%1 %2", self.o.browseTitle, IIF(filterActive, "filtered", "all")))
  END IF
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseOrd) getFilterForm() RETURNS STRING
  DEFINE frm STRING
  LET frm =
    IIF(self.o.filterForm IS NOT NULL, self.o.filterForm, self.o.inputForm)
  RETURN frm
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseOrd) getFilter() RETURNS STRING
  DEFINE where STRING
  DEFINE winId INT
  VAR filterForm = self.getFilterForm()
  IF filterForm IS NOT NULL THEN
    LET winId = utils.openDynamicWindow(filterForm)
    IF self.o.filterTitle IS NULL THEN
      LET self.o.filterTitle =
        SFMT("%1: Input filter criteria", utils.getFormTitle())
    END IF
  ELSE
    IF self.o.filterTitle IS NULL THEN
      LET self.o.filterTitle =
        SFMT("%1: Input filter criteria", self.browseFormTextOrig)
    END IF
  END IF
  CALL fgl_settitle(self.o.filterTitle)
  MESSAGE "Input filter criteria"
  LET int_flag = FALSE
  -- restore the filter from the previous run is *not* easy possible
  CONSTRUCT BY NAME where ON items.*
  IF int_flag THEN
    LET where = ""
  END IF
  CALL utils.closeDynamicWindow(winId)
  RETURN where
END FUNCTION

FUNCTION showItems(order_num LIKE orders.order_num, custName STRING)
  DEFINE arr T_items
  LET m_order_num = order_num

  VAR opts T_SingleTableDAOptions =
    (browseForm: "items",
      --inputForm: "customers_singlerow",
      --filterForm: "customers_singlerow",
      hasUpdate: TRUE,
      addToolBar: TRUE)
  IF m_order_num >= 0 THEN
    LET opts.browseTitle =
      SFMT("Items of Order %1,Customer: %2", order_num, custName)
  ELSE
    LET opts.hasDelete = TRUE
    LET opts.hasFilter = TRUE
  END IF
  CALL utils.dbconnect()
  CALL browseArray(opts, arr)
END FUNCTION

FUNCTION main()
  CALL showItems(-1, "")
END FUNCTION
