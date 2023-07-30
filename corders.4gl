#-- demonstrates classic DISPLAY ARRAY/INPUT/CONSTRUCT statements
#-- for the orders table.
#-- its basically a copy/paste/replace of the ccustomers module
&include "myassert.inc"
IMPORT FGL utils
IMPORT FGL utils_customer
IMPORT FGL citems
SCHEMA stores
PUBLIC TYPE T_order RECORD LIKE orders.*
TYPE T_orders DYNAMIC ARRAY OF T_order

DEFINE m_cust_num INT
DEFINE m_custName STRING

FUNCTION fetchOrders(orders T_orders, where STRING)
  DEFINE order T_order
  DEFINE n INT
  DEFINE rwhere STRING
  CALL orders.clear()
  LET rwhere = IIF(m_cust_num >= 0, SFMT("customer_num=%1", m_cust_num), where)
  VAR sql = SFMT("SELECT * FROM orders WHERE %1", rwhere)
  PREPARE s1 FROM sql
  DECLARE cu1 CURSOR FOR s1
  FOREACH cu1 INTO order.*
    LET n = n + 1
    LET orders[n].* = order.*
  END FOREACH
END FUNCTION

FUNCTION updateOrder(order T_order) RETURNS T_order
  DEFINE num LIKE orders.order_num = order.order_num
  IF NOT int_flag THEN
    UPDATE orders SET orders.* = order.* WHERE @order_num = $num
  ELSE
    --re read to repair changes to the record, this avoids a save var
    SELECT * INTO order.* FROM orders WHERE @order_num == $num
  END IF
  RETURN order
END FUNCTION

FUNCTION browseArray(options T_SingleTableDAOptions, orders T_orders)
  DEFINE ba TM_BrowseOrd
  LET ba.o = options
  CALL ba.browseArray(orders)
END FUNCTION

TYPE TM_BrowseOrd RECORD
  o utils.T_SingleTableDAOptions,
  browseFormTextOrig STRING
END RECORD

FUNCTION (self TM_BrowseOrd) browseArray(orders T_orders)
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
      CALL fetchOrders(orders, where)
      MESSAGE SFMT("Found %1 records", orders.getLength())
    END IF
    IF filterActive AND orders.getLength() == 0 THEN
      LET filterActive = utils.filterNoRecords(filterActive)
      CONTINUE WHILE
    END IF
    CALL self.setBrowseTitle(filterActive)
    DISPLAY ARRAY orders TO scr.* ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE)
      BEFORE DISPLAY
        CALL utils.checkCommonDA_Actions(DIALOG, self.o)
        IF m_cust_num >= 0 THEN
          CALL DIALOG.setActionHidden("show_customer", 1)
        END IF
      ON ACTION cancel
        LET done = TRUE
        EXIT DISPLAY
      ON UPDATE
        CALL self.inputRow(orders[arr_curr()].*, TRUE)
            RETURNING orders[arr_curr()].*
      ON APPEND
        CALL self.inputRow(orders[arr_curr()].*, FALSE)
            RETURNING orders[arr_curr()].*
      ON DELETE
        IF utils.reallyDeleteRecords() THEN
          VAR num = orders[arr_curr()].order_num
          DELETE FROM items WHERE @order_num = num
          DELETE FROM orders WHERE @order_num = num
        END IF
      ON ACTION filter
        LET prevWhere = IIF(filterActive, where, NULL)
        LET filterActive = TRUE
        EXIT DISPLAY
      ON ACTION clear_filter
        LET prevWhere = NULL
        LET filterActive = FALSE
        EXIT DISPLAY
      ON ACTION show_customer ATTRIBUTE(ROWBOUND)
        CALL utils_customer.showCustomer(orders[arr_curr()].customer_num)
      ON ACTION show_items
        CALL citems.showItems(orders[arr_curr()].order_num, m_custName)
    END DISPLAY
  END WHILE
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseOrd)
    inputRow(
    order T_order, update BOOLEAN)
    RETURNS T_order
  UNUSED(self)
  --VAR winId = openDynamicWindow("customers_singlerow")
  --CALL fgl_settitle(SFMT("Order: %1", IIF(update, "Update", "New")))
  LET int_flag = FALSE
  INPUT BY NAME order.* ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS = update)
  IF update THEN
    CALL updateOrder(order.*) RETURNING order.*
  ELSE
    INSERT INTO orders VALUES order.*
    LET order.order_num = sqlca.sqlerrd[2]
  END IF
  --CALL utils.closeDynamicWindow(winId)
  RETURN order
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
  CONSTRUCT BY NAME where ON orders.*
  IF int_flag THEN
    LET where = ""
  END IF
  CALL utils.closeDynamicWindow(winId)
  RETURN where
END FUNCTION

FUNCTION showOrders(
    customer_num LIKE customer.customer_num,
    fname LIKE customer.fname,
    lname LIKE customer.lname)
  DEFINE arr T_orders
  LET m_cust_num = customer_num

  VAR opts T_SingleTableDAOptions =
      (browseForm: "orders",
          --inputForm: "customers_singlerow",
          --filterForm: "customers_singlerow",
          hasUpdate: TRUE,
          addToolBar: TRUE)
  IF m_cust_num >= 0 THEN
    LET m_custName = SFMT("%1 %2", fname CLIPPED, lname CLIPPED)
    LET opts.browseTitle =
        SFMT("Orders of Customer %1: %2", customer_num, m_custName)
  ELSE
    LET opts.hasAppend = TRUE
    LET opts.hasDelete = TRUE
    LET opts.hasFilter = TRUE
  END IF
  CALL utils.dbconnect()
  CALL browseArray(opts, arr)
END FUNCTION

FUNCTION main()
  CALL showOrders(-1, "", "")
END FUNCTION
