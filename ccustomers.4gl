#-- demonstrates classic DISPLAY ARRAY/INPUT/CONSTRUCT statements
#-- for the customers table.
#-- The variable part constraints (AFTER FIELD, ON ACTION etc) are the same
#-- as in customers.4gl, but we need more code
&include "myassert.inc"
IMPORT util
IMPORT FGL utils
IMPORT FGL fgldialog
IMPORT FGL orders
IMPORT FGL utils_customer
IMPORT FGL cols_customer -- columns of customer
SCHEMA stores
PUBLIC TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF T_customer

FUNCTION fetchCustomers(customers T_customers, where STRING)
  DEFINE customer T_customer
  DEFINE n INT
  CALL customers.clear()
  VAR sql = SFMT("SELECT * FROM customer WHERE %1", where)
  PREPARE s1 FROM sql
  DECLARE cu1 CURSOR FOR s1
  FOREACH cu1 INTO customer.*
    LET n = n + 1
    LET customers[n].* = customer.*
  END FOREACH
END FUNCTION

FUNCTION updateCustomer(customer T_customer) RETURNS T_customer
  DEFINE num LIKE customer.customer_num = customer.customer_num
  IF NOT int_flag THEN
    UPDATE customer SET customer.* = customer.* WHERE @customer_num = $num
  ELSE
    --re read to repair changes to the record, this avoids a save var
    SELECT * INTO customer.* FROM customer WHERE @customer_num == $num
  END IF
  RETURN customer
END FUNCTION

FUNCTION browseArray(options T_SingleTableDAOptions, customers T_customers)
  DEFINE ba TM_BrowseCust
  LET ba.o = options
  CALL ba.browseArray(customers)
END FUNCTION

TYPE TM_BrowseCust RECORD
  o utils.T_SingleTableDAOptions,
  browseFormTextOrig STRING
END RECORD

FUNCTION (self TM_BrowseCust) browseArray(customers T_customers)
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
      CALL fetchCustomers(customers, where)
      MESSAGE SFMT("Found %1 records", customers.getLength())
    END IF
    IF filterActive AND customers.getLength() == 0 THEN
      LET filterActive = utils.filterNoRecords(filterActive)
      CONTINUE WHILE
    END IF
    CALL self.setBrowseTitle(filterActive)
    DISPLAY ARRAY customers TO scr.* ATTRIBUTE(UNBUFFERED, ACCEPT = FALSE)
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
      BEFORE ROW
        CALL self.checkOrders(DIALOG, customers[arr_curr()].customer_num)
      ON ACTION cancel
        LET done = TRUE
        EXIT DISPLAY
      ON UPDATE
        CALL self.inputRow(customers[arr_curr()].*, TRUE)
          RETURNING customers[arr_curr()].*
      ON APPEND
        CALL self.inputRow(customers[arr_curr()].*, TRUE)
          RETURNING customers[arr_curr()].*
      ON DELETE
        IF utils.reallyDeleteRecords() THEN
          DELETE FROM customer
            WHERE @customer_num = $customers[arr_curr()].customer_num
        END IF
      ON ACTION filter
        LET prevWhere = IIF(filterActive, where, NULL)
        LET filterActive = TRUE
        EXIT DISPLAY
      ON ACTION clear_filter
        LET prevWhere = NULL
        LET filterActive = FALSE
        EXIT DISPLAY
      ON ACTION show_orders ATTRIBUTE(ROWBOUND)
        VAR curr = customers[arr_curr()]
        CALL orders.showOrders(curr.customer_num, curr.fname, curr.lname)
    END DISPLAY
  END WHILE
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseCust)
  inputRow(
  customer T_customer, update BOOLEAN)
  RETURNS T_customer
  UNUSED(self)
  VAR winId = openDynamicWindow("customers_singlerow")
  CALL fgl_settitle(SFMT("Customer: %1", IIF(update, "Update", "New")))
  LET int_flag = FALSE
  INPUT BY NAME customer.* ATTRIBUTES(UNBUFFERED, WITHOUT DEFAULTS = update)
    AFTER FIELD zipcode --the variable part
      IF length(customer.zipcode) <> 5 THEN
        ERROR "Zipcode must have 5 digits"
        NEXT FIELD CURRENT
      END IF
    ON ACTION custom_action
      MESSAGE "custom_action"
  END INPUT
  CALL updateCustomer(customer.*) RETURNING customer.*
  CALL utils.closeDynamicWindow(winId)
  RETURN customer
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseCust) setBrowseTitle(filterActive BOOLEAN)
  IF NOT self.o.hasFilter THEN
    CALL fgl_settitle(self.o.browseTitle)
  ELSE
    CALL fgl_settitle(
      SFMT("%1 %2", self.o.browseTitle, IIF(filterActive, "filtered", "all")))
  END IF
END FUNCTION

CONSTANT SHOW_ORDERS = "show_orders"

PRIVATE FUNCTION (self TM_BrowseCust)
  checkOrders(
  d ui.Dialog, num LIKE customer.customer_num)
  VAR numOrders = count_orders(num)
  VAR active = numOrders > 0
  --DISPLAY "numOrders:", numOrders, ",active:", active
  CALL d.setActionActive(SHOW_ORDERS, active)
  CASE numOrders
    WHEN 0
      CALL d.setActionText(SHOW_ORDERS, "No Orders")
    WHEN 1
      CALL d.setActionText(SHOW_ORDERS, "Show Order")
    OTHERWISE
      CALL d.setActionText(SHOW_ORDERS, SFMT("Show %1 Orders", numOrders))
  END CASE
  IF self.o.hasDelete THEN
    CALL d.setActionActive("delete", numOrders == 0)
  END IF
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseCust) getFilterForm() RETURNS STRING
  DEFINE frm STRING
  LET frm =
    IIF(self.o.filterForm IS NOT NULL, self.o.filterForm, self.o.inputForm)
  RETURN frm
END FUNCTION

PRIVATE FUNCTION (self TM_BrowseCust) getFilter() RETURNS STRING
  DEFINE where STRING
  DEFINE winId INT
  VAR filterForm=self.getFilterForm()
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
  CONSTRUCT BY NAME where ON customer.*
  IF int_flag THEN
    LET where = ""
  END IF
  CALL utils.closeDynamicWindow(winId)
  RETURN where
END FUNCTION

PRIVATE FUNCTION count_orders(num LIKE customer.customer_num) RETURNS INT
  DEFINE n INT
  SELECT COUNT(*) INTO n FROM orders WHERE @customer_num == num
  RETURN n
END FUNCTION

FUNCTION main()
  DEFINE arr T_customers
  DEFINE opts T_SingleTableDAOptions =
    (browseForm: "customers",
     inputForm: "customers_singlerow",
      --filterForm: "customers_singlerow",
      hasUpdate: TRUE,
      hasAppend: FALSE,
      hasDelete: TRUE,
      hasFilter: TRUE,
      filterInitially: FALSE,
      addToolBar: TRUE)
  CALL utils.dbconnect()
  CALL browseArray(opts, arr)
END FUNCTION
