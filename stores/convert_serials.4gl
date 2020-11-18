&include "../myassert.inc"
IMPORT util
IMPORT FGL utils
SCHEMA stores
TYPE T_item RECORD LIKE items.*
TYPE T_items DYNAMIC ARRAY OF T_item
TYPE T_order RECORD LIKE orders.*
TYPE T_orders DYNAMIC ARRAY OF RECORD
  o T_order,
  items T_items
END RECORD
TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF RECORD
  c T_customer,
  oarr T_orders
END RECORD
--we delete all pre set data depending on SERIAL and insert it again with INSERT
--this avoids wrong serials for other DBs using internally SEQUENCES
--another possibility would be to do dummy INSERTS/DELETES until the max of the initial serial column is reached
--(which was set by the LOAD statement)
--this would avoid memory exhausting on large datasets.
MAIN
  DEFINE cu T_customers
  DEFINE i, j, k, len, n, nOrders, nItems INT
  DEFINE order_num LIKE orders.order_num
  CALL utils.dbconnect()
  CALL fetch_customers(cu)
  FOR i = 1 TO cu.getLength()
    CALL fetch_orders_by_cust(cu[i].oarr, cu[i].c.customer_num)
    LET nOrders = nOrders + cu[i].oarr.getLength()
    FOR j = 1 TO cu[i].oarr.getLength()
      LET order_num = cu[i].oarr[j].o.order_num
      CALL fetch_items_by_order(cu[i].oarr[j].items, order_num)
      LET nItems = nItems + cu[i].oarr[j].items.getLength()
      DELETE FROM items WHERE @order_num = order_num
      DELETE FROM orders WHERE @order_num = order_num
    END FOR
    DELETE FROM customer WHERE @customer_num = cu[i].c.customer_num
  END FOR
  SELECT COUNT(*) INTO n FROM customer
  MYASSERT(n == 0)
  SELECT COUNT(*) INTO n FROM orders
  MYASSERT(n == 0)
  SELECT COUNT(*) INTO n FROM items
  MYASSERT(n == 0)
  FOR i = 1 TO cu.getLength()
    --LET old_cust_num=cu[i].c.customer_num
    INSERT INTO customer VALUES cu[i].c.*
    LET cu[i].c.customer_num = sqlca.sqlerrd[2]
    FOR j = 1 TO cu[i].oarr.getLength()
      LET cu[i].oarr[j].o.customer_num = cu[i].c.customer_num
      INSERT INTO orders VALUES cu[i].oarr[j].o.*
      LET len = cu[i].oarr[j].items.getLength()
      LET order_num = cu[i].oarr[j].o.order_num
      LET cu[i].oarr[j].o.order_num = sqlca.sqlerrd[2]
      --DISPLAY sfmt("write %1 items for old order num:%2,new ordernum:%3",len,order_num,cu[i].oarr[j].o.order_num)
      FOR k = 1 TO len
        LET cu[i].oarr[j].items[k].order_num = cu[i].oarr[j].o.order_num
        INSERT INTO items VALUES cu[i].oarr[j].items[k].*
      END FOR
    END FOR
  END FOR
  SELECT COUNT(*) INTO n FROM customer
  MYASSERT(n == cu.getLength())
  SELECT COUNT(*) INTO n FROM orders
  MYASSERT(n == nOrders)
  SELECT COUNT(*) INTO n FROM items
  MYASSERT(n == nItems)
  DISPLAY "convert_serials done."
END MAIN

FUNCTION fetch_customers(customers T_customers)
  DEFINE customer T_customer
  DEFINE n INT
  DECLARE cu1 CURSOR FOR SELECT * INTO customer.* FROM customer
  FOREACH cu1
    LET n = n + 1
    LET customers[n].c.* = customer.*
  END FOREACH
END FUNCTION

FUNCTION fetch_orders_by_cust(
    orders T_orders, customer_num LIKE customer.customer_num)
  DEFINE order T_order
  DEFINE n INT
  DECLARE cu2 CURSOR FOR
      SELECT *
          INTO order.*
          FROM orders
          WHERE customer_num = @orders.customer_num
  FOREACH cu2
    LET n = n + 1
    LET orders[n].o.* = order.*
  END FOREACH
END FUNCTION

FUNCTION fetch_items_by_order(items T_items, order_num LIKE orders.order_num)
  DEFINE item T_item
  DEFINE n INT
  DECLARE cu3 CURSOR FOR
      SELECT * INTO item.* FROM items WHERE order_num = @items.order_num
  FOREACH cu3
    LET n = n + 1
    LET items[n].* = item.*
  END FOREACH
  --DISPLAY sfmt("did fetch:%1 items for order:%2",items.getLength(),order_num)
  --DISPLAY util.JSON.stringify(items)
END FUNCTION
