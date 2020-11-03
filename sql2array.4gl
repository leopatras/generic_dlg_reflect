#+ shows how one can use the reflect API to read a SQL query into arbitrary arrays:
#+ the assignment is done "by name" (and the types must be suitable of course)
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils

SCHEMA stores

TYPE T_customer RECORD LIKE customer.*
TYPE T_customers DYNAMIC ARRAY OF T_customer
TYPE T_customersSub DYNAMIC ARRAY OF RECORD
  cust T_customer,
  cust_ex RECORD LIKE cust_ex.*
END RECORD

TYPE T_customersAndOrders DYNAMIC ARRAY OF RECORD
  cust T_customer,
  order RECORD LIKE orders.*
END RECORD

TYPE T_x RECORD
  sub RECORD
    customer_num
    LIKE customer.customer_num ATTRIBUTE(json_name = "Customer Number"),
    fname LIKE customer.fname
  END RECORD,
  unrelated INT
END RECORD

TYPE T_xarr DYNAMIC ARRAY OF T_x


MAIN
  DEFINE a,a1 T_customers
  DEFINE b,b1 T_customersSub
  DEFINE c,c1 T_customersAndOrders
  DEFINE d,d1 T_xarr
  CALL utils.dbconnect()
  --demonstrates an ARRAY with a RECORD LIKE definition
  CALL readIntoArray(reflect.Value.valueOf(a), "select * from customer",FALSE)
  CALL fetch_customers(a1)
  MYASSERT(util.JSON.stringify(a).equals(util.JSON.stringify(a1)))
  --demonstrates an ARRAY with a RECORD LIKE definition as a sub record ...as we have customer_num twice
  --we need to read in by position
  CALL readIntoArray(reflect.Value.valueOf(b), "select * FROM customer,cust_ex",TRUE)
  CALL fetch_customers_ex(b1)
  MYASSERT(util.JSON.stringify(b).equals(util.JSON.stringify(b1)))
  CALL fetch_customers_and_orders(customersAndOrders: c)
  CALL readIntoArray(reflect.Value.valueOf(c1),"SELECT UNIQUE * FROM customer,orders WHERE customer.customer_num = orders.customer_num",TRUE)
  --DISPLAY util.JSON.stringify(c[1])
  --DISPLAY util.JSON.stringify(c1[1])
  MYASSERT(util.JSON.stringify(c).equals(util.JSON.stringify(c1)))
  --demonstrates an ARRAY of RECORD with having some LIKE members in a sub RECORD, 
  --and an unrelated member
  --(to be used in dialog code for example)
  --the relevant members are fetched "by name" (similar to JSON)
  --this case shows also how to retrieve a type attribute(json_name)
  CALL fetch_sparse(d)
  CALL readIntoArray(reflect.Value.valueOf(d1), "select * from customer",FALSE)
  MYASSERT(util.JSON.stringify(d).equals(util.JSON.stringify(d1)))
  DISPLAY util.JSON.stringify(d1)
END MAIN

FUNCTION fetch_customers(customers T_customers)
  DEFINE customer T_customer
  DEFINE n INT
  DECLARE cu1 CURSOR FOR SELECT * INTO customer.* FROM customer
  FOREACH cu1
      LET n = n + 1
     LET customers[n].* = customer.*
  END FOREACH
END FUNCTION

FUNCTION fetch_customers_ex(customersX T_customersSub)
  DEFINE customer T_customer
  DEFINE cust_ex RECORD LIKE cust_ex.*
  DEFINE n INT
  DECLARE cu2 CURSOR FOR SELECT * INTO customer.*,cust_ex.* FROM customer,cust_ex
  FOREACH cu2
      LET n = n + 1
     LET customersX[n].cust.*  = customer.*
     LET customersX[n].cust_ex.* = cust_ex.*
  END FOREACH
END FUNCTION

FUNCTION fetch_customers_and_orders(customersAndOrders T_customersAndOrders)
  DEFINE cust T_customer
  DEFINE order RECORD LIKE orders.*
  DEFINE n INT
  DECLARE cu3 CURSOR FOR SELECT UNIQUE * INTO cust.*,order.* FROM customer,orders  WHERE customer.customer_num = orders.customer_num
  FOREACH cu3
      LET n = n + 1
     LET customersAndOrders[n].cust.*  = cust.*
     LET customersAndOrders[n].order.* = order.*
  END FOREACH
END FUNCTION

FUNCTION fetch_sparse(xarr T_xarr)
  DEFINE x T_x
  DEFINE n INT
  DECLARE cu4 CURSOR FOR SELECT customer_num,fname INTO x.sub.customer_num,x.sub.fname FROM customer
  FOREACH cu4
     LET n = n + 1
     LET xarr[n].sub.* = x.sub.*
  END FOREACH
END FUNCTION

TYPE T_2idx RECORD
  idx INT,
  subIdx INT
END RECORD

TYPE T_2idxDICT DICTIONARY OF T_2idx

PRIVATE FUNCTION setName2Index(
  name2Index T_2idxDICT, name STRING, idx INT, subIdx INT)
  DEFINE tuple T_2idx = (idx: idx, subIdx: subIdx)
  MYASSERT(NOT name2Index.contains(name))
  LET name2Index[name] = tuple
END FUNCTION

--reads an sql query into the array and sets the members "by name"
FUNCTION readIntoArray(arr reflect.Value, sql STRING,byPosition BOOLEAN) RETURNS()
  DEFINE tarr, trec, tf, subtf reflect.Type
  DEFINE cnt, subcnt, fieldIndex, subIndex INT
  DEFINE recName,name STRING
  DEFINE pos INT
  --DEFINE json_name STRING
  DEFINE name2Index T_2idxDICT
  LET tarr = arr.getType()
  MYASSERT(tarr.getKind() == "ARRAY")
  CALL arr.clear()
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  LET trec = tarr.getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  LET cnt = trec.getFieldCount()
  --build a dictionary to have a record position for a record member name
  FOR fieldIndex = 1 TO cnt
    LET tf = trec.getFieldType(fieldIndex)
    IF tf.getKind() == "RECORD" THEN
      LET recName=trec.getFieldName(fieldIndex)
      DISPLAY "recName:",recName
      LET subcnt = tf.getFieldCount()
      FOR subIndex = 1 TO subcnt
        LET pos=pos+1
        LET subtf = tf.getFieldType(subIndex)
        LET name = tf.getFieldName(subIndex)
        IF NOT byPosition THEN
          CALL setName2Index(name2Index, name, fieldIndex, subIndex)
        END IF
      END FOR
    ELSE
      LET pos=pos+1
      LET name = trec.getFieldName(fieldIndex)
      --LET json_name = tf.getAttribute("json_name")

      --DISPLAY "name:", name, " ", tf.toString(), " ", tf.getKind(),",index:",fieldIndex,",json_name:",json_name
      IF NOT byPosition THEN
        CALL setName2Index(name2Index, name, fieldIndex, subIndex)
      END IF
    END IF
  END FOR
  CALL fillArrayWithQueryData(arr, name2Index, sql,byPosition, pos)
END FUNCTION

FUNCTION assignResultsByName(h base.SqlHandle,name2Index T_2idxDICT,recv reflect.Value,resultCount INT)
  DEFINE tuple T_2idx
  DEFINE name, sqltype STRING
  DEFINE value reflect.Value
  DEFINE fv reflect.Value
  DEFINE i, idx INT
    FOR i = 1 TO resultCount
      LET name = h.getResultName(i)
      LET tuple = name2Index[name]
      IF tuple.idx IS NULL THEN
        DISPLAY "can't find index for name:", name
        CONTINUE FOR
      END IF
      LET sqltype = h.getResultType(i)
      LET value = reflect.Value.copyOf(h.getResultValue(i))
      --DISPLAY sfmt("did find index:%1 for %2,type:%3 value:%4",idx,name,sqltype,value.toString())
      LET idx = tuple.idx
      LET fv = recv.getField(idx)
      IF tuple.subIdx > 0 THEN
        MYASSERT(fv.getType().getKind() == "RECORD")
        LET fv = fv.getField(tuple.subIdx)
      END IF
      --type check
      IF NOT fv.getType().isAssignableFrom(value.getType()) THEN
        DISPLAY SFMT("field %1 is not assignable , has type:%",
          name, fv.getType().toString())
        CONTINUE FOR
      END IF
      CALL fv.set(value)
    END FOR
END FUNCTION

FUNCTION assignResultsByPos(h base.SqlHandle,recv reflect.Value)
  DEFINE value reflect.Value
  DEFINE fv,rv reflect.Value
  DEFINE trec,tf,tfsub reflect.Type
  DEFINE fieldIndex,subIndex,pos,cnt,subcnt INT
  LET trec=recv.getType()
  LET cnt=trec.getFieldCount()
  FOR fieldIndex = 1 TO cnt
    LET tf = trec.getFieldType(fieldIndex)
    IF tf.getKind() == "RECORD" THEN
      LET rv=recv.getField(fieldIndex)
      LET subcnt = tf.getFieldCount()
      FOR subIndex = 1 TO subcnt
        LET pos=pos+1
        LET value = reflect.Value.copyOf(h.getResultValue(pos))
        LET tfsub = tf.getFieldType(subIndex)
        MYASSERT(tfsub.isAssignableFrom(value.getType()))
        LET fv=rv.getField(subIndex)
        CALL fv.set(value)
      END FOR
    ELSE
      LET pos=pos+1
      LET value = reflect.Value.copyOf(h.getResultValue(pos))
      MYASSERT(tf.isAssignableFrom(value.getType()))
      LET fv = recv.getField(fieldIndex)
      CALL fv.set(value)
    END IF
  END FOR
END FUNCTION

FUNCTION fillArrayWithQueryData(
  arr reflect.Value, name2Index T_2idxDICT, sql STRING,byPosition BOOLEAN,numFields INT)
  DEFINE h base.SqlHandle
  DEFINE cnt, mystatus INT
  DEFINE recv reflect.Value
  LET h = base.SqlHandle.create()
  CALL h.prepare(sql)
  CALL h.open()
  CALL h.fetch()
  LET cnt = h.getResultCount()
  IF byPosition THEN
    MYASSERT(cnt==numFields)
  END IF
  WHILE (mystatus := status) == 0
    CALL arr.appendArrayElement()
    LET recv = arr.getArrayElement(arr.getLength())
    IF byPosition THEN
      CALL assignResultsByPos(h,recv)
    ELSE
      CALL assignResultsByName(h,name2Index,recv,cnt)
    END IF
    CALL h.fetch()
  END WHILE
END FUNCTION
