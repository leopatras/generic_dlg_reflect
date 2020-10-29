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
  delete_img STRING
END RECORD

TYPE T_x DYNAMIC ARRAY OF RECORD
  x RECORD
  customer_num
    LIKE customer.customer_num ATTRIBUTE(json_name = "Customer Number"),
  fname LIKE customer.fname
  END RECORD,
  unrelated INT
END RECORD


MAIN
  DEFINE a T_customers
  DEFINE b T_customersSub
  DEFINE c T_x
  CALL utils.dbconnect()
  --demonstrates an ARRAY with a RECORD LIKE definition
  CALL readIntoArray(reflect.Value.valueOf(a), "select * from customer")
  DISPLAY util.JSON.stringify(a)
  --demonstrates an ARRAY with a RECORD LIKE definition as a sub record
  CALL readIntoArray(reflect.Value.valueOf(b), "select * from customer")
  DISPLAY util.JSON.stringify(b)
  --demonstrates an ARRAY of RECORD with having some LIKE members, and an unrelated member
  --(to be used in dialog code for example)
  --the relevant members are fetched "by name" (similar to JSON)
  --this case shows also how to retrieve a type attribute(json_name)
  CALL readIntoArray(reflect.Value.valueOf(c), "select * from customer")
  DISPLAY util.JSON.stringify(c)
END MAIN

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
FUNCTION readIntoArray(arr reflect.Value, sql STRING) RETURNS()
  DEFINE tarr, trec, tf, subtf reflect.Type
  DEFINE cnt, subcnt, fieldIndex, subIndex INT
  DEFINE name STRING
  --DEFINE json_name STRING
  DEFINE name2Index T_2idxDICT
  LET tarr = arr.getType()
  MYASSERT(tarr.getKind() == "ARRAY")
  CALL utils.clearReflectArray(arr)
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  LET trec = tarr.getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  LET cnt = trec.getFieldCount()
  --build a dictionary to have a record position for a record member name
  FOR fieldIndex = 1 TO cnt
    LET tf = trec.getFieldType(fieldIndex)
    IF tf.getKind() == "RECORD" THEN
      LET subcnt = tf.getFieldCount()
      FOR subIndex = 1 TO subcnt
        LET subtf = tf.getFieldType(subIndex)
        LET name = tf.getFieldName(subIndex)
        CALL setName2Index(name2Index, name, fieldIndex, subIndex)
      END FOR
    ELSE
      LET name = trec.getFieldName(fieldIndex)
      --LET json_name = tf.getAttribute("json_name")

      --DISPLAY "name:", name, " ", tf.toString(), " ", tf.getKind(),",index:",fieldIndex,",json_name:",json_name
      CALL setName2Index(name2Index, name, fieldIndex, 0)
    END IF
  END FOR
  CALL fillArrayWithQueryData(arr, name2Index, sql)
END FUNCTION

FUNCTION fillArrayWithQueryData(
  arr reflect.Value, name2Index T_2idxDICT, sql STRING)
  DEFINE h base.SqlHandle
  DEFINE i, idx, cnt, mystatus INT
  DEFINE name, sqltype STRING
  DEFINE value reflect.Value
  DEFINE recv, fv reflect.Value
  DEFINE tuple T_2idx
  LET h = base.SqlHandle.create()
  CALL h.prepare(sql)
  CALL h.open()
  CALL h.fetch()
  LET cnt = h.getResultCount()
  WHILE (mystatus := status) == 0
    CALL arr.appendArrayElement()
    LET recv = arr.getArrayElement(arr.getLength())
    FOR i = 1 TO cnt
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
    CALL h.fetch()
  END WHILE
END FUNCTION
