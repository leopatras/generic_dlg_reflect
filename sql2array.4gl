#+ shows how one can use the reflect API to read a SQL query into arbitrary arrays:
#+ the assignment is done "by name" (and the types must be suitable of course)
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils

SCHEMA stores

PUBLIC TYPE T_customer RECORD LIKE customer.*
PUBLIC TYPE T_customers DYNAMIC ARRAY OF T_customer

DEFINE a T_customers
TYPE T_x DYNAMIC ARRAY OF RECORD
  customer_num LIKE customer.customer_num ATTRIBUTE(json_name="Customer Number"),
  fname LIKE customer.fname,
  unrelated INT
END RECORD

DEFINE b T_x

TYPE T_INTdict DICTIONARY OF INT

MAIN
  CALL utils.dbconnect()
  --demonstrates an ARRAY with a RECORD LIKE
  CALL readIntoArray(reflect.Value.valueOf(a), "select * from customer")
  DISPLAY util.JSON.stringify(a)
  --demonstrates an ARRAY of RECORD with having some LIKE members, and an unrelated member
  --(to be used in dialog code for example)
  --the relevant members are fetched "by name" (similar to JSON)
  --this case shows also how to retrieve a type attribute(json_name) 
  CALL readIntoArray(reflect.Value.valueOf(b), "select * from customer")
  DISPLAY util.JSON.stringify(b)
END MAIN

--reads an sql query into the array and sets the members "by name"
FUNCTION readIntoArray(arr reflect.Value, sql STRING) RETURNS()
  DEFINE tarr, trec, tf reflect.Type
  DEFINE cnt, fieldIndex INT
  DEFINE name,json_name STRING
  DEFINE name2Index T_INTdict
  LET tarr = arr.getType()
  MYASSERT(tarr.getKind()=="ARRAY")
  CALL utils.clearReflectArray(arr)
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  LET trec = tarr.getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  LET cnt = trec.getFieldCount()
  --build a dictionary to have a record position for a record member name
  FOR fieldIndex = 1 TO cnt
    LET tf = trec.getFieldType(fieldIndex)
    LET name = trec.getFieldName(fieldIndex)
    LET json_name = tf.getAttribute("json_name") 
    DISPLAY "name:", name, " ", tf.toString(), " ", tf.getKind(),",index:",fieldIndex,",json_name:",json_name
    LET name2Index[name] = fieldIndex
  END FOR
  CALL fillArrayWithQueryData(arr, name2Index, sql)
END FUNCTION

FUNCTION fillArrayWithQueryData(arr reflect.Value, name2Index T_INTdict, sql STRING)
  DEFINE h base.SqlHandle
  DEFINE i, idx, cnt,mystatus INT
  DEFINE name,sqltype STRING
  DEFINE value reflect.Value
  DEFINE recv, fv reflect.Value
  LET h = base.SqlHandle.create()
  CALL h.prepare(sql)
  CALL h.open()
  CALL h.fetch()
  LET cnt = h.getResultCount()
  WHILE (mystatus:=status) == 0
    CALL arr.appendArrayElement()
    LET recv = arr.getArrayElement(arr.getLength())
    FOR i = 1 TO cnt
      LET name = h.getResultName(i)
      LET idx = name2Index[name]
      IF idx IS NULL THEN
        --DISPLAY "can't find index for name:",name
        CONTINUE FOR
      END IF
      LET sqltype=h.getResultType(i)
      LET value = reflect.Value.copyOf(h.getResultValue(i))
      --DISPLAY sfmt("did find index:%1 for %2,type:%3 value:%4",idx,name,sqltype,value.toString())
      LET fv = recv.getField(idx)
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
