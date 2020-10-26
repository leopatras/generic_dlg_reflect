#+ tests various assignments of RECORDs and ARRAY of RECORDs
#+ using reflection whenever the regular assignment is not possible
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT FGL utils
SCHEMA stores
TYPE T_1 RECORD LIKE customer.*
TYPE T_2 RECORD LIKE customer.*
TYPE T_3m RECORD LIKE customer.* --this RECORD has methods
TYPE T_4m RECORD LIKE customer.* --this RECORD has other methods
TYPE T_5 RECORD
  lname LIKE customer.lname,
  fname LIKE customer.fname,
  birthdate DATE,
  unrelated INT
END RECORD


TYPE T_1arr DYNAMIC ARRAY OF T_1
TYPE T_2arr DYNAMIC ARRAY OF T_2
TYPE T_3arr DYNAMIC ARRAY OF T_3m
TYPE T_4arr DYNAMIC ARRAY OF T_4m
TYPE T_5arr DYNAMIC ARRAY OF T_5
MAIN
  DEFINE t1 T_1, t2 T_2, t3 T_3m,t4 T_4m,t5 T_5
  DEFINE t1arr T_1arr
  DEFINE t2arr T_2arr
  DEFINE t3arr T_3arr
  DEFINE t4arr T_4arr
  DEFINE t5arr T_5arr
  LET t1.fname="Hans"
  LET t1.lname="Hansen"
  LET t2=t1
  MYASSERT(t2.fname="Hans")
  MYASSERT(t2.lname="Hansen")
  LET t3=t2 --this surprisingly works, reflection says no..so smells like
            --an inconsistency...a RECORD having no methods is assigned to
            --a RECORD having methods
  MYASSERT(t3.fname="Hans")
  MYASSERT(t3.lname="Hansen")
  LET t4=t3 --this surprisingly works, reflection says no..so smells like
            --an inconsistency...a RECORD having a set of methods is assigned to
            --a RECORD having a set of other methods
  MYASSERT(t4.fname="Hans")
  MYASSERT(t4.lname="Hansen")

  --LET t5=t1 --doesn't work, compiler rejected
  --in this case use the ByName helper
  CALL utils.copyRecordByName(reflect.Value.valueOf(t1),reflect.Value.valueOf(t5))
  MYASSERT(t5.fname="Hans")
  MYASSERT(t5.lname="Hansen")
  
  LET t1arr[1].fname="Hans"
  LET t1arr[2].lname="Hansen"

  --CALL t1arr.copyTo(t2arr) --this fails with "illegal argument"
                             --which isn't logical to me because a single RECORD                             --assignment is accepted by the compiler
  CALL utils.copyArrayOfRecord(reflect.Value.valueOf(t1arr),reflect.Value.valueOf(t2arr))
  --using this will fail and advise you to use copyArrayOfRecord()
  --CALL utils.copyArrayOfRecordByName(reflect.Value.valueOf(t1arr),reflect.Value.valueOf(t2arr))
  DISPLAY util.JSON.stringify(t2arr)
  MYASSERT(t2arr[1].fname="Hans")
  MYASSERT(t2arr[2].lname="Hansen")
  MYASSERT(t2arr.getLength()=2)
  CALL utils.copyArrayOfRecord(reflect.Value.valueOf(t2arr),reflect.Value.valueOf(t3arr))
  DISPLAY util.JSON.stringify(t3arr)
  MYASSERT(t3arr[1].fname="Hans")
  MYASSERT(t3arr[2].lname="Hansen")
  MYASSERT(t3arr.getLength()=2)
  CALL utils.copyArrayOfRecord(reflect.Value.valueOf(t3arr),reflect.Value.valueOf(t4arr))
  DISPLAY util.JSON.stringify(t4arr)
  MYASSERT(t4arr[1].fname="Hans")
  MYASSERT(t4arr[2].lname="Hansen")
  MYASSERT(t4arr.getLength()=2)
  --the following would fail: member count different
  --CALL utils.copyArrayOfRecord(reflect.Value.valueOf(t1arr),reflect.Value.valueOf(t5arr))
  --therefore we have the "ByName" variant
  CALL utils.copyArrayOfRecordByName(reflect.Value.valueOf(t1arr),reflect.Value.valueOf(t5arr))
  DISPLAY util.JSON.stringify(t5arr)
  MYASSERT(t5arr[1].fname="Hans")
  MYASSERT(t5arr[2].lname="Hansen")
  MYASSERT(t5arr.getLength()=2)
  DISPLAY "tests passed"
  LET t5arr[1].unrelated = 5
  MYASSERT(utils.getArrayRecEl(reflect.Value.valueOf(t5arr),1,"fname")=="Hans")
  MYASSERT(utils.getArrayRecElINT(reflect.Value.valueOf(t5arr),1,"unrelated")==5)
  LET t5arr[1].birthdate = TODAY
  MYASSERT(utils.getArrayRecElDATE(reflect.Value.valueOf(t5arr),1,"birthdate")==TODAY)
  CALL utils.setArrayRecEl(reflect.Value.valueOf(t5arr),1,"unrelated",6)
  MYASSERT(utils.getArrayRecElINT(reflect.Value.valueOf(t5arr),1,"unrelated")==6)
  MYASSERT(t5arr[1].unrelated=6)
END MAIN

FUNCTION (self T_3m) a_method()
END FUNCTION

FUNCTION (self T_4m) another_method()
END FUNCTION
