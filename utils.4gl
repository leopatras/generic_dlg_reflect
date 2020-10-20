&include "myassert.inc"
IMPORT reflect
PUBLIC CONSTANT C_AFTER_FIELD = "AFTER FIELD"
PUBLIC CONSTANT C_BEFORE_FIELD = "BEFORE FIELD"
PUBLIC CONSTANT C_ON_ACTION = "ON ACTION"
DEFINE mShowStack BOOLEAN

FUNCTION myerrAndStackTrace(errstr STRING)
  LET mShowStack = TRUE
  CALL myerr(errstr)
END FUNCTION

FUNCTION myerr(errstr STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile("<stderr>", "w")
  IF mShowStack THEN
    CALL ch.writeLine(
      SFMT("ERROR:%1,stack:\n%2", errstr, base.Application.getStackTrace()))
  ELSE
    CALL ch.writeLine(SFMT("ERROR:%1", errstr))
  END IF
  CALL ch.close()
  EXIT PROGRAM 1
END FUNCTION

FUNCTION dbconnect()
  DEFINE dbName STRING = "stores.dbs"
  DEFINE driver STRING = "sqlite"
  DEFINE db STRING
  LET db =
    IIF(NOT driver.equals("default"),
      SFMT("%1+driver='%2'", dbName, driver),
      dbName)
  DATABASE db
END FUNCTION

--copies a record using reflection, the RECORD member count must be equal,
--the names at a given member position must match and the members must be
--assignable
FUNCTION copyRecord(src reflect.Value, dest reflect.Value)
  DEFINE tsrc, tdst reflect.Type
  DEFINE fsrc, fdst reflect.Value
  DEFINE cnt, idx INT
  DEFINE directAssignable BOOLEAN
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  --DISPLAY src.toString()
  LET tsrc = src.getType()
  LET tdst = dest.getType()
  MYASSERT(tsrc.getKind() == "RECORD")
  MYASSERT(tdst.getKind() == "RECORD")
  IF tdst.isAssignableFrom(tsrc) THEN
    --both src and dest have the *same* RECORD type:
    --you should rather use a direct RECORD assignment
    CALL myerrAndStackTrace(
      "RECORD types match: use a direct RECORD assignment instead(performance)")
  END IF
  --number of elements must be equal
  MYASSERT(tsrc.getFieldCount() == tdst.getFieldCount())
  LET cnt = tsrc.getFieldCount()
  FOR idx = 1 TO cnt
    --name at position must be equal
    MYASSERT(tsrc.getFieldName(idx) == tdst.getFieldName(idx))
    LET fsrc = src.getField(idx)
    LET fdst = dest.getField(idx)
    --type check
    MYASSERT(fdst.getType().isAssignableFrom(fsrc.getType()))
    CALL fdst.set(fsrc)
  END FOR
END FUNCTION

--copies a record using reflection,relaxed version
--only record members which have matching names and matching types are copied
FUNCTION copyRecordByName(src reflect.Value, dest reflect.Value)
  DEFINE tsrc, tdst reflect.Type
  DEFINE fsrc, fdst reflect.Value
  DEFINE cnt, idx, idxsrc INT
  DEFINE name STRING
  DEFINE name2Index DICTIONARY OF INT
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  --DISPLAY src.toString()
  LET tsrc = src.getType()
  LET tdst = dest.getType()
  MYASSERT(tsrc.getKind() == "RECORD")
  MYASSERT(tdst.getKind() == "RECORD")
  IF tdst.isAssignableFrom(tsrc) THEN
    --both src and dest have the *same* RECORD type:
    --you should rather use a direct RECORD assignment
    CALL myerrAndStackTrace(
      "RECORD types match: use a direct RECORD assignment instead(performance)")
  END IF
  --first iterate thru the source record
  LET cnt = tsrc.getFieldCount()
  FOR idx = 1 TO cnt
    LET name2Index[tsrc.getFieldName(idx)] = idx
  END FOR
  LET cnt = tdst.getFieldCount()
  --2nd iterate thru the destination record
  FOR idx = 1 TO cnt
    LET name = tdst.getFieldName(idx)
    IF (idxsrc := name2Index[name]) IS NOT NULL THEN
      --we did find a matching name in the src record
      LET fsrc = src.getField(idxsrc)
      LET fdst = dest.getField(idx)
      IF fdst.getType().isAssignableFrom(fsrc.getType()) THEN
        --and types match  too
        CALL fdst.set(fsrc)
      END IF
    END IF
  END FOR
END FUNCTION

--copies to a destination ARRAY which must have the same number of
--RECORD members and the same order of names and types
--please clear the destination array for efficiency before calling this func
FUNCTION copyArrayOfRecord(src reflect.Value, dest reflect.Value)
  DEFINE tsrc, tdst, trecsrc, trecdst reflect.Type
  DEFINE elsrc, eldst, fieldsrc, fielddst reflect.Value
  DEFINE numFields, idx, j, len INT
  DEFINE directAssignable BOOLEAN
  LET tsrc = src.getType()
  LET tdst = dest.getType()
  --DISPLAY src.toString() --not working, JSON would be cool
  --DISPLAY "kind:",tsrc.getKind(),tsrc.toString()
  MYASSERT(tsrc.getKind() == "ARRAY")
  MYASSERT(tdst.getKind() == "ARRAY")
  LET trecsrc = tsrc.getElementType()
  LET trecdst = tdst.getElementType()
  MYASSERT(trecsrc.getKind() == "RECORD")
  MYASSERT(trecdst.getKind() == "RECORD")
  IF trecdst.toString() == trecsrc.toString() THEN
    CALL myerrAndStackTrace(
      SFMT("RECORD types are equal (%1): use the build in copyTo() method of ARRAY instead of this function (performance)",
        trecdst.toString()))
  END IF
  MYASSERT(trecsrc.getFieldCount() == trecdst.getFieldCount())
  LET numFields = trecsrc.getFieldCount()
  IF NOT trecdst.isAssignableFrom(trecsrc) THEN
    --come in here for ex if we have RECORDs with different methods
    --check field count, name matching and matching types
    FOR idx = 1 TO numFields
      --name at position must be equal
      MYASSERT(trecsrc.getFieldName(idx) == trecdst.getFieldName(idx))
      MYASSERT(trecdst.getFieldType(idx).isAssignableFrom(trecsrc.getFieldType(idx)))
    END FOR
  END IF
  --TODO: have a clearArray() method
  WHILE (len := dest.getLength()) > 0
    CALL dest.deleteArrayElement(len)
  END WHILE
  LET len = src.getLength()
  FOR idx = 1 TO len
    CALL dest.appendArrayElement()
    LET elsrc = src.getArrayElement(idx)
    LET eldst = dest.getArrayElement(idx)
    MYASSERT(dest.getLength()==idx AND eldst IS NOT NULL AND NOT eldst.isNull())
    IF FALSE AND directAssignable THEN
      DISPLAY "here"
      MYASSERT(eldst.getType().isAssignableFrom(elsrc.getType()))
      CALL eldst.set(eldst)
    ELSE
      FOR j = 1 TO numFields
        LET fieldsrc = elsrc.getField(j)
        LET fielddst = eldst.getField(j)
        MYASSERT(fielddst.getType().isAssignableFrom(fieldsrc.getType()))
        CALL fielddst.set(fieldsrc)
      END FOR
    END IF
  END FOR
END FUNCTION

--relaxed variant of copyArrayOfRecord, we can pass as destination
--an array which may have a completely different member count for the RECORD
--only RECORD members with a matching name and type are copied over
--please clear the destination array for efficiency before calling this func
FUNCTION copyArrayOfRecordByName(src reflect.Value, dest reflect.Value)
  DEFINE tsrc, tdst, trecsrc, trecdst, fieldtsrc, fieldtdst reflect.Type
  DEFINE elsrc, eldst, fieldsrc, fielddst reflect.Value
  DEFINE cnt, idx, idxsrc, j, len, len2 INT
  DEFINE name2Index DICTIONARY OF INT
  DEFINE idxarrsrc, idxarrdst DYNAMIC ARRAY OF INT
  LET tsrc = src.getType()
  LET tdst = dest.getType()
  --DISPLAY src.toString() --not working, JSON would be cool
  --DISPLAY "kind:",tsrc.getKind(),tsrc.toString()
  MYASSERT(tsrc.getKind() == "ARRAY")
  MYASSERT(tdst.getKind() == "ARRAY")
  LET trecsrc = tsrc.getElementType()
  LET trecdst = tdst.getElementType()
  MYASSERT(trecsrc.getKind() == "RECORD")
  MYASSERT(trecdst.getKind() == "RECORD")
  IF trecdst.toString() == trecsrc.toString() THEN
    CALL myerrAndStackTrace(
      SFMT("RECORD types are equal (%1): use the build in copyTo() method of ARRAY instead of this function (performance)",
        trecdst.toString()))
  END IF
  IF trecdst.isAssignableFrom(trecsrc) THEN
    CALL myerrAndStackTrace(
      "RECORD types match: use the copyArrayOfRecord() function instead of this function (performance)")
    RETURN
  END IF
  LET cnt = trecsrc.getFieldCount()
  FOR idx = 1 TO cnt
    LET name2Index[trecsrc.getFieldName(idx)] = idx
  END FOR
  LET cnt = trecdst.getFieldCount()
  --build 2 indexarrays to quickly iterate fields
  FOR idx = 1 TO cnt
    IF (idxsrc := name2Index[trecdst.getFieldName(idx)]) IS NOT NULL THEN
      --we did find a matching name in the src record
      LET fieldtsrc = trecsrc.getFieldType(idxsrc)
      LET fieldtdst = trecdst.getFieldType(idx)
      IF fieldtdst.isAssignableFrom(fieldtsrc) THEN
        --and types match too
        LET idxarrsrc[idxarrsrc.getLength() + 1] = idxsrc
        LET idxarrdst[idxarrdst.getLength() + 1] = idx
      END IF
    END IF
  END FOR
  --for huge existing destination arrays  this might be slow
  --TODO: have a clearArray() method
  WHILE (len := dest.getLength()) > 0
    CALL dest.deleteArrayElement(len)
  END WHILE
  LET len = src.getLength()
  LET len2 = idxarrsrc.getLength()
  --now walk thru the source array and assign
  --members by field indexes
  FOR idx = 1 TO len
    CALL dest.appendArrayElement()
    LET elsrc = src.getArrayElement(idx)
    LET eldst = dest.getArrayElement(idx)
    FOR j = 1 TO len2
      LET fieldsrc = elsrc.getField(idxarrsrc[j])
      LET fielddst = eldst.getField(idxarrdst[j])
      MYASSERT(fielddst.getType().isAssignableFrom(fieldsrc.getType()))
      CALL fielddst.set(fieldsrc)
    END FOR
  END FOR
END FUNCTION
