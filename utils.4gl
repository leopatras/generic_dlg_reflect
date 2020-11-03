OPTIONS
SHORT CIRCUIT
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT os
PUBLIC CONSTANT C_AFTER_FIELD = "AFTER FIELD"
PUBLIC CONSTANT C_BEFORE_FIELD = "BEFORE FIELD"
PUBLIC CONSTANT C_ON_ACTION = "ON ACTION"
PUBLIC CONSTANT C_BEFORE_ROW = "BEFORE ROW"
PUBLIC CONSTANT C_AFTER_ROW = "AFTER ROW"

PUBLIC TYPE T_STRINGARR DYNAMIC ARRAY OF STRING
--PUBLIC TYPE T_STRINGDICT DICTIONARY OF STRING
PUBLIC TYPE T_INT_DICT DICTIONARY OF STRING

DEFINE mShowStack BOOLEAN
DEFINE mWindows ARRAY[10] OF BOOLEAN

MAIN
  DEFINE d T_INT_DICT
  LET d = readNamesFromScreenRecord("scr", "customers_singlerow.42f", TRUE)
  --LET d = readNamesFromScreenRecord("scr", "customers.42f",TRUE)
  DISPLAY util.JSON.stringify(d)
END MAIN

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
  IF (base.Application.getResourceEntry(
    "dbi.database.stores.source")) IS NOT NULL THEN
    DATABASE stores
  ELSE
    LET db =
      IIF(NOT driver.equals("default"),
        SFMT("%1+driver='%2'", dbName, driver),
        dbName)
    DATABASE db
  END IF
END FUNCTION

FUNCTION getArrayRecField(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS reflect.Value
  DEFINE t, trec reflect.Type
  DEFINE el, field reflect.Value
  LET t = arrVal.getType()
  MYASSERT(t.getKind() == "ARRAY")
  LET trec = t.getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  MYASSERT(idx >= 1 AND idx <= arrVal.getLength())
  LET el = arrVal.getArrayElement(idx)
  IF (field := el.getFieldByName(member)) IS NULL THEN
    CALL myerrAndStackTrace(
      SFMT("Can't find RECORD member '%1' in array", member))
  END IF
  RETURN field
END FUNCTION

--function to get arbitray member values for an array index ( ARRAY OF RECORD )
FUNCTION getArrayRecEl(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS STRING
  DEFINE field reflect.Value
  LET field = getArrayRecField(arrVal, idx, member)
  RETURN field.toString()
END FUNCTION

--function to set arbitray member values for an array index ( ARRAY OF RECORD )
FUNCTION setArrayRecEl(
  arrVal reflect.Value, idx INT, member STRING, newVal STRING)
  RETURNS()
  DEFINE field, newValR reflect.Value
  LET field = getArrayRecField(arrVal, idx, member)
  LET newValR = reflect.Value.valueOf(newVal)
  MYASSERT(field.getType().isAssignableFrom(newValR.getType()))
  CALL field.set(newValR)
END FUNCTION

--sample about how one could return a specific member type (INT)
FUNCTION getArrayRecElINT(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS INT
  DEFINE field reflect.Value
  DEFINE x INT
  LET field = getArrayRecField(arrVal, idx, member)
  --we explicitly check for the type to avoid nonsense NULL return values
  IF NOT field.getType().toString().equals("INTEGER") THEN
    CALL myerrAndStackTrace(
      SFMT("member '%1' doesn't have type INTEGER, actual: %2",
        member, field.getType().toString()))
  END IF
  LET x = field.toString()
  RETURN x
END FUNCTION

--sample about how one could return a specific member type (DATE)
FUNCTION getArrayRecElDATE(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS INT
  DEFINE field reflect.Value
  DEFINE x DATE
  LET field = getArrayRecField(arrVal, idx, member)
  --we explicitly check for the type to avoid nonsense NULL return values
  IF NOT field.getType().toString().equals("DATE") THEN
    CALL myerrAndStackTrace(
      SFMT("member '%1' doesn't have type DATE, actual: %2",
        member, field.getType().toString()))
  END IF
  LET x = field.toString()
  RETURN x
END FUNCTION

--copies a record using reflection, the RECORD member count must be equal,
--the names at a given member position must match and the members must be
--assignable
FUNCTION copyRecord(src reflect.Value, dest reflect.Value)
  DEFINE tsrc, tdst reflect.Type
  DEFINE fsrc, fdst reflect.Value
  DEFINE cnt, idx INT
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  --DISPLAY src.toString()
  LET tsrc = src.getType()
  LET tdst = dest.getType()
  MYASSERT(tsrc.getKind() == "RECORD")
  MYASSERT(tdst.getKind() == "RECORD")
  {
  IF tdst.isAssignableFrom(tsrc) THEN
    --both src and dest have the *same* RECORD type:
    --you should rather use a direct RECORD assignment
    CALL myerrAndStackTrace(
      "RECORD types match: use a direct RECORD assignment instead(performance)")
  END IF
  }
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
  IF trecdst.isAssignableFrom(trecsrc) THEN
    LET directAssignable = TRUE
  ELSE
    --come in here for ex if we have RECORDs with different methods
    --check field count, name matching and matching types
    FOR idx = 1 TO numFields
      --name at position must be equal
      MYASSERT(trecsrc.getFieldName(idx) == trecdst.getFieldName(idx))
      MYASSERT(trecdst.getFieldType(idx).isAssignableFrom(trecsrc.getFieldType(idx)))
    END FOR
  END IF
  CALL dest.clear()
  LET len = src.getLength()
  FOR idx = 1 TO len
    CALL dest.appendArrayElement()
    LET elsrc = src.getArrayElement(idx)
    LET eldst = dest.getArrayElement(idx)
    MYASSERT(dest.getLength() == idx AND eldst IS NOT NULL AND NOT eldst.isNull())
    IF directAssignable THEN
      DISPLAY "here"
      MYASSERT(eldst.getType().isAssignableFrom(elsrc.getType()))
      CALL eldst.set(elsrc)
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
  CALL dest.clear()
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

FUNCTION getFormTitle() RETURNS STRING
  RETURN ui.Window.getCurrent().getForm().getNode().getAttribute("text")
END FUNCTION

FUNCTION changeFileExtension(fname, newExt)
  DEFINE fname, ext, newExt STRING
  LET ext = os.Path.extension(fname)
  LET fname = fname.subString(1, fname.getLength() - ext.getLength()), newExt
  RETURN fname
END FUNCTION

FUNCTION append42f(fname STRING) RETURNS STRING
  DEFINE f42f STRING
  IF os.Path.extension(fname).getLength() == 0 THEN
    LET f42f = fname, ".42f"
  ELSE
    LET f42f = changeFileExtension(fname, "42f")
  END IF
  DISPLAY "f42f:", f42f
  RETURN f42f
END FUNCTION

--unfortunately there is no ui.XX function to retrieve
--complete information about screen record member names
--so we need eventually to open the .42f file and scan a bit the DOM
--if qualified is set, the returned names are "tableName.columnName"
FUNCTION readNamesFromScreenRecord(
  screenRec STRING, f42f STRING, qualified BOOLEAN)
  RETURNS T_INT_DICT
  DEFINE doc om.DomDocument
  DEFINE root, t, tc, rv, link om.DomNode
  DEFINE l om.NodeList
  DEFINE i INT
  DEFINE name, fieldId STRING
  DEFINE win ui.Window
  DEFINE frmO ui.Form
  DEFINE dict T_INT_DICT
  DEFINE fieldIdDict DICTIONARY OF STRING
  MYASSERT(screenRec IS NOT NULL)
  IF (win := ui.Window.getCurrent()) IS NULL
    OR (frmO := win.getForm()) IS NULL THEN
    MYASSERT((doc := om.DomDocument.createFromXmlFile(f42f)) IS NOT NULL)
    LET root = doc.getDocumentElement()
  ELSE
    LET root = frmO.getNode()
  END IF
  --first search: Table
  LET l = root.selectByPath(SFMT('//Table[@tabName="%1"]', screenRec))
  IF l.getLength() == 1 THEN
    LET t = l.item(1)
    LET l = t.selectByTagName("TableColumn")
    FOR i = 1 TO l.getLength()
      LET tc = l.item(i)
      LET name =
        IIF(qualified, tc.getAttribute("name"), tc.getAttribute("colName"))
      LET dict[name] = i
    END FOR
    RETURN dict
  END IF
  --2nd search: RecordView..we need the .42f
  IF doc IS NULL THEN
    MYASSERT((doc := om.DomDocument.createFromXmlFile(f42f)) IS NOT NULL)
    LET root = doc.getDocumentElement()
  END IF
  IF qualified THEN
    LET fieldIdDict = getFieldIds(root)
    DISPLAY "fieldIdDict:", util.JSON.stringify(fieldIdDict)
  END IF
  LET l = root.selectByPath(SFMT('//RecordView[@tabName="%1"]', screenRec))
  IF l.getLength() == 0 THEN
    CALL myerrAndStackTrace(
      SFMT("Can't find screen record '%1' in '%2'", screenRec, f42f))
  END IF
  LET rv = l.item(1)
  LET l = rv.selectByTagName("Link")
  FOR i = 1 TO l.getLength()
    LET link = l.item(i)
    IF qualified THEN
      LET fieldId = link.getAttribute("fieldIdRef")
      LET name = fieldIdDict[fieldId]
    ELSE
      LET name = link.getAttribute("colName")
    END IF
    MYASSERT(name IS NOT NULL)
    LET dict[name] = i
  END FOR
  RETURN dict
END FUNCTION

PRIVATE FUNCTION addFieldNamesToDict(
  dict T_INT_DICT, root om.DomNode, tagName STRING, qualified BOOLEAN)
  DEFINE l om.NodeList
  DEFINE node om.DomNode
  DEFINE name STRING
  DEFINE i, len INT
  LET l = root.selectByTagName(tagName)
  LET len = l.getLength()
  FOR i = 1 TO len
    LET node = l.item(i)
    LET name =
      IIF(qualified, node.getAttribute("name"), node.getAttribute("colName"))
    LET dict[name] = dict.getLength()
  END FOR
END FUNCTION

PRIVATE FUNCTION addFullNamesByFieldId(
  dict DICTIONARY OF STRING, root om.DomNode, tagName STRING)
  DEFINE l om.NodeList
  DEFINE node om.DomNode
  DEFINE name, fieldId STRING
  DEFINE i, len INT
  LET l = root.selectByTagName(tagName)
  LET len = l.getLength()
  FOR i = 1 TO len
    LET node = l.item(i)
    LET name = node.getAttribute("name")
    LET fieldId = node.getAttribute("fieldId")
    LET dict[fieldId] = name
  END FOR
END FUNCTION

PRIVATE FUNCTION getFieldIds(root om.DomNode) RETURNS(DICTIONARY OF STRING)
  DEFINE dict DICTIONARY OF STRING
  CALL addFullNamesByFieldId(dict, root, "FormField")
  CALL addFullNamesByFieldId(dict, root, "Matrix")
  RETURN dict
END FUNCTION

--retrieves all inputtable fields in the current form
FUNCTION getInputColNames(qualified BOOLEAN) RETURNS T_INT_DICT
  DEFINE dict T_INT_DICT
  DEFINE root om.DomNode
  LET root = ui.Window.getCurrent().getForm().getNode()
  CALL addFieldNamesToDict(dict, root, "TableColumn", qualified)
  CALL addFieldNamesToDict(dict, root, "FormField", qualified)
  CALL addFieldNamesToDict(dict, root, "Matrix", qualified)
  RETURN dict
END FUNCTION

--really not nice, but not a road block
--it would be very handy if we wouldn't need to code a function like that
FUNCTION openDynamicWindow(frm STRING) RETURNS INT
  DEFINE found STRING
  DEFINE i INT
  FOR i = 1 TO 10
    IF NOT mWindows[i] THEN
      LET mWindows[i] = TRUE
      LET found = TRUE
      EXIT FOR
    END IF
  END FOR
  IF NOT found THEN
    CALL myerrAndStackTrace("No more windows in openDynamicWindow")
  END IF
  CASE i
    WHEN 1
      OPEN WINDOW dynw1 WITH FORM frm
    WHEN 2
      OPEN WINDOW dynw2 WITH FORM frm
    WHEN 3
      OPEN WINDOW dynw3 WITH FORM frm
    WHEN 4
      OPEN WINDOW dynw4 WITH FORM frm
    WHEN 5
      OPEN WINDOW dynw5 WITH FORM frm
    WHEN 6
      OPEN WINDOW dynw6 WITH FORM frm
    WHEN 7
      OPEN WINDOW dynw7 WITH FORM frm
    WHEN 8
      OPEN WINDOW dynw8 WITH FORM frm
    WHEN 9
      OPEN WINDOW dynw9 WITH FORM frm
    WHEN 10
      OPEN WINDOW dynw10 WITH FORM frm
  END CASE
  RETURN i
END FUNCTION

FUNCTION closeDynamicWindow(winId STRING)
  CASE winId
    WHEN 0 --winId was never initialized
      RETURN
    WHEN 1
      CLOSE WINDOW dynw1
    WHEN 2
      CLOSE WINDOW dynw2
    WHEN 3
      CLOSE WINDOW dynw3
    WHEN 4
      CLOSE WINDOW dynw4
    WHEN 5
      CLOSE WINDOW dynw5
    WHEN 6
      CLOSE WINDOW dynw6
    WHEN 7
      CLOSE WINDOW dynw7
    WHEN 8
      CLOSE WINDOW dynw8
    WHEN 9
      CLOSE WINDOW dynw9
    WHEN 10
      CLOSE WINDOW dynw10
    OTHERWISE
      CALL myerrAndStackTrace(
        SFMT("Wrong winId:%1,must be between 0 and 10", winId))
  END CASE
  LET mWindows[winId] = FALSE
END FUNCTION
