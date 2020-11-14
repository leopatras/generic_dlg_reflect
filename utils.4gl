OPTIONS
SHORT CIRCUIT
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT os
IMPORT FGL aui_const
IMPORT FGL fgldialog
PUBLIC CONSTANT C_AFTER_FIELD = "AFTER FIELD"
PUBLIC CONSTANT C_BEFORE_FIELD = "BEFORE FIELD"
PUBLIC CONSTANT C_ON_ACTION = "ON ACTION"
PUBLIC CONSTANT C_BEFORE_ROW = "BEFORE ROW"
PUBLIC CONSTANT C_AFTER_ROW = "AFTER ROW"
--aui_const has no PhantomColumn...and the other 2 are also missing
PUBLIC CONSTANT TAG_PhantomColumn = "PhantomColumn"
PUBLIC CONSTANT TAG_RecordView = "RecordView"
PUBLIC CONSTANT TAG_Link = "Link"

PUBLIC CONSTANT OA_delete = "delete"
PUBLIC CONSTANT OA_update = "update"

PUBLIC CONSTANT C_RECORD = "RECORD"
PUBLIC CONSTANT C_ARRAY = "ARRAY"
PUBLIC CONSTANT C_PRIMITIVE = "PRIMITIVE"
PUBLIC CONSTANT C_STORES_SCH = "stores.sch"
PUBLIC CONSTANT C_DATE = "DATE"
PUBLIC CONSTANT C_INTEGER = "DATE"

PUBLIC TYPE T_STRING_ARR DYNAMIC ARRAY OF STRING
PUBLIC TYPE T_STRING_DICT DICTIONARY OF STRING
PUBLIC TYPE T_INT_DICT DICTIONARY OF INT
PUBLIC TYPE T_fields DYNAMIC ARRAY OF RECORD
  name STRING, -- a column name
  type STRING -- a column type
END RECORD

PUBLIC TYPE T_Action RECORD
  name STRING,
  image STRING,
  text STRING
END RECORD

PUBLIC TYPE T_ACTION_ARR DYNAMIC ARRAY OF T_Action

PUBLIC TYPE T_SingleTableDAOptions RECORD
  delegateDA reflect.Value,
  arrayValue reflect.Value,
  browseForm STRING,
  browseRecord STRING,
  browseTitle STRING,
  filterForm STRING,
  filterTitle STRING,
  hasFilter BOOLEAN,
  autoPhantom BOOLEAN,
  inputForm STRING,
  inputTitle STRING,
  filterInitially BOOLEAN,
  sqlAll STRING,
  sqlFilterBase STRING, --if not set, this defaults to sqlAll
  sqlFetchByPosition BOOLEAN, --if not set, defaults to fetch by column name
  hasAppend BOOLEAN,
  hasUpdate BOOLEAN,
  hasDelete BOOLEAN,
  addClickableImages BOOLEAN,
  addToolBar BOOLEAN,
  qualifiedNames
      BOOLEAN --if set field names are referenced by "tableName.columnName",
    --and hence also the names in the ARRAY
    --this means RECORDs *must* have sub RECORD names *matching* the table names/aliases
    --, multiRowSelection BOOLEAN
    ,
  tabname STRING --optional tablename
END RECORD

DEFINE mShowStack BOOLEAN
DEFINE mWindows ARRAY[10] OF BOOLEAN

MAIN
  DEFINE d T_INT_DICT
  --LET d = readNamesFromScreenRecord("scr", "customers_singlerow.42f", TRUE)
  --LET d = readNamesFromScreenRecord("scr", "customers.42f",TRUE)
  --DISPLAY util.JSON.stringify(d)
  LET d = getColumnsForTable("customer")
  DISPLAY util.JSON.stringify(d)
  DISPLAY getColumnForTableByPos("customer", 1)
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

FUNCTION printRVInt(title STRING, val reflect.Value, indent STRING)
  DEFINE i INT
  VAR t = val.getType()
  DISPLAY indent,
    title,
    " ",
    t.toString(),
    " ",
    t.getKind(),
    "=",
    val.toString()
  LET indent = indent, "  "
  CASE
    WHEN t.getKind() == C_RECORD
      FOR i = 1 TO t.getFieldCount()
        CALL printRVInt(t.getFieldName(i), val.getField(i), indent)
      END FOR
    WHEN t.getKind() == C_ARRAY
      IF val.getLength() == 0 THEN
        DISPLAY indent, "[]"
      ELSE
        VAR el = val.getArrayElement(1)
        CALL printRVInt("1", el, indent)
      END IF
  END CASE
END FUNCTION

FUNCTION printRV(title STRING, val reflect.Value)
  CALL printRVInt(title, val, "")
END FUNCTION

FUNCTION getArrayRecField(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS reflect.Value
  DEFINE t, trec reflect.Type
  DEFINE el, field reflect.Value
  LET t = arrVal.getType()
  MYASSERT(t.getKind() == C_ARRAY)
  LET trec = t.getElementType()
  MYASSERT(trec.getKind() == C_RECORD)
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
  VAR field = getArrayRecField(arrVal, idx, member)
  VAR newValR = reflect.Value.valueOf(newVal)
  MYASSERT(field.getType().isAssignableFrom(newValR.getType()))
  CALL field.set(newValR)
END FUNCTION

--sample about how one could return a specific member type (INT)
FUNCTION getArrayRecElINT(
  arrVal reflect.Value, idx INT, member STRING)
  RETURNS INT
  DEFINE x INT
  VAR field = getArrayRecField(arrVal, idx, member)
  --we explicitly check for the type to avoid nonsense NULL return values
  IF NOT field.getType().toString().equals(C_INTEGER) THEN
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
  DEFINE x DATE
  VAR field = getArrayRecField(arrVal, idx, member)
  --we explicitly check for the type to avoid nonsense NULL return values
  IF NOT field.getType().toString().equals(C_DATE) THEN
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
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  --DISPLAY src.toString()
  VAR tsrc = src.getType()
  VAR tdst = dest.getType()
  MYASSERT(tsrc.getKind() == C_RECORD)
  MYASSERT(tdst.getKind() == C_RECORD)
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
  VAR cnt = tsrc.getFieldCount()
  VAR idx INT
  FOR idx = 1 TO cnt
    --name at position must be equal
    MYASSERT(tsrc.getFieldName(idx) == tdst.getFieldName(idx))
    VAR fsrc = src.getField(idx)
    VAR fdst = dest.getField(idx)
    --type check
    MYASSERT(fdst.getType().isAssignableFrom(fsrc.getType()))
    CALL fdst.set(fsrc)
  END FOR
END FUNCTION

--copies a record using reflection,relaxed version
--only record members which have matching names and matching types are copied
FUNCTION copyRecordByName(src reflect.Value, dest reflect.Value)
  DEFINE name2Index DICTIONARY OF INT
  --DISPLAY "toString:", tarr.toString(), ",kind:", tarr.getKind()
  --DISPLAY src.toString()
  VAR tsrc = src.getType()
  VAR tdst = dest.getType()
  MYASSERT(tsrc.getKind() == C_RECORD)
  MYASSERT(tdst.getKind() == C_RECORD)
  IF tdst.isAssignableFrom(tsrc) THEN
    --both src and dest have the *same* RECORD type:
    --you should rather use a direct RECORD assignment
    CALL myerrAndStackTrace(
      "RECORD types match: use a direct RECORD assignment instead(performance)")
  END IF
  --first iterate thru the source record
  VAR cnt = tsrc.getFieldCount()
  VAR idx, idxsrc INT
  FOR idx = 1 TO cnt
    LET name2Index[tsrc.getFieldName(idx)] = idx
  END FOR
  LET cnt = tdst.getFieldCount()
  --2nd iterate thru the destination record
  FOR idx = 1 TO cnt
    VAR name = tdst.getFieldName(idx)
    IF (idxsrc := name2Index[name]) IS NOT NULL THEN
      --we did find a matching name in the src record
      VAR fsrc = src.getField(idxsrc)
      VAR fdst = dest.getField(idx)
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
  DEFINE idx, j INT
  VAR tsrc = src.getType()
  VAR tdst = dest.getType()
  --DISPLAY src.toString() --not working, JSON would be cool
  --DISPLAY "kind:",tsrc.getKind(),tsrc.toString()
  MYASSERT(tsrc.getKind() == C_ARRAY)
  MYASSERT(tdst.getKind() == C_ARRAY)
  VAR trecsrc = tsrc.getElementType()
  VAR trecdst = tdst.getElementType()
  MYASSERT(trecsrc.getKind() == C_RECORD)
  MYASSERT(trecdst.getKind() == C_RECORD)
  IF trecdst.toString() == trecsrc.toString() THEN
    CALL myerrAndStackTrace(
      SFMT("RECORD types are equal (%1): use the build in copyTo() method of ARRAY instead of this function (performance)",
        trecdst.toString()))
  END IF
  MYASSERT(trecsrc.getFieldCount() == trecdst.getFieldCount())
  VAR numFields = trecsrc.getFieldCount()
  VAR directAssignable BOOLEAN
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
  VAR len = src.getLength()
  FOR idx = 1 TO len
    CALL dest.appendArrayElement()
    VAR elsrc = src.getArrayElement(idx)
    VAR eldst = dest.getArrayElement(idx)
    MYASSERT(dest.getLength() == idx AND eldst IS NOT NULL AND NOT eldst.isNull())
    IF directAssignable THEN
      DISPLAY "here"
      MYASSERT(eldst.getType().isAssignableFrom(elsrc.getType()))
      CALL eldst.set(elsrc)
    ELSE
      FOR j = 1 TO numFields
        VAR fieldsrc = elsrc.getField(j)
        VAR fielddst = eldst.getField(j)
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
  DEFINE idx, idxsrc, j INT
  DEFINE name2Index DICTIONARY OF INT
  DEFINE idxarrsrc, idxarrdst DYNAMIC ARRAY OF INT
  VAR tsrc = src.getType()
  VAR tdst = dest.getType()
  --DISPLAY src.toString() --not working, JSON would be cool
  --DISPLAY "kind:",tsrc.getKind(),tsrc.toString()
  MYASSERT(tsrc.getKind() == C_ARRAY)
  MYASSERT(tdst.getKind() == C_ARRAY)
  VAR trecsrc = tsrc.getElementType()
  VAR trecdst = tdst.getElementType()
  MYASSERT(trecsrc.getKind() == C_RECORD)
  MYASSERT(trecdst.getKind() == C_RECORD)
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
  VAR cnt = trecsrc.getFieldCount()
  FOR idx = 1 TO cnt
    LET name2Index[trecsrc.getFieldName(idx)] = idx
  END FOR
  LET cnt = trecdst.getFieldCount()
  --build 2 indexarrays to quickly iterate fields
  FOR idx = 1 TO cnt
    IF (idxsrc := name2Index[trecdst.getFieldName(idx)]) IS NOT NULL THEN
      --we did find a matching name in the src record
      VAR fieldtsrc = trecsrc.getFieldType(idxsrc)
      VAR fieldtdst = trecdst.getFieldType(idx)
      IF fieldtdst.isAssignableFrom(fieldtsrc) THEN
        --and types match too
        LET idxarrsrc[idxarrsrc.getLength() + 1] = idxsrc
        LET idxarrdst[idxarrdst.getLength() + 1] = idx
      END IF
    END IF
  END FOR
  CALL dest.clear()
  VAR len = src.getLength()
  VAR len2 = idxarrsrc.getLength()
  --now walk thru the source array and assign
  --members by field indexes
  FOR idx = 1 TO len
    CALL dest.appendArrayElement()
    VAR elsrc = src.getArrayElement(idx)
    VAR eldst = dest.getArrayElement(idx)
    FOR j = 1 TO len2
      VAR fieldsrc = elsrc.getField(idxarrsrc[j])
      VAR fielddst = eldst.getField(idxarrdst[j])
      MYASSERT(fielddst.getType().isAssignableFrom(fieldsrc.getType()))
      CALL fielddst.set(fieldsrc)
    END FOR
  END FOR
END FUNCTION

FUNCTION cutFieldPrefix(colName STRING) RETURNS STRING
  VAR idx = colName.getIndexOf(str: ".", startIndex: 1)
  RETURN IIF(idx > 0, colName.subString(idx + 1, colName.getLength()), colName)
END FUNCTION

--checks in depth 1
FUNCTION getRecursiveFieldByName(
  recv reflect.Value, name STRING, qualified BOOLEAN)
  RETURNS reflect.Value
  DEFINE idx INT
  DEFINE subname STRING
  VAR fv = recv.getFieldByName(name)
  IF fv IS NOT NULL THEN
    MYASSERT(fv.getType().getKind() == C_PRIMITIVE)
    RETURN fv
  END IF
  VAR trec = recv.getType()
  MYASSERT(trec.getKind() == C_RECORD)
  LET subname = IIF(qualified, cutFieldPrefix(name), name)
  --DISPLAY "subname:",subname,",fieldCnt:",trec.getFieldCount()
  FOR idx = 1 TO trec.getFieldCount()
    VAR tf = trec.getFieldType(idx)
    --DISPLAY "fieldName:",trec.getFieldName(idx),",subname:",subname
    IF tf.getKind() == C_RECORD THEN
      VAR fvidx = recv.getField(idx)
      VAR fvsub = fvidx.getFieldByName(subname)
      IF fvsub IS NOT NULL THEN
        IF qualified THEN
          VAR thisName STRING
          LET thisName = trec.getFieldName(idx), ".", subname
          IF NOT thisName.equals(name) THEN
            DISPLAY "thisName:", thisName, "<>", name
            CONTINUE FOR
          END IF
        END IF
        MYASSERT(fvsub.getType().getKind() == C_PRIMITIVE)
        RETURN fvsub
      END IF
    END IF
  END FOR
  RETURN NULL
END FUNCTION

FUNCTION getRecursiveTypeByName(
  trec reflect.Type, name STRING, qualified BOOLEAN)
  RETURNS reflect.Type
  DEFINE subname STRING
  MYASSERT(trec.getKind() == C_RECORD)
  VAR ft = trec.getFieldTypeByName(name)
  IF ft IS NOT NULL THEN
    MYASSERT(ft.getKind() == C_PRIMITIVE)
    RETURN ft
  END IF
  LET subname = IIF(qualified, cutFieldPrefix(name), name)
  --DISPLAY "subname:",subname,",fieldCnt:",trec.getFieldCount()
  VAR idx INT
  FOR idx = 1 TO trec.getFieldCount()
    VAR tf = trec.getFieldType(idx)
    --DISPLAY "fieldName:",trec.getFieldName(idx),",subname:",subname
    IF tf.getKind() == C_RECORD THEN
      VAR tfsub = tf.getFieldTypeByName(subname)
      IF tfsub IS NOT NULL THEN
        IF qualified THEN
          VAR thisName STRING
          LET thisName = trec.getFieldName(idx), ".", subname
          IF NOT thisName.equals(name) THEN
            DISPLAY "thisName:", thisName, "<>", name
            CONTINUE FOR
          END IF
        END IF
        MYASSERT(tfsub.getKind() == C_PRIMITIVE)
        RETURN tfsub
      END IF
    END IF
  END FOR
  RETURN NULL
END FUNCTION

FUNCTION getFormTitle() RETURNS STRING
  RETURN ui.Window.getCurrent().getForm().getNode().getAttribute("text")
END FUNCTION

FUNCTION changeFileExtension(fname STRING, newExt STRING)
  VAR ext = os.Path.extension(fname)
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

FUNCTION getFieldName(field om.DomNode, qualified BOOLEAN) RETURNS STRING
  RETURN IIF(qualified,
    field.getAttribute(A_name),
    field.getAttribute(A_colName))
END FUNCTION

FUNCTION scanTable(t om.DomNode, qualified BOOLEAN) RETURNS T_STRING_DICT
  DEFINE i, loop INT
  DEFINE name STRING
  DEFINE dict T_STRING_DICT
  FOR loop = 1 TO 2
    VAR l2
      = t.selectByTagName(IIF(loop == 1, TAG_TableColumn, TAG_PhantomColumn))
    FOR i = 1 TO l2.getLength()
      VAR tc = l2.item(i)
      LET name = getFieldName(tc, qualified)
      LET dict[name] = tc.getAttribute(A_sqlTabName)
    END FOR
  END FOR
  RETURN dict
END FUNCTION

FUNCTION getCurrentForm() RETURNS om.DomNode
  DEFINE win ui.Window
  DEFINE frm ui.Form
  MYASSERT((win := ui.Window.getCurrent()) IS NOT NULL)
  MYASSERT((frm := win.getForm()) IS NOT NULL)
  RETURN frm.getNode()
END FUNCTION

FUNCTION getTableByScreenRecord(
  formRoot om.DomNode, screenRec STRING)
  RETURNS om.DomNode
  MYASSERT(formRoot IS NOT NULL)
  MYASSERT(formRoot.getTagName() == TAG_Form)
  VAR l
    = formRoot.selectByPath(
      SFMT('//' || TAG_Table || '[@' || A_tabName || '="%1"]', screenRec))
  VAR len = l.getLength()
  IF len > 0 THEN
    MYASSERT(len) --there can be only one table with a screenRec
    VAR tabNode = l.item(1)
    MYASSERT(tabNode.getTagName() == TAG_Table)
    RETURN tabNode
  END IF
  RETURN NULL
END FUNCTION

--unfortunately there is no ui.XX function to retrieve
--complete information about screen record member names
--so we need eventually to open the .42f file and scan a bit the DOM
--if qualified is set, the returned names are "tableName.columnName"
FUNCTION readNamesFromScreenRecord(
  screenRec STRING, f42f STRING, qualified BOOLEAN)
  RETURNS T_STRING_DICT
  DEFINE doc om.DomDocument
  DEFINE root om.DomNode
  DEFINE win ui.Window
  DEFINE frmO ui.Form
  MYASSERT(screenRec IS NOT NULL)
  IF (win := ui.Window.getCurrent()) IS NULL
    OR (frmO := win.getForm()) IS NULL THEN
    MYASSERT((doc := om.DomDocument.createFromXmlFile(f42f)) IS NOT NULL)
    LET root = doc.getDocumentElement()
  ELSE
    LET root = frmO.getNode()
  END IF
  --first search: Table
  VAR tabNode = getTableByScreenRecord(root, screenRec)
  IF tabNode IS NOT NULL THEN
    RETURN scanTable(tabNode, qualified)
  END IF
  --2nd search: RecordView..we need the .42f
  IF doc IS NULL THEN
    MYASSERT((doc := om.DomDocument.createFromXmlFile(f42f)) IS NOT NULL)
    LET root = doc.getDocumentElement()
  END IF
  RETURN scanRecordView(root, screenRec, f42f, qualified)
END FUNCTION

FUNCTION getAllRealInputtableFieldNames(
  f42f STRING, qualified BOOLEAN)
  RETURNS T_STRING_ARR
  DEFINE doc om.DomDocument
  DEFINE namesdict T_STRING_DICT
  MYASSERT((doc := om.DomDocument.createFromXmlFile(f42f)) IS NOT NULL)
  VAR root = doc.getDocumentElement()
  CALL addFieldNamesToDict(namesdict, root, TAG_TableColumn, qualified)
  CALL addFieldNamesToDict(namesdict, root, TAG_FormField, qualified)
  CALL addFieldNamesToDict(namesdict, root, TAG_Matrix, qualified)
  RETURN namesdict.getKeys()
END FUNCTION

FUNCTION scanRecordView(
  root om.DomNode, screenRec STRING, f42f STRING, qualified BOOLEAN)
  DEFINE dict T_STRING_DICT
  DEFINE fieldIdDict T_NODE_DICT
  DEFINE i INT
  DEFINE name STRING
  LET fieldIdDict = getFieldIds(root)
  --DISPLAY "fieldIdDict:", util.JSON.stringify(fieldIdDict)
  VAR l
    = root.selectByPath(
      SFMT('//' || TAG_RecordView || '[@' || A_tabName || '="%1"]', screenRec))
  IF l.getLength() == 0 THEN
    CALL myerrAndStackTrace(
      SFMT("Can't find screen record '%1' in '%2'", screenRec, f42f))
  END IF
  VAR rv = l.item(1)
  LET l = rv.selectByTagName(TAG_Link)
  FOR i = 1 TO l.getLength()
    VAR link = l.item(i)
    VAR fieldId = link.getAttribute(A_fieldIdRef)
    VAR node = fieldIdDict[fieldId]
    IF qualified THEN
      LET name = node.getAttribute(A_name)
    ELSE
      LET name = link.getAttribute(A_colName)
      MYASSERT(name == node.getAttribute(A_colName))
    END IF
    MYASSERT(name IS NOT NULL)
    LET dict[name] = node.getAttribute(A_sqlTabName)
  END FOR
  RETURN dict
END FUNCTION

PRIVATE FUNCTION addFieldNamesToDict(
  fieldsDict T_STRING_DICT, root om.DomNode, tagName STRING, qualified BOOLEAN)
  DEFINE name STRING
  DEFINE i INT
  VAR l = root.selectByTagName(tagName)
  VAR len = l.getLength()
  FOR i = 1 TO len
    VAR node = l.item(i)
    LET name = getFieldName(node, qualified)
    LET fieldsDict[name] = node.getAttribute(A_sqlTabName)
  END FOR
END FUNCTION

TYPE T_NODE_DICT DICTIONARY OF om.DomNode

PRIVATE FUNCTION addFullNamesByFieldId(
  dict T_NODE_DICT, root om.DomNode, tagName STRING)
  DEFINE i INT
  VAR l = root.selectByTagName(tagName)
  VAR len = l.getLength()
  FOR i = 1 TO len
    VAR node = l.item(i)
    --VAR name = node.getAttribute(A_name)
    VAR fieldId = node.getAttribute(A_fieldId)
    LET dict[fieldId] = node
  END FOR
END FUNCTION

PRIVATE FUNCTION getFieldIds(root om.DomNode) RETURNS T_NODE_DICT
  DEFINE dict T_NODE_DICT
  CALL addFullNamesByFieldId(dict, root, TAG_FormField)
  CALL addFullNamesByFieldId(dict, root, TAG_Matrix)
  RETURN dict
END FUNCTION

--retrieves all inputtable fields in the current form
FUNCTION getInputColumnNamesAndTableNames(
  qualified BOOLEAN)
  RETURNS T_STRING_DICT
  DEFINE namesdict T_STRING_DICT
  DEFINE root om.DomNode
  LET root = ui.Window.getCurrent().getForm().getNode()
  CALL addFieldNamesToDict(namesdict, root, TAG_TableColumn, qualified)
  CALL addFieldNamesToDict(namesdict, root, TAG_PhantomColumn, qualified)
  CALL addFieldNamesToDict(namesdict, root, TAG_FormField, qualified)
  CALL addFieldNamesToDict(namesdict, root, TAG_Matrix, qualified)
  RETURN namesdict
END FUNCTION

FUNCTION checkCloseScreen()
  VAR w = ui.Window.forName("screen")
  IF w IS NULL THEN
    RETURN
  END IF
  IF w.getForm() IS NOT NULL THEN
    RETURN
  END IF
  --screen has no usage we can close it
  DISPLAY "CLOSE screen, no form"
  CLOSE WINDOW screen
END FUNCTION

FUNCTION addActionsToFormToolBar(actionList T_ACTION_ARR)
  DEFINE tb om.DomNode
  VAR form = getCurrentForm()
  VAR list = form.selectByTagName(TAG_ToolBar)
  VAR haveItems = FALSE
  IF list.getLength() > 0 THEN
    LET tb = list.item(1)
    LET haveItems = tb.selectByTagName(TAG_ToolBarItem).getLength() > 0
  ELSE
    --create one
    LET tb = form.createChild(TAG_ToolBar)
    CALL form.appendChild(tb)
  END IF
  IF haveItems THEN
    CALL tb.appendChild(tb.createChild(TAG_ToolBarSeparator))
  END IF
  VAR len = actionList.getLength()
  VAR i INT
  FOR i = 1 TO len
    VAR item = tb.createChild(TAG_ToolBarItem)
    VAR action = actionList[i]
    CALL item.setAttribute(A_name, action.name)
    IF action.image IS NOT NULL THEN
      CALL item.setAttribute(A_image, action.image)
    END IF
    IF action.text IS NOT NULL THEN
      CALL item.setAttribute(A_text, action.text)
    END IF
    CALL tb.appendChild(item)
  END FOR
END FUNCTION

-- createDisplayArrayForm {{{
FUNCTION createDisplayArrayForm(tabName STRING, name STRING, fields T_fields)
  VAR w = ui.Window.getCurrent()
  VAR f = w.createForm("test")
  VAR form = f.getNode()
  CALL form.setAttribute(A_name, name)
  CALL form.setAttribute(A_text, tabName)
  VAR window = form.getParent()
  CALL window.setAttribute(A_text, tabName)
  VAR grid = form.createChild(TAG_Grid)
  CALL grid.setAttribute(A_width, 1)
  CALL grid.setAttribute(A_height, 1)
  VAR table = grid.createChild(TAG_Table)
  CALL table.setAttribute(A_doubleClick, "update")
  CALL table.setAttribute(A_tabName, tabName)
  CALL table.setAttribute(A_pageSize, 10)
  CALL table.setAttribute(A_gridWidth, 1)
  CALL table.setAttribute(A_gridHeight, 1)
  VAR i INT
  FOR i = 1 TO fields.getLength()
    VAR formfield = table.createChild(TAG_TableColumn)
    VAR colName = fields[i].name
    VAR colType = fields[i].type
    CALL formfield.setAttribute(A_text, colName)
    CALL formfield.setAttribute(A_colName, colName)
    CALL formfield.setAttribute(A_name, tabName || "." || colName)
    CALL formfield.setAttribute(A_sqlType, colType)
    --CALL formfield.setAttribute("fieldId", i)
    CALL formfield.setAttribute(A_tabIndex, i + 1)
    VAR edit = formfield.createChild(TAG_Edit)
    CALL edit.setAttribute(A_width, bestWidth(colType))
  END FOR
  --CALL form.writeXml("test.42f")
END FUNCTION

FUNCTION bestWidth(t)
  DEFINE t STRING
  DEFINE i, j, len INT
  IF (i := t.getIndexOf('(', 1)) > 0 THEN
    IF (j := t.getIndexOf(',', i + 1)) == 0 THEN
      LET j = t.getIndexOf(')', i + 1)
    END IF
    LET len = t.subString(i + 1, j - 1)
    LET t = t.subString(1, i - 1)
  END IF
  CASE t
    WHEN "BOOLEAN"
      RETURN 1
    WHEN "TINYINT"
      RETURN 4
    WHEN "SMALLINT"
      RETURN 6
    WHEN "INTEGER"
      RETURN 11
    WHEN "BIGINT"
      RETURN 20
    WHEN "SMALLFLOAT"
      RETURN 14
    WHEN "FLOAT"
      RETURN 14
    WHEN "STRING"
      RETURN 20
    WHEN "DECIMAL"
      RETURN IIF(len IS NULL, 16, len + 2)
    WHEN "MONEY"
      RETURN IIF(len IS NULL, 16, len + 2)
    WHEN "CHAR"
      RETURN IIF(len IS NULL, 1, IIF(len > 20, 20, len))
    WHEN "VARCHAR"
      RETURN IIF(len IS NULL, 1, IIF(len > 20, 20, len))
    WHEN "DATE"
      RETURN 10
    OTHERWISE
      RETURN 20
  END CASE
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

TYPE T_schemaVal RECORD
  isSerial BOOLEAN,
  isNULL BOOLEAN,
  typeStr BOOLEAN
END RECORD

FUNCTION getSchemaVal(
  tabname STRING, colname STRING, dt INT, len INT)
  RETURNS T_schemaVal
  DEFINE schVal T_schemaVal
  --typeStr *should* match the type strings used in reflection
  --TODO need a test
  CASE dt MOD 256
    WHEN 0
      LET schVal.typeStr = SFMT("CHAR(%1)", len)
    WHEN 1
      LET schVal.typeStr = "SMALLINT"
    WHEN 2
      LET schVal.typeStr = C_INTEGER
    WHEN 3
      LET schVal.typeStr = "FLOAT"
    WHEN 4
      LET schVal.typeStr = "SMALLFLOAT"
    WHEN 5
      LET schVal.typeStr = "DECIMAL"
    WHEN 6
      --DISPLAY "serial for:", tabname, " ", colname
      LET schVal.isSerial = TRUE
      LET schVal.typeStr = C_INTEGER
    WHEN 7
      LET schVal.typeStr = C_DATE
    WHEN 8
      LET schVal.typeStr = "MONEY"
    WHEN 10
      LET schVal.typeStr = "DATETIME" --TODO len
    WHEN 11
      LET schVal.typeStr = "BYTE"
    WHEN 12
      LET schVal.typeStr = "TEXT"
    WHEN 13
      LET schVal.typeStr = "VARCHAR" --TODO len
    WHEN 14
      LET schVal.typeStr = "INTERVAL" --TODO len
    WHEN 15
      LET schVal.typeStr = "CHAR" --TODO len
    WHEN 16
      LET schVal.typeStr = "VARCHAR" --TODO len
    WHEN 17 --INT8
      LET schVal.typeStr = "BIGINT"
    WHEN 18 --SERIAL8
      LET schVal.isSerial = TRUE
      LET schVal.typeStr = "BIGINT"
    WHEN 45 --SQLBOOL
      LET schVal.typeStr = "BOOLEAN"
    WHEN 52
      LET schVal.typeStr = "BIGINT"
    WHEN 53 --BIGSERIAL
      LET schVal.isSerial = TRUE
      LET schVal.typeStr = "BIGINT"
    WHEN 201 --VARCHAR2
      LET schVal.typeStr = "VARCHAR" --TODO len
    WHEN 202 --NVARCHAR2
      LET schVal.typeStr = "VARCHAR" --TODO len
    OTHERWISE
      DISPLAY SFMT("unhandled type:%1 for table:%2 column:%3",
        dt MOD 256, tabname, colname)
  END CASE
  LET schVal.isNULL = dt / 256 == 1
  RETURN schVal
END FUNCTION

TYPE T_SCH_DICT DICTIONARY OF T_schemaVal
DEFINE m_schema DICTIONARY OF T_SCH_DICT
DEFINE m_colsByName DICTIONARY OF T_INT_DICT
DEFINE m_colsByPos DICTIONARY OF T_STRING_ARR

FUNCTION readSchema(schFile STRING)
  IF m_schema.getLength() > 0 THEN
    RETURN
  END IF
  VAR ch = base.Channel.create()
  CALL ch.setDelimiter("^")
  CALL ch.openFile(schFile, "r")
  VAR
    tabname, colname STRING,
    dt, len, pos INT
  WHILE ch.read([tabname, colname, dt, len, pos])
    VAR schVal T_schemaVal
    CALL getSchemaVal(tabname, colname, dt, len) RETURNING schVal.*
    LET m_schema[tabname][colname] = schVal
    LET m_colsByName[tabname][colname] = pos
    LET m_colsByPos[tabname][pos] = colname
  END WHILE
  MYASSERT(m_schema.getLength() > 0)
  CALL ch.close()
END FUNCTION

FUNCTION isSerialColumnForTable(colname STRING, tabname STRING)
  CALL readSchema(C_STORES_SCH)
  VAR schv T_schemaVal
  LET schv = m_schema[tabname][colname]
  RETURN schv.isSerial
END FUNCTION

FUNCTION getDataTypeStrOfColumn(colname STRING, tabname STRING)
  CALL readSchema(C_STORES_SCH)
  VAR schv T_schemaVal
  LET schv = m_schema[tabname][colname]
  RETURN schv.typeStr
END FUNCTION

FUNCTION getColumnsForTable(tabname STRING) RETURNS T_INT_DICT
  CALL readSchema(C_STORES_SCH)
  RETURN m_colsByName[tabname]
END FUNCTION

FUNCTION getColumnForTableByPos(tabname STRING, pos INT) RETURNS STRING
  CALL readSchema(C_STORES_SCH)
  RETURN m_colsByPos[tabname][pos]
END FUNCTION

FUNCTION filterNoRecords(filterActive BOOLEAN) RETURNS BOOLEAN
  DEFINE ans STRING
      CALL fgl_winButton(
	title: "No records found",
	"Please enter other criteria or show all data.",
	ans: "Input other filter criteria",
	items: "Input other filter criteria|Show all data",
	icon: "attention",
	dang: 0)
	RETURNING ans
      IF NOT ans.equals("Input other filter criteria") THEN
	LET filterActive = FALSE
      END IF
  RETURN filterActive
END FUNCTION

FUNCTION reallyDeleteRecords() RETURNS BOOLEAN
  VAR ans STRING
  LET ans=fgldialog.fgl_winQuestion(
        title: "Attention",
        message: "Do you really want to delete this RECORD?",
        ans: "yes",
        items: "yes|no",
        icon: "quest",
        dang: 0)
  RETURN ans=="yes"
END FUNCTION

FUNCTION checkToolBar(options T_SingleTableDAOptions)
  DEFINE actions T_ACTION_ARR
  DEFINE len INT
  IF NOT options.addToolBar THEN
    RETURN
  END IF
  VAR form = getCurrentForm()
  VAR tabNode = getTableByScreenRecord(form, options.browseRecord)
  IF tabNode IS NULL THEN
    --add navigation toolbar.. not really needed for tables
    LET len = actions.getLength() + 1
    LET actions[len].name = "firstrow"
    LET len = actions.getLength() + 1
    LET actions[len].name = "prevrow"
    LET len = actions.getLength() + 1
    LET actions[len].name = "nextrow"
    LET len = actions.getLength() + 1
    LET actions[len].name = "lastrow"
    CALL addActionsToFormToolBar(actions)
    CALL actions.clear()
  END IF
  IF options.hasUpdate THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = OA_update
    LET actions[len].image = "fa-edit"
  END IF
  IF options.hasDelete THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = OA_delete
    LET actions[len].image = "fa-trash-o"
  END IF
  IF options.hasAppend THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = "append"
    LET actions[len].image = "fa-plus"
  END IF
  DISPLAY ">>>>actions:", util.JSON.stringify(actions)
  IF actions.getLength() > 0 THEN
    CALL addActionsToFormToolBar(actions)
    CALL actions.clear()
  END IF
  IF options.hasFilter THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = "filter"
    LET actions[len].image = "fa-filter"
    LET actions[len].text = "Filter"
    LET len = actions.getLength() + 1
    LET actions[len].name = "clear_filter"
    LET actions[len].image = "clear_filter.svg"
    LET actions[len].text = "Clear F."
    CALL addActionsToFormToolBar(actions)
  END IF
END FUNCTION
