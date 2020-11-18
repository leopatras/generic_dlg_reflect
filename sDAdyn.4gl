OPTIONS
SHORT CIRCUIT
&include "myassert.inc"
IMPORT reflect
IMPORT util
IMPORT os
IMPORT FGL utils
IMPORT FGL fgldialog
IMPORT FGL sql2array
IMPORT FGL aui_const
CONSTANT C_RECORD = "RECORD"
CONSTANT C_AUTO_FORM = "__auto__form__"
PUBLIC CONSTANT ON_ACTION_accept = "ON ACTION accept"
PUBLIC CONSTANT ON_ACTION_cancel = "ON ACTION cancel"
--the I_ prefix indicates an interface

--define a set of interfaces with single methods custom RECORDs can implement
PUBLIC TYPE I_InitDA INTERFACE
  InitDA(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
END INTERFACE

PUBLIC TYPE I_OnEventInDA INTERFACE
  OnEventInDA(d ui.Dialog, row INT, event STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_BeforeRow INTERFACE
  BeforeRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_CanUpdateRow INTERFACE
  CanUpdateRow(row INT) RETURNS(BOOLEAN)
END INTERFACE

PUBLIC TYPE I_CanDeleteRow INTERFACE
  CanDeleteRow(row INT) RETURNS(BOOLEAN)
END INTERFACE

PUBLIC TYPE I_AfterRow INTERFACE
  AfterRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_DeleteRow INTERFACE
  DeleteRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_OnActionInDA INTERFACE
  OnActionInDA(actionName STRING, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_InitINPUT INTERFACE
  InitINPUT(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
END INTERFACE

PUBLIC TYPE I_BeforeField INTERFACE
  BeforeField(d ui.Dialog, fieldName STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_AfterField INTERFACE
  AfterField(d ui.Dialog, fieldName STRING, oldValue STRING) RETURNS(STRING)
END INTERFACE

PUBLIC TYPE I_OnActionInINPUT INTERFACE
  OnActionInINPUT(d ui.Dialog, actionName STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_OnEventInINPUT INTERFACE
  OnEventInINPUT(
      d ui.Dialog, ev STRING, fieldName STRING, value STRING)
      RETURNS(BOOLEAN, STRING)
END INTERFACE

PUBLIC TYPE I_InsertOrUpdate INTERFACE
  InsertOrUpdate(update BOOLEAN) RETURNS()
END INTERFACE

PUBLIC TYPE I_InsertOrUpdateOfRow INTERFACE
  InsertOrUpdateOfRow(update BOOLEAN, row INT) RETURNS()
END INTERFACE

--PUBLIC TYPE T_MDOptions RECORD
--  sdopts T_SingleTableDAOptions,
--  MDarray reflect.Value
--END RECORD

--public interface for TM_SingleTableDA if
--TM_SingleTableDA is passed to a callback method
--(InitDA, InitINPUT)
PUBLIC TYPE I_SingleTableDA INTERFACE
  addOnAction(d ui.Dialog, actionName STRING),
  addOnActionRowBound(d ui.Dialog, actionName STRING)
END INTERFACE

--note this RECORD TYPE with methods is *not* public
--hence nobody can access the RECORD members from another module
--the public settable options are above defined in
--T_SingleTableDAOptions RECORD
TYPE TM_SingleTableDA RECORD
  o T_SingleTableDAOptions,
  dlgDA ui.Dialog,
  filter DYNAMIC ARRAY OF RECORD
    name STRING, -- a column name
    value STRING
  END RECORD,
  rowBounds DYNAMIC ARRAY OF STRING,
  browseFormTextOrig STRING,
  tableNamesINPUT T_INT_DICT,
  tableNamesDA T_INT_DICT
END RECORD

--hides/shows some build in actions and the custom
--row bound actions
FUNCTION (self TM_SingleTableDA)
    checkRowBoundActions(
    d ui.Dialog, arrVal reflect.Value)
  DEFINE empty BOOLEAN
  DEFINE i INT
  LET empty = arrVal.getLength() == 0
  IF self.o.hasUpdate THEN
    CALL d.setActionHidden(OA_update, empty)
  END IF
  IF self.o.hasDelete THEN
    CALL d.setActionHidden(OA_delete, empty)
  END IF
  FOR i = 1 TO self.rowBounds.getLength()
    CALL d.setActionHidden(self.rowBounds[i], empty)
    CALL d.setActionActive(self.rowBounds[i], NOT empty)
  END FOR
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA) getSQLFilterBase() RETURNS STRING
  DEFINE sqlFilterBase STRING
  LET sqlFilterBase =
      IIF(self.o.sqlFilterBase IS NOT NULL, self.o.sqlFilterBase, self.o.sqlAll)
  MYASSERT(sqlFilterBase IS NOT NULL)
  RETURN (sqlFilterBase)
END FUNCTION

FUNCTION (self TM_SingleTableDA)
    addOnActionRowBound(
    d ui.Dialog, actionName STRING)
  CALL self.addOnAction(d, actionName)
  --ensure the function is only called in the DISPLAY ARRAY context
  MYASSERT(self.dlgDA == d)
  LET self.rowBounds[self.rowBounds.getLength() + 1] = actionName
END FUNCTION

FUNCTION (self TM_SingleTableDA) addOnAction(d ui.Dialog, actionName STRING)
  UNUSED(self)
  MYASSERT(actionName IS NOT NULL)
  CALL d.addTrigger(C_ON_ACTION || " " || actionName)
END FUNCTION

FUNCTION (self TM_SingleTableDA) checkInterfaces()
  DEFINE i1 I_SingleTableDA
  MYASSERT(FALSE)
  LET i1 = self
END FUNCTION

FUNCTION actionFromEvent(event STRING) RETURNS STRING
  DEFINE actionName STRING
  LET actionName = event.subString(length(C_ON_ACTION) + 2, event.getLength())
  DISPLAY "actionName:'", actionName, "'"
  RETURN actionName
END FUNCTION

--entry function callable by the outer world to
--call the inner RECORD method
FUNCTION browseArray(options T_SingleTableDAOptions INOUT)
  DEFINE sDA TM_SingleTableDA
  --copy all options in a single step, hence  changes to
  --the options by user code do not affect the function of
  --the browseArray method
  LET sDA.o = options
  CALL sDA.browseArray()
END FUNCTION

--implements a DISPLAY ARRAY with a dynamic dialog and calls back
--into application code using interface methods in case they are
--implemented
--The DISPLAY ARRAY is surrounded by a WHILE loop to change filter conditions
--depending on the passed options
FUNCTION (self TM_SingleTableDA) browseArray() RETURNS()
  DEFINE event, ans, sql, prevSQL, rec STRING
  DEFINE d ui.Dialog
  DEFINE done, filterActive, hasWherePart BOOLEAN
  DEFINE arrval, el reflect.Value
  DEFINE trec reflect.Type
  DEFINE fields T_fields
  DEFINE row, winId INT
  DEFINE names T_INT_DICT
  DEFINE ifvarOE I_OnEventInDA
  DEFINE ifvarBR I_BeforeRow
  DEFINE ifvarUR I_CanUpdateRow
  DEFINE ifvarDR I_CanDeleteRow
  DEFINE ifvarAR I_AfterRow
  DEFINE ifvarOA I_OnActionInDA
  DEFINE delegateDA reflect.Value
  LET arrval = self.o.arrayValue
  LET delegateDA = self.o.delegateDA
  LET trec = arrval.getType().getElementType()
  MYASSERT(trec.getKind() == C_RECORD)
  MYASSERT(self.o.sqlAll IS NOT NULL)
  CALL self.checkAutoForm(trec)
  LET rec = self.o.browseRecord
  LET winId = utils.openDynamicWindow(self.o.browseForm)
  CALL utils.checkCloseScreen()
  CALL self.checkToolBar()
  CALL self.checkClickableImages()
  LET names = self.describeFieldsForRecord(rec, self.o.browseForm, trec, fields)
  CALL checkDATEjustify(trec, names, rec, self.o.qualifiedNames)
  DISPLAY "browseArray names:", util.JSON.stringify(names)
  DISPLAY "fields:", util.JSON.stringify(fields)
  --we need to store the original form text
  --because fgl_settitle changes the form text too
  LET self.browseFormTextOrig = utils.getFormTitle()
  IF self.o.browseTitle IS NULL THEN
    LET self.o.browseTitle = SFMT("%1: Browse", self.browseFormTextOrig)
  END IF
  IF self.o.hasFilter AND self.o.filterInitially THEN
    LET filterActive = TRUE
  ELSE
  END IF
  LET done = FALSE
  WHILE NOT done
    IF filterActive THEN
      CALL self.getFilter(trec) RETURNING sql, hasWherePart
      IF (sql IS NOT NULL)
          AND (NOT hasWherePart)
          AND (self.o.sqlFilterBase IS NULL) THEN
        --DISPLAY "filter wasn't actually set"
        LET filterActive = FALSE
      ELSE
        IF sql IS NULL THEN --user cancelled the filter
          IF prevSQL IS NOT NULL THEN
            LET sql = prevSQL
          ELSE
            LET filterActive = FALSE
            LET sql = self.o.sqlAll
          END IF
        END IF
      END IF
    ELSE
      MYASSERT(self.o.sqlAll IS NOT NULL)
      LET sql = self.o.sqlAll
    END IF
    DISPLAY "sql:", sql
    IF sql IS NOT NULL THEN
      CALL sql2array.readIntoArray(arrval, sql, self.o.sqlFetchByPosition)
      MESSAGE SFMT("Found %1 records", arrval.getLength())
    END IF
    IF filterActive AND arrval.getLength() == 0 THEN
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
      CONTINUE WHILE
    END IF
    CALL self.setBrowseTitle(filterActive)
    CALL self.rowBounds.clear()
    LET d = ui.Dialog.createDisplayArrayTo(fields, rec)
    LET self.dlgDA = d
    CALL d.setArrayLength(rec, arrval.getLength())
    CALL d.addTrigger("ON ACTION Exit")
    IF self.o.hasUpdate THEN
      CALL d.addTrigger("ON UPDATE")
    END IF
    IF self.o.hasAppend THEN
      CALL d.addTrigger("ON APPEND")
    END IF
    IF self.o.hasDelete THEN
      CALL d.addTrigger("ON DELETE")
    END IF
    --CALL d.addTrigger("ON FILL BUFFER")
    IF self.o.hasFilter THEN
      CALL d.addTrigger("ON ACTION filter")
      CALL d.setActionText("filter", IIF(filterActive, "New Filter", "Filter"))
      IF filterActive THEN
        CALL d.addTrigger("ON ACTION clear_filter")
        CALL d.setActionText("clear_filter", "Clear Filter")
      END IF
    END IF
    CALL self.setArrayData(d, rec, arrval, trec, names)
    CALL self.handleInitDA(d, arrval)
    CALL self.checkRowBoundActions(d, arrval)

    WHILE TRUE -- event loop for dialog d
      LET event = d.nextEvent()
      --DISPLAY "here:", event
      CASE
        WHEN event = "ON ACTION Exit"
          LET done = TRUE
          EXIT WHILE
        WHEN event = "BEFORE ROW"
          LET row = d.getCurrentRow(rec)
          IF delegateDA IS NOT NULL
              AND delegateDA.canAssignToVariable(ifvarBR) THEN
            CALL delegateDA.assignToVariable(ifvarBR)
            CALL ifvarBR.BeforeRow(d, row)
          END IF
          IF row > 0 THEN
            MYASSERT(row > 0 AND row <= arrval.getLength())
            LET el = arrval.getArrayElement(row)
            IF el.canAssignToVariable(ifvarBR) THEN
              CALL el.assignToVariable(ifvarBR)
              CALL ifvarBR.BeforeRow(d, row)
            END IF
            IF self.o.hasUpdate THEN
              IF el.canAssignToVariable(ifvarUR) THEN
                CALL el.assignToVariable(ifvarUR)
                VAR canUpdateRow = ifvarUR.CanUpdateRow(row)
                CALL d.setActionActive(OA_update, canUpdateRow)
                IF self.o.addClickableImages THEN
                  --TODO: if ON FILL BUFFER is used instead of filling the whole array
                  DISPLAY "WARNING: clickable images don't work with the CanUpdateRow method in a Table container"
                END IF
              END IF
            END IF
            IF self.o.hasDelete THEN
              IF el.canAssignToVariable(ifvarDR) THEN
                CALL el.assignToVariable(ifvarDR)
                VAR canDeleteRow = ifvarDR.CanDeleteRow(row)
                CALL d.setActionActive(OA_delete, canDeleteRow)
                IF self.o.addClickableImages THEN
                  DISPLAY "WARNING: clickable images don't work with the CanDeleteRow method in a Table container"
                END IF
              END IF
            END IF
          END IF
        WHEN event = "AFTER ROW"
          LET row = d.getCurrentRow(rec)
          IF delegateDA IS NOT NULL
              AND delegateDA.canAssignToVariable(ifvarAR) THEN
            CALL delegateDA.assignToVariable(ifvarAR)
            CALL ifvarAR.AfterRow(d, row)
          END IF
          IF row > 0 THEN
            MYASSERT(row > 0 AND row <= arrval.getLength())
            LET el = arrval.getArrayElement(row)
            IF el.canAssignToVariable(ifvarAR) THEN
              CALL el.assignToVariable(ifvarAR)
              CALL ifvarAR.AfterRow(d, row)
            END IF
          END IF
        WHEN event = "ON UPDATE"
          CALL self.handleUpdateRow(arrval, d, names, filterActive)
        WHEN event = "ON APPEND"
          CALL self.handleAppendRow(arrval, d, names, filterActive)
        WHEN event = "ON DELETE"
          CALL self.handleDeleteRow(arrval, d, names)
        WHEN event = "ON ACTION filter"
          LET prevSQL = IIF(filterActive, sql, NULL)
          LET filterActive = TRUE
          EXIT WHILE -- restarts the dialog d
        WHEN event = "ON ACTION clear_filter"
          LET prevSQL = NULL
          LET filterActive = FALSE
          EXIT WHILE -- restarts the dialog d
        WHEN event = "AFTER DISPLAY"
          EXIT WHILE
        WHEN event.getIndexOf(C_ON_ACTION, 1) = 1
          LET row = d.getCurrentRow(rec)
          --Call action trigger for the delegateDA if present
          IF delegateDA IS NOT NULL
              AND delegateDA.canAssignToVariable(ifvarOA) THEN
            CALL delegateDA.assignToVariable(ifvarOA)
            CALL ifvarOA.OnActionInDA(actionFromEvent(event), row)
          END IF
          IF row > 0 AND row <= arrval.getLength() THEN
            --CALL action trigger for the record element if present
            LET el = arrval.getArrayElement(row)
            IF el.canAssignToVariable(ifvarOA) THEN
              CALL el.assignToVariable(ifvarOA)
              CALL ifvarOA.OnActionInDA(actionFromEvent(event), row)
            END IF
          END IF
        OTHERWISE
          LET row = d.getCurrentRow(rec)
          IF delegateDA IS NOT NULL
              AND delegateDA.canAssignToVariable(ifvarOE) THEN
            CALL delegateDA.assignToVariable(ifvarOE)
            CALL ifvarOE.OnEventInDA(d, row, event)
          END IF
      END CASE
    END WHILE
    CALL d.close()
  END WHILE
  INITIALIZE self.dlgDA TO NULL
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

FUNCTION (self TM_SingleTableDA) handleInitDA(d ui.Dialog, arrval reflect.Value)
  DEFINE ifvarDA I_InitDA
  VAR delegateDA = self.o.delegateDA
  --call back the delegate for initial add on's of the DISPLAY ARRAY
  IF delegateDA IS NOT NULL AND delegateDA.canAssignToVariable(ifvarDA) THEN
    CALL delegateDA.assignToVariable(ifvarDA)
    CALL ifvarDA.InitDA(self, d)
  END IF
  --append an artificial element and check if we can call
  --the RECORDs InitDA method
  --of course access to the RECORD itself is useless
  --but we have a callback opportunity to add row bound actions
  CALL arrval.appendArrayElement()
  VAR lastrow = arrval.getLength()
  VAR el = arrval.getArrayElement(lastrow)
  IF el.canAssignToVariable(ifvarDA) THEN
    CALL el.assignToVariable(ifvarDA)
    CALL ifvarDA.InitDA(self, d)
  END IF
  CALL arrval.deleteArrayElement(lastrow)
END FUNCTION

FUNCTION (self TM_SingleTableDA) checkAutoForm(trec reflect.Type)
  IF self.o.browseForm IS NOT NULL THEN
    RETURN
  END IF
  --if we didn't pass a form we generate one
  VAR tabname = readTableNameFromRecordDesc(trec)
  DISPLAY "got tabname:", tabname
  IF tabname IS NULL THEN
    MYASSERT(self.o.tabname IS NOT NULL)
    LET tabname = self.o.tabname
  END IF
  VAR fields = sql2array.getDialogFieldsFromTable(tabname)
  MYASSERT(fields.getLength() <> 0)
  VAR fname = SFMT("%1_%2", C_AUTO_FORM, tabname)
  DISPLAY "generate:", fname, " in memory"
  OPEN WINDOW _tmp_ WITH 1 ROWS, 1 COLUMNS
  CALL utils.createDisplayArrayForm(tabname, fname, fields)
  VAR frmNode = utils.getCurrentForm()
  CALL frmNode.writeXml(SFMT("%1.42f", fname))
  CLOSE WINDOW _tmp_
  LET self.o.browseForm = fname
  LET self.o.browseRecord = tabname
END FUNCTION

--sets all unJUSTIFYed DATE colummns to be right aligned
FUNCTION checkDATEjustify(
    trec reflect.Type, names T_INT_DICT, screenRec STRING, qualified BOOLEAN)
  VAR formRoot = utils.getCurrentForm()
  VAR table = utils.getTableByScreenRecord(formRoot, screenRec)
  IF table IS NULL THEN
    RETURN
  END IF
  VAR l = table.selectByTagName(TAG_TableColumn)
  VAR len = l.getLength()
  VAR i INT
  FOR i = 1 TO len
    VAR tc = l.item(i)
    VAR name = utils.getFieldName(tc, qualified)
    IF names.contains(name) THEN
      VAR fieldType = utils.getRecursiveTypeByName(trec, name, qualified)
      IF fieldType.toString() == C_DATE THEN
        VAR firstChild = tc.getFirstChild()
        VAR justify = firstChild.getAttribute(A_justify)
        IF justify IS NULL THEN
          CALL firstChild.setAttribute(A_justify, "right")
        END IF
      END IF
    END IF
  END FOR
END FUNCTION

FUNCTION (self TM_SingleTableDA)
    handleUpdateRow(
    arrVal reflect.Value, d ui.Dialog, names T_INT_DICT, filterActive BOOLEAN)
  DEFINE currRow, savedrow INT
  DEFINE currVal, savedVal reflect.Value
  LET currRow = d.getCurrentRow(self.o.browseRecord)
  --backup the current currRow
  CALL arrVal.appendArrayElement()
  LET savedrow = arrVal.getLength()
  LET savedVal = arrVal.getArrayElement(savedrow)
  LET currVal = arrVal.getArrayElement(currRow)
  CALL utils.copyRecord(currVal, savedVal)
  CALL self.inputRow(arrVal.getArrayElement(currRow), "Update", names)
  IF int_flag THEN
    --user might have modified data in currVal inbetween
    CALL utils.copyRecord(savedVal, currVal)
  END IF
  --delete the saved row
  CALL arrVal.deleteArrayElement(savedrow)
  CALL self.setBrowseTitle(filterActive)
END FUNCTION

FUNCTION (self TM_SingleTableDA)
    handleAppendRow(
    arrVal reflect.Value, d ui.Dialog, names T_INT_DICT, filterActive BOOLEAN)
  DEFINE newRow INT
  CALL arrVal.appendArrayElement()
  LET newRow = arrVal.getLength()
  LET int_flag = FALSE
  CALL self.inputRow(arrVal.getArrayElement(newRow), "Append", names)
  IF int_flag THEN
    CALL arrVal.deleteArrayElement(newRow)
  ELSE
    CALL self.setClickableImages(d)
  END IF
  CALL self.checkRowBoundActions(d, arrVal)
  CALL self.setBrowseTitle(filterActive)
END FUNCTION

FUNCTION (self TM_SingleTableDA)
    callDeleteRow(
    el reflect.Value, d ui.Dialog, currRow INT, names T_INT_DICT)
  DEFINE ifvarDelRow I_DeleteRow
  WHENEVER ERROR RAISE
  IF self.o.delegateDA IS NOT NULL --first check array delegate
      AND self.o.delegateDA.canAssignToVariable(ifvarDelRow) THEN
    CALL ifvarDelRow.DeleteRow(d, currRow)
  ELSE --then element delegate
    IF el.canAssignToVariable(ifvarDelRow) THEN
      CALL el.assignToVariable(ifvarDelRow)
      CALL ifvarDelRow.DeleteRow(d, currRow)
    ELSE
      VAR sqlDATableCount = self.tableNamesDA.getKeys().getLength()
      MYASSERT(sqlDATableCount == 1)
      VAR tabName = self.tableNamesDA.getKeys()[1]
      CALL sql2array.deleteRecordInDB(el, names, tabName, self.o.qualifiedNames)
    END IF
  END IF
  WHENEVER ERROR STOP
END FUNCTION

FUNCTION (self TM_SingleTableDA)
    handleDeleteRow(
    arrVal reflect.Value, d ui.Dialog, names T_INT_DICT)
  DEFINE currRow INT
  DEFINE el reflect.Value
  LET currRow = d.getCurrentRow(self.o.browseRecord)
  LET int_flag = FALSE
  MYASSERT(currRow > 0 AND currRow <= arrVal.getLength())
  LET el = arrVal.getArrayElement(currRow)
  IF NOT int_flag THEN
    IF utils.reallyDeleteRecords() THEN
      TRY
        BEGIN WORK
        CALL self.callDeleteRow(el, d, currRow, names)
        COMMIT WORK
        CALL arrVal.deleteArrayElement(currRow)
      CATCH
        CALL fgl_winMessage(
            title: "The deletion failed failed",
            SFMT("status:%1,err_get:%2", status, err_get(status)),
            "error")
        ROLLBACK WORK
        LET int_flag = TRUE
      END TRY
    ELSE
      --inform the dyn dialog that no deletion should occur
      LET int_flag = TRUE
    END IF
  END IF
  CALL self.checkRowBoundActions(d, arrVal)
END FUNCTION

PRIVATE FUNCTION appendClickableImageColumn(
    t om.DomNode, colName STRING, actionName STRING)
  DEFINE tc, img om.DomNode
  MYASSERT(t IS NOT NULL)
  MYASSERT(colName IS NOT NULL)
  MYASSERT(actionName IS NOT NULL)
  LET tc = t.createChild(TAG_TableColumn)
  CALL tc.setAttribute("colName", colName)
  CALL tc.setAttribute("name", SFMT("formonly.%1", colName))
  CALL tc.setAttribute("sqlTabName", "formonly")
  LET img = tc.createChild(TAG_Image)
  CALL img.setAttribute("width", "2")
  CALL img.setAttribute("action", actionName)
END FUNCTION

CONSTANT IMG_MOD = "img_mod"
CONSTANT IMG_DEL = "img_del"

PRIVATE FUNCTION (self TM_SingleTableDA) checkClickableImages() RETURNS()
  DEFINE root, t om.DomNode
  DEFINE l om.NodeList
  DEFINE rec STRING
  LET rec = self.o.browseRecord
  IF NOT self.o.addClickableImages
      OR ((NOT self.o.hasDelete) AND (NOT self.o.hasUpdate)) THEN
    RETURN
  END IF
  LET root = ui.Window.getCurrent().getForm().getNode()
  LET l = root.selectByPath(SFMT('//Table[@tabName="%1"]', rec))
  IF l.getLength() == 0 THEN
    CALL myerrAndStackTrace(
        SFMT("No way to add clickable images:Can't find screen record '%1' in Table nodes in '%2'",
            rec, self.o.browseForm))
  END IF
  MYASSERT(l.getLength() == 1)
  LET t = l.item(1)
  IF self.o.hasUpdate THEN
    CALL appendClickableImageColumn(t, IMG_MOD, OA_update)
  END IF
  IF self.o.hasDelete THEN
    CALL appendClickableImageColumn(t, IMG_DEL, OA_delete)
  END IF
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA) setBrowseTitle(filterActive BOOLEAN)
  IF NOT self.o.hasFilter THEN
    CALL fgl_settitle(self.o.browseTitle)
  ELSE
    CALL fgl_settitle(
        SFMT("%1 %2", self.o.browseTitle, IIF(filterActive, "filtered", "all")))
  END IF
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA) getFilterForm() RETURNS STRING
  DEFINE frm STRING
  LET frm =
      IIF(self.o.filterForm IS NOT NULL, self.o.filterForm, self.o.inputForm)
  RETURN frm
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA)
    getFilter(
    trec reflect.Type)
    RETURNS(STRING, BOOLEAN)
  DEFINE d ui.Dialog
  DEFINE i, j, winId INT
  DEFINE s, q, sql STRING
  DEFINE fields T_fields
  DEFINE names T_INT_DICT
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
  CALL self.describeFieldsForINPUT(trec, fields) RETURNING names
  LET d = ui.Dialog.createConstructByName(fields)
  -- restores the filter from the previous run,
  -- this implements a "CONSTRUCT WITHOUT DEFAULTS"
  FOR i = 1 TO self.filter.getLength()
    CALL d.setFieldValue(self.filter[i].name, self.filter[i].value)
  END FOR
  --
  CALL d.addTrigger(ON_ACTION_accept)
  CALL d.addTrigger(ON_ACTION_cancel)
  WHILE TRUE
    CASE d.nextEvent()
      WHEN ON_ACTION_accept
        CALL d.accept()
      WHEN ON_ACTION_cancel
        CALL d.close()
        CALL utils.closeDynamicWindow(winId)
        RETURN NULL, FALSE
      WHEN "AFTER CONSTRUCT"
        EXIT WHILE
    END CASE
  END WHILE
  -- creates the query
  CALL self.filter.clear()
  FOR i = 1 TO fields.getLength()
    LET s = d.getQueryFromField(fields[i].name)
    -- getQueryFromField(name) returns a query expession from this field:
    -- example: returns 'fname matches "Antony*"' if name=="fname" and value=="Antony*"
    IF s IS NOT NULL THEN
      LET j = j + 1
      -- save the filter
      LET self.filter[j].name = fields[i].name
      LET self.filter[j].value = d.getFieldValue(fields[i].name)
      --
      IF q IS NOT NULL THEN
        LET q = q, " AND "
      END IF
      LET q = q, s CLIPPED
    END IF
  END FOR
  --DISPLAY q
  CALL d.close()
  LET sql = self.getSQLFilterBase()
  IF q IS NOT NULL THEN
    LET sql = sql, " WHERE ", q
  END IF
  CALL utils.closeDynamicWindow(winId)
  --DISPLAY "filter SQL:", sql, ",q IS NULL:", q IS NOT NULL
  RETURN (sql),
      q IS NOT NULL -- Need to make braces around sql: The compiler reads RETURN sql as RETURN; SQL ..
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA)
    describeFieldsForINPUT(
    trec reflect.type, fields T_fields)
    RETURNS T_INT_DICT
  DEFINE columnNames T_STRING_DICT
  LET columnNames =
      utils.getInputColumnNamesAndTableNames(self.o.qualifiedNames)
  DISPLAY "columnNames:", util.JSON.stringify(columnNames)
  RETURN self.describeFieldsINT(
      columnNames, trec, fields, "current form", NULL, self.tableNamesINPUT)
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA)
    describeFieldsForRecord(
    rec STRING, frm STRING, trec reflect.type, fields T_fields)
    RETURNS T_INT_DICT
  DEFINE f42f STRING
  DEFINE dict T_STRING_DICT
  LET f42f = utils.append42f(frm)
  LET dict = utils.readNamesFromScreenRecord(rec, f42f, self.o.qualifiedNames)
  DISPLAY "describeFieldsForRecord:", util.JSON.stringify(dict)
  RETURN self.describeFieldsINT(
      dict,
      trec,
      fields,
      SFMT("screen record '%1'", rec),
      rec,
      self.tableNamesDA)
END FUNCTION

PRIVATE FUNCTION assignFieldFromType(
    fields T_fields,
    t reflect.Type,
    dict T_STRING_DICT,
    name STRING,
    autoPhantom BOOLEAN,
    out T_INT_DICT,
    tableNames T_INT_DICT,
    where STRING)
  DEFINE idx INT
  MYASSERT(name IS NOT NULL)
  CASE
    WHEN dict.contains(name)
      LET idx = fields.getLength() + 1
      LET fields[idx].name = name
      LET fields[idx].type = t.toString()
      VAR sqltabName = dict[name]
      LET tableNames[sqltabName] = tableNames.getLength()
      CALL dict.remove(name)
      LET out[name] = idx --have the array field pos as value
    WHEN NOT autoPhantom
      CALL myerrAndStackTrace(
          SFMT("array record member '%1' not found in %2", name, where))
  END CASE
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA)
    describeFieldsINT(
    dict T_STRING_DICT,
    trec reflect.Type,
    fields T_fields,
    where STRING,
    rec STRING,
    tableNames T_INT_DICT)
    RETURNS T_INT_DICT
  DEFINE name, subname STRING
  DEFINE out T_INT_DICT
  DEFINE keys T_STRING_ARR
  DEFINE i, idx, j, cnt, subIdx, subCnt INT
  DEFINE tf reflect.Type
  DEFINE qualified BOOLEAN = self.o.qualifiedNames
  CALL fields.clear()
  CALL tableNames.clear()
  LET cnt = trec.getFieldCount()
  VAR autoPhantom = self.o.autoPhantom
  FOR idx = 1 TO cnt
    LET name = trec.getFieldName(idx)
    LET tf = trec.getFieldType(idx)
    IF tf.getKind() == C_RECORD THEN
      DISPLAY "RECORD with name:", name
      LET subCnt = tf.getFieldCount()
      FOR subIdx = 1 TO subCnt
        VAR subType = tf.getFieldType(subIdx)
        MYASSERT(subType.getKind().equals(C_RECORD) == FALSE)
        LET subname = tf.getFieldName(subIdx)
        LET subname = IIF(qualified, SFMT("%1.%2", name, subname), subname)
        CALL assignFieldFromType(
            fields, subType, dict, subname, autoPhantom, out, tableNames, where)
      END FOR
    ELSE
      CALL assignFieldFromType(
          fields, tf, dict, name, autoPhantom, out, tableNames, where)
    END IF
  END FOR
  IF rec IS NOT NULL THEN
    LET keys = dict.getKeys()
    --process the remaining names from the dictionary
    FOR i = 1 TO keys.getLength()
      LET name = keys[i]
      IF self.o.addClickableImages
          AND ((NOT qualified AND (name == IMG_MOD OR name == IMG_DEL))
              OR (qualified
                  AND (name == "formonly." || IMG_MOD
                      OR name == "formonly." || IMG_DEL))) THEN
        DISPLAY "auto create clickable image col:", name
      ELSE
        IF NOT self.o.autoPhantom THEN
          CALL myerrAndStackTrace(
              SFMT("WARNING: screen record member '%1' of '%2' not found in array record",
                  name, rec))
        END IF
      END IF
      LET j = fields.getLength() + 1
      --fill this in as a dummy field
      LET fields[j].name = name
      LET fields[j].type = "STRING"
    END FOR
  END IF
  DISPLAY "tableNames:", util.JSON.stringify(tableNames)
  RETURN out
END FUNCTION

PRIVATE FUNCTION describeFields(trec reflect.Type, fields T_fields)
  DEFINE i INT
  CALL fields.clear()
  --note this is by position , but one could do the same as in
  --sql2array and assign by name
  FOR i = 1 TO trec.getFieldCount()
    LET fields[i].name = trec.getFieldName(i)
    LET fields[i].type = trec.getFieldType(i).toString()
  END FOR
END FUNCTION

--assigns a value from the reflected field to the Dialog field
PRIVATE FUNCTION assignReflectValue(
    value reflect.Value, name STRING, names T_INT_DICT, d ui.Dialog)
  --only if the screen record contains the given (eventually qualified) name we assign
  IF NOT names.contains(name) THEN
    RETURN
  END IF
  --DISPLAY SFMT("set field value for:%1=%2", name, value.toString())
  CALL d.setFieldValue(name, value.toString())
END FUNCTION

--assign a value from the Dialog field to the reflected field
PRIVATE FUNCTION assignDlgValue(
    fv reflect.Value,
    name STRING,
    names T_INT_DICT,
    d ui.Dialog,
    DA ui.Dialog,
    browseNames T_INT_DICT)
  DEFINE value reflect.Value
  --only if the screen record contains the given (eventually qualified) name we assign
  IF names.contains(name) THEN
    --MYASSERT(names[name] == idx)
    --DISPLAY SFMT("set field value for:%1=%2", name, value)
    LET value = reflect.Value.copyOf(d.getFieldValue(name))
    MYASSERT(fv.getType().isAssignableFrom(value.getType()))
    CALL fv.set(value)
  ELSE
    --DISPLAY "assignDlgValue: no field:", name
  END IF
  IF browseNames.contains(name) THEN
    CALL DA.setFieldValue(name, fv.toString())
  END IF
END FUNCTION

FUNCTION (self TM_SingleTableDA) setClickableImages(d ui.Dialog)
  IF self.o.addClickableImages THEN
    IF self.o.hasUpdate THEN
      CALL d.setFieldValue(IMG_MOD, "fa-edit")
    END IF
    IF self.o.hasDelete THEN
      CALL d.setFieldValue(IMG_DEL, "fa-trash-o")
    END IF
  END IF
END FUNCTION

--fills the internal dialog structure from an array passed by reflection
PRIVATE FUNCTION (self TM_SingleTableDA)
    setArrayData(
    d ui.Dialog,
    screenRec STRING,
    arrval reflect.Value,
    trec reflect.Type,
    names T_INT_DICT)
  DEFINE i, len, fieldCnt INT
  DEFINE recv reflect.Value
  DEFINE qualified BOOLEAN = self.o.qualifiedNames
  LET len = arrval.getLength()
  LET fieldCnt = trec.getFieldCount()
  DISPLAY "setArrayData names:",
      util.JSON.stringify(names),
      ",fieldCnt:",
      fieldCnt
  FOR i = 1 TO len
    LET recv = arrval.getArrayElement(i)
    -- must set the current row before setting values
    CALL d.setCurrentRow(screenRec, i)
    CALL copyRecValues2DlgValues(recv, trec, fieldCnt, d, names, qualified)
    CALL self.setClickableImages(d)
  END FOR
  IF arrval.getLength() > 0 THEN
    CALL d.setCurrentRow(screenRec, 1) -- TODO: should be done by the runtime
  END IF
END FUNCTION

PRIVATE FUNCTION getSubValueAndName(
    fv reflect.Value,
    fvt reflect.Type,
    subIdx INT,
    name STRING,
    qualified BOOLEAN)
    RETURNS(reflect.Value, STRING)
  DEFINE subname STRING
  DEFINE subValue reflect.Value
  LET subname = fvt.getFieldName(subIdx)
  LET subname = IIF(qualified, SFMT("%1.%2", name, subname), subname)
  LET subValue = fv.getField(subIdx)
  RETURN subValue, subname
END FUNCTION

PRIVATE FUNCTION copyRecValues2DlgValues(
    recv reflect.Value,
    trec reflect.Type,
    fieldCnt INT,
    d ui.Dialog,
    names T_INT_DICT,
    qualified BOOLEAN)
  DEFINE idx, subIdx, subCnt INT
  DEFINE fv, fvsub reflect.Value
  DEFINE fvt reflect.Type
  DEFINE name, subname STRING
  {
  DEFINE i INT
  DEFINE name STRING
  FOR i = 1 TO trec.getFieldCount()
    LET name = trec.getFieldName(i)
    IF names.contains(name) THEN
      MYASSERT(names[name] == i)
      CALL d.setFieldValue(name, recv.getField(i).toString())
    END IF
  END FOR
  }
  FOR idx = 1 TO fieldCnt
    LET fv = recv.getField(idx)
    LET name = trec.getFieldName(idx)
    LET fvt = fv.getType()
    IF fvt.getKind() == C_RECORD THEN
      LET subCnt = fvt.getFieldCount()
      FOR subIdx = 1 TO subCnt
        CALL getSubValueAndName(
            fv, fvt, subIdx, name, qualified)
            RETURNING fvsub, subname
        CALL assignReflectValue(fvsub, subname, names, d)
      END FOR
    ELSE
      CALL assignReflectValue(fv, name, names, d)
    END IF
  END FOR
END FUNCTION

PRIVATE FUNCTION copyDlgValues2RecValues(
    recv reflect.Value,
    trec reflect.Type,
    d ui.Dialog,
    names T_INT_DICT,
    DA ui.Dialog,
    browseNames T_INT_DICT,
    qualified BOOLEAN)
  DEFINE idx, subIdx INT
  DEFINE name, subname STRING
  DEFINE fv, fvsub reflect.Value
  DEFINE fvt reflect.Type
  FOR idx = 1 TO trec.getFieldCount()
    LET name = trec.getFieldName(idx)
    LET fvt = trec.getFieldType(idx)
    LET fv = recv.getField(idx)
    IF fvt.getKind() == C_RECORD THEN
      FOR subIdx = 1 TO fvt.getFieldCount()
        CALL getSubValueAndName(
            fv, fvt, subIdx, name, qualified)
            RETURNING fvsub, subname
        CALL assignDlgValue(fvsub, subname, names, d, DA, browseNames)
      END FOR
    ELSE
      CALL assignDlgValue(fv, name, names, d, DA, browseNames)
    END IF
  END FOR
END FUNCTION

PRIVATE FUNCTION getRecordFieldByName(
    recv reflect.Value, fieldName STRING, qualified BOOLEAN)
    RETURNS reflect.Value
  DEFINE idx, subIdx INT
  DEFINE name, subname STRING
  DEFINE trec, fvt reflect.Type
  DEFINE fv, fvsub reflect.Value
  LET trec = recv.getType()
  FOR idx = 1 TO trec.getFieldCount()
    LET name = trec.getFieldName(idx)
    LET fvt = trec.getFieldType(idx)
    LET fv = recv.getField(idx)
    IF fvt.getKind() == C_RECORD THEN
      FOR subIdx = 1 TO fvt.getFieldCount()
        CALL getSubValueAndName(
            fv, fvt, subIdx, name, qualified)
            RETURNING fvsub, subname
        IF subname == fieldName THEN
          RETURN fvsub
        END IF
      END FOR
    ELSE
      IF name == fieldName THEN
        RETURN fv
      END IF
    END IF
  END FOR
  RETURN NULL
END FUNCTION

--assigns the dialog field name value to the RECORD member with
--this name
PRIVATE FUNCTION assignFieldValue(
    d ui.Dialog, fieldName STRING, recordVal reflect.Value, qualified BOOLEAN)
  DEFINE fv, value reflect.Value
  MYASSERT(fieldName.getLength() > 0)
  LET fv = getRecordFieldByName(recordVal, fieldName, qualified)
  MYASSERT(fv IS NOT NULL)
  LET value = reflect.Value.copyOf(d.getFieldValue(fieldName))
  DISPLAY "value of:", fieldName, " is:", value.toString()
  MYASSERT(fv.getType().isAssignableFrom(value.getType()))
  CALL fv.set(value)
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA)
    handleInsertOrUpdate(
    update BOOLEAN, recordVal reflect.Value, names T_INT_DICT)
  DEFINE ifvarIU I_InsertOrUpdate
  DEFINE ifvarIURow I_InsertOrUpdateOfRow
  DEFINE row INT
  WHENEVER ERROR RAISE --propagate error to the caller
  IF recordVal.canAssignToVariable(ifvarIU) THEN
    CALL recordVal.assignToVariable(ifvarIU)
    CALL ifvarIU.InsertOrUpdate(update)
  ELSE
    IF self.o.delegateDA IS NOT NULL
        AND self.o.delegateDA.canAssignToVariable(ifvarIURow) THEN
      CALL self.o.delegateDA.assignToVariable(ifvarIURow)
      LET row = self.dlgDA.getCurrentRow(self.o.browseRecord)
      CALL ifvarIURow.InsertOrUpdateOfRow(update, row)
    ELSE
      --we require that there is exclusively 1 sqlTabName is used
      --to generate an automatic INSERT/UPDATE sql statement based on
      --serial / PK column defs
      VAR sqlInputTableCount = self.tableNamesINPUT.getKeys().getLength()
      MYASSERT(sqlInputTableCount == 1)
      VAR tabName = self.tableNamesINPUT.getKeys()[1]
      --DISPLAY "tabName:", tabName
      IF update THEN
        CALL sql2array.updateRecordInDBWithNames(
            recordVal, names, tabName, self.o.qualifiedNames)
      ELSE
        CALL sql2array.insertRecordIntoDBWithNames(
            recordVal, names, tabName, self.o.qualifiedNames)
      END IF
    END IF
  END IF
  WHENEVER ERROR STOP
END FUNCTION

--implents an INPUT with a dynamic dialog and calls
--various interface methods of recordVal in application code
--if they are implemented
PRIVATE FUNCTION (self TM_SingleTableDA)
    inputRow(
    recordVal reflect.Value, title STRING, browseNames T_INT_DICT)
  DEFINE ev, err, curr, oldValue STRING
  DEFINE d ui.Dialog
  DEFINE winId INT
  DEFINE handled BOOLEAN
  DEFINE ifvarII I_InitINPUT
  DEFINE ifvarBF I_BeforeField
  DEFINE ifvarAF I_AfterField
  DEFINE ifvarOA I_OnActionInINPUT
  DEFINE ifvarIE I_OnEventInINPUT
  DEFINE names T_INT_DICT
  DEFINE fields T_fields
  DEFINE trec reflect.Type
  LET trec = recordVal.getType()
  IF self.o.inputForm.getLength() > 0 THEN
    --create a separate window , otherwise self.browseForm is used for INPUT
    LET winId = openDynamicWindow(self.o.inputForm)
    IF self.o.inputTitle IS NULL THEN
      LET self.o.inputTitle = utils.getFormTitle()
    END IF
  ELSE
    IF self.o.inputTitle IS NULL THEN
      LET self.o.inputTitle = self.browseFormTextOrig
    END IF
  END IF
  --for the title processing we could add another callback
  CALL fgl_settitle(SFMT("%1: %2", self.o.inputTitle, title))
  CALL self.describeFieldsForINPUT(trec, fields) RETURNING names
  --DISPLAY "fields:",util.JSON.stringify(fields)
  LET d = ui.Dialog.createInputByName(fields)
  CALL d.addTrigger(ON_ACTION_accept)
  CALL d.addTrigger(ON_ACTION_cancel)
  -- copy values from Array to Input
  CALL copyRecValues2DlgValues(
      recordVal, trec, trec.getFieldCount(), d, names, self.o.qualifiedNames)
  --call back the init method in INPUT
  --there one can add custom actions, hide fields etc
  IF recordVal.canAssignToVariable(ifvarII) THEN
    CALL recordVal.assignToVariable(ifvarII)
    CALL ifvarII.InitINPUT(self, d)
  END IF
  LABEL again:
  WHILE TRUE
    LET ev = d.nextEvent()
    LET curr = d.getCurrentItem()
    DISPLAY "ev:", ev, ",curr:", curr
    CASE
      WHEN ev.getIndexOf(C_BEFORE_FIELD, 1) = 1
        MYASSERT(curr.getLength() > 0)
        LET oldValue = d.getFieldValue(curr)
        IF recordVal.canAssignToVariable(ifvarBF) THEN
          CALL recordVal.assignToVariable(ifvarBF)
          CALL ifvarBF.BeforeField(d, curr)
        END IF
        CONTINUE WHILE
      WHEN ev.getIndexOf(C_AFTER_FIELD, 1) = 1
        --after field: we copy the dialog value into the fields value
        IF recordVal.canAssignToVariable(ifvarAF) THEN
          CALL recordVal.assignToVariable(ifvarAF)
          CALL assignFieldValue(d, curr, recordVal, self.o.qualifiedNames)
          CALL ifvarAF.AfterField(d, curr, oldValue) RETURNING err
          IF err IS NOT NULL THEN
            ERROR err
            CALL d.nextField(d.getCurrentItem())
          END IF
        END IF
        CONTINUE WHILE
      WHEN ev.getIndexOf(C_ON_ACTION, 1) = 1
        IF recordVal.canAssignToVariable(ifvarOA) THEN
          CALL recordVal.assignToVariable(ifvarOA)
          CALL ifvarOA.OnActionInINPUT(d, actionFromEvent(ev))
        END IF
      OTHERWISE --pass any other event to the OnEventInINPUT function
        IF recordVal.canAssignToVariable(ifvarIE) THEN
          CALL recordVal.assignToVariable(ifvarIE)
          CALL ifvarIE.OnEventInINPUT(
                  d,
                  ev,
                  curr,
                  IIF(curr.getLength() > 0, d.getFieldValue(curr), NULL))
              RETURNING handled, err
          IF handled THEN
            CONTINUE WHILE
          END IF
          IF err IS NOT NULL THEN
            ERROR err
            CALL d.nextField(d.getCurrentItem())
          END IF
        END IF
    END CASE
    CASE ev
      WHEN ON_ACTION_accept
        CALL d.accept()
      WHEN ON_ACTION_cancel
        LET int_flag = TRUE
        EXIT WHILE
      WHEN "AFTER INPUT"
        EXIT WHILE
    END CASE
  END WHILE
  IF NOT int_flag THEN
    -- copy values from Input to DisplayArray and to the backing RECORD
    CALL copyDlgValues2RecValues(
        recordVal,
        trec,
        d,
        names,
        self.dlgDA,
        browseNames,
        self.o.qualifiedNames)
    TRY
      BEGIN WORK
      CALL self.handleInsertOrUpdate(title == "Update", recordVal, names)
      COMMIT WORK
    CATCH
      --CALL printRV("recordVal After Insert", recordVal)
      CALL fgl_winMessage(
          title: SFMT("The %1 failed", title),
          SFMT("status:%1,err_get:%2", status, err_get(status)),
          "error")
      ROLLBACK WORK
      --CALL fgl_winMessage(title: sfmt("The %1 failed",title), message: sqlca.sqlerrm,icon: "error")
      GOTO again
    END TRY
    --TODO: check SQL error here
    --we may have got an update in the insert (serial) so we need to sync the record
    --to the DA dialog values
    CALL copyRecValues2DlgValues(
        recv: recordVal,
        trec: trec,
        fieldCnt: trec.getFieldCount(),
        d: self.dlgDA,
        names: browseNames,
        qualified: self.o.qualifiedNames)
  END IF
  CALL d.close()
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

PRIVATE FUNCTION (self TM_SingleTableDA) checkToolBar()
  DEFINE actions T_ACTION_ARR
  DEFINE len INT
  IF NOT self.o.addToolBar THEN
    RETURN
  END IF
  VAR form = utils.getCurrentForm()
  VAR tabNode = utils.getTableByScreenRecord(form, self.o.browseRecord)
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
    CALL utils.addActionsToFormToolBar(actions)
    CALL actions.clear()
  END IF
  IF self.o.hasUpdate THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = OA_update
    LET actions[len].image = "fa-edit"
  END IF
  IF self.o.hasDelete THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = OA_delete
    LET actions[len].image = "fa-trash-o"
  END IF
  IF self.o.hasAppend THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = "append"
    LET actions[len].image = "fa-plus"
  END IF
  DISPLAY ">>>>actions:", util.JSON.stringify(actions)
  IF actions.getLength() > 0 THEN
    CALL utils.addActionsToFormToolBar(actions)
    CALL actions.clear()
  END IF
  IF self.o.hasFilter THEN
    LET len = actions.getLength() + 1
    LET actions[len].name = "filter"
    LET actions[len].image = "fa-filter"
    LET actions[len].text = "Filter"
    LET len = actions.getLength() + 1
    LET actions[len].name = "clear_filter"
    LET actions[len].image = "clear_filter.svg"
    LET actions[len].text = "Clear F."
    CALL utils.addActionsToFormToolBar(actions)
  END IF
END FUNCTION
