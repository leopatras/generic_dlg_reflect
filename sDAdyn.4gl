&include "myassert.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL fgldialog
IMPORT FGL sql2array

PUBLIC TYPE I_SingleTableDA INTERFACE
  addOnAction(d ui.Dialog, actionName STRING)
END INTERFACE

--define a set of interfaces with single methods custom RECORDs can implement
PUBLIC TYPE I_sDAdynOnDAEvent INTERFACE
  onDAevent(d ui.Dialog, row INT, event STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynBeforeRow INTERFACE
  BeforeRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynAfterRow INTERFACE
  AfterRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynDeleteRow INTERFACE
  DeleteRow(d ui.Dialog, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynOnActionInDA INTERFACE
  OnActionInDA(actionName STRING, row INT) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynInitInput INTERFACE
  initINPUT(sdi I_SingleTableDA, d ui.Dialog) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynBeforeField INTERFACE
  BeforeField(d ui.Dialog, fieldName STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynAfterField INTERFACE
  AfterField(d ui.Dialog, fieldName STRING, oldValue STRING) RETURNS(STRING)
END INTERFACE

PUBLIC TYPE I_sDAdynOnActionInInput INTERFACE
  OnActionInINPUT(d ui.Dialog, name STRING) RETURNS()
END INTERFACE

PUBLIC TYPE I_sDAdynOnInputEvent INTERFACE
  onINPUTevent(
    d ui.Dialog, ev STRING, fieldName STRING, value STRING)
    RETURNS(BOOLEAN, STRING)
END INTERFACE

PUBLIC TYPE I_sDAdynInsertUpdate INTERFACE
  insertOrUpdate(update BOOLEAN) RETURNS()
END INTERFACE

PUBLIC TYPE T_initDA FUNCTION(sdi I_SingleTableDA, d ui.Dialog) RETURNS()

PUBLIC TYPE T_SingleTableDA RECORD
  delegateDA reflect.Value,
  initDA T_initDA,
  dlgDA ui.Dialog,
  browseForm STRING,
  browseRecord STRING,
  browseTitle STRING,
  filterForm STRING,
  filterTitle STRING,
  hasFilter BOOLEAN,
  inputForm STRING,
  inputTitle STRING,
  filterInitially BOOLEAN,
  sqlAll STRING,
  sqlFilterBase STRING, --if not set, this defaults to sqlAll
  filter DYNAMIC ARRAY OF RECORD
    name STRING, -- a column name
    value STRING
  END RECORD,
  browseFormTextOrig STRING, --private
  hasAppend BOOLEAN, --private
  hasUpdate BOOLEAN, --private
  hasDelete BOOLEAN --private
END RECORD

TYPE T_fields DYNAMIC ARRAY OF RECORD
  name STRING, -- a column name
  type STRING -- a column type
END RECORD

PRIVATE FUNCTION implementsIF(
  el reflect.Value, ifval reflect.Value)
  RETURNS BOOLEAN
  IF el IS NULL THEN
    RETURN FALSE
  END IF
  MYASSERT(ifval IS NOT NULL)
  IF ifval.getType().isAssignableFrom(el.getType()) THEN
    CALL ifval.set(el)
    RETURN TRUE
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION (self T_SingleTableDA)
  checkUpdateDelete(
  d ui.Dialog, arrval reflect.Value)
  DEFINE empty BOOLEAN
  LET empty = arrval.getLength() == 0
  IF self.hasUpdate THEN
    CALL d.setActionHidden("update", empty)
  END IF
  IF self.hasDelete THEN
    CALL d.setActionHidden("delete", empty)
  END IF
END FUNCTION

PRIVATE FUNCTION (self T_SingleTableDA) getSQLFilterBase() RETURNS STRING
  DEFINE sqlFilterBase STRING
  LET sqlFilterBase =
    IIF(self.sqlFilterBase IS NOT NULL, self.sqlFilterBase, self.sqlAll)
  MYASSERT(sqlFilterBase IS NOT NULL)
  RETURN (sqlFilterBase)
END FUNCTION

FUNCTION (self T_SingleTableDA) addOnAction(d ui.Dialog, actionName STRING)
  MYASSERT(actionName IS NOT NULL)
  CALL d.addTrigger(C_ON_ACTION || " " || actionName)
END FUNCTION

FUNCTION (self T_SingleTableDA) addOnAppend(d ui.Dialog)
  CALL d.addTrigger("ON APPEND")
  LET self.hasAppend = TRUE
END FUNCTION

FUNCTION (self T_SingleTableDA) addOnDelete(d ui.Dialog)
  CALL d.addTrigger("ON DELETE")
  LET self.hasDelete = TRUE
END FUNCTION

FUNCTION (self T_SingleTableDA) addOnUpdate(d ui.Dialog)
  CALL d.addTrigger("ON UPDATE")
  LET self.hasDelete = TRUE
END FUNCTION

FUNCTION actionFromEvent(event STRING) RETURNS STRING
  DEFINE actionName STRING
  LET actionName = event.subString(LENGTH(C_ON_ACTION) + 2, event.getLength())
  DISPLAY "actionName:'", actionName, "'"
  RETURN actionName
END FUNCTION

FUNCTION (self T_SingleTableDA) browseArray(arrval reflect.Value)
  DEFINE event, title, ans, sql, actionName STRING
  DEFINE d ui.Dialog
  DEFINE done, filterActive, hasWherePart BOOLEAN
  DEFINE e, ifval reflect.Value
  DEFINE trec reflect.Type
  DEFINE fields T_fields
  DEFINE row, winId INT
  DEFINE ifvarOE I_sDAdynOnDAEvent
  DEFINE ifvarDelRow I_sDAdynDeleteRow
  DEFINE ifvarBR I_sDAdynBeforeRow
  DEFINE ifvarAR I_sDAdynAfterRow
  DEFINE ifvarOA I_sDAdynOnActionInDA
  --DEFINE ifvalOA reflect.Value
  --LET ifvalOA=reflect.Value.valueOf(ifvarOA)

  --DISPLAY arrval.getType().toString()
  LET trec = arrval.getType().getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  MYASSERT(self.sqlAll IS NOT NULL)
  CALL describeFields(trec, fields)
  LET winId = utils.openDynamicWindow(self.browseForm)
  --we need to store the original form text
  --because fgl_settitle changes the form text too
  LET self.browseFormTextOrig = utils.getFormTitle()
  IF self.browseTitle IS NULL THEN
    LET self.browseTitle = SFMT("%1: Browse", self.browseFormTextOrig)
  END IF
  IF self.hasFilter AND self.filterInitially THEN
    LET filterActive = TRUE
  ELSE
  END IF
  LET done = FALSE
  WHILE NOT done
    IF filterActive THEN
      CALL self.getFilter(fields) RETURNING sql, hasWherePart
      IF (sql IS NOT NULL)
        AND (NOT hasWherePart)
        AND (self.sqlFilterBase IS NULL) THEN
        --DISPLAY "filter wasn't actually set"
        LET filterActive = FALSE
      ELSE
        IF sql IS NULL THEN --user cancelled the filter
          IF arrval.getLength() == 0 THEN
            LET filterActive = FALSE
            LET sql = self.sqlAll
          END IF
        END IF
      END IF
    ELSE
      MYASSERT(self.sqlAll IS NOT NULL)
      LET sql = self.sqlAll
    END IF
    DISPLAY "sql:", sql
    IF sql IS NOT NULL THEN
      CALL sql2array.readIntoArray(arrval, sql)
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
    LET d = ui.Dialog.createDisplayArrayTo(fields, self.browseRecord)
    LET self.dlgDA = d
    CALL d.setArrayLength(self.browseRecord, arrval.getLength())
    CALL d.addTrigger("ON ACTION Exit")
    IF self.hasUpdate THEN
      CALL d.addTrigger("ON UPDATE")
    END IF
    IF self.hasAppend THEN
      CALL d.addTrigger("ON APPEND")
    END IF
    IF self.hasDelete THEN
      CALL d.addTrigger("ON DELETE")
    END IF
    CALL self.checkUpdateDelete(d, arrval)
    --CALL d.addTrigger("ON FILL BUFFER")
    IF self.hasFilter THEN
      CALL d.addTrigger("ON ACTION filter")
      CALL d.setActionText("filter", IIF(filterActive, "New Filter", "Filter"))
      IF filterActive THEN
        CALL d.addTrigger("ON ACTION clear_filter")
        CALL d.setActionText("clear_filter", "Clear Filter")
      END IF
    END IF
    CALL setArrayData(d, self.browseRecord, arrval)
    --call back the custom function for initial add on's of the DISPLAY ARRAY
    IF self.initDA IS NOT NULL THEN
      CALL self.initDA(self, d)
    END IF

    WHILE TRUE -- event loop for dialog d
      LET event = d.nextEvent()
      --DISPLAY "here:", event
      CASE
        WHEN event = "ON ACTION Exit"
          LET done = TRUE
          EXIT WHILE
        WHEN event = "BEFORE ROW"
          LET row = d.getCurrentRow(self.browseRecord)
          IF row > 0 THEN
            MYASSERT(row > 0 AND row <= arrval.getLength())
            IF implementsIF(
              arrval.getArrayElement(row), reflect.value.valueOf(ifvarBR)) THEN
              CALL ifvarBR.BeforeRow(d, row)
            ELSE
              DISPLAY "before row interface not implemented"
            END IF
          END IF
        WHEN event = "AFTER ROW"
          LET row = d.getCurrentRow(self.browseRecord)
          DISPLAY "after row:", row
          IF row > 0 THEN
            MYASSERT(row > 0 AND row <= arrval.getLength())
            IF implementsIF(
              arrval.getArrayElement(row), reflect.value.valueOf(ifvarAR)) THEN
              CALL ifvarAR.AfterRow(d, row)
            END IF
          END IF
        WHEN event = "ON UPDATE"
          LET row = d.getCurrentRow(self.browseRecord)
          CALL self.inputRow(d, fields, arrval.getArrayElement(row), "Update")
          CALL self.setBrowseTitle(filterActive)
        WHEN event = "ON APPEND"
          CALL arrval.appendArrayElement()
          LET row = arrval.getLength()
          LET int_flag = FALSE
          CALL self.inputRow(d, fields, arrval.getArrayElement(row), "Append")
          IF int_flag THEN
            CALL arrval.deleteArrayElement(row)
          END IF
          CALL self.checkUpdateDelete(d, arrval)
          CALL self.setBrowseTitle(filterActive)
        WHEN event = "ON DELETE"
          LET row = d.getCurrentRow(self.browseRecord)
          LET int_flag = FALSE
          MYASSERT(row > 0 AND row < arrval.getLength())
          IF implementsIF(
            arrval.getArrayElement(row),
            reflect.value.valueOf(ifvarDelRow)) THEN
            CALL ifvarDelRow.DeleteRow(d: d, row: row)
          END IF
          IF NOT int_flag THEN
            IF fgldialog.fgl_winQuestion(
                title: "Attention",
                message: "Do you really want to delete this RECORD?",
                ans: "yes",
                items: "yes|no",
                icon: "quest",
                dang: 0)
              == "yes" THEN
              CALL arrval.deleteArrayElement(row)
            ELSE
              --inform the dyn dialog that no deletion should occur
              LET int_flag = TRUE
            END IF
          END IF
          CALL self.checkUpdateDelete(d, arrval)
        WHEN event = "ON ACTION filter"
          LET filterActive = TRUE
          EXIT WHILE -- restarts the dialog d
        WHEN event = "ON ACTION clear_filter"
          LET filterActive = FALSE
          EXIT WHILE -- restarts the dialog d
        WHEN event = "AFTER DISPLAY"
          EXIT WHILE
        WHEN event.getIndexOf(C_ON_ACTION, 1) = 1
          DISPLAY "ON ACTION:",
            event,
            ",delegate NULL:",
            self.delegateDA IS NULL
          LET row = d.getCurrentRow(self.browseRecord)
          IF implementsIF(self.delegateDA, reflect.Value.valueOf(ifvarOA)) THEN
            CALL ifvarOA.OnActionInDA(actionFromEvent(event), row)
          ELSE
            DISPLAY "no interface for ifvarOA"
          END IF
          IF row > 0 AND row <= arrval.getLength() THEN
            DISPLAY "row:", row
            --CALL action trigger for the record element if present
            IF implementsIF(
              arrval.getArrayElement(row), reflect.Value.valueOf(ifvarOA)) THEN
              CALL ifvarOA.OnActionInDA(actionFromEvent(event), row)
            END IF
          END IF
        OTHERWISE
          LET row = d.getCurrentRow(self.browseRecord)
          IF row > 0 AND row <= arrval.getLength() THEN
            IF implementsIF(
              arrval.getArrayElement(row), reflect.value.valueOf(ifvarOE)) THEN
              CALL ifvarOE.onDAevent(d, row, event)
            ELSE
              DISPLAY "no DA ev IF"
            END IF
          END IF
      END CASE
    END WHILE
    CALL d.close()
  END WHILE
  INITIALIZE self.dlgDA TO NULL
  CALL utils.closeDynamicWindow(winId)
END FUNCTION

PRIVATE FUNCTION (self T_SingleTableDA) setBrowseTitle(filterActive BOOLEAN)
  DEFINE titleBase STRING
  CALL fgl_settitle(
    SFMT("%1 %2", self.browseTitle, IIF(filterActive, "filtered", "all")))
END FUNCTION

PRIVATE FUNCTION (self T_SingleTableDA) getFilterForm() RETURNS STRING
  DEFINE frm STRING
  LET frm = IIF(self.filterForm IS NOT NULL, self.filterForm, self.inputForm)
  RETURN frm
END FUNCTION

PRIVATE FUNCTION (self T_SingleTableDA)
  getFilter(
  fields T_fields)
  RETURNS(STRING, BOOLEAN)
  DEFINE d ui.Dialog
  DEFINE i, j, winId INT
  DEFINE s, q, sql STRING

  IF self.getFilterForm() IS NOT NULL THEN
    LET winId = utils.openDynamicWindow(self.filterForm)
    IF self.filterTitle IS NULL THEN
      LET self.filterTitle =
        SFMT("%1: Input filter criteria", utils.getFormTitle())
    END IF
  ELSE
    IF self.filterTitle IS NULL THEN
      LET self.filterTitle =
        SFMT("%1: Input filter criteria", self.browseFormTextOrig)
    END IF
  END IF
  CALL fgl_settitle(self.filterTitle)
  MESSAGE "Input filter criteria"

  LET d = ui.Dialog.createConstructByName(fields)
  -- restores the filter from the previous run,
  -- this implements a "CONSTRUCT WITHOUT DEFAULTS"
  FOR i = 1 TO self.filter.getLength()
    CALL d.setFieldValue(self.filter[i].name, self.filter[i].value)
  END FOR
  --
  CALL d.addTrigger("ON ACTION accept")
  CALL d.addTrigger("ON ACTION cancel")
  WHILE TRUE
    CASE d.nextEvent()
      WHEN "ON ACTION accept"
        CALL d.accept()
      WHEN "ON ACTION cancel"
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
  DISPLAY "filter SQL:", sql, ",q IS NULL:", q IS NOT NULL
  RETURN (sql),
    q IS NOT NULL -- Need to make braces around sql: The compiler reads RETURN sql as RETURN; SQL ..
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

--fills the internal dialog structure from an array passed by reflection
PRIVATE FUNCTION setArrayData(
  d ui.Dialog, screenRec STRING, arrval reflect.Value)
  DEFINE i, idx, len INT
  DEFINE recv, fv reflect.Value
  DEFINE trec reflect.Type
  DEFINE name, value STRING
  LET len = arrval.getLength()
  FOR i = 1 TO len
    LET recv = arrval.getArrayElement(i)
    LET trec = recv.getType()
    -- must set the current row before setting values
    CALL d.setCurrentRow(screenRec, i)
    FOR idx = 1 TO trec.getFieldCount()
      LET fv = recv.getField(idx)
      LET name = trec.getFieldName(idx)
      LET value = fv.toString()
      --DISPLAY sfmt("set field value for:%1=%2",name,value)
      CALL d.setFieldValue(name, value)
    END FOR
  END FOR
  IF arrval.getLength() > 0 THEN
    CALL d.setCurrentRow(screenRec, 1) -- TODO: should be done by the runtime
  END IF
END FUNCTION

PRIVATE FUNCTION (self T_SingleTableDA)
  inputRow(
  DA ui.Dialog, fields T_fields, recordVal reflect.Value, title STRING)
  DEFINE ev, err, curr, oldValue, name, actionName STRING
  DEFINE d ui.Dialog
  DEFINE i, winId INT
  DEFINE handled BOOLEAN
  DEFINE ifvarII I_sDAdynInitInput
  DEFINE ifvarBF I_sDAdynBeforeField
  DEFINE ifvarAF I_sDAdynAfterField
  DEFINE ifvarIU I_sDAdynInsertUpdate
  DEFINE ifvarOA I_sDAdynOnActionInInput
  DEFINE ifvarIE I_sDAdynOnInputEvent
  DEFINE fv, value reflect.Value
  --DEFINE trec reflect.Type
  IF self.inputForm.getLength() > 0 THEN
    --create a separate window , otherwise self.browseForm is used for INPUT
    LET winId = openDynamicWindow(self.inputForm)
    IF self.inputTitle IS NULL THEN
      LET self.inputTitle = utils.getFormTitle()
    END IF
  ELSE
    IF self.inputTitle IS NULL THEN
      LET self.inputTitle = self.browseFormTextOrig
    END IF
  END IF
  --for the title processing we could add another callback
  CALL fgl_settitle(SFMT("%1: %2", self.inputTitle, title))
  LET d = ui.Dialog.createInputByName(fields)
  CALL d.addTrigger("ON ACTION accept")
  CALL d.addTrigger("ON ACTION cancel")
  -- copy values from DisplayArray to Input
  FOR i = 1 TO fields.getLength()
    CALL d.setFieldValue(fields[i].name, DA.getFieldValue(fields[i].name))
  END FOR
  --call back the init method in INPUT
  --there one can add custom actions, hide fields etc
  IF implementsIF(recordVal, reflect.value.valueOf(ifvarII)) THEN
    CALL ifvarII.initINPUT(self, d)
  END IF

  WHILE TRUE
    LET ev = d.nextEvent()
    LET curr = d.getCurrentItem()
    --DISPLAY "ev:",ev,",curr:",curr
    CASE
      WHEN ev.getIndexOf(C_BEFORE_FIELD, 1) = 1
        MYASSERT(curr.getLength() > 0)
        LET oldValue = d.getFieldValue(curr)
        IF implementsIF(recordVal, reflect.value.valueOf(ifvarBF)) THEN
          CALL ifvarBF.BeforeField(d, curr)
        END IF
        CONTINUE WHILE
      WHEN ev.getIndexOf(C_AFTER_FIELD, 1) = 1
        --after field: we copy the dialog value into the fields value
        IF implementsIF(recordVal, reflect.value.valueOf(ifvarAF)) THEN
          MYASSERT(curr.getLength() > 0)
          LET fv = utils.getReflectFieldByName(recordVal, curr)
          MYASSERT(fv IS NOT NULL)
          LET value = reflect.Value.copyOf(d.getFieldValue(curr))
          MYASSERT(fv.getType().isAssignableFrom(value.getType()))
          CALL fv.set(value)
          CALL ifvarAF.AfterField(d, curr, oldValue) RETURNING err
          IF err IS NOT NULL THEN
            ERROR err
            CALL d.nextField(d.getCurrentItem())
          END IF
        END IF
        CONTINUE WHILE
      WHEN ev.getIndexOf(C_ON_ACTION, 1) = 1
        IF implementsIF(recordVal, reflect.value.valueOf(ifvarOA)) THEN
          CALL ifvarOA.OnActionInINPUT(d, actionFromEvent(ev))
        END IF
      OTHERWISE --pass any other event the onINPUTevent function
        IF implementsIF(recordVal, reflect.value.valueOf(ifvarIE)) THEN

          CALL ifvarIE.onINPUTevent(
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
      WHEN "ON ACTION accept"
        CALL d.accept()
      WHEN "ON ACTION cancel"
        LET int_flag = TRUE
        EXIT WHILE
      WHEN "AFTER INPUT"
        EXIT WHILE
    END CASE
  END WHILE
  IF NOT int_flag THEN
    -- copy values from Input to DisplayArray and to the backing RECORD
    FOR i = 1 TO fields.getLength()
      LET fv = recordVal.getField(i)
      LET name = fields[i].name
      LET value = reflect.Value.copyOf(d.getFieldValue(name))
      MYASSERT(fv.getType().isAssignableFrom(value.getType()))
      CALL fv.set(value)
      CALL da.setFieldValue(name, d.getFieldValue(fields[i].name))
    END FOR
    IF implementsIF(recordVal, reflect.value.valueOf(ifvarIU)) THEN
      CALL ifvarIU.insertOrUpdate(title == "Update")
    END IF
  END IF
  --TODO SQL errors ?
  CALL d.close()
  CALL utils.closeDynamicWindow(winId)
END FUNCTION
