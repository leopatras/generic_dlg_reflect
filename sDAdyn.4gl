&include "myassert.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL fgldialog
--define a set of interface methods custom RECORDs must implement
PUBLIC TYPE T_initDA FUNCTION(d ui.Dialog) RETURNS()
PUBLIC TYPE T_sDAdyn INTERFACE
  --initDA(d ui.Dialog) RETURNS (),
  initINPUT(d ui.Dialog) RETURNS(),
  BeforeRow(d ui.Dialog, row INT) RETURNS(),
  AfterRow(d ui.Dialog, row INT) RETURNS(),
  DeleteRow(d ui.Dialog, row INT) RETURNS (),
  onDAevent( d ui.Dialog, row INT, event STRING) RETURNS(),
  BeforeField(d ui.Dialog, fieldName STRING) RETURNS(),
  AfterField(d ui.Dialog, fieldName STRING, oldValue STRING) RETURNS(STRING),
  onINPUTevent(
    d ui.Dialog, ev STRING, fieldName STRING, value STRING)
    RETURNS(BOOLEAN, STRING)
END INTERFACE

TYPE T_fields DYNAMIC ARRAY OF RECORD
  name STRING, -- a column name
  type STRING -- a column type
END RECORD

--technique to return a specific interface out of a reflect representation
--(array member of a DYNAMIC ARRAY OF T_sDAdyn)
PRIVATE FUNCTION reflectVal2IF(el reflect.Value) RETURNS T_sDAdyn
  DEFINE ifval reflect.Value
  DEFINE ifvar T_sDAdyn
  --we "cast" a specific interface out of reflect
  LET ifval = reflect.Value.valueOf(ifvar)
  MYASSERT(el IS NOT NULL)
  MYASSERT(ifval.getType().isAssignableFrom(el.getType()))
  CALL ifval.set(el)
  RETURN ifvar
END FUNCTION

PRIVATE FUNCTION getRecordIFfromArray( arrval reflect.Value, row INT)
  RETURNS T_sDAdyn
  DEFINE ifvar T_sDAdyn
  --DISPLAY "row:",row
  IF row==0 THEN --append an element ,retrieve interface
    CALL arrval.appendArrayElement()
    LET ifvar=reflectVal2IF(arrval.getArrayElement(1))
  ELSE
    MYASSERT(row > 0 AND row <= arrval.getLength())
  LET ifvar = reflectVal2IF(arrval.getArrayElement(row))
  END IF
  RETURN ifvar
END FUNCTION

PRIVATE FUNCTION releaseRecordIFfromArray(arrval reflect.Value,row INT) RETURNS ()
  IF row==0 THEN --release the artificial element
    DISPLAY "delete dummy element at 1"
    CALL arrval.deleteArrayElement(1)
  END IF
END FUNCTION
FUNCTION checkUpdateDelete(d ui.Dialog, arrval reflect.Value)
  DEFINE empty BOOLEAN
  LET empty=arrval.getLength()==0
  CALL d.setActionHidden("update",empty)
  CALL d.setActionHidden("delete",empty)
END FUNCTION

FUNCTION browseArray(
  arrval reflect.Value, screenRec STRING, frm STRING, initDA T_initDA)
  DEFINE event STRING
  DEFINE d ui.Dialog
  DEFINE done BOOLEAN
  DEFINE e, ifval reflect.Value
  DEFINE ifvar T_sDAdyn
  DEFINE trec reflect.Type
  DEFINE fields T_fields
  DEFINE row INT
  DISPLAY arrval.getType().toString()
  LET trec = arrval.getType().getElementType()
  MYASSERT(trec.getKind() == "RECORD")
  CALL describeFields(trec, fields)

  OPEN WINDOW w WITH FORM frm
  LET done = FALSE
  WHILE NOT done
    LET d = ui.Dialog.createDisplayArrayTo(fields, screenRec)
    CALL d.setArrayLength(screenRec, arrval.getLength())
    CALL d.addTrigger("ON ACTION Done")
    CALL d.addTrigger("ON UPDATE")
    CALL d.addTrigger("ON APPEND")
    CALL d.addTrigger("ON DELETE")
    CALL checkUpdateDelete(d,arrval)
    --CALL d.addTrigger("ON FILL BUFFER")
    --CALL d.addTrigger("ON ACTION Filter")
    CALL fillArray(d, screenRec, arrval)
    --call back the method for custom init additions of the DISPLAY ARRAY
    CALL initDA(d)

    WHILE TRUE -- a event loop for dialog d
      LET event = d.nextEvent()
      --DISPLAY "here:", event
      CASE event
        WHEN "ON ACTION Done"
          LET done = TRUE
          EXIT WHILE
        WHEN "BEFORE ROW"
          LET row = d.getCurrentRow(screenRec)
          LET ifvar = getRecordIFfromArray(arrval, row)
          CALL ifvar.BeforeRow(d, row)
        WHEN "AFTER ROW"
          LET row = d.getCurrentRow(screenRec)
          DISPLAY "after row:",row
          IF row>0 THEN
            LET ifvar = getRecordIFfromArray(arrval,row)
            CALL ifvar.AfterRow(d, row)
          END IF
        WHEN "ON UPDATE"
          LET row = d.getCurrentRow(screenRec)
          CALL inputRow(d, fields, arrval.getArrayElement(row), "Update")
          -- TODO: perform an SQL UPDATE
        WHEN "ON APPEND"
          CALL arrval.appendArrayElement()
          LET row=arrval.getLength()
          LET int_flag=FALSE
          CALL inputRow(d, fields, arrval.getArrayElement(row), "Append")
          IF int_flag THEN
            CALL arrval.deleteArrayElement(row)
          END IF
          CALL checkUpdateDelete(d,arrval)
          -- TODO: perform an SQL INSERT
        WHEN "ON DELETE"
          LET ifvar = getRecordIFfromArray(arrval, row)
          LET int_flag=FALSE
          CALL ifvar.DeleteRow(d, row)
          IF NOT int_flag THEN
            IF fgldialog.fgl_winQuestion(title: "Attention",message: "Do you really want to delete this RECORD?",ans: "yes",items: "yes|no",icon: "quest",dang: 0) == "yes" THEN
              CALL arrval.deleteArrayElement(row)
            ELSE
              --inform the dyn dialog that no deletion should occur
              LET int_flag=TRUE
            END IF
          END IF
          CALL checkUpdateDelete(d,arrval)
        --WHEN "ON ACTION Filter"
        --  EXIT WHILE -- restarts the dialog d
        WHEN "AFTER DISPLAY"
          EXIT WHILE
        OTHERWISE
          LET row = d.getCurrentRow(screenRec)
          LET ifvar = getRecordIFfromArray(arrval,row)
          CALL ifvar.onDAevent(d,row,event)
          --row can be 0...don't how to define rowbound in dyn da
          CALL releaseRecordIFfromArray(arrval,row)
      END CASE
    END WHILE
    CALL d.close()
  END WHILE
  CLOSE WINDOW w
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

PRIVATE FUNCTION fillArray(d ui.Dialog, screenRec STRING, arrval reflect.Value)
  DEFINE i, idx, len INT
  DEFINE recv, fv reflect.Value
  DEFINE trec reflect.Type
  DEFINE name, value STRING
  LET len = arrval.getLength()
  FOR i = 1 TO len
    LET recv = arrval.getArrayElement(i)
    LET trec = recv.getType()
    CALL d.setCurrentRow(
      screenRec, i) -- must set the current row before setting values
    FOR idx = 1 TO trec.getFieldCount()
      LET fv = recv.getField(idx)
      LET name = trec.getFieldName(idx)
      LET value = fv.toString()
      --DISPLAY sfmt("set field value for:%1=%2",name,value)
      CALL d.setFieldValue(name, value)
    END FOR
  END FOR
  IF arrval.getLength()>0 THEN
    CALL d.setCurrentRow(screenRec, 1) -- TODO: should be done by the runtime
  END IF
END FUNCTION

PRIVATE FUNCTION getFieldByName(
  recv reflect.Value, name STRING)
  RETURNS reflect.Value
  DEFINE i, len INT
  DEFINE trec reflect.Type
  LET trec = recv.getType()
  LET len = trec.getFieldCount()
  FOR i = 1 TO len
    IF trec.getFieldName(i) == name THEN
      RETURN recv.getField(i)
    END IF
  END FOR
  RETURN NULL
END FUNCTION

FUNCTION inputRow(
  DA ui.Dialog, fields T_fields, origRecordVal reflect.Value, title STRING)
  DEFINE ev, err, curr, oldValue, name STRING
  DEFINE d ui.Dialog
  DEFINE i INT
  DEFINE handled BOOLEAN
  DEFINE ifvar T_sDAdyn
  DEFINE fv, value reflect.Value
  --DEFINE trec reflect.Type
  LET ifvar = reflectVal2IF(origRecordVal)
  LET d = ui.Dialog.createInputByName(fields)
  CALL d.addTrigger("ON ACTION accept")
  CALL d.addTrigger("ON ACTION cancel")
  -- copy values from DisplayArray to Input
  FOR i = 1 TO fields.getLength()
    CALL d.setFieldValue(fields[i].name, DA.getFieldValue(fields[i].name))
  END FOR
  --call back the init method in INPUT
  --there one can add custom actions, hide fields etc
  CALL ifvar.initINPUT(d)

  WHILE TRUE
    LET ev = d.nextEvent()
    LET curr = d.getCurrentItem()
    --DISPLAY "ev:",ev,",curr:",curr
    CASE
      WHEN ev.getIndexOf(C_BEFORE_FIELD, 1) = 1
        MYASSERT(curr.getLength() > 0)
        LET oldValue = d.getFieldValue(curr)
        CALL ifvar.BeforeField(d, curr)
        CONTINUE WHILE
      WHEN ev.getIndexOf(C_AFTER_FIELD, 1) = 1
        --after field: we copy the dialog value into the fields value
        MYASSERT(curr.getLength() > 0)
        LET fv = getFieldByName(origRecordVal, curr)
        MYASSERT(fv IS NOT NULL)
        LET value = reflect.Value.copyOf(d.getFieldValue(curr))
        MYASSERT(fv.getType().isAssignableFrom(value.getType()))
        CALL fv.set(value)
        CALL ifvar.AfterField(d, curr, oldValue) RETURNING err
        IF err IS NOT NULL THEN
          ERROR err
          CALL d.nextField(d.getCurrentItem())
        END IF
        CONTINUE WHILE
      OTHERWISE --pass any other event the onINPUTevent function
        CALL ifvar.onINPUTevent(
            d, ev, curr, IIF(curr.getLength() > 0, d.getFieldValue(curr), NULL))
          RETURNING handled, err
        IF handled THEN
          CONTINUE WHILE
        END IF
        IF err IS NOT NULL THEN
          ERROR err
          CALL d.nextField(d.getCurrentItem())
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
  -- copy values from Input to DisplayArray and to the backing RECORD
  FOR i = 1 TO fields.getLength()
    LET fv = origRecordVal.getField(i)
    LET name = fields[i].name
    LET value = reflect.Value.copyOf(d.getFieldValue(name))
    MYASSERT(fv.getType().isAssignableFrom(value.getType()))
    CALL fv.set(value)
    CALL da.setFieldValue(name, d.getFieldValue(fields[i].name))
  END FOR
  CALL d.close()
END FUNCTION
