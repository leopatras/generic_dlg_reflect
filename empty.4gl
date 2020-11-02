&include "myassert.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores

TYPE TM_empty RECORD 
  arr DYNAMIC ARRAY OF RECORD LIKE empty.*
END RECORD

FUNCTION (self TM_empty) checkIF()
  DEFINE iBF I_BeforeRow
  DEFINE iAF I_AfterRow
  DEFINE iIU I_InsertOrUpdateOfRow
  LET iBF=self
  LET iAF=self
  LET iIU=self
END FUNCTION

FUNCTION (self TM_empty) BeforeRow(d ui.Dialog, row INT)
  UNUSED(self)
  UNUSED(d)
  DISPLAY sfmt("TM_empty BeforeRow:%1",row)
END FUNCTION

FUNCTION (self TM_empty) AfterRow(d ui.Dialog, row INT)
  UNUSED(self)
  UNUSED(d)
  DISPLAY sfmt("TM_empty AfterRow:%1",row)
END FUNCTION

FUNCTION (self TM_empty) InsertOrUpdateOfRow(update BOOLEAN,row INT)
  IF update THEN
    CALL myerrAndStackTrace("Update not supported")
  END IF
  INSERT INTO empty VALUES self.arr[row].*
  LET self.arr[row].empty_num = sqlca.sqlerrd[2]
END FUNCTION

FUNCTION MAIN()
  DEFINE me TM_empty
  DEFINE opts sDAdyn.T_SingleTableDAOptions =
    (browseForm: "empty", browseRecord: "scr")
  CALL utils.dbconnect()
  LET opts.delegateDA = reflect.Value.valueOf(me)
  LET opts.arrayValue = reflect.Value.valueOf(me.arr)
  LET opts.sqlAll = "SELECT * FROM empty"
  LET opts.hasAppend = TRUE
  CALL sDAdyn.browseArray(opts)
END FUNCTION
