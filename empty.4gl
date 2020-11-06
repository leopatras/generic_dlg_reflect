&include "myassert.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
--empty test table to see what row indexes we get in case
--the array is empty 
TYPE TM_empty RECORD 
  arr DYNAMIC ARRAY OF RECORD LIKE empty.*
END RECORD

FUNCTION (self TM_empty) checkIF()
  DEFINE iBF I_BeforeRow
  DEFINE iAF I_AfterRow
  LET iBF=self
  LET iAF=self
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

FUNCTION main()
  DEFINE me TM_empty
  CALL utils.dbconnect()
  VAR opts sDAdyn.T_SingleTableDAOptions =
    (browseForm: "empty", browseRecord: "scr",
     sqlAll: "SELECT * FROM empty",hasAppend:TRUE,hasDelete:TRUE,hasUpdate:TRUE)
  LET opts.delegateDA = reflect.Value.valueOf(me)
  LET opts.arrayValue = reflect.Value.valueOf(me.arr)
  CALL sDAdyn.browseArray(opts)
END FUNCTION
