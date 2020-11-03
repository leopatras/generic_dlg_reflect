&include "myassert.inc"
IMPORT util
IMPORT FGL utils
IMPORT FGL fgldbutl
TYPE T_STRING_ARR DYNAMIC ARRAY OF STRING
DEFINE m_init BOOLEAN
DEFINE m_cols DICTIONARY OF T_INT_DICT
--dirty hack to acces the internals of fgldbslib
&include "fgldbslib.4gl"

FUNCTION prepareSerials()
  DEFINE i,len INT
  DEFINE t,c STRING
  IF NOT m_init THEN
    LET m_init=TRUE
    CALL fgldbsch_init()
    LET params.dbtype=fgldbutl.db_get_database_type()
    CALL fgldbsch_extract() RETURNING status
    LET len=serialcols.getLength()
    MYASSERT(len>0)
    FOR i=1 TO len
      LET t = serialcols[i].tabname 
      LET c = serialcols[i].colname
      LET m_cols[t][c]=i
    END FOR
    DISPLAY util.JSON.stringify(m_cols)
  END IF
END FUNCTION

FUNCTION getSerialColumnsForTable(tabName STRING) RETURNS T_INT_DICT
  CALL prepareSerials()
  RETURN m_cols[tabName]
END FUNCTION

FUNCTION isSerialColumnForTable(colName STRING,tabName STRING) RETURNS BOOLEAN
  DEFINE dict T_INT_DICT
  CALL prepareSerials()
  RETURN m_cols[tabName].contains(colName)
END FUNCTION

FUNCTION MAIN()
  DEFINE dict T_INT_DICT
  CALL utils.dbconnect()
  LET dict=getSerialColumnsForTable("customer")
  DISPLAY util.JSON.stringify(dict)
END FUNCTION
