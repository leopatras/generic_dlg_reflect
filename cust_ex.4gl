&include "myassert.inc"
IMPORT reflect
IMPORT FGL utils
IMPORT FGL sDAdyn
SCHEMA stores
--show case RECORDs with duplicate column names
TYPE TM_custex RECORD
  cust RECORD LIKE customer.*, --"cust" is a table alias name
  cust_ex RECORD LIKE cust_ex.* --"cust_ex" is a real table name
END RECORD

--method called by the browseArray function
--after INPUT mode whenever the current INPUT data must be written to DB
--depending on the update flag one needs to invoke the suitable SQL statements here
FUNCTION (self TM_custex) InsertOrUpdate(update BOOLEAN)
  --propagate the error handling to the generic dialog
  --so that it can be handled in a generic way
  WHENEVER ERROR RAISE
  IF update THEN
    UPDATE customer
        SET customer.* = self.cust.*
        WHERE @customer_num = $self.cust.customer_num
    IF self.cust.customer_num == 101
        --we provoke an SQL error on the first customer
        THEN
      UPDATE cust_ex
          SET a = 1, b = 2, c = 4
          WHERE @customer_num = self.cust.customer_num
    ELSE
      UPDATE cust_ex
          SET cust_ex.* = self.cust_ex.*
          WHERE @customer_num = self.cust.customer_num
    END IF
  ELSE
    INSERT INTO customer VALUES self.cust.*
    LET self.cust.customer_num = sqlca.sqlerrd[2]
    LET self.cust_ex.customer_num = self.cust.customer_num
    INSERT INTO cust_ex VALUES self.cust_ex.*
  END IF
  WHENEVER ERROR STOP
END FUNCTION

--method called by the browseArray function
--whenever a row is actually deleted
FUNCTION (self TM_custex) DeleteRow(d ui.Dialog, row INT) RETURNS()
  UNUSED(d)
  DISPLAY SFMT("cust_ex DeleteRow:%1", row)
  WHENEVER ERROR RAISE
  DELETE FROM customer WHERE @customer_num = self.cust.customer_num
  IF self.cust.customer_num == 101 THEN
    DELETE FROM cust_ex WHERE haha = -1 --some nonsense for first cust
  ELSE
    DELETE FROM cust_ex WHERE @customer_num = self.cust.customer_num
  END IF
  WHENEVER ERROR STOP
END FUNCTION

FUNCTION (self TM_custex) checkInterfaces()
  --dummy func to enable a compiler check until IMPLEMENTS is there
  DEFINE iIU I_InsertOrUpdate
  DEFINE iDR I_DeleteRow
  MYASSERT(FALSE)
  LET iIU = self
  LET iDR = self
END FUNCTION

FUNCTION main()
  DEFINE arr DYNAMIC ARRAY OF TM_custex
  DEFINE opts T_SingleTableDAOptions =
      (sqlAll:
               "SELECT * FROM CUSTOMER AS cust, cust_ex WHERE cust.customer_num=cust_ex.customer_num",
          browseForm: "cust_ex",
          browseRecord: "scr",
          sqlFetchByPosition:
               TRUE, --must use fetch by position because of the multiple names
          qualifiedNames:
               TRUE, --must use qualified names to avoid a run time error (multiple column names)
          --autoPhantom: TRUE, --tolerates missing FormFields/TableColumns in the forms
          addClickableImages: TRUE,
          hasUpdate: TRUE,
          hasAppend: TRUE,
          hasDelete: TRUE)
  CALL utils.dbconnect()
  --we must pass the reflect value of the array,
  --the browseArray function fills the array with SQL data
  --and calls implemented methods of the TM_customer type
  --for the current RECORD in the array
  LET opts.arrayValue = reflect.Value.valueOf(arr)
  CALL sDAdyn.browseArray(opts)
END FUNCTION
