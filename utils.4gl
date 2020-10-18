PUBLIC CONSTANT C_AFTER_FIELD="AFTER FIELD"
PUBLIC CONSTANT C_BEFORE_FIELD="BEFORE FIELD"
PUBLIC CONSTANT C_ON_ACTION="ON ACTION"
FUNCTION myerr(errstr STRING)
  DEFINE ch base.Channel
  LET ch=base.Channel.create()
  CALL ch.openFile("<stderr>","w")
  CALL ch.writeLine(sfmt("ERROR:%1",errstr))
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
