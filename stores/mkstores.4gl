IMPORT FGL fgldbutl
IMPORT os

MAIN
  IF NOT os.Path.exists("LICENSE") THEN
    CALL myerr("must run mkstores being in the topdir")
  END IF
  CALL createDatabase()
END MAIN

FUNCTION myerr(errstr STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile("<stderr>", "w")
  CALL ch.writeLine(SFMT("ERROR:%1", errstr))
  CALL ch.close()
  EXIT PROGRAM 1
END FUNCTION

FUNCTION createDatabase()
  DEFINE appdir, src, db STRING
  DEFINE haveProfile BOOLEAN
  DEFINE driver STRING
  LET haveProfile = FALSE
  LET driver = "sqlite"

  IF (src
    := base.Application.getResourceEntry(
      "dbi.database.stores.source")) IS NOT NULL THEN
    LET driver = base.Application.getResourceEntry("dbi.database.stores.driver")
    DISPLAY "db src:", src, ",db driver:", driver
    LET haveProfile = TRUE
    IF driver == "sqlite" THEN
      TRY
        DATABASE stores
        RETURN --exists already
      END TRY
    END IF
  ELSE --connect to sqlite
    TRY
      CONNECT TO SFMT("stores.dbs+driver='%1'", driver)
      RETURN
    END TRY
  END IF
  IF haveProfile == TRUE THEN
    IF driver == "sqlite" THEN
      --we need to make the file available
      RUN "touch stores.dbs"
    END IF
    DATABASE stores
  ELSE
    CREATE DATABASE "stores.dbs"
    LET db = SFMT("stores.dbs+driver='%1'", driver)
    CONNECT TO db
  END IF

  CREATE TABLE customer(
    customer_num SERIAL,
    fname CHAR(15),
    lname CHAR(15),
    company CHAR(20),
    address1 CHAR(20),
    address2 CHAR(20),
    city CHAR(15),
    state CHAR(2),
    zipcode CHAR(5),
    phone CHAR(18))
  CREATE UNIQUE INDEX c_num_ix ON customer(customer_num)
  CREATE INDEX zip_ix ON customer(zipcode)
  CREATE TABLE cust_ex(customer_num INTEGER, comments CHAR(255))
  CREATE INDEX cext_num_ix ON cust_ex(customer_num)

  CREATE TABLE orders(
    order_num SERIAL,
    order_date DATE,
    customer_num INTEGER,
    ship_instruct CHAR(40),
    backlog CHAR(1),
    po_num CHAR(10),
    ship_date DATE,
    ship_weight DECIMAL(8, 2),
    ship_charge MONEY(6, 2),
    paid_date DATE)
  CREATE UNIQUE INDEX o_num_ix ON orders(order_num)
  CREATE INDEX o_c_num_ix ON orders(customer_num)

  CREATE TABLE items(
    --item_num SERIAL,
    item_num INTEGER,
    order_num INTEGER,
    stock_num SMALLINT,
    manu_code CHAR(3),
    quantity SMALLINT,
    total_price MONEY(8, 2))
  --CREATE UNIQUE INDEX o_item_ix ON items (item_num)
  CREATE UNIQUE INDEX o_item_ix ON items(order_num, item_num)
  CREATE INDEX i_o_num_ix ON items(order_num)
  CREATE INDEX st_man_ix ON items(stock_num, manu_code)

  CREATE TABLE stock(
    stock_num SMALLINT,
    manu_code CHAR(3),
    description CHAR(15),
    unit_price MONEY(6, 2),
    unit CHAR(4),
    unit_descr CHAR(15))
  CREATE UNIQUE INDEX stk_man_ix ON stock(stock_num, manu_code)

  CREATE TABLE manufact(manu_code CHAR(3), manu_name CHAR(15), unknown1 INT)
  CREATE UNIQUE INDEX man_cd_ix ON manufact(manu_code)

  CREATE TABLE state(code CHAR(2), sname CHAR(15))
  CREATE UNIQUE INDEX st_x1 ON state(code)

  CREATE TABLE empty(empty_num SERIAL, x CHAR(1), xx CHAR(2))

  LET appdir = NVL(fgl_getenv("FGLAPPDIR"), ".")
  BEGIN WORK
  LOAD FROM appdir || "/stores/customer.unl" INSERT INTO CUSTOMER
  LOAD FROM appdir || "/stores/cust_ex.unl" INSERT INTO CUST_EX
  LOAD FROM appdir || "/stores/items.unl" INSERT INTO ITEMS
  LOAD FROM appdir || "/stores/manufact.unl" INSERT INTO MANUFACT
  LOAD FROM appdir || "/stores/orders.unl" INSERT INTO ORDERS
  LOAD FROM appdir || "/stores/state.unl" INSERT INTO STATE
  LOAD FROM appdir || "/stores/stock.unl" INSERT INTO STOCK
  COMMIT WORK
  DISCONNECT ALL
  IF haveProfile THEN
    CALL checkRUN("fgldbsch -db stores")
  ELSE
    CALL checkRUN(sfmt("fgldbsch -dv %1 -of stores -db stores.dbs", driver))
  END IF
  DISPLAY "created stores db"
  IF fgldbutl.db_get_database_type() == "PGS" THEN
    CALL checkRUN("stores/convert_serials")
  END IF
END FUNCTION

FUNCTION checkRUN(cmd STRING)
  DEFINE code INT
  RUN cmd RETURNING code
  IF code THEN
    CALL myerr(sfmt("command '%1' failed.",cmd))
  END IF
END FUNCTION
