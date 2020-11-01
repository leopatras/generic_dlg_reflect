# Property of Four Js*
# (c) Copyright Four Js 1995, 2020. All Rights Reserved.
# * Trademark of Four Js Development Tools Europe Ltd
#   in the United States and elsewhere
# 
# Four Js and its suppliers do not warrant or guarantee that these
# samples are accurate and suitable for your purposes. Their inclusion is
# purely for information purposes only.

MAIN
    TRY
        CONNECT TO "stores.dbs+driver='sqlite'"
        RETURN
    END TRY
    CALL createDatabase()
END MAIN

FUNCTION createDatabase()
    DEFINE appdir STRING

    TRY
        CONNECT TO "stores.dbs+driver='sqlite'"
        RETURN
    END TRY
    CREATE DATABASE "stores.dbs"
    CONNECT TO "stores.dbs+driver='sqlite'"
    
    CREATE TABLE customer
    (
    customer_num SERIAL,
    fname CHAR(15),
    lname CHAR(15),
    company CHAR(20),
    address1 CHAR(20),
    address2 CHAR(20),
    city CHAR(15),
    state CHAR(2),
    zipcode CHAR(5),
    phone CHAR(18)
    )
    CREATE UNIQUE INDEX c_num_ix ON customer (customer_num)
    CREATE INDEX zip_ix ON customer (zipcode)
    CREATE TABLE cust_ex
    (
     customer_num INTEGER,
     comments CHAR(255)
    )
    CREATE INDEX cext_num_ix ON cust_ex (customer_num)

    CREATE TABLE orders
    (
    order_num SERIAL,
    order_date DATE,
    customer_num INTEGER,
    ship_instruct CHAR(40),
    backlog CHAR(1),
    po_num CHAR(10),
    ship_date DATE,
    ship_weight DECIMAL(8, 2),
    ship_charge MONEY(6, 2),
    paid_date DATE
    )
    CREATE UNIQUE INDEX o_num_ix ON orders (order_num)
    CREATE INDEX o_c_num_ix ON orders (customer_num)

    CREATE TABLE items
    (
    --item_num SERIAL,
    item_num INTEGER,
    order_num INTEGER,
    stock_num SMALLINT,
    manu_code CHAR(3),
    quantity SMALLINT,
    total_price MONEY(8, 2)
    )
    --CREATE UNIQUE INDEX o_item_ix ON items (item_num)
    CREATE UNIQUE INDEX o_item_ix ON items (order_num, item_num)
    CREATE INDEX i_o_num_ix ON items (order_num)
    CREATE INDEX st_man_ix ON items (stock_num, manu_code)

    CREATE TABLE stock
    (
    stock_num SMALLINT,
    manu_code CHAR(3),
    description CHAR(15),
    unit_price MONEY(6, 2),
    unit CHAR(4),
    unit_descr CHAR(15)
    )
    CREATE UNIQUE INDEX stk_man_ix ON stock (stock_num, manu_code)

    CREATE TABLE manufact
    (
    manu_code CHAR(3),
    manu_name CHAR(15),
    unknown1  INT
    )
    CREATE UNIQUE INDEX man_cd_ix ON manufact (manu_code)

    CREATE TABLE state
    (
    code CHAR(2),
    sname CHAR(15)
    )
    CREATE UNIQUE INDEX st_x1 ON state (code)

    LET appdir = NVL(fgl_getenv("FGLAPPDIR"), ".")
    BEGIN WORK
    LOAD FROM appdir || "/stores.exp/customer.unl" INSERT INTO CUSTOMER
    LOAD FROM appdir || "/stores.exp/cust_ex.unl" INSERT INTO CUST_EX
    LOAD FROM appdir || "/stores.exp/items.unl" INSERT INTO ITEMS
    LOAD FROM appdir || "/stores.exp/manufact.unl" INSERT INTO MANUFACT
    LOAD FROM appdir || "/stores.exp/orders.unl" INSERT INTO ORDERS
    LOAD FROM appdir || "/stores.exp/state.unl" INSERT INTO STATE
    LOAD FROM appdir || "/stores.exp/stock.unl" INSERT INTO STOCK
    COMMIT WORK
    DISCONNECT ALL
END FUNCTION

