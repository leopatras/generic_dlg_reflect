# generic_dlg_reflect
how one could use dynamic dialogs, reflect and RECORD methods to build generic dialogs in Genero

`sDADyn.4gl` implements a reusable combination of `DISPLAY ARRAY/INPUT` and `CONSTRUCT` with dynamic dialogs and a callback mechanism via Interfaces into application code.

`customers.4gl,orders.4gl` and `items.4gl` use one and the same `sDADyn` dialog with different array types and different callbacks.

`ccustomers.4gl,corders.4gl` and `citems.4gl` show (almost) the same feature set implemented using classic `DISPLAY ARRAY/INPUT/CONSTRUCT` combinations.

The abstraction level in the code using the new dialog type is higher, the re usage level is higer and less code lines are needed in the application code.

| File          | Number Of Lines  |
| ------------- | ----------------:|
| ccustomers.4gl|              228 |
| customers.4gl |              107 |
|               |                  |
| corders.4gl   |              214 |
| orders.4gl    |               80 |
|               |                  |
| citems.4gl    |              200 |
| items.4gl     |               50 |
|               |                  |
| sDADyn.4gl    |          ca 1000 |
