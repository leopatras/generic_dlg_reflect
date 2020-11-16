  WHILE NOT done
    ...
    IF hasFilter THEN
      LET whereCond=filter()
      CALL fillArray(whereCond)
    ELSE
      CALL fillArray(" 1=1")
    END IF
    ...--set variable window title 
    DISPLAY ARRAY arr TO scr.*
      BEFORE DISPLAY
        ...--set up visibility of actions depending on some flags
      ON ACTION accept
        LET done=TRUE
        EXIT DISPLAY
      ON APPEND...or ON UPDATE
        CALL input(arr[arr_curr()].*,update) RETURNING arr[arr_curr()].*
      ON DELETE
        ...--call table specific deletion code
      ON ACTION filter
        LET hasFilter=TRUE
        EXIT WHILE
      ON ACTION clear_filter
        LET hasFilter=TRUE
        EXIT WHILE
      ON ACTION ...--...table specific DISPLAY ARRAY actions
    END DISPLAY
  END WHILE

FUNCTION filter()
  ... --eventually open window with detail form
  CONSTRUCT BY NAME whereCond ON tabname.*
  ...
  RETURN whereCond
  ... --eventually close window with detail form
END FUNCTION

FUNCTION input(rec LIKE tabname.*) 
  ... --eventually open window with detail form
  INPUT BY NAME rec.*  ...
    AFTER field... --table specific field constraints
    ON ACTION...   --table specific INPUT actions
  END INPUT
  ... --eventually close window with detail form
END FUNCTION
