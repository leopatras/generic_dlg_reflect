MAIN
  DEFINE ch, out base.Channel
  DEFINE tabname, colname STRING
  DEFINE open BOOLEAN
  DEFINE x, y, z INT
  IF num_args() <> 3 THEN
    DISPLAY "usage: gen_col_names <.sch-file> <table-name> <out.4gl>"
    EXIT PROGRAM 1
  END IF
  LET ch = base.Channel.create()
  LET out = base.Channel.create()
  CALL ch.setDelimiter("^")
  CALL ch.openFile(arg_val(1), "r")
  WHILE ch.read([tabname, colname, x, y, z])
    IF tabname == arg_val(2) THEN
      IF NOT open THEN
        LET open = TRUE
        CALL out.openFile(arg_val(3), "w")
      END IF
      CALL out.writeLine(SFMT("PUBLIC CONSTANT C_%1='%2'", colname, colname))
    END IF

  END WHILE
  IF NOT open THEN
    DISPLAY SFMT("Could not find table:%1 in %2", arg_val(2), arg_val(1))
    EXIT PROGRAM 1
  END IF
END MAIN
