--generator to create CONSTANTs for all inputtable field names in a compiled form file
IMPORT FGL utils
MAIN
  IF num_args() <> 2 THEN
    DISPLAY "usage: gen_form_names <.42f> <out.4gl>"
    EXIT PROGRAM 1
  END IF
  VAR names = utils.getAllRealInputtableFieldNames(arg_val(1), FALSE)
  VAR out = base.Channel.create()
  CALL out.openFile(arg_val(2), "w")
  VAR i INT
  FOR i = 1 TO names.getLength()
    VAR name = names[i]
    CALL out.writeLine(SFMT("PUBLIC CONSTANT C_%1='%2'", name, name))
  END FOR
END MAIN
