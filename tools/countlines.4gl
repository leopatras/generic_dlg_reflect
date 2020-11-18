DEFINE cnt INT
MAIN
  VAR ch = base.Channel.create()
  CALL ch.openFile(arg_val(1), "r")
  VAR line STRING
  WHILE (line := ch.readLine()) IS NOT NULL
    CALL countLine(line)
  END WHILE
  DISPLAY SFMT("%1", cnt)
END MAIN

FUNCTION countLine(line STRING)
  VAR t = line.trimWhiteSpace()
  --don't count empty lines
  IF t.getLength() = 0 THEN
    RETURN
  END IF
  --don't count comments and UNUSED
  IF t.getIndexOf("#", 1) == 1
      OR t.getIndexOf("--", 1) == 1
      OR t.getIndexOf("UNUSED(", 1) == 1 THEN
    RETURN
  END IF
  {
  --also ignore the import lines
  IF getIndexOfI(t, "import ", 1) == 1 THEN
    RETURN
  END IF
  }
  LET cnt = cnt + 1
END FUNCTION

--case insensitive version of string.getIndexOf
FUNCTION getIndexOfI(src, pattern, idx)
  DEFINE src, pattern STRING
  DEFINE idx INTEGER
  LET src = src.toLowerCase()
  LET pattern = pattern.toLowerCase()
  RETURN src.getIndexOf(pattern, idx)
END FUNCTION
