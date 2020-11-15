DEFINE cnt INT
MAIN
  VAR ch=base.Channel.create()
  CALL ch.openFile(arg_val(1),"r")
  VAR line STRING
  WHILE (line:=ch.readLine()) IS NOT NULL
    CALL countLine(line) 
  END WHILE
  DISPLAY sfmt("%1",cnt)
END MAIN

FUNCTION countLine(line STRING)
  VAR t=line.trimWhiteSpace()
  IF t.getLength()=0 THEN
    RETURN
  END IF
  IF t.getIndexOf("#",1)==1 OR t.getIndexOf("--",1)==1 THEN
     RETURN
  END IF
  IF getIndexOfI(t,"import ",1)==1 THEN
    RETURN
  END IF
  LET cnt=cnt+1
END FUNCTION

--case insensitive version of string.getIndexOf
FUNCTION getIndexOfI(src,pattern,idx)
  DEFINE src,pattern STRING
  DEFINE idx INTEGER
  LET src=src.toLowerCase()
  LET pattern=pattern.toLowerCase()
  RETURN src.getIndexOf(pattern,idx)
END FUNCTION

