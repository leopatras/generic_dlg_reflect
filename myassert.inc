&define MYASSERT(x) IF NOT NVL(x,0) THEN \
     CALL myerr(sfmt("ASSERTION failed:%1 in file:%2,line:%3,stack:\n%4",#x,__FILE__,__LINE__,base.Application.getStackTrace())) \
   END IF

&define UNUSED(x) IF FALSE THEN INITIALIZE x TO NULL END IF
