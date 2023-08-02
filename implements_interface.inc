--handy macro to enable compile time checks for interface conformance
--by creating a dummy function which does assign the record to an interface
&define IMPLEMENTS_INTERFACE(recType,iface) \
  FUNCTION (self recType) __checkInterface_##iface() \
    DEFINE test iface \
    LET test = self \
  END FUNCTION
