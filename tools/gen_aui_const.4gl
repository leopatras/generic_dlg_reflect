--generator to create CONSTANTs for AUI attribute and tag names
--in a filed called "aui_const.4gl" to enable auto completion
&include "../myassert.inc"
IMPORT os

TYPE T_STRING_ARR DYNAMIC ARRAY OF STRING

CONSTANT AUI_CHILDREN = 1
CONSTANT AUI_ATTRIBUTES = 2
DEFINE m_auiRoot om.DomNode

MAIN
  CALL openAUIxa()
  VAR arr T_STRING_ARR
  VAR ch = base.Channel.create()
  CALL ch.openFile("aui_const.4gl", "w")
  VAR i INT
  VAR len = get_all_aui_attrs(arr)
  FOR i = 1 TO len
    CALL ch.writeLine(SFMT("PUBLIC CONSTANT A_%1='%2'", arr[i], arr[i]))
  END FOR
  CALL arr.clear()
  LET len = get_all_aui_tags(arr)
  FOR i = 1 TO len
    CALL ch.writeLine(SFMT("PUBLIC CONSTANT TAG_%1='%2'", arr[i], arr[i]))
  END FOR
END MAIN

PRIVATE FUNCTION getAUIxa()
  RETURN os.Path.join(base.Application.getFglDir(), "lib/aui.xa")
END FUNCTION

PRIVATE FUNCTION openAUIxa()
  DEFINE doc om.DomDocument
  LET doc = om.DomDocument.createFromXmlFile(getAUIxa())
  IF doc IS NULL THEN
    CALL myerrAndStackTrace(
        SFMT("can't read aui definition file %1", getAUIxa()))
  END IF
  LET m_auiRoot = doc.getDocumentElement()
END FUNCTION

PRIVATE FUNCTION get_attrs_per_tag(tagName, arr)
  DEFINE tagName STRING
  DEFINE arr DYNAMIC ARRAY OF STRING
  RETURN get_aui_info(tagName, arr, AUI_ATTRIBUTES)
END FUNCTION

PRIVATE FUNCTION get_child_tags(tagName, arr)
  DEFINE tagName STRING
  DEFINE arr DYNAMIC ARRAY OF STRING
  RETURN get_aui_info(tagName, arr, AUI_CHILDREN)
END FUNCTION

PRIVATE FUNCTION get_name_list(
    root om.DomNode,
    xpath STRING,
    arr T_STRING_ARR,
    omitVolatile BOOLEAN,
    clearArray BOOLEAN)
    RETURNS INT
  DEFINE node om.DomNode
  DEFINE nodelist om.NodeList
  DEFINE i, len, arrcount INTEGER
  IF clearArray THEN
    CALL arr.clear()
  END IF
  LET arrcount = arr.getLength()
  LET nodelist = root.selectByPath(xpath)
  LET len = nodelist.getLength()
  FOR i = 1 TO len
    LET node = nodelist.item(i)
    IF omitVolatile AND node.getAttribute("volatile") = "1" THEN
      CONTINUE FOR
    END IF
    LET arrcount = arrcount + 1
    LET arr[arrcount] = node.getAttribute("name")
  END FOR
  RETURN arrcount
END FUNCTION

PRIVATE FUNCTION get_xpath_attribute(root, xpath, attrName)
  DEFINE root om.DomNode
  DEFINE xpath STRING
  DEFINE attrName STRING
  DEFINE node om.DomNode
  DEFINE nodelist om.NodeList
  DEFINE len INTEGER
  LET nodelist = root.selectByPath(xpath)
  LET len = nodelist.getLength()
  MYASSERT(len == 1)
  LET node = nodelist.item(1)
  RETURN node.getAttribute(attrName)
END FUNCTION

PRIVATE FUNCTION isItemInAuiDefinition(root, xpath)
  DEFINE root om.DomNode
  DEFINE xpath STRING
  DEFINE nodelist om.NodeList
  DEFINE len INTEGER
  LET nodelist = root.selectByPath(xpath)
  LET len = nodelist.getLength()
  IF len == 1 THEN
    RETURN TRUE
  ELSE
    RETURN FALSE
  END IF
END FUNCTION

PRIVATE FUNCTION get_all_aui_attrs(arr) RETURNS INT
  DEFINE arr DYNAMIC ARRAY OF STRING
  RETURN get_name_list(
      m_auiRoot, "/AuiDef/AuiAttrDefList/AuiAttrDef", arr, FALSE, TRUE)
END FUNCTION

PRIVATE FUNCTION get_volatile_aui_attrs(arr)
  DEFINE arr DYNAMIC ARRAY OF STRING
  RETURN get_name_list(
      m_auiRoot,
      "/AuiDef/AuiAttrDefList/AuiAttrDef[@volatile=\"1\"]",
      arr,
      FALSE,
      TRUE)
END FUNCTION

PRIVATE FUNCTION get_volatile_aui_tags(arr)
  DEFINE arr DYNAMIC ARRAY OF STRING
  RETURN get_name_list(
      m_auiRoot, "/AuiDef/AuiNodeList/AuiNode[@volatile=\"1\"]", arr, 0, 1)
END FUNCTION

-- Generates a list of tags
PRIVATE FUNCTION get_all_aui_tags(arr T_STRING_ARR) RETURNS INT
  RETURN get_name_list(m_auiRoot, "/AuiDef/AuiNodeList/AuiNode", arr, 1, 0)
END FUNCTION

PRIVATE FUNCTION get_aui_info(tagName, arr, info)
  DEFINE tagName STRING
  DEFINE arr DYNAMIC ARRAY OF STRING
  DEFINE info SMALLINT
  DEFINE xpath STRING

  LET xpath = SFMT("/AuiDef/AuiNodeList/AuiNode[@name=\"%1\"]", tagName)
  CASE info
    WHEN AUI_CHILDREN
      LET xpath =
          SFMT("/AuiDef/AuiNodeList/AuiNode[@name=\"%1\"]/AuiElement", tagName)
    WHEN AUI_ATTRIBUTES
      LET xpath =
          SFMT("/AuiDef/AuiNodeList/AuiNode[@name=\"%1\"]/AuiAttr", tagName)
    OTHERWISE
      LET xpath = ("/XXX")
  END CASE
  RETURN get_name_list(m_auiRoot, xpath, arr, 1, TRUE)
END FUNCTION

PRIVATE FUNCTION get_attr_def(attrName, what)
  DEFINE attrName STRING
  DEFINE what STRING
  DEFINE xpath STRING
  DEFINE node om.DomNode
  DEFINE nodelist om.NodeList
  IF NOT (what = "type" OR what = "usage") THEN
    CALL myerr(
        SFMT("get_attr_def:wrong what '%1',must be 'type' or 'usage'", what))
  END IF

  LET xpath = SFMT("/AuiDef/AuiAttrDefList/AuiAttrDef[@name=\"%1\"]", attrName)
  LET nodelist = m_auiRoot.selectByPath(xpath)
  MYASSERT(nodelist.getLength() == 1)
  LET node = nodelist.item(1)
  RETURN node.getAttribute("type")
END FUNCTION

DEFINE mShowStack BOOLEAN

PRIVATE FUNCTION myerrAndStackTrace(errstr STRING)
  LET mShowStack = TRUE
  CALL myerr(errstr)
END FUNCTION

PRIVATE FUNCTION myerr(errstr STRING)
  DEFINE ch base.Channel
  LET ch = base.Channel.create()
  CALL ch.openFile("<stderr>", "w")
  IF mShowStack THEN
    CALL ch.writeLine(
        SFMT("ERROR:%1,stack:\n%2", errstr, base.Application.getStackTrace()))
  ELSE
    CALL ch.writeLine(SFMT("ERROR:%1", errstr))
  END IF
  CALL ch.close()
  EXIT PROGRAM 1
END FUNCTION
