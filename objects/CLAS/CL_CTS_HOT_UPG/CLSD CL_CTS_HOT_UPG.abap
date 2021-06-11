class-pool .
*"* class pool for class CL_CTS_HOT_UPG

*"* local type definitions
include CL_CTS_HOT_UPG================ccdef.

*"* class CL_CTS_HOT_UPG definition
*"* public declarations
  include CL_CTS_HOT_UPG================cu.
*"* protected declarations
  include CL_CTS_HOT_UPG================co.
*"* private declarations
  include CL_CTS_HOT_UPG================ci.
endclass. "CL_CTS_HOT_UPG definition

*"* macro definitions
include CL_CTS_HOT_UPG================ccmac.
*"* local class implementation
include CL_CTS_HOT_UPG================ccimp.

*"* test class
include CL_CTS_HOT_UPG================ccau.

class CL_CTS_HOT_UPG implementation.
*"* method's implementations
  include methods.
endclass. "CL_CTS_HOT_UPG implementation
