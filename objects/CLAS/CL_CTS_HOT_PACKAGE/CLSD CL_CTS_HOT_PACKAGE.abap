class-pool .
*"* class pool for class CL_CTS_HOT_PACKAGE

*"* local type definitions
include CL_CTS_HOT_PACKAGE============ccdef.

*"* class CL_CTS_HOT_PACKAGE definition
*"* public declarations
  include CL_CTS_HOT_PACKAGE============cu.
*"* protected declarations
  include CL_CTS_HOT_PACKAGE============co.
*"* private declarations
  include CL_CTS_HOT_PACKAGE============ci.
endclass. "CL_CTS_HOT_PACKAGE definition

*"* macro definitions
include CL_CTS_HOT_PACKAGE============ccmac.
*"* local class implementation
include CL_CTS_HOT_PACKAGE============ccimp.

*"* test class
include CL_CTS_HOT_PACKAGE============ccau.

class CL_CTS_HOT_PACKAGE implementation.
*"* method's implementations
  include methods.
endclass. "CL_CTS_HOT_PACKAGE implementation
