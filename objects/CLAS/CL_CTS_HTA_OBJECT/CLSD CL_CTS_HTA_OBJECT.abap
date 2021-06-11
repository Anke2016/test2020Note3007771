class-pool .
*"* class pool for class CL_CTS_HTA_OBJECT

*"* local type definitions
include CL_CTS_HTA_OBJECT=============ccdef.

*"* class CL_CTS_HTA_OBJECT definition
*"* public declarations
  include CL_CTS_HTA_OBJECT=============cu.
*"* protected declarations
  include CL_CTS_HTA_OBJECT=============co.
*"* private declarations
  include CL_CTS_HTA_OBJECT=============ci.
endclass. "CL_CTS_HTA_OBJECT definition

*"* macro definitions
include CL_CTS_HTA_OBJECT=============ccmac.
*"* local class implementation
include CL_CTS_HTA_OBJECT=============ccimp.

*"* test class
include CL_CTS_HTA_OBJECT=============ccau.

class CL_CTS_HTA_OBJECT implementation.
*"* method's implementations
  include methods.
endclass. "CL_CTS_HTA_OBJECT implementation
