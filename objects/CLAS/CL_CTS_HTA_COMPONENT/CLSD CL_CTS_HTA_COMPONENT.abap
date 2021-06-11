class-pool .
*"* class pool for class CL_CTS_HTA_COMPONENT

*"* local type definitions
include CL_CTS_HTA_COMPONENT==========ccdef.

*"* class CL_CTS_HTA_COMPONENT definition
*"* public declarations
  include CL_CTS_HTA_COMPONENT==========cu.
*"* protected declarations
  include CL_CTS_HTA_COMPONENT==========co.
*"* private declarations
  include CL_CTS_HTA_COMPONENT==========ci.
endclass. "CL_CTS_HTA_COMPONENT definition

*"* macro definitions
include CL_CTS_HTA_COMPONENT==========ccmac.
*"* local class implementation
include CL_CTS_HTA_COMPONENT==========ccimp.

*"* test class
include CL_CTS_HTA_COMPONENT==========ccau.

class CL_CTS_HTA_COMPONENT implementation.
*"* method's implementations
  include methods.
endclass. "CL_CTS_HTA_COMPONENT implementation
