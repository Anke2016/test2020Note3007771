class-pool MESSAGE-ID SCTS_HOT.
*"* class pool for class CX_CTS_HTA

*"* local type definitions
include CX_CTS_HTA====================ccdef.

*"* class CX_CTS_HTA definition
*"* public declarations
  include CX_CTS_HTA====================cu.
*"* protected declarations
  include CX_CTS_HTA====================co.
*"* private declarations
  include CX_CTS_HTA====================ci.
endclass. "CX_CTS_HTA definition

*"* macro definitions
include CX_CTS_HTA====================ccmac.
*"* local class implementation
include CX_CTS_HTA====================ccimp.

*"* test class
include CX_CTS_HTA====================ccau.

class CX_CTS_HTA implementation.
*"* method's implementations
  include methods.
endclass. "CX_CTS_HTA implementation
