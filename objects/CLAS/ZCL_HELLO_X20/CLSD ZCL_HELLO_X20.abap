class-pool .
*"* class pool for class ZCL_HELLO_X20

*"* local type definitions
include ZCL_HELLO_X20=================ccdef.

*"* class ZCL_HELLO_X20 definition
*"* public declarations
  include ZCL_HELLO_X20=================cu.
*"* protected declarations
  include ZCL_HELLO_X20=================co.
*"* private declarations
  include ZCL_HELLO_X20=================ci.
endclass. "ZCL_HELLO_X20 definition

*"* macro definitions
include ZCL_HELLO_X20=================ccmac.
*"* local class implementation
include ZCL_HELLO_X20=================ccimp.

*"* test class
include ZCL_HELLO_X20=================ccau.

class ZCL_HELLO_X20 implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_HELLO_X20 implementation
