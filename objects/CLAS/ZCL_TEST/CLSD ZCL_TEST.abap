class-pool .
*"* class pool for class ZCL_TEST

*"* local type definitions
include ZCL_TEST======================ccdef.

*"* class ZCL_TEST definition
*"* public declarations
  include ZCL_TEST======================cu.
*"* protected declarations
  include ZCL_TEST======================co.
*"* private declarations
  include ZCL_TEST======================ci.
endclass. "ZCL_TEST definition

*"* macro definitions
include ZCL_TEST======================ccmac.
*"* local class implementation
include ZCL_TEST======================ccimp.

*"* test class
include ZCL_TEST======================ccau.

class ZCL_TEST implementation.
*"* method's implementations
  include methods.
endclass. "ZCL_TEST implementation
