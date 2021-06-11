class-pool MESSAGE-ID SCTS_HOT.
*"* class pool for class CX_HANA_OBJECT_TRANSPORT

*"* local type definitions
include CX_HANA_OBJECT_TRANSPORT======ccdef.

*"* class CX_HANA_OBJECT_TRANSPORT definition
*"* public declarations
  include CX_HANA_OBJECT_TRANSPORT======cu.
*"* protected declarations
  include CX_HANA_OBJECT_TRANSPORT======co.
*"* private declarations
  include CX_HANA_OBJECT_TRANSPORT======ci.
endclass. "CX_HANA_OBJECT_TRANSPORT definition

*"* macro definitions
include CX_HANA_OBJECT_TRANSPORT======ccmac.
*"* local class implementation
include CX_HANA_OBJECT_TRANSPORT======ccimp.

class CX_HANA_OBJECT_TRANSPORT implementation.
*"* method's implementations
  include methods.
endclass. "CX_HANA_OBJECT_TRANSPORT implementation
