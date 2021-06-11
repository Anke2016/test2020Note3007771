*"* Use this include only for the publication or definition of local
*"* class and interface definitions which you also use in the private
*"* section of the global class.
*"*
*"* Example 1
*"* When do you have to publish a local class or interface here?
*"* ============================================================
*"*
*"*   PRIVATE SECTION.
*"*     ...
*"*     DATA my_attribute_class     TYPE REF TO lcl_my_local_class.
*"*     DATA my_attribute_interface TYPE REF TO lcl_my_local_interface.
*"*
*"* If you declare attributes with references to your local
*"* classes and interfaces in the private section of your global class,
*"* you have to publish the local classes and interfaces in this
*"* include by adding the following statements:
*"*
*"*   CLASS lcl_my_local_class DEFINITION DEFERRED.
*"*   INTERFACE lif_my_local_interface DEFERRED.
*"*
*"* The local classes and implementations can stay in the include
*"* for local types.
*"*
*"* Example 2
*"* When do you have to define a local class or interface here?
*"* ===========================================================
*"*
*"*   PRIVATE SECTION.
*"*     ...
*"*     DATA my_attribute TYPE lcl_my_local_class=>ty_table.
*"*
*"* If you use components (e.g., types, constants) of local classes
*"* or interfaces you have to move the definition of the class or
*"* interface from the includes for local types to this include.
*"* A publication (as described in example 1) won't be sufficient.
*"*