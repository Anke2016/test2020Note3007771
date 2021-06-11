  PRIVATE SECTION.
    CLASS-METHODS:
      "! This method takes hex input maximal length 20bytes (40 chars)
      "! and converts to base32 string of length 32.
      hex_to_base32 IMPORTING i_hex_string    TYPE xstring
                    RETURNING VALUE(e_base32) TYPE string,

      "! This method takes hex input maximal length 20bytes (40 chars)
      "! and converts to base41 string of length 30.<br/>
      "! BUT ATTENTION: It is not real 1 to 1 conversion due to limits in high decimal numbers.
      "! Therefore the hex_string is split into 5 chunks, each having 8 chars, and then these 8
      "! chars are converted from hex to base41!
      hex_to_base41 IMPORTING i_hex_string    TYPE xstring
                    RETURNING VALUE(e_base41) TYPE string.



