CLASS cl_cts_hot_utility DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_split_text_50,
        chunk1 TYPE char50,
        chunk2 TYPE char50,
        chunk3 TYPE char50,
        chunk4 TYPE char50,
      END OF ty_split_text_50.

    CLASS-METHODS:
      "! Creates hash (SHA-1) for input string and returns this hash
      "! as string in base32 (0 to V) format.
      string_to_hash_as_base32 IMPORTING i_string                    TYPE string
                               RETURNING VALUE(e_hash_base32_string) TYPE string
                               RAISING   cx_abap_message_digest,

      "! Creates hash (SHA-1) for input string and returns this hash
      "! as string in base41 (0 to Z and -_.=;) format.<br/>
      "! BUT ATTENTION: SHA-1 result, hex string, is not really converted 1 to 1 base 41 due to limits
      "! in high decimal numbers. Therefore the hex_string is split into 5 chunks, each having 8 chars,
      "! and then these 8 chars are converted from hex to base41!
      string_to_hash_as_base41 IMPORTING i_string                    TYPE string
                               RETURNING VALUE(e_hash_base41_string) TYPE string
                               RAISING   cx_abap_message_digest,

      "! Splits the passed text into 4 chunks of 50 chars and "forgets" the rest.
      split_text_50_chars
        IMPORTING
          i_text          TYPE string
        RETURNING
          VALUE(r_result) TYPE ty_split_text_50,

      "! Convert timestamp to ISO 8601 (YYYY-MM-DD hh:mm:ss)
      "! @parameter iv_timestamp | Timestamp to format. If not defined, timestamp will be created and formatted
      "! @parameter rv_result |  Timestamp formatted as YYYY-MM-DD hh:mm:ss
      get_formatted_timestamp
        IMPORTING
          iv_timestamp     TYPE timestamp OPTIONAL
        RETURNING
          VALUE(rv_result) TYPE string,

      "! This method takes hex input maximal length 20bytes (40 chars)
      "! and converts to base41 string of length 256. <br/>
      "! @parameter iv_file_content | xstring input
      "! @parameter rv_web_ide_sha256 | string of length 256
      "! @raising cx_cts_hta_hdi | exception
      string_to_hash_as_base256
        IMPORTING
          iv_file_content          TYPE xstring
        RETURNING
          VALUE(rv_web_ide_sha256) TYPE string
        RAISING
          cx_cts_hta_hdi,

      "! Convert string into a text table
      "! @parameter iv_buffer | String
      "! @parameter ev_buffer_size | Size of the string buffer
      "! @parameter et_texttab | Text table
      convert_string_to_text_table
        IMPORTING
          iv_buffer      TYPE string
        EXPORTING
          ev_buffer_size TYPE i
          et_texttab     TYPE STANDARD TABLE.
