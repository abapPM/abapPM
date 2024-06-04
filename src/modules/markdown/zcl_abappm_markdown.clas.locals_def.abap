*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
"!
"! Value type interface
"!
INTERFACE lif_value_type.
  METHODS: copy IMPORTING source TYPE REF TO lif_value_type.
ENDINTERFACE.                    "lif_value_type

"!
"! String class for use in template objects
"!
CLASS lcl_string DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_value_type.
    DATA: data TYPE string.
    ALIASES: copy FOR lif_value_type~copy.
ENDCLASS.                    "lcl_string DEFINITION

"!
"! String array class for use in template objects
"!
CLASS lcl_string_array DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_value_type.
    DATA: data TYPE TABLE OF string.

    METHODS:
      append IMPORTING value TYPE clike,
      append_array IMPORTING array TYPE REF TO lcl_string_array,
      delete IMPORTING value TYPE clike,
      find_val IMPORTING value TYPE clike RETURNING VALUE(index) TYPE i.

    ALIASES: copy FOR lif_value_type~copy.
ENDCLASS.                    "lcl_array DEFINITION

"!
"! Hashmap template class
"! The key type is `string`, and the value type must be an object.
"!
"! A compound value type may be used, separating the basic type and its subsequent
"!  value type by a colon.
"! Ex: 'lcl_hashmap:lcl_string_array' =&gt; The value type will be lcl_hashmap,
"!     and the value hashmaps' values type will be of type lcl_string_array.
"! Ex: 'lcl_hashmap:lcl_hashmap:lcl_hashmap:lcl_string' =&gt; Recursive composition,
"!     for use of a 4-dimensional hashmap.
"!
CLASS lcl_hashmap DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES lif_value_type.
    TYPES:
      BEGIN OF ty_item,
        key   TYPE string,
        value TYPE REF TO lif_value_type,
      END OF ty_item,
      ty_hashmap TYPE HASHED TABLE OF ty_item WITH UNIQUE KEY key.

    DATA: data TYPE ty_hashmap.

    METHODS:
      constructor
        IMPORTING
          VALUE(value_type) TYPE clike DEFAULT 'lcl_string',
      new
        IMPORTING
          key          TYPE clike
        RETURNING
          VALUE(value) TYPE REF TO lif_value_type,
      exists
        IMPORTING
          key           TYPE clike
        RETURNING
          VALUE(exists) TYPE abap_bool,
      get
        IMPORTING
          key          TYPE clike
        RETURNING
          VALUE(value) TYPE REF TO lif_value_type,
      set
        IMPORTING
          key   TYPE clike
          value TYPE REF TO lif_value_type,
      delete
        IMPORTING
          key TYPE string.

    ALIASES: copy FOR lif_value_type~copy.

  PRIVATE SECTION.
    DATA: value_type                    TYPE string,
          subsequent_hashmap_value_type TYPE string.
ENDCLASS.                    "lcl_hashmap DEFINITION
