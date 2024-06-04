*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
*!
CLASS lcl_string IMPLEMENTATION.
  METHOD lif_value_type~copy.
    " Copies the value of the source object to itself
    DATA lo_string TYPE REF TO lcl_string.
    lo_string ?= source.
    me->data = lo_string->data.
  ENDMETHOD.                    "lif_value_type~copy
ENDCLASS.                    "lcl_string IMPLEMENTATION


*!
CLASS lcl_string_array IMPLEMENTATION.
  METHOD append.
    " Append a value to the end of the array
    APPEND value TO me->data.
  ENDMETHOD.                    "append

  METHOD append_array.
    " Append the items of an array to this array
    FIELD-SYMBOLS <item> TYPE string.
    LOOP AT array->data ASSIGNING <item>.
      append( <item> ).
    ENDLOOP.
  ENDMETHOD.                    "append_array

  METHOD delete.
    " Deletes a value from the array
    DELETE me->data WHERE table_line = value.
  ENDMETHOD.                    "delete

  METHOD find_val.
    " Returns the index of the first occurrence of a value in the array,
    "  or 0 if not found.
    READ TABLE me->data WITH KEY table_line = value
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      index = sy-tabix.
    ENDIF.
  ENDMETHOD.                    "find

  METHOD lif_value_type~copy.
    " Copies the value of the source object to itself
    DATA lo_sa TYPE REF TO lcl_string_array.
    lo_sa ?= source.
    me->data = lo_sa->data.
  ENDMETHOD.                    "lif_value_type~copy
ENDCLASS.                    "lcl_array IMPLEMENTATION


*!
CLASS lcl_hashmap IMPLEMENTATION.
  METHOD constructor.
    " Hashmap constructor
    " @parameter value_type The value part class name. This must be a valid
    "                       ABAP class name, or a composition of valid ABAP
    "                       class names separated by a colon.
    IF value_type CS ':'.
      FIND REGEX '^([^\s:]+)(?::(.+))?$' IN value_type
        SUBMATCHES me->value_type me->subsequent_hashmap_value_type.
      IF sy-subrc <> 0.
        me->value_type = value_type.
      ENDIF.
    ELSE.
      me->value_type = value_type.
    ENDIF.
    TRANSLATE me->value_type TO UPPER CASE.
    TRANSLATE me->subsequent_hashmap_value_type TO UPPER CASE.
  ENDMETHOD.                    "constructor

  METHOD new.
    " Adds a new item to the hashmap
    " The value part in the new item will be created dynamically with
    " the type passed to the constructor (sorta like a template based hashmap).
    " @return The instance of the created item's value part, or empty if the item already exists.
    DATA ls_new_item TYPE ty_item.
    FIELD-SYMBOLS <item> TYPE ty_item.
    ls_new_item-key = key.
    INSERT ls_new_item INTO TABLE me->data ASSIGNING <item>.
    CHECK sy-subrc = 0.

    IF me->value_type = 'LCL_HASHMAP' AND me->subsequent_hashmap_value_type IS NOT INITIAL.
      CREATE OBJECT <item>-value TYPE lcl_hashmap
        EXPORTING
          value_type = me->subsequent_hashmap_value_type.
    ELSE.
      CREATE OBJECT <item>-value TYPE (me->value_type).
    ENDIF.
    value = <item>-value.
  ENDMETHOD.                    "new

  METHOD exists.
    " Checks if a item exists in the hashmap.
    " @return A flag indicating if the item exists.
    READ TABLE me->data WITH KEY key = key
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      exists = 'X'.
    ENDIF.
  ENDMETHOD.                    "exists

  METHOD get.
    " Gets an item reference from the hashmap.
    " If the item is not found, a new item is created, as if using the method new.
    " @return The reference to the value part of the item.
    FIELD-SYMBOLS <item> TYPE ty_item.
    READ TABLE me->data ASSIGNING <item>
      WITH KEY key = key.
    IF sy-subrc = 0.
      value = <item>-value.
    ELSE.
      value = new( key ).
    ENDIF.
  ENDMETHOD.                    "get

  METHOD set.
    " Sets the value of an item in the hashmap.
    " If the item does not yet exist, an item is created with the passed key/value pair.
    " If the item already exists, its value is replaced with the passed value.
    DATA lo_item TYPE REF TO lif_value_type.
    lo_item = get( key ).
    lo_item->copy( value ).
  ENDMETHOD.                    "set

  METHOD delete.
    " Deletes an item from the hashmap.
    DELETE me->data WHERE key = key.
  ENDMETHOD.                    "delete

  METHOD lif_value_type~copy.
    " Copies the contents of another hashmap to this hashmap
    " @parameter hashmap The other (source) hashmap
    DATA: lo_hashmap TYPE REF TO lcl_hashmap,
          lo_value   TYPE REF TO lif_value_type.
    FIELD-SYMBOLS <item> TYPE ty_item.
    lo_hashmap ?= source.
    LOOP AT lo_hashmap->data ASSIGNING <item>.
      lo_value = new( <item>-key ).
      lo_value->copy( <item>-value ).
    ENDLOOP.
  ENDMETHOD.                    "lif_value_type~copy
ENDCLASS.                    "lcl_hashmap IMPLEMENTATION
