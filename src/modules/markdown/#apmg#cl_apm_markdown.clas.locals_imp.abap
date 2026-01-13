*!
CLASS lcl_string IMPLEMENTATION.

  METHOD get_data.
    result = data.
  ENDMETHOD.

  METHOD set_data.
    me->data = data.
  ENDMETHOD.

  METHOD lif_value_type~copy.
    " Copies the value of the source object to itself
    DATA string TYPE REF TO lcl_string.

    string ?= source.
    data = string->data.
  ENDMETHOD.
ENDCLASS.


*!
CLASS lcl_string_array IMPLEMENTATION.

  METHOD get_data.
    result = data.
  ENDMETHOD.

  METHOD set_data.
    me->data = data.
  ENDMETHOD.

  METHOD append.
    " Append a value to the end of the array
    APPEND value TO me->data.
  ENDMETHOD.

  METHOD append_array.
    " Append the items of an array to this array
    FIELD-SYMBOLS <item> TYPE string.

    LOOP AT array->data ASSIGNING <item>.
      append( <item> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.
    " Deletes a value from the array
    DELETE data WHERE table_line = value.
  ENDMETHOD.

  METHOD find_val.
    " Returns the index of the first occurrence of a value in the array,
    "  or 0 if not found.
    result = line_index( data[ table_line = value ] ).
  ENDMETHOD.

  METHOD lif_value_type~copy.
    " Copies the value of the source object to itself
    DATA sa TYPE REF TO lcl_string_array.

    sa ?= source.
    data = sa->data.
  ENDMETHOD.
ENDCLASS.


*!
CLASS lcl_hashmap IMPLEMENTATION.

  METHOD constructor.
    " Hashmap constructor
    " @parameter value_type The value part class name. This must be a valid
    "                       ABAP class name, or a composition of valid ABAP
    "                       class names separated by a colon.
    IF value_type CS ':'.
      FIND REGEX '^([^\s:]+)(?::(.+))?$' IN value_type
        SUBMATCHES me->value_type me->subsequent_hashmap_value_type ##REGEX_POSIX.
      IF sy-subrc <> 0.
        me->value_type = value_type.
      ENDIF.
    ELSE.
      me->value_type = value_type.
    ENDIF.
    TRANSLATE me->value_type TO UPPER CASE.
    TRANSLATE me->subsequent_hashmap_value_type TO UPPER CASE.
  ENDMETHOD.

  METHOD new.
    " Adds a new item to the hashmap
    " The value part in the new item will be created dynamically with
    " the type passed to the constructor (sorta like a template based hashmap).
    " @return The instance of the created item's value part, or empty if the item already exists.
    DATA new_item TYPE ty_item.

    FIELD-SYMBOLS <item> TYPE ty_item.

    new_item-key = key.
    INSERT new_item INTO TABLE data ASSIGNING <item>.
    CHECK sy-subrc = 0.

    IF me->value_type = 'LCL_HASHMAP' AND me->subsequent_hashmap_value_type IS NOT INITIAL.
      <item>-value = NEW lcl_hashmap( value_type = me->subsequent_hashmap_value_type ).
    ELSE.
      CREATE OBJECT <item>-value TYPE (me->value_type).
    ENDIF.
    result = <item>-value.
  ENDMETHOD.

  METHOD exists.
    " Checks if a item exists in the hashmap.
    " @return A flag indicating if the item exists.
    IF line_exists( data[ key = key ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    " Gets an item reference from the hashmap.
    " If the item is not found, a new item is created, as if using the method new.
    " @return The reference to the value part of the item.
    FIELD-SYMBOLS <item> TYPE ty_item.

    READ TABLE data ASSIGNING <item> WITH KEY key = key.
    IF sy-subrc = 0.
      result = <item>-value.
    ELSE.
      result = new( key ).
    ENDIF.
  ENDMETHOD.

  METHOD set.
    " Sets the value of an item in the hashmap.
    " If the item does not yet exist, an item is created with the passed key/value pair.
    " If the item already exists, its value is replaced with the passed value.
    DATA item TYPE REF TO lif_value_type.

    item = get( key ).
    item->copy( value ).
  ENDMETHOD.

  METHOD delete.
    " Deletes an item from the hashmap.
    DELETE data WHERE key = key.
  ENDMETHOD.

  METHOD get_data.
    result = data.
  ENDMETHOD.

  METHOD set_data.
    me->data = data.
  ENDMETHOD.

  METHOD lif_value_type~copy.
    " Copies the contents of another hashmap to this hashmap
    " @parameter hashmap The other (source) hashmap
    DATA: hashmap TYPE REF TO lcl_hashmap,
          value   TYPE REF TO lif_value_type.

    FIELD-SYMBOLS <item> TYPE ty_item.

    hashmap ?= source.
    LOOP AT hashmap->data ASSIGNING <item>.
      value = new( <item>-key ).
      value->copy( <item>-value ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


*!
CLASS lcl_alerts IMPLEMENTATION.
  METHOD get.
    IF line CS '[!NOTE]'.
      result-tag   = '[!NOTE!]'.
      result-class = 'alert-note'.
      result-color = '#4493F8'.
      result-icon  = note( ).
      result-text  = 'Note'.
    ELSEIF line CS '[!TIP]'.
      result-tag   = '[!TIP!]'.
      result-class = 'alert-tip'.
      result-color = '#3FB950'.
      result-icon  = tip( ).
      result-text  = 'Tip'.
    ELSEIF line CS '[!IMPORTANT]'.
      result-tag   = '[!IMPORTANT!]'.
      result-class = 'alert-important'.
      result-color = '#AB7DF8'.
      result-icon  = important( ).
      result-text  = 'Important'.
    ELSEIF line CS '[!WARNING]'.
      result-tag   = '[!WARNING!]'.
      result-class = 'alert-warning'.
      result-color = '#D29922'.
      result-icon  = warning( ).
      result-text  = 'Warning'.
    ELSEIF line CS '[!CAUTION]'.
      result-tag   = '[!CAUTION!]'.
      result-class = 'alert-caution'.
      result-color = '#F85149'.
      result-icon  = caution( ).
      result-text  = 'Caution'.
    ENDIF.
  ENDMETHOD.

  METHOD note.
    result = '<svg class="info" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true">'
      && '<path d="M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8Zm8-6.5a6.5 6.5 0 1 0 0 13 6.5 6.5 0 0 0 0-13ZM6.5 7.75A.75.75'
      && ' 0 0 1 7.25 7h1a.75.75 0 0 1 .75.75v2.75h.25a.75.75 0 0 1 0 1.5h-2a.75.75 0 0 1 0-1.5h.25v-2h-.25a.75.75'
      && ' 0 0 1-.75-.75ZM8 6a1 1 0 1 1 0-2 1 1 0 0 1 0 2Z" stroke="#4493F8">'
      && '</path></svg>'.
  ENDMETHOD.

  METHOD tip.
    result = '<svg class="light-bulb" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true">'
      && '<path d="M8 1.5c-2.363 0-4 1.69-4 3.75 0 .984.424 1.625.984 2.304l.214.253c.223.264.47.556.673.848.284.'
      && '411.537.896.621 1.49a.75.75 0 0 1-1.484.211c-.04-.282-.163-.547-.37-.847a8.456 8.456 0 0 0-.542-.68c-.'
      && '084-.1-.173-.205-.268-.32C3.201 7.75 2.5 6.766 2.5 5.25 2.5 2.31 4.863 0 8 0s5.5 2.31 5.5 5.25c0 1.516-'
      && '.701 2.5-1.328 3.259-.095.115-.184.22-.268.319-.207.245-.383.453-.541.681-.208.3-.33.565-.37.847a.751.751'
      && ' 0 0 1-1.485-.212c.084-.593.337-1.078.621-1.489.203-.292.45-.584.673-.848.075-.088.147-.173.213-.253.561-'
      && '.679.985-1.32.985-2.304 0-2.06-1.637-3.75-4-3.75ZM5.75 12h4.5a.75.75 0 0 1 0 1.5h-4.5a.75.75 0 0 1 0-1.'
      && '5ZM6 15.25a.75.75 0 0 1 .75-.75h2.5a.75.75 0 0 1 0 1.5h-2.5a.75.75 0 0 1-.75-.75Z" stroke="#3FB950">'
      && '</path></svg>'.
  ENDMETHOD.

  METHOD important.
    result = '<svg class="report" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true">'
      && '<path d="M0 1.75C0 .784.784 0 1.75 0h12.5C15.216 0 16 .784 16 1.75v9.5A1.75 1.75 0 0 1 14.25 13H8.06l-2'
      && '.573 2.573A1.458 1.458 0 0 1 3 14.543V13H1.75A1.75 1.75 0 0 1 0 11.25Zm1.75-.25a.25.25 0 0 0-.25.25v9.5c0'
      && ' .138.112.25.25.25h2a.75.75 0 0 1 .75.75v2.19l2.72-2.72a.749.749 0 0 1 .53-.22h6.5a.25.25 0 0 0 .25-.25v-'
      && '9.5a.25.25 0 0 0-.25-.25Zm7 2.25v2.5a.75.75 0 0 1-1.5 0v-2.5a.75.75 0 0 1 1.5 0ZM9 9a1 1 0 1 1-2 0 1 1 0'
      && ' 0 1 2 0Z" stroke="#AB7DF8">'
      && '</path></svg>'.
  ENDMETHOD.

  METHOD warning.
    result = '<svg class="alert" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true">'
      && '<path d="M6.457 1.047c.659-1.234 2.427-1.234 3.086 0l6.082 11.378A1.75 1.75 0 0 1 14.082 15H1.918a1.75'
      && ' 1.75 0 0 1-1.543-2.575Zm1.763.707a.25.25 0 0 0-.44 0L1.698 13.132a.25.25 0 0 0 .22.368h12.164a.25.25 0'
      && ' 0 0 .22-.368Zm.53 3.996v2.5a.75.75 0 0 1-1.5 0v-2.5a.75.75 0 0 1 1.5 0ZM9 11a1 1 0 1 1-2 0 1 1 0 0 1'
      && ' 2 0Z" stroke="#D29922">'
      && '</path></svg>'.
  ENDMETHOD.

  METHOD caution.
    result = '<svg class="stop" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true">'
      && '<path d="M4.47.22A.749.749 0 0 1 5 0h6c.199 0 .389.079.53.22l4.25 4.25c.141.14.22.331.22.53v6a.749.749'
      && ' 0 0 1-.22.53l-4.25 4.25A.749.749 0 0 1 11 16H5a.749.749 0 0 1-.53-.22L.22 11.53A.749.749 0 0 1 0 11V5c0-'
      && '.199.079-.389.22-.53Zm.84 1.28L1.5 5.31v5.38l3.81 3.81h5.38l3.81-3.81V5.31L10.69 1.5ZM8 4a.75.75 0 0 1'
      && ' .75.75v3.5a.75.75 0 0 1-1.5 0v-3.5A.75.75 0 0 1 8 4Zm0 8a1 1 0 1 1 0-2 1 1 0 0 1 0 2Z" stroke="#F85149">'
      && '</path></svg>'.
  ENDMETHOD.
ENDCLASS.
