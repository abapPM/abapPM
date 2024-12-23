CLASS ZCL_ABAPPM_HIGHLIGHTER_DIFF DEFINITION
  PUBLIC
  INHERITING FROM ZCL_ABAPPM_HIGHLIGHTER
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_css,
        ins     TYPE string VALUE 'diff_ins',
        del     TYPE string VALUE 'diff_del',
        test    TYPE string VALUE 'diff_upd',
        comment TYPE string VALUE 'comment',
      END OF c_css,
      BEGIN OF c_token,
        ins     TYPE c VALUE 'I',
        del     TYPE c VALUE 'D',
        test    TYPE c VALUE 'T',
        comment TYPE c VALUE 'C',
      END OF c_token,
      BEGIN OF c_regex,
        ins     TYPE string VALUE '^\+.*',
        del     TYPE string VALUE '^-.*',
        test    TYPE string VALUE '^!.*',
        comment TYPE string VALUE '^#.*',
      END OF c_regex.

    METHODS constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPPM_HIGHLIGHTER_DIFF IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expressions

    add_rule( iv_regex = c_regex-ins
              iv_token = c_token-ins
              iv_style = c_css-ins ).

    add_rule( iv_regex = c_regex-del
              iv_token = c_token-del
              iv_style = c_css-del ).

    add_rule( iv_regex = c_regex-test
              iv_token = c_token-test
              iv_style = c_css-test ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
ENDCLASS.
