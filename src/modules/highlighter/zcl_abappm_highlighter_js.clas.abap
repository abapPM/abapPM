CLASS zcl_abappm_highlighter_js DEFINITION
  PUBLIC
  INHERITING FROM zcl_abappm_highlighter
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      " JavaScript
      " 1) General keywords
      " 2) Variable types
      " 3) HTML Tags
      BEGIN OF c_css,
        keyword   TYPE string VALUE 'keyword',
        text      TYPE string VALUE 'text',
        comment   TYPE string VALUE 'comment',
        variables TYPE string VALUE 'variables',
      END OF c_css,
      BEGIN OF c_token,
        keyword   TYPE c VALUE 'K',
        text      TYPE c VALUE 'T',
        comment   TYPE c VALUE 'C',
        variables TYPE c VALUE 'V',
      END OF c_token,
      BEGIN OF c_regex,
        " comments /* ... */ or //
        comment TYPE string VALUE '\/\*.*\*\/|\/\*|\*\/|\/\/',
        " single or double quoted strings
        text    TYPE string VALUE '"|''|`',
        " in general keywords don't contain numbers (except -ms-scrollbar-3dlight-color)
        keyword TYPE string VALUE '\b[a-z-]+\b',
      END OF c_regex.

    CLASS-METHODS class_constructor.

    METHODS constructor.

  PROTECTED SECTION.

    TYPES: ty_token TYPE c LENGTH 1.

    TYPES: BEGIN OF ty_keyword,
             keyword TYPE string,
             token   TYPE ty_token,
           END OF ty_keyword.

    CLASS-DATA keywords TYPE HASHED TABLE OF ty_keyword WITH UNIQUE KEY keyword.
    CLASS-DATA comment TYPE abap_bool.

    CLASS-METHODS init_keywords.

    CLASS-METHODS insert_keywords
      IMPORTING
        list  TYPE string
        token TYPE ty_token.

    CLASS-METHODS is_keyword
      IMPORTING
        chunk         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS order_matches REDEFINITION.

    METHODS parse_line REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abappm_highlighter_js IMPLEMENTATION.


  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR comment.

    " Initialize instances of regular expression
    add_rule( regex = c_regex-keyword
              token = c_token-keyword
              style = c_css-keyword ).

    add_rule( regex = c_regex-comment
              token = c_token-comment
              style = c_css-comment ).

    add_rule( regex = c_regex-text
              token = c_token-text
              style = c_css-text ).

    " Styles for keywords
    add_rule( regex = ''
              token = c_token-variables
              style = c_css-variables ).

  ENDMETHOD.


  METHOD init_keywords.

    CLEAR keywords.

    " 1) General keywords
    DATA(list) =
    'alert|all|body|break|bytetostring|case|continue|default|delete|do|document|else|event|export|for|function|if|' &&
    'import|in|innerhtml|isnan|item|mimetypes|navigator|new|onabort|onblur|onchange|onclick|ondblclick|ondragdrop|' &&
    'onerror|onfocus|onkeydown|onkeypress|onkeyup|onload|onmousedown|onmousemove|onmouseout|onmouseover|onmouseup|' &&
    'onmove|onreset|onselect|onsubmit|onunload|onresize|options|parsefloat|parseint|prototype|return|screen|switch|' &&
    'unit|var|void|while|window|with|anchor|applet|area|button|checkbox|fileupload|form|frame|hidden|link|mimetype|' &&
    'password|plugin|radio|reset|select|submit|text|textarea|abs|acos|alert|anchor|asin|atan|atan2|back|big|blink|' &&
    'blur|bold|captureevents|ceil|charat|charcodeat|clearinterval|cleartimeout|click|close|concat|confirm|cos|' &&
    'disableexternalcapture|enableexternalcapture|eval|exp|find|fixed|floor|focus|fontcolor|fontsize|forward|' &&
    'fromcharcode|getdate|getday|getelementbyid|gethours|getminutes|getmonth|getoptionvalue|getoptionvaluecount|' &&
    'getseconds|getselection|gettime|gettimezoneoffset|getyear|go|handleevent|home|indexof|italics|javaenabled|join|' &&
    'lastindexof|link|load|log|match|max|min|moveabove|movebelow|moveby|moveto|movetoabsolute|open|parse|plugins|' &&
    'pop|pow|preference|print|prompt|push|random|refresh|releaseevents|reload|replace|reset|resizeby|resizeto|' &&
    'reverse|round|routeevent|scroll|scrollby|scrollto|search|select|setdate|sethours|setinterval|setminutes|' &&
    'setmonth|setseconds|settime|settimeout|setyear|shift|sin|slice|small|sort|splice|split|sqrt|stop|strike|sub|' &&
    'submit|substr|substring|sup|taintenabled|tan|togmtstring|tolocalestring|tolowercase|tostring|touppercase|' &&
    'unshift|unwatch|utc|valueof|watch|write|writeln|e|ln10|ln2|log10e|log2e|max_value|min_value|negative_infinity|' &&
    'nan|pi|positive_infinity|url|above|action|alinkcolor|anchors|appcodename|appname|appversion|applets|arguments|' &&
    'arity|availheight|availwidth|background|backgroundcolor|below|bgcolor|border|bottom|caller|cancelbubble|' &&
    'checked|clientheight|clientwidth|clientx|clienty|clip|closed|color|colordepth|complete|constructor|cookie|' &&
    'count|current|defaultchecked|defaultselected|defaultstatus|defaultvalue|description|display|document|domain|' &&
    'elements|embeds|enabledplugin|encoding|false|fgcolor|filename|form|formname|forms|frames|hash|height|history|' &&
    'host|hostname|href|hspace|images|innerheight|innerwidth|language|lastmodified|layers|left|length|linkcolor|' &&
    'links|location|locationbar|lowsrc|menubar|method|mimetypes|name|next|null|offsetheight|offsetleft|offsetparent|' &&
    'offsetwidth|opener|outerheight|outerwidth|pagex|pagexoffset|pagey|pageyoffset|parent|parentlayer|pathname|' &&
    'personalbar|pixeldepth|platform|plugins|port|poswidth|previous|protocol|prototype|referrer|right|scrolltop|' &&
    'scrollbars|search|selected|selectedindex|self|siblingabove|siblingbelow|src|srcelement|status|statusbar|style|' &&
    'suffixes|tags|target|text|this|title|toolbar|top|true|type|useragent|value|visibility|vlinkcolor|vspace|width|' &&
    'window|zindex'.
    insert_keywords( list  = list
                     token = c_token-keyword ).

    " 2) Variable types
    list  =
    'array|boolean|date|function|image|layer|math|number|object|option|regexp|string'.
    insert_keywords( list  = list
                     token = c_token-variables ).

  ENDMETHOD.


  METHOD insert_keywords.

    SPLIT list AT '|' INTO TABLE DATA(keyword_list).

    LOOP AT keyword_list ASSIGNING FIELD-SYMBOL(<keyword>).
      DATA(keyword) = VALUE ty_keyword(
        keyword = <keyword>
        token   = token ).
      INSERT keyword INTO TABLE keywords.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_keyword.

    DATA(keyword) = to_lower( chunk ).
    READ TABLE keywords WITH TABLE KEY keyword = keyword TRANSPORTING NO FIELDS.
    result = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD order_matches.

    FIELD-SYMBOLS <prev_match> TYPE ty_match.

    " Longest matches
    SORT matches BY offset length DESCENDING.

    DATA(line_len)   = strlen( line ).
    DATA(prev_token) = ''.
    DATA(prev_end)   = ''.

    " Check if this is part of multi-line comment and mark it accordingly
    IF comment = abap_true.
      READ TABLE matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR matches.
        APPEND INITIAL LINE TO matches ASSIGNING FIELD-SYMBOL(<match>).
        <match>-token = c_token-comment.
        <match>-offset = 0.
        <match>-length = line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT matches ASSIGNING <match>.
      " Delete matches after open text match
      IF prev_token = c_token-text AND <match>-token <> c_token-text.
        CLEAR <match>-token.
        CONTINUE.
      ENDIF.

      DATA(match) = substring( val = line
                               off = <match>-offset
                               len = <match>-length ).

      CASE <match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <match>-offset < prev_end.
            CLEAR <match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific token
          match = to_lower( match ).
          READ TABLE keywords ASSIGNING FIELD-SYMBOL(<keyword>) WITH TABLE KEY keyword = match.
          IF sy-subrc = 0.
            <match>-token = <keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF match = '/*'.
            DELETE matches WHERE offset > <match>-offset.
            <match>-length = line_len - <match>-offset.
            comment = abap_true.
          ELSEIF match = '//'.
            DELETE matches WHERE offset > <match>-offset.
            <match>-length = line_len - <match>-offset.
          ELSEIF match = '*/'.
            DELETE matches WHERE offset < <match>-offset.
            <match>-length = <match>-offset + 2.
            <match>-offset = 0.
            comment = abap_false.
          ELSE.
            DATA(cmmt_end) = <match>-offset + <match>-length.
            DELETE matches WHERE offset > <match>-offset AND offset <= cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <match>-text_tag = match.
          IF prev_token = c_token-text.
            IF <match>-text_tag = <prev_match>-text_tag.
              <prev_match>-length = <match>-offset + <match>-length - <prev_match>-offset.
              CLEAR prev_token.
            ENDIF.
            CLEAR <match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      prev_token = <match>-token.
      prev_end   = <match>-offset + <match>-length.
      ASSIGN <match> TO <prev_match>.
    ENDLOOP.

    DELETE matches WHERE token IS INITIAL.

  ENDMETHOD.


  METHOD parse_line. "REDEFINITION

    result = super->parse_line( line ).

    " Remove non-keywords
    LOOP AT result ASSIGNING FIELD-SYMBOL(<match>) WHERE token = c_token-keyword.
      IF abap_false = is_keyword( substring( val = line
                                             off = <match>-offset
                                             len = <match>-length ) ).
        CLEAR <match>-token.
      ENDIF.
    ENDLOOP.

    DELETE result WHERE token IS INITIAL.

  ENDMETHOD.
ENDCLASS.
