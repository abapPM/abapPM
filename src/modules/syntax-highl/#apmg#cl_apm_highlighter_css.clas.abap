CLASS /apmg/cl_apm_highlighter_css DEFINITION
  PUBLIC
  INHERITING FROM /apmg/cl_apm_highlighter
  CREATE PUBLIC.

************************************************************************
* Syntax Highlighter
*
* Copyright (c) 2014 abapGit Contributors
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS:
      " CSS Standard            https://www.w3.org/TR/css-2025/
      " CSS Reference           https://www.w3schools.com/cssref/default.asp
      " We used a mixture of above as reference for the keyword list
      " 1) CSS Properties       https://www.w3schools.com/cssref/default.asp
      " 2) CSS Values           https://www.w3.org/TR/css-values-4/
      " 3) CSS Selectors        https://www.w3.org/TR/css-2025/#selectors
      " 4) CSS Functions        https://www.w3schools.com/cssref/css_functions.asp
      " 5) CSS Colors           https://www.w3schools.com/colors/colors_names.asp
      " 6) CSS Extensions
      " 7) CSS At-Rules         https://www.w3.org/TR/css-2025/#at-rules
      " 8) HTML Tags
      BEGIN OF c_css,
        keyword    TYPE string VALUE 'keyword',
        text       TYPE string VALUE 'text',
        comment    TYPE string VALUE 'comment',
        selectors  TYPE string VALUE 'selectors',
        units      TYPE string VALUE 'units',
        properties TYPE string VALUE 'properties',
        values     TYPE string VALUE 'values',
        functions  TYPE string VALUE 'functions',
        colors     TYPE string VALUE 'colors',
        extensions TYPE string VALUE 'extensions',
        at_rules   TYPE string VALUE 'at_rules',
        html       TYPE string VALUE 'html',
      END OF c_css,
      BEGIN OF c_token,
        keyword    TYPE c VALUE 'K',
        text       TYPE c VALUE 'T',
        comment    TYPE c VALUE 'C',
        selectors  TYPE c VALUE 'S',
        units      TYPE c VALUE 'U',
        properties TYPE c VALUE 'P',
        values     TYPE c VALUE 'V',
        functions  TYPE c VALUE 'F',
        colors     TYPE c VALUE 'Z',
        extensions TYPE c VALUE 'E',
        at_rules   TYPE c VALUE 'A',
        html       TYPE c VALUE 'H',
      END OF c_token,
      BEGIN OF c_regex,
        " comments /* ... */
        comment   TYPE string VALUE '\/\*.*\*\/|\/\*|\*\/',
        " single or double quoted strings
        text      TYPE string VALUE '("[^"]*")|(''[^'']*'')|(`[^`]*`)',
        " Digits occur in HTML tags and CSS functions (h1, matrix3d)
        keyword   TYPE string VALUE '--[a-z][a-z0-9\-]*\b|@-?[a-z][a-z0-9\-]*\b|\b[a-z][a-z0-9\-]*\b',
        " selectors begin with :
        selectors TYPE string VALUE '::?[a-z][a-z0-9\-]*\b',
        " CSS numbers followed by a unit or %, excluding the preceding identifier boundary
        units     TYPE string
        VALUE '(^|[^a-z0-9_.-])([+-]?([0-9]+(\.[0-9]+)?|\.[0-9]+)(e[+-]?[0-9]+)?((' &
        'cm|mm|q|in|pt|pc|px|' &
        'em|rem|ex|rex|cap|rcap|ch|rch|ic|ric|lh|rlh|' &
        '[sld]?v(w|h|i|b|min|max)|cqw|cqh|cqi|cqb|cqmin|cqmax|' &
        'deg|grad|rad|turn|s|ms|hz|khz|dpi|dpcm|dppx|x|fr)\b|%))',
      END OF c_regex.

    CLASS-METHODS class_constructor.

    METHODS constructor.

  PROTECTED SECTION.

    TYPES:
      ty_token TYPE c LENGTH 1,
      BEGIN OF ty_keyword,
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



CLASS /apmg/cl_apm_highlighter_css IMPLEMENTATION.


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

    add_rule( regex = c_regex-selectors
              token = c_token-selectors
              style = c_css-selectors ).

    add_rule( regex    = c_regex-units
              token    = c_token-units
              style    = c_css-units
              submatch = 2 ).

    " Styles for keywords
    add_rule( regex = ''
              token = c_token-html
              style = c_css-html ).

    add_rule( regex = ''
              token = c_token-properties
              style = c_css-properties ).

    add_rule( regex = ''
              token = c_token-values
              style = c_css-values ).

    add_rule( regex = ''
              token = c_token-functions
              style = c_css-functions ).

    add_rule( regex = ''
              token = c_token-colors
              style = c_css-colors ).

    add_rule( regex = ''
              token = c_token-extensions
              style = c_css-extensions ).

    add_rule( regex = ''
              token = c_token-at_rules
              style = c_css-at_rules ).

  ENDMETHOD.


  METHOD init_keywords.

    CLEAR keywords.

    " Shared keywords keep the first inserted token unless followed directly by ( (function call).
    " 1) CSS Properties
    DATA(keyword_list) =
    'align-content|align-items|align-self|animation|animation-delay|animation-direction|animation-duration|' &&
    'animation-fill-mode|animation-iteration-count|animation-name|animation-play-state|animation-timing-function|' &&
    'backface-visibility|background|background-attachment|background-blend-mode|background-clip|background-color|' &&
    'background-image|background-origin|background-position|background-repeat|background-size|border|' &&
    'border-bottom|border-bottom-color|border-bottom-left-radius|border-bottom-right-radius|border-bottom-style|' &&
    'border-bottom-width|border-collapse|border-color|border-image|border-image-outset|border-image-repeat|' &&
    'border-image-slice|border-image-source|border-image-width|border-left|border-left-color|border-left-style|' &&
    'border-left-width|border-radius|border-right|border-right-color|border-right-style|border-right-width|' &&
    'border-spacing|border-style|border-top|border-top-color|border-top-left-radius|border-top-right-radius|' &&
    'border-top-style|border-top-width|border-width|box-decoration-break|box-shadow|box-sizing|caption-side|' &&
    'caret-color|clear|clip|color|column-count|column-fill|column-gap|column-rule|column-rule-color|' &&
    'column-rule-style|column-rule-width|column-span|column-width|columns|content|counter-increment|' &&
    'counter-reset|cursor|direction|display|empty-cells|filter|flex|flex-basis|flex-direction|flex-flow|' &&
    'flex-grow|flex-shrink|flex-wrap|float|font|font-family|font-kerning|font-size|font-size-adjust|' &&
    'font-stretch|font-style|font-variant|font-weight|grid|grid-area|grid-auto-columns|grid-auto-flow|' &&
    'grid-auto-rows|grid-column|grid-column-end|grid-column-gap|grid-column-start|grid-gap|grid-row|' &&
    'grid-row-end|grid-row-gap|grid-row-start|grid-template|grid-template-areas|grid-template-columns|' &&
    'grid-template-rows|hanging-punctuation|height|hyphens|isolation|justify-content|' &&
    'letter-spacing|line-height|list-style|list-style-image|list-style-position|list-style-type|margin|' &&
    'margin-bottom|margin-left|margin-right|margin-top|max-height|max-width|min-height|min-width|' &&
    'mix-blend-mode|object-fit|object-position|opacity|order|outline|outline-color|outline-offset|' &&
    'outline-style|outline-width|overflow|overflow-x|overflow-y|padding|padding-bottom|padding-left|' &&
    'padding-right|padding-top|page-break-after|page-break-before|page-break-inside|perspective|' &&
    'perspective-origin|pointer-events|position|quotes|resize|scroll-behavior|tab-size|table-layout|' &&
    'text-align|text-align-last|text-decoration|text-decoration-color|text-decoration-line|' &&
    'text-decoration-style|text-indent|text-justify|text-overflow|text-rendering|text-shadow|text-transform|' &&
    'transform|transform-origin|transform-style|transition|transition-delay|transition-duration|' &&
    'transition-property|transition-timing-function|unicode-bidi|user-select|vertical-align|visibility|' &&
    'white-space|width|word-break|word-spacing|word-wrap|writing-mode|z-index|' &&
    'accent-color|all|appearance|aspect-ratio|backdrop-filter|background-position-x|background-position-y|' &&
    'block-size|border-block|border-block-color|border-block-end|border-block-end-color|' &&
    'border-block-end-style|border-block-end-width|border-block-start|border-block-start-color|' &&
    'border-block-start-style|border-block-start-width|border-block-style|border-block-width|' &&
    'border-end-end-radius|border-end-start-radius|border-inline|border-inline-color|border-inline-end|' &&
    'border-inline-end-color|border-inline-end-style|border-inline-end-width|border-inline-start|' &&
    'border-inline-start-color|border-inline-start-style|border-inline-start-width|border-inline-style|' &&
    'border-inline-width|border-start-end-radius|border-start-start-radius|bottom|break-after|break-before|' &&
    'break-inside|clip-path|contain|container|container-name|container-type|content-visibility|counter-set|' &&
    'font-feature-settings|font-optical-sizing|font-variant-caps|font-variant-east-asian|' &&
    'font-variant-ligatures|font-variant-numeric|font-variant-position|font-variation-settings|gap|' &&
    'image-rendering|inline-size|inset|inset-block|inset-block-end|inset-block-start|inset-inline|' &&
    'inset-inline-end|inset-inline-start|justify-items|justify-self|left|line-break|margin-block|' &&
    'margin-block-end|margin-block-start|margin-inline|margin-inline-end|margin-inline-start|mask|' &&
    'mask-clip|mask-composite|mask-image|mask-mode|mask-origin|mask-position|mask-repeat|mask-size|mask-type|' &&
    'max-block-size|max-inline-size|min-block-size|min-inline-size|offset|offset-anchor|offset-distance|' &&
    'offset-path|offset-position|offset-rotate|orphans|overflow-anchor|overflow-clip-margin|overflow-wrap|' &&
    'overscroll-behavior|overscroll-behavior-block|overscroll-behavior-inline|overscroll-behavior-x|' &&
    'overscroll-behavior-y|padding-block|padding-block-end|padding-block-start|padding-inline|' &&
    'padding-inline-end|padding-inline-start|place-content|place-items|place-self|print-color-adjust|right|' &&
    'rotate|row-gap|scale|scroll-margin|scroll-margin-block|scroll-margin-block-end|scroll-margin-block-start|' &&
    'scroll-margin-bottom|scroll-margin-inline|scroll-margin-inline-end|scroll-margin-inline-start|' &&
    'scroll-margin-left|scroll-margin-right|scroll-margin-top|scroll-padding|scroll-padding-block|' &&
    'scroll-padding-block-end|scroll-padding-block-start|scroll-padding-bottom|scroll-padding-inline|' &&
    'scroll-padding-inline-end|scroll-padding-inline-start|scroll-padding-left|scroll-padding-right|' &&
    'scroll-padding-top|scroll-snap-align|scroll-snap-stop|scroll-snap-type|scrollbar-color|scrollbar-gutter|' &&
    'scrollbar-width|shape-image-threshold|shape-margin|shape-outside|text-combine-upright|' &&
    'text-decoration-skip-ink|text-decoration-thickness|text-orientation|text-size-adjust|' &&
    'text-underline-offset|text-underline-position|text-wrap|top|touch-action|translate|widows|will-change|zoom'.
    insert_keywords( list  = keyword_list
                     token = c_token-properties ).

    " 2) CSS Values
    keyword_list =
    'absolute|auto|block|bold|border-box|both|center|cover|dashed|fixed|hidden|important|' &&
    'inherit|initial|inline-block|italic|max-content|middle|min-content|no-repeat|none|normal|pointer|' &&
    'relative|solid|table-cell|text|underline|unset|revert|revert-layer|' &&
    'alternate|alternate-reverse|always|avoid|avoid-column|avoid-page|backwards|baseline|bolder|capitalize|' &&
    'collapse|column-reverse|contain|content-box|contents|crosshair|default|dense|disc|dotted|double|' &&
    'ease|ease-in|ease-in-out|ease-out|ellipsis|end|fill|flex-end|flex-start|flow-root|forwards|grab|grabbing|' &&
    'groove|help|infinite|inline|inline-flex|inline-grid|inline-table|inside|keep-all|lighter|linear|' &&
    'list-item|lowercase|ltr|move|no-drop|nowrap|not-allowed|outside|overline|paused|progress|repeat|' &&
    'repeat-x|repeat-y|reverse|ridge|round|row|row-reverse|rtl|running|scroll|separate|space|' &&
    'space-around|space-between|space-evenly|start|static|step-end|step-start|sticky|stretch|subgrid|' &&
    'table-caption|table-column|table-column-group|table-footer-group|table-header-group|table-row|' &&
    'table-row-group|uppercase|visible|wait|wrap|wrap-reverse|zoom-in|zoom-out|' &&
    'balance|break-all|break-word|column|cursive|ew-resize|fantasy|horizontal-tb|justify|large|larger|' &&
    'line-through|medium|monospace|n-resize|ne-resize|nesw-resize|ns-resize|nw-resize|nwse-resize|' &&
    'oblique|pre-line|pre-wrap|print|s-resize|sans-serif|screen|se-resize|serif|small|small-caps|' &&
    'smaller|smooth|sw-resize|thick|thin|vertical-lr|vertical-rl|w-resize|x-large|x-small|xx-large|xx-small'.
    insert_keywords( list  = keyword_list
                     token = c_token-values ).

    " 3) CSS Selectors
    keyword_list =
    ':active|::after|::before|:checked|:disabled|:empty|:enabled|:first-child|::first-letter|::first-line|' &&
    ':first-of-type|:focus|:hover|:lang|:last-child|:last-of-type|:link|:not|:nth-child|:nth-last-child|' &&
    ':nth-last-of-type|:nth-of-type|:only-child|:only-of-type|:root|:target|:visited|' &&
    ':any-link|:default|:defined|:dir|:focus-visible|:focus-within|:has|:indeterminate|:in-range|' &&
    ':invalid|:is|:optional|:out-of-range|:placeholder-shown|:read-only|:read-write|:required|' &&
    ':user-invalid|:user-valid|:valid|:where|::backdrop|::file-selector-button|::marker|::part|' &&
    '::placeholder|::selection|::slotted'.
    insert_keywords( list  = keyword_list
                     token = c_token-selectors ).

    " 4) CSS Functions
    keyword_list =
    'attr|blur|brightness|calc|circle|clamp|color|color-mix|conic-gradient|contrast|counter|counters|' &&
    'cubic-bezier|drop-shadow|ellipse|env|fit-content|grayscale|hsl|hsla|hue-rotate|hwb|inset|invert|' &&
    'lab|lch|linear-gradient|matrix|matrix3d|max|min|minmax|oklab|oklch|opacity|perspective|polygon|' &&
    'radial-gradient|repeat|repeating-conic-gradient|repeating-linear-gradient|repeating-radial-gradient|' &&
    'rgb|rgba|rotate|rotate3d|rotatex|rotatey|rotatez|saturate|scale|scale3d|scalex|scaley|scalez|sepia|' &&
    'skew|skewx|skewy|steps|translate|translate3d|translatex|translatey|translatez|url|var|math|theme'.
    insert_keywords( list  = keyword_list
                     token = c_token-functions ).

    " 5) CSS Colors
    keyword_list =
    'currentcolor|transparent|aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|' &&
    'blanchedalmond|blue|blueviolet|brown|' &&
    'burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|' &&
    'darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|' &&
    'darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|' &&
    'deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|' &&
    'ghostwhite|gold|goldenrod|gray|green|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|' &&
    'lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|' &&
    'lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|' &&
    'lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|' &&
    'mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|' &&
    'mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|' &&
    'orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|' &&
    'peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|' &&
    'seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|' &&
    'tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen'.
    insert_keywords( list  = keyword_list
                     token = c_token-colors ).

    " 6) CSS Extensions
    keyword_list =
    'moz|moz-binding|moz-border-bottom-colors|moz-border-left-colors|moz-border-right-colors|' &&
    'moz-border-top-colors|moz-box-align|moz-box-direction|moz-box-flex|moz-box-ordinal-group|' &&
    'moz-box-orient|moz-box-pack|moz-box-shadow|moz-context-properties|moz-float-edge|' &&
    'moz-force-broken-image-icon|moz-image-region|moz-orient|moz-osx-font-smoothing|' &&
    'moz-outline-radius|moz-outline-radius-bottomleft|moz-outline-radius-bottomright|' &&
    'moz-outline-radius-topleft|moz-outline-radius-topright|moz-stack-sizing|moz-system-metric|' &&
    'moz-transform|moz-transform-origin|moz-transition|moz-transition-delay|moz-user-focus|' &&
    'moz-user-input|moz-user-modify|moz-window-dragging|moz-window-shadow|ms|ms-accelerator|' &&
    'ms-block-progression|ms-content-zoom-chaining|ms-content-zoom-limit|' &&
    'ms-content-zoom-limit-max|ms-content-zoom-limit-min|ms-content-zoom-snap|' &&
    'ms-content-zoom-snap-points|ms-content-zoom-snap-type|ms-content-zooming|ms-filter|' &&
    'ms-flow-from|ms-flow-into|ms-high-contrast-adjust|ms-hyphenate-limit-chars|' &&
    'ms-hyphenate-limit-lines|ms-hyphenate-limit-zone|ms-ime-align|ms-overflow-style|' &&
    'ms-scroll-chaining|ms-scroll-limit|ms-scroll-limit-x-max|ms-scroll-limit-x-min|' &&
    'ms-scroll-limit-y-max|ms-scroll-limit-y-min|ms-scroll-rails|ms-scroll-snap-points-x|' &&
    'ms-scroll-snap-points-y|ms-scroll-snap-x|ms-scroll-snap-y|ms-scroll-translation|' &&
    'ms-scrollbar-3dlight-color|ms-scrollbar-arrow-color|ms-scrollbar-base-color|' &&
    'ms-scrollbar-darkshadow-color|ms-scrollbar-face-color|ms-scrollbar-highlight-color|' &&
    'ms-scrollbar-shadow-color|ms-scrollbar-track-color|ms-transform|ms-text-autospace|' &&
    'ms-touch-select|ms-wrap-flow|ms-wrap-margin|ms-wrap-through|o|o-transform|webkit|' &&
    'webkit-animation-trigger|webkit-app-region|webkit-appearance|webkit-aspect-ratio|' &&
    'webkit-backdrop-filter|webkit-background-composite|webkit-border-after|' &&
    'webkit-border-after-color|webkit-border-after-style|webkit-border-after-width|' &&
    'webkit-border-before|webkit-border-before-color|webkit-border-before-style|' &&
    'webkit-border-before-width|webkit-border-end|webkit-border-end-color|' &&
    'webkit-border-end-style|webkit-border-end-width|webkit-border-fit|' &&
    'webkit-border-horizontal-spacing|webkit-border-radius|webkit-border-start|' &&
    'webkit-border-start-color|webkit-border-start-style|webkit-border-start-width|' &&
    'webkit-border-vertical-spacing|webkit-box-align|webkit-box-direction|webkit-box-flex|' &&
    'webkit-box-flex-group|webkit-box-lines|webkit-box-ordinal-group|webkit-box-orient|' &&
    'webkit-box-pack|webkit-box-reflect|webkit-box-shadow|webkit-column-axis|' &&
    'webkit-column-break-after|webkit-column-break-before|webkit-column-break-inside|' &&
    'webkit-column-progression|webkit-cursor-visibility|webkit-dashboard-region|' &&
    'webkit-font-size-delta|webkit-font-smoothing|webkit-highlight|webkit-hyphenate-character|' &&
    'webkit-hyphenate-limit-after|webkit-hyphenate-limit-before|webkit-hyphenate-limit-lines|' &&
    'webkit-initial-letter|webkit-line-align|webkit-line-box-contain|webkit-line-clamp|' &&
    'webkit-line-grid|webkit-line-snap|webkit-locale|webkit-logical-height|' &&
    'webkit-logical-width|webkit-margin-after|webkit-margin-after-collapse|' &&
    'webkit-margin-before|webkit-margin-before-collapse|webkit-margin-bottom-collapse|' &&
    'webkit-margin-collapse|webkit-margin-end|webkit-margin-start|webkit-margin-top-collapse|' &&
    'webkit-marquee|webkit-marquee-direction|webkit-marquee-increment|' &&
    'webkit-marquee-repetition|webkit-marquee-speed|webkit-marquee-style|webkit-mask-box-image|' &&
    'webkit-mask-box-image-outset|webkit-mask-box-image-repeat|webkit-mask-box-image-slice|' &&
    'webkit-mask-box-image-source|webkit-mask-box-image-width|webkit-mask-repeat-x|' &&
    'webkit-mask-repeat-y|webkit-mask-source-type|webkit-max-logical-height|' &&
    'webkit-max-logical-width|webkit-min-logical-height|webkit-min-logical-width|' &&
    'webkit-nbsp-mode|webkit-padding-after|webkit-padding-before|webkit-padding-end|' &&
    'webkit-padding-start|webkit-perspective-origin-x|webkit-perspective-origin-y|' &&
    'webkit-print-color-adjust|webkit-rtl-ordering|webkit-svg-shadow|' &&
    'webkit-tap-highlight-color|webkit-text-combine|webkit-text-decoration-skip|' &&
    'webkit-text-decorations-in-effect|webkit-text-fill-color|webkit-text-security|' &&
    'webkit-text-stroke|webkit-text-stroke-color|webkit-text-stroke-width|webkit-text-zoom|' &&
    'webkit-transform|webkit-transform-origin|webkit-transform-origin-x|' &&
    'webkit-transform-origin-y|webkit-transform-origin-z|webkit-transition|' &&
    'webkit-transition-delay|webkit-user-drag|webkit-user-modify|overflow-clip-box|' &&
    'overflow-clip-box-block|overflow-clip-box-inline|moz-appearance|moz-user-select|ms-user-select|' &&
    'webkit-animation|webkit-animation-delay|webkit-animation-direction|webkit-animation-duration|' &&
    'webkit-animation-fill-mode|webkit-animation-iteration-count|webkit-animation-name|' &&
    'webkit-animation-play-state|webkit-animation-timing-function|webkit-user-select'.
    insert_keywords( list  = keyword_list
                     token = c_token-extensions ).

    " 7) CSS At-Rules (including SASS/SCSS)
    keyword_list =
    '@charset|@container|@counter-style|@font-face|@font-feature-values|@font-palette-values|@import|' &&
    '@keyframes|@layer|@media|@namespace|@page|@property|@scope|@starting-style|@supports|@viewport|' &&
    '@-moz-keyframes|@-o-keyframes|@-webkit-keyframes|@use|@mixin|@include|@extend'.
    insert_keywords( list  = keyword_list
                     token = c_token-at_rules ).

    " 8) HTML Tags (including legacy elements)
    keyword_list =
    'a|abbr|acronym|address|applet|area|article|aside|audio|b|base|basefont|bdi|bdo|bgsound|big|blink|' &&
    'blockquote|body|br|button|canvas|caption|center|cite|code|col|colgroup|data|datalist|dd|del|details|' &&
    'dfn|dialog|dir|div|dl|dt|em|embed|fieldset|figcaption|figure|font|footer|form|frame|frameset|' &&
    'h1|h2|h3|h4|h5|h6|head|header|hgroup|hr|html|i|iframe|ilayer|img|input|ins|isindex|kbd|keygen|' &&
    'label|layer|legend|li|link|listing|main|map|mark|menu|meta|meter|multicol|nav|nobr|noembed|noframes|' &&
    'nolayer|noscript|object|ol|optgroup|option|output|p|param|picture|plaintext|pre|progress|q|rp|rt|' &&
    'ruby|s|samp|script|search|section|select|server|slot|small|sound|source|spacer|span|strike|strong|' &&
    'style|sub|summary|sup|table|tbody|td|template|textarea|tfoot|th|thead|time|title|tr|track|tt|u|ul|' &&
    'var|video|wbr|xmp|xml|xsl'.
    insert_keywords( list  = keyword_list
                     token = c_token-html ).

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

    result = xsdbool( line_exists( keywords[ keyword = to_lower( chunk ) ] ) ).

  ENDMETHOD.


  METHOD order_matches.

    FIELD-SYMBOLS <prev_match> TYPE ty_match.

    " Longest matches
    SORT matches BY offset length DESCENDING.

    DATA(line_len)   = strlen( line ).
    DATA(next)       = 0.
    DATA(prev_token) = ``.
    DATA(prev_end)   = 0.

    " Check if this is part of multi-line comment and mark it accordingly
    IF comment = abap_true.
      IF NOT line_exists( matches[ token = c_token-comment ] ).
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

          " Map generic keyword to specific CSS token
          match = to_lower( match ).
          READ TABLE keywords ASSIGNING FIELD-SYMBOL(<keyword>) WITH TABLE KEY keyword = match.
          IF sy-subrc = 0.
            <match>-token = <keyword>-token.
          ENDIF.

          " A keyword directly followed by ( is a function call; keep @ rules as at-rules
          next = <match>-offset + <match>-length.
          IF next < line_len AND match(1) <> '@' AND line+next(1) = '('.
            <match>-token = c_token-functions.
          ENDIF.

        WHEN c_token-comment.
          CASE match.
            WHEN '/*'.
              DELETE matches WHERE offset > <match>-offset.
              <match>-length = line_len - <match>-offset.
              comment = abap_true.
            WHEN '*/'.
              DELETE matches WHERE offset < <match>-offset.
              <match>-length = <match>-offset + 2.
              <match>-offset = 0.
              comment = abap_false.
            WHEN OTHERS.
              DATA(cmmt_end) = <match>-offset + <match>-length.
              DELETE matches WHERE offset > <match>-offset AND offset <= cmmt_end.
          ENDCASE.

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
      IF NOT is_keyword( substring( val = line
                                    off = <match>-offset
                                    len = <match>-length ) ).
        CLEAR <match>-token.
      ENDIF.
    ENDLOOP.

    DELETE result WHERE token IS INITIAL.

  ENDMETHOD.
ENDCLASS.
