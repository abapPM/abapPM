CLASS zcl_abappm_logo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* apm Logo
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CLASS-METHODS xml
      IMPORTING
        iv_svg        TYPE string
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS svg_logo
      IMPORTING
        iv_height     TYPE i DEFAULT 25
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS svg_logo_with_text
      IMPORTING
        iv_height     TYPE i DEFAULT 25
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS svg_cube
      IMPORTING
        iv_height     TYPE i DEFAULT 25
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS svg_text
      IMPORTING
        iv_height     TYPE i DEFAULT 25
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS replace_width_height
      IMPORTING
        iv_svg        TYPE string
        iv_ratio      TYPE p
        iv_height     TYPE i
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_abappm_logo IMPLEMENTATION.


  METHOD replace_width_height.

    DATA lv_width TYPE i.

    lv_width = iv_height * iv_ratio.

    result = replace(
      val  = iv_svg
      sub  = '$'
      with = |{ lv_width }| ).
    result = replace(
      val  = result
      sub  = '$'
      with = |{ iv_height }| ).

  ENDMETHOD.


  METHOD svg_cube.

    result = |<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 232 252" width="$" height="$">\n|
      && |<g>|
      && |<path id="1b" d="M 0 62 L 116 120 L 116 254 L 0 196 L 0 62 Z" stroke="#000000" |
      && |stroke-width="0" fill="#000000" stroke-miterlimit="1"/>|
      && |<path id="1w" d="M 116 120 L 116 254 L 232 196 L 232 62 L 116 120 Z" stroke="#808080" |
      && |stroke-width="0.5" fill="#FFFFFF" stroke-miterlimit="1"/>|
      && |<path id="1g" d="M 0 62 L 116 2 L 232 62 L 116 120 L 0 62 Z" stroke="#808080" |
      && |stroke-width="0" fill="#808080" stroke-miterlimit="1"/>|
      && |</g>|
      && |</svg>|.

    result = replace_width_height(
      iv_svg    = result
      iv_ratio  = '0.92'
      iv_height = iv_height ).

  ENDMETHOD.


  METHOD svg_logo.

    result = |<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 512 512" width="$" height="$">\n|
      && |<g>\n|
      && |<path id="1b" d="M 116 62 L 232 120 L 232 254 L 116 196 L 116 62 Z" stroke="#000000" |
      && |stroke-width="0" fill="#000000" stroke-miterlimit="1" />\n|
      && |<path id="1w" d="M 232 120 L 232 254 L 350 196 L 350 62 L 232 120 Z" stroke="#808080" |
      && |stroke-width="0.5" fill="#FFFFFF" stroke-miterlimit="1" />\n|
      && |<path id="1g" d="M 116 62 L 232 2 L 350 62 L 232 120 L 116 62 Z" stroke="#808080" |
      && |stroke-width="0" fill="#808080" stroke-miterlimit="1" />\n|
      && |<path id="2b" d="M 2 254 L 116 312 L 116 446 L 2 388 L 2 254 Z" stroke="#000000" |
      && |stroke-width="0" fill="#000000" stroke-miterlimit="1" />\n|
      && |<path id="2w" d="M 116 312 L 116 446 L 232 388 L 232 254 L 116 312 Z" stroke="#808080" |
      && |stroke-width="0.5" fill="#FFFFFF" stroke-miterlimit="1" />\n|
      && |<path id="2g" d="M 2 254 L 116 196 L 232 254 L 116 312 L 2 254 Z" stroke="#808080" |
      && |stroke-width="0" fill="#808080" stroke-miterlimit="1" />\n|
      && |<path id="3b" d="M 232 254 L 350 312 L 350 446 L 232 388 L 232 254 Z" stroke="#000000" |
      && |stroke-width="0" fill="#000000" stroke-miterlimit="1" />\n|
      && |<path id="3w" d="M 350 312 L 350 446 L 468 388 L 468 254 L 350 312 Z" stroke="#808080" |
      && |stroke-width="0.5" fill="#FFFFFF" stroke-miterlimit="1" />\n|
      && |<path id="3g" d="M 232 254 L 350 196 L 468 254 L 350 312 L 232 254 Z" stroke="#808080" |
      && |stroke-width="0" fill="#808080" stroke-miterlimit="1" />\n|
      && |</g>\n|
      && |</svg>|.

    result = replace_width_height(
      iv_svg    = result
      iv_ratio  = '1.0'
      iv_height = iv_height ).

  ENDMETHOD.


  METHOD svg_logo_with_text.
    " TODO: Replace with styles
    result = svg_logo( iv_height ) && |<span>&nbsp;</span>| && svg_text( iv_height - 5 ).
  ENDMETHOD.


  METHOD svg_text.

    result = |<svg version="1.0" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 804 331" |
      && |width="$" height="$">\n|
      && |<g transform="translate(0,331) scale(0.1,-0.1)" fill=" " stroke="none">\n|
      && |<path d="M1090 3083 c-121 -19 -259 -65 -360 -120 -387 -211 -593 -624 -551 |
      && |-1103 46 -522 377 -884 881 -965 120 -19 204 -19 313 0 186 32 372 123 542 |
      && |264 27 22 54 40 58 38 5 -1 53 -63 106 -137 l97 -135 67 -3 67 -3 0 1046 0 |
      && |1045 -64 0 -64 0 -98 -117 c-53 -65 -100 -121 -104 -125 -3 -5 -33 15 -65 43 |
      && |-161 136 -365 238 -539 268 -57 10 -232 13 -286 4z m306 -558 c74 -25 182 |
      && |-103 237 -171 161 -198 180 -450 52 -670 -43 -74 -150 -174 -225 -211 -83 -41 |
      && |-136 -52 -236 -53 -91 0 -163 18 -245 62 -190 100 -294 341 -259 598 33 239 |
      && |168 402 378 455 74 19 230 14 298 -10z"/>\n|
      && |<path d="M3685 3079 c-190 -25 -392 -121 -575 -273 -53 -44 -56 -45 -70 -27 |
      && |-8 10 -55 66 -104 125 l-89 106 -68 0 -69 0 2 -1442 3 -1443 268 -3 267 -2 0 |
      && |469 0 470 118 -60 c64 -33 153 -72 197 -86 74 -26 92 -27 250 -28 155 0 179 2 |
      && |268 27 181 51 332 136 453 256 209 206 315 478 315 807 0 214 -34 375 -113 |
      && |530 -65 128 -106 184 -207 285 -104 103 -200 166 -334 220 -146 58 -366 88 |
      && |-512 69z m238 -545 c257 -66 409 -292 394 -584 -16 -311 -235 -530 -529 -530 |
      && |-247 0 -449 159 -513 405 -20 76 -20 243 0 320 64 250 256 402 509 404 43 1 |
      && |105 -6 139 -15z"/>\n|
      && |<path d="M5840 3043 c-122 -20 -291 -93 -379 -162 l-37 -30 -69 79 -70 79 -67 |
      && |0 -68 1 0 -1045 0 -1045 265 0 264 0 3 693 3 692 23 50 c47 102 129 155 243 |
      && |155 109 0 195 -55 240 -155 17 -37 19 -84 19 -737 l0 -698 270 0 270 0 0 693 |
      && |0 692 24 50 c47 103 127 154 236 155 82 0 152 -31 203 -90 69 -81 67 -57 67 |
      && |-817 l0 -683 265 0 266 0 -3 738 -3 737 -32 95 c-111 327 -389 541 -723 557 |
      && |-202 10 -380 -51 -506 -172 l-67 -66 -50 51 c-92 94 -188 145 -329 174 -71 15 |
      && |-198 19 -258 9z"/>\n|
      && |</g>\n|
      && |</svg>|.

    result = replace_width_height(
      iv_svg    = result
      iv_ratio  = '2.43'
      iv_height = iv_height ).

  ENDMETHOD.


  METHOD xml.
    result = |<?xml version="1.0" encoding="utf-8"?>\n{ iv_svg }\n|.
  ENDMETHOD.
ENDCLASS.
