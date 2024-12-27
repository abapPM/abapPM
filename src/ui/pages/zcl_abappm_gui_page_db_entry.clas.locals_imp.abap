CLASS lcl_json_editor DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS get_javascript
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.

CLASS lcl_json_editor IMPLEMENTATION.

  METHOD get_javascript.

    result = |class JSON_Editor extends HTMLElement \{\n|
      && |    constructor() \{\n|
      && |        super()\n|
      && |        const template = document.createElement('template')\n|
      && |        template.innerHTML = `\n|
      && |            <style>\n|
      && |                :host(json-editor) \{\n|
      && |                    display: inline-flex;\n|
      && |                    width: 300px;\n|
      && |                    height: 150px;\n|
      && |                    background: #252530;\n|
      && |                    color: #fff;\n|
      && |                    font-family: monospace;\n|
      && |                    padding: 4px;\n|
      && |                \}\n|
      && |\n|
      && |                div \{\n|
      && |                    outline: 0;\n|
      && |                    flex-grow: 1;\n|
      && |                    overflow: auto;\n|
      && |                \}\n|
      && |\n|
      && |               *[part=number]        \{ color: #a9dc76 \}\n|
      && |               *[part=braces]        \{ color: #84aecc \}\n|
      && |               *[part=brackets]      \{ color: #d26a6a \}\n|
      && |               *[part=colon]         \{ color: #ffffff \}\n|
      && |               *[part=comma]         \{ color: #ffff25 \}\n|
      && |               *[part=string]        \{ color: #78dce8 \}\n|
      && |               *[part=string_quotes] \{ color: #E393FF \}\n|
      && |               *[part=key]           \{ color: #ff6188 \}\n|
      && |               *[part=key_quotes]    \{ color: #fc9867 \}\n|
      && |               *[part=null]          \{ color: #cccccc \}\n|
      && |               *[part=true]          \{ color: #c2e69f \}\n|
      && |               *[part=false]         \{ color: #e69fc2 \}\n|
      && |            </style>\n|
      && |            <div id="editor" contentEditable="true" tabIndex="0"></div>\n|
      && |        `\n|
      && |\n|
      && |        this.last_string_content = ''\n|
      && |        this.attachShadow(\{ mode: 'open' \})\n|
      && |        this.shadowRoot.appendChild( template.content.cloneNode(true) )\n|
      && |        this.editor = this.shadowRoot.getElementById('editor')\n|
      && |        this.addEventListener('keyup', _ => this.format() )\n|
      && |    \}\n|
      && |\n|
      && |    connectedCallback() \{\n|
      && |        this.indent = Number(this.getAttribute('indent')) \|\| 3\n|
      && |        this.value = this.getAttribute('value')\n|
      && |    \}\n|
      && |\n|
      && |    //===[ Caret Control ]=================================================\n|
      && |\n|
      && |    get_selection() \{\n|
      && |        if( this.shadowRoot.getSelection )\n|
      && |            return this.shadowRoot.getSelection()\n|
      && |        return document.getSelection()\n|
      && |    \}\n|
      && |\n|
      && |    // return a "pointer" with relevant information about the caret position\n|
      && |    get_caret_pointer() \{\n|
      && |        const selection = this.get_selection()\n|
      && |        if (selection.rangeCount > 0) \{\n|
      && |            const range = selection.getRangeAt(0)\n|
      && |            const caret_range = range.cloneRange()\n|
      && |            caret_range.selectNodeContents(this.editor)\n|
      && |            caret_range.setEnd(range.endContainer, range.endOffset)\n|
      && |            const section = caret_range.toString()\n|
      && |            const character = section[section.length-1]\n|
      && |            const occurrence = this.get_number_of_occurrences(section, character)\n|
      && |            return \{ character, occurrence, section \}\n|
      && |        \}\n|
      && |        return null\n|
      && |    \}\n|
      && |\n|
      && |    // set the caret position based on pointer information\n|
      && |    set_caret_from_pointer(pointer) \{\n|
      && |        const selection = window.getSelection()\n|
      && |        const range = document.createRange()\n|
      && |        let nodes_to_explore = this.get_text_nodes(this.editor)\n|
      && |        let occurrence = pointer.occurrence\n|
      && |        let fount_at = 0\n|
      && |        let i=0\n|
      && |\n|
      && |        for(i=0; i<nodes_to_explore.length; i++) \{\n|
      && |            const node = nodes_to_explore[i]\n|
      && |            fount_at = this.get_position_of_occurrence(node.textContent, pointer.character, occurrence)\n|
      && |            if(fount_at >= 0 )\n|
      && |                break\n|
      && |            occurrence -= this.get_number_of_occurrences(node.textContent, pointer.character)\n|
      && |        \}\n|
      && |\n|
      && |        fount_at++\n|
      && |        range.setStart(nodes_to_explore[i], fount_at)\n|
      && |        range.setEnd(nodes_to_explore[i], fount_at)\n|
      && |        selection.removeAllRanges()\n|
      && |        selection.addRange(range)\n|
      && |    \}\n|
      && |\n|
      && |    //===[ Utils ]=========================================================\n|
      && |\n|
      && |    // escape string special characters used in regular expressions\n|
      && |    escape_regex_string(string) \{\n|
      " /[.*+?^${}()|[\]\\]/g, '\\$&'
      && |        return string.replace(/[.*+?^$\{\}()\|[\\]\\\\]/g, '\\\\$&')\n|
      && |    \}\n|
      && |\n|
      && |    // return the position of the occurrence-th sub_string occurrence\n|
      && |    get_position_of_occurrence(string, sub_string, occurrence) \{\n|
      && |        const position = string.split(sub_string, occurrence).join(sub_string).length\n|
      && |        return position === string.length ? -1 : position\n|
      && |    \}\n|
      && |\n|
      && |    // return the number of sub_string occurrences\n|
      && |    get_number_of_occurrences(string, sub_string) \{\n|
      && |        return sub_string ? string.replace(new RegExp(`[^$\{this.escape_regex_string|
      && |(sub_string)\}]`, 'g'), '').length : 0\n|
      && |    \}\n|
      && |\n|
      && |    // return the element's children text nodes\n|
      && |    get_text_nodes(element) \{\n|
      && |        let node, list=[], walk=document.createTreeWalker(element, NodeFilter.SHOW_TEXT, null, false)\n|
      && |        while(node=walk.nextNode())\n|
      && |            list.push(node)\n|
      && |        return list\n|
      && |    \}\n|
      && |\n|
      && |    //===[ Formatting ]====================================================\n|
      && |\n|
      && |    escape_html(input) \{\n|
      && |        const replace = [ ['&', '&amp;'], ['<', '&lt;'], ['>', '&gt;'], ['"', '&quot;'], ["'", '&#039;'] ]\n|
      && |        return replace.reduce( ( escaped, replacement) => escaped.replaceAll( ...replacement ), input)\n|
      && |    \}\n|
      && |   \n|
      && |    // format a json object\n|
      && |    format_object(input, offset=0) \{\n|
      && |        // in JS typeof null returns "object" (legacy bug), for null input we just return null\n|
      && |        if( input === null )\n|
      && |            return '<span part="null">null</span>'\n|
      && |        let output = ''\n|
      && |        output += `<span part="braces">\{</span><br>\n`\n|
      && |        output += Object.keys(input).map((key, index, list) => \{\n|
      && |            return `$\{'&nbsp;'.repeat(offset+this.indent)\}<span part="key" part="key">|
      && |<span part="key_quotes">\\"</span>|
      && |$\{this.escape_html(key)\}<span part="key_quotes">\\"</span></span><span part="colon">:</span>|
      && |<span part="value">$\{this.format_input(input[key], offset+this.indent)\}</span>|
      && |$\{index < list.length-1 ? '<span part="comma">,</span>' : ''\}<br>\n`\n|
      && |        \}).join('')\n|
      && |        output += '&nbsp;'.repeat(offset)\n|
      && |        output += `<span part="braces">\}</span>`\n|
      && |        return output\n|
      && |    \}\n|
      && |\n|
      && |    // format a json array\n|
      && |    format_array(input, offset=0) \{\n|
      && |        let output = ''\n|
      && |        output += `<span part="brackets">[</span><br>\n`\n|
      && |        output += input.map((value, index, list) => \{\n|
      && |            return `$\{'&nbsp;'.repeat(offset+this.indent)\}<span>$\{this.format_input(value, |
      && |offset+this.indent)\}</span>|
      && |$\{index < list.length-1 ? '<span part="comma">,</span>' : ''\}<br>\n`\n|
      && |        \}).join('')\n|
      && |        output += '&nbsp;'.repeat(offset)\n|
      && |        output += `<span part="brackets">]</span>`\n|
      && |        return output\n|
      && |    \}\n|
      && |\n|
      && |    // format a json string\n|
      && |    format_string(input) \{\n|
      && |        return `<span part="string"><span part="string_quotes">\\"</span>$\{this.escape_html(input)\}|
      && |<span part="string_quotes">\\"</span></span>`;\n|
      && |    \}\n|
      && |\n|
      && |    // format a boolean\n|
      && |    format_boolean(input) \{\n|
      && |        return `<span part="$\{input\}">$\{input\}</span>`;\n|
      && |    \}\n|
      && |\n|
      && |    // format a number\n|
      && |    format_number(input) \{\n|
      && |        return `<span part="number">$\{input\}</span>`;\n|
      && |    \}\n|
      && |\n|
      && |    // format a json input\n|
      && |    format_input(input, offset=0) \{\n|
      && |        const type = Array.isArray(input) ? 'array' : typeof input\n|
      && |        switch (type) \{\n|
      && |            case 'object':\n|
      && |                return this.format_object(input, offset)\n|
      && |            case 'array':\n|
      && |                return this.format_array(input, offset)\n|
      && |            case 'string':\n|
      && |                return this.format_string(input)\n|
      && |            case 'boolean':\n|
      && |                return this.format_boolean(input)\n|
      && |            case 'number':\n|
      && |                return this.format_number(input)\n|
      && |            default:\n|
      && |                return input\n|
      && |        \}\n|
      && |    \}\n|
      && |\n|
      && |    format() \{\n|
      && |        const editor = this.editor\n|
      && |        const pointer = this.get_caret_pointer()\n|
      && |        let content = ''\n|
      && |        try \{\n|
      && |            content = JSON.parse(this.raw_string)\n|
      && |        \}\n|
      && |        catch(exception) \{\n|
      && |            return\n|
      && |        \}\n|
      && |\n|
      && |        // prevent unnecesary render\n|
      && |        const current_string_content = JSON.stringify(content)\n|
      && |        if(!content \|\| current_string_content == this.last_string_content)\n|
      && |            return\n|
      && |\n|
      && |        editor.innerHTML = this.format_input(content)\n|
      && |        this.last_string_content = current_string_content\n|
      && |        if(pointer && focus)\n|
      && |            this.set_caret_from_pointer(pointer)\n|
      && |    \}\n|
      && |\n|
      && |    //===[ Getters / Setters ]=============================================\n|
      && |\n|
      && |    get raw_string() \{\n|
      && |        // remove %A0 (NBSP) characters, which are no valid in JSON\n|
      && |        return this.editor.innerText?.replaceAll('\\xa0', '') \|\| ''\n|
      && |    \}\n|
      && |\n|
      && |    set raw_string( input ) \{\n|
      && |        this.string_value = input\n|
      && |    \}\n|
      && |\n|
      && |    get string_value() \{\n|
      && |        return this.last_string_content\n|
      && |    \}\n|
      && |\n|
      && |    set string_value( input ) \{\n|
      && |        this.editor.innerText = input\n|
      && |        this.format()\n|
      && |    \}\n|
      && |\n|
      && |    get value() \{\n|
      && |        return this.string_value\n|
      && |    \}\n|
      && |\n|
      && |    set value( input ) \{\n|
      && |        return this.string_value = input\n|
      && |    \}\n|
      && |\n|
      && |    get json_value() \{\n|
      && |        return JSON.parse( this.string_value )\n|
      && |    \}\n|
      && |\n|
      && |    set json_value( input ) \{\n|
      && |        this.string_value = JSON.stringify( input )\n|
      && |    \}\n|
      && |\n|
      && |    is_valid() \{\n|
      && |        try \{\n|
      && |            JSON.parse( this.raw_string )\n|
      && |            return true\n|
      && |        \}\n|
      && |        catch(e) \{\n|
      && |            return false\n|
      && |        \}\n|
      && |    \}\n|
      && |\}\n|
      && |\n|
      && |customElements.define('json-editor', JSON_Editor)\n|.

  ENDMETHOD.

ENDCLASS.
