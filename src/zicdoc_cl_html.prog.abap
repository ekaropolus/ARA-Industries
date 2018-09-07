*&---------------------------------------------------------------------*
*&  Include           ZICDOC_CL_HTML
*&---------------------------------------------------------------------*
* Contains helper methods for creating HTML file.

CLASS zcl_ic_html DEFINITION.
  PUBLIC SECTION.

*   Table with content of HTML document (source text of document).
    DATA: gt_content TYPE STANDARD TABLE OF string.

    METHODS: constructor.
    METHODS: reset.

*   Returns string afetr conversion to HTML internal format
*   (e.c. '<' is replaced by '&lt;'
    METHODS: str2html IMPORTING i_str TYPE string
                      RETURNING value(e_str) TYPE string.
*   Add string with or  without conversion to internal format
    METHODS: add_string IMPORTING str TYPE string,
             add_string_raw IMPORTING str TYPE string.

*   Moves content of internal line buffer to document (like new line).
    METHODS: flush_buffer.

    METHODS: add_tag_start IMPORTING tag TYPE string,
             add_tag_end IMPORTING tag TYPE string,
             add_tag IMPORTING tag TYPE string str TYPE string.
    METHODS: add_header1 IMPORTING str TYPE string,
             add_header2 IMPORTING str TYPE string,
             add_header3 IMPORTING str TYPE string,
             add_header4 IMPORTING str TYPE string,
             add_header5 IMPORTING str TYPE string,
             add_header6 IMPORTING str TYPE string,
             add_paragraph IMPORTING str TYPE string,
             add_line_break,
             add_blank_line.

*   Returns string (str...) or appends (add...) tag elements
*   for links inside document. a_name is an anchor on a page,
*   a_href is a refernce to an anchor. i_label must be unique
*   identifier of an anchor. i_text is a text displayed as a reference.
    METHODS: str_a_href IMPORTING i_label TYPE string
                                  i_text TYPE string
                        RETURNING value(e_str) TYPE string,
             str_a_name IMPORTING i_label TYPE string
                                  i_text TYPE string
                        RETURNING value(e_str) TYPE string,
             add_a_href IMPORTING i_label TYPE string
                                  i_text TYPE string,
             add_a_name IMPORTING i_label TYPE string
                                  i_text TYPE string.
*   Starts new document - place several tags
    METHODS: start_document IMPORTING i_title TYPE string
                                      i_code_page TYPE string
                                      i_css_file_name TYPE string
                                      i_comment TYPE string,
             end_document.

*   Writes HTML document to disk
    METHODS: write_file IMPORTING file_name TYPE string
                        RETURNING value(ret_code) TYPE i.

  PRIVATE SECTION.

    DATA: lv_buffer TYPE string.

ENDCLASS.                    "zcl_ic_html DEFINITION


*----------------------------------------------------------------------*
*       CLASS zcl_ic_html IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS zcl_ic_html IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD reset.
  ENDMETHOD.                    "constructor

  METHOD reset.
    REFRESH: gt_content.
    CLEAR: lv_buffer.
  ENDMETHOD.                    "reset

  METHOD add_tag_start.
    CONCATENATE lv_buffer '<' tag '>' INTO lv_buffer.
  ENDMETHOD.                    "add_tag_start

  METHOD add_tag_end.
    CONCATENATE lv_buffer '</' tag '>' INTO lv_buffer.
  ENDMETHOD.                    "add_tag_end

  METHOD add_tag.
    CALL METHOD flush_buffer.
    CALL METHOD add_tag_start( tag ).
    CALL METHOD add_string( str ).
    CALL METHOD add_tag_end( tag ).
    CALL METHOD flush_buffer.
  ENDMETHOD.                    "add_tag

  METHOD str2html.
    DATA: str TYPE string,
          len TYPE i, cur TYPE i, cnt TYPE i,
          off1 TYPE i,
          lv_char TYPE c, lv_spec(20) TYPE c.

    off1 = 0.
    len = STRLEN( i_str ).
    DO len TIMES.
      cur = sy-index - 1.
      lv_char = i_str+cur(1).
      CASE lv_char.
        WHEN '<'.
          lv_spec = '&lt;'.
        WHEN '>'.
          lv_spec = '&gt;'.
        WHEN '&'.
          lv_spec = '&amp;'.
        WHEN '|'.
          lv_spec = '&brvbar;'.
        WHEN '~'.
          lv_spec = '&tilde;'.
        WHEN OTHERS.
          IF sy-index = len.   " last char
            lv_spec = lv_char.
          ELSE.
            lv_spec = space.
          ENDIF.
      ENDCASE.
      IF lv_spec <> space.
        cnt = cur - off1.
        CONCATENATE str i_str+off1(cnt) lv_spec INTO str.
        off1 = sy-index.
      ENDIF.
    ENDDO.
    e_str = str.
  ENDMETHOD.                                                "str2html


  METHOD add_string.
    DATA: txt TYPE string.
    txt = str2html( str ).
    CONCATENATE lv_buffer txt INTO lv_buffer.
  ENDMETHOD.                    "add_string


  METHOD add_string_raw.
    CONCATENATE lv_buffer str INTO lv_buffer.
  ENDMETHOD.                    "add_string_raw


  METHOD flush_buffer.
    IF NOT lv_buffer IS INITIAL.
      APPEND lv_buffer TO gt_content.
      CLEAR: lv_buffer.
    ENDIF.
  ENDMETHOD.                    "flush_buffer


  METHOD add_header1.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h1'
        str = str.
  ENDMETHOD.                    "append_header1


  METHOD add_header2.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h2'
        str = str.
  ENDMETHOD.                    "append_header2


  METHOD add_header3.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h3'
        str = str.
  ENDMETHOD.                    "append_header3


  METHOD add_header4.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h4'
        str = str.
  ENDMETHOD.                    "add_header4


  METHOD add_header5.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h5'
        str = str.
  ENDMETHOD.                    "add_header5


  METHOD add_header6.
    CALL METHOD add_tag
      EXPORTING
        tag = 'h6'
        str = str.
  ENDMETHOD.                    "add_header6


  METHOD add_paragraph.
    CALL METHOD add_tag
      EXPORTING
        tag = 'p'
        str = str.
  ENDMETHOD.                    "append_paragraph


  METHOD add_line_break.
    CALL METHOD add_string_raw( '<br>' ).
  ENDMETHOD.                    "append_line_break


  METHOD add_blank_line.
    CALL METHOD add_string_raw( '<p><br></p>' ).
  ENDMETHOD.


  METHOD str_a_href.
    e_str = str2html( i_text ).
    CONCATENATE '<a href="#' i_label '">' e_str '</a>' INTO e_str.
  ENDMETHOD.                    "str_a_href


  METHOD str_a_name.
    e_str = str2html( i_text ).
    CONCATENATE '<a name="' i_label '">' e_str '</a>' INTO e_str.
  ENDMETHOD.                    "str_a_name


  METHOD add_a_href.
    DATA: str TYPE string.
    str = str2html( i_text ).
    CONCATENATE '<a href="#' i_label '">' str '</a>' INTO str.
    CALL METHOD add_string_raw( str ).
  ENDMETHOD.                    "add_a_href


  METHOD add_a_name.
    DATA: str TYPE string.
    str = str2html( i_text ).
    CONCATENATE '<a name="' i_label '">' str '</a>' INTO str.
    CALL METHOD add_string_raw( str ).
  ENDMETHOD.                    "add_a_name


  METHOD start_document.

    DATA: l_str TYPE string.

    CALL METHOD add_string_raw( '<html>' ).
    CALL METHOD flush_buffer.
    CALL METHOD add_string_raw( '<head>' ).
    CALL METHOD flush_buffer.

    CONCATENATE '<META CONTENT="text/html; charset=' i_code_page '">'
                INTO l_str.
    CALL METHOD add_string_raw( l_str ).
    CALL METHOD flush_buffer.

    IF i_css_file_name IS NOT INITIAL.
      CONCATENATE '<link rel="Stylesheet" type="text/css" href="'
                  i_css_file_name '" />'  INTO l_str.
      CALL METHOD add_string_raw( l_str ).
      CALL METHOD flush_buffer.
    ENDIF.

    IF i_comment IS NOT INITIAL.
      CALL METHOD add_string_raw( i_comment ).
      CALL METHOD flush_buffer.
    ENDIF.

    CALL METHOD add_tag
      EXPORTING
        tag = 'title'
        str = i_title.
    CALL METHOD add_string_raw( '</head>' ).
    CALL METHOD flush_buffer.
    CALL METHOD add_string_raw( '<body>' ).
    CALL METHOD flush_buffer.

  ENDMETHOD.                    "start_document


  METHOD end_document.
    CALL METHOD add_string_raw( '</body>' ).
    CALL METHOD flush_buffer.
    CALL METHOD add_string_raw( '</html>' ).
    CALL METHOD flush_buffer.
  ENDMETHOD.                    "end_document


  METHOD write_file.

    CALL METHOD flush_buffer.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = file_name
      TABLES
        data_tab                = gt_content
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    ret_code = sy-subrc.

  ENDMETHOD.                    "write_file

ENDCLASS.                    "zcl_ic_html IMPLEMENTATION
