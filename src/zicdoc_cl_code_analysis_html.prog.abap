*&---------------------------------------------------------------------*
*&  Include           ZICDOC_CL_CODE_ANALYSIS_HTML
*&---------------------------------------------------------------------*
* @Author Ireneusz Cwir
* @Date   June 2006
* @Info   Include contains definition and implementation of class
* ZCL_IC_CODE_ANALYSIS_HTML. This class handles analysis of ABAP
* program and creation of HTML file with analysis results.
* The class uses class ZCL_IC_CODE_ANALYSIS to scan source code and
* store results in public tables. This class reads these tables and
* writes HTML file.
* HTML document creation is handled by class ZCL_IC_HTML.


INCLUDE zicdoc_cl_code_analysis.
INCLUDE zicdoc_cl_html.


* Texts and templates used for creation of HTML file
* They must be in HTML source format (ec. '&lt;' for displaying '<')
* Variables in templates should have format: @n@,
* where n = number of paparmeter
*
* Section 0 - general data (document header)
* Section 1 - programs and includes hierarchy
* Section 2 - database tables used in programs
* Section 3 - programs and includes summary
* Section 4 - class components
* Section 5 - non-class code units
* Section 6 - code units calls hierarchy

* Code page placed in document header
CONSTANTS: c_html_code_page TYPE string VALUE 'windows-1250'.

* Cascade style sheet (CSS) file name (with path)
CONSTANTS: c_css_file_name TYPE string VALUE 'ZIcDocStyle.css'.

* A kind of info about author of the tool
CONSTANTS: c_zicdoc_author TYPE string VALUE
                    '<!-- Genereted by ZICDOC, by Ireneusz Cwir -->'.

* HTML title (displayed in explorer window's title)
CONSTANTS: c_html_title TYPE string VALUE
                  '@1@ - Source code documentation'.


* Sections' headers
CONSTANTS: c_html_header_0 TYPE string VALUE
                  '<h1>@1@ - Source code documentation</h1><br>',
           c_html_header_1 TYPE string VALUE
                  '<br><h2>Programs and includes</h2><br>',
           c_html_header_2 TYPE string VALUE
                  '<br><h2>Database tables</h2><br>',
           c_html_header_3 TYPE string VALUE
                  '<br><h2>Programs and includes summary</h2><br>',
           c_html_header_4 TYPE string VALUE
                  '<br><h2>Class components</h2><br>',
           c_html_header_5 TYPE string VALUE
                  '<br><h2>Non-class code units</h2><br>',
           c_html_header_6 TYPE string VALUE
                  '<br><h2>Code units calls hierarchy</h2><br>'.


* General templates
CONSTANTS:
* Template for writting all elements of comment. variables are for:
* 1. Tag, 2. Parameter, 3. Text of comment
  c_html_comment_complex      TYPE string VALUE
             '<dl><dt>@1@</dt><dd><var>@2@ </var>@3@</dd></dl>',
* Code unit details - header. 1. Unit name
  c_html_unit_det_header      TYPE string VALUE
             '<hr><h5>@1@</h5>',
* Code unit details - Declaration. 1. Unit name, 2. Type, 3. Params
  c_html_unit_det_declaration TYPE string VALUE
                                '<dl><dt><code>@2@ @1@</code></dt><dd><code>@3@</code></dd></dl>'.


* For section 1 - hierarchy of includes
* Prefix representing next level of nesting of include (like . )
CONSTANTS: c_html_s1_nesting_level TYPE string VALUE '.&nbsp;',

* Style for placing program name
           c_html_s1_program_name  TYPE string VALUE
                    '<dfn>@1@</dfn><br>'.


* For section 2 - database tables
* Short info
CONSTANTS: c_html_s2_sub_header    TYPE string VALUE
              '<h4>Database tables access:</h4>',
* Line of HTML table containing info about database table
* 1. Table name, 2. Access mode, 3. Table description from DDIC
           c_html_s2_db_table_info TYPE string VALUE
                                     '<tr><td class="dbname"><dfn>@1@</dfn></td>' &
           '<td class="dbaccess">@2@</td>' &
           '<td>@3@</td><td>@4@</td></tr>'.


* For section 3 - includes summary
CONSTANTS:
* Program name (as a section header)
  c_html_s3_program   TYPE string VALUE
        '<h3>@1@</h3>',
* Program title (description from dictionary)
  c_html_s3_title     TYPE string VALUE
                        '<table><tr><td><samp>Title:</samp></td><td>@1@</td></tr></table>',
* Header for code units list
  c_html_s3_units     TYPE string VALUE
           '<h4>Forms, events, functions:</h4>',
* Table line for code unit info: 1. Type, 2. Name
  c_html_s3_unit_info TYPE string VALUE
                        '<tr><td><samp>@1@</samp></td><td><dfn>@2@</dfn></td></tr>'.


* For section 4 - class components
CONSTANTS:
* Class name (as subsection header). 1. Clas/interf, 2. Name
  c_html_s4_class_name        TYPE string VALUE
              '<h3>@1@ @2@</h3>',
* Header for components list
  c_html_s4_components        TYPE string VALUE
              '<h4>Components:</h4>',
* Table line for general class info - program, base class and declar.
  c_html_s4_class_program     TYPE string VALUE
                                '<tr><td><samp>Program:</samp></td><td><dfn>@1@</dfn></td></tr>',
  c_html_s4_class_base        TYPE string VALUE
                                '<tr><td><samp>Base class:</samp></td><td><dfn>@1@</dfn></td></tr>',
  c_html_s4_class_declaration TYPE string VALUE
                                '<tr><td><samp>Declaration:</samp></td><td><dfn>@1@</dfn></td></tr>',
* Table line for component summary info: 1. Type, 2. Name
  c_html_s4_comp_sum_info     TYPE string VALUE
      '<tr><td><samp>@1@</samp></td><td><dfn>@2@</dfn></td></tr>'.


* For section 5 - non-class code units
CONSTANTS:
* Program name (as subsection header). 1. Name
           c_html_s5_program TYPE string VALUE
                '<h3>@1@</h3>'.


* For section 6 - code units calls hierarchy

* Prefix representing next level of nesting of include (like . )
CONSTANTS: c_html_s6_nesting_level TYPE string VALUE '.&nbsp;',

* Style for placing: 1. Unit name, 2. Unit type
           c_html_s6_unit          TYPE string VALUE
                            '<dfn>@1@</dfn> <samp>@2@</samp><br>'.


*----------------------------------------------------------------------
* Program uses HTML keywords "A NAME" and "A HREF" (anchors) to build
* links to section describing refrenced objects. Anchors must have
* unique labels (IDs) within document. Program creates them
* in the following way (space is a separator in case of concatenation):
* - Programs (includes) - <program_name> (i.e. 'Z_INC_TEST')
* - Classes - 'Class definition' + <class_name>
*             (i.e. 'Class definition ZCL_TEST')
* - Interfaces - 'Interface' + <interface_name>
* - Class/Interf. component - <comp_type> + <class_name> + <comp_name>
*             (i.e. 'Data ZCL_TEST GV_CNT')
* - Other code units - <unit_type> + <name> (i.e. 'Form DO_TEST')


*----------------------------------------------------------------------*
*       CLASS zcl_ic_code_analysis_html DEFINITION
*----------------------------------------------------------------------*


CLASS zcl_ic_code_analysis_html DEFINITION
                                INHERITING FROM zcl_ic_code_analysis.

  PUBLIC SECTION.

*   References to class handling writing HTML file
    DATA: gr_html TYPE REF TO zcl_ic_html.   " Ref to HTML object

    METHODS: constructor.
    METHODS: reset REDEFINITION.

*   Writes results of analysis into HTML file. Must ne called after
*   one of methods ANALYSE_xxx of base class.
*   Parameters decide, which sections of analysis are written
*   to the file.
    METHODS: write_html_file IMPORTING
                                       i_filename                    TYPE string
                                       i_write_includes_hier         TYPE c
                                       i_write_includes_details      TYPE c
                                       i_write_class_components      TYPE c
                                       i_write_class_components_prot TYPE c
                                       i_write_class_components_priv TYPE c
                                       i_write_code_units_hier       TYPE c
                                       i_write_code_units_details    TYPE c
                                       i_write_tables                TYPE c
                                       i_html_comments               TYPE c
                             RETURNING VALUE(ret_code)               TYPE i.

  PRIVATE SECTION.

*   Parameters from WRITE_HTML_FILE
    DATA: lv_write_includes_hier         TYPE c,
          lv_write_includes_details      TYPE c,
          lv_write_code_units_hier       TYPE c,
          lv_write_code_units_details    TYPE c,
          lv_write_tables                TYPE c,
          lv_write_class_components      TYPE c,
          lv_write_class_components_prot TYPE c,
          lv_write_class_components_priv TYPE c,
          lv_html_comments               TYPE c.

    METHODS: write_general_info,
      write_includes_hier,
      write_database_tables,
      write_includes_details,
      write_class_components,
      write_non_class_code_units,
      write_code_units_hier.

    METHODS: do_write_comments IMPORTING is_comment TYPE ts_comment,
      do_write_db_tables IMPORTING it_tables TYPE tt_dbtables,
      do_write_db_tables_for_unit
        IMPORTING i_clsname TYPE seoclsname
                  i_name    TYPE t_unitname
                  i_type    TYPE t_unittype,
      do_write_comp_summary_single
        IMPORTING is_cmp TYPE ts_classcmp,
      do_write_comp_summary IMPORTING i_exp   TYPE seoexpose
                                      it_cmps TYPE tt_classcmps,
      do_write_unit_details
        IMPORTING i_name     TYPE string
                  i_exp      TYPE string
                  i_type     TYPE string
                  i_label    TYPE string
                  is_param   TYPE ts_parameters
                  is_comment TYPE ts_comment,
      do_write_details_class_comp
        IMPORTING is_cmp TYPE ts_classcmp,
      do_write_details_nonclass_unit
        IMPORTING is_unit TYPE ts_modunit.

*   Builds label for anchor for code unit. It can be used in 'a name'
*   and 'a href' HTML tags.
    METHODS: build_a_label_unit IMPORTING is_unit TYPE ts_modunit
                                EXPORTING e_label TYPE string,
      build_a_label_class_name
        IMPORTING i_clsname TYPE seoclsname
                  i_clstype TYPE seoclstype
        EXPORTING e_label   TYPE string,
      build_a_label_class_comp IMPORTING is_cmp  TYPE ts_classcmp
                               EXPORTING e_label TYPE string.

*   Adds to HTML document string after replacing varaibles @n@ (n=1..3)
*   with provided value.
    METHODS: add_str_var IMPORTING
                           i_str    TYPE string
                           i_value1 TYPE string OPTIONAL
                           i_value2 TYPE string OPTIONAL
                           i_value3 TYPE string OPTIONAL
                           i_value4 TYPE string OPTIONAL.

ENDCLASS.                    "zcl_ic_code_analysis_html DEFINITION


*----------------------------------------------------------------------*
*       CLASS zcl_ic_code_analysis_html IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS zcl_ic_code_analysis_html IMPLEMENTATION.

  METHOD constructor.
    CALL METHOD super->constructor.
    CALL METHOD reset.
  ENDMETHOD.                    "constructor


* Resets all atributes of the object.
  METHOD reset.
    CALL METHOD super->reset.
    CLEAR: gr_html.
    CLEAR: lv_write_includes_hier, lv_write_includes_details,
           lv_write_class_components, lv_write_class_components_prot,
           lv_write_class_components_priv,
           lv_write_code_units_hier, lv_write_code_units_details,
           lv_write_tables, lv_html_comments.
  ENDMETHOD.                    "reset


* Writes results of analysis into HTML file. It must be called after
* one of methods ANALYSE_xxx.

  METHOD write_html_file.

    DATA: l_str TYPE string.

*   Check if analysis has ben already performed
    ret_code = 101.
    CHECK gv_main_program_name IS NOT INITIAL.

*   Store parameters in local atributes - to be used by other methods
    lv_write_includes_hier          = i_write_includes_hier.
    lv_write_includes_details       = i_write_includes_details.
    lv_write_class_components       = i_write_class_components.
    lv_write_class_components_prot  = i_write_class_components_prot.
    lv_write_class_components_priv  = i_write_class_components_priv.
    lv_write_code_units_hier        = i_write_code_units_hier.
    lv_write_code_units_details     = i_write_code_units_details.
    lv_write_tables                 = i_write_tables.
    lv_html_comments                = i_html_comments.

*   Start preparing HTML document (prepare heder)
    CREATE OBJECT gr_html.

    l_str = c_html_title.
    REPLACE FIRST OCCURRENCE OF '@1@' IN l_str
            WITH gv_main_program_name.
    CALL METHOD gr_html->start_document
      EXPORTING
        i_title         = l_str
        i_code_page     = c_html_code_page
        i_css_file_name = c_css_file_name
        i_comment       = c_zicdoc_author.

*   Section 0 - document title
    CALL METHOD write_general_info.

*   Section 1 - programs (includes) hierarchy
    IF i_write_includes_hier IS NOT INITIAL.
      CALL METHOD write_includes_hier.
    ENDIF.

*   Section 2 - access to database tables
    IF i_write_tables IS NOT INITIAL.
      CALL METHOD write_database_tables.
    ENDIF.

*   Section 3 - content of programs (includes)
    IF i_write_includes_details IS NOT INITIAL.
      CALL METHOD write_includes_details.
    ENDIF.

*   Section 4 - class components
    IF i_write_class_components IS NOT INITIAL.
      CALL METHOD write_class_components.
    ENDIF.

*   Section 5 - non-class code units
    IF i_write_code_units_details IS NOT INITIAL.
      CALL METHOD write_non_class_code_units.
    ENDIF.

*   Section 6 - hierarchy of code units calls
    IF i_write_code_units_hier IS NOT INITIAL.
      CALL METHOD write_code_units_hier.
    ENDIF.

*   Finish HTML document
    CALL METHOD gr_html->end_document.

*   Write HTML document to disk
    ret_code = gr_html->write_file( i_filename ).

*   Reset all atributes
    CALL METHOD gr_html->reset.
    CLEAR: gr_html.

  ENDMETHOD.                    "write_html_file


* Section 0 - general info (document title).

  METHOD write_general_info.

    DATA: l_str TYPE string.

    l_str = gv_main_program_name.
    CALL METHOD add_str_var
      EXPORTING
        i_str    = c_html_header_0
        i_value1 = l_str.

  ENDMETHOD.                    "write_general_info


* Section 1 - hierarchy of programs and includes.

  METHOD write_includes_hier.

    DATA: ls_prog TYPE ts_program.
    DATA: l_out TYPE string,
          l_str TYPE string.
    DATA: l_cnt TYPE i.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_1 ).
    CALL METHOD gr_html->flush_buffer.

*   Write nested includes (hierarchy)
    LOOP AT gt_programs INTO ls_prog.

*     String representin depth of nesting (. . .)
      CLEAR: l_out.
      l_cnt = ls_prog-depth - 1.
      DO l_cnt TIMES.
        CONCATENATE l_out c_html_s1_nesting_level INTO l_out.
      ENDDO.

*     Program name. If details printed - place also HTML link
      l_str = ls_prog-program.
      IF lv_write_includes_details IS NOT INITIAL.
        CALL METHOD gr_html->str_a_href
          EXPORTING
            i_label = l_str
            i_text  = l_str
          RECEIVING
            e_str   = l_str.
      ENDIF.
      CONCATENATE l_out l_str INTO l_out.
      CALL METHOD add_str_var
        EXPORTING
          i_str    = c_html_s1_program_name
          i_value1 = l_out.
    ENDLOOP.

  ENDMETHOD.                    "write_includes_hier


* Section 2 - access to database tables.

  METHOD write_database_tables.

*    break c00002143.

    DATA: lt_tables TYPE tt_dbtables,
          ls_table  TYPE ts_dbtable,
          ls_last   TYPE ts_dbtable.

*   Table GT_DBTABLES can contains the same table in several records
*   for instance, access from different forms. Therefore, we need
*   to prepare unique record only.
    SORT gt_dbtables BY table mode.
    LOOP AT gt_dbtables INTO ls_table.

      IF ls_table-name <> ls_last-name  AND
         ls_table-mode <> ls_last-mode.
        APPEND ls_table TO lt_tables.
        ls_last = ls_table.
      ENDIF.

    ENDLOOP.

*   At least one record
    CHECK lt_tables IS NOT  INITIAL.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_2 ).
    CALL METHOD gr_html->flush_buffer.

*   Write list of tables
    CALL METHOD do_write_db_tables( lt_tables ).

  ENDMETHOD.                    "write_tables


* Writes list of database tables.

  METHOD do_write_db_tables.

    DATA: ls_table TYPE ts_dbtable.
    DATA: l_str1 TYPE string, l_str2 TYPE string, l_str3 TYPE string.
    DATA l_str4 TYPE string.

*   At least one record
    CHECK it_tables IS NOT  INITIAL.

*   Subsection info
    CALL METHOD gr_html->add_string_raw( c_html_s2_sub_header ).
    CALL METHOD gr_html->flush_buffer.

*   Write database tables in HTML table
    gr_html->add_tag_start( 'table' ).
    CALL METHOD gr_html->flush_buffer.

    LOOP AT it_tables INTO ls_table.

      l_str1 = ls_table-table.
      l_str2 = get_db_access_mode_desc( ls_table-mode ).
      l_str3 = ls_table-ddtext.
      l_str3 = gr_html->str2html( l_str3 ).
      l_str4 = ls_table-statement.

      CALL METHOD add_str_var
        EXPORTING
          i_str    = c_html_s2_db_table_info
          i_value1 = l_str1
          i_value2 = l_str2
          i_value3 = l_str3
          i_value4 = l_str4.

    ENDLOOP.

    gr_html->add_tag_end( 'table' ).
    CALL METHOD gr_html->flush_buffer.

  ENDMETHOD.                    "do_write_db_tables


* Writes tables accessed from code unit.

  METHOD do_write_db_tables_for_unit.

    DATA: lt_tables TYPE tt_dbtables,
          ls_table  TYPE ts_dbtable.

*   Prepare list of tables for code unit
    LOOP AT gt_dbtables INTO ls_table WHERE clsname = i_clsname
                                        AND name = i_name
                                        AND type = i_type.
      APPEND ls_table TO lt_tables.
    ENDLOOP.

*   At least one record
    CHECK lt_tables IS NOT  INITIAL.

*   Write list of tables
    CALL METHOD do_write_db_tables( lt_tables ).

  ENDMETHOD.                    "do_write_db_tables_for_unit


* Section 3 - content of INCLUDEs (summary).

  METHOD write_includes_details.

    DATA: ls_prog TYPE ts_program,
          ls_unit TYPE ts_modunit.
    DATA: l_str   TYPE string,
          l_str2  TYPE string,
          l_str3  TYPE string,
          l_label TYPE string.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_3 ).
    CALL METHOD gr_html->flush_buffer.

*   For all programs (includes)
    LOOP AT gt_programs INTO ls_prog.

*     Program name (as anchor)
      l_str = ls_prog-program.
      CALL METHOD gr_html->str_a_name
        EXPORTING
          i_label = l_str
          i_text  = l_str
        RECEIVING
          e_str   = l_str.

      CALL METHOD add_str_var
        EXPORTING
          i_str    = c_html_s3_program
          i_value1 = l_str.

*     Program title (in a table)
      IF ls_prog-title IS NOT INITIAL.
        l_str = ls_prog-title.
        CALL METHOD add_str_var
          EXPORTING
            i_str    = c_html_s3_title
            i_value1 = l_str.
      ENDIF.

*     Program comments
      CALL METHOD do_write_comments( ls_prog-comment ).

*     Code units list header
      CALL METHOD gr_html->add_string_raw( c_html_s3_units ).
      CALL METHOD gr_html->flush_buffer.

*     Write code units in HTML table
      gr_html->add_tag_start( 'table' ).
      CALL METHOD gr_html->flush_buffer.

*     All code units for current program
      LOOP AT gt_modunits INTO ls_unit WHERE program = ls_prog-program.

        l_str = get_code_unit_type_desc( ls_unit-type ).
        l_str2 = ls_unit-name.

*       If details for code units or class components are issued
        IF ( ls_unit-type = 'M'  OR ls_unit-type = 'I' OR
             ls_unit-type = 'CD' OR ls_unit-type = 'CI' ) AND
            lv_write_class_components IS NOT INITIAL  OR
            lv_write_code_units_details IS NOT INITIAL.

*         Creates label for anchor
          CALL METHOD build_a_label_unit
            EXPORTING
              is_unit = ls_unit
            IMPORTING
              e_label = l_str3.
          CALL METHOD gr_html->str_a_href
            EXPORTING
              i_label = l_str3
              i_text  = l_str2
            RECEIVING
              e_str   = l_str2.
        ENDIF.
        CALL METHOD add_str_var
          EXPORTING
            i_str    = c_html_s3_unit_info
            i_value1 = l_str
            i_value2 = l_str2.

      ENDLOOP.

      gr_html->add_tag_end( 'table' ).
      CALL METHOD gr_html->flush_buffer.

    ENDLOOP.

  ENDMETHOD.                    "write_includes_detail


* Section 4 - class components.

  METHOD write_class_components.

    DATA: lt_cmps TYPE tt_classcmps,
          ls_cmp  TYPE ts_classcmp.
    DATA: ls_prog  TYPE ts_program,
          ls_class TYPE ts_class,
*          ls_param  TYPE ts_param_detail,
          l_type   TYPE t_unittype,
          l_str    TYPE string, l_str2 TYPE string, l_str3 TYPE string.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_4 ).
    CALL METHOD gr_html->flush_buffer.

*   Display classes sorted by programs
    LOOP AT gt_programs INTO ls_prog.

*     Classes defined in the program
      LOOP AT gt_classes INTO ls_class
                        WHERE program = ls_prog-program.

*       Class name - section header. We also build anchor for reference
        l_type = 'CD'.
        IF ls_class-clstype = '1'.
          l_type = 'I'.
        ENDIF.
        l_str = get_code_unit_type_desc( l_type ).
        l_str2 = ls_class-clsname.
        CALL METHOD build_a_label_class_name
          EXPORTING
            i_clsname = ls_class-clsname
            i_clstype = ls_class-clstype
          IMPORTING
            e_label   = l_str3.
        CALL METHOD gr_html->str_a_name
          EXPORTING
            i_label = l_str3
            i_text  = l_str2
          RECEIVING
            e_str   = l_str2.
        CALL METHOD add_str_var
          EXPORTING
            i_str    = c_html_s4_class_name
            i_value1 = l_str
            i_value2 = l_str2.

*       Write general info in HTML table
        gr_html->add_tag_start( 'table' ).
        CALL METHOD gr_html->flush_buffer.

*       Base class
        IF ls_class-super IS NOT INITIAL.

          l_str2 = ls_class-super.
*         If we have base class in tables components - build reference
          READ TABLE gt_classes TRANSPORTING NO FIELDS
                                WITH KEY  clsname = ls_class-super
                                          clstype = '0'.
          IF sy-subrc = 0.
            CALL METHOD build_a_label_class_name
              EXPORTING
                i_clsname = ls_class-super
                i_clstype = '0'
              IMPORTING
                e_label   = l_str3.
            CALL METHOD gr_html->str_a_href
              EXPORTING
                i_label = l_str3
                i_text  = l_str2
              RECEIVING
                e_str   = l_str2.
          ENDIF.
          CALL METHOD add_str_var
            EXPORTING
              i_str    = c_html_s4_class_base
              i_value1 = l_str2.

        ENDIF.

*       Class declaration
        IF ls_class-param-text IS NOT INITIAL.
          CALL METHOD add_str_var
            EXPORTING
              i_str    = c_html_s4_class_declaration
              i_value1 = ls_class-param-text.
        ENDIF.
*        READ TABLE ls_class-param-params INTO ls_param INDEX 1.
*        IF sy-subrc = 0.
*
*          l_str = ls_param-text.
*          CALL METHOD add_str_var
*            EXPORTING
*              i_str    = c_html_s4_class_declaration
*              i_value1 = l_str.
*        ENDIF.

*       Program name
        l_str = ls_class-program.
        IF lv_write_includes_details IS NOT INITIAL.
          CALL METHOD gr_html->str_a_href
            EXPORTING
              i_label = l_str
              i_text  = l_str
            RECEIVING
              e_str   = l_str.
        ENDIF.
        CALL METHOD add_str_var
          EXPORTING
            i_str    = c_html_s4_class_program
            i_value1 = l_str.

*       Close general info
        gr_html->add_tag_end( 'table' ).
        CALL METHOD gr_html->flush_buffer.

*       Class comments
        CALL METHOD do_write_comments( ls_class-comment ).

*       Prepare local table with comp. belonging to the current class
        REFRESH: lt_cmps.
        LOOP AT gt_classcmps INTO ls_cmp
                            WHERE clsname = ls_class-clsname
                              AND clstype = ls_class-clstype.
          APPEND ls_cmp TO lt_cmps.
        ENDLOOP.      " components

*       Components summary list header
        CALL METHOD gr_html->add_string_raw( c_html_s4_components ).
        CALL METHOD gr_html->flush_buffer.
*       Write components summary in HTML table
        gr_html->add_tag_start( 'table' ).
        CALL METHOD gr_html->flush_buffer.

*       Write public components
        CALL METHOD do_write_comp_summary
          EXPORTING
            i_exp   = '2'
            it_cmps = lt_cmps.
*       Write protected components
        IF lv_write_class_components_prot IS NOT INITIAL.
          CALL METHOD do_write_comp_summary
            EXPORTING
              i_exp   = '1'
              it_cmps = lt_cmps.
        ENDIF.
*       Write private components
        IF lv_write_class_components_priv IS NOT INITIAL.
          CALL METHOD do_write_comp_summary
            EXPORTING
              i_exp   = '0'
              it_cmps = lt_cmps.
        ENDIF.

*       Close components summary table
        gr_html->add_tag_end( 'table' ).
        CALL METHOD gr_html->flush_buffer.

*       Components details
        LOOP AT lt_cmps INTO ls_cmp.
          CALL METHOD do_write_details_class_comp( ls_cmp ).
        ENDLOOP.

      ENDLOOP.      " classes
    ENDLOOP.      " programs

  ENDMETHOD.                    "write_class_components


* Writes summary (table) with class components for one exposure.

  METHOD do_write_comp_summary.

    DATA: ls_cmp TYPE ts_classcmp.

*   Interfaces
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'I'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Types
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'T'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Constants
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'C'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Aliases
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'A'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Class-data
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'CD'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Data
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'D'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Class-methods
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'CM'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Methods
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'M'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Class-events
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'CE'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.
*   Events
    LOOP AT it_cmps INTO ls_cmp WHERE exposure = i_exp
                                  AND cmptype  = 'E'.
      CALL METHOD do_write_comp_summary_single( ls_cmp ).
    ENDLOOP.


  ENDMETHOD.                    "do_write_comp_summary


* Writes single line in table of class components summary.

  METHOD do_write_comp_summary_single.

    DATA: l_str  TYPE string, l_str2 TYPE string, l_str3 TYPE string.

*   Build exposure and component type string (i.e. Public Data)
    l_str  = get_class_cmp_exposure_desc( is_cmp-exposure ).
    l_str3 = get_class_cmp_type_desc( is_cmp-cmptype ).
    CONCATENATE l_str l_str3 INTO l_str SEPARATED BY space.

    l_str2 = is_cmp-cmpname.
*   If detailed code info is written - build a reference
    IF lv_write_code_units_details IS NOT INITIAL.

      CALL METHOD build_a_label_class_comp
        EXPORTING
          is_cmp  = is_cmp
        IMPORTING
          e_label = l_str3.
      CALL METHOD gr_html->str_a_href
        EXPORTING
          i_label = l_str3            " ie. Method do_something
          i_text  = l_str2
        RECEIVING
          e_str   = l_str2.
    ENDIF.

    CALL METHOD add_str_var
      EXPORTING
        i_str    = c_html_s4_comp_sum_info
        i_value1 = l_str
        i_value2 = l_str2.

  ENDMETHOD.                    "do_write_comp_summary_single


* Writes details of class component.

  METHOD do_write_details_class_comp.

    DATA: ls_unit TYPE ts_modunit.
    DATA: l_name TYPE t_unitname.
    DATA: l_str1 TYPE string,
          l_str2 TYPE string,
          l_str3 TYPE string,
          l_str4 TYPE string.

    l_str1 = get_class_cmp_exposure_desc( is_cmp-exposure ).
    l_str2 = get_class_cmp_type_desc( is_cmp-cmptype ).
    l_str3 = is_cmp-cmpname.

    CALL METHOD build_a_label_class_comp
      EXPORTING
        is_cmp  = is_cmp
      IMPORTING
        e_label = l_str4.

    CALL METHOD do_write_unit_details
      EXPORTING
        i_name     = l_str3
        i_exp      = l_str1
        i_type     = l_str2
        i_label    = l_str4
        is_param   = is_cmp-param
        is_comment = is_cmp-comment.

*   For methods - write additional info
    CHECK is_cmp-cmptype = 'M'  OR  is_cmp-cmptype = 'CM'.

*   Methods can have additional comments in implementation section.
*   These commenta are not in table of class components, but in
*   code units.
    READ TABLE gt_modunits INTO ls_unit
                             WITH KEY name = is_cmp-cmpname
                                      type = 'M'
                                      clsname = is_cmp-clsname.
    IF sy-subrc = 0.
      CALL METHOD do_write_comments( ls_unit-comment ).
    ENDIF.

*   Database tables
    l_name = is_cmp-cmpname.
    CALL METHOD do_write_db_tables_for_unit
      EXPORTING
        i_clsname = is_cmp-clsname
        i_name    = l_name
        i_type    = 'M'.

  ENDMETHOD.                    "do_write_details_class_comp


* Section 5 - details of non-class code units (forms, functions, ...)

  METHOD write_non_class_code_units.

    DATA: ls_prog TYPE ts_program,
          ls_unit TYPE ts_modunit.
    DATA: l_str  TYPE string, l_str2 TYPE string, l_str3 TYPE string.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_5 ).
    CALL METHOD gr_html->flush_buffer.

*   Display units sorted by programs
    LOOP AT gt_programs INTO ls_prog.

*     Program name
      l_str = ls_prog-program.
      IF lv_write_includes_details IS NOT INITIAL.

        CALL METHOD gr_html->str_a_href
          EXPORTING
            i_label = l_str
            i_text  = l_str
          RECEIVING
            e_str   = l_str.
      ENDIF.
      CALL METHOD add_str_var
        EXPORTING
          i_str    = c_html_s5_program
          i_value1 = l_str.

*     Units defined in the program
      LOOP AT gt_modunits INTO ls_unit
                           WHERE program = ls_prog-program.

*       Skip some unit types (related to classes - they are displayed
*       in dedicated section
        CHECK ls_unit-type <> 'CD'  AND  ls_unit-type <> 'I'  AND
              ls_unit-type <> 'M'.

*       Details of code unit
        CALL METHOD do_write_details_nonclass_unit( ls_unit ).

      ENDLOOP.      " units
    ENDLOOP.        " programs

  ENDMETHOD.                    "write_code_units_details


* Writes details of single non-class code unit.

  METHOD do_write_details_nonclass_unit.

    DATA: l_str1 TYPE string,
          l_str2 TYPE string,
          l_str3 TYPE string.

    l_str1 = get_code_unit_type_desc( is_unit-type ).
    l_str2 = is_unit-name.

    CALL METHOD build_a_label_unit
      EXPORTING
        is_unit = is_unit
      IMPORTING
        e_label = l_str3.

    CALL METHOD do_write_unit_details
      EXPORTING
        i_name     = l_str2
        i_type     = l_str1
        i_exp      = ''
        i_label    = l_str3
        is_param   = is_unit-param
        is_comment = is_unit-comment.

    CALL METHOD do_write_db_tables_for_unit
      EXPORTING
        i_clsname = is_unit-clsname
        i_name    = is_unit-name
        i_type    = is_unit-type.

  ENDMETHOD.                    "do_write_details_nonclass_unit


* Section 6 - hierarchy of code units calls (functions, forms, ...).

  METHOD write_code_units_hier.

    DATA: ls_hier TYPE ts_unitshier,
          ls_unit TYPE ts_modunit.
    DATA: l_str  TYPE string, l_str2 TYPE string, l_str3 TYPE string,
          l_out  TYPE string.
    DATA: l_cnt TYPE i.

*   Section title
    CALL METHOD gr_html->add_string_raw( c_html_header_6 ).
    CALL METHOD gr_html->flush_buffer.

*   Process the following calls
*    BREAK-POINT.
    LOOP AT gt_unitshier INTO ls_hier WHERE depth = 1 OR depth = 2.

*     String representin depth of nesting (. . .)
      CLEAR: l_out.
*      l_cnt = ls_hier-depth - 1.
*      DO l_cnt TIMES.
*      CONCATENATE l_out c_html_s6_nesting_level INTO l_out.
*      ENDDO.

      l_str = get_code_unit_type_desc( ls_hier-type2 ).
      l_str2 = ls_hier-name2.
*     At the moment - no reference for methods (we don't know
*     called class name)
*      IF lv_write_code_units_details IS NOT INITIAL  AND
*         ls_hier-type2 <> 'M'.
**       Check if we have details of called unit
*        READ TABLE gt_modunits TRANSPORTING NO FIELDS
*             WITH KEY name = ls_hier-name2  type = ls_hier-type2.
*        IF sy-subrc = 0.
**         Create label for anchor
*          CLEAR: ls_unit.
*          ls_unit-name = ls_hier-name2.
*          ls_unit-type = ls_hier-type2.
*          CALL METHOD build_a_label_unit
*            EXPORTING
*              is_unit = ls_unit
*            IMPORTING
*              e_label = l_str3.
*          CALL METHOD gr_html->str_a_href
*            EXPORTING
*              i_label = l_str3
*              i_text  = l_str2
*            RECEIVING
*              e_str   = l_str2.
*        ENDIF.
*      ENDIF.
      IF ls_hier-type2 = 'F'.
        CONCATENATE 'call' l_str2 INTO l_str2 SEPARATED BY space.
      ENDIF.
      CONCATENATE l_out l_str2 ';' '//'  INTO l_out.
      CALL METHOD add_str_var
        EXPORTING
          i_str    = c_html_s6_unit
          i_value1 = l_out
          i_value2 = l_str.

    ENDLOOP.

  ENDMETHOD.                    "write_code_units_hier


* Writes details of single code unit. Details should be passed as
* strings (no further processing is performed on them).

  METHOD do_write_unit_details.

    DATA: ls_param  TYPE ts_param_detail.
    DATA: l_str  TYPE string, l_str2 TYPE string.

*   Header of code unit - name must be 'a name'
    CALL METHOD gr_html->str_a_name
      EXPORTING
        i_label = i_label
        i_text  = i_name
      RECEIVING
        e_str   = l_str.
    CALL METHOD add_str_var
      EXPORTING
        i_str    = c_html_unit_det_header
        i_value1 = l_str.

*   For class components, concatenate exposure (ie. Public) with
*   component type (ie. Method).
    l_str2 = i_type.
    IF i_exp IS NOT  INITIAL.
      CONCATENATE i_exp i_type INTO l_str2 SEPARATED BY space.
    ENDIF.

*   Prepare string with parameters (declaration)
*    CLEAR: l_str.
*    READ TABLE is_param-params INTO ls_param INDEX 1.
*    IF sy-subrc = 0.
*      l_str = ls_param-text.
*    ENDIF.

*   Unit declaration
    CALL METHOD add_str_var
      EXPORTING
        i_str    = c_html_unit_det_declaration
        i_value1 = i_name
        i_value2 = l_str2
        i_value3 = is_param-text.         " l_str.

*   Unit comments
    CALL METHOD do_write_comments( is_comment ).

  ENDMETHOD.                    "do_write_unit_details


* Writes comments formated as a HTML dictionary.

  METHOD do_write_comments.

    DATA: ls_tag TYPE ts_commtag.
    DATA: l_last_param TYPE ts_commtag-param.
    DATA: l_str1 TYPE string,
          l_str2 TYPE string,
          l_str3 TYPE string.

*   Check, if comment is filled
    CHECK is_comment IS NOT INITIAL.

*   Start dictionary
    gr_html->add_tag_start( 'dl' ).
    CALL METHOD gr_html->flush_buffer.

    LOOP AT is_comment-tags INTO ls_tag.

*     If parametr name is repeated - we write it only once
      IF ls_tag-param = l_last_param.
        CLEAR: ls_tag-param.
      ELSE.
        l_last_param = ls_tag-param.
      ENDIF.

*     Text conversions
      l_str1 = ls_tag-tag.
      l_str1 = gr_html->str2html( l_str1 ).
      l_str2 = ls_tag-param.
      l_str2 = gr_html->str2html( l_str2 ).
      l_str3 = ls_tag-text.
      IF lv_html_comments IS INITIAL.
        l_str3 = gr_html->str2html( l_str3 ).
      ENDIF.

*     If tag is INFO, it is printed directly as definition
      IF ls_tag-tag = 'INFO'.
        CALL METHOD gr_html->add_tag
          EXPORTING
            tag = 'dd'
            str = l_str3.
      ELSE.
*       For other tags - write full info
        CALL METHOD add_str_var
          EXPORTING
            i_str    = c_html_comment_complex
            i_value1 = l_str1
            i_value2 = l_str2
            i_value3 = l_str3.
      ENDIF.
    ENDLOOP.

*   Close dictionary
    gr_html->add_tag_end( 'dl' ).
    CALL METHOD gr_html->flush_buffer.

  ENDMETHOD.                    "do_write_comments


* Builds label for anchor for code unit. It can be used in 'a name' and
* 'a href' HTML tags.

  METHOD build_a_label_unit.

    DATA: l_type    TYPE t_unittype.
    DATA: l_typestr TYPE string.

    l_type = is_unit-type.
*   Class implementation change to class definition
    IF l_type = 'CI'.
      l_type = 'CD'.
    ENDIF.
    l_typestr = get_code_unit_type_desc( l_type ).

*   For methods we must include class name in an anchor
    IF l_type = 'M'.
      CONCATENATE l_typestr is_unit-clsname is_unit-name INTO e_label
                                                  SEPARATED BY space.
    ELSE.
      CONCATENATE l_typestr is_unit-name INTO e_label
                                                  SEPARATED BY space.
    ENDIF.

  ENDMETHOD.                    "build_a_label_unit


* Builds label for anchor for class definition or interface.
* It can be used in 'a name' and 'a href' HTML tags.

  METHOD build_a_label_class_name.

    DATA: l_type    TYPE t_unittype.
    DATA: l_typestr TYPE string.

    l_type = 'CD'.
    IF i_clstype = '1'.
      l_type = 'I'.
    ENDIF.
    l_typestr = get_code_unit_type_desc( l_type ).

    CONCATENATE l_typestr i_clsname INTO e_label SEPARATED BY space.

  ENDMETHOD.                    "build_a_label_class_name


* Builds label for anchor for class component.
* It can be used in 'a name' and 'a href' HTML tags.

  METHOD build_a_label_class_comp.

    DATA: l_typestr TYPE string.

    l_typestr = get_class_cmp_type_desc( is_cmp-cmptype ).
    CONCATENATE l_typestr is_cmp-clsname is_cmp-cmpname INTO e_label
                                                  SEPARATED BY space.

  ENDMETHOD.                    "build_a_label_class_comp


* Adds to HTML document string after replacing varaibles @n@ (n=1..3)
* with provided value.

  METHOD add_str_var.

    DATA: str TYPE string.

    str = i_str.
    REPLACE ALL OCCURRENCES OF '@1@' IN str WITH i_value1.
    REPLACE ALL OCCURRENCES OF '@2@' IN str WITH i_value2.
    REPLACE ALL OCCURRENCES OF '@3@' IN str WITH i_value3.
    REPLACE ALL OCCURRENCES OF '@4@' IN str WITH i_value4.
    CALL METHOD gr_html->add_string_raw( str ).
    CALL METHOD gr_html->flush_buffer.

  ENDMETHOD.                    "add_str_var

ENDCLASS.                    "zcl_ic_code_analysis_html IMPLEMENTATION
