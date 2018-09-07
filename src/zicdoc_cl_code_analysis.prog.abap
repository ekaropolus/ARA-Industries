*&---------------------------------------------------------------------*
*&  Include           ZICDOC_CL_CODE_ANALYSIS
*&---------------------------------------------------------------------*
* @Author Ireneusz Cwir
* @Date   June 2006
* @Info
* Definiotion and implementation of class CL_IC_CODE_ANALYSIS handling
* ABAP sorce code scan and analysis. Results of analysis are placed in
* several public tables. Class does not writes results of analysis
* to any devices - it must be done bu user of the class.


* Types defined to be used in the class and other programs.
* They are used to declare tables with results of analysis.

* Comment tag. Represents segment or section of code comments.
* Each segment starts with tag name: @NAME.
* If no tag is placed in the beginning of comment, INFO is used
* by default.
* All following lines of comment are concatenated into one string.
* Blank line can be used to finish comment (or any ABAP statemnt).
* To place line break, you can use HTML commands (like <br>).
* Comments:
* - Program level comment - starts with: *&. They are added in the
*   beginning of include and by pretty-printer for forms.<br>
* - Function parameters - starts with: *" <br>
* - Other comments - start with *. <br>
* - Comments starting with " are ignored <br>
* - Obligatory to put SPACE after above chars <br>
* - Between program comment and following form comment there should be
*   one blank line placed
* - Predefined comment variables (tags) start with: @ <br>
* - Comments can contain HTML formating commands <br>

* Comment section - starting with special tag (like @Author or @Info).
TYPES: BEGIN OF ts_commtag,
         tag(30)   TYPE c,         " tag name (e.c. INFO, AUTHOR)
         param(50) TYPE c,       " optional parameter
         text      TYPE string,       " text of comment
       END OF ts_commtag.
TYPES: tt_commtag TYPE STANDARD TABLE OF ts_commtag
                       WITH DEFAULT KEY.

* Comment. Represents continuous part of code comments. It can consist
* of several tags (segments, sections).
TYPES: BEGIN OF ts_comment,
         tags TYPE tt_commtag,
       END OF ts_comment.


* Structure describing individual parameter of method, form, class
* declaration, etc. In the current version it is not implemented yet,
* so it contains just one dummy field. In the future, it can contain
* field for parameter name, type, import/export, etc.
TYPES: BEGIN OF ts_param_detail,
         dummy TYPE c,
       END OF ts_param_detail.
TYPES: tt_param_details TYPE STANDARD TABLE OF ts_param_detail
                             WITH DEFAULT KEY.

* Structure describing all parameters of single method, form, etc.
* At the moment, TEXT field containing the whole declaration. In the
* future, declaration cab further analyzed and split into individual
* parameters (name, type, etc.).
TYPES: BEGIN OF ts_parameters,
         text   TYPE string,
         params TYPE tt_param_details,
       END OF ts_parameters.


* Hierarchy of programs and includes (based on LEVELS returned by SCAN).
* For macros (DEFINE...) a special prog type is used ('9')
TYPES: BEGIN OF ts_program,
         program TYPE programm,    " Prog (include) name
         subc    TYPE subc,        " Prog types (1=executable, ...)
         title   TYPE repti,       " Report title
         depth   TYPE level_dpth,  " Nesting depth of code unit
         level   TYPE level_levl,  " Index of superior code unit
         comment TYPE ts_comment,  " Comment for program
       END OF ts_program.


* Class/Interface parameters
* This structure is filled-up from class/interface definition.
TYPES: BEGIN OF ts_class,
         clsname TYPE seoclsname,    " Class/intf name
         clstype TYPE seoclstype,    " 0-class, 1-interface
         program TYPE programm,      " Program, Include name
         super   TYPE seoclsname,    " Class/intf name of superclass
         param   TYPE ts_parameters, " Parameters
         comment TYPE ts_comment,    " Comment for class
       END OF ts_class.

* Components of class/interface
* Components types:
*       I  - interface
*       T  - type
*       C  - constant
*       A  - aliases
*       D  - data
*       CD - class-data
*       M  - method
*       CM - class-method
*       E  - event
*       CE - class-event
TYPES: t_clscmptype(2) TYPE c.

* This structure is filled-up from class/interface definition. For
* methods, additional comments are also placed in code units (from
* method implementation).

TYPES: BEGIN OF ts_classcmp,
         clsname  TYPE seoclsname,   " Class/intf name
         clstype  TYPE seoclstype,   " 0-class, 1-interface
         exposure TYPE seoexpose,    " 0-private, 1-protected, 2-public
         cmpname  TYPE seocmpname,   " Component name
         cmptype  TYPE t_clscmptype, " Component type (see above)
         param    TYPE ts_parameters, " Parameters
         comment  TYPE ts_comment,   " Comment for component
       END OF ts_classcmp.
TYPES: tt_classcmps TYPE STANDARD TABLE OF ts_classcmp.


* Type for code unit name. It must be long enough to store
* interface + ~ + method name
TYPES: t_unitname(70) TYPE c.

*   Types of code modularization units and calls
*   Types: F  - form
*          FM - function module
*          CF - customer function
*          B  - BADI
*          T  - transaction
*          E  - main event (GET, INITIALIZATION, START-OF-SELECTION...
*          CD - class definition
*          CI - class implementation
*          I  - interface definition
*          M  - method
*          S  - submit program
TYPES: t_unittype(2) TYPE c.

* Source code modularization units details. It contains information
* from implementation.

TYPES: BEGIN OF ts_modunit,
         name    TYPE t_unitname,   " Name of form, function, event,
         type    TYPE t_unittype,   " Type of unit - see above
         program TYPE programm,     " Program, Include name
         clsname TYPE seoclsname,   " class name (required for methods)
         param   TYPE ts_parameters, " Parameters (as string)
         comment TYPE ts_comment,   " Comment for unit
       END OF ts_modunit.


* Hierarchy of code units calls
TYPES: BEGIN OF ts_unitshier,
         name1    TYPE t_unitname,  " Caller name (main)
         type1    TYPE t_unittype,  " Caller type (main)
         prog1    TYPE programm,    " Caller program (main)
         clsname1 TYPE seoclsname, " Caller class name
         name2    TYPE t_unitname,  " Callee name (sub)
         type2    TYPE t_unittype,  " Callee type (sub)
         depth    TYPE level_dpth,  " Nesting depth of call
       END OF ts_unitshier.


*   Database tables usage
*   Access modes: T - Tables statement
*                 S - select (read)
*                 U - Update
*                 I - Insert
*                 M - Modify (U lub I)
*                 D - Delete
TYPES: t_dbaccmode TYPE c.

TYPES: BEGIN OF ts_dbtable,
         table     TYPE tabname,    " Table name
         mode      TYPE t_dbaccmode, " Access mode - see above
         ddtext    TYPE as4text,    " Table description
         program   TYPE programm,   " Program, Include
         clsname   TYPE seoclsname, " class name (required for methods)
         name      TYPE t_unitname, " Function or form name (access)
         type      TYPE t_unittype, " Module type
         statement TYPE string,
       END OF ts_dbtable.
TYPES: tt_dbtables TYPE STANDARD TABLE OF ts_dbtable.


*----------------------------------------------------------------------*
*       CLASS zcl_ic_code_analysis DEFINITION
*----------------------------------------------------------------------*
* Class contains methods used for source code analysis. Results are
* stored in atributes (tables) and they are not output. It must be
* done by another program (for instance subclas).
*----------------------------------------------------------------------*


CLASS zcl_ic_code_analysis DEFINITION.

  PUBLIC SECTION.

*   Main (root) program name - program, from which analysis starts
    DATA: gv_main_program_name TYPE progname.

*   Results of detailed analysis. They can be read by user of class
*   and then displayed.
*   Table contains hierarchy of includes (starting from main program).
    DATA: gt_programs  TYPE STANDARD TABLE OF ts_program,
*   Table contains general information about classes and interfaces.
          gt_classes   TYPE STANDARD TABLE OF ts_class,
*   Table contains components (attributes, methods) of classes and
*   interfaces.
          gt_classcmps TYPE STANDARD TABLE OF ts_classcmp,
*   Table contains non-class code units (like forms, functions). As an
*   exception, it contains comments for method implementation.
          gt_modunits  TYPE STANDARD TABLE OF ts_modunit,
*   Table contains hierarchy of calls of code subunits (like calling
*   form from anther one).
          gt_unitshier TYPE STANDARD TABLE OF ts_unitshier,
*   Table contans information about database tables affected by
*   code units.
          gt_dbtables  TYPE STANDARD TABLE OF ts_dbtable.

    METHODS: constructor.
*   Resets (clears) all atributes of the object.
    METHODS: reset.

*   The main method which performs analysis of a program.
*   Program must be available in the system.
*   Results are stored in atribute tables (GT_XXX).
*   @IMPORTING I_PROGRAM - The name of program in the system to be
*              analyzed.
*   @RETURNING RET_CODE - Returns result of analysis: 0-OK, <>0-error
    METHODS: analyse_program IMPORTING i_program       TYPE any
                             RETURNING VALUE(ret_code) TYPE i,
*   This method loads source code from the file and then performs
*   analysis. With this method it is not possible to analyse
*   nested includes.
*   @IMPORTING I_FILENAME - The name with a path of program on local
*              disk to loaded and analyzed.
*   @RETURNING RET_CODE - Returns result of analysis: 0-OK, <>0-error
      analyse_file    IMPORTING i_filename      TYPE string
                      RETURNING VALUE(ret_code) TYPE i.

*   Returns description of code unit type ('Form' for 'F'...).
    METHODS: get_code_unit_type_desc IMPORTING i_type        TYPE t_unittype
                                     RETURNING VALUE(e_desc) TYPE string,
*   Returns desription of class component type ('Method' for 'M'...).
      get_class_cmp_type_desc IMPORTING i_type        TYPE t_clscmptype
                              RETURNING VALUE(e_desc) TYPE string,
*   Returns description of class exposure type ('Public' for '2')
      get_class_cmp_exposure_desc IMPORTING i_exp         TYPE seoexpose
                                  RETURNING VALUE(e_desc) TYPE string,
*   Return description of database access mode ('Select' for 'S'...)
      get_db_access_mode_desc IMPORTING i_mode        TYPE t_dbaccmode
                              RETURNING VALUE(e_desc) TYPE string.

    "   Helper funtions for testing
*   Displays results of SCAN ABAP-CODE (dumps contents of returned
*   tables). It helps understand results of built-in keyword.
*   You will find very useful information in online help for
*   keyword <code>SCAN ABAP-SOURCE<code>.
    METHODS: dump_tables_scan,
*   Displays individual statements and tokens with additional details.
*   It also help understand results of SCAN keyword.
      dump_statements_detailed,
*   Displays statements (and tokens building statements) as source code
*   lines. This is how ZICDOC sees source code after replacing
*   INCLUDE with content of that program, unpacking statemts with ':'
*   and other conversions.
      dump_statements.

  PROTECTED SECTION.

*   COMPATYBILITY!!!!!
*   For compatibility (SCAN keyword parameters changed in WAS)
    TYPES: ts_token TYPE stokesx.                     " in >= WAS 6.10
*    TYPES: begin of ts_token,                        " in R/3 4.6C
*              str type string,
*              row type TOKEN_ROW,
*              OFF2 type TOKEN_OFF2,
*              OFF3 type TOKEN_OFF3,
*              COL type TOKEN_COL,
*              LEN1 type TOKEN_LEN1,
*              LEN2 type TOKEN_LEN2,
*              LEN3 type TOKEN_LEN3,
*              TYPE type TOKEN_TYPE,
*            end of ts_token.


*   Source code of a program
    TYPES: BEGIN OF ts_srccode,
             line(200) TYPE c,
           END OF ts_srccode.
    DATA: lt_srccode TYPE STANDARD TABLE OF ts_srccode.

*   Direct results of SCAN ABAP-SOURCE
    DATA: lt_tokens     TYPE STANDARD TABLE OF ts_token,
*   Direct results of SCAN ABAP-SOURCE
          lt_statements TYPE STANDARD TABLE OF sstmnt,
*   Direct results of SCAN ABAP-SOURCE
          lt_levels     TYPE STANDARD TABLE OF slevel.

  PRIVATE SECTION.

*   Hierarchy of calls before further reworking.
    DATA: lt_uhier TYPE STANDARD TABLE OF ts_unitshier.

*   Current program, class and module unit during analysis
    DATA: ls_cur_prog  TYPE slevel,
          ls_cur_class TYPE ts_class,
          ls_cur_unit  TYPE ts_modunit.
*   The last read comment. It can be assigne to the next ABAP statement.
    DATA: ls_last_comment TYPE ts_comment.

*   Current statement and token during analysis
    DATA: lv_cur_token_idx TYPE i,
          lv_cur_stmnt_idx TYPE i.
    DATA: ls_cur_stmnt TYPE sstmnt,
          ls_cur_token TYPE ts_token.

*   Performs analysis of source code loaded into private table. It is
*   used by the main function of the class.
    METHODS: do_analysis RETURNING VALUE(ret_code) TYPE i.

    "   Analysis of specific statements groups
    METHODS: do_events,
      do_databases,
      do_code_units,
      do_classes,
      do_class_components,
      do_comments,
      do_others.

*   Reads the following tokens (starting from the next) and places
*   them into parametr structure.
    METHODS: do_parameters EXPORTING es_param TYPE ts_parameters.

*   Fills-up current program nad module unit during analysis
    METHODS: set_cur_prog,
      set_cur_class IMPORTING i_clsname TYPE seoclsname
                              i_clstype TYPE seoclstype
                              i_super   TYPE seoclsname
                              is_param  TYPE ts_parameters,
      set_cur_unit IMPORTING i_name   TYPE t_unitname
                             i_type   TYPE t_unittype
                             is_param TYPE ts_parameters.
*   Appends current module unit to attribute table.
    METHODS: append_cur_unit,
      append_cur_class,
      append_class_cmp IMPORTING is_clscmp TYPE ts_classcmp,
      append_sub_unit IMPORTING i_name TYPE t_unitname
                                i_type TYPE t_unittype,
      append_dbtable IMPORTING i_table TYPE tabname
                               i_mode  TYPE c,
      append_cur_prog_comment,
      append_cur_class_comment,
      append_cur_unit_comment,
      append_comment.

    METHODS: levels_to_programms,
      rework_units_hier,
      do_hier_path IMPORTING is_start TYPE ts_unitshier.

    METHODS: get_first_statement EXPORTING  es_stmnt TYPE sstmnt,
      get_next_statement EXPORTING es_stmnt TYPE sstmnt,
      get_cur_statement EXPORTING es_stmnt TYPE sstmnt,
      get_first_token EXPORTING es_token TYPE ts_token,
      get_next_token EXPORTING es_token TYPE ts_token,
      get_cur_token EXPORTING es_token TYPE ts_token.

*   Compatybility!!!!
    METHODS: do_scan_code_46c RETURNING VALUE(ret_code) TYPE i,
      do_scan_code_610 RETURNING VALUE(ret_code) TYPE i.

ENDCLASS.                    "zcl_ic_code_analysis DEFINITION


*----------------------------------------------------------------------*
*       CLASS zcl_ic_code_analysis IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS zcl_ic_code_analysis IMPLEMENTATION.

* Clears all atributes. It is not necessary, but just to do anything.
  METHOD constructor.
    CALL METHOD reset.
  ENDMETHOD.                    "constructor


* Resets (clears) all atributes of the object.
  METHOD reset.
    CLEAR: gv_main_program_name.
    CLEAR: lv_cur_stmnt_idx, lv_cur_token_idx.
    CLEAR: ls_cur_stmnt, ls_cur_token.
    CLEAR: ls_cur_prog, ls_cur_class, ls_cur_unit, ls_last_comment.
    FREE: lt_srccode, lt_tokens, lt_statements, lt_levels, lt_uhier.
    FREE: gt_programs, gt_modunits, gt_unitshier, gt_dbtables,
          gt_classes, gt_classcmps.
  ENDMETHOD.                    "reset

*======================================================================

* Returns description of code unit type.

  METHOD get_code_unit_type_desc.

    CASE i_type.
      WHEN 'F'.
        e_desc = 'Form'.
      WHEN 'FM'.
        e_desc = 'Function module'.
      WHEN 'CF'.
        e_desc = 'Customer function'.
      WHEN 'B'.
        e_desc = 'BADi'.
      WHEN 'T'.
        e_desc = 'Transaction'.
      WHEN 'E'.
        e_desc = 'List event'.
      WHEN 'S'.
        e_desc = 'Submit program'.
      WHEN 'CD'.
        e_desc = 'Class definition'.
      WHEN 'CI'.
        e_desc = 'Class implementation'.
      WHEN 'I'.
        e_desc = 'Interface'.
      WHEN 'M'.
        e_desc = 'Method'.    " Must be the same as in the next method
    ENDCASE.

  ENDMETHOD.                    "get_code_unit_type_desc


* Returns description of class component type.

  METHOD get_class_cmp_type_desc.

    CASE i_type.
      WHEN 'T'.
        e_desc = 'Type'.
      WHEN 'C'.
        e_desc = 'Constant'.
      WHEN 'I'.
        e_desc = 'Interface'.
      WHEN 'A'.
        e_desc = 'Alias'.
      WHEN 'D'.
        e_desc = 'Data'.
      WHEN 'CD'.
        e_desc = 'Class-data'.
      WHEN 'M'.
        e_desc = 'Method'.    " Must be the same as in the prev method
      WHEN 'CM'.
        e_desc = 'Class-method'.
      WHEN 'E'.
        e_desc = 'Event'.
      WHEN 'CE'.
        e_desc = 'Class-event'.
    ENDCASE.

  ENDMETHOD.                    "get_class_cmp_type_desc


* Returns description of class component exposure.

  METHOD  get_class_cmp_exposure_desc.

    CASE i_exp.
      WHEN '0'.
        e_desc = 'Private'.
      WHEN '1'.
        e_desc = 'Protected'.
      WHEN '2'.
        e_desc = 'Public'.
    ENDCASE.

  ENDMETHOD.                    "get_class_cmp_exposure_desc


* Returns description of database access mode.

  METHOD get_db_access_mode_desc.

    CASE i_mode.
      WHEN 'T'.
        e_desc = 'Table'.
      WHEN 'S'.
        e_desc = 'Select'.
      WHEN 'U'.
        e_desc = 'Update'.
      WHEN 'I'.
        e_desc = 'Insert'.
      WHEN 'M'.
        e_desc = 'Modify (U,I)'.
      WHEN 'D'.
        e_desc = 'Delete'.
    ENDCASE.

  ENDMETHOD.                    "get_db_access_mode_desc


*======================================================================

* The main method used to analyse code of program stored in the system.

  METHOD analyse_program.

    CALL METHOD reset.

*   Read source code of the program into local table
    READ REPORT i_program INTO lt_srccode.
    ret_code = sy-subrc.
    CHECK ret_code = 0.

*   Store the main (starting) program name and analyse code.
    gv_main_program_name = i_program.
    CALL METHOD do_analysis
      RECEIVING
        ret_code = ret_code.

  ENDMETHOD.                    "analyse_program


* The main method used to analyse code loaded from file on presentation
* server.

  METHOD analyse_file.

    CALL METHOD reset.

*   Load code from local file
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = i_filename
        filetype                = 'ASC'
      TABLES
        data_tab                = lt_srccode
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    ret_code = sy-subrc.
    CHECK ret_code = 0.

*   Store the main (starting) program name and analyze code.
    gv_main_program_name = i_filename.
    TRANSLATE gv_main_program_name TO UPPER CASE.
    CALL METHOD do_analysis
      RECEIVING
        ret_code = ret_code.

  ENDMETHOD.                    "analyse_file

*======================================================================

* Scans source code - to be used in version 4.6.
* Statement SCAN ABAP_SOURCE behaves in different way in system
* 4.6 and >= WAS 6.1. It uses other types of parameters.

  METHOD do_scan_code_46c.

    DATA: lt_tok         TYPE STANDARD TABLE OF stokex,
          ls_tok         TYPE stokex,
          ls_token       TYPE ts_token,
          lv_over(65535) TYPE c.

*    COMPATYBILITY!!!!!
*    SCAN ABAP-SOURCE lt_srccode TOKENS INTO lt_tok
*                                OVERFLOW INTO lv_over
*                                STATEMENTS INTO lt_statements
*                                WITH ANALYSIS
*                                WITH INCLUDES LEVELS INTO lt_levels
*                                WITH COMMENTS.
*    ret_code = sy-subrc.
**   Konwersja do formatu z 6.10 (STOKESX)
*    LOOP AT lt_tok INTO ls_tok.
*      MOVE-CORRESPONDING ls_tok TO ls_token.
*      IF ls_tok-ovfl <> space.
*        ls_token-str = lv_over+ls_tok-off1(ls_tok-len).
*      ENDIF.
*      APPEND ls_token TO lt_tokens.
*    ENDLOOP.

  ENDMETHOD.                    "do_analyse_46C


* Scans source code - to be used in version WAS 6.1 and newer.

  METHOD do_scan_code_610.

*    COMPATYBILITY!!!!!
    SCAN ABAP-SOURCE lt_srccode TOKENS INTO lt_tokens
                                STATEMENTS INTO lt_statements
                                WITH ANALYSIS
                                WITH INCLUDES LEVELS INTO lt_levels
                                WITH COMMENTS.
    ret_code = sy-subrc.

  ENDMETHOD.                    "do_analyse_610

*======================================================================

* The main method performing analysis of loaded source code.
* It scans source code and then converts returned tables into tables
* used later for documentation generation (global attributes).

  METHOD do_analysis.

    DATA: ls_stmnt TYPE sstmnt,
          ls_token TYPE ts_token.

*   Scan source code and fill tables of statements and tokens
    IF sy-saprl >= '610'.
      CALL METHOD do_scan_code_610
        RECEIVING
          ret_code = ret_code.
    ELSE.
      CALL METHOD do_scan_code_46c
        RECEIVING
          ret_code = ret_code.
    ENDIF.
    CHECK ret_code < 4.
    ret_code = 0.

*   Convert levels into GT_PROGRAMS
    CALL METHOD levels_to_programms.

*   Analyse statements of code one-by-one
    CALL METHOD get_first_statement
      IMPORTING
        es_stmnt = ls_stmnt.
    WHILE ls_stmnt-type <> space.

*     For comments - they recognized by statemnt type.
      IF ls_stmnt-type = 'P'.
        CALL METHOD do_comments.

*     For other keywords
      ELSE.

*       The first token of the current statement
        CALL METHOD get_first_token
          IMPORTING
            es_token = ls_token.

        CASE ls_token-str.
          WHEN 'START-OF-SELECTION' OR
               'END-OF-SELECTION'   OR
               'INITIALIZATION'     OR
               'GET'                OR
               'AT'.
            CALL METHOD do_events.

          WHEN 'TABLES' OR
               'SELECT' OR
               'UPDATE' OR
               'INSERT' OR
               'MODIFY' OR
               'DELETE'.
            CALL METHOD do_databases.

          WHEN 'FORM'         OR
               'ENDFORM'      OR
               'FUNCTION'     OR
               'ENDFUNCTION'  OR
               'METHOD'       OR
               'ENDMETHOD'    OR
               'PERFORM'      OR
               'CALL'         OR
               'SUBMIT'.
            CALL METHOD do_code_units.

          WHEN 'CLASS'      OR
               'ENDCLASS'   OR
               'INTERFACE'  OR
               'ENDINTERFACE'.
            CALL METHOD do_classes.

          WHEN OTHERS.
            CALL METHOD do_others.
        ENDCASE.
        CLEAR: ls_last_comment.
      ENDIF.

      CALL METHOD get_next_statement
        IMPORTING
          es_stmnt = ls_stmnt.
    ENDWHILE.

*   Convert code units calls hierarchy to more suitable for further
*   processing
    CALL METHOD rework_units_hier.

  ENDMETHOD.                    "do_analysis


* Performs analysis of statements representing list events

  METHOD do_events.

    DATA: ls_token TYPE ts_token,
          ls_param TYPE ts_parameters.
    DATA: l_name    TYPE t_unitname.

    CALL METHOD get_cur_token
      IMPORTING
        es_token = ls_token.
    l_name = ls_token-str.

*   For AT, we use two tokens to build name
    IF ls_token-str = 'AT'.
      CALL METHOD get_next_token
        IMPORTING
          es_token = ls_token.
      CONCATENATE l_name ls_token-str INTO l_name SEPARATED BY space.
    ENDIF.

*   The rest of tokens builds parameter
    CALL METHOD do_parameters
      IMPORTING
        es_param = ls_param.

    CALL METHOD set_cur_unit
      EXPORTING
        i_name   = l_name
        i_type   = 'E'
        is_param = ls_param.
    CALL METHOD append_cur_unit.

  ENDMETHOD.                    "do_events


* Performs analysis of statement representing definition and
* implementation of several types of code units and calls.

  METHOD do_code_units.

    DATA: ls_cmp  TYPE ts_classcmp.
    DATA: ls_token TYPE ts_token,
          ls_param TYPE ts_parameters.
    DATA: l_name TYPE t_unitname,
          l_type TYPE t_unittype.

    CALL METHOD get_cur_token
      IMPORTING
        es_token = ls_token.

    CASE ls_token-str.

      WHEN 'FORM'.
*       Form name
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        l_name = ls_token-str.
*       Form parameters
        CALL METHOD do_parameters
          IMPORTING
            es_param = ls_param.
        CALL METHOD set_cur_unit
          EXPORTING
            i_name   = l_name
            i_type   = 'F'
            is_param = ls_param.
        CALL METHOD append_cur_unit.

      WHEN 'FUNCTION' OR
           'METHOD'.
        l_type = 'FM'.
        IF ls_token-str = 'METHOD'.
          l_type = 'M'.
        ENDIF.
*       Name
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        l_name = ls_token-str.
        CALL METHOD set_cur_unit
          EXPORTING
            i_name   = l_name
            i_type   = l_type
            is_param = ls_param.
        CALL METHOD append_cur_unit.
*       In case of interface method implementation, it is not added to
*       class components from class definition. We must do there.
*       We can call function for all methods (not only interfaces).
        IF l_type = 'M'.
          ls_cmp-cmpname  = l_name.
          ls_cmp-cmptype  = l_type.
          ls_cmp-clsname  = ls_cur_class-clsname..
          ls_cmp-clstype  = '0'.
          ls_cmp-exposure = '2'.
          CALL METHOD append_class_cmp
            EXPORTING
              is_clscmp = ls_cmp.
        ENDIF.

      WHEN 'ENDFORM'      OR
           'ENDMETHOD'    OR
           'ENDFUNCTION'.
        CLEAR: ls_cur_unit.

      WHEN 'PERFORM'  OR
           'SUBMIT'.
        l_type = 'F'.
        IF ls_token-str = 'SUBMIT'.
          l_type = 'S'.
        ENDIF.
*       Name
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        l_name = ls_token-str.
        CALL METHOD append_sub_unit
          EXPORTING
            i_name = l_name
            i_type = l_type.

      WHEN 'CALL'.
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
*       Check the secont part of call
        CASE ls_token-str.
          WHEN 'FUNCTION'.
            l_type = 'FM'.
          WHEN 'CUSTOMER-FUNCTION'.
            l_type = 'CF'.
          WHEN 'BADI'.
            l_type = 'B'.
          WHEN 'METHOD'.
            l_type = 'M'.
          WHEN 'TRANSATION'.
            l_type = 'T'.
        ENDCASE.
        IF l_type <> space.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
          l_name = ls_token-str.
          CALL METHOD append_sub_unit
            EXPORTING
              i_name = l_name
              i_type = l_type.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "do_calls


* Performs analysis of statement representing definition and
* implementation of classes.

  METHOD do_classes.

    DATA: ls_token TYPE ts_token,
          ls_param TYPE ts_parameters.
    DATA: l_name    TYPE t_unitname,
          l_clsname TYPE seoclsname,
          l_super   TYPE seoclsname,
          l_type    TYPE t_unittype,
          l_clstype TYPE seoclstype.

    CALL METHOD get_cur_token
      IMPORTING
        es_token = ls_token.

    CASE ls_token-str.

      WHEN 'CLASS'  OR
           'INTERFACE'.
        l_clstype = '0'.                " class
        IF ls_token-str = 'INTERFACE'.
          l_clstype = '1'.              " interface
          l_type = 'I'.
        ENDIF.

*       Get class name
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        l_clsname = ls_token-str.

*       Get parameters for analysing special parameters of class.
*       They are not stored in dedicated components - it will be done
*       in the second run.
        WHILE ls_token-str <> space.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
          CASE ls_token-str.
*           Skip these types of declartions
            WHEN 'DEFERRED'  OR
                 'LOAD'.
              RETURN.
            WHEN 'DEFINITION'.
              l_type = 'CD'.
            WHEN 'IMPLEMENTATION'.
              l_type = 'CI'.
            WHEN 'FROM'.
              CALL METHOD get_next_token
                IMPORTING
                  es_token = ls_token.
              l_super = ls_token-str.
          ENDCASE.
        ENDWHILE.

*       The second run for filling standard component
*       First, we must skip starting keywords
        CALL METHOD get_first_token
          IMPORTING
            es_token = ls_token.
        DO 2 TIMES.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
        ENDDO.

        CALL METHOD do_parameters
          IMPORTING
            es_param = ls_param.

*       set_cur_class must be called before set_cur_unit
        CALL METHOD set_cur_class
          EXPORTING
            i_clsname = l_clsname
            i_clstype = l_clstype
            i_super   = l_super
            is_param  = ls_param.
        l_name = l_clsname.
        CALL METHOD set_cur_unit
          EXPORTING
            i_name   = l_name
            i_type   = l_type
            is_param = ls_param.

        CALL METHOD append_cur_unit.

*       For interface and class definition
        IF l_type = 'I'  OR  l_type = 'CD'.
          CALL METHOD append_cur_class.
*         Analyse components of class/interface
          CALL METHOD do_class_components.
          CLEAR: ls_cur_unit,
                 ls_cur_class.
        ENDIF.

      WHEN 'ENDCLASS'  OR
           'ENDINTERFACE'.
        CLEAR: ls_cur_unit,
               ls_cur_class.

    ENDCASE.

  ENDMETHOD.                    "do_classes


* Performs analysis of class definition - stores components.

  METHOD do_class_components.

    DATA: ls_stmnt TYPE sstmnt,
          ls_token TYPE ts_token.
    DATA: ls_cmp   TYPE ts_classcmp.

*   Set default values
    ls_cmp-exposure = 2.             " Public

*   Method is called from CLASS / INTERFACE statement, so need the next
    CALL METHOD get_next_statement
      IMPORTING
        es_stmnt = ls_stmnt.
    WHILE ls_stmnt-type <> space.

*     Clear selected fields
      CLEAR: ls_cmp-cmpname, ls_cmp-cmptype, ls_cmp-param.

*     We must handle comments
      IF ls_stmnt-type = 'P'.
        CALL METHOD do_comments.

      ELSE.

*       The first token of the current statement
        CALL METHOD get_first_token
          IMPORTING
            es_token = ls_token.

        CASE ls_token-str.

          WHEN 'ENDCLASS'  OR
               'ENDINTERFACE'.
            EXIT.

          WHEN 'PUBLIC'.
            ls_cmp-exposure = '2'.
          WHEN 'PROTECTED'.
            ls_cmp-exposure = '1'.
          WHEN 'PRIVATE'.
            ls_cmp-exposure = '0'.

          WHEN 'TYPES'.
            ls_cmp-cmptype = 'T'.
          WHEN 'CONSTANTS'.
            ls_cmp-cmptype = 'C'.
          WHEN 'INTERFACES'.
            ls_cmp-cmptype = 'I'.
          WHEN 'ALIASES'.
            ls_cmp-cmptype = 'A'.
          WHEN 'DATA'.
            ls_cmp-cmptype = 'D'.
          WHEN 'CLASS-DATA'.
            ls_cmp-cmptype = 'CD'.
          WHEN 'METHODS'.
            ls_cmp-cmptype = 'M'.
          WHEN 'CLASS-METHODS'.
            ls_cmp-cmptype = 'CM'.
          WHEN 'EVENTS'.
            ls_cmp-cmptype = 'E'.
          WHEN 'CLASS-EVENTS'.
            ls_cmp-cmptype = 'CE'.

        ENDCASE.

*       If CLSTYPE set, then we have component
        IF ls_cmp-cmptype IS NOT INITIAL.

*         The next token contains name
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
          ls_cmp-cmpname = ls_token-str.

*         The rest contains parameters
          CALL METHOD do_parameters
            IMPORTING
              es_param = ls_cmp-param.

*         Add component to the table
          CALL METHOD append_class_cmp
            EXPORTING
              is_clscmp = ls_cmp.

        ENDIF.
      ENDIF.

*   Get the next statement
      CALL METHOD get_next_statement
        IMPORTING
          es_stmnt = ls_stmnt.

    ENDWHILE.

  ENDMETHOD.                    "do_class_components


* Performs analysis of statement representing access to database table.

  METHOD do_databases.

    DATA: ls_token TYPE ts_token.
    DATA: l_table TYPE tabname,
          l_mode  TYPE c.

    CALL METHOD get_cur_token
      IMPORTING
        es_token = ls_token.


    CLEAR gv_statement.
    DATA: ls_statement LIKE LINE OF lt_statements.

    READ TABLE lt_statements WITH KEY from = lv_cur_token_idx
    INTO ls_statement.
    IF sy-subrc = 0.

      DATA ls_tokens_s LIKE LINE OF lt_tokens.
      LOOP AT lt_tokens INTO ls_tokens_s FROM ls_statement-from TO ls_statement-to.
        CONCATENATE gv_statement ls_tokens_s-str INTO gv_statement SEPARATED BY space.
      ENDLOOP.
    ENDIF.

    CASE ls_token-str.

      WHEN 'TABLES' OR 'UPDATE' OR 'MODIFY'.
        l_mode = ls_token-str(1).
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        l_table = ls_token-str.
        CALL METHOD append_dbtable
          EXPORTING
            i_table = l_table
            i_mode  = l_mode.

      WHEN 'SELECT'.
        WHILE ls_token-str <> 'FROM'  AND  ls_token-str <> space.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
        ENDWHILE.
        IF ls_token-str = 'FROM'.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
          l_table = ls_token-str.
          CALL METHOD append_dbtable
            EXPORTING
              i_table = l_table
              i_mode  = 'S'.
        ENDIF.
        DO 1000 TIMES.
          WHILE ls_token-str <> 'JOIN'  AND  ls_token-str <> space.
            CALL METHOD get_next_token
              IMPORTING
                es_token = ls_token.
          ENDWHILE.
          IF ls_token-str = 'JOIN'.
            CALL METHOD get_next_token
              IMPORTING
                es_token = ls_token.
            l_table = ls_token-str.
            CALL METHOD append_dbtable
              EXPORTING
                i_table = l_table
                i_mode  = 'S'.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

      WHEN 'INSERT'.
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        IF ls_token-str = 'INTO'.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
        ENDIF.
        l_table = ls_token-str.
        CALL METHOD append_dbtable
          EXPORTING
            i_table = l_table
            i_mode  = 'I'.

      WHEN 'DELETE'.
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        IF ls_token-str = 'FROM'.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
        ENDIF.
        l_table = ls_token-str.
        CALL METHOD append_dbtable
          EXPORTING
            i_table = l_table
            i_mode  = 'D'.
    ENDCASE.

  ENDMETHOD.                    "do_databases


* Performs analysis of continuous code section of comments. If we
* enter several lines of comments, even with balnk lines inside,
* SCAN returns them as one statemnt (each line will be separate token).
* Results are placed in structure and can be used by by other methods.
* Typically, coments are placed BEFORE code we are interested in.
* Comment is stored in LS_LAST_COMMENT atribute.

  METHOD do_comments.

    DATA: ls_token TYPE ts_token,
          ls_tag   TYPE ts_commtag.
    DATA: l_str TYPE string,
          l_buf TYPE string.
    DATA: l_comm_type TYPE c,
          l_char      TYPE c.
    DATA: l_last_row TYPE i,
          l_len      TYPE i,
          l_offs     TYPE i.

    CLEAR: ls_last_comment.
    CALL METHOD get_first_token
      IMPORTING
        es_token = ls_token.
    l_last_row = ls_token-row.

*   Analyse all lines of comment
    WHILE ls_token-str <> space.

      l_len = strlen( ls_token-str ).

*     Process only comments starting with '*' or " in the first column
      IF l_len >= 1  AND  ls_token-str(1) = '"'  AND  ls_token-col > 0.
        CALL METHOD get_next_token
          IMPORTING
            es_token = ls_token.
        CONTINUE.
      ENDIF.

*     Set comment type (program, function or other). It is called for
*     the first line of the comment (also after blank line).
      IF l_comm_type = space.
        IF l_len >= 2  AND  ls_token-str(2) = '*&'.
          l_comm_type = 'P'.      " like program beginning
        ELSEIF l_len >= 2  AND  ls_token-str(2) = '*"'.
          l_comm_type = 'F'.      " like function parameters
        ELSE.
          l_comm_type = 'O'.      " other comments
        ENDIF.
      ENDIF.

*     Check if blank lines appeares between lines of comment. Blank line
*     ends the current part of comment.
      l_offs = ls_token-row - l_last_row.
      l_last_row = ls_token-row.
      IF l_offs > 1.
        IF l_comm_type = 'P'  OR  l_comm_type = 'F'.
          IF ls_tag-text <> space.
            APPEND ls_tag TO ls_last_comment-tags.
          ENDIF.
          CALL METHOD append_comment.
        ENDIF.
*       Starting next paragraph
        CLEAR: ls_tag.
        CLEAR: l_comm_type.
        CONTINUE.
      ENDIF.

*     Ommit semi-lines (strings of the same characters)
      l_len = strlen( ls_token-str ).
      IF l_len >= 6  AND  ls_token-str+3(1) = ls_token-str+4(1)  AND
                          ls_token-str+3(1) = ls_token-str+5(1).
        l_char = ls_token-str+3(1).
        IF l_char  <> space.
          IF ls_tag-text <> space.
            APPEND ls_tag TO ls_last_comment-tags.
          ENDIF.
          CLEAR: ls_tag.
          CALL METHOD get_next_token
            IMPORTING
              es_token = ls_token.
          CONTINUE.
        ENDIF.
      ENDIF.

*     Remove beginning markers
      l_len = strlen( ls_token-str ).
      l_offs = 0.
      WHILE l_offs < l_len  AND  ls_token-str+l_offs(1) CA '*"& '.
        l_offs = l_offs + 1.
      ENDWHILE.
      l_str = ls_token-str+l_offs.

*     Special tags in comments (starting with '@')
      l_len = strlen( l_str ).
      IF l_len > 0.
        IF l_str(1) = '@'.
          IF ls_tag-text <> space.
            APPEND ls_tag TO ls_last_comment-tags.
          ENDIF.
          CLEAR: ls_tag.
          SPLIT l_str+1 AT space INTO ls_tag-tag l_buf.
          TRANSLATE ls_tag-tag TO UPPER CASE.
*         Tags with parameters
          IF ls_tag-tag = 'PARAM'  OR ls_tag-tag = 'TABLES'    OR
             ls_tag-tag = 'USING'  OR
             ls_tag-tag = 'IMPORT' OR ls_tag-tag = 'IMPORTING' OR
             ls_tag-tag = 'EXPORT' OR ls_tag-tag = 'EXPORTING' OR
             ls_tag-tag = 'CHANGE' OR ls_tag-tag = 'CHANGING'  OR
             ls_tag-tag = 'RETURN' OR ls_tag-tag = 'RETURNING' OR
             ls_tag-tag = 'RECEIVING'  OR
             ls_tag-tag = 'EXCEPTION'  OR ls_tag-tag = 'THROWS' OR
             ls_tag-tag = 'RAISING'.
            CLEAR: l_buf.
            SPLIT l_str+1 AT space INTO ls_tag-tag ls_tag-param l_buf.
            TRANSLATE ls_tag-tag TO UPPER CASE.
          ENDIF.
          ls_tag-text = l_buf.
        ELSE.
          CONCATENATE ls_tag-text l_str INTO ls_tag-text
                                        SEPARATED BY space.
          SHIFT ls_tag-text LEFT DELETING LEADING space.
          IF ls_tag-tag = space.
            ls_tag-tag = 'INFO'.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD get_next_token
        IMPORTING
          es_token = ls_token.

    ENDWHILE.

    IF ls_tag-text <> space.
      APPEND ls_tag TO ls_last_comment-tags.
    ENDIF.
    IF l_comm_type = 'P'  OR  l_comm_type = 'F'.
      CALL METHOD append_comment.
    ENDIF.

  ENDMETHOD.                    "do_comments


* Performs analysis of other statements.

  METHOD do_others.

    IF ls_cur_stmnt-type = 'A'.
*      break c00002143.
      CALL METHOD do_classes.
    ENDIF.
  ENDMETHOD.                    "do_others


* Reads token from current statemnt (starting from the next)
* and creates parameter structure.

  METHOD do_parameters.

    DATA: ls_token  TYPE ts_token.
    DATA: ls_detail TYPE ts_param_detail.

    CALL METHOD get_next_token
      IMPORTING
        es_token = ls_token.

    WHILE ls_token-str <> space.

      CONCATENATE es_param-text ls_token-str INTO es_param-text
                                              SEPARATED BY space.

      CALL METHOD get_next_token
        IMPORTING
          es_token = ls_token.

    ENDWHILE.

    SHIFT es_param-text LEFT DELETING LEADING space.

*    SHIFT ls_detail-text LEFT DELETING LEADING space.
*    IF ls_detail IS NOT INITIAL.
*      APPEND ls_detail TO es_param-params.
*    ENDIF.

  ENDMETHOD.                    "do_parameters


* Sets the program name of current statement. Called when
* the following statementr is taken for analysis.

  METHOD set_cur_prog.

    DATA: ls_level TYPE slevel.
    DATA: l_name TYPE programm.

    l_name = ls_cur_prog-name.
    CLEAR: ls_cur_prog.
    LOOP AT lt_levels INTO ls_level.
      IF lv_cur_stmnt_idx >= ls_level-from  AND
         lv_cur_stmnt_idx <= ls_level-to.
        ls_cur_prog = ls_level.
        IF ls_level-level = 0.
          ls_cur_prog-name = gv_main_program_name.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF ls_cur_prog-name <> l_name.
      CLEAR: ls_last_comment.
    ENDIF.

  ENDMETHOD.                    "set_cur_prog_name


* Sets the name and parameters of class/interf definition/implement.
* Called in the beginning of class definiotion/implement (CLASS, ITER..)

  METHOD set_cur_class.

    CLEAR: ls_cur_class.
    ls_cur_class-clsname = i_clsname.
    ls_cur_class-clstype = i_clstype.
    ls_cur_class-super   = i_super.
    ls_cur_class-param   = is_param.
    ls_cur_class-program = ls_cur_prog-name.
    ls_cur_class-comment = ls_last_comment.
    CLEAR: ls_last_comment.

  ENDMETHOD.                    "set_cur_class


* Sets the name and parameters of code unit of current statement.
* Called in the beginning of unit code definiotion (FORM, CLASS, ..., ).

  METHOD set_cur_unit.

    CLEAR: ls_cur_unit.
    ls_cur_unit-name = i_name.
    ls_cur_unit-type = i_type.
    ls_cur_unit-param = is_param.
    ls_cur_unit-program = ls_cur_prog-name.
    ls_cur_unit-clsname = ls_cur_class-clsname.
    ls_cur_unit-comment = ls_last_comment.
    CLEAR: ls_last_comment.

  ENDMETHOD.                    "set_cur_unit


* Appends parameters (declaration) of current class to results table

  METHOD append_cur_class.

    CHECK ls_cur_class-clsname IS NOT INITIAL.
    APPEND ls_cur_class TO gt_classes.

  ENDMETHOD.                    "append_cur_func


* Append class component to result table.

  METHOD append_class_cmp.

    DATA: ls_cmp TYPE ts_classcmp.
    DATA: l_tabix TYPE syst-tabix.

    CHECK is_clscmp-cmpname IS NOT INITIAL.

*   Check if table already contains component. It can be called twice:
*   for definition and implementation. For the first time we populate
*   more fields than for the second.
    READ TABLE gt_classcmps INTO ls_cmp
              WITH KEY clsname = ls_cur_class-clsname
                       clstype = ls_cur_class-clstype
                       cmpname = is_clscmp-cmpname.
*   If already exists - update comment only
    IF sy-subrc = 0.
      l_tabix = sy-tabix.
      APPEND  LINES OF ls_last_comment-tags TO ls_cmp-comment-tags.
      MODIFY gt_classcmps INDEX l_tabix FROM ls_cmp
            TRANSPORTING comment.
*   Append new component
    ELSE.
      ls_cmp = is_clscmp.
      ls_cmp-clsname = ls_cur_class-clsname.
      ls_cmp-clstype = ls_cur_class-clstype.
      ls_cmp-comment = ls_last_comment.
      APPEND ls_cmp TO gt_classcmps.
    ENDIF.
    CLEAR: ls_last_comment.

  ENDMETHOD.                    "append_class_cmp


* Appends parameters of current code unit to results table.

  METHOD append_cur_unit.

    CHECK ls_cur_unit-name IS NOT INITIAL.
    APPEND ls_cur_unit TO gt_modunits.

  ENDMETHOD.                    "append_cur_unit

* Appends information about sub code unit called from current unit
* Called from PERFORM..., CALL... etc.

  METHOD append_sub_unit.

    DATA: ls_unitshier TYPE ts_unitshier.
    ls_unitshier-name1    = ls_cur_unit-name.
    ls_unitshier-type1    = ls_cur_unit-type.
    ls_unitshier-prog1    = ls_cur_unit-program.
    ls_unitshier-clsname1 = ls_cur_class-clsname.
    ls_unitshier-name2    = i_name.
    ls_unitshier-type2    = i_type.
    CHECK ls_unitshier-name1 <> space  AND  ls_unitshier-name2 <> space.
    APPEND ls_unitshier TO lt_uhier.

  ENDMETHOD.                    "append_sub_func


* Appends information about access to database table.

  METHOD append_dbtable.

    DATA: ls_dbtable TYPE ts_dbtable.
    DATA: lv_tabclass TYPE tabclass.

*   If table name is provided dynamically - just store information.
*   If statically - check and get table parameters in DDIC.
    IF i_table(1) <> '('.
*     Check if table exists and get description.
      SELECT SINGLE tabclass INTO (lv_tabclass) FROM dd02l
        WHERE tabname = i_table.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      SELECT SINGLE ddtext INTO (ls_dbtable-ddtext) FROM dd02t
        WHERE tabname = i_table
          AND ddlanguage = sy-langu
          AND as4local = 'A'.
    ENDIF.
    ls_dbtable-table = i_table.
    ls_dbtable-mode = i_mode.
    ls_dbtable-program = ls_cur_prog-name.
    ls_dbtable-clsname = ls_cur_class-clsname.
    ls_dbtable-name = ls_cur_unit-name.
    ls_dbtable-type = ls_cur_unit-type.
    ls_dbtable-statement = gv_statement.
    APPEND ls_dbtable TO gt_dbtables.

  ENDMETHOD.                    "append_dbtable


* Appends last comment to the current program (include). Appends only
* if is not entered yet. It is because comments beginning with *&
* are added by pretty-printer for forms.


  METHOD append_cur_prog_comment.

    DATA: ls_prog TYPE ts_program.
    DATA: l_tabix TYPE sy-tabix.

*   This comment must be outside code unit
    IF ls_cur_unit-name IS INITIAL.
*     Store program comment only if it is empty yet.
      READ TABLE gt_programs INTO ls_prog
                 WITH KEY program = ls_cur_prog-name.
      IF sy-subrc = 0  AND  ls_prog-comment IS INITIAL.
        l_tabix = sy-tabix.
        ls_prog-comment = ls_last_comment.
        MODIFY gt_programs FROM ls_prog INDEX l_tabix.
        CLEAR: ls_last_comment.       " only if appended
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "append_prog_comment


* Appends last comment to the current class (general level).

  METHOD append_cur_class_comment.

    DATA: ls_class TYPE ts_class.
    DATA: l_tabix TYPE sy-tabix.

    READ TABLE gt_classes INTO ls_class
               WITH KEY clsname = ls_cur_class-clsname
                        clstype = ls_cur_class-clstype.
    IF sy-subrc = 0.
      l_tabix = sy-tabix.
      IF ls_class-comment IS INITIAL.
        ls_class-comment = ls_last_comment.
      ELSE.
        APPEND LINES OF ls_last_comment-tags TO ls_class-comment-tags.
      ENDIF.
      MODIFY gt_classes FROM ls_class INDEX l_tabix.
    ENDIF.
    CLEAR: ls_last_comment.

  ENDMETHOD.                    "append_prog_comment


* Appends last comment to the current code unit.

  METHOD append_cur_unit_comment.

    DATA: ls_unit TYPE ts_modunit.
    DATA: l_tabix TYPE sy-tabix.

    READ TABLE gt_modunits INTO ls_unit
               WITH KEY name = ls_cur_unit-name
                        type = ls_cur_unit-type.
    IF sy-subrc = 0.
      l_tabix = sy-tabix.
      IF ls_unit-comment IS INITIAL.
        ls_unit-comment = ls_last_comment.
      ELSE.
        APPEND LINES OF ls_last_comment-tags TO ls_unit-comment-tags.
      ENDIF.
      MODIFY gt_modunits FROM ls_unit INDEX l_tabix.
    ENDIF.
    CLEAR: ls_last_comment.

  ENDMETHOD.                    "append_cur_unit_comment


* Appends comment to right object - depends, if it is inside form or
* class or outside them

  METHOD append_comment.

*   If inside code unit, comment is added to this unit
    IF ls_cur_unit-name IS NOT INITIAL.
      CALL METHOD append_cur_unit_comment.
*   If inside class definition/implementation (but outside method)
    ELSEIF ls_cur_class-clsname IS NOT INITIAL.
      CALL METHOD append_cur_class_comment.
*   In other cases add to program's comment
    ELSEIF ls_cur_prog-name IS NOT INITIAL.
      CALL METHOD append_cur_prog_comment.
    ENDIF.

  ENDMETHOD.                    "append_comment


* Converts program levels (raw results of SCAN ABAP-CODE) into more
* suitable format of global table GT_PROGRAMS

  METHOD levels_to_programms.

    DATA: ls_level TYPE slevel,
          ls_prog  TYPE ts_program.

*   Convert levels into GT_PROGRAMS
    LOOP AT lt_levels INTO ls_level.

      CLEAR: ls_prog.
      ls_prog-depth   = ls_level-depth.
      ls_prog-program = ls_level-name.
      IF ls_level-level = 0.
        ls_prog-program = gv_main_program_name.
      ENDIF.

*     For macros
      IF ls_level-type = 'D'.
        ls_prog-subc = '9'.                         " non-standard
        ls_prog-title = 'User macro'.
*       Each call of macro is placed in the table, so we maust check
*       if already appended
        READ TABLE gt_programs TRANSPORTING NO FIELDS WITH KEY
                      program = ls_prog-program
                      depth   = ls_prog-depth.
        CHECK sy-subrc <> 0.

*     Main program, include (type = P)
      ELSE.
*       Get program type (1, ...)
        SELECT SINGLE subc INTO (ls_prog-subc) FROM trdir
          WHERE name = ls_prog-program.
*       Title of the program is determined from program's properties
*       in logon language, or in EN/DE or in any available.
        SELECT SINGLE text INTO (ls_prog-title) FROM trdirt
          WHERE name = ls_prog-program
            AND sprsl = sy-langu.
        IF sy-subrc <> 0.
          SELECT SINGLE text INTO (ls_prog-title) FROM trdirt
            WHERE name = ls_prog-program
              AND sprsl IN ('E', 'D').
          IF sy-subrc <> 0.
            SELECT SINGLE text INTO (ls_prog-title) FROM trdirt
              WHERE name = ls_prog-program.
          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_prog TO gt_programs.
    ENDLOOP.

  ENDMETHOD.                    "levels_to_programms


* Converts results code analysis (hierarchy of code units calls)
* into format more suitable for further processing.

  METHOD rework_units_hier.

    DATA: ls_hier TYPE ts_unitshier,
          ls_prog TYPE ts_program,
          ls_unit TYPE ts_modunit.

*   If main program is an executable (type 1) program we will start
*   with events in main program
    READ TABLE gt_programs INTO ls_prog INDEX 1.
    IF ls_prog-subc = '1'.
      LOOP AT gt_modunits INTO ls_unit WHERE program = ls_prog-program
                                         AND type = 'E'.
        CLEAR: ls_hier.
        ls_hier-name2 = ls_unit-name.
        ls_hier-type2 = ls_unit-type.
        ls_hier-prog1 = ls_unit-program.
        ls_hier-clsname1 = ls_unit-clsname.
        ls_hier-depth = '1'.
        CALL METHOD do_hier_path( ls_hier ).
      ENDLOOP.

*   For other program types, we go through all programs and build
*   tree for every unit which has no caller
    ELSE.
      LOOP AT gt_programs INTO ls_prog.
        LOOP AT gt_modunits INTO ls_unit
                            WHERE program = ls_prog-program.
*         Check if it has caller
          READ TABLE lt_uhier WITH KEY name2 = ls_unit-name
                                       type2 = ls_unit-type
                              TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            CLEAR: ls_hier.
            ls_hier-name2 = ls_unit-name.
            ls_hier-type2 = ls_unit-type.
            ls_hier-prog1 = ls_unit-program.
            ls_hier-clsname1 = ls_unit-clsname.
            ls_hier-depth = '1'.
            CALL METHOD do_hier_path( ls_hier ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "rework_units_hier


* This method does not work for calls cycles (a->b and b->a)

  METHOD do_hier_path.  " importing is_start type ts_unitshier

    DATA: ls_hier TYPE ts_unitshier.

    APPEND is_start TO gt_unitshier.
    LOOP AT lt_uhier INTO ls_hier WHERE name1 = is_start-name2
                                    AND type1 = is_start-type2.
      ls_hier-depth = is_start-depth + 1.
      CALL METHOD do_hier_path( ls_hier ).
    ENDLOOP.

  ENDMETHOD.                    "do_hier_path


*======================================================================

* Gets the first statement of code. Statement is a part of code ended
* with dot.

  METHOD get_first_statement.

    CLEAR: lv_cur_stmnt_idx.
    CALL METHOD get_next_statement
      IMPORTING
        es_stmnt = es_stmnt.

  ENDMETHOD.                    "get_first_statement


* Gets the next statement of code. In case of error (no more
* statements), returns empty statement (you can check field TYPE).

  METHOD get_next_statement.

    DATA: l_cnt TYPE i.
    CLEAR: ls_cur_stmnt, ls_cur_token,
           lv_cur_token_idx.

    DESCRIBE TABLE lt_statements LINES l_cnt.
    IF lv_cur_stmnt_idx <= l_cnt.             " pozwalamy wyjsc idx
      lv_cur_stmnt_idx = lv_cur_stmnt_idx + 1.
    ENDIF.
    IF lv_cur_stmnt_idx > 0  AND  lv_cur_stmnt_idx <= l_cnt.
      READ TABLE lt_statements INTO ls_cur_stmnt INDEX lv_cur_stmnt_idx.
      CALL METHOD set_cur_prog.
    ENDIF.
    es_stmnt = ls_cur_stmnt.

  ENDMETHOD.                    "get_next_statement


* Returns again current statement.

  METHOD get_cur_statement.
    es_stmnt = ls_cur_stmnt.
  ENDMETHOD.                    "get_cur_statement


* Gets the first token (keyword, operator) in the current statement.

  METHOD get_first_token.

    CLEAR: lv_cur_token_idx.
    CALL METHOD get_next_token
      IMPORTING
        es_token = es_token.

  ENDMETHOD.                    "get_first_token


* Gets the next token of current statemnt. In case of error (no more
* tokens), returns empty token (you can check field STR or TYPE).

  METHOD get_next_token.

    CLEAR: ls_cur_token.
    IF lv_cur_token_idx < ls_cur_stmnt-from.
      lv_cur_token_idx = ls_cur_stmnt-from.
    ELSEIF lv_cur_token_idx <= ls_cur_stmnt-to.
      lv_cur_token_idx = lv_cur_token_idx + 1.
    ENDIF.
    IF lv_cur_token_idx >= ls_cur_stmnt-from  AND
       lv_cur_token_idx <= ls_cur_stmnt-to.
      READ TABLE lt_tokens INTO ls_cur_token INDEX lv_cur_token_idx.
    ENDIF.
    es_token = ls_cur_token.

  ENDMETHOD.                    "get_next_token


* Returns again current token of current statemnt.

  METHOD get_cur_token.
    es_token = ls_cur_token.
  ENDMETHOD.                    "get_cur_token


*======================================================================
* The followinh methods are used for test purposes - they dump
* to the screen results of SCAN ABAP-CODE.

* Displays all statements and tokens with details (type, level, posit.).

  METHOD dump_statements_detailed.

    DATA: l_stmnt TYPE sstmnt,
          l_token TYPE ts_token.

    WRITE:/ 'Program:', gv_main_program_name.
    WRITE: / '==================================================='.
    WRITE: / 'S T A T E M E N T S   D E T A I L S'.
    WRITE: / '==================================================='.

    CALL METHOD get_first_statement
      IMPORTING
        es_stmnt = l_stmnt.
    WHILE l_stmnt-type <> space.
      WRITE:/ 'Statement idx=', lv_cur_stmnt_idx, 'Type=', l_stmnt-type.
      CALL METHOD get_first_token
        IMPORTING
          es_token = l_token.
      WHILE l_token-type <> space.
        WRITE:/ '       Token idx=', lv_cur_token_idx,
                'Type=', l_token-type, 'Str=', l_token-str.
        CALL METHOD get_next_token
          IMPORTING
            es_token = l_token.
      ENDWHILE.
      CALL METHOD get_next_statement
        IMPORTING
          es_stmnt = l_stmnt.
    ENDWHILE.

  ENDMETHOD.                    "dump_statements_detailed


* Displays statements as lines of code (one code = one line).

  METHOD dump_statements.

    DATA: l_stmnt TYPE sstmnt,
          l_token TYPE ts_token.

    WRITE:/ 'Program:', gv_main_program_name.
    WRITE: / '==================================================='.
    WRITE: / 'S T A T E M E N T S'.
    WRITE: / '==================================================='.

    CALL METHOD get_first_statement
      IMPORTING
        es_stmnt = l_stmnt.
    WHILE l_stmnt-type <> space.

      WRITE:/ '===>'.
      CALL METHOD get_first_token
        IMPORTING
          es_token = l_token.
      WHILE l_token-type <> space.
        WRITE: l_token-str.
        CALL METHOD get_next_token
          IMPORTING
            es_token = l_token.
      ENDWHILE.
      CALL METHOD get_next_statement
        IMPORTING
          es_stmnt = l_stmnt.
    ENDWHILE.

  ENDMETHOD.                    "dump_statements


* Displays content of tables returned by SCAN ABAP-CODE.

  METHOD dump_tables_scan.

    DATA: ls_srccode    TYPE string,
          ls_tokens     TYPE ts_token,
          ls_statements TYPE sstmnt,
          ls_levels     TYPE slevel.

    WRITE:/ 'Program:', gv_main_program_name.

    WRITE: / '==================================================='.
    WRITE: / 'T O K E N S'.
    WRITE:/ '       Lp.  TYPE    ROW       OFF2       OFF3',
            '  COL  LEN1  LEN2  LEN3  STR'.
    WRITE: / '==================================================='.

    LOOP AT lt_tokens INTO ls_tokens.
      WRITE:/ sy-tabix,
              ls_tokens-type, ls_tokens-row, ls_tokens-off2,
              ls_tokens-off3, ls_tokens-col,
              ls_tokens-len1, ls_tokens-len2, ls_tokens-len3,
              ls_tokens-str.
    ENDLOOP.

    WRITE: / '==================================================='.
    WRITE: / 'S T A T E M E N T S'.
    WRITE: / '       Lp.      LEVEL      STRUC       FROM         TO',
             '    NUMBER   COLONROW       TROW COCOL  TCOL PRLEN',
             ' TYPE.TERM ENHMT'.
    WRITE: / '==================================================='.
    LOOP AT lt_statements INTO ls_statements.
      WRITE:/ sy-tabix,
              ls_statements-level, ls_statements-struc,
              ls_statements-from,
              ls_statements-to, ls_statements-number,
              ls_statements-colonrow, ls_statements-trow,
              ls_statements-coloncol, ls_statements-tcol,
              ls_statements-prefixlen, ls_statements-type,
              ls_statements-terminator.      ", ls_statements-enhmt.
    ENDLOOP.

    WRITE: / '==================================================='.
    WRITE: / 'L E V E L S'.
    WRITE: / '       Lp.      DEPTH      LEVEL      STMNT',
             '      FROM         TO  TYPE NAME'.
    WRITE: / '==================================================='.
    LOOP AT lt_levels INTO ls_levels.
      WRITE:/ sy-tabix,
              ls_levels-depth, ls_levels-level, ls_levels-stmnt,
              ls_levels-from, ls_levels-to,
              ls_levels-type, ls_levels-name.
    ENDLOOP.

*    WRITE: / '==================================================='.
*    WRITE: / 'S T R U C T U R E S'.
*    WRITE: / '       Lp.  TYPE  STMNT_TYPE   K_START    KEY_END',
*             '   STMNT_FROM   STMNT_TO STRUC_FROM   STRUC_TO',
*             '      BACK'.
*    WRITE: / '==================================================='.
*    LOOP AT gt_structures.
*      WRITE:/ sy-tabix,
*              gt_structures-type, '        ',
*              gt_structures-stmnt_type, '        ',
*              gt_structures-key_start, '        ',
*              gt_structures-key_end, '      ',
*              gt_structures-stmnt_from, gt_structures-stmnt_to,
*              gt_structures-struc_from, gt_structures-struc_to,
*              gt_structures-back.
*    ENDLOOP.

  ENDMETHOD.                    "dump_tables_scan

ENDCLASS.                    "cl_ic_code_analysis IMPLEMENTATION
