*&---------------------------------------------------------------------*
*& Report  ZICDOC
*&
*&---------------------------------------------------------------------*
* @Author Ireneusz Cwir
* The main program of ZICDOC generator. It shows also how to call
* program analysis and HTML file generation from other code.

REPORT  zicdoc LINE-SIZE 256.

DATA gv_statement TYPE string.

* These texts are defined in properties of the program.
* You can uncomment them (optionally) and copy to right fields.
*
* Title (Goto -> Attributes)
* ZICDOC - ABAP documentation generator
*
* Selection parametrs (Goto -> Text elements -> Selection texts):
*P_CCPRIV	Private class components
*P_CCPROT	Protected class components
*P_CLSCMP	Class components
*P_FHTML  Output file name (html)
*P_FSRC	Cource code file name
*P_HTMLCM	Comments include HTML tags
*P_INCDET	INCLUDES - details
*P_INCHIE	INCLUDES - hierarchy
*P_PROG	Analysed program name
*P_TABLES	Database tables
*P_UNTDET	Non-class code units
*P_UNTHIE	Code units - hierarchy

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_prog LIKE trdir-name DEFAULT 'ZICDOC_TEST1',
            p_fsrc LIKE rlgrap-filename DEFAULT 'd:\Z22IC002.txt'.
*SELECTION-SCREEN SKIP 1.
PARAMETERS: p_fhtml LIKE rlgrap-filename DEFAULT 'D:\ZICDOC_TEST1.html'.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: p_inchie AS CHECKBOX DEFAULT 'X',
            p_incdet AS CHECKBOX DEFAULT 'X',
            p_tables AS CHECKBOX DEFAULT 'X',
            p_clscmp AS CHECKBOX DEFAULT 'X',
            p_untdet AS CHECKBOX DEFAULT 'X',
            p_unthie AS CHECKBOX DEFAULT 'X'.

*SELECTION-SCREEN SKIP 1.
PARAMETERS: p_ccprot AS CHECKBOX DEFAULT 'X',
            p_ccpriv AS CHECKBOX DEFAULT 'X'.

*SELECTION-SCREEN SKIP 1.
PARAMETERS: p_htmlcm AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------

* Include classe for code analysis and HTML file generation
INCLUDE zicdoc_cl_code_analysis_html.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fhtml.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_fhtml.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fsrc.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_fsrc.



*----------------------------------------------------------------------
START-OF-SELECTION.

*----------------------------------------------------------------------
* Reference to the main objectof analysis.
  DATA: ref_html TYPE REF TO zcl_ic_code_analysis_html.

* Additional data
  DATA: gv_str TYPE string.
  DATA: gv_retcode TYPE i.

* Create object for code analysis
  CREATE OBJECT ref_html.

* Depending on parameters entered on selection screen, call method for
* anlysing code of program in the system or code uploaded
* from local file (from presentation server or PC).
  IF p_prog IS NOT INITIAL.

    gv_retcode = ref_html->analyse_program( p_prog ).

  ELSEIF p_fsrc IS NOT INITIAL.

    gv_str = p_fsrc.
    gv_retcode = ref_html->analyse_file( gv_str ).

  ELSE.

    WRITE:/ 'No program specified for analysis!'.
    EXIT.

  ENDIF.

  IF gv_retcode <> 0.
    WRITE:/ 'Scan ERROR:', gv_retcode.
  ENDIF.
  CHECK gv_retcode = 0.

* Type conversion (output file name)
  gv_str = p_fhtml.

* Create HTML file with documentation.
  CALL METHOD ref_html->write_html_file
    EXPORTING
      i_filename                    = gv_str
      i_write_includes_hier         = p_inchie
      i_write_includes_details      = p_incdet
      i_write_class_components      = p_clscmp
      i_write_class_components_prot = p_ccprot
      i_write_class_components_priv = p_ccpriv
      i_write_code_units_hier       = p_unthie
      i_write_code_units_details    = p_untdet
      i_write_tables                = p_tables
      i_html_comments               = p_htmlcm
    RECEIVING
      ret_code                      = gv_retcode.


* Helper function for displaying tables with results of code scanning.
* Uncomment them if you are interested in further development of ZICDOC
* and you want to understand details of the code analysis.
*  CALL METHOD ref_html->dump_statements.
*  CALL METHOD ref_html->dump_statements_detailed.
*  CALL METHOD ref_html->dump_tables_scan.
