REPORT zmm_003.

TABLES: sflight.

*PARAMETERS: p_connid TYPE s_conn_id.
SELECT-OPTIONS s_connid FOR sflight-connid.


DATA: gt_outtab TYPE TABLE OF sflight.
DATA: gr_table  TYPE REF TO cl_salv_table.

*... Select data
SELECT * FROM sflight INTO CORRESPONDING FIELDS OF TABLE gt_outtab
WHERE connid IN s_connid.

*... Create Instance
CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = gr_table
  CHANGING
    t_table      = gt_outtab.


*... Display table
gr_table->display( ).
