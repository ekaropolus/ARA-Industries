REPORT zmm_003.

TABLES: sflight.

*PARAMETERS: p_connid TYPE bukrs.
SELECT-OPTIONS s_connid FOR bkpf-bukrs.


DATA: gt_outtab TYPE TABLE OF bkpf.
DATA: gr_table  TYPE REF TO cl_salv_table.

*... Select data
SELECT * FROM bkpf INTO CORRESPONDING FIELDS OF TABLE gt_outtab
WHERE bukrs IN s_connid.

*... Create Instance
CALL METHOD cl_salv_table=>factory
  IMPORTING
    r_salv_table = gr_table
  CHANGING
    t_table      = gt_outtab.


*... Display table
gr_table->display( ).
