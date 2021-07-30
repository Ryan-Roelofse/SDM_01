*&---------------------------------------------------------------------*
*&  Include           /GDA/SDM_MM_POE_ART_ERRS_POST
*&---------------------------------------------------------------------*
data: begin of fehlerliste_buffer occurs 0.
        include structure fehlertext.
data: end   of fehlerliste_buffer.

data: additional_line like line of fehlerliste_buffer.


loop at fehlerliste assigning field-symbol(<error_on_posting>) from 3.
  if <error_on_posting>-text_zeile cs '/GDA/SDM_ART'.
    if <error_on_posting>-text_zeile cs '(Basic Data)'   or
       <error_on_posting>-text_zeile cs '(Listing)'      or
       <error_on_posting>-text_zeile cs '(Basic Data 2)' or
       <error_on_posting>-text_zeile cs '(Acc)'          or
       <error_on_posting>-text_zeile cs '(PIR)'.

      concatenate 'Material : ' <error_on_posting>-msgv1 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      if <error_on_posting>-msgv1 is not INITIAL.
      replace all occurrences of <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.
      endif.
      clear <error_on_posting>-msgv1.
      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

 if <error_on_posting>-text_zeile cs '(Sales)'.
   if <error_on_posting>-msgv2 is INITIAL.
      concatenate 'Material : ' <error_on_posting>-msgv1 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      if <error_on_posting>-msgv1 is not initial.
      replace all occurrences of <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.
      clear <error_on_posting>-msgv1.
      endif.
      append <error_on_posting> to fehlerliste_buffer.
      continue.

    endif.
   if <error_on_posting>-msgv1 is not INITIAL and <error_on_posting>-msgv2 is not INITIAL.
      concatenate 'Sales Org: ' <error_on_posting>-msgv1 'Distribution Channel: ' <error_on_posting>-msgv2 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      replace all occurrences of <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.
      clear <error_on_posting>-msgv1.
      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

 endif.

    if <error_on_posting>-text_zeile cs '(Purchasing)'.
* Vendor Purchase Org
      concatenate 'Vendor : ' <error_on_posting>-msgv2 'Purchase Org : ' <error_on_posting>-msgv3
      into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      if <error_on_posting>-msgv2 is not initial.
        replace all occurrences of substring <error_on_posting>-msgv2 in <error_on_posting>-text_zeile with space.
      endif.
      if <error_on_posting>-msgv3 is not initial.
        replace all occurrences of substring <error_on_posting>-msgv3 in <error_on_posting>-text_zeile with space.
      endif.
      clear:
        <error_on_posting>-msgv1,
        <error_on_posting>-msgv2,
        <error_on_posting>-msgv3.

      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

    if <error_on_posting>-text_zeile cs '(Logistics DC)' or
       <error_on_posting>-text_zeile cs '(MRP)'   or
       <error_on_posting>-text_zeile cs '(DC QM)' or
       <error_on_posting>-text_zeile cs '(Tariffs)'.

* DC
      shift <error_on_posting>-msgv1 left deleting leading space.
      concatenate 'Distribution Centre : ' <error_on_posting>-msgv1 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      replace all occurrences of substring <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.

      clear:
        <error_on_posting>-msgv1,
        <error_on_posting>-msgv2,
        <error_on_posting>-msgv3.

      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

    if <error_on_posting>-text_zeile cs '(Store Logistics)'.

* Store
      shift <error_on_posting>-msgv1 left deleting leading space.
      concatenate 'Store : ' <error_on_posting>-msgv1 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      replace all occurrences of substring <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.

      clear:
        <error_on_posting>-msgv1,
        <error_on_posting>-msgv2,
        <error_on_posting>-msgv3.

      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

* POS
    if <error_on_posting>-text_zeile cs '(POS)'.
      shift <error_on_posting>-msgv1 left deleting leading space.
      concatenate 'Sales Org: ' <error_on_posting>-msgv1 'Distribution Channel: ' <error_on_posting>-msgv2 into additional_line-text_zeile.
      append additional_line to fehlerliste_buffer.

      if <error_on_posting>-msgv1 is not initial.
      replace all occurrences of substring <error_on_posting>-msgv1 in <error_on_posting>-text_zeile with space.
      endif.
      if <error_on_posting>-msgv2 is not initial.
        replace all occurrences of substring <error_on_posting>-msgv2 in <error_on_posting>-text_zeile with space.
      endif.
      clear:
        <error_on_posting>-msgv1,
        <error_on_posting>-msgv2,
        <error_on_posting>-msgv3.

      append <error_on_posting> to fehlerliste_buffer.
      continue.
    endif.

    concatenate 'Validity Undefined : ' <error_on_posting>-msgv1 into additional_line-text_zeile.
    append additional_line to fehlerliste_buffer.
    clear <error_on_posting>-msgv1.
    append <error_on_posting> to fehlerliste_buffer.

  else.
    delete fehlerliste index sy-tabix.
  endif.

endloop.

fehlerliste[] = fehlerliste_buffer[].
