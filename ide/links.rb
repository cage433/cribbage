#!/usr/bin/ruby -w
$map = {"DECLARATION" => "d_declar.htm", \
"DYNAMIC-EXTENT" => "d_dynami.htm", \
"FTYPE" => "d_ftype.htm", \
"IGNORE" => "d_ignore.htm", \
"IGNORABLE" => "d_ignore.htm", \
"INLINE" => "d_inline.htm", \
"NOTINLINE" => "d_inline.htm", \
"OPTIMIZE" => "d_optimi.htm", \
"SPECIAL" => "d_specia.htm", \
"TYPE" => "d_type.htm", \
"ARITHMETIC-ERROR" => "e_arithm.htm", \
"CELL-ERROR" => "e_cell_e.htm", \
"CONDITION" => "e_cnd.htm", \
"CONTROL-ERROR" => "e_contro.htm", \
"DIVISION-BY-ZERO" => "e_divisi.htm", \
"END-OF-FILE" => "e_end_of.htm", \
"ERROR" => "e_error.htm", \
"FILE-ERROR" => "e_file_e.htm", \
"FLOATING-POINT-INEXACT" => "e_floa_1.htm", \
"FLOATING-POINT-OVERFLOW" => "e_floa_2.htm", \
"FLOATING-POINT-UNDERFLOW" => "e_floa_3.htm", \
"FLOATING-POINT-INVALID-OPERATION" => "e_floati.htm", \
"PARSE-ERROR" => "e_parse_.htm", \
"PACKAGE-ERROR" => "e_pkg_er.htm", \
"PRINT-NOT-READABLE" => "e_pr_not.htm", \
"PROGRAM-ERROR" => "e_progra.htm", \
"READER-ERROR" => "e_rder_e.htm", \
"SERIOUS-CONDITION" => "e_seriou.htm", \
"SIMPLE-CONDITION" => "e_smp_cn.htm", \
"SIMPLE-ERROR" => "e_smp_er.htm", \
"SIMPLE-TYPE-ERROR" => "e_smp_tp.htm", \
"SIMPLE-WARNING" => "e_smp_wa.htm", \
"STREAM-ERROR" => "e_stm_er.htm", \
"STORAGE-CONDITION" => "e_storag.htm", \
"STYLE-WARNING" => "e_style_.htm", \
"TYPE-ERROR" => "e_tp_err.htm", \
"UNBOUND-VARIABLE" => "e_unbo_1.htm", \
"UNBOUND-SLOT" => "e_unboun.htm", \
"UNDEFINED-FUNCTION" => "e_undefi.htm", \
"WARNING" => "e_warnin.htm", \
"1+" => "f_1pl_1_.htm", \
"1-" => "f_1pl_1_.htm", \
"ABORT" => "f_abortc.htm", \
"CONTINUE" => "f_abortc.htm", \
"MUFFLE-WARNING" => "f_abortc.htm", \
"ABS" => "f_abs.htm", \
"ACONS" => "f_acons.htm", \
"Generic" => "Function ADD-METHOD f_add_me.htm", \
"ADJOIN" => "f_adjoin.htm", \
"ADJUSTABLE-ARRAY-P" => "f_adju_1.htm", \
"ADJUST-ARRAY" => "f_adjust.htm", \
"Generic" => "Function ALLOCATE-INSTANCE f_alloca.htm", \
"ALPHA-CHAR-P" => "f_alpha_.htm", \
"ALPHANUMERICP" => "f_alphan.htm", \
"APPEND" => "f_append.htm", \
"APPLY" => "f_apply.htm", \
"APROPOS" => "f_apropo.htm", \
"APROPOS-LIST" => "f_apropo.htm", \
"ARRAY-DIMENSIONS" => "f_ar_d_1.htm", \
"ARRAY-DIMENSION" => "f_ar_dim.htm", \
"ARRAY-DISPLACEMENT" => "f_ar_dis.htm", \
"AREF" => "f_aref.htm", \
"ARRAY-ELEMENT-TYPE" => "f_ar_ele.htm", \
"ARRAY-HAS-FILL-POINTER-P" => "f_ar_has.htm", \
"ARRAY-IN-BOUNDS-P" => "f_ar_in_.htm", \
"ARITHMETIC-ERROR-OPERANDS" => "f_arithm.htm", \
"ARRAY-RANK" => "f_ar_ran.htm", \
"ARRAYP" => "f_arrayp.htm", \
"ARRAY-ROW-MAJOR-INDEX" => "f_ar_row.htm", \
"ARRAY-TOTAL-SIZE" => "f_ar_tot.htm", \
"ASH" => "f_ash.htm", \
"ASIN" => "f_asin_.htm", \
"ACOS" => "f_asin_.htm", \
"ATAN" => "f_asin_.htm", \
"ASSOC" => "f_assocc.htm", \
"ASSOC-IF" => "f_assocc.htm", \
"ASSOC-IF-NOT" => "f_assocc.htm", \
"ATOM" => "f_atom.htm", \
"BOOLE" => "f_boole.htm", \
"BOUNDP" => "f_boundp.htm", \
"BREAK" => "f_break.htm", \
"BROADCAST-STREAM-STREAMS" => "f_broadc.htm", \
"BIT-AND" => "f_bt_and.htm", \
"BIT-ANDC1" => "f_bt_and.htm", \
"BIT-ANDC2" => "f_bt_and.htm", \
"BIT" => "f_bt_sb.htm", \
"SBIT" => "f_bt_sb.htm", \
"BIT-VECTOR-P" => "f_bt_vec.htm", \
"BUTLAST" => "f_butlas.htm", \
"NBUTLAST" => "f_butlas.htm", \
"BYTE" => "f_by_by.htm", \
"BYTE-SIZE" => "f_by_by.htm", \
"BYTE-POSITION" => "f_by_by.htm", \
"Function" => "CALL-NEXT-METHOD f_call_n.htm", \
"CAR" => "f_car_c.htm", \
"CDR" => "f_car_c.htm", \
"CAAR" => "f_car_c.htm", \
"CADR" => "f_car_c.htm", \
"CDAR" => "f_car_c.htm", \
"CELL-ERROR-NAME" => "f_cell_e.htm", \
"CERROR" => "f_cerror.htm", \
"CHAR-CODE" => "f_char_c.htm", \
"CHAR=" => "f_chareq.htm", \
"CHAR/=" => "f_chareq.htm", \
"CHAR&lt" => "f_chareq.htm", \
"CHAR&gt" => "f_chareq.htm", \
"CHAR" => "f_char_.htm", \
"SCHAR" => "f_char_.htm", \
"CHAR-INT" => "f_char_i.htm", \
"CHAR-NAME" => "f_char_n.htm", \
"CHAR-UPCASE" => "f_char_u.htm", \
"CHAR-DOWNCASE" => "f_char_u.htm", \
"Generic" => "Function CHANGE-CLASS f_chg_cl.htm", \
"CHARACTER" => "f_ch.htm", \
"CHARACTERP" => "f_chp.htm", \
"CIS" => "f_cis.htm", \
"CLASS-OF" => "f_clas_1.htm", \
"Generic" => "Function CLASS-NAME f_class_.htm", \
"CLEAR-INPUT" => "f_clear_.htm", \
"CLOSE" => "f_close.htm", \
"CLRHASH" => "f_clrhas.htm", \
"COMPILE-FILE-PATHNAME" => "f_cmp__1.htm", \
"COMPILED-FUNCTION-P" => "f_cmpd_f.htm", \
"COMPILE-FILE" => "f_cmp_fi.htm", \
"COMPILE" => "f_cmp.htm", \
"COMPILER-MACRO-FUNCTION" => "f_cmp_ma.htm", \
"CODE-CHAR" => "f_code_c.htm", \
"COERCE" => "f_coerce.htm", \
"COMPUTE-RESTARTS" => "f_comp_1.htm", \
"COMPLEX" => "f_comp_2.htm", \
"COMPLEXP" => "f_comp_3.htm", \
"COMPLEMENT" => "f_comple.htm", \
"Generic" => "Function COMPUTE-APPLICABLE-METHODS f_comput.htm", \
"CONCATENATED-STREAM-STREAMS" => "f_conc_1.htm", \
"CONCATENATE" => "f_concat.htm", \
"CONJUGATE" => "f_conjug.htm", \
"CONSTANTLY" => "f_cons_1.htm", \
"CONS" => "f_cons.htm", \
"CONSP" => "f_consp.htm", \
"CONSTANTP" => "f_consta.htm", \
"COUNT" => "f_countc.htm", \
"COUNT-IF" => "f_countc.htm", \
"COUNT-IF-NOT" => "f_countc.htm", \
"COPY-ALIST" => "f_cp_ali.htm", \
"COPY-LIST" => "f_cp_lis.htm", \
"COPY-PPRINT-DISPATCH" => "f_cp_ppr.htm", \
"COPY-READTABLE" => "f_cp_rdt.htm", \
"COPY-SEQ" => "f_cp_seq.htm", \
"COPY-STRUCTURE" => "f_cp_stu.htm", \
"COPY-SYMBOL" => "f_cp_sym.htm", \
"COPY-TREE" => "f_cp_tre.htm", \
"DECODE-FLOAT" => "f_dec_fl.htm", \
"SCALE-FLOAT" => "f_dec_fl.htm", \
"DECODE-UNIVERSAL-TIME" => "f_dec_un.htm", \
"DELETE-FILE" => "f_del_fi.htm", \
"DELETE-PACKAGE" => "f_del_pk.htm", \
"DEPOSIT-FIELD" => "f_deposi.htm", \
"Generic" => "Function DESCRIBE-OBJECT f_desc_1.htm", \
"DESCRIBE" => "f_descri.htm", \
"DIGIT-CHAR-P" => "f_digi_1.htm", \
"DIGIT-CHAR" => "f_digit_.htm", \
"DIRECTORY" => "f_dir.htm", \
"DISASSEMBLE" => "f_disass.htm", \
"Generic" => "Function DOCUMENTATION f_docume.htm", \
"DPB" => "f_dpb.htm", \
"DRIBBLE" => "f_dribbl.htm", \
"ECHO-STREAM-INPUT-STREAM" => "f_echo_s.htm", \
"ED" => "f_ed.htm", \
"ELT" => "f_elt.htm", \
"ENCODE-UNIVERSAL-TIME" => "f_encode.htm", \
"ENDP" => "f_endp.htm", \
"ENSURE-DIRECTORIES-EXIST" => "f_ensu_1.htm", \
"ENSURE-GENERIC-FUNCTION" => "f_ensure.htm", \
"EQ" => "f_eq.htm", \
"EQL" => "f_eql.htm", \
"EQUAL" => "f_equal.htm", \
"EQUALP" => "f_equalp.htm", \
"ERROR" => "f_error.htm", \
"EVAL" => "f_eval.htm", \
"EVENP" => "f_evenpc.htm", \
"ODDP" => "f_evenpc.htm", \
"EVERY" => "f_everyc.htm", \
"SOME" => "f_everyc.htm", \
"NOTEVERY" => "f_everyc.htm", \
"NOTANY" => "f_everyc.htm", \
"EXP" => "f_exp_e.htm", \
"EXPT" => "f_exp_e.htm", \
"EXPORT" => "f_export.htm", \
"FBOUNDP" => "f_fbound.htm", \
"FDEFINITION" => "f_fdefin.htm", \
"FILE-AUTHOR" => "f_file_a.htm", \
"FILE-ERROR-PATHNAME" => "f_file_e.htm", \
"FILE-LENGTH" => "f_file_l.htm", \
"FILE-POSITION" => "f_file_p.htm", \
"FILE-STRING-LENGTH" => "f_file_s.htm", \
"FILE-WRITE-DATE" => "f_file_w.htm", \
"FILL" => "f_fill.htm", \
"FILL-POINTER" => "f_fill_p.htm", \
"FIND-ALL-SYMBOLS" => "f_find_a.htm", \
"FIND-CLASS" => "f_find_c.htm", \
"FIND" => "f_find_.htm", \
"FIND-IF" => "f_find_.htm", \
"FIND-IF-NOT" => "f_find_.htm", \
"Generic" => "Function FIND-METHOD f_find_m.htm", \
"FIND-PACKAGE" => "f_find_p.htm", \
"FIND-RESTART" => "f_find_r.htm", \
"FIND-SYMBOL" => "f_find_s.htm", \
"FINISH-OUTPUT" => "f_finish.htm", \
"FORCE-OUTPUT" => "f_finish.htm", \
"FIRST" => "f_firstc.htm", \
"SECOND" => "f_firstc.htm", \
"THIRD" => "f_firstc.htm", \
"FOURTH" => "f_firstc.htm", \
"FLOAT" => "f_float.htm", \
"FLOATP" => "f_floatp.htm", \
"FLOOR" => "f_floorc.htm", \
"FFLOOR" => "f_floorc.htm", \
"CEILING" => "f_floorc.htm", \
"FCEILING" => "f_floorc.htm", \
"FMAKUNBOUND" => "f_fmakun.htm", \
"Generic" => "Function FUNCTION-KEYWORDS f_fn_kwd.htm", \
"FUNCTION-LAMBDA-EXPRESSION" => "f_fn_lam.htm", \
"FUNCTIONP" => "f_fnp.htm", \
"FORMAT" => "f_format.htm", \
"FUNCALL" => "f_funcal.htm", \
"GCD" => "f_gcd.htm", \
"GENSYM" => "f_gensym.htm", \
"GENTEMP" => "f_gentem.htm", \
"GET-INTERNAL-RUN-TIME" => "f_get__1.htm", \
"GETF" => "f_getf.htm", \
"GETHASH" => "f_gethas.htm", \
"GET" => "f_get.htm", \
"GET-INTERNAL-REAL-TIME" => "f_get_in.htm", \
"GET-OUTPUT-STREAM-STRING" => "f_get_ou.htm", \
"GET-PROPERTIES" => "f_get_pr.htm", \
"GET-SETF-EXPANSION" => "f_get_se.htm", \
"GET-UNIVERSAL-TIME" => "f_get_un.htm", \
"GET-DECODED-TIME" => "f_get_un.htm", \
"GRAPHIC-CHAR-P" => "f_graphi.htm", \
"HASH-TABLE-COUNT" => "f_hash_1.htm", \
"HASH-TABLE-REHASH-SIZE" => "f_hash_2.htm", \
"HASH-TABLE-REHASH-THRESHOLD" => "f_hash_3.htm", \
"HASH-TABLE-SIZE" => "f_hash_4.htm", \
"HASH-TABLE-TEST" => "f_hash_5.htm", \
"HASH-TABLE-P" => "f_hash_t.htm", \
"-" => "f__.htm", \
"IDENTITY" => "f_identi.htm", \
"IMPORT" => "f_import.htm", \
"Generic" => "Function INITIALIZE-INSTANCE f_init_i.htm", \
"INSPECT" => "f_inspec.htm", \
"INPUT-STREAM-P" => "f_in_stm.htm", \
"OUTPUT-STREAM-P" => "f_in_stm.htm", \
"INTEGERP" => "f_inte_1.htm", \
"INTEGER-LENGTH" => "f_intege.htm", \
"INTERACTIVE-STREAM-P" => "f_intera.htm", \
"INTERN" => "f_intern.htm", \
"INVALID-METHOD-ERROR" => "f_invali.htm", \
"INVOKE-RESTART" => "f_invo_1.htm", \
"INVOKE-RESTART-INTERACTIVELY" => "f_invo_2.htm", \
"INVOKE-DEBUGGER" => "f_invoke.htm", \
"INTERSECTION" => "f_isec_.htm", \
"NINTERSECTION" => "f_isec_.htm", \
"KEYWORDP" => "f_kwdp.htm", \
"LAST" => "f_last.htm", \
"LCM" => "f_lcm.htm", \
"LDB" => "f_ldb.htm", \
"LDB-TEST" => "f_ldb_te.htm", \
"LDIFF" => "f_ldiffc.htm", \
"TAILP" => "f_ldiffc.htm", \
"LOAD-LOGICAL-PATHNAME-TRANSLATIONS" => "f_ld_log.htm", \
"LENGTH" => "f_length.htm", \
"LISP-IMPLEMENTATION-TYPE" => "f_lisp_i.htm", \
"LIST-ALL-PACKAGES" => "f_list_a.htm", \
"LISTEN" => "f_listen.htm", \
"LIST" => "f_list_.htm", \
"LIST*" => "f_list_.htm", \
"LIST-LENGTH" => "f_list_l.htm", \
"LISTP" => "f_listp.htm", \
"LOAD" => "f_load.htm", \
"LOGAND" => "f_logand.htm", \
"LOGANDC1" => "f_logand.htm", \
"LOGANDC2" => "f_logand.htm", \
"LOGBITP" => "f_logbtp.htm", \
"LOGCOUNT" => "f_logcou.htm", \
"LOG" => "f_log.htm", \
"LOGICAL-PATHNAME" => "f_logi_1.htm", \
"LOGICAL-PATHNAME-TRANSLATIONS" => "f_logica.htm", \
"LOGTEST" => "f_logtes.htm", \
"MACHINE-INSTANCE" => "f_mach_i.htm", \
"MACHINE-TYPE" => "f_mach_t.htm", \
"MACHINE-VERSION" => "f_mach_v.htm", \
"MACRO-FUNCTION" => "f_macro_.htm", \
"MAKUNBOUND" => "f_makunb.htm", \
"MAPC" => "f_mapc_.htm", \
"MAPCAR" => "f_mapc_.htm", \
"MAPCAN" => "f_mapc_.htm", \
"MAPL" => "f_mapc_.htm", \
"MAPHASH" => "f_maphas.htm", \
"MAP" => "f_map.htm", \
"MAP-INTO" => "f_map_in.htm", \
"MASK-FIELD" => "f_mask_f.htm", \
"MAX" => "f_max_m.htm", \
"MIN" => "f_max_m.htm", \
"MEMBER" => "f_mem_m.htm", \
"MEMBER-IF" => "f_mem_m.htm", \
"MEMBER-IF-NOT" => "f_mem_m.htm", \
"MERGE-PATHNAMES" => "f_merge_.htm", \
"MERGE" => "f_merge.htm", \
"METHOD-COMBINATION-ERROR" => "f_meth_1.htm", \
"Generic" => "Function METHOD-QUALIFIERS f_method.htm", \
"MACROEXPAND" => "f_mexp_.htm", \
"MACROEXPAND-1" => "f_mexp_.htm", \
"MINUSP" => "f_minusp.htm", \
"PLUSP" => "f_minusp.htm", \
"MISMATCH" => "f_mismat.htm", \
"MAKE-ARRAY" => "f_mk_ar.htm", \
"MAKE-BROADCAST-STREAM" => "f_mk_bro.htm", \
"MAKE-CONDITION" => "f_mk_cnd.htm", \
"MAKE-CONCATENATED-STREAM" => "f_mk_con.htm", \
"MAKE-DISPATCH-MACRO-CHARACTER" => "f_mk_dis.htm", \
"MAKE-ECHO-STREAM" => "f_mk_ech.htm", \
"MAKE-HASH-TABLE" => "f_mk_has.htm", \
"Generic" => "Function MAKE-INSTANCES-OBSOLETE f_mk_i_1.htm", \
"Generic" => "Function MAKE-INSTANCE f_mk_ins.htm", \
"MAKE-LOAD-FORM-SAVING-SLOTS" => "f_mk_l_1.htm", \
"Generic" => "Function MAKE-LOAD-FORM f_mk_ld_.htm", \
"MAKE-LIST" => "f_mk_lis.htm", \
"MAKE-PACKAGE" => "f_mk_pkg.htm", \
"MAKE-PATHNAME" => "f_mk_pn.htm", \
"MAKE-RANDOM-STATE" => "f_mk_rnd.htm", \
"MAKE-STRING-INPUT-STREAM" => "f_mk_s_1.htm", \
"MAKE-STRING-OUTPUT-STREAM" => "f_mk_s_2.htm", \
"MAKE-SEQUENCE" => "f_mk_seq.htm", \
"MAKE-STRING" => "f_mk_stg.htm", \
"MAKE-SYMBOL" => "f_mk_sym.htm", \
"MAKE-SYNONYM-STREAM" => "f_mk_syn.htm", \
"MAKE-TWO-WAY-STREAM" => "f_mk_two.htm", \
"MOD" => "f_mod_r.htm", \
"REM" => "f_mod_r.htm", \
"NAME-CHAR" => "f_name_c.htm", \
"NAMESTRING" => "f_namest.htm", \
"FILE-NAMESTRING" => "f_namest.htm", \
"NCONC" => "f_nconc.htm", \
"Function" => "NEXT-METHOD-P f_next_m.htm", \
"Generic" => "Function NO-APPLICABLE-METHOD f_no_app.htm", \
"Generic" => "Function NO-NEXT-METHOD f_no_nex.htm", \
"NOT" => "f_not.htm", \
"NTHCDR" => "f_nthcdr.htm", \
"NTH" => "f_nth.htm", \
"NULL" => "f_null.htm", \
"NUMERATOR" => "f_numera.htm", \
"DENOMINATOR" => "f_numera.htm", \
"NUMBERP" => "f_nump.htm", \
"OPEN" => "f_open.htm", \
"OPEN-STREAM-P" => "f_open_s.htm", \
"Generic" => "Function (SETF CLASS-NAME) f_opsetf.htm", \
"PAIRLIS" => "f_pairli.htm", \
"PARSE-NAMESTRING" => "f_pars_1.htm", \
"PARSE-INTEGER" => "f_parse_.htm", \
"PEEK-CHAR" => "f_peek_c.htm", \
"PHASE" => "f_phase.htm", \
"PACKAGE-USED-BY-LIST" => "f_pkg__1.htm", \
"PACKAGE-ERROR-PACKAGE" => "f_pkg_er.htm", \
"PACKAGE-NAME" => "f_pkg_na.htm", \
"PACKAGE-NICKNAMES" => "f_pkg_ni.htm", \
"PACKAGEP" => "f_pkgp.htm", \
"PACKAGE-SHADOWING-SYMBOLS" => "f_pkg_sh.htm", \
"PACKAGE-USE-LIST" => "f_pkg_us.htm", \
"+" => "f_pl.htm", \
"PATHNAME-HOST" => "f_pn_hos.htm", \
"PATHNAME-DEVICE" => "f_pn_hos.htm", \
"PATHNAME" => "f_pn.htm", \
"PATHNAME-MATCH-P" => "f_pn_mat.htm", \
"PATHNAMEP" => "f_pnp.htm", \
"POSITION" => "f_pos_p.htm", \
"POSITION-IF" => "f_pos_p.htm", \
"POSITION-IF-NOT" => "f_pos_p.htm", \
"PPRINT-DISPATCH" => "f_ppr_di.htm", \
"PPRINT-FILL" => "f_ppr_fi.htm", \
"PPRINT-LINEAR" => "f_ppr_fi.htm", \
"PPRINT-INDENT" => "f_ppr_in.htm", \
"PPRINT-NEWLINE" => "f_ppr_nl.htm", \
"PPRINT-TAB" => "f_ppr_ta.htm", \
"PRINT-NOT-READABLE-OBJECT" => "f_pr_not.htm", \
"PROBE-FILE" => "f_probe_.htm", \
"Generic" => "Function PRINT-OBJECT f_pr_obj.htm", \
"PROCLAIM" => "f_procla.htm", \
"PROVIDE" => "f_provid.htm", \
"REQUIRE" => "f_provid.htm", \
"RANDOM" => "f_random.htm", \
"RASSOC" => "f_rassoc.htm", \
"RASSOC-IF" => "f_rassoc.htm", \
"RASSOC-IF-NOT" => "f_rassoc.htm", \
"RATIONALP" => "f_rati_1.htm", \
"RATIONAL" => "f_ration.htm", \
"RATIONALIZE" => "f_ration.htm", \
"READ-BYTE" => "f_rd_by.htm", \
"READ-CHAR-NO-HANG" => "f_rd_c_1.htm", \
"READ-CHAR" => "f_rd_cha.htm", \
"READ-DELIMITED-LIST" => "f_rd_del.htm", \
"READ-FROM-STRING" => "f_rd_fro.htm", \
"READ-LINE" => "f_rd_lin.htm", \
"READ" => "f_rd_rd.htm", \
"READ-PRESERVING-WHITESPACE" => "f_rd_rd.htm", \
"READ-SEQUENCE" => "f_rd_seq.htm", \
"READTABLEP" => "f_rdta_1.htm", \
"READTABLE-CASE" => "f_rdtabl.htm", \
"REALPART" => "f_realpa.htm", \
"IMAGPART" => "f_realpa.htm", \
"REALP" => "f_realp.htm", \
"REDUCE" => "f_reduce.htm", \
"Generic" => "Function REINITIALIZE-INSTANCE f_reinit.htm", \
"REMHASH" => "f_remhas.htm", \
"REMPROP" => "f_rempro.htm", \
"REPLACE" => "f_replac.htm", \
"REST" => "f_rest.htm", \
"REVAPPEND" => "f_revapp.htm", \
"NRECONC" => "f_revapp.htm", \
"REVERSE" => "f_revers.htm", \
"NREVERSE" => "f_revers.htm", \
"REMOVE-DUPLICATES" => "f_rm_dup.htm", \
"DELETE-DUPLICATES" => "f_rm_dup.htm", \
"Generic" => "Function REMOVE-METHOD f_rm_met.htm", \
"REMOVE" => "f_rm_rm.htm", \
"REMOVE-IF" => "f_rm_rm.htm", \
"REMOVE-IF-NOT" => "f_rm_rm.htm", \
"RANDOM-STATE-P" => "f_rnd_st.htm", \
"RENAME-FILE" => "f_rn_fil.htm", \
"RENAME-PACKAGE" => "f_rn_pkg.htm", \
"ROOM" => "f_room.htm", \
"ROW-MAJOR-AREF" => "f_row_ma.htm", \
"RPLACA" => "f_rplaca.htm", \
"RPLACD" => "f_rplaca.htm", \
"RESTART-NAME" => "f_rst_na.htm", \
"SUBSTITUTE" => "f_sbs_s.htm", \
"SUBSTITUTE-IF" => "f_sbs_s.htm", \
"SEARCH" => "f_search.htm", \
"SET-DISPATCH-MACRO-CHARACTER" => "f_set__1.htm", \
"SET-DIFFERENCE" => "f_set_di.htm", \
"NSET-DIFFERENCE" => "f_set_di.htm", \
"SET-EXCLUSIVE-OR" => "f_set_ex.htm", \
"NSET-EXCLUSIVE-OR" => "f_set_ex.htm", \
"SET" => "f_set.htm", \
"SET-MACRO-CHARACTER" => "f_set_ma.htm", \
"GET-MACRO-CHARACTER" => "f_set_ma.htm", \
"SET-PPRINT-DISPATCH" => "f_set_pp.htm", \
"SET-SYNTAX-FROM-CHAR" => "f_set_sy.htm", \
"SHADOW" => "f_shadow.htm", \
"Generic" => "Function SHARED-INITIALIZE f_shared.htm", \
"SHADOWING-IMPORT" => "f_shdw_i.htm", \
"SHORT-SITE-NAME" => "f_short_.htm", \
"LONG-SITE-NAME" => "f_short_.htm", \
"SIGNAL" => "f_signal.htm", \
"SIGNUM" => "f_signum.htm", \
"SIN" => "f_sin_c.htm", \
"COS" => "f_sin_c.htm", \
"TAN" => "f_sin_c.htm", \
"SINH" => "f_sinh_.htm", \
"COSH" => "f_sinh_.htm", \
"TANH" => "f_sinh_.htm", \
"ASINH" => "f_sinh_.htm", \
"SLEEP" => "f_sleep.htm", \
"/" => "f_sl.htm", \
"SLOT-BOUNDP" => "f_slt_bo.htm", \
"SLOT-EXISTS-P" => "f_slt_ex.htm", \
"SLOT-MAKUNBOUND" => "f_slt_ma.htm", \
"Generic" => "Function SLOT-MISSING f_slt_mi.htm", \
"Generic" => "Function SLOT-UNBOUND f_slt_un.htm", \
"SLOT-VALUE" => "f_slt_va.htm", \
"SIMPLE-BIT-VECTOR-P" => "f_smp_bt.htm", \
"SIMPLE-CONDITION-FORMAT-CONTROL" => "f_smp_cn.htm", \
"SIMPLE-STRING-P" => "f_smp_st.htm", \
"SIMPLE-VECTOR-P" => "f_smp_ve.htm", \
"SORT" => "f_sort_.htm", \
"STABLE-SORT" => "f_sort_.htm", \
"SPECIAL-OPERATOR-P" => "f_specia.htm", \
"SQRT" => "f_sqrt_.htm", \
"ISQRT" => "f_sqrt_.htm", \
"STANDARD-CHAR-P" => "f_std_ch.htm", \
"STRING=" => "f_stgeq_.htm", \
"STRING/=" => " f_stgeq_.htm", \
"STRING&lt" => "f_stgeq_.htm", \
"STRINGP" => "f_stgp.htm", \
"STRING-TRIM" => "f_stg_tr.htm", \
"STRING-LEFT-TRIM" => "f_stg_tr.htm", \
"STRING-UPCASE" => "f_stg_up.htm", \
"STRING-DOWNCASE" => "f_stg_up.htm", \
"*" => "f_st.htm", \
"STREAM-ELEMENT-TYPE" => "f_stm_el.htm", \
"STREAM-ERROR-STREAM" => "f_stm_er.htm", \
"STREAM-EXTERNAL-FORMAT" => "f_stm_ex.htm", \
"STREAMP" => "f_stmp.htm", \
"STRING" => "f_string.htm", \
"SUBLIS" => "f_sublis.htm", \
"NSUBLIS" => "f_sublis.htm", \
"SUBSEQ" => "f_subseq.htm", \
"SUBSETP" => "f_subset.htm", \
"SUBST" => "f_substc.htm", \
"SUBST-IF" => "f_substc.htm", \
"SUBST-IF-NOT" => "f_substc.htm", \
"SUBTYPEP" => "f_subtpp.htm", \
"SVREF" => "f_svref.htm", \
"SOFTWARE-TYPE" => "f_sw_tpc.htm", \
"SOFTWARE-VERSION" => "f_sw_tpc.htm", \
"SXHASH" => "f_sxhash.htm", \
"SYMBOL-FUNCTION" => "f_symb_1.htm", \
"SYMBOL-NAME" => "f_symb_2.htm", \
"SYMBOL-PACKAGE" => "f_symb_3.htm", \
"SYMBOL-PLIST" => "f_symb_4.htm", \
"SYMBOL-VALUE" => "f_symb_5.htm", \
"SYMBOLP" => "f_symbol.htm", \
"SYNONYM-STREAM-SYMBOL" => "f_syn_st.htm", \
"TERPRI" => "f_terpri.htm", \
"FRESH-LINE" => "f_terpri.htm", \
"TRUENAME" => "f_tn.htm", \
"TYPE-ERROR-DATUM" => "f_tp_err.htm", \
"TYPE-ERROR-EXPECTED-TYPE" => "f_tp_err.htm", \
"TYPE-OF" => "f_tp_of.htm", \
"TREE-EQUAL" => "f_tree_e.htm", \
"TRANSLATE-LOGICAL-PATHNAME" => "f_tr_log.htm", \
"TRANSLATE-PATHNAME" => "f_tr_pn.htm", \
"TWO-WAY-STREAM-INPUT-STREAM" => "f_two_wa.htm", \
"TYPEP" => "f_typep.htm", \
"UNBOUND-SLOT-INSTANCE" => "f_unboun.htm", \
"UNEXPORT" => "f_unexpo.htm", \
"UNINTERN" => "f_uninte.htm", \
"UNION" => "f_unionc.htm", \
"NUNION" => "f_unionc.htm", \
"UNREAD-CHAR" => "f_unrd_c.htm", \
"UNUSE-PACKAGE" => "f_unuse_.htm", \
"Generic" => "Function UPDATE-INSTANCE-FOR-REDEFINED-CLASS f_upda_1.htm", \
"Generic" => "Function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS f_update.htm", \
"UPGRADED-ARRAY-ELEMENT-TYPE" => "f_upgr_1.htm", \
"UPGRADED-COMPLEX-PART-TYPE" => "f_upgrad.htm", \
"UPPER-CASE-P" => "f_upper_.htm", \
"LOWER-CASE-P" => "f_upper_.htm", \
"USE-PACKAGE" => "f_use_pk.htm", \
"USER-HOMEDIR-PATHNAME" => "f_user_h.htm", \
"VALUES-LIST" => "f_vals_l.htm", \
"VALUES" => "f_values.htm", \
"VECTORP" => "f_vecp.htm", \
"VECTOR-POP" => "f_vec_po.htm", \
"VECTOR-PUSH" => "f_vec_ps.htm", \
"VECTOR-PUSH-EXTEND" => "f_vec_ps.htm", \
"VECTOR" => "f_vector.htm", \
"WARN" => "f_warn.htm", \
"WILD-PATHNAME-P" => "f_wild_p.htm", \
"WRITE-BYTE" => "f_wr_by.htm", \
"WRITE-CHAR" => "f_wr_cha.htm", \
"WRITE" => "f_wr_pr.htm", \
"PRIN1" => "f_wr_pr.htm", \
"PRINT" => "f_wr_pr.htm", \
"PPRINT" => "f_wr_pr.htm", \
"WRITE-SEQUENCE" => "f_wr_seq.htm", \
"WRITE-STRING" => "f_wr_stg.htm", \
"WRITE-LINE" => "f_wr_stg.htm", \
"WRITE-TO-STRING" => "f_wr_to_.htm", \
"PRIN1-TO-STRING" => "f_wr_to_.htm", \
"Y-OR-N-P" => "f_y_or_n.htm", \
"YES-OR-NO-P" => "f_y_or_n.htm", \
"ZEROP" => "f_zerop.htm", \
"AND" => "m_and.htm", \
"ASSERT" => "m_assert.htm", \
"Macro" => "CALL-METHOD m_call_m.htm", \
"MAKE-METHOD" => "m_call_m.htm", \
"CASE" => "m_case_.htm", \
"CCASE" => "m_case_.htm", \
"ECASE" => "m_case_.htm", \
"CHECK-TYPE" => "m_check_.htm", \
"COND" => "m_cond.htm", \
"DECLAIM" => "m_declai.htm", \
"DEFCLASS" => "m_defcla.htm", \
"DEFCONSTANT" => "m_defcon.htm", \
"DEFGENERIC" => "m_defgen.htm", \
"DEFINE-SYMBOL-MACRO" => "m_defi_1.htm", \
"DEFINE-MODIFY-MACRO" => "m_defi_2.htm", \
"DEFINE-SETF-EXPANDER" => "m_defi_3.htm", \
"DEFINE-METHOD-COMBINATION" => "m_defi_4.htm", \
"DEFINE-CONDITION" => "m_defi_5.htm", \
"DEFINE-COMPILER-MACRO" => "m_define.htm", \
"DEFMACRO" => "m_defmac.htm", \
"DEFMETHOD" => "m_defmet.htm", \
"DEFPARAMETER" => "m_defpar.htm", \
"DEFVAR" => "m_defpar.htm", \
"DEFPACKAGE" => "m_defpkg.htm", \
"DEFSETF" => "m_defset.htm", \
"DEFSTRUCT" => "m_defstr.htm", \
"DEFTYPE" => "m_deftp.htm", \
"DEFUN" => "m_defun.htm", \
"DESTRUCTURING-BIND" => "m_destru.htm", \
"DO" => "m_do_do.htm", \
"DO*" => "m_do_do.htm", \
"DOLIST" => "m_dolist.htm", \
"DO-SYMBOLS" => "m_do_sym.htm", \
"DO-EXTERNAL-SYMBOLS" => "m_do_sym.htm", \
"DOTIMES" => "m_dotime.htm", \
"FORMATTER" => "m_format.htm", \
"HANDLER-CASE" => "m_hand_1.htm", \
"HANDLER-BIND" => "m_handle.htm", \
"IGNORE-ERRORS" => "m_ignore.htm", \
"INCF" => "m_incf_.htm", \
"DECF" => "m_incf_.htm", \
"IN-PACKAGE" => "m_in_pkg.htm", \
"LAMBDA" => "m_lambda.htm", \
"Macro" => "LOOP-FINISH m_loop_f.htm", \
"LOOP" => "m_loop.htm", \
"MULTIPLE-VALUE-LIST" => "m_mult_1.htm", \
"MULTIPLE-VALUE-SETQ" => "m_mult_2.htm", \
"MULTIPLE-VALUE-BIND" => "m_multip.htm", \
"NTH-VALUE" => "m_nth_va.htm", \
"OR" => "m_or.htm", \
"POP" => "m_pop.htm", \
"Macro" => "PPRINT-EXIT-IF-LIST-EXHAUSTED m_ppr_ex.htm", \
"PPRINT-LOGICAL-BLOCK" => "m_ppr_lo.htm", \
"Macro" => "PPRINT-POP m_ppr_po.htm", \
"PROG1" => "m_prog1c.htm", \
"PROG2" => "m_prog1c.htm", \
"PROG" => "m_prog_.htm", \
"PROG*" => "m_prog_.htm", \
"PRINT-UNREADABLE-OBJECT" => "m_pr_unr.htm", \
"PSETQ" => "m_psetq.htm", \
"PUSHNEW" => "m_pshnew.htm", \
"PUSH" => "m_push.htm", \
"REMF" => "m_remf.htm", \
"RETURN" => "m_return.htm", \
"ROTATEF" => "m_rotate.htm", \
"RESTART-BIND" => "m_rst_bi.htm", \
"RESTART-CASE" => "m_rst_ca.htm", \
"SETF" => "m_setf_.htm", \
"PSETF" => "m_setf_.htm", \
"SHIFTF" => "m_shiftf.htm", \
"STEP" => "m_step.htm", \
"TIME" => "m_time.htm", \
"TYPECASE" => "m_tpcase.htm", \
"CTYPECASE" => "m_tpcase.htm", \
"ETYPECASE" => "m_tpcase.htm", \
"TRACE" => "m_tracec.htm", \
"UNTRACE" => "m_tracec.htm", \
"WITH-ACCESSORS" => "m_w_acce.htm", \
"WITH-CONDITION-RESTARTS" => "m_w_cnd_.htm", \
"WITH-COMPILATION-UNIT" => "m_w_comp.htm", \
"WITH-HASH-TABLE-ITERATOR" => "m_w_hash.htm", \
"WHEN" => "m_when_.htm", \
"UNLESS" => "m_when_.htm", \
"WITH-INPUT-FROM-STRING" => "m_w_in_f.htm", \
"WITH-OPEN-STREAM" => "m_w_op_1.htm", \
"WITH-OPEN-FILE" => "m_w_open.htm", \
"WITH-OUTPUT-TO-STRING" => "m_w_out_.htm", \
"WITH-PACKAGE-ITERATOR" => "m_w_pkg_.htm", \
"WITH-SLOTS" => "m_w_slts.htm", \
"WITH-SIMPLE-RESTART" => "m_w_smp_.htm", \
"WITH-STANDARD-IO-SYNTAX" => "m_w_std_.htm", \
"ABORT" => "r_abort.htm", \
"CONTINUE" => "r_contin.htm", \
"MUFFLE-WARNING" => "r_muffle.htm", \
"STORE-VALUE" => "r_store_.htm", \
"USE-VALUE" => "r_use_va.htm", \
"Operator" => "BLOCK s_block.htm", \
"Operator" => "CATCH s_catch.htm", \
"DECLARE" => "s_declar.htm", \
"Operator" => "EVAL-WHEN s_eval_w.htm", \
"Operator" => "FLET s_flet_.htm", \
"LABELS" => "s_flet_.htm", \
"MACROLET" => "s_flet_.htm", \
"Operator" => "FUNCTION s_fn.htm", \
"Operator" => "GO s_go.htm", \
"Operator" => "IF s_if.htm", \
"LAMBDA" => "s_lambda.htm", \
"Operator" => "LOAD-TIME-VALUE s_ld_tim.htm", \
"Operator" => "LET s_let_l.htm", \
"LET*" => "s_let_l.htm", \
"Operator" => "LOCALLY s_locall.htm", \
"Operator" => "MULTIPLE-VALUE-PROG1 s_mult_1.htm", \
"Operator" => "MULTIPLE-VALUE-CALL s_multip.htm", \
"Operator" => "PROGN s_progn.htm", \
"Operator" => "PROGV s_progv.htm", \
"Operator" => "QUOTE s_quote.htm", \
"Operator" => "RETURN-FROM s_ret_fr.htm", \
"Form" => "SETQ s_setq.htm", \
"Operator" => "SYMBOL-MACROLET s_symbol.htm", \
"Operator" => "TAGBODY s_tagbod.htm", \
"Operator" => "THE s_the.htm", \
"Operator" => "THROW s_throw.htm", \
"Operator" => "UNWIND-PROTECT s_unwind.htm", \
"Specifier" => "AND t_and.htm", \
"Class" => "ARRAY t_array.htm", \
"ATOM" => "t_atom.htm", \
"BOOLEAN" => "t_ban.htm", \
"BASE-CHAR" => "t_base_c.htm", \
"BASE-STRING" => "t_base_s.htm", \
"BIGNUM" => "t_bignum.htm", \
"BIT" => "t_bit.htm", \
"Class" => "BROADCAST-STREAM t_broadc.htm", \
"Class" => "BIT-VECTOR t_bt_vec.htm", \
"Class" => "BUILT-IN-CLASS t_built_.htm", \
"Class" => "CHARACTER t_ch.htm", \
"Class" => "CLASS t_class.htm", \
"COMPILED-FUNCTION" => "t_cmpd_f.htm", \
"Class" => "COMPLEX t_comple.htm", \
"Class" => "CONCATENATED-STREAM t_concat.htm", \
"Class" => "CONS t_cons.htm", \
"Class" => "ECHO-STREAM t_echo_s.htm", \
"Specifier" => "EQL t_eql.htm", \
"EXTENDED-CHAR" => "t_extend.htm", \
"Class" => "FILE-STREAM t_file_s.htm", \
"FIXNUM" => "t_fixnum.htm", \
"Class" => "FLOAT t_float.htm", \
"Class" => "FUNCTION t_fn.htm", \
"Class" => "GENERIC-FUNCTION t_generi.htm", \
"Class" => "HASH-TABLE t_hash_t.htm", \
"Class" => "INTEGER t_intege.htm", \
"KEYWORD" => "t_kwd.htm", \
"Class" => "LIST t_list.htm", \
"Class" => "LOGICAL-PATHNAME t_logica.htm", \
"Specifier" => "MEMBER t_member.htm", \
"Class" => "METHOD-COMBINATION t_meth_1.htm", \
"Class" => "METHOD t_method.htm", \
"Specifier" => "MOD t_mod.htm", \
"NIL" => "t_nil.htm", \
"Specifier" => "NOT t_not.htm", \
"Class" => "NULL t_null.htm", \
"Class" => "NUMBER t_number.htm", \
"Specifier" => "OR t_or.htm", \
"Class" => "PACKAGE t_pkg.htm", \
"Class" => "PATHNAME t_pn.htm", \
"Class" => "RATIO t_ratio.htm", \
"Class" => "RATIONAL t_ration.htm", \
"Class" => "READTABLE t_rdtabl.htm", \
"Class" => "REAL t_real.htm", \
"Class" => "RANDOM-STATE t_rnd_st.htm", \
"Class" => "RESTART t_rst.htm", \
"Specifier" => "SATISFIES t_satisf.htm", \
"Class" => "SEQUENCE t_seq.htm", \
"SIGNED-BYTE" => "t_sgn_by.htm", \
"SHORT-FLOAT" => "t_short_.htm", \
"SINGLE-FLOAT" => "t_short_.htm", \
"DOUBLE-FLOAT" => "t_short_.htm", \
"SIMPLE-ARRAY" => "t_smp_ar.htm", \
"SIMPLE-BASE-STRING" => "t_smp_ba.htm", \
"SIMPLE-BIT-VECTOR" => "t_smp_bt.htm", \
"SIMPLE-STRING" => "t_smp_st.htm", \
"SIMPLE-VECTOR" => "t_smp_ve.htm", \
"STANDARD-CHAR" => "t_std_ch.htm", \
"Class" => "STANDARD-CLASS t_std_cl.htm", \
"Class" => "STANDARD-GENERIC-FUNCTION t_std_ge.htm", \
"Class" => "STANDARD-METHOD t_std_me.htm", \
"STANDARD-OBJECT" => "t_std_ob.htm", \
"Class" => "STRING-STREAM t_stg_st.htm", \
"Class" => "STREAM t_stream.htm", \
"Class" => "STRING t_string.htm", \
"Class" => "STRUCTURE-CLASS t_stu_cl.htm", \
"STRUCTURE-OBJECT" => "t_stu_ob.htm", \
"Class" => "SYMBOL t_symbol.htm", \
"Class" => "SYNONYM-STREAM t_syn_st.htm", \
"Class" => "T t_t.htm", \
"Class" => "TWO-WAY-STREAM t_two_wa.htm", \
"UNSIGNED-BYTE" => "t_unsgn_.htm", \
"Specifier" => "VALUES t_values.htm", \
"Class" => "VECTOR t_vector.htm", \
"ARRAY-DIMENSION-LIMIT" => "v_ar_dim.htm", \
"ARRAY-RANK-LIMIT" => "v_ar_ran.htm", \
"ARRAY-TOTAL-SIZE-LIMIT" => "v_ar_tot.htm", \
"BOOLE-1" => "v_b_1_b.htm", \
"BOOLE-2" => "v_b_1_b.htm", \
"*BREAK-ON-SIGNALS*" => "v_break_.htm", \
"CALL-ARGUMENTS-LIMIT" => "v_call_a.htm", \
"CHAR-CODE-LIMIT" => "v_char_c.htm", \
"*COMPILE-FILE-PATHNAME*" => "v_cmp_fi.htm", \
"*COMPILE-PRINT*" => "v_cmp_pr.htm", \
"*COMPILE-VERBOSE*" => "v_cmp_pr.htm", \
"*DEBUGGER-HOOK*" => "v_debugg.htm", \
"*DEBUG-IO*" => "v_debug_.htm", \
"*ERROR-OUTPUT*" => "v_debug_.htm", \
"*DEFAULT-PATHNAME-DEFAULTS*" => "v_defaul.htm", \
"*FEATURES*" => "v_featur.htm", \
"*GENSYM-COUNTER*" => "v_gensym.htm", \
"INTERNAL-TIME-UNITS-PER-SECOND" => "v_intern.htm", \
"LAMBDA-PARAMETERS-LIMIT" => "v_lamb_1.htm", \
"LAMBDA-LIST-KEYWORDS" => "v_lambda.htm", \
"*LOAD-PATHNAME*" => "v_ld_pns.htm", \
"*LOAD-TRUENAME*" => "v_ld_pns.htm", \
"*LOAD-PRINT*" => "v_ld_prs.htm", \
"*LOAD-VERBOSE*" => "v_ld_prs.htm", \
"*MACROEXPAND-HOOK*" => "v_mexp_h.htm", \
"*MODULES*" => "v_module.htm", \
"MOST-POSITIVE-SHORT-FLOAT" => "v_most_1.htm", \
"MOST-POSITIVE-FIXNUM" => "v_most_p.htm", \
"MULTIPLE-VALUES-LIMIT" => "v_multip.htm", \
"NIL" => "v_nil.htm", \
"PI" => "v_pi.htm", \
"*PACKAGE*" => "v_pkg.htm", \
"+" => "v_pl_plp.htm", \
"++" => "v_pl_plp.htm", \
"+++" => "v_pl_plp.htm", \
"*PRINT-ARRAY*" => "v_pr_ar.htm", \
"*PRINT-BASE*" => "v_pr_bas.htm", \
"*PRINT-RADIX*" => "v_pr_bas.htm", \
"*PRINT-CASE*" => "v_pr_cas.htm", \
"*PRINT-CIRCLE*" => "v_pr_cir.htm", \
"*PRINT-ESCAPE*" => "v_pr_esc.htm", \
"*PRINT-GENSYM*" => "v_pr_gen.htm", \
"*PRINT-LEVEL*" => "v_pr_lev.htm", \
"*PRINT-LENGTH*" => "v_pr_lev.htm", \
"*PRINT-LINES*" => "v_pr_lin.htm", \
"*PRINT-MISER-WIDTH*" => "v_pr_mis.htm", \
"*PRINT-PPRINT-DISPATCH*" => "v_pr_ppr.htm", \
"*PRINT-PRETTY*" => "v_pr_pre.htm", \
"*PRINT-READABLY*" => "v_pr_rda.htm", \
"*PRINT-RIGHT-MARGIN*" => "v_pr_rig.htm", \
"*READ-BASE*" => "v_rd_bas.htm", \
"*READ-DEFAULT-FLOAT-FORMAT*" => "v_rd_def.htm", \
"*READ-EVAL*" => "v_rd_eva.htm", \
"*READ-SUPPRESS*" => "v_rd_sup.htm", \
"*READTABLE*" => "v_rdtabl.htm", \
"*RANDOM-STATE*" => "v_rnd_st.htm", \
"SHORT-FLOAT-EPSILON" => "v_short_.htm", \
"/" => "v_sl_sls.htm", \
"//" => "v_sl_sls.htm", \
"///" => "v_sl_sls.htm", \
"*" => "v__stst_.htm", \
"**" => "v__stst_.htm", \
"***" => "v__stst_.htm", \
"*TERMINAL-IO*" => "v_termin.htm", \
"T" => "v_t.htm"}
