#!/usr/bin/ruby -w
$map = {"declaration" => "d_declar.htm", "dynamic-extent" => "d_dynami.htm", "ftype" => "d_ftype.htm", "ignore" => "d_ignore.htm", \
"ignorable" => "d_ignore.htm", "inline" => "d_inline.htm", "notinline" => "d_inline.htm", "optimize" => "d_optimi.htm", \
"special" => "d_specia.htm", "type" => "d_type.htm", "arithmetic-error" => "e_arithm.htm", "cell-error" => "e_cell_e.htm", \
"condition" => "e_cnd.htm", "control-error" => "e_contro.htm", "division-by-zero" => "e_divisi.htm", "end-of-file" => "e_end_of.htm", \
"error" => "e_error.htm", "file-error" => "e_file_e.htm", "floating-point-inexact" => "e_floa_1.htm", "floating-point-overflow" => "e_floa_2.htm", \
"floating-point-underflow" => "e_floa_3.htm", "floating-point-invalid-operation" => "e_floati.htm", "parse-error" => "e_parse_.htm", "package-error" => "e_pkg_er.htm", \
"print-not-readable" => "e_pr_not.htm", "program-error" => "e_progra.htm", "reader-error" => "e_rder_e.htm", "serious-condition" => "e_seriou.htm", \
"simple-condition" => "e_smp_cn.htm", "simple-error" => "e_smp_er.htm", "simple-type-error" => "e_smp_tp.htm", "simple-warning" => "e_smp_wa.htm", \
"stream-error" => "e_stm_er.htm", "storage-condition" => "e_storag.htm", "style-warning" => "e_style_.htm", "type-error" => "e_tp_err.htm", \
"unbound-variable" => "e_unbo_1.htm", "unbound-slot" => "e_unboun.htm", "undefined-function" => "e_undefi.htm", "warning" => "e_warnin.htm", \
"1+" => "f_1pl_1_.htm", "1-" => "f_1pl_1_.htm", "abort" => "f_abortc.htm", "continue" => "f_abortc.htm", \
"muffle-warning" => "f_abortc.htm", "abs" => "f_abs.htm", "acons" => "f_acons.htm", "generic" => "function add-method f_add_me.htm", \
"adjoin" => "f_adjoin.htm", "adjustable-array-p" => "f_adju_1.htm", "adjust-array" => "f_adjust.htm", "generic" => "function allocate-instance f_alloca.htm", \
"alpha-char-p" => "f_alpha_.htm", "alphanumericp" => "f_alphan.htm", "append" => "f_append.htm", "apply" => "f_apply.htm", \
"apropos" => "f_apropo.htm", "apropos-list" => "f_apropo.htm", "array-dimensions" => "f_ar_d_1.htm", "array-dimension" => "f_ar_dim.htm", \
"array-displacement" => "f_ar_dis.htm", "aref" => "f_aref.htm", "array-element-type" => "f_ar_ele.htm", "array-has-fill-pointer-p" => "f_ar_has.htm", \
"array-in-bounds-p" => "f_ar_in_.htm", "arithmetic-error-operands" => "f_arithm.htm", "array-rank" => "f_ar_ran.htm", "arrayp" => "f_arrayp.htm", \
"array-row-major-index" => "f_ar_row.htm", "array-total-size" => "f_ar_tot.htm", "ash" => "f_ash.htm", "asin" => "f_asin_.htm", \
"acos" => "f_asin_.htm", "atan" => "f_asin_.htm", "assoc" => "f_assocc.htm", "assoc-if" => "f_assocc.htm", \
"assoc-if-not" => "f_assocc.htm", "atom" => "f_atom.htm", "boole" => "f_boole.htm", "boundp" => "f_boundp.htm", \
"break" => "f_break.htm", "broadcast-stream-streams" => "f_broadc.htm", "bit-and" => "f_bt_and.htm", "bit-andc1" => "f_bt_and.htm", \
"bit-andc2" => "f_bt_and.htm", "bit" => "f_bt_sb.htm", "sbit" => "f_bt_sb.htm", "bit-vector-p" => "f_bt_vec.htm", \
"butlast" => "f_butlas.htm", "nbutlast" => "f_butlas.htm", "byte" => "f_by_by.htm", "byte-size" => "f_by_by.htm", \
"byte-position" => "f_by_by.htm", "function" => "call-next-method f_call_n.htm", "car" => "f_car_c.htm", "cdr" => "f_car_c.htm", \
"caar" => "f_car_c.htm", "cadr" => "f_car_c.htm", "cdar" => "f_car_c.htm", "cell-error-name" => "f_cell_e.htm", \
"cerror" => "f_cerror.htm", "char-code" => "f_char_c.htm", "char=" => "f_chareq.htm", "char/=" => "f_chareq.htm", \
"char&lt" => "f_chareq.htm", "char&gt" => "f_chareq.htm", "char" => "f_char_.htm", "schar" => "f_char_.htm", \
"char-int" => "f_char_i.htm", "char-name" => "f_char_n.htm", "char-upcase" => "f_char_u.htm", "char-downcase" => "f_char_u.htm", \
"generic" => "function change-class f_chg_cl.htm", "character" => "f_ch.htm", "characterp" => "f_chp.htm", "cis" => "f_cis.htm", \
"class-of" => "f_clas_1.htm", "generic" => "function class-name f_class_.htm", "clear-input" => "f_clear_.htm", "close" => "f_close.htm", \
"clrhash" => "f_clrhas.htm", "compile-file-pathname" => "f_cmp__1.htm", "compiled-function-p" => "f_cmpd_f.htm", "compile-file" => "f_cmp_fi.htm", \
"compile" => "f_cmp.htm", "compiler-macro-function" => "f_cmp_ma.htm", "code-char" => "f_code_c.htm", "coerce" => "f_coerce.htm", \
"compute-restarts" => "f_comp_1.htm", "complex" => "f_comp_2.htm", "complexp" => "f_comp_3.htm", "complement" => "f_comple.htm", \
"generic" => "function compute-applicable-methods f_comput.htm", "concatenated-stream-streams" => "f_conc_1.htm", "concatenate" => "f_concat.htm", "conjugate" => "f_conjug.htm", \
"constantly" => "f_cons_1.htm", "cons" => "f_cons.htm", "consp" => "f_consp.htm", "constantp" => "f_consta.htm", \
"count" => "f_countc.htm", "count-if" => "f_countc.htm", "count-if-not" => "f_countc.htm", "copy-alist" => "f_cp_ali.htm", \
"copy-list" => "f_cp_lis.htm", "copy-pprint-dispatch" => "f_cp_ppr.htm", "copy-readtable" => "f_cp_rdt.htm", "copy-seq" => "f_cp_seq.htm", \
"copy-structure" => "f_cp_stu.htm", "copy-symbol" => "f_cp_sym.htm", "copy-tree" => "f_cp_tre.htm", "decode-float" => "f_dec_fl.htm", \
"scale-float" => "f_dec_fl.htm", "decode-universal-time" => "f_dec_un.htm", "delete-file" => "f_del_fi.htm", "delete-package" => "f_del_pk.htm", \
"deposit-field" => "f_deposi.htm", "generic" => "function describe-object f_desc_1.htm", "describe" => "f_descri.htm", "digit-char-p" => "f_digi_1.htm", \
"digit-char" => "f_digit_.htm", "directory" => "f_dir.htm", "disassemble" => "f_disass.htm", "generic" => "function documentation f_docume.htm", \
"dpb" => "f_dpb.htm", "dribble" => "f_dribbl.htm", "echo-stream-input-stream" => "f_echo_s.htm", "ed" => "f_ed.htm", \
"elt" => "f_elt.htm", "encode-universal-time" => "f_encode.htm", "endp" => "f_endp.htm", "ensure-directories-exist" => "f_ensu_1.htm", \
"ensure-generic-function" => "f_ensure.htm", "eq" => "f_eq.htm", "eql" => "f_eql.htm", "equal" => "f_equal.htm", \
"equalp" => "f_equalp.htm", "error" => "f_error.htm", "eval" => "f_eval.htm", "evenp" => "f_evenpc.htm", \
"oddp" => "f_evenpc.htm", "every" => "f_everyc.htm", "some" => "f_everyc.htm", "notevery" => "f_everyc.htm", \
"notany" => "f_everyc.htm", "exp" => "f_exp_e.htm", "expt" => "f_exp_e.htm", "export" => "f_export.htm", \
"fboundp" => "f_fbound.htm", "fdefinition" => "f_fdefin.htm", "file-author" => "f_file_a.htm", "file-error-pathname" => "f_file_e.htm", \
"file-length" => "f_file_l.htm", "file-position" => "f_file_p.htm", "file-string-length" => "f_file_s.htm", "file-write-date" => "f_file_w.htm", \
"fill" => "f_fill.htm", "fill-pointer" => "f_fill_p.htm", "find-all-symbols" => "f_find_a.htm", "find-class" => "f_find_c.htm", \
"find" => "f_find_.htm", "find-if" => "f_find_.htm", "find-if-not" => "f_find_.htm", "generic" => "function find-method f_find_m.htm", \
"find-package" => "f_find_p.htm", "find-restart" => "f_find_r.htm", "find-symbol" => "f_find_s.htm", "finish-output" => "f_finish.htm", \
"force-output" => "f_finish.htm", "first" => "f_firstc.htm", "second" => "f_firstc.htm", "third" => "f_firstc.htm", \
"fourth" => "f_firstc.htm", "float" => "f_float.htm", "floatp" => "f_floatp.htm", "floor" => "f_floorc.htm", \
"ffloor" => "f_floorc.htm", "ceiling" => "f_floorc.htm", "fceiling" => "f_floorc.htm", "fmakunbound" => "f_fmakun.htm", \
"generic" => "function function-keywords f_fn_kwd.htm", "function-lambda-expression" => "f_fn_lam.htm", "functionp" => "f_fnp.htm", "format" => "f_format.htm", \
"funcall" => "f_funcal.htm", "gcd" => "f_gcd.htm", "gensym" => "f_gensym.htm", "gentemp" => "f_gentem.htm", \
"get-internal-run-time" => "f_get__1.htm", "getf" => "f_getf.htm", "gethash" => "f_gethas.htm", "get" => "f_get.htm", \
"get-internal-real-time" => "f_get_in.htm", "get-output-stream-string" => "f_get_ou.htm", "get-properties" => "f_get_pr.htm", "get-setf-expansion" => "f_get_se.htm", \
"get-universal-time" => "f_get_un.htm", "get-decoded-time" => "f_get_un.htm", "graphic-char-p" => "f_graphi.htm", "hash-table-count" => "f_hash_1.htm", \
"hash-table-rehash-size" => "f_hash_2.htm", "hash-table-rehash-threshold" => "f_hash_3.htm", "hash-table-size" => "f_hash_4.htm", "hash-table-test" => "f_hash_5.htm", \
"hash-table-p" => "f_hash_t.htm", "-" => "f__.htm", "identity" => "f_identi.htm", "import" => "f_import.htm", \
"generic" => "function initialize-instance f_init_i.htm", "inspect" => "f_inspec.htm", "input-stream-p" => "f_in_stm.htm", "output-stream-p" => "f_in_stm.htm", \
"integerp" => "f_inte_1.htm", "integer-length" => "f_intege.htm", "interactive-stream-p" => "f_intera.htm", "intern" => "f_intern.htm", \
"invalid-method-error" => "f_invali.htm", "invoke-restart" => "f_invo_1.htm", "invoke-restart-interactively" => "f_invo_2.htm", "invoke-debugger" => "f_invoke.htm", \
"intersection" => "f_isec_.htm", "nintersection" => "f_isec_.htm", "keywordp" => "f_kwdp.htm", "last" => "f_last.htm", \
"lcm" => "f_lcm.htm", "ldb" => "f_ldb.htm", "ldb-test" => "f_ldb_te.htm", "ldiff" => "f_ldiffc.htm", \
"tailp" => "f_ldiffc.htm", "load-logical-pathname-translations" => "f_ld_log.htm", "length" => "f_length.htm", "lisp-implementation-type" => "f_lisp_i.htm", \
"list-all-packages" => "f_list_a.htm", "listen" => "f_listen.htm", "list" => "f_list_.htm", "list*" => "f_list_.htm", \
"list-length" => "f_list_l.htm", "listp" => "f_listp.htm", "load" => "f_load.htm", "logand" => "f_logand.htm", \
"logandc1" => "f_logand.htm", "logandc2" => "f_logand.htm", "logbitp" => "f_logbtp.htm", "logcount" => "f_logcou.htm", \
"log" => "f_log.htm", "logical-pathname" => "f_logi_1.htm", "logical-pathname-translations" => "f_logica.htm", "logtest" => "f_logtes.htm", \
"machine-instance" => "f_mach_i.htm", "machine-type" => "f_mach_t.htm", "machine-version" => "f_mach_v.htm", "macro-function" => "f_macro_.htm", \
"makunbound" => "f_makunb.htm", "mapc" => "f_mapc_.htm", "mapcar" => "f_mapc_.htm", "mapcan" => "f_mapc_.htm", \
"mapl" => "f_mapc_.htm", "maphash" => "f_maphas.htm", "map" => "f_map.htm", "map-into" => "f_map_in.htm", \
"mask-field" => "f_mask_f.htm", "max" => "f_max_m.htm", "min" => "f_max_m.htm", "member" => "f_mem_m.htm", \
"member-if" => "f_mem_m.htm", "member-if-not" => "f_mem_m.htm", "merge-pathnames" => "f_merge_.htm", "merge" => "f_merge.htm", \
"method-combination-error" => "f_meth_1.htm", "generic" => "function method-qualifiers f_method.htm", "macroexpand" => "f_mexp_.htm", "macroexpand-1" => "f_mexp_.htm", \
"minusp" => "f_minusp.htm", "plusp" => "f_minusp.htm", "mismatch" => "f_mismat.htm", "make-array" => "f_mk_ar.htm", \
"make-broadcast-stream" => "f_mk_bro.htm", "make-condition" => "f_mk_cnd.htm", "make-concatenated-stream" => "f_mk_con.htm", "make-dispatch-macro-character" => "f_mk_dis.htm", \
"make-echo-stream" => "f_mk_ech.htm", "make-hash-table" => "f_mk_has.htm", "generic" => "function make-instances-obsolete f_mk_i_1.htm", "generic" => "function make-instance f_mk_ins.htm", \
"make-load-form-saving-slots" => "f_mk_l_1.htm", "generic" => "function make-load-form f_mk_ld_.htm", "make-list" => "f_mk_lis.htm", "make-package" => "f_mk_pkg.htm", \
"make-pathname" => "f_mk_pn.htm", "make-random-state" => "f_mk_rnd.htm", "make-string-input-stream" => "f_mk_s_1.htm", "make-string-output-stream" => "f_mk_s_2.htm", \
"make-sequence" => "f_mk_seq.htm", "make-string" => "f_mk_stg.htm", "make-symbol" => "f_mk_sym.htm", "make-synonym-stream" => "f_mk_syn.htm", \
"make-two-way-stream" => "f_mk_two.htm", "mod" => "f_mod_r.htm", "rem" => "f_mod_r.htm", "name-char" => "f_name_c.htm", \
"namestring" => "f_namest.htm", "file-namestring" => "f_namest.htm", "nconc" => "f_nconc.htm", "function" => "next-method-p f_next_m.htm", \
"generic" => "function no-applicable-method f_no_app.htm", "generic" => "function no-next-method f_no_nex.htm", "not" => "f_not.htm", "nthcdr" => "f_nthcdr.htm", \
"nth" => "f_nth.htm", "null" => "f_null.htm", "numerator" => "f_numera.htm", "denominator" => "f_numera.htm", \
"numberp" => "f_nump.htm", "open" => "f_open.htm", "open-stream-p" => "f_open_s.htm", "generic" => "function (setf class-name) f_opsetf.htm", \
"pairlis" => "f_pairli.htm", "parse-namestring" => "f_pars_1.htm", "parse-integer" => "f_parse_.htm", "peek-char" => "f_peek_c.htm", \
"phase" => "f_phase.htm", "package-used-by-list" => "f_pkg__1.htm", "package-error-package" => "f_pkg_er.htm", "package-name" => "f_pkg_na.htm", \
"package-nicknames" => "f_pkg_ni.htm", "packagep" => "f_pkgp.htm", "package-shadowing-symbols" => "f_pkg_sh.htm", "package-use-list" => "f_pkg_us.htm", \
"+" => "f_pl.htm", "pathname-host" => "f_pn_hos.htm", "pathname-device" => "f_pn_hos.htm", "pathname" => "f_pn.htm", \
"pathname-match-p" => "f_pn_mat.htm", "pathnamep" => "f_pnp.htm", "position" => "f_pos_p.htm", "position-if" => "f_pos_p.htm", \
"position-if-not" => "f_pos_p.htm", "pprint-dispatch" => "f_ppr_di.htm", "pprint-fill" => "f_ppr_fi.htm", "pprint-linear" => "f_ppr_fi.htm", \
"pprint-indent" => "f_ppr_in.htm", "pprint-newline" => "f_ppr_nl.htm", "pprint-tab" => "f_ppr_ta.htm", "print-not-readable-object" => "f_pr_not.htm", \
"probe-file" => "f_probe_.htm", "generic" => "function print-object f_pr_obj.htm", "proclaim" => "f_procla.htm", "provide" => "f_provid.htm", \
"require" => "f_provid.htm", "random" => "f_random.htm", "rassoc" => "f_rassoc.htm", "rassoc-if" => "f_rassoc.htm", \
"rassoc-if-not" => "f_rassoc.htm", "rationalp" => "f_rati_1.htm", "rational" => "f_ration.htm", "rationalize" => "f_ration.htm", \
"read-byte" => "f_rd_by.htm", "read-char-no-hang" => "f_rd_c_1.htm", "read-char" => "f_rd_cha.htm", "read-delimited-list" => "f_rd_del.htm", \
"read-from-string" => "f_rd_fro.htm", "read-line" => "f_rd_lin.htm", "read" => "f_rd_rd.htm", "read-preserving-whitespace" => "f_rd_rd.htm", \
"read-sequence" => "f_rd_seq.htm", "readtablep" => "f_rdta_1.htm", "readtable-case" => "f_rdtabl.htm", "realpart" => "f_realpa.htm", \
"imagpart" => "f_realpa.htm", "realp" => "f_realp.htm", "reduce" => "f_reduce.htm", "generic" => "function reinitialize-instance f_reinit.htm", \
"remhash" => "f_remhas.htm", "remprop" => "f_rempro.htm", "replace" => "f_replac.htm", "rest" => "f_rest.htm", \
"revappend" => "f_revapp.htm", "nreconc" => "f_revapp.htm", "reverse" => "f_revers.htm", "nreverse" => "f_revers.htm", \
"remove-duplicates" => "f_rm_dup.htm", "delete-duplicates" => "f_rm_dup.htm", "generic" => "function remove-method f_rm_met.htm", "remove" => "f_rm_rm.htm", \
"remove-if" => "f_rm_rm.htm", "remove-if-not" => "f_rm_rm.htm", "random-state-p" => "f_rnd_st.htm", "rename-file" => "f_rn_fil.htm", \
"rename-package" => "f_rn_pkg.htm", "room" => "f_room.htm", "row-major-aref" => "f_row_ma.htm", "rplaca" => "f_rplaca.htm", \
"rplacd" => "f_rplaca.htm", "restart-name" => "f_rst_na.htm", "substitute" => "f_sbs_s.htm", "substitute-if" => "f_sbs_s.htm", \
"search" => "f_search.htm", "set-dispatch-macro-character" => "f_set__1.htm", "set-difference" => "f_set_di.htm", "nset-difference" => "f_set_di.htm", \
"set-exclusive-or" => "f_set_ex.htm", "nset-exclusive-or" => "f_set_ex.htm", "set" => "f_set.htm", "set-macro-character" => "f_set_ma.htm", \
"get-macro-character" => "f_set_ma.htm", "set-pprint-dispatch" => "f_set_pp.htm", "set-syntax-from-char" => "f_set_sy.htm", "shadow" => "f_shadow.htm", \
"generic" => "function shared-initialize f_shared.htm", "shadowing-import" => "f_shdw_i.htm", "short-site-name" => "f_short_.htm", "long-site-name" => "f_short_.htm", \
"signal" => "f_signal.htm", "signum" => "f_signum.htm", "sin" => "f_sin_c.htm", "cos" => "f_sin_c.htm", \
"tan" => "f_sin_c.htm", "sinh" => "f_sinh_.htm", "cosh" => "f_sinh_.htm", "tanh" => "f_sinh_.htm", \
"asinh" => "f_sinh_.htm", "sleep" => "f_sleep.htm", "/" => "f_sl.htm", "slot-boundp" => "f_slt_bo.htm", \
"slot-exists-p" => "f_slt_ex.htm", "slot-makunbound" => "f_slt_ma.htm", "generic" => "function slot-missing f_slt_mi.htm", "generic" => "function slot-unbound f_slt_un.htm", \
"slot-value" => "f_slt_va.htm", "simple-bit-vector-p" => "f_smp_bt.htm", "simple-condition-format-control" => "f_smp_cn.htm", "simple-string-p" => "f_smp_st.htm", \
"simple-vector-p" => "f_smp_ve.htm", "sort" => "f_sort_.htm", "stable-sort" => "f_sort_.htm", "special-operator-p" => "f_specia.htm", \
"sqrt" => "f_sqrt_.htm", "isqrt" => "f_sqrt_.htm", "standard-char-p" => "f_std_ch.htm", "string=" => "f_stgeq_.htm", \
"string/=" => " f_stgeq_.htm", "string&lt" => "f_stgeq_.htm", "stringp" => "f_stgp.htm", "string-trim" => "f_stg_tr.htm", \
"string-left-trim" => "f_stg_tr.htm", "string-upcase" => "f_stg_up.htm", "string-downcase" => "f_stg_up.htm", "*" => "f_st.htm", \
"stream-element-type" => "f_stm_el.htm", "stream-error-stream" => "f_stm_er.htm", "stream-external-format" => "f_stm_ex.htm", "streamp" => "f_stmp.htm", \
"string" => "f_string.htm", "sublis" => "f_sublis.htm", "nsublis" => "f_sublis.htm", "subseq" => "f_subseq.htm", \
"subsetp" => "f_subset.htm", "subst" => "f_substc.htm", "subst-if" => "f_substc.htm", "subst-if-not" => "f_substc.htm", \
"subtypep" => "f_subtpp.htm", "svref" => "f_svref.htm", "software-type" => "f_sw_tpc.htm", "software-version" => "f_sw_tpc.htm", \
"sxhash" => "f_sxhash.htm", "symbol-function" => "f_symb_1.htm", "symbol-name" => "f_symb_2.htm", "symbol-package" => "f_symb_3.htm", \
"symbol-plist" => "f_symb_4.htm", "symbol-value" => "f_symb_5.htm", "symbolp" => "f_symbol.htm", "synonym-stream-symbol" => "f_syn_st.htm", \
"terpri" => "f_terpri.htm", "fresh-line" => "f_terpri.htm", "truename" => "f_tn.htm", "type-error-datum" => "f_tp_err.htm", \
"type-error-expected-type" => "f_tp_err.htm", "type-of" => "f_tp_of.htm", "tree-equal" => "f_tree_e.htm", "translate-logical-pathname" => "f_tr_log.htm", \
"translate-pathname" => "f_tr_pn.htm", "two-way-stream-input-stream" => "f_two_wa.htm", "typep" => "f_typep.htm", "unbound-slot-instance" => "f_unboun.htm", \
"unexport" => "f_unexpo.htm", "unintern" => "f_uninte.htm", "union" => "f_unionc.htm", "nunion" => "f_unionc.htm", \
"unread-char" => "f_unrd_c.htm", "unuse-package" => "f_unuse_.htm", \
"generic" => "function update-instance-for-redefined-class f_upda_1.htm", \
"generic" => "function update-instance-for-different-class f_update.htm", \
"upgraded-array-element-type" => "f_upgr_1.htm", "upgraded-complex-part-type" => "f_upgrad.htm", "upper-case-p" => "f_upper_.htm", "lower-case-p" => "f_upper_.htm", \
"use-package" => "f_use_pk.htm", "user-homedir-pathname" => "f_user_h.htm", "values-list" => "f_vals_l.htm", "values" => "f_values.htm", \
"vectorp" => "f_vecp.htm", "vector-pop" => "f_vec_po.htm", "vector-push" => "f_vec_ps.htm", "vector-push-extend" => "f_vec_ps.htm", \
"vector" => "f_vector.htm", "warn" => "f_warn.htm", "wild-pathname-p" => "f_wild_p.htm", "write-byte" => "f_wr_by.htm", \
"write-char" => "f_wr_cha.htm", "write" => "f_wr_pr.htm", "prin1" => "f_wr_pr.htm", "print" => "f_wr_pr.htm", \
"pprint" => "f_wr_pr.htm", "write-sequence" => "f_wr_seq.htm", "write-string" => "f_wr_stg.htm", "write-line" => "f_wr_stg.htm", \
"write-to-string" => "f_wr_to_.htm", "prin1-to-string" => "f_wr_to_.htm", "y-or-n-p" => "f_y_or_n.htm", "yes-or-no-p" => "f_y_or_n.htm", \
"zerop" => "f_zerop.htm", "and" => "m_and.htm", "assert" => "m_assert.htm", "macro" => "call-method m_call_m.htm", \
"make-method" => "m_call_m.htm", "case" => "m_case_.htm", "ccase" => "m_case_.htm", "ecase" => "m_case_.htm", \
"check-type" => "m_check_.htm", "cond" => "m_cond.htm", "declaim" => "m_declai.htm", "defclass" => "m_defcla.htm", \
"defconstant" => "m_defcon.htm", "defgeneric" => "m_defgen.htm", "define-symbol-macro" => "m_defi_1.htm", "define-modify-macro" => "m_defi_2.htm", \
"define-setf-expander" => "m_defi_3.htm", "define-method-combination" => "m_defi_4.htm", "define-condition" => "m_defi_5.htm", "define-compiler-macro" => "m_define.htm", \
"defmacro" => "m_defmac.htm", "defmethod" => "m_defmet.htm", "defparameter" => "m_defpar.htm", "defvar" => "m_defpar.htm", \
"defpackage" => "m_defpkg.htm", "defsetf" => "m_defset.htm", "defstruct" => "m_defstr.htm", "deftype" => "m_deftp.htm", \
"defun" => "m_defun.htm", "destructuring-bind" => "m_destru.htm", "do" => "m_do_do.htm", "do*" => "m_do_do.htm", \
"dolist" => "m_dolist.htm", "do-symbols" => "m_do_sym.htm", "do-external-symbols" => "m_do_sym.htm", "dotimes" => "m_dotime.htm", \
"formatter" => "m_format.htm", "handler-case" => "m_hand_1.htm", "handler-bind" => "m_handle.htm", "ignore-errors" => "m_ignore.htm", \
"incf" => "m_incf_.htm", "decf" => "m_incf_.htm", "in-package" => "m_in_pkg.htm", "lambda" => "m_lambda.htm", \
"macro" => "loop-finish m_loop_f.htm", "loop" => "m_loop.htm", "multiple-value-list" => "m_mult_1.htm", "multiple-value-setq" => "m_mult_2.htm", \
"multiple-value-bind" => "m_multip.htm", "nth-value" => "m_nth_va.htm", "or" => "m_or.htm", "pop" => "m_pop.htm", \
"macro" => "pprint-exit-if-list-exhausted m_ppr_ex.htm", "pprint-logical-block" => "m_ppr_lo.htm", "macro" => "pprint-pop m_ppr_po.htm", "prog1" => "m_prog1c.htm", \
"prog2" => "m_prog1c.htm", "prog" => "m_prog_.htm", "prog*" => "m_prog_.htm", "print-unreadable-object" => "m_pr_unr.htm", \
"psetq" => "m_psetq.htm", "pushnew" => "m_pshnew.htm", "push" => "m_push.htm", "remf" => "m_remf.htm", \
"return" => "m_return.htm", "rotatef" => "m_rotate.htm", "restart-bind" => "m_rst_bi.htm", "restart-case" => "m_rst_ca.htm", \
"setf" => "m_setf_.htm", "psetf" => "m_setf_.htm", "shiftf" => "m_shiftf.htm", "step" => "m_step.htm", \
"time" => "m_time.htm", "typecase" => "m_tpcase.htm", "ctypecase" => "m_tpcase.htm", "etypecase" => "m_tpcase.htm", \
"trace" => "m_tracec.htm", "untrace" => "m_tracec.htm", "with-accessors" => "m_w_acce.htm", "with-condition-restarts" => "m_w_cnd_.htm", \
"with-compilation-unit" => "m_w_comp.htm", "with-hash-table-iterator" => "m_w_hash.htm", "when" => "m_when_.htm", "unless" => "m_when_.htm", \
"with-input-from-string" => "m_w_in_f.htm", "with-open-stream" => "m_w_op_1.htm", "with-open-file" => "m_w_open.htm", "with-output-to-string" => "m_w_out_.htm", \
"with-package-iterator" => "m_w_pkg_.htm", "with-slots" => "m_w_slts.htm", "with-simple-restart" => "m_w_smp_.htm", "with-standard-io-syntax" => "m_w_std_.htm", \
"abort" => "r_abort.htm", "continue" => "r_contin.htm", "muffle-warning" => "r_muffle.htm", "store-value" => "r_store_.htm", \
"use-value" => "r_use_va.htm", "operator" => "block s_block.htm", "operator" => "catch s_catch.htm", "declare" => "s_declar.htm", \
"operator" => "eval-when s_eval_w.htm", "operator" => "flet s_flet_.htm", "labels" => "s_flet_.htm", "macrolet" => "s_flet_.htm", \
"operator" => "function s_fn.htm", "operator" => "go s_go.htm", "operator" => "if s_if.htm", "lambda" => "s_lambda.htm", \
"operator" => "load-time-value s_ld_tim.htm", "operator" => "let s_let_l.htm", "let*" => "s_let_l.htm", "operator" => "locally s_locall.htm", \
"operator" => "multiple-value-prog1 s_mult_1.htm", "operator" => "multiple-value-call s_multip.htm", "operator" => "progn s_progn.htm", "operator" => "progv s_progv.htm", \
"operator" => "quote s_quote.htm", "operator" => "return-from s_ret_fr.htm", "form" => "setq s_setq.htm", "operator" => "symbol-macrolet s_symbol.htm", \
"operator" => "tagbody s_tagbod.htm", "operator" => "the s_the.htm", "operator" => "throw s_throw.htm", "operator" => "unwind-protect s_unwind.htm", \
"specifier" => "and t_and.htm", "class" => "array t_array.htm", "atom" => "t_atom.htm", "boolean" => "t_ban.htm", \
"base-char" => "t_base_c.htm", "base-string" => "t_base_s.htm", "bignum" => "t_bignum.htm", "bit" => "t_bit.htm", \
"class" => "broadcast-stream t_broadc.htm", "class" => "bit-vector t_bt_vec.htm", "class" => "built-in-class t_built_.htm", "class" => "character t_ch.htm", \
"class" => "class t_class.htm", "compiled-function" => "t_cmpd_f.htm", "class" => "complex t_comple.htm", "class" => "concatenated-stream t_concat.htm", \
"class" => "cons t_cons.htm", "class" => "echo-stream t_echo_s.htm", "specifier" => "eql t_eql.htm", "extended-char" => "t_extend.htm", \
"class" => "file-stream t_file_s.htm", "fixnum" => "t_fixnum.htm", "class" => "float t_float.htm", "class" => "function t_fn.htm", \
"class" => "generic-function t_generi.htm", "class" => "hash-table t_hash_t.htm", "class" => "integer t_intege.htm", "keyword" => "t_kwd.htm", \
"class" => "list t_list.htm", "class" => "logical-pathname t_logica.htm", "specifier" => "member t_member.htm", "class" => "method-combination t_meth_1.htm", \
"class" => "method t_method.htm", "specifier" => "mod t_mod.htm", "nil" => "t_nil.htm", "specifier" => "not t_not.htm", \
"class" => "null t_null.htm", "class" => "number t_number.htm", "specifier" => "or t_or.htm", "class" => "package t_pkg.htm", \
"class" => "pathname t_pn.htm", "class" => "ratio t_ratio.htm", "class" => "rational t_ration.htm", "class" => "readtable t_rdtabl.htm", \
"class" => "real t_real.htm", "class" => "random-state t_rnd_st.htm", "class" => "restart t_rst.htm", "specifier" => "satisfies t_satisf.htm", \
"class" => "sequence t_seq.htm", "signed-byte" => "t_sgn_by.htm", "short-float" => "t_short_.htm", "single-float" => "t_short_.htm", \
"double-float" => "t_short_.htm", "simple-array" => "t_smp_ar.htm", "simple-base-string" => "t_smp_ba.htm", "simple-bit-vector" => "t_smp_bt.htm", \
"simple-string" => "t_smp_st.htm", "simple-vector" => "t_smp_ve.htm", "standard-char" => "t_std_ch.htm", "class" => "standard-class t_std_cl.htm", \
"class" => "standard-generic-function t_std_ge.htm", "class" => "standard-method t_std_me.htm", "standard-object" => "t_std_ob.htm", "class" => "string-stream t_stg_st.htm", \
"class" => "stream t_stream.htm", "class" => "string t_string.htm", "class" => "structure-class t_stu_cl.htm", "structure-object" => "t_stu_ob.htm", \
"class" => "symbol t_symbol.htm", "class" => "synonym-stream t_syn_st.htm", "class" => "t t_t.htm", "class" => "two-way-stream t_two_wa.htm", \
"unsigned-byte" => "t_unsgn_.htm", "specifier" => "values t_values.htm", "class" => "vector t_vector.htm", "array-dimension-limit" => "v_ar_dim.htm", \
"array-rank-limit" => "v_ar_ran.htm", "array-total-size-limit" => "v_ar_tot.htm", "boole-1" => "v_b_1_b.htm", "boole-2" => "v_b_1_b.htm", \
"*break-on-signals*" => "v_break_.htm", "call-arguments-limit" => "v_call_a.htm", "char-code-limit" => "v_char_c.htm", "*compile-file-pathname*" => "v_cmp_fi.htm", \
"*compile-print*" => "v_cmp_pr.htm", "*compile-verbose*" => "v_cmp_pr.htm", "*debugger-hook*" => "v_debugg.htm", "*debug-io*" => "v_debug_.htm", \
"*error-output*" => "v_debug_.htm", "*default-pathname-defaults*" => "v_defaul.htm", "*features*" => "v_featur.htm", "*gensym-counter*" => "v_gensym.htm", \
"internal-time-units-per-second" => "v_intern.htm", "lambda-parameters-limit" => "v_lamb_1.htm", "lambda-list-keywords" => "v_lambda.htm", "*load-pathname*" => "v_ld_pns.htm", \
"*load-truename*" => "v_ld_pns.htm", "*load-print*" => "v_ld_prs.htm", "*load-verbose*" => "v_ld_prs.htm", "*macroexpand-hook*" => "v_mexp_h.htm", \
"*modules*" => "v_module.htm", "most-positive-short-float" => "v_most_1.htm", "most-positive-fixnum" => "v_most_p.htm", "multiple-values-limit" => "v_multip.htm", \
"nil" => "v_nil.htm", "pi" => "v_pi.htm", "*package*" => "v_pkg.htm", "+" => "v_pl_plp.htm", \
"++" => "v_pl_plp.htm", "+++" => "v_pl_plp.htm", "*print-array*" => "v_pr_ar.htm", "*print-base*" => "v_pr_bas.htm", \
"*print-radix*" => "v_pr_bas.htm", "*print-case*" => "v_pr_cas.htm", "*print-circle*" => "v_pr_cir.htm", "*print-escape*" => "v_pr_esc.htm", \
"*print-gensym*" => "v_pr_gen.htm", "*print-level*" => "v_pr_lev.htm", "*print-length*" => "v_pr_lev.htm", "*print-lines*" => "v_pr_lin.htm", \
"*print-miser-width*" => "v_pr_mis.htm", "*print-pprint-dispatch*" => "v_pr_ppr.htm", "*print-pretty*" => "v_pr_pre.htm", "*print-readably*" => "v_pr_rda.htm", \
"*print-right-margin*" => "v_pr_rig.htm", "*read-base*" => "v_rd_bas.htm", "*read-default-float-format*" => "v_rd_def.htm", "*read-eval*" => "v_rd_eva.htm", \
"*read-suppress*" => "v_rd_sup.htm", "*readtable*" => "v_rdtabl.htm", "*random-state*" => "v_rnd_st.htm", "short-float-epsilon" => "v_short_.htm", \
"/" => "v_sl_sls.htm", "//" => "v_sl_sls.htm", "///" => "v_sl_sls.htm", "*" => "v__stst_.htm", \
"**" => "v__stst_.htm", "***" => "v__stst_.htm", "*terminal-io*" => "v_termin.htm", "t" => "v_t.htm"}

symbol=ARGV[0]
page = $map[symbol]
url= "file:///home/alex/repos/doc/HyperSpec/Body/#{page}"
exec("lynx -vikeys #{url}")


