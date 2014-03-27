
(defvar systemc-mode-hook nil)

;; define several class of keywords
(setq systemc-keywords '("sc_clock" "sc_in_clk" "sc_reset" "sc_buffer" "sc_fifo" "sc_fifo_in" "sc_fifo_out" "sc_fifo_nonblocking_in_if" "sc_fifo_blocking_in_if" "sc_fifo_in_if" "sc_fifo_nonblocking_out_if" "sc_fifo_blocking_out_if" "sc_fifo_out_if" "sc_event" "sc_event_queue" "sc_event_and_list" "sc_event_or_list" "sc_in" "sc_inout" "sc_out" "sc_in_rv" "sc_inout_rv" "sc_out_rv" "sc_signal" "sc_channel" "sc_port" "sc_export" "sc_interface" "sc_channel" "sc_prim_channel" "sc_semaphore" "sc_semaphore_if" "sc_mutex" "sc_mutex_if" "sc_host_mutex" "sc_process_handle" "sc_unwind_exception"))

(setq systemc-types '("sc_object" "sc_attribute" "sc_attr_cltn" "sc_module" "sc_module_name" "sc_spawn" "sc_spawn_options" "sc_time" "sc_bit" "sc_bv" "sc_logic" "sc_lv" "sc_signed" "sc_unsigned" "sc_int" "sc_uint" "sc_bigint" "sc_biguint" "sc_fix" "sc_fixed" "sc_ufix" "sc_ufixed" "sc_fxval" "sc_fxnum" "sc_fix_fast" "sc_fixed_fast" "sc_ufix_fast" "sc_ufixed_fast" "sc_fxval_fast" "sc_fxnum_fast" "SC_ZERO_TIME" "SC_FS" "SC_PS" "SC_NS" "SC_US" "SC_MS" "SC_SEC" "SC_DEC" "SC_CSD" "SC_BIN" "SC_BIN_US" "SC_BIN_SM" "SC_OCT" "SC_OCT_US" "SC_OCT_SM" "SC_HEX" "SC_HEX_US" "SC_HEX_SM" "SC_LOGIC_0" "SC_LOGIC_1" "SC_LOGIC_Z" "SC_LOGIC_X"))

(setq systemc-functions '("get_value()" "post()" "trywait()" "kind()" "unlock()" "trylock()" "lock()" "num_available()" "negedge_event()" "posedge_event()" "default_event()" "value_change_event()" "duty_cycle()" "name()" "valid()" "proc_kind()" "period()" "negedge()" "posedge()" "neg()" "pos()" "event()" "suspend()" "resume()" "sync_reset_on()" "sync_reset_off()" "reset()" "reset_event()" "is_reset()" "kill()" "terminated()" "terminated_event()" "throw_it" "notify()" "to_bool()" "to_char()" "to_int()" "to_uint()" "to_short()" "to_ushort()" "to_long()" "to_ulong()" "to_int64()" "to_uint64()" "to_double" "to_float()" "to_dec()" "to_bin()" "to_oct()" "to_hex()" "to_and_reduce()" "to_nand_reduce()" "to_or_reduce()" "to_nor_reduce()" "to_xor_reduce()" "to_xnor_reduce()" "reset_signal_is" "async_reset_signal_is" "initialize" "dont_initialize" "next_trigger" "wait" "before_of_elaboration" "end_of_elaboration" "start_of_simulation" "end_of_simulation" "sc_time_stamp" "sc_main" "sc_start" "sc_stop" "sc_create_vcd_trace_file" "sc_close_vcd_trace_file" "sc_trace" "sc_trace_file" "sc_write_comment" "sc_set_time_resolution" "sc_set_default_time_unit" "sc_get_default_time_unit" "sc_get_time_resolution" "sc_assert" "sc_bind" "sc_get_current_process_handle" "sc_is_unwinding" "sc_gen_unique_name" "sensitive" "sensitive_neg" "sensitive_pos" "to_string"))

(setq systemc-macro '("IEEE_1666_SYSTEMC" "SC_VERSION_MAJOR" "SC_VERSION_MINOR" "SC_VERSION_PATCH" "SC_VERSION_ORIGINATOR" "SC_VERSION_RELEASE_DATE" "SC_VERSION_PRERELEASE" "SC_VERSION" "SC_COPYRIGHT" "SC_IS_PRERELEASE" "SC_DEFAULT_STACK_SIZE" "SC_RUN_TO_TIME" "SC_EXIT_ON_STARVATION" "SC_STOP_FINISH_DELTA" "SC_STOP_IMMEDIATE" "SC_ELABORATION" "SC_BEFORE_END_OF_ELABORATION" "SC_START_OF_SIMULATION" "SC_RUNNING" "SC_PAUSED" "SC_STOPPED" "SC_END_OF_SIMULATION" "SC_FORK" "SC_JOIN" "SC_PROTOCOL" "SC_CTHREAD" "SC_THREAD" "SC_CTOR" "SC_METHOD" "SC_MODULE" "SC_HAS_PROCESS" "SC_REPORT_INFO" "SC_REPORT_WARNING" "SC_REPORT_ERROR" "SC_REPORT_FATAL" "SC_REPORT_INFO_VERB" "SC_INFO" "SC_WARNING" "SC_ERROR" "SC_FATAL" "SC_MAX_SEVERITY" "SC_NONE" "SC_LOW" "SC_MEDIUM" "SC_HIGH" "SC_FULL" "SC_DEBUG" "SC_UNSPECIFIED" "SC_DO_NOTHING" "SC_THROW" "SC_LOG" "SC_DISPLAY" "SC_CACHE_REPORT" "SC_INTERRUPT" "SC_STOP" "SC_ABORT"))

;; create the regex string for each class of keywords
(setq systemc-keywords-regexp (regexp-opt systemc-keywords 'words))
(setq systemc-type-regexp (regexp-opt systemc-types 'words))
(setq systemc-functions-regexp (regexp-opt systemc-functions 'words))
(setq systemc-macro-regexp (regexp-opt systemc-macro 'words))

;; clear memory
(setq systemc-keywords nil)
(setq systemc-types nil)
(setq systemc-functions nil)
(setq systemc-macro nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
;; font-lock-variable-name-face font-lock-builtin-face
;; font-lock-keyword-face font-lock-constant-face
(setq systemc-font-lock-keywords
  `(
    (,systemc-type-regexp . font-lock-type-face)
    (,systemc-functions-regexp . font-lock-function-name-face)
    (,systemc-keywords-regexp . font-lock-keyword-face)
    (,systemc-macro-regexp . font-lock-constant-face)
))

; (define-derived-mode systemc-mode c++-mode
(define-derived-mode systemc-mode c++-mode "SystemC"
  "Major mode for editing SystemC Language"
  ;; code for syntax highlighting
  ;; (setq font-lock-defaults '((systemc-font-lock-keywords)))
  (setq-local font-lock-defaults
    '(systemc-font-lock-keywords))
  ;; clear memory
  (setq systemc-keywords-regexp nil)
  (setq systemc-types-regexp nil)
  (setq systemc-functions-regexp nil)
  (setq systemc-macro-regexp nil)

  (setq mode-name "SystemC")
  (run-hooks 'systemc-mode-hook)
)




(provide 'systemc-mode)
