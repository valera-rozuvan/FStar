open Prims
let tc_one_file remaining uenv use_cache =
  let uu____32 = uenv in
  match uu____32 with
  | (dsenv,env) ->
      let uu____47 =
        match remaining with
        | intf::impl::remaining1 when
            FStar_Universal.needs_interleaving intf impl ->
            let uu____70 =
              FStar_Universal.tc_one_file_and_intf (Some intf) impl dsenv env
                use_cache in
            (match uu____70 with
             | (uu____87,dsenv1,env1,use_cache1) ->
                 (((Some intf), impl), dsenv1, env1, remaining1, use_cache1))
        | intf_or_impl::remaining1 ->
            let uu____105 =
              FStar_Universal.tc_one_file_and_intf None intf_or_impl dsenv
                env use_cache in
            (match uu____105 with
             | (uu____122,dsenv1,env1,use_cache1) ->
                 ((None, intf_or_impl), dsenv1, env1, remaining1, use_cache1))
        | [] -> failwith "Impossible" in
      (match uu____47 with
       | ((intf,impl),dsenv1,env1,remaining1,use_cache1) ->
           ((intf, impl), (dsenv1, env1), None, remaining1, use_cache1))
let tc_prims:
  Prims.unit -> (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env) =
  fun uu____183  ->
    let uu____184 = FStar_Universal.tc_prims () in
    match uu____184 with | (uu____192,dsenv,env) -> (dsenv, env)
type env_t = (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env)
type modul_t = FStar_Syntax_Syntax.modul Prims.option
type stack_t = (env_t* modul_t) Prims.list
let pop uu____217 msg =
  match uu____217 with
  | (uu____221,env) ->
      (FStar_Universal.pop_context env msg; FStar_Options.pop ())
let push:
  (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env) ->
    Prims.bool ->
      Prims.bool ->
        Prims.string -> (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env)
  =
  fun uu____236  ->
    fun lax1  ->
      fun restore_cmd_line_options1  ->
        fun msg  ->
          match uu____236 with
          | (dsenv,env) ->
              let env1 =
                let uu___220_247 = env in
                {
                  FStar_TypeChecker_Env.solver =
                    (uu___220_247.FStar_TypeChecker_Env.solver);
                  FStar_TypeChecker_Env.range =
                    (uu___220_247.FStar_TypeChecker_Env.range);
                  FStar_TypeChecker_Env.curmodule =
                    (uu___220_247.FStar_TypeChecker_Env.curmodule);
                  FStar_TypeChecker_Env.gamma =
                    (uu___220_247.FStar_TypeChecker_Env.gamma);
                  FStar_TypeChecker_Env.gamma_cache =
                    (uu___220_247.FStar_TypeChecker_Env.gamma_cache);
                  FStar_TypeChecker_Env.modules =
                    (uu___220_247.FStar_TypeChecker_Env.modules);
                  FStar_TypeChecker_Env.expected_typ =
                    (uu___220_247.FStar_TypeChecker_Env.expected_typ);
                  FStar_TypeChecker_Env.sigtab =
                    (uu___220_247.FStar_TypeChecker_Env.sigtab);
                  FStar_TypeChecker_Env.is_pattern =
                    (uu___220_247.FStar_TypeChecker_Env.is_pattern);
                  FStar_TypeChecker_Env.instantiate_imp =
                    (uu___220_247.FStar_TypeChecker_Env.instantiate_imp);
                  FStar_TypeChecker_Env.effects =
                    (uu___220_247.FStar_TypeChecker_Env.effects);
                  FStar_TypeChecker_Env.generalize =
                    (uu___220_247.FStar_TypeChecker_Env.generalize);
                  FStar_TypeChecker_Env.letrecs =
                    (uu___220_247.FStar_TypeChecker_Env.letrecs);
                  FStar_TypeChecker_Env.top_level =
                    (uu___220_247.FStar_TypeChecker_Env.top_level);
                  FStar_TypeChecker_Env.check_uvars =
                    (uu___220_247.FStar_TypeChecker_Env.check_uvars);
                  FStar_TypeChecker_Env.use_eq =
                    (uu___220_247.FStar_TypeChecker_Env.use_eq);
                  FStar_TypeChecker_Env.is_iface =
                    (uu___220_247.FStar_TypeChecker_Env.is_iface);
                  FStar_TypeChecker_Env.admit =
                    (uu___220_247.FStar_TypeChecker_Env.admit);
                  FStar_TypeChecker_Env.lax = lax1;
                  FStar_TypeChecker_Env.lax_universes =
                    (uu___220_247.FStar_TypeChecker_Env.lax_universes);
                  FStar_TypeChecker_Env.type_of =
                    (uu___220_247.FStar_TypeChecker_Env.type_of);
                  FStar_TypeChecker_Env.universe_of =
                    (uu___220_247.FStar_TypeChecker_Env.universe_of);
                  FStar_TypeChecker_Env.use_bv_sorts =
                    (uu___220_247.FStar_TypeChecker_Env.use_bv_sorts);
                  FStar_TypeChecker_Env.qname_and_index =
                    (uu___220_247.FStar_TypeChecker_Env.qname_and_index)
                } in
              let res = FStar_Universal.push_context (dsenv, env1) msg in
              (FStar_Options.push ();
               if restore_cmd_line_options1
               then
                 (let uu____253 =
                    FStar_Options.restore_cmd_line_options false in
                  FStar_All.pipe_right uu____253 Prims.ignore)
               else ();
               res)
let mark:
  (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env) ->
    (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env)
  =
  fun uu____261  ->
    match uu____261 with
    | (dsenv,env) ->
        let dsenv1 = FStar_ToSyntax_Env.mark dsenv in
        let env1 = FStar_TypeChecker_Env.mark env in
        (FStar_Options.push (); (dsenv1, env1))
let reset_mark uu____281 =
  match uu____281 with
  | (uu____284,env) ->
      let dsenv = FStar_ToSyntax_Env.reset_mark () in
      let env1 = FStar_TypeChecker_Env.reset_mark env in
      (FStar_Options.pop (); (dsenv, env1))
let cleanup uu____297 =
  match uu____297 with
  | (dsenv,env) -> FStar_TypeChecker_Env.cleanup_interactive env
let commit_mark:
  (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env) ->
    (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env)
  =
  fun uu____308  ->
    match uu____308 with
    | (dsenv,env) ->
        let dsenv1 = FStar_ToSyntax_Env.commit_mark dsenv in
        let env1 = FStar_TypeChecker_Env.commit_mark env in (dsenv1, env1)
let check_frag:
  (FStar_ToSyntax_Env.env* FStar_TypeChecker_Env.env) ->
    FStar_Syntax_Syntax.modul Prims.option ->
      FStar_Parser_ParseIt.input_frag ->
        (FStar_Syntax_Syntax.modul Prims.option* (FStar_ToSyntax_Env.env*
          FStar_TypeChecker_Env.env)* Prims.int) Prims.option
  =
  fun uu____333  ->
    fun curmod  ->
      fun text1  ->
        match uu____333 with
        | (dsenv,env) ->
            (try
               let uu____363 =
                 FStar_Universal.tc_one_fragment curmod dsenv env text1 in
               match uu____363 with
               | Some (m,dsenv1,env1) ->
                   let uu____385 =
                     let uu____392 = FStar_Errors.get_err_count () in
                     (m, (dsenv1, env1), uu____392) in
                   Some uu____385
               | uu____402 -> None
             with
             | FStar_Errors.Error (msg,r) when
                 let uu____424 = FStar_Options.trace_error () in
                 Prims.op_Negation uu____424 ->
                 (FStar_TypeChecker_Err.add_errors env [(msg, r)]; None)
             | FStar_Errors.Err msg when
                 let uu____437 = FStar_Options.trace_error () in
                 Prims.op_Negation uu____437 ->
                 ((let uu____439 =
                     let uu____443 =
                       let uu____446 = FStar_TypeChecker_Env.get_range env in
                       (msg, uu____446) in
                     [uu____443] in
                   FStar_TypeChecker_Err.add_errors env uu____439);
                  None))
let report_fail: Prims.unit -> Prims.unit =
  fun uu____459  ->
    (let uu____461 = FStar_Errors.report_all () in
     FStar_All.pipe_right uu____461 Prims.ignore);
    FStar_ST.write FStar_Errors.num_errs (Prims.parse_int "0")
type input_chunks =
  | Push of (Prims.bool* Prims.int* Prims.int)
  | Pop of Prims.string
  | Code of (Prims.string* (Prims.string* Prims.string))
  | Info of (Prims.string* Prims.bool* (Prims.string* Prims.int* Prims.int)
  Prims.option)
  | Completions of Prims.string
let uu___is_Push: input_chunks -> Prims.bool =
  fun projectee  ->
    match projectee with | Push _0 -> true | uu____503 -> false
let __proj__Push__item___0:
  input_chunks -> (Prims.bool* Prims.int* Prims.int) =
  fun projectee  -> match projectee with | Push _0 -> _0
let uu___is_Pop: input_chunks -> Prims.bool =
  fun projectee  ->
    match projectee with | Pop _0 -> true | uu____524 -> false
let __proj__Pop__item___0: input_chunks -> Prims.string =
  fun projectee  -> match projectee with | Pop _0 -> _0
let uu___is_Code: input_chunks -> Prims.bool =
  fun projectee  ->
    match projectee with | Code _0 -> true | uu____540 -> false
let __proj__Code__item___0:
  input_chunks -> (Prims.string* (Prims.string* Prims.string)) =
  fun projectee  -> match projectee with | Code _0 -> _0
let uu___is_Info: input_chunks -> Prims.bool =
  fun projectee  ->
    match projectee with | Info _0 -> true | uu____571 -> false
let __proj__Info__item___0:
  input_chunks ->
    (Prims.string* Prims.bool* (Prims.string* Prims.int* Prims.int)
      Prims.option)
  = fun projectee  -> match projectee with | Info _0 -> _0
let uu___is_Completions: input_chunks -> Prims.bool =
  fun projectee  ->
    match projectee with | Completions _0 -> true | uu____604 -> false
let __proj__Completions__item___0: input_chunks -> Prims.string =
  fun projectee  -> match projectee with | Completions _0 -> _0
type interactive_state =
  {
  chunk: FStar_Util.string_builder;
  stdin: FStar_Util.stream_reader Prims.option FStar_ST.ref;
  buffer: input_chunks Prims.list FStar_ST.ref;
  log: FStar_Util.file_handle Prims.option FStar_ST.ref;}
let the_interactive_state: interactive_state =
  let uu____667 = FStar_Util.new_string_builder () in
  let uu____668 = FStar_Util.mk_ref None in
  let uu____673 = FStar_Util.mk_ref [] in
  let uu____678 = FStar_Util.mk_ref None in
  { chunk = uu____667; stdin = uu____668; buffer = uu____673; log = uu____678
  }
let rec read_chunk: Prims.unit -> input_chunks =
  fun uu____691  ->
    let s = the_interactive_state in
    let log =
      let uu____696 = FStar_Options.debug_any () in
      if uu____696
      then
        let transcript =
          let uu____700 = FStar_ST.read s.log in
          match uu____700 with
          | Some transcript -> transcript
          | None  ->
              let transcript = FStar_Util.open_file_for_writing "transcript" in
              (FStar_ST.write s.log (Some transcript); transcript) in
        fun line  ->
          (FStar_Util.append_to_file transcript line;
           FStar_Util.flush_file transcript)
      else (fun uu____714  -> ()) in
    let stdin =
      let uu____716 = FStar_ST.read s.stdin in
      match uu____716 with
      | Some i -> i
      | None  ->
          let i = FStar_Util.open_stdin () in
          (FStar_ST.write s.stdin (Some i); i) in
    let line =
      let uu____728 = FStar_Util.read_line stdin in
      match uu____728 with
      | None  -> FStar_All.exit (Prims.parse_int "0")
      | Some l -> l in
    log line;
    (let l = FStar_Util.trim_string line in
     if FStar_Util.starts_with l "#end"
     then
       let responses =
         match FStar_Util.split l " " with
         | uu____738::ok::fail1::[] -> (ok, fail1)
         | uu____741 -> ("ok", "fail") in
       let str = FStar_Util.string_of_string_builder s.chunk in
       (FStar_Util.clear_string_builder s.chunk; Code (str, responses))
     else
       if FStar_Util.starts_with l "#pop"
       then (FStar_Util.clear_string_builder s.chunk; Pop l)
       else
         if FStar_Util.starts_with l "#push"
         then
           (FStar_Util.clear_string_builder s.chunk;
            (let lc_lax =
               let uu____752 =
                 FStar_Util.substring_from l (FStar_String.length "#push") in
               FStar_Util.trim_string uu____752 in
             let lc =
               match FStar_Util.split lc_lax " " with
               | l1::c::"#lax"::[] ->
                   let uu____764 = FStar_Util.int_of_string l1 in
                   let uu____765 = FStar_Util.int_of_string c in
                   (true, uu____764, uu____765)
               | l1::c::[] ->
                   let uu____768 = FStar_Util.int_of_string l1 in
                   let uu____769 = FStar_Util.int_of_string c in
                   (false, uu____768, uu____769)
               | uu____770 ->
                   (FStar_Util.print_warning
                      (Prims.strcat
                         "Error locations may be wrong, unrecognized string after #push: "
                         lc_lax);
                    (false, (Prims.parse_int "1"), (Prims.parse_int "0"))) in
             Push lc))
         else
           if FStar_Util.starts_with l "#info "
           then
             (match FStar_Util.split l " " with
              | uu____774::symbol::[] ->
                  (FStar_Util.clear_string_builder s.chunk;
                   Info (symbol, true, None))
              | uu____784::symbol::file::row::col::[] ->
                  (FStar_Util.clear_string_builder s.chunk;
                   (let uu____790 =
                      let uu____798 =
                        let uu____803 =
                          let uu____807 = FStar_Util.int_of_string row in
                          let uu____808 = FStar_Util.int_of_string col in
                          (file, uu____807, uu____808) in
                        Some uu____803 in
                      (symbol, false, uu____798) in
                    Info uu____790))
              | uu____816 ->
                  (FStar_Util.print_error
                     (Prims.strcat "Unrecognized \"#info\" request: " l);
                   FStar_All.exit (Prims.parse_int "1")))
           else
             if FStar_Util.starts_with l "#completions "
             then
               (match FStar_Util.split l " " with
                | uu____820::prefix1::"#"::[] ->
                    (FStar_Util.clear_string_builder s.chunk;
                     Completions prefix1)
                | uu____823 ->
                    (FStar_Util.print_error
                       (Prims.strcat
                          "Unrecognized \"#completions\" request: " l);
                     FStar_All.exit (Prims.parse_int "1")))
             else
               if l = "#finish"
               then FStar_All.exit (Prims.parse_int "0")
               else
                 (FStar_Util.string_builder_append s.chunk line;
                  FStar_Util.string_builder_append s.chunk "\n";
                  read_chunk ()))
let shift_chunk: Prims.unit -> input_chunks =
  fun uu____832  ->
    let s = the_interactive_state in
    let uu____834 = FStar_ST.read s.buffer in
    match uu____834 with
    | [] -> read_chunk ()
    | chunk::chunks -> (FStar_ST.write s.buffer chunks; chunk)
let fill_buffer: Prims.unit -> Prims.unit =
  fun uu____848  ->
    let s = the_interactive_state in
    let uu____850 =
      let uu____852 = FStar_ST.read s.buffer in
      let uu____857 = let uu____859 = read_chunk () in [uu____859] in
      FStar_List.append uu____852 uu____857 in
    FStar_ST.write s.buffer uu____850
let deps_of_our_file:
  Prims.string -> (Prims.string Prims.list* Prims.string Prims.option) =
  fun filename  ->
    let deps =
      FStar_Dependencies.find_deps_if_needed
        FStar_Parser_Dep.VerifyFigureItOut [filename] in
    let uu____872 =
      FStar_List.partition
        (fun x  ->
           let uu____878 = FStar_Parser_Dep.lowercase_module_name x in
           let uu____879 = FStar_Parser_Dep.lowercase_module_name filename in
           uu____878 <> uu____879) deps in
    match uu____872 with
    | (deps1,same_name) ->
        let maybe_intf =
          match same_name with
          | intf::impl::[] ->
              ((let uu____896 =
                  (let uu____897 = FStar_Parser_Dep.is_interface intf in
                   Prims.op_Negation uu____897) ||
                    (let uu____898 = FStar_Parser_Dep.is_implementation impl in
                     Prims.op_Negation uu____898) in
                if uu____896
                then
                  let uu____899 =
                    FStar_Util.format2
                      "Found %s and %s but not an interface + implementation"
                      intf impl in
                  FStar_Util.print_warning uu____899
                else ());
               Some intf)
          | impl::[] -> None
          | uu____902 ->
              ((let uu____905 =
                  FStar_Util.format1 "Unexpected: ended up with %s"
                    (FStar_String.concat " " same_name) in
                FStar_Util.print_warning uu____905);
               None) in
        (deps1, maybe_intf)
type m_timestamps =
  (Prims.string Prims.option* Prims.string* FStar_Util.time Prims.option*
    FStar_Util.time) Prims.list
let rec tc_deps:
  modul_t ->
    stack_t ->
      env_t ->
        Prims.string Prims.list ->
          m_timestamps -> Prims.bool -> (stack_t* env_t* m_timestamps)
  =
  fun m  ->
    fun stack  ->
      fun env  ->
        fun remaining  ->
          fun ts  ->
            fun use_cache  ->
              match remaining with
              | [] -> (stack, env, ts)
              | uu____941 ->
                  let stack1 = (env, m) :: stack in
                  let env1 =
                    let uu____952 = FStar_Options.lax () in
                    push env uu____952 true "typecheck_modul" in
                  let uu____953 = tc_one_file remaining env1 use_cache in
                  (match uu____953 with
                   | ((intf,impl),env2,modl,remaining1,use_cache1) ->
                       let uu____988 =
                         let intf_t =
                           match intf with
                           | Some intf1 ->
                               let uu____996 =
                                 FStar_Util.get_file_last_modification_time
                                   intf1 in
                               Some uu____996
                           | None  -> None in
                         let impl_t =
                           FStar_Util.get_file_last_modification_time impl in
                         (intf_t, impl_t) in
                       (match uu____988 with
                        | (intf_t,impl_t) ->
                            tc_deps m stack1 env2 remaining1
                              ((intf, impl, intf_t, impl_t) :: ts) use_cache1))
let update_deps:
  Prims.string ->
    modul_t ->
      stack_t -> env_t -> m_timestamps -> (stack_t* env_t* m_timestamps)
  =
  fun filename  ->
    fun m  ->
      fun stk  ->
        fun env  ->
          fun ts  ->
            let is_stale intf impl intf_t impl_t =
              let impl_mt = FStar_Util.get_file_last_modification_time impl in
              (FStar_Util.is_before impl_t impl_mt) ||
                (match (intf, intf_t) with
                 | (Some intf1,Some intf_t1) ->
                     let intf_mt =
                       FStar_Util.get_file_last_modification_time intf1 in
                     FStar_Util.is_before intf_t1 intf_mt
                 | (None ,None ) -> false
                 | (uu____1062,uu____1063) ->
                     failwith
                       "Impossible, if the interface is None, the timestamp entry should also be None") in
            let rec iterate depnames st env' ts1 good_stack good_ts =
              let match_dep depnames1 intf impl =
                match intf with
                | None  ->
                    (match depnames1 with
                     | dep1::depnames' ->
                         if dep1 = impl
                         then (true, depnames')
                         else (false, depnames1)
                     | uu____1127 -> (false, depnames1))
                | Some intf1 ->
                    (match depnames1 with
                     | depintf::dep1::depnames' ->
                         if (depintf = intf1) && (dep1 = impl)
                         then (true, depnames')
                         else (false, depnames1)
                     | uu____1144 -> (false, depnames1)) in
              let rec pop_tc_and_stack env1 stack ts2 =
                match ts2 with
                | [] -> env1
                | uu____1191::ts3 ->
                    (pop env1 "";
                     (let uu____1213 =
                        let uu____1221 = FStar_List.hd stack in
                        let uu____1226 = FStar_List.tl stack in
                        (uu____1221, uu____1226) in
                      match uu____1213 with
                      | ((env2,uu____1240),stack1) ->
                          pop_tc_and_stack env2 stack1 ts3)) in
              match ts1 with
              | ts_elt::ts' ->
                  let uu____1274 = ts_elt in
                  (match uu____1274 with
                   | (intf,impl,intf_t,impl_t) ->
                       let uu____1292 = match_dep depnames intf impl in
                       (match uu____1292 with
                        | (b,depnames') ->
                            let uu____1303 =
                              (Prims.op_Negation b) ||
                                (is_stale intf impl intf_t impl_t) in
                            if uu____1303
                            then
                              let env1 =
                                pop_tc_and_stack env'
                                  (FStar_List.rev_append st []) ts1 in
                              tc_deps m good_stack env1 depnames good_ts true
                            else
                              (let uu____1315 =
                                 let uu____1323 = FStar_List.hd st in
                                 let uu____1328 = FStar_List.tl st in
                                 (uu____1323, uu____1328) in
                               match uu____1315 with
                               | (stack_elt,st') ->
                                   iterate depnames' st' env' ts' (stack_elt
                                     :: good_stack) (ts_elt :: good_ts))))
              | [] -> tc_deps m good_stack env' depnames good_ts true in
            let uu____1368 = deps_of_our_file filename in
            match uu____1368 with
            | (filenames,uu____1377) ->
                iterate filenames (FStar_List.rev_append stk []) env
                  (FStar_List.rev_append ts []) [] []
let rec go:
  (Prims.int* Prims.int) ->
    Prims.string -> stack_t -> modul_t -> env_t -> m_timestamps -> Prims.unit
  =
  fun line_col  ->
    fun filename  ->
      fun stack  ->
        fun curmod  ->
          fun env  ->
            fun ts  ->
              let uu____1428 = shift_chunk () in
              match uu____1428 with
              | Info (symbol,fqn_only,pos_opt) ->
                  let uu____1440 = env in
                  (match uu____1440 with
                   | (dsenv,tcenv) ->
                       let info_at_pos_opt =
                         match pos_opt with
                         | None  -> None
                         | Some (file,row,col) ->
                             FStar_TypeChecker_Err.info_at_pos
                               (Prims.snd env) file row col in
                       let info_opt =
                         match info_at_pos_opt with
                         | Some uu____1483 -> info_at_pos_opt
                         | None  ->
                             if symbol = ""
                             then None
                             else
                               (let lid =
                                  let uu____1512 =
                                    FStar_List.map FStar_Ident.id_of_text
                                      (FStar_Util.split symbol ".") in
                                  FStar_Ident.lid_of_ids uu____1512 in
                                let lid1 =
                                  if fqn_only
                                  then lid
                                  else
                                    (let uu____1516 =
                                       FStar_ToSyntax_Env.resolve_to_fully_qualified_name
                                         dsenv lid in
                                     match uu____1516 with
                                     | None  -> lid
                                     | Some lid1 -> lid1) in
                                let uu____1519 =
                                  FStar_TypeChecker_Env.try_lookup_lid
                                    (Prims.snd env) lid1 in
                                FStar_All.pipe_right uu____1519
                                  (FStar_Util.map_option
                                     (fun uu____1545  ->
                                        match uu____1545 with
                                        | ((uu____1555,typ),r) ->
                                            ((FStar_Util.Inr lid1), typ, r)))) in
                       ((match info_opt with
                         | None  -> FStar_Util.print_string "\n#done-nok\n"
                         | Some (name_or_lid,typ,rng) ->
                             let uu____1580 =
                               match name_or_lid with
                               | FStar_Util.Inl name -> (name, None)
                               | FStar_Util.Inr lid ->
                                   let uu____1590 =
                                     FStar_Ident.string_of_lid lid in
                                   let uu____1591 =
                                     let uu____1593 =
                                       FStar_ToSyntax_Env.try_lookup_doc
                                         dsenv lid in
                                     FStar_All.pipe_right uu____1593
                                       (FStar_Util.map_option Prims.fst) in
                                   (uu____1590, uu____1591) in
                             (match uu____1580 with
                              | (name,doc1) ->
                                  let uu____1610 =
                                    FStar_TypeChecker_Err.format_info
                                      (Prims.snd env) name typ rng doc1 in
                                  FStar_Util.print1 "%s\n#done-ok\n"
                                    uu____1610));
                        go line_col filename stack curmod env ts))
              | Completions search_term ->
                  let rec measure_anchored_match search_term1 candidate =
                    match (search_term1, candidate) with
                    | ([],uu____1633) -> Some ([], (Prims.parse_int "0"))
                    | (uu____1641,[]) -> None
                    | (hs::ts1,hc::tc) ->
                        let hc_text = FStar_Ident.text_of_id hc in
                        if FStar_Util.starts_with hc_text hs
                        then
                          (match ts1 with
                           | [] -> Some (candidate, (FStar_String.length hs))
                           | uu____1671 ->
                               let uu____1673 = measure_anchored_match ts1 tc in
                               FStar_All.pipe_right uu____1673
                                 (FStar_Util.map_option
                                    (fun uu____1692  ->
                                       match uu____1692 with
                                       | (matched,len) ->
                                           ((hc :: matched),
                                             (((FStar_String.length hc_text)
                                                 + (Prims.parse_int "1"))
                                                + len)))))
                        else None in
                  let rec locate_match needle candidate =
                    let uu____1728 = measure_anchored_match needle candidate in
                    match uu____1728 with
                    | Some (matched,n1) -> Some ([], matched, n1)
                    | None  ->
                        (match candidate with
                         | [] -> None
                         | hc::tc ->
                             let uu____1770 = locate_match needle tc in
                             FStar_All.pipe_right uu____1770
                               (FStar_Util.map_option
                                  (fun uu____1799  ->
                                     match uu____1799 with
                                     | (prefix1,matched,len) ->
                                         ((hc :: prefix1), matched, len)))) in
                  let str_of_ids ids =
                    let uu____1825 =
                      FStar_List.map FStar_Ident.text_of_id ids in
                    FStar_Util.concat_l "." uu____1825 in
                  let match_lident_against needle lident =
                    locate_match needle
                      (FStar_List.append lident.FStar_Ident.ns
                         [lident.FStar_Ident.ident]) in
                  let shorten_namespace uu____1854 =
                    match uu____1854 with
                    | (prefix1,matched,match_len) ->
                        let naked_match =
                          match matched with
                          | uu____1872::[] -> true
                          | uu____1873 -> false in
                        let uu____1875 =
                          FStar_ToSyntax_Env.shorten_module_path
                            (Prims.fst env) prefix1 naked_match in
                        (match uu____1875 with
                         | (stripped_ns,shortened) ->
                             let uu____1890 = str_of_ids shortened in
                             let uu____1891 = str_of_ids matched in
                             let uu____1892 = str_of_ids stripped_ns in
                             (uu____1890, uu____1891, uu____1892, match_len)) in
                  let prepare_candidate uu____1903 =
                    match uu____1903 with
                    | (prefix1,matched,stripped_ns,match_len) ->
                        if prefix1 = ""
                        then (matched, stripped_ns, match_len)
                        else
                          ((Prims.strcat prefix1 (Prims.strcat "." matched)),
                            stripped_ns,
                            (((FStar_String.length prefix1) + match_len) +
                               (Prims.parse_int "1"))) in
                  let needle = FStar_Util.split search_term "." in
                  let all_lidents_in_env =
                    FStar_TypeChecker_Env.lidents (Prims.snd env) in
                  let matches =
                    let case_a_find_transitive_includes orig_ns m id =
                      let dsenv = Prims.fst env in
                      let exported_names =
                        FStar_ToSyntax_Env.transitive_exported_ids dsenv m in
                      let matched_length =
                        FStar_List.fold_left
                          (fun out  ->
                             fun s  ->
                               ((FStar_String.length s) + out) +
                                 (Prims.parse_int "1"))
                          (FStar_String.length id) orig_ns in
                      FStar_All.pipe_right exported_names
                        (FStar_List.filter_map
                           (fun n1  ->
                              if FStar_Util.starts_with n1 id
                              then
                                let lid =
                                  FStar_Ident.lid_of_ns_and_id
                                    (FStar_Ident.ids_of_lid m)
                                    (FStar_Ident.id_of_text n1) in
                                let uu____1986 =
                                  FStar_ToSyntax_Env.resolve_to_fully_qualified_name
                                    dsenv lid in
                                FStar_Option.map
                                  (fun fqn  ->
                                     let uu____1994 =
                                       let uu____1996 =
                                         FStar_List.map
                                           FStar_Ident.id_of_text orig_ns in
                                       FStar_List.append uu____1996
                                         [fqn.FStar_Ident.ident] in
                                     ([], uu____1994, matched_length))
                                  uu____1986
                              else None)) in
                    let case_b_find_matches_in_env uu____2015 =
                      let uu____2022 = env in
                      match uu____2022 with
                      | (dsenv,uu____2030) ->
                          let matches =
                            FStar_List.filter_map
                              (match_lident_against needle)
                              all_lidents_in_env in
                          FStar_All.pipe_right matches
                            (FStar_List.filter
                               (fun uu____2060  ->
                                  match uu____2060 with
                                  | (ns,id,uu____2068) ->
                                      let uu____2073 =
                                        let uu____2075 =
                                          FStar_Ident.lid_of_ids id in
                                        FStar_ToSyntax_Env.resolve_to_fully_qualified_name
                                          dsenv uu____2075 in
                                      (match uu____2073 with
                                       | None  -> false
                                       | Some l ->
                                           let uu____2077 =
                                             FStar_Ident.lid_of_ids
                                               (FStar_List.append ns id) in
                                           FStar_Ident.lid_equals l
                                             uu____2077))) in
                    let uu____2078 = FStar_Util.prefix needle in
                    match uu____2078 with
                    | (ns,id) ->
                        let matched_ids =
                          match ns with
                          | [] -> case_b_find_matches_in_env ()
                          | uu____2103 ->
                              let l =
                                FStar_Ident.lid_of_path ns
                                  FStar_Range.dummyRange in
                              let uu____2106 =
                                FStar_ToSyntax_Env.resolve_module_name
                                  (Prims.fst env) l true in
                              (match uu____2106 with
                               | None  -> case_b_find_matches_in_env ()
                               | Some m ->
                                   case_a_find_transitive_includes ns m id) in
                        FStar_All.pipe_right matched_ids
                          (FStar_List.map
                             (fun x  ->
                                let uu____2139 = shorten_namespace x in
                                prepare_candidate uu____2139)) in
                  ((let uu____2145 =
                      FStar_Util.sort_with
                        (fun uu____2153  ->
                           fun uu____2154  ->
                             match (uu____2153, uu____2154) with
                             | ((cd1,ns1,uu____2169),(cd2,ns2,uu____2172)) ->
                                 (match FStar_String.compare cd1 cd2 with
                                  | _0_32 when _0_32 = (Prims.parse_int "0")
                                      -> FStar_String.compare ns1 ns2
                                  | n1 -> n1)) matches in
                    FStar_List.iter
                      (fun uu____2183  ->
                         match uu____2183 with
                         | (candidate,ns,match_len) ->
                             let uu____2190 =
                               FStar_Util.string_of_int match_len in
                             FStar_Util.print3 "%s %s %s \n" uu____2190 ns
                               candidate) uu____2145);
                   FStar_Util.print_string "#done-ok\n";
                   go line_col filename stack curmod env ts)
              | Pop msg ->
                  (pop env msg;
                   (let uu____2194 =
                      match stack with
                      | [] ->
                          (FStar_Util.print_error "too many pops";
                           FStar_All.exit (Prims.parse_int "1"))
                      | hd1::tl1 -> (hd1, tl1) in
                    match uu____2194 with
                    | ((env1,curmod1),stack1) ->
                        (if
                           (FStar_List.length stack1) =
                             (FStar_List.length ts)
                         then cleanup env1
                         else ();
                         go line_col filename stack1 curmod1 env1 ts)))
              | Push (lax1,l,c) ->
                  let uu____2261 =
                    if (FStar_List.length stack) = (FStar_List.length ts)
                    then
                      let uu____2284 =
                        update_deps filename curmod stack env ts in
                      (true, uu____2284)
                    else (false, (stack, env, ts)) in
                  (match uu____2261 with
                   | (restore_cmd_line_options1,(stack1,env1,ts1)) ->
                       let stack2 = (env1, curmod) :: stack1 in
                       let env2 =
                         push env1 lax1 restore_cmd_line_options1 "#push" in
                       go (l, c) filename stack2 curmod env2 ts1)
              | Code (text1,(ok,fail1)) ->
                  let fail2 curmod1 env_mark =
                    report_fail ();
                    FStar_Util.print1 "%s\n" fail1;
                    (let env1 = reset_mark env_mark in
                     go line_col filename stack curmod1 env1 ts) in
                  let env_mark = mark env in
                  let frag =
                    {
                      FStar_Parser_ParseIt.frag_text = text1;
                      FStar_Parser_ParseIt.frag_line = (Prims.fst line_col);
                      FStar_Parser_ParseIt.frag_col = (Prims.snd line_col)
                    } in
                  let res = check_frag env_mark curmod frag in
                  (match res with
                   | Some (curmod1,env1,n_errs) ->
                       if n_errs = (Prims.parse_int "0")
                       then
                         (FStar_Util.print1 "\n%s\n" ok;
                          (let env2 = commit_mark env1 in
                           go line_col filename stack curmod1 env2 ts))
                       else fail2 curmod1 env_mark
                   | uu____2364 -> fail2 curmod env_mark)
let interactive_mode: Prims.string -> Prims.unit =
  fun filename  ->
    (let uu____2376 =
       let uu____2377 = FStar_Options.codegen () in
       FStar_Option.isSome uu____2377 in
     if uu____2376
     then
       FStar_Util.print_warning
         "code-generation is not supported in interactive mode, ignoring the codegen flag"
     else ());
    (let uu____2380 = deps_of_our_file filename in
     match uu____2380 with
     | (filenames,maybe_intf) ->
         let env = tc_prims () in
         let uu____2394 = tc_deps None [] env filenames [] true in
         (match uu____2394 with
          | (stack,env1,ts) ->
              let uu____2409 =
                match maybe_intf with
                | Some intf ->
                    let frag =
                      let uu____2422 = FStar_Util.file_get_contents intf in
                      {
                        FStar_Parser_ParseIt.frag_text = uu____2422;
                        FStar_Parser_ParseIt.frag_line =
                          (Prims.parse_int "0");
                        FStar_Parser_ParseIt.frag_col = (Prims.parse_int "0")
                      } in
                    let uu____2423 = check_frag env1 None frag in
                    (match uu____2423 with
                     | Some (curmod,env2,n_errs) ->
                         (if n_errs <> (Prims.parse_int "0")
                          then
                            ((let uu____2453 =
                                FStar_Util.format1
                                  "Found the interface %s but it has errors!"
                                  intf in
                              FStar_Util.print_warning uu____2453);
                             FStar_All.exit (Prims.parse_int "1"))
                          else ();
                          FStar_Util.print_string
                            "Reminder: fst+fsti in interactive mode is unsound.\n";
                          (curmod, env2))
                     | None  ->
                         ((let uu____2466 =
                             FStar_Util.format1
                               "Found the interface %s but could not parse it first!"
                               intf in
                           FStar_Util.print_warning uu____2466);
                          FStar_All.exit (Prims.parse_int "1")))
                | None  -> (None, env1) in
              (match uu____2409 with
               | (initial_mod,env2) ->
                   let uu____2481 =
                     (FStar_Options.record_hints ()) ||
                       (FStar_Options.use_hints ()) in
                   if uu____2481
                   then
                     let uu____2482 =
                       let uu____2483 = FStar_Options.file_list () in
                       FStar_List.hd uu____2483 in
                     FStar_SMTEncoding_Solver.with_hints_db uu____2482
                       (fun uu____2485  ->
                          go ((Prims.parse_int "1"), (Prims.parse_int "0"))
                            filename stack initial_mod env2 ts)
                   else
                     go ((Prims.parse_int "1"), (Prims.parse_int "0"))
                       filename stack initial_mod env2 ts)))