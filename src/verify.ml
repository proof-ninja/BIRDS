let verify debug timeout ast =
  begin try
    let lean_version = Utils.check_command_version "lean" in
    print_endline @@ Printf.sprintf "INFO: The current version of Lean is: '%s'. Please be sure it is greater than or equal to 3.4.2 to avoid unexpected errors of verification." lean_version;
    (* check lean paths *)
    let tmp_chklib_file = Filename.temp_file "" ".lean" in
    let chklib = open_out tmp_chklib_file in
    Printf.fprintf chklib "%s\n" "import bx";
    close_out chklib;
    let leanstatus, _leanmessage = Utils.exe_command @@ Printf.sprintf "lean %s" tmp_chklib_file in
    if not (leanstatus = 0) then
      Printf.fprintf stderr "%s\n" "WARNING: Lean paths to BIRDS's verification folder are not configured correctly! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written. Please change the Lean path configuration in ~/.lean/leanpkg.path and check by 'lean --path'. More details at https://github.com/dangtv/BIRDS."
    else ();
  with
    | Utils.EnvErr _msg -> Printf.fprintf stderr "%s\n" "WARNING: Command 'lean' can not be called! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written.";
    | e -> prerr_endline @@ Printexc.to_string e
  end;

  begin try
    let z3_version = Utils.check_command_version "z3" in
    print_endline @@ Printf.sprintf "INFO: The current version of Z3 is: '%s'. Please be sure it is greater than or equal to 4.8.7 to avoid unexpected errors of verification." z3_version;
  with
    | Utils.EnvErr _msg -> Printf.fprintf stderr "%s\n" "WARNING: Command 'z3' can not be called! Verifications will not be performed correctly, and hence Datalog rules of the view definition must be explicitly written.";
    | e -> prerr_endline @@ Printexc.to_string e
  end;

  let constr_ast = Utils.constraint2rule ast in
  let disdelta_thm = Ast2theorem.lean_simp_theorem_of_disjoint_delta debug constr_ast in
  let getput_thm = Ast2theorem.lean_simp_theorem_of_getput debug constr_ast in
  let putget_thm = Ast2theorem.lean_simp_theorem_of_putget debug constr_ast in

  let satisfying_getput = ref false in
  let satisfying_putget = ref false in
  let satisfying_deltadis = ref false in
  let verification_mess = ref "" in

  if debug then
    print_endline "==> Verifying the delta disjointness property";
  let lean_code_disdelta = Ast2theorem.gen_lean_code_for_theorems [ disdelta_thm ] in
  let exitcode, message = Utils.verify_fo_lean debug timeout lean_code_disdelta in
  if not (exitcode = 0) then
    if (exitcode = 124) then
      begin
        if debug then
          print_endline "Stop verifying the delta disjointness property: Timeout";
        verification_mess := String.concat "\n\n" [
          !verification_mess;
          "Stop verifying the delta disjointness property: Timeout";
        ]
      end
    else
      if Utils.str_contains message "type mismatch at application" then
        (* type error *)
        begin
          let m = String.concat "" [
            "Invalidity of delta disjointness: Type mismatch: \n";
            Utils.cut_str_by_word message "error";
            if debug then Printf.sprintf "\nError messange: %s" message else "";
          ] in
          if debug then print_endline m;
          verification_mess := String.concat "\n\n" [!verification_mess; m]
        end
      else
        begin
          let m = String.concat "" [
            Printf.sprintf "Invalidity: Deltas in the datalog program are not disjoint \nExit code: %i" exitcode;
            if debug then Printf.sprintf "\nError messange: %s" message else "";
          ]
          in
          if debug then print_endline m;
          verification_mess := (!verification_mess) ^ "\n\n" ^ m
        end
  else
    satisfying_deltadis := true;

  (* verify getput property *)
  if debug then print_endline "==> verifying the getput property";
  let lean_code_getput = Ast2theorem.gen_lean_code_for_theorems [ getput_thm ] in
  let exitcode, message = Utils.verify_fo_lean debug timeout lean_code_getput in
  if not (exitcode = 0) then
    if (exitcode = 124) then
      begin
        if debug then
          print_endline "Stop verifying the getput property: Timeout";
        verification_mess := String.concat "\n\n" [!verification_mess; "Stop verifying the getput property: Timeout"]
      end
    else
      if Utils.str_contains message "type mismatch at application" then
      (* type error *)
      begin
        let m = String.concat "" [
          "Invalidity of getput: Type mismatch: \n";
          Utils.cut_str_by_word message "error";
          if debug then Printf.sprintf "\nError messange: %s" message else "";
        ] in
        if debug then print_endline m;
        verification_mess := String.concat "\n\n" [!verification_mess; m]
      end
      else
        begin
          let m = String.concat "" [
            Printf.sprintf "Invalidity: Property getput is not validated \nExit code: %i" exitcode;
            if debug then Printf.sprintf "\nError messange: %s" message else "";
          ] in
          if debug then print_endline m;
          verification_mess := String.concat "\n\n" [!verification_mess; m]
        end
  else
    satisfying_getput := true;

  (* verify putget *)
  if debug then print_endline "==> Verifying the putget property";
  let lean_code_putget = Ast2theorem.gen_lean_code_for_theorems [ putget_thm ] in
  let exitcode, message = Utils.verify_fo_lean debug timeout lean_code_putget in
  if not (exitcode = 0) then
    if (exitcode = 124) then
      begin
        if debug then
          print_endline "Stop verifying the getput property: Timeout";
        verification_mess := String.concat "\n\n" [!verification_mess; "Stop verifying the getput property: Timeout"]
      end
    else
      if Utils.str_contains message "Type mismatch at application" then
      (* type error *)
      begin
        let m = String.concat "" [
          "Invalidity of putget: Type mismatch: \n";
          Utils.cut_str_by_word message "error";
          if debug then Printf.sprintf "\nError messange: %s" message else "";
        ] in
        verification_mess := String.concat"\n\n" [!verification_mess; m]
      end
      else
        begin
          let m = String.concat "" [
            Printf.sprintf "Invalidity: Property putget is not validated \nExit code: %i" exitcode;
            if debug then Printf.sprintf "\nError messange: %s" message else "";
          ] in
          if debug then print_endline m;
          verification_mess := String.concat "\n\n" [!verification_mess; m]
        end
  else
    satisfying_putget := true;

  (* conclusion *)
  if !satisfying_deltadis && !satisfying_getput && !satisfying_putget then
    begin
      if debug then
        print_endline "The program satisfies all delta disjointness, getput and putget"
    end
  else
    raise (Utils.ChkErr !verification_mess)
