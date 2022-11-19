let window () =
  match Raylib.get_window_handle () with
  | None -> failwith "Raylib not initialized"
  | Some win_ptr ->
    (* GLFW does not use Ctypes so we need to convert a fat ctypes pointer to the GLFW bindings format. *)
    (* The bindings use a C integer + 1, i.e. the normal OCaml representation for ints. *)
    let raw = Ctypes.raw_address_of_ptr win_ptr in
    let raw' = Int64.of_nativeint raw |> Int64.to_int in
    GLFW.window_magic (Obj.magic raw' : GLFW.window)

let init ?(title="BOGUE Window") ?x ?y ~w ~h () =
  Raylib.set_trace_log_level Raylib.TraceLogLevel.None;
  Raylib.init_window w h title;
  let window = window () in

  GLFW.setWindowAttrib ~window ~attribute:GLFW.Resizable ~value:true;
  GLFW.setWindowSizeLimits ~window
    ~minWidth:(Some w) ~minHeight:(Some h)
    ~maxWidth:None ~maxHeight:None;

  Font.init ();
  ()

let quit () =
  Raylib.close_window ()
