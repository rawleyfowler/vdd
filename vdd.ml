open GMain

(* Utilities *)
let write_iso iso disk =
  Sys.command @@ Printf.sprintf "dd if=%s of=%s bs=1M status=progress" iso disk

let execute_write ~(combo : GEdit.combo_box) ~column ~iso () =
  match combo#active_iter with
  | None -> -1
  | Some row ->
     let data = combo#model#get ~row ~column in
     write_iso iso data
 
let get_disks () =
  let dir = "/dev" in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Unix.((stat @@ dir ^ "/" ^ f).st_kind = S_BLK))

let alert_if_not_iso ~chooser () =
  let file_name = List.hd chooser#get_filenames in
  if not (Filename.check_suffix file_name ".iso" || Filename.check_suffix file_name ".img")
  then
    let d = GWindow.message_dialog
              ~message:"That doesn't look like an ISO or IMG file! You can still flash this file however."
              ~message_type:`INFO
              ~buttons:GWindow.Buttons.close
              ~title:"Warning!"
              ~show:true ()
    in
    ignore @@ d#connect#response ~callback:(function `CLOSE | `DELETE_EVENT -> d#destroy ());
  else ()
  
(* GTK Stuff *)
let _ = GtkMain.Main.init ()

let window = GWindow.window ~width:320 ~height:240 ~title:"vdd" ()

let vbox = GPack.vbox ~packing:window#add ()

let iso_chooser = GFile.chooser_button
                    ~action:`OPEN
                    ~title:"iso"
                    ~width:40
                    ~height:20
                    ~show:true
                    ~packing:vbox#add ()

let (combo, column) =
  let strings = get_disks () in
  let (combo, (_, column)) = GEdit.combo_box_text
    ~strings
    ~wrap_width:1
    ~packing:vbox#add ()
  in combo, column

let write_button = GButton.button
               ~label:"Write to disk"
               ~packing:vbox#add ()

let main () =
  let _ = window#connect#destroy ~callback:Main.quit in
  let _ = write_button#connect#clicked
            ~callback:(fun () -> ignore @@
                                   execute_write
                                     ~combo:combo
                                     ~column
                                     ~iso:(List.hd iso_chooser#get_filenames) ())
  in
  let _ = iso_chooser#connect#selection_changed
            ~callback:(alert_if_not_iso ~chooser:iso_chooser)
  in
  window#show ();
  Main.main ()

let () = main ()
