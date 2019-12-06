open Owl
module Sqlexpr = Sqlexpr_sqlite.Make (Sqlexpr_concurrency.Id)
module S = Sqlexpr

let _ = Printexc.record_backtrace true
let dbfile = Cmdargs.(get_string "-db" |> force ~usage:"-db [file]")
let db = S.open_db dbfile

let sessions_for_mouse m =
  S.select db [%sql "select @d{session}, mouse from sessions where mouse=%d"] m


let sta i =
  let spikes =
    S.select db [%sql "select @d{t}, @d{session} from spikes where cell=%d"] i
  in
  let n_sessions, sta =
    List.fold_left
      (fun (i, accu) (t, session) ->
        let imu =
          S.select
            db
            [%sql "select @f{yaw} from head where session=%d and t>%d limit 20"]
            session
            t
          |> Array.of_list
        in
        if Array.length imu >= 20
        then (
          let imu = Mat.of_array imu 1 (-1) in
          i + 1, Mat.(accu + imu -$ get imu 0 0))
        else i, accu)
      (0, Mat.zeros 1 20)
      spikes
  in
  Mat.(sta /$ Int.to_float n_sessions)


let _ =
  S.select db [%sql "select @d{cell} from cells"]
  |> fun m ->
  [ List.hd m ]
  |> List.map (fun i -> Mat.to_array (sta i))
  |> Array.of_list
  |> Mat.of_arrays
  |> Mat.transpose
  |> fun m -> Mat.save_txt m "stas"


let n_mice = S.select_one db [%sql "select @d{count(distinct mouse)} from sessions"]
let _ = Printf.printf "n mouse = %i" n_mice
let () = S.close_db db
