open Owl
module Sqlexpr = Sqlexpr_sqlite.Make (Sqlexpr_concurrency.Id)
module S = Sqlexpr

let dbfile = Cmdargs.(get_string "-db" |> force ~usage:"-db [file]")
let db = S.open_db dbfile

let sessions_for_mouse m =
  S.select db [%sql "select @d{session}, mouse from sessions where mouse=%d"] m


(* get the first k elements in a list *)
let first k x =
  let rec iter accu k rest =
    if k = 0
    then Some (List.rev accu)
    else (
      match rest with
      | [] -> None
      | hd :: tl -> iter (hd :: accu) (k - 1) tl)
  in
  iter [] k x


let consume_until t x =
  let rec iter = function
    | (t', _) :: rest as current -> if t' < t then iter rest else current
    | [] -> []
  in
  iter x


let sta n_bins i =
  (* extract all sessions in which neuron i was recorded *)
  let n_spikes, sta =
    S.fold
      db
      (fun (count, accu) s ->
        (* get the IMU for that session *)
        let imu =
          S.select
            db
            [%sql "select @d{t}, @f{yaw} from head where session=%d order by t asc"]
            s
        in
        (* fold over the spikes *)
        S.fold
          db
          (fun (count, accu) t ->
            match first n_bins (consume_until t imu) with
            | None -> count, accu
            | Some imu ->
              let imu = Mat.of_array (imu |> List.map snd |> Array.of_list) 1 n_bins in
              count + 1, Mat.(accu + imu -$ get imu 0 0))
          (count, accu)
          [%sql "select @d{t} from spikes where cell=%d and session=%d"]
          i
          s)
      (0, Mat.zeros 1 n_bins)
      [%sql "select @d{session} from sessions join cells using (mouse) where cell=%d"]
      i
  in
  Mat.(sta /$ Int.to_float n_spikes)


let _ =
  S.select
    db
    [%sql
      "select @d{cell} from cells where mouse in (select mouse from (select mouse, \
       count(*) as c from sessions group by mouse) where c>10)"]
  |> Array.of_list
  |> (fun v -> Array.sub v 0 5)
  |> Array.map (fun i ->
         Printf.printf "neuron %i\n%!" i;
         Mat.to_array (sta 20 i))
  |> Mat.of_arrays
  |> Mat.transpose
  |> fun m -> Mat.save_txt m "stas"


let n_mice = S.select_one db [%sql "select @d{count(distinct mouse)} from sessions"]
let _ = Printf.printf "n mouse = %i" n_mice
let () = S.close_db db
