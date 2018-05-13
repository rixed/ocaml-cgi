(*
 * ocamlcgi - Objective Caml library for writing CGIs
 * Copyright (C) 1997 Daniel de Rauglaudre, INRIA
 * Copyright (C) 1998 Jean-Christophe FILLIATRE
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* $Id: cgi.ml,v 1.9 2006/05/24 06:28:45 filliatr Exp $ *)

(* Code from wserver.ml, (C) 1997 Daniel de Rauglaudre, INRIA. *)

let hexa_digit x =
  if x >= 10 then Char.chr (Char.code 'A' + x - 10)
  else Char.chr (Char.code '0' + x)

let hexa_val conf =
  match conf with
    | '0'..'9' -> Char.code conf - Char.code '0'
    | 'a'..'f' -> Char.code conf - Char.code 'a' + 10
    | 'A'..'F' -> Char.code conf - Char.code 'A' + 10
    | _ -> 0

let raw_decode s =
  let rec need_decode i =
    if i < String.length s then
      match s.[i] with
  | '%' | '+' -> true
  | _ -> need_decode (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s -> i + 3
          | _ -> succ i
      in
      compute_len i (succ i1)
    else i1
  in
  let rec copy_decode_in s1 i i1 =
    if i < String.length s then
      let i =
        match s.[i] with
          | '%' when i + 2 < String.length s ->
              let v = hexa_val s.[i + 1] * 16 + hexa_val s.[i + 2] in
              s1.[i1] <- Char.chr v; i + 3
          | '+' -> s1.[i1] <- ' '; succ i
          | x -> s1.[i1] <- x; succ i
      in
      copy_decode_in s1 i (succ i1)
    else s1
  in
  if need_decode 0 then
    let len = compute_len 0 0 in
    let s1 = Bytes.create len in
    copy_decode_in s1 0 0 |>
    Bytes.to_string
  else
    s

let decode s =
  let rs = raw_decode s in
  let rec strip_heading_and_trailing_spaces s =
    if String.length s > 0 then
      if s.[0] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 1 (String.length s - 1))
      else if s.[String.length s - 1] == ' ' then
        strip_heading_and_trailing_spaces
          (String.sub s 0 (String.length s - 1))
      else
        s
    else
      s
  in
  strip_heading_and_trailing_spaces rs

(* special characters must be encoded. According to RFC 1738 they are: *)

let special = function
  | '\000'..'\031' | '\127'..'\255'                      (* non US ASCII *)
  | '<' | '>' | '"' | '#' | '%'                          (* space should be here, but its encoding uses only one char *)
  | '{' | '}' | '|' | '\\' | '^' | '~' | '[' | ']' | '`' (* unsafe *)
  | ';' | '/' | '?' | ':' | '@' | '=' | '&'              (* reserved *)
      -> true
  | '+' -> true
  | _ -> false

(* '"' *)

let encode s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
        | ' ' -> true
  | x ->
    if special x then true else need_code (succ i)
    else false
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 = if special s.[i] then i1 + 3 else succ i1 in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
          | ' ' -> s1.[i1] <- '+'; succ i1
          | c ->
              if special c then
              begin
                  s1.[i1] <- '%';
                  s1.[i1 + 1] <- hexa_digit (Char.code c / 16);
                  s1.[i1 + 2] <- hexa_digit (Char.code c mod 16);
                  i1 + 3
              end
              else begin s1.[i1] <- c; succ i1 end
      in
      copy_code_in s1 (succ i) i1
    else
      s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in
    copy_code_in (Bytes.create len) 0 0 |>
    Bytes.to_string
  else
    s


(* Using the following function, we avoid the use of the Str library
 * which is not compatible with the threads of ocaml.
 * Thanks to Olivier Montanuy. *)

let split separator text =
  let len = String.length text in
  let rec loop pos =
    if pos < len then
      try
        let last = String.index_from text pos separator in
        let str = String.sub text pos (last-pos) in
        str::(loop (succ last))
      with Not_found ->
        if pos < len then [String.sub text pos (len-pos)]
        else []
    else []
  in
  loop 0

let string_starts_with s pref =
  String.length s >= String.length pref &&
  String.sub s 0 (String.length pref) = pref

(* parse_args: parsing of the CGI arguments *)

let safe_getenv ?default s =
  try
    Sys.getenv s
  with Not_found ->
    match default with
    | Some d -> d
    | None ->
      failwith ("Cgi: the environment variable " ^ s ^ " is not set")

let one_assoc s =
  try
    let i = String.index s '=' in
    decode (String.sub s 0 i),
    decode (String.sub s (succ i) (String.length s - i - 1))
  with
    | Not_found -> s,""

(* read_body: return the body of the query *)
let read_body =
  let body_is_gone = ref false in
  fun () ->
    if !body_is_gone then "" else
    let n = int_of_string (safe_getenv ~default:"-1" "CONTENT_LENGTH") in
    if n >= 0 then (
      let buf = Bytes.create n in
      really_input stdin buf 0 n;
      body_is_gone := true;
      Bytes.to_string buf
    ) else (
      (* Read until EOF *)
      let rec blit_prevs b e = function
        | [] -> ()
        | (s, chunk)::prevs ->
          Bytes.blit chunk 0 b (e - s) s;
          blit_prevs b (e - s) prevs in
      let rec loop tot_s prevs =
        let n = 4096 in
        let buf = Bytes.create n in
        let s = input stdin buf 0 n in
        if s > 0 then (
          loop (tot_s + s) ((s, buf)::prevs)
        ) else (
          let res = Bytes.create tot_s in
          blit_prevs res tot_s prevs;
          Bytes.to_string res
        ) in
      loop 0 []
    )

let parse_args () =
  let req_method = safe_getenv "REQUEST_METHOD" in
  let s =
    if req_method = "GET" || req_method = "HEAD" then
      safe_getenv ~default:"" "QUERY_STRING"
    else begin
      let mime_type = safe_getenv "CONTENT_TYPE" in
      if req_method = "POST" then (
        if mime_type = "application/x-www-form-urlencoded" then (
          read_body ()
        ) else (
          (* That's fine, user can still read the body and content type
           * and do the right thing *)
          ""
        )
      ) else (
        failwith ("Cgi: cannot handle " ^ req_method ^ " request with type " ^
                  mime_type)
      )
    end
  in
  let assocs = split '&' s in
  List.map one_assoc assocs


(* parse_cookies: parsing of cookies sent by browser *)

let parse_cookies () =
  let data = safe_getenv ~default:"" "HTTP_COOKIE" in
  let cookies = Str.(split (regexp "; +") data) in
  List.map one_assoc cookies

(* parse_multipart_args: parsing of the CGI arguments for multipart/form-data
   encoding *)

let boundary_re1 =
  Str.regexp_case_fold "boundary=\"\\([^\"]+\\)\""
let boundary_re2 =
  Str.regexp_case_fold "boundary=\\([^ \t\r\n]+\\)"
let name_re1 =
  Str.regexp_case_fold "name=\"\\([^\"]+\\)\""
let name_re2 =
  Str.regexp_case_fold "name=\\([^ \t\r\n;:]+\\)"
let filename_re1 =
  Str.regexp_case_fold "filename=\"\\([^\"]*\\)\""
let filename_re2 =
  Str.regexp_case_fold "filename=\\([^ \t\r\n;:]+\\)"
let content_type_re1 =
  Str.regexp_case_fold "Content-type:[ \t]*\"\\([^\"]+\\)\""
let content_type_re2 =
  Str.regexp_case_fold "Content-type:[ \t]*\\([^ \t\r\n;:]+\\)"
let separator_re =
  Str.regexp "\r\n\r\n"

let match_string re1 re2 str =
  try
    ignore(Str.search_forward re1 str 0); Str.matched_group 1 str
  with Not_found ->
    ignore(Str.search_forward re2 str 0); Str.matched_group 1 str

(* Extract field name and value from a chunk.  Raise Not_found if not
   a valid chunk. *)

type field_data = {
  value: string;
  filename: string;
  content_type: string
}

let extract_field chunk =
  let pos_separator = Str.search_forward separator_re chunk 0 in
  let header = String.sub chunk 0 pos_separator in
  let field_name = match_string name_re1 name_re2 header in
  let field_filename =
    try match_string filename_re1 filename_re2 header
    with Not_found -> "" in
  let field_content_type =
    try match_string content_type_re1 content_type_re2 header
    with Not_found -> "" in
  let beg_value = pos_separator + 4 in
  (* Chop final \r\n that browsers insist on putting *)
  let end_value =
    let len = String.length chunk in
    if len >= beg_value && String.sub chunk (len - 2) 2 = "\r\n"
    then len - 2
    else len in
  let field_value =
    String.sub chunk beg_value (end_value - beg_value) in
  (field_name, { filename = field_filename;
                 content_type = field_content_type;
                 value = field_value })

(* Same, for a list of chunks *)

let rec extract_fields accu = function
  | [] ->
      accu
  | chunk :: rem ->
      extract_fields
        (try extract_field chunk :: accu with Not_found -> accu)
        rem

let parse_multipart_args () =
  let req_method = safe_getenv "REQUEST_METHOD"
  and mime_type = safe_getenv "CONTENT_TYPE" in
  if not (req_method = "POST" &&
          string_starts_with mime_type "multipart/form-data")
  then
    failwith ("Cgi: cannot handle " ^ req_method ^ " request with type " ^
              mime_type);
  (* Determine boundary delimiter *)
  let boundary =
    try
      match_string boundary_re1 boundary_re2 mime_type
    with Not_found ->
      failwith ("Cgi: no boundary provided in " ^ mime_type) in
  (* Weird code organization intended to help GC reclaim early.
     Equivalent, clearer code:
       let data = read_body () in
       let chunks = Str.split (Str.regexp_string ("--" ^ boundary)) data in
       extract_fields [] chunks *)
  extract_fields []
    (Str.split (Str.regexp_string ("--" ^ boundary))
      (read_body ()))

(*s PATH\_INFO *)

let path_info =
  try
    match split '/' (Sys.getenv "PATH_INFO") with
      | _ :: t -> t
      | [] -> []
  with Not_found ->
    []

let nth_path_info index =
  try
    List.nth path_info index
  with Failure _ ->
    ""

(* content-type *)

let header ?(status=200) ?err_msg ?(cookies=[]) ?(content_type="text/html") () =
  let pick_err_msg = function
    | _, Some e -> e
    | 200, None -> "OK"
    | 302, None -> "Found"
    | 400, None -> "Bad Request"
    | 501, None -> "Not Implemented"
    | _ -> "Hope that's fine" in
  List.iter (fun (n,v) ->
    Printf.printf "Set-Cookie: %s=%s\n" n v) cookies ;
  if status <> 200 || err_msg <> None then
    Printf.printf "Status: %03d %s\n" status (pick_err_msg (status, err_msg)) ;
  Printf.printf "Content-type: %s\n\n" content_type

let header_error m =
  Printf.printf "Content-type: %s\nStatus: 500 Error in CGI script\n\n"
    (if m="" then "text/html" else m)

(* returns the URL of the CGI *)

let this_url () =
  let port =
    try int_of_string (safe_getenv "SERVER_PORT")
    with _ -> 80 in
  "http://" ^ (safe_getenv "SERVER_NAME") ^
              (if port <> 80 then ":"^(string_of_int port) else "") ^
              (safe_getenv "SCRIPT_NAME")
