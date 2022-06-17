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

(* $Id: cgi.mli,v 1.5 2002/09/20 08:04:59 filliatr Exp $ *)

(* Decodes the CGI arguments. Returns an association list.
   Works whatever the method is (GET or POST) *)
val parse_args : unit -> (string * string) list

(* Decodes Cookies sent by the browser. Returns an association list.
   Returns an empty list if no cookies were sent. *)
val parse_cookies : unit -> (string * string) list

(* Read the body of the query and return it as a string. *)
val read_body : unit -> string

(* Decodes the CGI arguments for multipart/form-data encoding. *)
type field_data = {
  value : string;
  filename : string;
  content_type : string;
}
val parse_multipart_args : unit -> (string * field_data) list

(* Prints the content-type + cookie headers + status.
   The argument to content type is the MIME type and will be "text/html" if not set. *)
val header : ?status:int ->
             ?err_msg:string ->
             ?cookies:(string * string) list ->
             ?content_type:string -> unit -> unit

(* Returns the address of the CGI *)
val this_url : unit -> string

(* The list of items found in PATH_INFO *)
val path_info : string list

(* Given a zero-based index, returns the [i]-th info element;
   returns the empty string if [i] is out of bounds *)
val nth_path_info : int -> string

(* Coding and decoding of CGI arguments.
   The following code may be useful but is already called in [parse_args].
   Code from wserver, (c) Daniel de Rauglaudre, INRIA. *)

val decode : string -> string
val encode : string -> string
