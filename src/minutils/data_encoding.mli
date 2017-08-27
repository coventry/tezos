(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Representation of a binary encoding scheme for an OCaml type. *)
type 'a t
type 'a encoding = 'a t

(** In memory JSON data *)
type json =
  [ `O of (string * json) list (* e.g.{"foo":true} => `O [("foo",`Bool true)]*)
  | `Bool of bool
  | `Float of float
  | `A of json list (* e.g. ["a", 1.] => `A [`String "a", `Float 1.] *)
  | `Null (* e.g. { "a" : null} => `O ["a",`Null]*)
  | `String of string ]

(** See
   https://github.com/OCamlPro/ocplib-json-typed/blob/master/src/json_schema.mli
   *)
type json_schema = Json_schema.schema

(** Raised when there's no match in a String_enum or a Union *)
exception No_case_matched

(** Raised when a tag is read which has no matches in the current Union
    encoding. Tags are small integers used to indicate which case of a Union
    appears next in the buffer. *)
exception Unexpected_tag of int

(** Raised when the cases in a Union contain an ambiguous tag. *)
exception Duplicated_tag of int

(** Raised when a tag is negative or too large for its binary representation. *)
exception Invalid_tag of int * [ `Uint8 | `Uint16 ]

(** Raised on binary read of a String_enum value which is not included in the
    encoding. The binary representation of a String_enum is a small integer, and
    that integer must be included in the values of the enum. *)
exception Unexpected_enum of string * string list

(** [classify encoding] is information about the how length of a serialization
    via [encoding] is represented in the serialization itself. *)
val classify : 'a encoding -> [

    (** All serializations have fixed length; no binary representation of length
        is necessary *)
    |  `Fixed of int

    (** Size of serialization is included at the start of it. See for instance
        the [Obj (Opt (`Dynamic, _, t))] vs the [Obj (Opt (`Variable, _, t))]
        branches of [Binary.read_rec] in the implementation. *)
    | `Dynamic

    (** The rest of the current buffer portion (based on the [offset] and
        [length] arguments to [Binary.read] etc.) is assumed to contain
        subobjects. E.g. if it's an array, the last element of the array is
        simply the object which is read when the end of the buffer is reached. *)
    | `Variable
  ]

(** [splitted json binary] is a dual json/binary encoding for an object type.
    See for instance [Time.encoding], where the binary encoding is an [int64],
    and the json is either that or an RFC 3339 timestamp string. *)
val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

(** Encoding of nothing: nothing is written to / read from the buffer, unit
    object is returned. Used for cases in unions where no subsequent information
    about the object follows the tag. *)
val null : unit encoding


(** Encoding of nothing: nothing is written to / read from the buffer, unit
   object is returned. Used for protocols where no input or output expected. *)
val empty : unit encoding


(** Encoding of nothing: nothing is written to / read from the buffer, unit
   object is returned. *)
val unit : unit encoding

(** Encoding of nothing: nothing is written to / read from the buffer, unit
    object is returned. Used to document components of encodings with constant
    strings. *)
val constant : string -> unit encoding

(** Encodings of primitive types. See the corresponding functions in the Binary
   module for their binary encodings. *)
val int8 : int encoding
val uint8 : int encoding
val int16 : int encoding
val uint16 : int encoding
val int31 : int encoding
val int32 : int32 encoding
val int64 : int64 encoding
val bool : bool encoding
val string : string encoding
val bytes : MBytes.t encoding
val float : float encoding

(** Encoding of 'a option (i.e. [Some of 'a | None])*)
val option : 'a encoding -> 'a option encoding

(** Encoding of [('a, 'b)] result (i.e. [Ok of 'a | Error of 'b)] *)
val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding

(** Encoding of a string -> 'a association list. *)
val string_enum : (string * 'a) list -> 'a encoding

(** Fixed-length encodings of strings and bytes. *)
module Fixed : sig

  (** [string n] is a fixed-length encoding of a string of length [n]. *)
  val string : int -> string encoding

  (** [bytes n] is a fixed-length encoding of an MBytes buffer of length [n]. *)
  val bytes : int -> MBytes.t encoding
end

(** Variable-length encodings of strings/bytes/arrays/lists. This means that
   [read] and [write] etc. assume the rest of the data in the specified buffer
   are the contents of the object *)
module Variable : sig
  val string : string encoding
  val bytes : MBytes.t encoding
  val array : 'a encoding -> 'a array encoding
  val list : 'a encoding -> 'a list encoding
end

(** [dynamic_size e] is an encoding of [e] objects which is prefixed with the
   length of the serializtion of the object. *)
val dynamic_size : 'a encoding -> 'a encoding

(** An encoding of a [json] object. See [type json] *)
val json : json encoding

(** An encoding of a Json_schema.schema object.*)
val json_schema : json_schema encoding

(** An encoding of a field in a json object. *)
type 'a field

(** [req t d n e] A required field in a json object. [t] is the field title, [d]
   the description, [n] the key in the object (e.g. the ["foo"] in
   [{"foo":null}, and e the encoding for the field contents.]) *)
val req :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't field

(** An optional *)
val opt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val varopt :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't option field
val dft :
  ?title:string -> ?description:string ->
  string -> 't encoding -> 't -> 't field

val obj1 :
  'f1 field -> 'f1 encoding
val obj2 :
  'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
val obj3 :
  'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
val obj4 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val obj5 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val obj6 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val obj7 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val obj8 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val obj9 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val obj10 :
  'f1 field -> 'f2 field -> 'f3 field -> 'f4 field -> 'f5 field ->
  'f6 field -> 'f7 field -> 'f8 field -> 'f9 field -> 'f10 field ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val tup1 :
  'f1 encoding ->
  'f1 encoding
val tup2 :
  'f1 encoding -> 'f2 encoding ->
  ('f1 * 'f2) encoding
val tup3 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding ->
  ('f1 * 'f2 * 'f3) encoding
val tup4 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4) encoding
val tup5 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding
val tup6 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding
val tup7 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding
val tup8 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding
val tup9 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding
val tup10 :
  'f1 encoding -> 'f2 encoding -> 'f3 encoding -> 'f4 encoding ->
  'f5 encoding -> 'f6 encoding -> 'f7 encoding -> 'f8 encoding ->
  'f9 encoding -> 'f10 encoding ->
  ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding
val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

val array : 'a encoding -> 'a array encoding
val list : 'a encoding -> 'a list encoding

val assoc : 'a encoding -> (string * 'a) list encoding

type 't case
val case :
  ?tag:int ->
  'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case
val union :
  ?tag_size:[ `Uint8 | `Uint16 ] -> 't case list -> 't encoding

val describe :
  ?title:string -> ?description:string ->
  't encoding ->'t encoding

val def : string -> 'a encoding -> 'a encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) ->
  ?schema:Json_schema.schema ->
  'b encoding -> 'a encoding

val mu : string -> ('a encoding -> 'a encoding) -> 'a encoding

module Json : sig

  val convert : 'a encoding -> 'a Json_encoding.encoding

  val schema : 'a encoding -> json_schema
  val construct : 't encoding -> 't -> json
  val destruct : 't encoding -> json -> 't

  (** JSON Error *)

  type path = path_item list
  and path_item =
    [ `Field of string
    (** A field in an object. *)
    | `Index of int
    (** An index in an array. *)
    | `Star
    (** Any / every field or index. *)
    | `Next
      (** The next element after an array. *) ]

  (** Exception raised by destructors, with the location in the original
      JSON structure and the specific error. *)
  exception Cannot_destruct of (path * exn)

  (** Unexpected kind of data encountered (w/ the expectation). *)
  exception Unexpected of string * string

  (** Some {!union} couldn't be destructed, w/ the reasons for each {!case}. *)
  exception No_case_matched of exn list

  (** Array of unexpected size encountered  (w/ the expectation). *)
  exception Bad_array_size of int * int

  (** Missing field in an object. *)
  exception Missing_field of string

  (** Supernumerary field in an object. *)
  exception Unexpected_field of string

  val print_error :
    ?print_unknown: (Format.formatter -> exn -> unit) ->
    Format.formatter -> exn -> unit

  (** Helpers for writing encoders. *)
  val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a
  val wrap_error : ('a -> 'b) -> 'a -> 'b

end

module Binary : sig

  (** [length encoding value] is the length of [value] in given [encoding] *)
  val length : 'a encoding -> 'a -> int

  (** When successful, [read encoding buffer offset length] is [Some(pos,obj)],
      where [pos] is the position after reading the object in, starting from
      [offset] and following the parsing rules specified by [encoding], and
      [obj] is the object read. Returns None on failure. *)
  val read : 'a encoding -> MBytes.t -> int -> int -> (int * 'a) option

  (** When successful, [write value encoding buffer offset] is [Some(pos)],
      where [pos] is the next postion in [buffer] after the representation of
      [value] just written to [buffer] according to the rules specified in
      [encoding]. Returns None on failure, but [buffer] is potentially left in
      partially written state. *)
  val write : 'a encoding -> 'a -> MBytes.t -> int -> int option

  (** [to_bytes encoding value] is a buffer containing the representation of
      [value] according to [encoding]. *)
  val to_bytes : 'a encoding -> 'a -> MBytes.t

  (** When successful, [of_bytes encoding buffer] is [Some(obj)] where [obj] is
      the object represented in the buffer according to the parsing rules
      specified by [encoding]. Returns None on failure. Will fail if the entire
      buffer is not read. *)
  val of_bytes : 'a encoding -> MBytes.t -> 'a option

  (** Like [of_bytes], but raises [Failure] exception on failure and returns raw
     value on success. *)
  val of_bytes_exn : 'a encoding -> MBytes.t -> 'a

  (** [to_bytes_list ?copy_blocks blocks_size encod data] encode the
      given data as a list of successive blocks of length
      'blocks_size' at most.

      NB. If 'copy_blocks' is false (default), the blocks of the list
      can be garbage-collected only when all the blocks are
      unreachable (because of the 'optimized' implementation of
      MBytes.sub used internally *)
  val to_bytes_list : ?copy_blocks:bool -> int  -> 'a t -> 'a -> MBytes.t list

  (** This type is used when decoding binary data incrementally.
      - In case of 'Success', the decoded data, the size of used data
       to decode the result, and the remaining data are returned
      - In case of error, 'Error' is returned
      - 'Await' status embeds a function that waits for additional data
       to continue decoding, when given data are not sufficient *)
  type 'a status =
    | Success of { res : 'a ; res_len : int ; remaining : MBytes.t list }
    | Await of (MBytes.t -> 'a status)
    | Error

  (** This function allows to decode (or to initialize decoding) a
      stream of 'MByte.t'. The given data encoding should have a
      'Fixed' or a 'Dynamic' size, otherwise an exception
      'Invalid_argument "streaming data with variable size"' is
      raised *)
  val read_stream_of_bytes : ?init:MBytes.t list -> 'a t -> 'a status

  (** Like read_stream_of_bytes, but only checks that the stream can
      be read. Note that this is an approximation because failures
      that may come from conversion functions present in encodings are
      not checked *)
  val check_stream_of_bytes : ?init:MBytes.t list -> 'a t -> unit status

  (** [fixed_length encoding] is [Some n] if [encoding] corresponds to a
      fixed-length representation of length [n], [None] otherwise. *)
  val fixed_length : 'a encoding -> int option

  (** Like [fixed_length], but raises [Invalid_argument] exception on failure
      and returns raw value on success. *)
  val fixed_length_exn : 'a encoding -> int

end
