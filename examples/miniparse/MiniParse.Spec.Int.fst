module MiniParse.Spec.Int
include MiniParse.Spec.Combinators

module U8 = FStar.UInt8

let parse_u8_kind = strong_parser_kind 1 1

let parse_u8 : parser parse_u8_kind U8.t = make_total_constant_size_parser 1 U8.t (fun x -> Seq.index x 0)