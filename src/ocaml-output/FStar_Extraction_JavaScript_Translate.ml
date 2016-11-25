
open Prims

type file =
(Prims.string * FStar_Extraction_JavaScript_Ast.t)


type env =
{names : name Prims.list; module_name : Prims.string Prims.list} 
 and name =
{pretty : Prims.string; mut : Prims.bool}


let is_Mkenv : env  ->  Prims.bool = (Obj.magic ((fun _ -> (FStar_All.failwith "Not yet implemented:is_Mkenv"))))


let is_Mkname : name  ->  Prims.bool = (Obj.magic ((fun _ -> (FStar_All.failwith "Not yet implemented:is_Mkname"))))


let empty : Prims.string Prims.list  ->  env = (fun module_name -> {names = []; module_name = module_name})


let mk_op_bin : Prims.string  ->  FStar_Extraction_JavaScript_Ast.op_bin Prims.option = (fun _82_1 -> (match (_82_1) with
| "op_Addition" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Plus)
end
| "op_Subtraction" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Minus)
end
| "op_Multiply" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Mult)
end
| "op_Division" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Div)
end
| "op_Equality" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Equal)
end
| "op_disEquality" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_NotEqual)
end
| "op_LessThanOrEqual" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_LessThanEqual)
end
| "op_GreaterThanOrEqual" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_GreaterThanEqual)
end
| "op_LessThan" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_LessThan)
end
| "op_GreaterThan" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_GreaterThan)
end
| "op_Modulus" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSB_Mod)
end
| _82_25 -> begin
None
end))


let is_op_bin : Prims.string  ->  Prims.bool = (fun op -> ((mk_op_bin op) <> None))


let mk_op_un : Prims.string  ->  FStar_Extraction_JavaScript_Ast.op_un Prims.option = (fun _82_2 -> (match (_82_2) with
| "op_Negation" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSU_Not)
end
| "op_Minus" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSU_Minus)
end
| "op_Bang" -> begin
(FStar_All.failwith "todo: translation [!]")
end
| _82_32 -> begin
None
end))


let is_op_un : Prims.string  ->  Prims.bool = (fun op -> ((mk_op_un op) <> None))


let mk_op_bool : Prims.string  ->  FStar_Extraction_JavaScript_Ast.op_log Prims.option = (fun _82_3 -> (match (_82_3) with
| "op_AmpAmp" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSL_And)
end
| "op_BarBar" -> begin
Some (FStar_Extraction_JavaScript_Ast.JSL_Or)
end
| _82_38 -> begin
None
end))


let is_op_bool : Prims.string  ->  Prims.bool = (fun op -> ((mk_op_bool op) <> None))


let mk_standart_type : Prims.string  ->  FStar_Extraction_JavaScript_Ast.typ Prims.option = (fun _82_4 -> (match (_82_4) with
| "unit" -> begin
Some (FStar_Extraction_JavaScript_Ast.JST_Null)
end
| "bool" -> begin
Some (FStar_Extraction_JavaScript_Ast.JST_Boolean)
end
| ("int") | ("nat") -> begin
Some (FStar_Extraction_JavaScript_Ast.JST_Number)
end
| "string" -> begin
Some (FStar_Extraction_JavaScript_Ast.JST_String)
end
| _82_47 -> begin
None
end))


let is_standart_type : Prims.string  ->  Prims.bool = (fun t -> ((mk_standart_type t) <> None))


let float_of_int : Prims.int  ->  FStar_BaseTypes.float = (fun i -> (FStar_Util.float_of_int32 (FStar_Util.int32_of_int i)))


let export_modules : Prims.string Prims.list FStar_ST.ref = (FStar_ST.alloc [])


let current_module_name : Prims.string FStar_ST.ref = (FStar_ST.alloc "")


let lp_generic : Prims.string Prims.list FStar_ST.ref = (FStar_ST.alloc [])


let isEqVar : Prims.bool FStar_ST.ref = (FStar_ST.alloc false)


let getName = (fun _82_52 -> (match (_82_52) with
| (path, n) -> begin
(

let path = (FStar_String.concat "_" path)
in if ((path = (FStar_ST.read current_module_name)) || (path = "")) then begin
((n), (None))
end else begin
(

let _82_55 = if (not ((let _179_37 = (FStar_ST.read export_modules)
in (FStar_List.existsb (fun x -> (x = path)) _179_37)))) then begin
(let _179_39 = (let _179_38 = (FStar_ST.read export_modules)
in (FStar_List.append ((path)::[]) _179_38))
in (FStar_ST.op_Colon_Equals export_modules _179_39))
end else begin
()
end
in (((Prims.strcat path (Prims.strcat "." n))), (None)))
end)
end))


let rec is_pure_expr = (fun e var -> (match (e.FStar_Extraction_ML_Syntax.expr) with
| (FStar_Extraction_ML_Syntax.MLE_Const (_)) | (FStar_Extraction_ML_Syntax.MLE_Name (_)) -> begin
true
end
| FStar_Extraction_ML_Syntax.MLE_Var (n, _82_67) -> begin
(n <> (Prims.fst var))
end
| FStar_Extraction_ML_Syntax.MLE_Proj (expr, _82_72) -> begin
(is_pure_expr expr var)
end
| FStar_Extraction_ML_Syntax.MLE_CTor (p, le) -> begin
(not ((let _179_43 = (FStar_List.map (fun x -> (is_pure_expr x var)) le)
in (FStar_List.contains false _179_43))))
end
| FStar_Extraction_ML_Syntax.MLE_Tuple (le) -> begin
(not ((let _179_45 = (FStar_List.map (fun x -> (is_pure_expr x var)) le)
in (FStar_List.contains false _179_45))))
end
| FStar_Extraction_ML_Syntax.MLE_Record (_82_84, lne) -> begin
(not ((let _179_47 = (FStar_List.map (fun _82_90 -> (match (_82_90) with
| (n, e) -> begin
(is_pure_expr e var)
end)) lne)
in (FStar_List.contains false _179_47))))
end
| FStar_Extraction_ML_Syntax.MLE_App (_82_92, args) -> begin
(not ((let _179_49 = (FStar_List.map (fun x -> (is_pure_expr x var)) args)
in (FStar_List.contains false _179_49))))
end
| _82_98 -> begin
false
end))


let isMutable = (fun tys -> (match (tys) with
| None -> begin
false
end
| Some (_82_102, ty) -> begin
(match (ty) with
| FStar_Extraction_ML_Syntax.MLTY_Named (_82_107, p) when ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.ref") -> begin
true
end
| _82_112 -> begin
false
end)
end))


let rec translate : FStar_Extraction_ML_Syntax.mllib  ->  file Prims.list = (fun _82_114 -> (match (_82_114) with
| FStar_Extraction_ML_Syntax.MLLib (modules) -> begin
(FStar_List.filter_map (fun m -> (

let m_name = (

let _82_123 = m
in (match (_82_123) with
| ((prefix, final), _82_120, _82_122) -> begin
(FStar_String.concat "." (FStar_List.append prefix ((final)::[])))
end))
in try
(match (()) with
| () -> begin
(

let _82_133 = (FStar_Util.print1 "Attempting to translate module %s\n" m_name)
in (

let _82_135 = (FStar_ST.op_Colon_Equals export_modules [])
in (let _179_84 = (translate_module m)
in Some (_179_84))))
end)
with
| e -> begin
(

let _82_129 = (let _179_86 = (FStar_Util.print_exn e)
in (FStar_Util.print2 "Unable to translate module: %s because:\n  %s\n" m_name _179_86))
in None)
end)) modules)
end))
and translate_module : ((Prims.string Prims.list * Prims.string) * (FStar_Extraction_ML_Syntax.mlsig * FStar_Extraction_ML_Syntax.mlmodule) Prims.option * FStar_Extraction_ML_Syntax.mllib)  ->  file = (fun _82_141 -> (match (_82_141) with
| (module_name, modul, _82_140) -> begin
(

let module_name = (FStar_List.append (Prims.fst module_name) (((Prims.snd module_name))::[]))
in (

let program = (match (modul) with
| Some (_signature, decls) -> begin
(

let _82_147 = (FStar_ST.op_Colon_Equals current_module_name (FStar_String.concat "_" module_name))
in (

let res = (FStar_List.filter_map translate_decl decls)
in (

let create_module_imports = (let _179_93 = (let _179_91 = (let _179_89 = (FStar_ST.read export_modules)
in (FStar_List.map (fun m -> FStar_Extraction_JavaScript_Ast.JSS_ImportDeclaration (((m), (None)))) _179_89))
in (FStar_All.pipe_right _179_91 (fun _179_90 -> FStar_Extraction_JavaScript_Ast.JSS_Block (_179_90))))
in (FStar_All.pipe_right _179_93 (fun _179_92 -> FStar_Extraction_JavaScript_Ast.JS_Statement (_179_92))))
in (FStar_List.append ((create_module_imports)::[]) res))))
end
| _82_153 -> begin
(FStar_All.failwith "Unexpected standalone interface or nested modules")
end)
in (((FStar_String.concat "_" module_name)), (program))))
end))
and translate_decl : FStar_Extraction_ML_Syntax.mlmodule1  ->  FStar_Extraction_JavaScript_Ast.source_t Prims.option = (fun d -> (match (d) with
| FStar_Extraction_ML_Syntax.MLM_Let (_82_157, c_flag, lfunc) -> begin
(

let for1 = (fun _82_171 -> (match (_82_171) with
| {FStar_Extraction_ML_Syntax.mllb_name = (name, _82_169); FStar_Extraction_ML_Syntax.mllb_tysc = tys; FStar_Extraction_ML_Syntax.mllb_add_unit = unit_b; FStar_Extraction_ML_Syntax.mllb_def = expr; FStar_Extraction_ML_Syntax.print_typ = pt} -> begin
(

let t = (

let _82_172 = (FStar_ST.op_Colon_Equals lp_generic [])
in if ((not (pt)) || unit_b) then begin
None
end else begin
(match (tys) with
| None -> begin
None
end
| Some (lp, ty) -> begin
(

let _82_183 = (let _179_98 = (FStar_List.map (fun _82_182 -> (match (_82_182) with
| (id, _82_181) -> begin
id
end)) lp)
in (FStar_ST.op_Colon_Equals lp_generic _179_98))
in (let _179_100 = (translate_type ty)
in (FStar_All.pipe_right _179_100 (fun _179_99 -> Some (_179_99)))))
end)
end)
in (

let is_private = (FStar_List.contains FStar_Extraction_ML_Syntax.Private c_flag)
in (

let c = if (is_pure_expr expr ((name), (None))) then begin
(

let var_decl_q = if (isMutable tys) then begin
FStar_Extraction_JavaScript_Ast.JSV_Let
end else begin
FStar_Extraction_JavaScript_Ast.JSV_Const
end
in (let _179_105 = (let _179_104 = (let _179_103 = (let _179_102 = (let _179_101 = (translate_expr_pure expr)
in Some (_179_101))
in ((FStar_Extraction_JavaScript_Ast.JGP_Identifier (((name), (t)))), (_179_102)))
in ((_179_103), (var_decl_q)))
in FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (_179_104))
in (_179_105)::[]))
end else begin
(let _179_106 = (FStar_ST.alloc [])
in (translate_expr expr ((name), (t)) None false _179_106 (isMutable tys)))
end
in if is_private then begin
c
end else begin
(FStar_Extraction_JavaScript_Ast.JSS_ExportDefaultDeclaration (((FStar_Extraction_JavaScript_Ast.JSE_Declaration (FStar_Extraction_JavaScript_Ast.JSS_Block (c))), (FStar_Extraction_JavaScript_Ast.ExportValue))))::[]
end)))
end))
in (let _179_112 = (let _179_110 = (let _179_108 = (FStar_List.collect for1 lfunc)
in (FStar_All.pipe_right _179_108 (fun _179_107 -> FStar_Extraction_JavaScript_Ast.JSS_Block (_179_107))))
in (FStar_All.pipe_right _179_110 (fun _179_109 -> FStar_Extraction_JavaScript_Ast.JS_Statement (_179_109))))
in (FStar_All.pipe_right _179_112 (fun _179_111 -> Some (_179_111)))))
end
| FStar_Extraction_ML_Syntax.MLM_Loc (_82_190) -> begin
None
end
| FStar_Extraction_ML_Syntax.MLM_Ty (((_82_193, name, tparams, body))::[]) -> begin
(

let tparams = (match (tparams) with
| [] -> begin
None
end
| _82_202 -> begin
(let _179_115 = (FStar_List.map (fun _82_206 -> (match (_82_206) with
| (id, _82_205) -> begin
id
end)) tparams)
in (FStar_All.pipe_right _179_115 (fun _179_114 -> Some (_179_114))))
end)
in (

let forbody = (fun body -> (

let get_export = (fun stmt -> (FStar_All.pipe_right ((FStar_Extraction_JavaScript_Ast.JSE_Declaration (stmt)), (FStar_Extraction_JavaScript_Ast.ExportType)) (fun _179_120 -> FStar_Extraction_JavaScript_Ast.JSS_ExportDefaultDeclaration (_179_120))))
in (match (body) with
| FStar_Extraction_ML_Syntax.MLTD_Abbrev (t) -> begin
(let _179_123 = (let _179_122 = (let _179_121 = (translate_type t)
in ((((name), (None))), (tparams), (_179_121)))
in FStar_Extraction_JavaScript_Ast.JSS_TypeAlias (_179_122))
in (get_export _179_123))
end
| FStar_Extraction_ML_Syntax.MLTD_Record (fields) -> begin
(

let tag = (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (None)))), (FStar_Extraction_JavaScript_Ast.JST_StringLiteral ((("Record"), (""))))))::[]
in (

let fields_t = (FStar_List.map (fun _82_219 -> (match (_82_219) with
| (n, t) -> begin
(let _179_125 = (translate_type t)
in ((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((((Prims.strcat "_" n)), (None)))), (_179_125)))
end)) fields)
in (get_export (FStar_Extraction_JavaScript_Ast.JSS_TypeAlias (((((name), (None))), (tparams), (FStar_Extraction_JavaScript_Ast.JST_Object ((((FStar_List.append tag fields_t)), ([]), ([]))))))))))
end
| FStar_Extraction_ML_Syntax.MLTD_DType (lfields) -> begin
(

let tag = (fun n -> (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (None)))), (FStar_Extraction_JavaScript_Ast.JST_StringLiteral (((n), (""))))))::[])
in (

let fields_t = (fun fields -> (FStar_List.mapi (fun i t -> (let _179_136 = (let _179_134 = (let _179_133 = (let _179_132 = (FStar_Util.string_of_int i)
in (Prims.strcat "_" _179_132))
in ((_179_133), (None)))
in FStar_Extraction_JavaScript_Ast.JSO_Identifier (_179_134))
in (let _179_135 = (translate_type t)
in ((_179_136), (_179_135))))) fields))
in (

let lfields_t = (FStar_List.map (fun _82_231 -> (match (_82_231) with
| (n, l) -> begin
(let _179_143 = (let _179_142 = (let _179_141 = (let _179_140 = (let _179_139 = (let _179_138 = (fields_t l)
in (FStar_List.append (tag n) _179_138))
in ((_179_139), ([]), ([])))
in FStar_Extraction_JavaScript_Ast.JST_Object (_179_140))
in ((((n), (None))), (tparams), (_179_141)))
in FStar_Extraction_JavaScript_Ast.JSS_TypeAlias (_179_142))
in (get_export _179_143))
end)) lfields)
in (

let tparams_gen = (match (tparams) with
| Some (t) -> begin
(let _179_146 = (FStar_List.map (fun x -> FStar_Extraction_JavaScript_Ast.JST_Generic (((FStar_Extraction_JavaScript_Ast.Unqualified (((x), (None)))), (None)))) t)
in (FStar_All.pipe_right _179_146 (fun _179_145 -> Some (_179_145))))
end
| None -> begin
None
end)
in (

let lnames = (FStar_List.map (fun _82_240 -> (match (_82_240) with
| (n, l) -> begin
FStar_Extraction_JavaScript_Ast.JST_Generic (((FStar_Extraction_JavaScript_Ast.Unqualified (((n), (None)))), (tparams_gen)))
end)) lfields)
in (

let union_t = (get_export (FStar_Extraction_JavaScript_Ast.JSS_TypeAlias (((((name), (None))), (tparams), (FStar_Extraction_JavaScript_Ast.JST_Union (lnames))))))
in FStar_Extraction_JavaScript_Ast.JSS_Block ((FStar_List.append lfields_t ((union_t)::[])))))))))
end)))
in (

let body_t = (match (body) with
| None -> begin
(FStar_All.failwith "todo: translate_decl [MLM_Ty] with empty body")
end
| Some (v) -> begin
(forbody v)
end)
in (let _179_150 = (FStar_All.pipe_right body_t (fun _179_148 -> FStar_Extraction_JavaScript_Ast.JS_Statement (_179_148)))
in (FStar_All.pipe_right _179_150 (fun _179_149 -> Some (_179_149)))))))
end
| FStar_Extraction_ML_Syntax.MLM_Ty (_82_248) -> begin
(FStar_All.failwith "todo: translate_decl [MLM_Ty]")
end
| FStar_Extraction_ML_Syntax.MLM_Top (_82_251) -> begin
(FStar_All.failwith "todo: translate_decl [MLM_Top]")
end
| FStar_Extraction_ML_Syntax.MLM_Exn (x, []) -> begin
(let _179_153 = (FStar_All.pipe_right (FStar_Extraction_JavaScript_Ast.JSS_Block ([])) (fun _179_151 -> FStar_Extraction_JavaScript_Ast.JS_Statement (_179_151)))
in (FStar_All.pipe_right _179_153 (fun _179_152 -> Some (_179_152))))
end
| FStar_Extraction_ML_Syntax.MLM_Exn (_82_258) -> begin
(FStar_All.failwith "todo: translate_decl [MLM_Exn]")
end))
and translate_expr : FStar_Extraction_ML_Syntax.mlexpr  ->  (Prims.string * FStar_Extraction_JavaScript_Ast.typ Prims.option)  ->  FStar_Extraction_JavaScript_Ast.statement_t Prims.list Prims.option  ->  Prims.bool  ->  Prims.string Prims.list FStar_ST.ref  ->  Prims.bool  ->  FStar_Extraction_JavaScript_Ast.statement_t Prims.list = (fun e var lstmt isDecl lDecl isMutableV -> (

let get_res = (fun expr new_fv -> (

let lstmt = (match (lstmt) with
| Some (v) -> begin
v
end
| None -> begin
[]
end)
in (

let isAssgmnt = (FStar_ST.alloc [])
in (

let expr = (match (expr) with
| FStar_Extraction_JavaScript_Ast.JSE_Assignment (_82_275) -> begin
(

let _82_277 = (FStar_ST.op_Colon_Equals isAssgmnt ((FStar_Extraction_JavaScript_Ast.JSS_Expression (expr))::[]))
in FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), (""))))
end
| _82_280 -> begin
expr
end)
in (

let expr = if isDecl then begin
FStar_Extraction_JavaScript_Ast.JSS_Expression (FStar_Extraction_JavaScript_Ast.JSE_Assignment (((FStar_Extraction_JavaScript_Ast.JGP_Identifier (var)), (expr))))
end else begin
(

let _82_282 = (let _179_165 = (let _179_164 = (FStar_ST.read lDecl)
in (FStar_List.append (((Prims.fst var))::[]) _179_164))
in (FStar_ST.op_Colon_Equals lDecl _179_165))
in (

let var_decl_q = if isMutableV then begin
FStar_Extraction_JavaScript_Ast.JSV_Let
end else begin
FStar_Extraction_JavaScript_Ast.JSV_Const
end
in FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (var)), (Some (expr)))), (var_decl_q)))))
end
in (match (new_fv) with
| Some (v) -> begin
if (FStar_ST.read isEqVar) then begin
(

let _82_288 = (FStar_ST.op_Colon_Equals isEqVar false)
in (let _179_168 = (FStar_ST.read v)
in (let _179_167 = (let _179_166 = (FStar_ST.read isAssgmnt)
in (FStar_List.append _179_166 ((FStar_Extraction_JavaScript_Ast.JSS_Block ((FStar_List.append ((expr)::[]) lstmt)))::[])))
in (FStar_List.append _179_168 _179_167))))
end else begin
(let _179_171 = (FStar_ST.read v)
in (let _179_170 = (let _179_169 = (FStar_ST.read isAssgmnt)
in (FStar_List.append _179_169 (FStar_List.append ((expr)::[]) lstmt)))
in (FStar_List.append _179_171 _179_170)))
end
end
| None -> begin
(let _179_172 = (FStar_ST.read isAssgmnt)
in (FStar_List.append _179_172 (FStar_List.append ((expr)::[]) lstmt)))
end))))))
in (match (e.FStar_Extraction_ML_Syntax.expr) with
| x when (is_pure_expr e var) -> begin
(

let expr = (translate_expr_pure e)
in (get_res expr None))
end
| FStar_Extraction_ML_Syntax.MLE_Let ((_82_294, _82_296, ({FStar_Extraction_ML_Syntax.mllb_name = (name, _82_303); FStar_Extraction_ML_Syntax.mllb_tysc = tys; FStar_Extraction_ML_Syntax.mllb_add_unit = add_unit; FStar_Extraction_ML_Syntax.mllb_def = body; FStar_Extraction_ML_Syntax.print_typ = pt})::[]), continuation) -> begin
if (is_pure_expr body ((name), (None))) then begin
(

let isEqName = (let _179_174 = (FStar_ST.read lDecl)
in (FStar_List.existsb (fun x -> (x = name)) _179_174))
in (

let lDecl = if isEqName then begin
(FStar_ST.alloc ((name)::[]))
end else begin
(

let _82_313 = (let _179_176 = (let _179_175 = (FStar_ST.read lDecl)
in (FStar_List.append ((name)::[]) _179_175))
in (FStar_ST.op_Colon_Equals lDecl _179_176))
in lDecl)
end
in (

let c = (translate_expr continuation var lstmt isDecl lDecl isMutableV)
in (

let var_decl_q = if (isMutable tys) then begin
FStar_Extraction_JavaScript_Ast.JSV_Let
end else begin
FStar_Extraction_JavaScript_Ast.JSV_Const
end
in (

let c = (let _179_182 = (let _179_181 = (let _179_180 = (let _179_179 = (let _179_178 = (let _179_177 = (translate_expr_pure body)
in Some (_179_177))
in ((FStar_Extraction_JavaScript_Ast.JGP_Identifier (((name), (None)))), (_179_178)))
in ((_179_179), (var_decl_q)))
in FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (_179_180))
in (_179_181)::[])
in (FStar_List.append _179_182 c))
in (FStar_Extraction_JavaScript_Ast.JSS_Block (c))::[])))))
end else begin
(

let c = (translate_expr continuation var lstmt isDecl lDecl isMutableV)
in (translate_expr body ((name), (None)) (Some (c)) false lDecl (isMutable tys)))
end
end
| FStar_Extraction_ML_Syntax.MLE_Let (_82_321) -> begin
(FStar_All.failwith "todo: translate_expr [MLE_Let]")
end
| FStar_Extraction_ML_Syntax.MLE_Fun (args, body) -> begin
(

let args = (FStar_List.map (fun _82_332 -> (match (_82_332) with
| ((n, _82_329), t) -> begin
(let _179_186 = (let _179_185 = (let _179_184 = (translate_type t)
in Some (_179_184))
in ((n), (_179_185)))
in FStar_Extraction_JavaScript_Ast.JGP_Identifier (_179_186))
end)) args)
in (

let generic_t = (match ((FStar_ST.read lp_generic)) with
| [] -> begin
None
end
| _82_336 -> begin
(let _179_187 = (FStar_ST.read lp_generic)
in Some (_179_187))
end)
in (

let body_t = if (is_pure_expr body var) then begin
(let _179_188 = (translate_expr_pure body)
in FStar_Extraction_JavaScript_Ast.JS_BodyExpression (_179_188))
end else begin
(let _179_191 = (let _179_190 = (let _179_189 = (FStar_ST.alloc [])
in (translate_expr body (("_res"), (None)) (Some ((FStar_Extraction_JavaScript_Ast.JSS_Return (Some (FStar_Extraction_JavaScript_Ast.JSE_Identifier ((("_res"), (None))))))::[])) true _179_189 true))
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier ((("_res"), (None)))), (None))), (FStar_Extraction_JavaScript_Ast.JSV_Let))))::[]) _179_190))
in FStar_Extraction_JavaScript_Ast.JS_BodyBlock (_179_191))
end
in (

let ret_t = (match ((Prims.snd var)) with
| None -> begin
None
end
| Some (v) -> begin
(match (v) with
| FStar_Extraction_JavaScript_Ast.JST_Function (_82_343, t2, _82_346) -> begin
Some (t2)
end
| _82_350 -> begin
None
end)
end)
in (

let expr = FStar_Extraction_JavaScript_Ast.JSE_ArrowFunction (((None), (args), (body_t), (ret_t), (generic_t)))
in (

let lstmt = (match (lstmt) with
| Some (v) -> begin
v
end
| None -> begin
[]
end)
in (

let expr = if isDecl then begin
(FStar_Extraction_JavaScript_Ast.JSS_Expression (FStar_Extraction_JavaScript_Ast.JSE_Assignment (((FStar_Extraction_JavaScript_Ast.JGP_Identifier ((((Prims.fst var)), (None)))), (expr)))))::[]
end else begin
(

let _82_357 = (let _179_193 = (let _179_192 = (FStar_ST.read lDecl)
in (FStar_List.append (((Prims.fst var))::[]) _179_192))
in (FStar_ST.op_Colon_Equals lDecl _179_193))
in (FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier ((((Prims.fst var)), (None)))), (Some (expr)))), (FStar_Extraction_JavaScript_Ast.JSV_Const))))::[])
end
in (FStar_List.append expr lstmt))))))))
end
| FStar_Extraction_ML_Syntax.MLE_If (cond, s1, s2) -> begin
(

let s1 = (let _179_195 = (let _179_194 = (FStar_ST.alloc [])
in (translate_expr s1 var None true _179_194 isMutableV))
in FStar_Extraction_JavaScript_Ast.JSS_Block (_179_195))
in (

let s2 = (match (s2) with
| Some (v) -> begin
(let _179_198 = (let _179_197 = (let _179_196 = (FStar_ST.alloc [])
in (translate_expr v var None true _179_196 isMutableV))
in FStar_Extraction_JavaScript_Ast.JSS_Block (_179_197))
in Some (_179_198))
end
| None -> begin
None
end)
in (

let c = if (is_pure_expr cond var) then begin
(let _179_201 = (let _179_200 = (let _179_199 = (translate_expr_pure cond)
in ((_179_199), (s1), (s2)))
in FStar_Extraction_JavaScript_Ast.JSS_If (_179_200))
in (_179_201)::[])
end else begin
(let _179_203 = (let _179_202 = (FStar_ST.alloc [])
in (translate_expr cond (("_cond"), (None)) (Some ((FStar_Extraction_JavaScript_Ast.JSS_If (((FStar_Extraction_JavaScript_Ast.JSE_Identifier ((("_cond"), (Some (FStar_Extraction_JavaScript_Ast.JST_Boolean))))), (s1), (s2))))::[])) true _179_202 true))
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier ((("_cond"), (Some (FStar_Extraction_JavaScript_Ast.JST_Boolean))))), (None))), (FStar_Extraction_JavaScript_Ast.JSV_Let))))::[]) _179_203))
end
in (

let c = if isDecl then begin
c
end else begin
(

let _82_371 = (let _179_205 = (let _179_204 = (FStar_ST.read lDecl)
in (FStar_List.append (((Prims.fst var))::[]) _179_204))
in (FStar_ST.op_Colon_Equals lDecl _179_205))
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (var)), (None))), (FStar_Extraction_JavaScript_Ast.JSV_Let))))::[]) c))
end
in (match (lstmt) with
| Some (v) -> begin
(FStar_List.append c v)
end
| None -> begin
c
end)))))
end
| FStar_Extraction_ML_Syntax.MLE_Raise (_82_378) -> begin
(FStar_All.failwith "todo: translate_expr [MLE_Raise]")
end
| FStar_Extraction_ML_Syntax.MLE_Try (_82_381) -> begin
(FStar_All.failwith "todo: translate_expr [MLE_Try]")
end
| FStar_Extraction_ML_Syntax.MLE_Coerce (in_e, t_from, t_to) -> begin
(

let var = (let _179_207 = (let _179_206 = (translate_type in_e.FStar_Extraction_ML_Syntax.mlty)
in Some (_179_206))
in (((Prims.fst var)), (_179_207)))
in (translate_expr in_e var lstmt isDecl lDecl isMutableV))
end
| FStar_Extraction_ML_Syntax.MLE_Match (e_in, lb) -> begin
(

let match_e = (let _179_208 = (FStar_Absyn_Util.gensym ())
in ((_179_208), (None)))
in (

let c = if (is_pure_expr e_in var) then begin
(let _179_215 = (let _179_212 = (let _179_211 = (let _179_210 = (let _179_209 = (translate_expr_pure e_in)
in Some (_179_209))
in ((FStar_Extraction_JavaScript_Ast.JGP_Identifier (match_e)), (_179_210)))
in ((_179_211), (FStar_Extraction_JavaScript_Ast.JSV_Const)))
in FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (_179_212))
in (let _179_214 = (let _179_213 = (translate_match lb (FStar_Extraction_JavaScript_Ast.JSE_Identifier (match_e)) var isMutableV)
in (_179_213)::[])
in (_179_215)::_179_214))
end else begin
(let _179_220 = (let _179_219 = (let _179_217 = (let _179_216 = (translate_match lb (FStar_Extraction_JavaScript_Ast.JSE_Identifier (match_e)) var isMutableV)
in (_179_216)::[])
in Some (_179_217))
in (let _179_218 = (FStar_ST.alloc [])
in (translate_expr e_in match_e _179_219 true _179_218 true)))
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (match_e)), (None))), (FStar_Extraction_JavaScript_Ast.JSV_Let))))::[]) _179_220))
end
in (

let c = if isDecl then begin
c
end else begin
(FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (var)), (None))), (FStar_Extraction_JavaScript_Ast.JSV_Let))))::[]) c)
end
in (match (lstmt) with
| Some (v) -> begin
(FStar_List.append c v)
end
| None -> begin
c
end))))
end
| FStar_Extraction_ML_Syntax.MLE_Seq (ls) -> begin
(

let rec translate_seq = (fun l -> (match (l) with
| [] -> begin
(FStar_All.failwith "Empty list in [MLE_Seq]")
end
| (x)::[] -> begin
(translate_expr x var None isDecl lDecl isMutableV)
end
| (hd)::tl -> begin
(let _179_224 = (let _179_223 = (translate_seq tl)
in Some (_179_223))
in (translate_expr hd (("_"), (None)) _179_224 false lDecl true))
end))
in (

let c = (translate_seq ls)
in (match (lstmt) with
| Some (v) -> begin
(FStar_List.append c v)
end
| None -> begin
c
end)))
end
| FStar_Extraction_ML_Syntax.MLE_App (e, args) -> begin
(

let new_fv = (FStar_ST.alloc [])
in (

let args = (create_pure_args new_fv args var)
in (

let expr = (translate_arg_app e args var)
in (get_res expr (Some (new_fv))))))
end
| FStar_Extraction_ML_Syntax.MLE_CTor ((path, c), lexpr) -> begin
(

let new_fv = (FStar_ST.alloc [])
in (

let lexpr = (create_pure_args new_fv lexpr var)
in (

let expr = (match (c) with
| x when ((x = "Cons") || (x = "Nil")) -> begin
(match (lexpr) with
| [] -> begin
FStar_Extraction_JavaScript_Ast.JSE_Array (None)
end
| (hd)::tl -> begin
FStar_Extraction_JavaScript_Ast.JSE_Call (((FStar_Extraction_JavaScript_Ast.JSE_Member (((FStar_Extraction_JavaScript_Ast.JSE_Array (Some ((hd)::[]))), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("concat"), (None))))))), (tl)))
end)
end
| x when ((x = "Some") || (x = "None")) -> begin
(match (lexpr) with
| [] -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), ("")))
end
| (hd)::tl -> begin
(FStar_List.nth lexpr (Prims.parse_int "0"))
end)
end
| _82_439 -> begin
(let _179_233 = (let _179_232 = (FStar_List.mapi (fun i x -> (let _179_231 = (let _179_230 = (let _179_229 = (let _179_228 = (let _179_227 = (FStar_Util.string_of_int i)
in (Prims.strcat "_" _179_227))
in ((_179_228), (None)))
in FStar_Extraction_JavaScript_Ast.JSO_Identifier (_179_229))
in ((_179_230), (x), (FStar_Extraction_JavaScript_Ast.JSO_Init)))
in FStar_Extraction_JavaScript_Ast.JSPO_Property (_179_231))) lexpr)
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSPO_Property (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String (c)), ("")))), (FStar_Extraction_JavaScript_Ast.JSO_Init))))::[]) _179_232))
in FStar_Extraction_JavaScript_Ast.JSE_Object (_179_233))
end)
in (get_res expr (Some (new_fv))))))
end
| FStar_Extraction_ML_Syntax.MLE_Record (path, fields) -> begin
(

let new_fv = (FStar_ST.alloc [])
in (

let create_fields = (FStar_List.map (fun _82_450 -> (match (_82_450) with
| (id, x) -> begin
(let _179_237 = (let _179_236 = (let _179_235 = (create_pure_args new_fv ((x)::[]) var)
in (FStar_List.nth _179_235 (Prims.parse_int "0")))
in ((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((((Prims.strcat "_" id)), (None)))), (_179_236), (FStar_Extraction_JavaScript_Ast.JSO_Init)))
in FStar_Extraction_JavaScript_Ast.JSPO_Property (_179_237))
end)) fields)
in (

let expr = FStar_Extraction_JavaScript_Ast.JSE_Object ((FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSPO_Property (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String ("Record")), ("")))), (FStar_Extraction_JavaScript_Ast.JSO_Init))))::[]) create_fields))
in (get_res expr (Some (new_fv))))))
end
| FStar_Extraction_ML_Syntax.MLE_Tuple (lexp) -> begin
(

let new_fv = (FStar_ST.alloc [])
in (

let create_fields = (FStar_List.map (fun x -> (let _179_239 = (create_pure_args new_fv ((x)::[]) var)
in (FStar_List.nth _179_239 (Prims.parse_int "0")))) lexp)
in (

let expr = FStar_Extraction_JavaScript_Ast.JSE_Array (Some (create_fields))
in (get_res expr (Some (new_fv))))))
end
| _82_460 -> begin
(FStar_All.failwith "todo: translation ml-expr")
end)))
and create_pure_args : FStar_Extraction_JavaScript_Ast.statement_t Prims.list FStar_ST.ref  ->  FStar_Extraction_ML_Syntax.mlexpr Prims.list  ->  (Prims.string * FStar_Extraction_JavaScript_Ast.typ Prims.option)  ->  FStar_Extraction_JavaScript_Ast.expression_t Prims.list = (fun new_fv args var -> (FStar_List.map (fun x -> (match (x.FStar_Extraction_ML_Syntax.expr) with
| FStar_Extraction_ML_Syntax.MLE_CTor ((path, c), _82_469) when ((c = "Nil") || (c = "None")) -> begin
(let _179_246 = (let _179_245 = (translate_expr_pure x)
in (let _179_244 = (translate_type x.FStar_Extraction_ML_Syntax.mlty)
in ((_179_245), (_179_244))))
in FStar_Extraction_JavaScript_Ast.JSE_TypeCast (_179_246))
end
| _82_473 -> begin
if (is_pure_expr x var) then begin
(translate_expr_pure x)
end else begin
(

let fv_x = (FStar_Absyn_Util.gensym ())
in (

let c = (match (x.FStar_Extraction_ML_Syntax.expr) with
| FStar_Extraction_ML_Syntax.MLE_Var (_82_476) -> begin
(

let _82_478 = (FStar_ST.op_Colon_Equals isEqVar true)
in (let _179_251 = (let _179_250 = (let _179_249 = (let _179_248 = (let _179_247 = (translate_expr_pure x)
in Some (_179_247))
in ((FStar_Extraction_JavaScript_Ast.JGP_Identifier (((fv_x), (None)))), (_179_248)))
in ((_179_249), (FStar_Extraction_JavaScript_Ast.JSV_Const)))
in FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (_179_250))
in (_179_251)::[]))
end
| _82_481 -> begin
(let _179_252 = (FStar_ST.alloc [])
in (translate_expr x ((fv_x), (None)) None false _179_252 false))
end)
in (

let _82_483 = (let _179_254 = (let _179_253 = (FStar_ST.read new_fv)
in (FStar_List.append _179_253 c))
in (FStar_ST.op_Colon_Equals new_fv _179_254))
in FStar_Extraction_JavaScript_Ast.JSE_Identifier (((fv_x), (None))))))
end
end)) args))
and translate_arg_app : FStar_Extraction_ML_Syntax.mlexpr  ->  FStar_Extraction_JavaScript_Ast.expression_t Prims.list  ->  (Prims.string * FStar_Extraction_JavaScript_Ast.typ Prims.option)  ->  FStar_Extraction_JavaScript_Ast.expression_t = (fun e args var -> (match (e.FStar_Extraction_ML_Syntax.expr) with
| FStar_Extraction_ML_Syntax.MLE_Name (("Prims")::[], op) when (is_op_bin op) -> begin
(let _179_261 = (let _179_260 = (FStar_Util.must (mk_op_bin op))
in (let _179_259 = (FStar_List.nth args (Prims.parse_int "0"))
in (let _179_258 = (FStar_List.nth args (Prims.parse_int "1"))
in ((_179_260), (_179_259), (_179_258)))))
in FStar_Extraction_JavaScript_Ast.JSE_Binary (_179_261))
end
| FStar_Extraction_ML_Syntax.MLE_Name (("Prims")::[], op) when (is_op_bool op) -> begin
(let _179_265 = (let _179_264 = (FStar_Util.must (mk_op_bool op))
in (let _179_263 = (FStar_List.nth args (Prims.parse_int "0"))
in (let _179_262 = (FStar_List.nth args (Prims.parse_int "1"))
in ((_179_264), (_179_263), (_179_262)))))
in FStar_Extraction_JavaScript_Ast.JSE_Logical (_179_265))
end
| FStar_Extraction_ML_Syntax.MLE_Name (("Prims")::[], op) when (is_op_un op) -> begin
(let _179_269 = (let _179_268 = (let _179_266 = (mk_op_un op)
in (FStar_Util.must _179_266))
in (let _179_267 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_268), (_179_267))))
in FStar_Extraction_JavaScript_Ast.JSE_Unary (_179_269))
end
| FStar_Extraction_ML_Syntax.MLE_Name (p) when (((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.Buffer.op_Array_Access") || ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.Buffer.index")) -> begin
(

let ind = (let _179_274 = (let _179_273 = (let _179_271 = (let _179_270 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_270), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_idx"), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_271))
in (let _179_272 = (FStar_List.nth args (Prims.parse_int "1"))
in ((FStar_Extraction_JavaScript_Ast.JSB_Plus), (_179_273), (_179_272))))
in FStar_Extraction_JavaScript_Ast.JSE_Binary (_179_274))
in (let _179_278 = (let _179_277 = (let _179_276 = (let _179_275 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_275), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_content"), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_276))
in ((_179_277), (FStar_Extraction_JavaScript_Ast.JSPM_Expression (ind))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_278)))
end
| FStar_Extraction_ML_Syntax.MLE_Name (p) when ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.Buffer.op_Array_Assignment") -> begin
(

let ind = (let _179_283 = (let _179_282 = (let _179_280 = (let _179_279 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_279), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_idx"), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_280))
in (let _179_281 = (FStar_List.nth args (Prims.parse_int "1"))
in ((FStar_Extraction_JavaScript_Ast.JSB_Plus), (_179_282), (_179_281))))
in FStar_Extraction_JavaScript_Ast.JSE_Binary (_179_283))
in (

let expr = (let _179_287 = (let _179_286 = (let _179_285 = (let _179_284 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_284), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_content"), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_285))
in ((_179_286), (FStar_Extraction_JavaScript_Ast.JSPM_Expression (ind))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_287))
in (let _179_289 = (let _179_288 = (FStar_List.nth args (Prims.parse_int "2"))
in ((FStar_Extraction_JavaScript_Ast.JGP_Expression (expr)), (_179_288)))
in FStar_Extraction_JavaScript_Ast.JSE_Assignment (_179_289))))
end
| FStar_Extraction_ML_Syntax.MLE_Name (p) when (((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.op_Bang") || ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.read")) -> begin
(let _179_291 = (let _179_290 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_290), (FStar_Extraction_JavaScript_Ast.JSPM_Expression (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), ("0")))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_291))
end
| FStar_Extraction_ML_Syntax.MLE_Name (p) when (((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.op_Colon_Equals") || ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.write")) -> begin
(

let expr = (let _179_293 = (let _179_292 = (FStar_List.nth args (Prims.parse_int "0"))
in ((_179_292), (FStar_Extraction_JavaScript_Ast.JSPM_Expression (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), ("0")))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_293))
in (let _179_295 = (let _179_294 = (FStar_List.nth args (Prims.parse_int "1"))
in ((FStar_Extraction_JavaScript_Ast.JGP_Expression (expr)), (_179_294)))
in FStar_Extraction_JavaScript_Ast.JSE_Assignment (_179_295)))
end
| FStar_Extraction_ML_Syntax.MLE_Name (p) when ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.alloc") -> begin
(let _179_298 = (let _179_297 = (let _179_296 = (FStar_List.nth args (Prims.parse_int "0"))
in (_179_296)::[])
in Some (_179_297))
in FStar_Extraction_JavaScript_Ast.JSE_Array (_179_298))
end
| FStar_Extraction_ML_Syntax.MLE_Name (path, function_name) -> begin
(let _179_301 = (let _179_300 = (let _179_299 = (getName ((path), (function_name)))
in FStar_Extraction_JavaScript_Ast.JSE_Identifier (_179_299))
in ((_179_300), (args)))
in FStar_Extraction_JavaScript_Ast.JSE_Call (_179_301))
end
| FStar_Extraction_ML_Syntax.MLE_Var (name, _82_523) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Call (((FStar_Extraction_JavaScript_Ast.JSE_Identifier (((name), (None)))), (args)))
end
| _82_527 -> begin
if (is_pure_expr e var) then begin
(let _179_303 = (let _179_302 = (translate_expr_pure e)
in ((_179_302), (args)))
in FStar_Extraction_JavaScript_Ast.JSE_Call (_179_303))
end else begin
(FStar_All.failwith "todo: translation [MLE_App]")
end
end))
and translate_expr_pure : FStar_Extraction_ML_Syntax.mlexpr  ->  FStar_Extraction_JavaScript_Ast.expression_t = (fun e -> (match (e.FStar_Extraction_ML_Syntax.expr) with
| FStar_Extraction_ML_Syntax.MLE_Const (c) -> begin
(translate_constant c)
end
| FStar_Extraction_ML_Syntax.MLE_Var (name, _82_533) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Identifier (((name), (None)))
end
| FStar_Extraction_ML_Syntax.MLE_Name (path, n) -> begin
(let _179_305 = (getName ((path), (n)))
in FStar_Extraction_JavaScript_Ast.JSE_Identifier (_179_305))
end
| FStar_Extraction_ML_Syntax.MLE_Tuple (lexp) -> begin
(let _179_307 = (let _179_306 = (FStar_List.map translate_expr_pure lexp)
in Some (_179_306))
in FStar_Extraction_JavaScript_Ast.JSE_Array (_179_307))
end
| FStar_Extraction_ML_Syntax.MLE_Record (path, fields) -> begin
(

let create_fields = (FStar_List.map (fun _82_548 -> (match (_82_548) with
| (id, x) -> begin
(let _179_310 = (let _179_309 = (translate_expr_pure x)
in ((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((((Prims.strcat "_" id)), (None)))), (_179_309), (FStar_Extraction_JavaScript_Ast.JSO_Init)))
in FStar_Extraction_JavaScript_Ast.JSPO_Property (_179_310))
end)) fields)
in FStar_Extraction_JavaScript_Ast.JSE_Object ((FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSPO_Property (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String ("Record")), ("")))), (FStar_Extraction_JavaScript_Ast.JSO_Init))))::[]) create_fields)))
end
| FStar_Extraction_ML_Syntax.MLE_CTor ((path, c), lexpr) -> begin
(match (c) with
| x when ((x = "Cons") || (x = "Nil")) -> begin
(match (lexpr) with
| [] -> begin
FStar_Extraction_JavaScript_Ast.JSE_Array (None)
end
| (hd)::tl -> begin
(let _179_318 = (let _179_317 = (let _179_315 = (let _179_314 = (let _179_313 = (let _179_312 = (let _179_311 = (translate_expr_pure hd)
in (_179_311)::[])
in Some (_179_312))
in FStar_Extraction_JavaScript_Ast.JSE_Array (_179_313))
in ((_179_314), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("concat"), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_315))
in (let _179_316 = (FStar_List.map translate_expr_pure tl)
in ((_179_317), (_179_316))))
in FStar_Extraction_JavaScript_Ast.JSE_Call (_179_318))
end)
end
| x when ((x = "Some") || (x = "None")) -> begin
(match (lexpr) with
| [] -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), ("")))
end
| (hd)::tl -> begin
(let _179_319 = (FStar_List.map translate_expr_pure lexpr)
in (FStar_List.nth _179_319 (Prims.parse_int "0")))
end)
end
| _82_567 -> begin
(let _179_329 = (let _179_328 = (FStar_List.mapi (fun i x -> (let _179_327 = (let _179_326 = (let _179_324 = (let _179_323 = (let _179_322 = (FStar_Util.string_of_int i)
in (Prims.strcat "_" _179_322))
in ((_179_323), (None)))
in FStar_Extraction_JavaScript_Ast.JSO_Identifier (_179_324))
in (let _179_325 = (translate_expr_pure x)
in ((_179_326), (_179_325), (FStar_Extraction_JavaScript_Ast.JSO_Init))))
in FStar_Extraction_JavaScript_Ast.JSPO_Property (_179_327))) lexpr)
in (FStar_List.append ((FStar_Extraction_JavaScript_Ast.JSPO_Property (((FStar_Extraction_JavaScript_Ast.JSO_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String (c)), ("")))), (FStar_Extraction_JavaScript_Ast.JSO_Init))))::[]) _179_328))
in FStar_Extraction_JavaScript_Ast.JSE_Object (_179_329))
end)
end
| FStar_Extraction_ML_Syntax.MLE_Coerce (e, _82_572, _82_574) -> begin
(translate_expr_pure e)
end
| FStar_Extraction_ML_Syntax.MLE_App (e, args) -> begin
(

let args = (FStar_List.map (fun x -> (match (x.FStar_Extraction_ML_Syntax.expr) with
| FStar_Extraction_ML_Syntax.MLE_CTor ((path, c), _82_586) when ((c = "Nil") || (c = "None")) -> begin
(let _179_333 = (let _179_332 = (translate_expr_pure x)
in (let _179_331 = (translate_type x.FStar_Extraction_ML_Syntax.mlty)
in ((_179_332), (_179_331))))
in FStar_Extraction_JavaScript_Ast.JSE_TypeCast (_179_333))
end
| _82_590 -> begin
(translate_expr_pure x)
end)) args)
in (translate_arg_app e args ((""), (None))))
end
| FStar_Extraction_ML_Syntax.MLE_Proj (expr, (path, name)) -> begin
(let _179_335 = (let _179_334 = (translate_expr_pure expr)
in ((_179_334), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((((Prims.strcat "_" name)), (None))))))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_335))
end
| _82_599 -> begin
(FStar_All.failwith "todo: translation ml-expr-pure")
end))
and translate_match : FStar_Extraction_ML_Syntax.mlbranch Prims.list  ->  FStar_Extraction_JavaScript_Ast.expression_t  ->  (Prims.string * FStar_Extraction_JavaScript_Ast.typ Prims.option)  ->  Prims.bool  ->  FStar_Extraction_JavaScript_Ast.statement_t = (fun lb fv_x var isMutableV -> (match (lb) with
| [] -> begin
FStar_Extraction_JavaScript_Ast.JSS_Throw (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String ("This value doesn\'t match!")), (""))))
end
| ((p, guard, expr_r))::tl -> begin
(

let expr_t = if (is_pure_expr expr_r var) then begin
(let _179_343 = (let _179_341 = (let _179_340 = (translate_expr_pure expr_r)
in ((FStar_Extraction_JavaScript_Ast.JGP_Identifier (var)), (_179_340)))
in FStar_Extraction_JavaScript_Ast.JSE_Assignment (_179_341))
in (FStar_All.pipe_right _179_343 (fun _179_342 -> FStar_Extraction_JavaScript_Ast.JSS_Expression (_179_342))))
end else begin
(let _179_346 = (let _179_344 = (FStar_ST.alloc [])
in (translate_expr expr_r var None true _179_344 isMutableV))
in (FStar_All.pipe_right _179_346 (fun _179_345 -> FStar_Extraction_JavaScript_Ast.JSS_Seq (_179_345))))
end
in (let _179_347 = (translate_match tl fv_x var isMutableV)
in (translate_pat_guard ((p), (guard)) fv_x expr_t _179_347)))
end))
and translate_pat_guard : (FStar_Extraction_ML_Syntax.mlpattern * FStar_Extraction_ML_Syntax.mlexpr Prims.option)  ->  FStar_Extraction_JavaScript_Ast.expression_t  ->  FStar_Extraction_JavaScript_Ast.statement_t  ->  FStar_Extraction_JavaScript_Ast.statement_t  ->  FStar_Extraction_JavaScript_Ast.statement_t = (fun _82_614 fv_x s1 s2 -> (match (_82_614) with
| (p, guard) -> begin
(match (guard) with
| None -> begin
(translate_pat p fv_x s1 s2)
end
| Some (v_guard) -> begin
(

let cond_stmt = (let _179_353 = (let _179_352 = (translate_expr_pure v_guard)
in ((_179_352), (s1), (Some (s2))))
in FStar_Extraction_JavaScript_Ast.JSS_If (_179_353))
in (translate_pat p fv_x cond_stmt s2))
end)
end))
and translate_pat : FStar_Extraction_ML_Syntax.mlpattern  ->  FStar_Extraction_JavaScript_Ast.expression_t  ->  FStar_Extraction_JavaScript_Ast.statement_t  ->  FStar_Extraction_JavaScript_Ast.statement_t  ->  FStar_Extraction_JavaScript_Ast.statement_t = (fun p fv_x s1 s2 -> (match (p) with
| FStar_Extraction_ML_Syntax.MLP_Var (name, _82_628) -> begin
FStar_Extraction_JavaScript_Ast.JSS_Seq ((FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (((name), (None)))), (Some (fv_x)))), (FStar_Extraction_JavaScript_Ast.JSV_Const))))::(s1)::[])
end
| FStar_Extraction_ML_Syntax.MLP_Wild -> begin
s1
end
| FStar_Extraction_ML_Syntax.MLP_Const (c) -> begin
(let _179_361 = (let _179_360 = (let _179_359 = (let _179_358 = (translate_constant c)
in ((FStar_Extraction_JavaScript_Ast.JSB_Equal), (fv_x), (_179_358)))
in FStar_Extraction_JavaScript_Ast.JSE_Binary (_179_359))
in ((_179_360), (s1), (Some (s2))))
in FStar_Extraction_JavaScript_Ast.JSS_If (_179_361))
end
| FStar_Extraction_ML_Syntax.MLP_CTor ((path, c), lp) -> begin
(

let rec translate_p_ctor = (fun lp fv_x s1 s2 i -> (

let new_fv_x = (match (c) with
| x when (x = "Some") -> begin
fv_x
end
| x when ((x = "Cons") && (i = (Prims.parse_int "0"))) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Expression (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), ("0")))))))
end
| x when ((x = "Cons") && (i = (Prims.parse_int "1"))) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("slice(1)"), (None))))))
end
| _82_650 -> begin
(let _179_376 = (let _179_375 = (let _179_374 = (let _179_373 = (let _179_372 = (FStar_Util.string_of_int i)
in (Prims.strcat "_" _179_372))
in ((_179_373), (None)))
in FStar_Extraction_JavaScript_Ast.JSPM_Identifier (_179_374))
in ((fv_x), (_179_375)))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_376))
end)
in (match (lp) with
| [] -> begin
s1
end
| (x)::[] -> begin
(translate_pat x new_fv_x s1 s2)
end
| (hd)::tl -> begin
(let _179_377 = (translate_p_ctor tl fv_x s1 s2 (i + (Prims.parse_int "1")))
in (translate_pat hd new_fv_x _179_377 s2))
end)))
in (

let if_stmt = (fun if_cond -> (let _179_381 = (let _179_380 = (translate_p_ctor lp fv_x s1 s2 (Prims.parse_int "0"))
in ((if_cond), (_179_380), (Some (s2))))
in FStar_Extraction_JavaScript_Ast.JSS_If (_179_381)))
in (match (c) with
| x when (x = "Cons") -> begin
(if_stmt (FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_GreaterThan), (FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("length"), (None))))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), ("0"))))))))
end
| x when (x = "Nil") -> begin
(if_stmt (FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_Equal), (FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("length"), (None))))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), ("0"))))))))
end
| x when (x = "Some") -> begin
(if_stmt (FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_NotEqual), (fv_x), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), (""))))))))
end
| x when (x = "None") -> begin
(if_stmt (FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_Equal), (fv_x), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), (""))))))))
end
| _82_665 -> begin
(

let isSimple = (match (fv_x) with
| FStar_Extraction_JavaScript_Ast.JSE_Identifier (_82_667) -> begin
true
end
| _82_670 -> begin
false
end)
in if isSimple then begin
(if_stmt (FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_StrictEqual), (FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String)))))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String (c)), (""))))))))
end else begin
(

let new_name = (FStar_Absyn_Util.gensym ())
in (

let if_cond = FStar_Extraction_JavaScript_Ast.JSE_Binary (((FStar_Extraction_JavaScript_Ast.JSB_StrictEqual), (FStar_Extraction_JavaScript_Ast.JSE_Member (((FStar_Extraction_JavaScript_Ast.JSE_Identifier (((new_name), (None)))), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((("_tag"), (Some (FStar_Extraction_JavaScript_Ast.JST_String)))))))), (FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String (c)), (""))))))
in (let _179_386 = (let _179_385 = (let _179_384 = (let _179_383 = (let _179_382 = (translate_p_ctor lp (FStar_Extraction_JavaScript_Ast.JSE_Identifier (((new_name), (None)))) s1 s2 (Prims.parse_int "0"))
in ((if_cond), (_179_382), (Some (s2))))
in FStar_Extraction_JavaScript_Ast.JSS_If (_179_383))
in (_179_384)::[])
in (FStar_Extraction_JavaScript_Ast.JSS_VariableDeclaration (((((FStar_Extraction_JavaScript_Ast.JGP_Identifier (((new_name), (None)))), (Some (fv_x)))), (FStar_Extraction_JavaScript_Ast.JSV_Const))))::_179_385)
in FStar_Extraction_JavaScript_Ast.JSS_Seq (_179_386))))
end)
end)))
end
| FStar_Extraction_ML_Syntax.MLP_Branch (lp) -> begin
(

let rec translate_p_branch = (fun lp fv_x s1 s2 -> (match (lp) with
| [] -> begin
(FStar_All.failwith "Empty list in translate_p_branch")
end
| (x)::[] -> begin
(translate_pat x fv_x s1 s2)
end
| (hd)::tl -> begin
(let _179_395 = (translate_p_branch tl fv_x s1 s2)
in (translate_pat hd fv_x s1 _179_395))
end))
in (translate_p_branch lp fv_x s1 s2))
end
| FStar_Extraction_ML_Syntax.MLP_Record (path, lp) -> begin
(

let rec translate_p_record = (fun lp fv_x s1 s2 -> (

let new_fv_x = (fun n -> FStar_Extraction_JavaScript_Ast.JSE_Member (((fv_x), (FStar_Extraction_JavaScript_Ast.JSPM_Identifier ((((Prims.strcat "_" n)), (None)))))))
in (match (lp) with
| [] -> begin
(FStar_All.failwith "Empty list in translate_p_record")
end
| (x)::[] -> begin
(translate_pat (Prims.snd x) (new_fv_x (Prims.fst x)) s1 s2)
end
| (hd)::tl -> begin
(let _179_406 = (translate_p_record tl fv_x s1 s2)
in (translate_pat (Prims.snd hd) (new_fv_x (Prims.fst hd)) _179_406 s2))
end)))
in (translate_p_record lp fv_x s1 s2))
end
| FStar_Extraction_ML_Syntax.MLP_Tuple (lp) -> begin
(

let rec translate_p_tuple = (fun lp d fv_x s1 s2 -> (

let new_fv_x = (let _179_421 = (let _179_420 = (let _179_419 = (let _179_418 = (let _179_417 = (FStar_Util.string_of_int d)
in ((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int d))), (_179_417)))
in FStar_Extraction_JavaScript_Ast.JSE_Literal (_179_418))
in FStar_Extraction_JavaScript_Ast.JSPM_Expression (_179_419))
in ((fv_x), (_179_420)))
in FStar_Extraction_JavaScript_Ast.JSE_Member (_179_421))
in (match (lp) with
| [] -> begin
(FStar_All.failwith "Empty list in translate_p_tuple")
end
| (x)::[] -> begin
(translate_pat x new_fv_x s1 s2)
end
| (hd)::tl -> begin
(let _179_422 = (translate_p_tuple tl (d + (Prims.parse_int "1")) fv_x s1 s2)
in (translate_pat hd new_fv_x _179_422 s2))
end)))
in (translate_p_tuple lp (Prims.parse_int "0") fv_x s1 s2))
end))
and translate_constant : FStar_Extraction_ML_Syntax.mlconstant  ->  FStar_Extraction_JavaScript_Ast.expression_t = (fun c -> (match (c) with
| FStar_Extraction_ML_Syntax.MLC_Unit -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Null), ("")))
end
| FStar_Extraction_ML_Syntax.MLC_Bool (b) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Boolean (b)), ("")))
end
| FStar_Extraction_ML_Syntax.MLC_Int (s, _82_725) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number ((float_of_int (Prims.parse_int "0")))), (s)))
end
| FStar_Extraction_ML_Syntax.MLC_Float (f) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_Number (f)), ((FStar_Util.string_of_float f))))
end
| FStar_Extraction_ML_Syntax.MLC_Char (_82_731) -> begin
(FStar_All.failwith "todo: translate_const [MLC_Char]")
end
| FStar_Extraction_ML_Syntax.MLC_String (s) -> begin
FStar_Extraction_JavaScript_Ast.JSE_Literal (((FStar_Extraction_JavaScript_Ast.JSV_String (s)), (s)))
end
| FStar_Extraction_ML_Syntax.MLC_Bytes (_82_736) -> begin
(FStar_All.failwith "todo: translate_const [MLC_Bytes]")
end))
and translate_type : FStar_Extraction_ML_Syntax.mlty  ->  FStar_Extraction_JavaScript_Ast.typ = (fun t -> (match (t) with
| (FStar_Extraction_ML_Syntax.MLTY_Tuple ([])) | (FStar_Extraction_ML_Syntax.MLTY_Top) -> begin
FStar_Extraction_JavaScript_Ast.JST_Any
end
| FStar_Extraction_ML_Syntax.MLTY_Var (id, _82_744) -> begin
FStar_Extraction_JavaScript_Ast.JST_Generic (((FStar_Extraction_JavaScript_Ast.Unqualified (((id), (None)))), (None)))
end
| FStar_Extraction_ML_Syntax.MLTY_Tuple (lt) -> begin
(let _179_425 = (FStar_List.map translate_type lt)
in FStar_Extraction_JavaScript_Ast.JST_Tuple (_179_425))
end
| FStar_Extraction_ML_Syntax.MLTY_Fun (t1, _82_751, t2) -> begin
(let _179_430 = (let _179_429 = (let _179_427 = (let _179_426 = (translate_type t1)
in (((("_1"), (None))), (_179_426)))
in (_179_427)::[])
in (let _179_428 = (translate_type t2)
in ((_179_429), (_179_428), (None))))
in FStar_Extraction_JavaScript_Ast.JST_Function (_179_430))
end
| FStar_Extraction_ML_Syntax.MLTY_Named (args, p) when ((FStar_Extraction_ML_Syntax.string_of_mlpath p) = "FStar.ST.ref") -> begin
(let _179_432 = (let _179_431 = (FStar_List.nth args (Prims.parse_int "0"))
in (translate_type _179_431))
in FStar_Extraction_JavaScript_Ast.JST_Array (_179_432))
end
| FStar_Extraction_ML_Syntax.MLTY_Named (args, (path, name)) -> begin
if (is_standart_type name) then begin
(FStar_Util.must (mk_standart_type name))
end else begin
if (let _179_433 = (FStar_Extraction_ML_Util.is_xtuple_ty ((path), (name)))
in (FStar_Option.isSome _179_433)) then begin
(let _179_434 = (FStar_List.map translate_type args)
in FStar_Extraction_JavaScript_Ast.JST_Tuple (_179_434))
end else begin
(

let args_t = (match (args) with
| [] -> begin
None
end
| _82_767 -> begin
(let _179_436 = (FStar_List.map translate_type args)
in (FStar_All.pipe_right _179_436 (fun _179_435 -> Some (_179_435))))
end)
in (let _179_439 = (let _179_438 = (let _179_437 = (getName ((path), (name)))
in FStar_Extraction_JavaScript_Ast.Unqualified (_179_437))
in ((_179_438), (args_t)))
in FStar_Extraction_JavaScript_Ast.JST_Generic (_179_439)))
end
end
end))




