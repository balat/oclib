

{shared{
val input :
  ?a:[< Html5_types.input_attrib > `Value ] Eliom_content.Html5.attrib list ->
  value:string -> unit ->
  [> Html5_types.input ] Eliom_content.Html5.elt
  * string Eliom_shared.React.S.t
val form_element :
  ?a:[< Html5_types.div_attrib ] Eliom_content.Html5.attrib list ->
  [< Html5_types.flow5_without_header_footer ] Eliom_content.Html5.elt list ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.elt list ->
  [> Html5_types.div ] Eliom_content.Html5.elt

val subform :
  ?ok:bool -> ?signal:_ React.signal ->
  [< Html5_types.input ] Eliom_content.Html5.elt ->
  [< Html5_types.h2_content_fun ] Eliom_content.Html5.elt list ->
  [< Html5_types.div_content_fun > `Footer `Header ]
  Eliom_content.Html5.elt ->
  [> Html5_types.div ] Eliom_content.Html5.elt
}}
