

{client{
val location_picker :
  ?default:Html5_types.cdata -> unit ->
  [> Html5_types.input ] Eliom_content.Html5.elt *
  (string * string * float * float) option React.signal *
  (unit -> (string * string * float * float) option)
 }}
