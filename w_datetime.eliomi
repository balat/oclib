

{shared{
type slot = float * float * bool (* all day *) * float option deriving (Json)
}}
{client{
(* There are two version of the slot picker:
   With [~basic:false] (default), it uses widgets from Ocsigen Toolkit.
   With [~basic:true], it uses html select tags.
*)
val slot_picker :
  ?a:[< Html5_types.div_attrib > `Class ]
    Eliom_content.Html5.attrib list ->
  ?basic:bool ->
  ?default:CalendarLib.Calendar.t * CalendarLib.Calendar.t * bool *
           CalendarLib.Calendar.t option ->
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  ([`Ok | `Issue of string] *
   (CalendarLib.Calendar.t * CalendarLib.Calendar.t * bool *
    CalendarLib.Calendar.t option)) React.signal
}}
