open Arg

let session_id = ref 0
let foreground = ref false
let project_id = ref 0
let configfile = ref ""
let latitude = ref 0.0
let longitude = ref 0.0
let audio_format = ref "mp3"
let bitrate = ref 128

let usage_msg = "roundstream"

let speclist = [
  ("--session_id", Set_int session_id,
    " The ID describing this stream.");
  ("--foreground", Set foreground,
   " If true, will remain in the foreground and log to STDOUT.");
  ("--project_id", Set_int project_id,
   " The ID of the project to play. Default = 0");
  ("--configfile", Set_string configfile,
   " The configuration file for the stream.");
  ("--latitude", Set_float latitude,
   " The latitude where the listener is listening from.");
  ("--longitude", Set_float longitude,
   " The longitude where the listener is listening from.");
  ("--audio_format", Symbol (["mp3";"ogg"], (fun str->audio_format := str)),
   " Audio format can be mp3 or ogg.");
  ("--bitrate", Set_int bitrate,
   " The bitrate of the audio to be streamed.");
  ];;

let () =
    parse (align speclist) (fun _ -> raise (Bad "arg")) usage_msg;
    (*TODO Create logger *) 
    Printf.printf "session_id=%d\nforeground=%B\nproject_id=%d\nconfigfile=%s\nlatitude=%F\nlongitude=%F\naudio_format=%s\nbitrate=%d\n" !session_id !foreground !project_id !configfile !latitude !longitude !audio_format !bitrate

