type stream_settings = {
    sample_rate : int;
    buffer_length : int;
    channels : int;
    host : string;
    port : int;
    user : string;
    password : string;
    mount : string;
    description : string
    };;

let round_settings = {
    sample_rate = 44100;
    buffer_length = 1024 * 16;
    channels = 2;
    host = "localhost";
    port = 8000;
    user = "source";
    password = "r0undice";
    mount = "test.mp3";
    description = "Roundware test";
    };;

(* TODO:
    * Ensure the buffer is always of the correct length
    * I don't check here that the number of channels and sample_rate match
      the stream. I should probably do resampling or something to ensure
      that it works properly. For now, the files all need to be the same.
*)
class wave_file_src (filename : string) =
    object
        val reader = new Audio.IO.Reader.of_wav_file filename
        val mutable finished = false

        method get_finished = finished

        method get_buffer length sample_rate =
            let ratio = float sample_rate /. float reader#sample_rate in
            let in_length = int_of_float (ceil (float length /. ratio)) in
            let buffer = Audio.create reader#channels in_length in
            let bytes_read = reader#read buffer 0 in_length in
            if bytes_read < in_length then finished <- true;
            let tmp = Audio.resample ratio buffer 0 in_length in
            match Array.length tmp with
                  1 -> Array.make 2 tmp.(0)
                | 2 -> tmp
                | _ -> failwith "Unknown number of channels."            
    end;;

class round_stream (settings : stream_settings) =
    object (self)
        val shout = Shout.new_shout ()
        val encoder = Lame.create_encoder ()
        val mutable inputs = new ThreadList.thread_list
        val mutable active = false

        method start () =
            try
                Shout.open_shout shout;
                active <- true;
                ignore (Thread.create self#play_loop ());
                true
            with Shout.No_connect ->
                Printf.printf "Unable to connect to icecase\n";
                false

        method private play_loop () =
            while active do
                inputs#prune (fun i->not i#get_finished);
                let buffer = Audio.create
                                settings.channels
                                settings.buffer_length in
                inputs#iter (fun i->
                    let newbuff = i#get_buffer
                                    settings.buffer_length
                                    settings.sample_rate in
                    Audio.add buffer 0 newbuff 0 settings.buffer_length
                );
                let out = Lame.encode_buffer_float encoder
                                                   buffer.(0)
                                                   buffer.(1)
                                                   settings.buffer_length in
                Shout.sync shout;
                Shout.send shout out;
                let ms = Shout.delay shout in
                Printf.printf "\rDelay %d ms, files %d %!" ms (inputs#length())
            done;
            Shout.close shout;
            Shout.shutdown ()

        method stop () = active <- false

        method add (filename : string) =
            Printf.printf "Adding %s\n" filename;
            inputs#add(new wave_file_src filename)

        initializer
            Shout.init ();
            Shout.set_host shout settings.host;
            Shout.set_protocol shout Shout.Protocol_http;
            Shout.set_port shout settings.port;
            Shout.set_user shout settings.user;
            Shout.set_password shout settings.password;
            Shout.set_mount shout settings.mount;
            Shout.set_format shout Shout.Format_mp3;
            Shout.set_description shout settings.description;
            Lame.init_params encoder;
    end;;

let boodler sound_times =
    Random.init (int_of_float (Unix.time ()));
    let stream = new round_stream(round_settings) in
    let rec add_sound_timeout (filename, min_delay, max_delay) =
            let delay = min_delay + Random.int (max_delay-min_delay+1) in
            ignore (GMain.Timeout.add
                        delay
                        (fun ()->
                             (try stream#add filename;
                                 add_sound_timeout
                                     (filename, min_delay, max_delay);
                             with
                               x -> Printf.printf "%s" (Printexc.to_string x));
                             false)) in
    if stream#start() then begin
        List.iter add_sound_timeout sound_times;
        GtkThread.main ()
    end

let _ =
    boodler [
      ("/usr/share/skype/sounds/CallBusy.wav", 3000, 10000);
      ("/usr/share/skype/sounds/CallFailed.wav", 9000, 9000)
    ]

