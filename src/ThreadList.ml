module ThreadList = struct

class ['a] thread_list =
    object
        val mutable items = []
        val mutex = Mutex.create()

        method add (item : 'a) =
            Mutex.lock mutex ;
            items <- item :: items;
            Mutex.unlock mutex

        method iter (f : 'a -> unit) =
            Mutex.lock mutex;
            List.iter f items;
            Mutex.unlock mutex

        method prune (f : 'a -> bool) =
            Mutex.lock mutex;
            items <- List.filter f items;
            Mutex.unlock mutex

        method length () = List.length items
    end;;

end

