let () =
  let chapters = [
    ( "1", Chap01.run );
    ( "2", Chap02.run );
    ( "4", Chap04.run );
    ( "5", Chap05.run );
    ( "6", Chap06.run );
    ( "7", Chap07.run );
    ( "8", Chap08.run );
    ( "9", Chap09.run );
    ( "10", Chap10.run );
    ( "11", Chap11.run );
    ( "12", Chap12.run );
    ( "13", Chap13.run ) ]
  in
  if (Array.length Sys.argv) < 2 then
    Printf.printf "Please specify which chapter demo you would like to run.\n"
  else
    let chapter = List.assoc Sys.argv.(1) chapters in
    chapter ()
