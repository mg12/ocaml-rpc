
let hex_of x =
 let hexa = "0123456789abcdef" in
 let r = ref "" in
 let of_char c =
   let x = Char.code c in
   r := !r ^ (Printf.sprintf "%c%c:" hexa.[x lsr 4] hexa.[x land 0xf]);
   ()
 in
 String.iter of_char x;
 !r
in

let test x = 

  Printf.printf "Rpc.to_string='%s'\n" (Rpc.to_string x);

  let sent_rpc = x in
  Printf.printf "sent_rpc_data: '%s' -->" (Rpc.to_string sent_rpc);

  let bson_data = Bsonrpc.to_string sent_rpc in
  Printf.printf "(len=%d:='%s')-->" (String.length bson_data) "" (*(hex_of bson_data)*); 

  let received_rpc = Bsonrpc.of_string bson_data in
  Printf.printf "received_rpc_data: rpc='%s'" (Rpc.to_string received_rpc);

  if sent_rpc = received_rpc
  then Printf.printf " --> passed \n"
  else (Printf.printf " --> FAILED: DONT MATCH\n"; failwith "")
in

List.iter (fun x->test x) 
[
  Rpc.Null;
  Rpc.Int 1000L;
  Rpc.String "HELLO";
  Rpc.Bool true;
  Rpc.Enum [Rpc.String "abc"; Rpc.String "def"; Rpc.Int 5L; Rpc.Bool false];
  Rpc.Dict [ ("field1", Rpc.String "xyz"); ("field2", Rpc.String "rst"); ("field3", Rpc.String "tuv"); ];  
];


let test_file path = 
  
  let string_of_file path =
    let ch = open_in path in
    let len = in_channel_length ch in
    let contents = really_input_string ch len in
    close_in ch;
    contents
  in
  
  let xapi_json_pci = string_of_file path in
  Printf.printf "\n\nxapi_json_pci_request: length=%d\n" (String.length xapi_json_pci);
  
  let rpc = Jsonrpc.of_string xapi_json_pci in
  let str_of_rpc = Rpc.to_string rpc in
  Printf.printf "xapi_json_pci_request->Rpc.to_string: length=%d\n" (String.length str_of_rpc) (*str_of_rpc*);
  
  let json_rpc_of_rpc = Jsonrpc.to_string rpc in
  Printf.printf "json_rpc_of_rpc: length=%d\n" (String.length json_rpc_of_rpc);
  
  let bson_rpc_of_rpc = Bsonrpc.to_string rpc in
  Printf.printf "bson_rpc_of_rpc: length=%d\n" (String.length bson_rpc_of_rpc);
  
  let xml_rpc_of_rpc = Xmlrpc.to_string rpc in
  Printf.printf "xml_rpc_of_rpc: length=%d\n" (String.length xml_rpc_of_rpc);
  
  let finally fct clean_f =
  	let result =
  		try
  			fct ();
  		with exn ->
  			(*Backtrace.is_important exn;*)
  			clean_f ();
  			raise exn in
  	clean_f ();
  	result
  in
  
  let time_this (name: string) loop f =
    let result = ref 0.0 in
    let start_time = Unix.gettimeofday () in
    let _ = finally (fun ()-> for i = 1 to loop do f () done; !result)
      (fun () ->
         try
           let end_time = Unix.gettimeofday () in
           let diff = (end_time -. start_time) in
           Printf.printf "timed %s: loop %d = %f seconds\n" name loop diff;
           result := diff
         with e ->
           failwith (Printf.sprintf "Ignoring exception %s while timing: %s" (Printexc.to_string e) name)
      ) in
    !result
  in
  
  let json_of_rpc = time_this "json_rpc_of_rpc" 1000 (fun ()->Jsonrpc.to_string rpc) in
  let bson_of_rpc = time_this "bson_rpc_of_rpc" 1000 (fun ()->Bsonrpc.to_string rpc) in
  let xml_of_rpc  = time_this "xml_rpc_of_rpc" 1000 (fun ()->Xmlrpc.to_string rpc) in
  
  let rpc_of_json = time_this "rpc_of_json_rpc" 1000 (fun ()->Jsonrpc.of_string json_rpc_of_rpc) in
  let rpc_of_bson = time_this "rpc_of_bson_rpc" 1000 (fun ()->Bsonrpc.of_string bson_rpc_of_rpc) in
  let rpc_of_xml  = time_this "rpc_of_xml_rpc" 1000 (fun ()->Xmlrpc.of_string xml_rpc_of_rpc) in
  
  Printf.printf "total rpc->json->rpc = %f seconds\n" (json_of_rpc +. rpc_of_json);
  Printf.printf "total rpc->bson->rpc = %f seconds\n" (bson_of_rpc +. rpc_of_bson);
  Printf.printf "total rpc->xml->rpc = %f seconds\n" (xml_of_rpc +. rpc_of_xml);
  
in

if Array.length Sys.argv > 1
  then test_file (Sys.argv.(1))
  else (
     Printf.printf "usage: <binary> <test_file.json>\n";
     Printf.printf "defaulting to ./test1.json ...";
     test_file "./test1.json"
  )

  


