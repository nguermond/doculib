
open Cohttp
open Cohttp_lwt_unix

exception UnexpectedDocumentType of string
                                  
let get_database_name : string -> string = function
  | "book" -> "openlibrary.org"
  | "article" -> "semanticscholar.org"
  | s -> raise (UnexpectedDocumentType s)

(* Open library *)
let query_book_string (search_str : string) : string Lwt.t =
  let url = "http://openlibrary.org/search.json?q=" in
  let fields = "&fields=title,author_name,first_publish_year,isbn" in
  let uri = (Uri.of_string (url ^ search_str ^ fields)) in
  prerr_endline ("Searching "^(url ^ search_str ^ fields));
  let open Lwt in
  Client.get uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  (prerr_endline "Successful search...");
  body

(* Semantic scholar *)
let query_article_string (search_str : string) : string Lwt.t =
  let url = "https://api.semanticscholar.org/graph/v1/paper/" in
  let query = "search?query=" in
  let fields = "&fields=authors,title,year,externalIds" in
  let uri = (Uri.of_string (url ^ query ^ search_str ^ fields)) in
  prerr_endline ("Searching "^(url ^ query ^ search_str ^ fields));
  let open Lwt in
  Client.get uri >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  (prerr_endline "Successful search...");
  body

let parse_article (doc_type : string) (article : Json.t) : Db.doc =
  let title = (Json.to_string (Json.default (`String "") (Json.get "title" article))) in
  let authors = (List.map (fun author ->
                     (Json.raise_opt "No name" (Json.get "name" author))
                     |> Json.to_string)
                   (Json.default (`List []) (Json.get "authors" article)
                    |> Json.to_list )) in
  let year_i = (Json.to_int (Json.default (`Int 0) (Json.get "year" article))) in
  let year = (if year_i = 0 then "" else (string_of_int year_i)) in
  let doi = Json.default `Null (Json.get "externalIds" article)
            |> (fun x -> Json.default (`String "") (Json.get "DOI" x))
            |> Json.to_string in
  let doc : Db.doc = {star = false;
                      title = title;
                      authors = authors;
                      year = year;
                      doi = doi;
                      isbn = "";
                      tags = [];
                      path = "";
                      doc_type = doc_type;
                     }
  in doc

   
let parse_book (doc_type : string) (book : Json.t) : Db.doc =
  let title = (Json.to_string (Json.default (`String "") (Json.get "title" book))) in
  let authors = (List.map Json.to_string
                   (Json.default (`List []) (Json.get "author_name" book)
                    |> Json.to_list)) in
  let year_i = (Json.to_int (Json.default (`Int 0) (Json.get "first_publish_year" book))) in
  let year = (if year_i = 0 then "" else (string_of_int year_i)) in
  let isbns = (Json.to_list (Json.default (`List []) (Json.get "isbn" book))) in
  let isbn = (Json.to_string (Json.default (`String "") (List.nth_opt isbns 0))) in
  let book_str = Json.pretty_to_string book in
  prerr_endline ("Output:\n" ^ book_str);
  let doc : Db.doc = {star = false;
                      title = title;
                      authors = authors;
                      year = year;
                      doi = "";
                      isbn = isbn;
                      tags = [];
                      path = "";
                      doc_type = doc_type;
                     }
  in doc

  
let search_article (doc_type : string) (search_str : string) : Db.doc list =
  let body = Lwt_main.run (query_article_string search_str) in
  let json = Json.from_string body in
  let data = try Json.to_list (Json.raise_opt "Unexpected result" (Json.get "data" json)) with
             | Json.ParsingFailure err -> let json_pp = Json.pretty_to_string json in
                                     (prerr_endline err);
                                     (prerr_endline ("Failed to parse JSON:\n"^json_pp));
                                     raise (Json.ParsingFailure "Parse failure")
  in (List.map (parse_article doc_type) data)                 


let search_book (doc_type : string) (search_str : string) : Db.doc list =
  let body = Lwt_main.run (query_book_string search_str) in
  let json = Json.from_string body in
  let data = try Json.to_list (Json.raise_opt "Unexpected result" (Json.get "docs" json)) with
             | Json.ParsingFailure err -> let json_pp = Json.pretty_to_string json in
                                     (prerr_endline err);
                                     (prerr_endline ("Failed to parse JSON:\n"^json_pp));
                                     raise (Json.ParsingFailure "Parse failure")
  in (List.map (parse_book doc_type) data)                 


let search_document (doc_type : string) (search_type : string) (search_str : string) : Db.doc list =
  match search_type with
  | "article" -> search_article doc_type search_str
  | "book" -> search_book doc_type search_str
  | _ -> prerr_endline ("Search for document type `"^doc_type^"` not supported!"); []
  
  