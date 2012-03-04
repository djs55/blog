(* Build the blog from bootstrap + markdown *)

open Stringext
open Pervasiveext

let ( |> ) f g = g f

let output_dir = ref ""
let input_dir = ref (Some "content/posts")
let header = ref (Some "templates/header.html")
let footer = ref (Some "templates/footer.html")

type ty = Html | Markdown

type post = {
  y: string;
  m: string;
  d: string;
  title: string;
  path: string;
  filename: string;
  ty: ty
}

let href_of_post p = Printf.sprintf "%s.html" p.filename

let month_of_post p = match int_of_string p.m with
  | 1 -> "Jan" | 2 -> "Feb" | 3 -> "Mar" | 4 -> "Apr" | 5 -> "May" | 6 -> "Jun"
  | 7 -> "Jul" | 8 -> "Aug" | 9 -> "Sep" | 10 -> "Oct" | 11 -> "Nov" | 12 -> "Dec"
  | x -> failwith (Printf.sprintf "Unknown month: %s (%s)" p.m p.filename)

let list_posts () = match !input_dir with
  | None ->
    failwith (Printf.sprintf "No -input-dir provided\n")
  | Some dir ->
    let all =
      List.fold_left
	(fun acc filename ->
	  let path = Filename.concat dir filename in
	  try
	    begin
	      match String.split ~limit:2 '.' filename with
		| [ filename; format ] ->
		  let ty = match format with
		    | "mdwn" -> Markdown
		    | "html" -> Html
		    | x -> failwith (Printf.sprintf "Unknown file extension: %s" filename) in
		  let y, m, d, title = match String.split ~limit:4 '-' filename with
		    | [ yyyy; mm; dd; title ] ->
		    yyyy, mm, dd, title
		    | _ -> failwith (Printf.sprintf "Failed to parse date from: %s" filename) in
		  let post = { y = y; m = m; d = d; title = title; path = path; filename = filename; ty = ty } in
		  post :: acc
		| _ -> failwith (Printf.sprintf "Failed to parse filename: %s" filename)
	    end
	  with
	    | e -> Printf.fprintf stderr "Skipping %s (%s)\n" filename (Printexc.to_string e);
	      acc
	) [] (Sys.readdir dir |> Array.to_list) in
    (* Sort into descending order on date *)
    List.stable_sort
      (fun a b ->
	compare [ b.y; b.m; b.d ] [ a.y; a.m; a.d ]
      ) all

let with_file filename f =
  let oc = open_out filename in
  finally (fun () -> f oc) (fun () -> close_out oc)

let print_file_to oc =
  let output_line oc txt = output_string oc txt; output_string oc "\n" in
  Unixext.file_lines_iter (output_line oc)

let generate oc all_posts this_post =
  Opt.iter (print_file_to oc) !header;

  (* Sidebar *)
  output_string oc
"    <div class=\"container-fluid\">
      <div class=\"row-fluid\">
        <div class=\"span3\">
          <div class=\"well sidebar-nav\">
            <ul class=\"nav nav-list\">
";
  let last_post = ref None in
  List.iter
    (fun post ->
      let this_date = post.y, post.m in
      let last_date = Opt.map (fun x -> x.y, x.m) !last_post in
      if Some this_date <> last_date
      then output_string oc (Printf.sprintf
"              <li class=\"nav-header\">%s %s</li>
" (month_of_post post) post.y);
      output_string oc (Printf.sprintf
"              <li%s><a href=\"%s\">%s</a></li>
" (if post = this_post then " class=\"active\"" else "") (href_of_post post) post.title);
      last_post := Some post;
    ) all_posts;
  output_string oc
"            </ul>
          </div><!--/.well -->
        </div><!--/span-->
";
  (* Main section *)
  output_string oc
"        <div class=\"span9\">
";
  begin match this_post.ty with
    | Html ->
      print_file_to oc this_post.path
    | Markdown ->
      let tmp = Filename.temp_file "blog." ".build" in
      if Sys.command (Printf.sprintf "/usr/bin/markdown %s > %s" this_post.path tmp) <> 0
      then failwith (Printf.sprintf "/usr/bin/markdown failed");
      print_file_to oc tmp;
      Sys.remove tmp
  end;
  output_string oc
"        </div><!--/span-->
      </div><!--/row-->

      <hr>

      <footer>
        <p>&copy; David Scott 2012</p>
      </footer>

    </div><!--/.fluid-container-->
";   
  Opt.iter (print_file_to oc) !footer

let _ =
  Arg.parse [
    "-output-dir", Arg.Set_string output_dir, "directory where generated files will be placed";
    "-input-dir", Arg.String (fun x -> input_dir := Some x), "directory where input files are found";
    "-header", Arg.String (fun x -> header := Some x), "filename of per-page header";
    "-footer", Arg.String (fun x -> footer := Some x), "filename of per-page footer";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Generate blog from html and markdown input files";

  if not(Sys.is_directory !output_dir)
  then failwith (Printf.sprintf "-output-dir (%s) doesn't exist" !output_dir);

  let posts = list_posts () in
  List.iter
    (fun post ->
     with_file (Printf.sprintf "%s/%s.html" !output_dir post.filename)
       (fun oc -> generate oc posts post)
    ) posts;
  match posts with
    | latest :: _ ->
      with_file (Printf.sprintf "%s/index.html" !output_dir)
	(fun oc -> print_file_to oc (Printf.sprintf "%s/%s.html" !output_dir latest.filename))
    | _ -> () (* No posts *)



