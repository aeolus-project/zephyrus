
open Ocamlbuild_plugin;;

Options.use_ocamlfind := true ;;

let _ = dispatch begin function
   | After_rules ->

       (* Disable Warning 24: bad source file name *)
       flag ["ocaml"; "compile"] & S[A"-w"; A"-24"];

       (* optimization to ocaml code *)
       flag ["ocaml"; "compile"] & S[A"-ccopt"; A"-O9"];

       flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
       flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);

       rule "atdgen: .atd -> _t.ml*, _j.ml*"
        ~prods:["%_t.ml";"%_t.mli";"%_j.ml";"%_j.mli";"%_v.ml";"%_v.mli";]
        ~dep:"%.atd"
        (begin fun env build ->
        let atdgen = "atdgen" in
        Seq [
        Cmd (S [A atdgen; A "-t"; P (env "%.atd")]);
        Cmd (S [A atdgen; A "-j"; A "-j-std"; P (env "%.atd")]);
        Cmd (S [A atdgen; A "-v"; P (env "%.atd")]);
        ]
        end)

   | _ -> ()
end
