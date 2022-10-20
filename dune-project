(lang dune 3.4)

(name vdd)
(generate_opam_files true)

(package
 (name vdd)
 (allow_empty)
 (synopsis "A GTK based application for using dd")
 (description "A GTK based application for using dd, with a focus on writing ISO images")
 (depends
   ocaml
   dune
   lablgtk3))
