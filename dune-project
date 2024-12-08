(lang dune 3.16)

(name exercise_jsoo)

(generate_opam_files true)

(source
 (github cedretaber/exercise_jsoo))

(authors "cedretaber")

(maintainers "cedretaber")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name exercise_jsoo)
 (synopsis "An exercise for jsoo.")
 (description "An exercise for jsoo.")
 (depends ocaml dune js_of_ocaml-ppx yojson)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
