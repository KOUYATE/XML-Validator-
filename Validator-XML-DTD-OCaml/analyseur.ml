open Str;;
open String;;
#load "str.cma";;

(*

  PARTIE 1
 
   Definition des operations pour le XML => type
   Un identifiant est defini par une chaine de caractere 

*)
type id = string ;;
(* 

   Une balise est associe a un identifiant et peut etre soit ouvrante ou fermante 

*)
type balise = Open of id | Close of id ;;
(* 

   Un entry est une liste d'entry ou une donnee dans deux balise ouvrante et fermante

*)
type entry = Id of balise * entry list * balise | Data of string ;;
(*

  Un document XML est une liste d entry 

*)
type documentXML = Document of entry list ;;

(*
 
  nettoyage

*)
let clean2 chaine =
  let clean1 =global_replace (regexp "<!-- .*-->") ""  chaine in
  let clean2 =global_replace (regexp "<[?].*[?]>") ""  clean1 in
  let clean3 =global_replace (regexp "[\n \t\r\"]*") "" clean2 in
  let clean4 =global_replace (regexp "><" ) "> <" clean3 in clean4 ;;


let clean chaine = 
  let clean4 =global_replace (regexp "> *[a-zA-Z0-9 ]*[a-zA-Z0-9 ]*<") "> <" (clean2 chaine) in
  clean4;;
(*

  fonction qui prend une chaine de caractere et remplace >< -> > < elle sera utile dans initialisateur balise

*)
let reforme_xml chaine = 
  let x= clean2 chaine in 
  let x = global_replace (regexp ">") "> " x in 
  let x = global_replace (regexp "<") " <" x in 
  let x = global_replace (regexp " +") " "x  in x ;;

(*

 FONCTION DE CONVERSION PARTIE1

*)

(*

  fonction qui convertie une chaine de balise ouvrante en une balise ouvrante exemple <teste> -> Open(teste) 

*)
let open_of_string str =
  let x = global_replace (regexp "[ <>]") "" str in
  Open(x);;
(*

  fonction qui convertie une chaine de balise fermante en une balise fermante exemple </teste> -> Close(teste)

 *)
let close_of_string str =
  let x = global_replace (regexp "[ /<>]") "" str in
  Close(x);;
(*
  
  fonction qui convertie une balise en chaine de caractere

*)
let string_of_balise = function
  |Open(a) -> a
  |Close(a)-> a;;

(*
  
  FONCTION DE TESTE PARTIE 1

*)

(*

  fonction qui teste si une balise est ouvrante

*)
let is_open str =
  string_match (regexp "^<[a-zA-Z]+>$") str 0;;

(*

  fonction qui teste si une balise est fermante

*)
let is_close str = 
  string_match (regexp "^</[a-zA-Z]+>$") str 0;;

(*

  fonction qui teste la validite de balise ouvrante et fermante

*)
let is_compatible op cl =
  match op,cl with 
    |Open(a),Close(b) -> a=b
    |_,_ ->false ;; 
let is_pcdata str= string_match (regexp "[a-zA-Z0-9 ]+") str 0;;
(*

  fonction qui fais la lecture d'un fichier et le renvoie sous forme de chaine de caractere

*)
let lectureFichier chemin  = 
  let canal_open = open_in(chemin) in 
  let rec aux = fun chaine canal_open ->
    try aux (chaine^(input_line canal_open)) canal_open with
	End_of_file -> chaine
      | _ -> aux (chaine^(input_line canal_open)) canal_open in aux "" canal_open;;

(*

  fonction qui formate une chaine 

*)
let formate_chaine chaine = let rec aux = fun i copie ->
  if(i>=length chaine) then split (regexp " ") copie
  else if (chaine.[i])='>' || ((length chaine)>(succ i)) && (chaine.[i+1])='<'
  then aux (succ i) (copie^(make 1 (chaine.[i]))^" ")
  else aux (succ i) (copie^(make 1 (chaine.[i]))) in aux 0 "" ;;

(* 
 
   PARTIE 2 

*)


(*
  
  fonction qui retrouve la balise fermante d'une balise ouvrant elle renvoie tous 
  qui se trouve a l'interieur et tout ce qui se trouve l'exterieur de la 
  balise sous forme de couple de (couple de liste) et la balise fermante elle meme

*)
let premiere_balise_fermante op liste =
  let rec aux interieur liste i =
    match liste with 
      |[] -> failwith "Fichier xml invalide !!"
      |h::t when (is_open h) && (open_of_string h)=op -> aux (interieur@[h]) t (i+1)
      |h::t -> 
	if is_close h && is_compatible op (close_of_string h) && i=0 then (interieur,t),close_of_string h  
	else if is_close h && is_compatible op (close_of_string h) && i>0 then aux (interieur@[h]) t (i-1)
	else aux (interieur@[h]) t i in 
  aux [] liste 0;; 

(*

  Initialisation de la structure xml a partir d'un fichier

*)
let rec initialisateurXML nomFichier  = 
  let liste_chaine = (split (regexp " ") (reforme_xml (lectureFichier nomFichier))) in
  Document(
    let rec aux = function liste_chaine -> 
      match liste_chaine with
	|[] -> []
	|h::t ->if is_pcdata h then [Data h]@(aux t) else
	    let donnee = (premiere_balise_fermante (open_of_string h) t) in 
	    let interieur = fst(fst donnee) in  
	    let exterieur = snd(fst donnee) in 
	    let fermeture_h = snd donnee in 
	    [Id(open_of_string h,(aux interieur),fermeture_h)]@(aux exterieur) 
    in
    aux liste_chaine );; 


(*
  Definition operation pour le DTD => type
  Un atom est defini par un identifiant ou un element 

*)
type atom = I of id | Elements of elements
    
(*
  
  Un element est defini par une occurende ou une liste d occurrence

*)
and elements = Occur of occurence 
	       | ListOccur of occurence list 
	       | DansOccur of occurence list
		   
(*

  Une occurrence est definie par un atom ou un atome etoile , 
  ou un atome plus ou un atom interro

*)
and occurence = Atom of atom 
		| AtomEtoile of atom 
		| AtomPlus of atom 
		| AtomIntero of atom ;;

(*

  Un model est soit Vide soit une donnee c est a dire une chaine ou un element

*)
type model = EMPTY 
	     | Donnee 
	     | Model of elements ;;

(*
  
  Une description est un id suivi de sont model

*)
type description = Desc of id * model ;;

(*

  Un document est une liste de description

*)
type documentDTD = DocumentDTD of description list ;; 

(*
  
  NETTOYAGE PARTIE 2

*)

let clean3 chaine =global_replace (regexp "[\t]") " " chaine;;



(*conversion PARTIE 2*)

(*

  fonction qui convertie un atom en id 

*)
let id_of_atom atom = match atom with
    I x -> x
  | _ -> failwith "Conversion non possible" ;;

(*

  fonction qui convertie une chaine en id 

*)
let id_of_string chaine = ((global_replace (regexp "[^a-zA-Z]") "" chaine):id) ;;

(*

  fonction qui convertie une chaine de caractère en Atom

*)
let atomI_of_string chaine = I ((global_replace (regexp "[^a-zA-Z]") "" chaine):id) ;;

(*
   
   fonction qui convertie une chaine de caractère en Occurrence
  
*)
let occur_of_string chaine = match chaine with
    x when string_match (regexp "[a-zA-Z]+[\\*]+$") x 0 -> AtomEtoile ( atomI_of_string x)
  | x when string_match (regexp "[a-zA-Z]+[\\+]+$") x 0 -> AtomPlus (atomI_of_string x)
  | x when string_match (regexp "[a-zA-Z]+[\\?]+$") x 0 -> AtomIntero (atomI_of_string x)
  | x -> Atom (atomI_of_string x);;

(*

   fonction qui convertie une occurrence en elements
   
*)
let elements_of_occurrence occur = Occur occur ;;

(*
   
   fonction qui convertie une chaine de caractère en elements (ListOccur)
 
*)
let listOccur_of_string chaine = 
  let tmp = split (regexp ",") (global_replace (regexp "[(<)> ]+") "" chaine) in 
  let rec aux = fun l liste ->
    if(liste=[]) then ListOccur l
    else aux (l@[occur_of_string (List.hd liste)]) (List.tl liste) in aux [] tmp ;;

(*

  fonction qui convertie une chaine de caractère en elements (DansOccur)
*)
let dansOccur_of_string chaine =
  let tmp = split (regexp "|") (global_replace (regexp "[(<)> ]+") "" chaine) in 
  let rec aux = fun l liste ->
    if(liste=[]) then DansOccur l
    else aux (l@[occur_of_string (List.hd liste)]) (List.tl liste) in aux [] tmp ;;

(*

   fonction qui convertie une chaine de caractère en model
 
*)
let model_of_string chaine = match chaine with
    x when string_match (regexp_case_fold "( *#PCDATA *)") chaine 0 -> Donnee
  | x when string_match (regexp_case_fold "( *EMPTY *)") chaine 0  -> EMPTY
  | x when string_match (regexp "([a-zA-Z \\+\\*\\?]+)") chaine 0 -> 
    Model (Occur (occur_of_string (global_replace (regexp "[(<)> ]+") "" chaine)))
  | x when string_match (regexp "([a-zA-Z ,\\+\\?\\*]+)") chaine 0 -> 
    Model ( listOccur_of_string chaine)
  | _ -> Model ( dansOccur_of_string chaine ) ;;

(*

   fonction qui convertie une chaine de caractère en description

*)
let description_of_string id model = Desc (id_of_string id, model_of_string model);;

let reforme_chaine chaine = split (regexp " ") 
  (global_replace (regexp "[\t\n\r ]+") " " (global_replace (regexp "><") "> <" (let rec aux = fun i bool ->
    if(i>=length chaine) then ""
    else if (i+1)<(length chaine) && chaine.[i+1]='(' then (make 1 chaine.[i])^" "^(aux (succ i) true)
    else if chaine.[i]='>' then (make 1 chaine.[i])^(aux (succ i) false)
    else if bool=true && chaine.[i]=' ' then aux (succ i) bool 
    else (make 1 chaine.[i])^(aux (succ i) bool) in aux 0 false))) ;;



(*
  
  Initialisation la structure DTD a partir d'un fichier
 
*)
let initialisateurDTD nomFichier = 
  let chaine = (lectureFichier nomFichier) in 
  let liste = reforme_chaine (chaine) in 
  if liste=[] then failwith "Le document DTD est vide !!!"
  else try
	 let rec aux = fun l liste ->
	   if liste=[] then DocumentDTD l
	   else aux (l@[description_of_string (List.nth liste 1)(List.nth liste 2)])
	     (List.tl (List.tl (List.tl liste))) in aux [] liste 
    with Failure c -> failwith "Document DTD non valide !!!";;

(*

   fonction qui renvoie le nombre d'occurrence d'un id
  
*)
let rec nombre_occurrence  = function id -> function  
  |[] -> 0
  |h::t -> match h with
      |Id(Open a,_,_) when a=id -> 1+( nombre_occurrence id t)
      |_-> nombre_occurrence id t;;

(*
  
   fonction qui teste une occurence par rapport à une liste d'occurence 
  
*)
let teste_occurrence = function occurence -> function entryliste ->
  match occurence with 
    |(Atom (I id))-> (nombre_occurrence id entryliste)=1
    |(AtomEtoile (I id))-> (nombre_occurrence id entryliste)>=0
    |(AtomPlus (I id)) -> (nombre_occurrence id entryliste) > 0
    |(AtomIntero (I id))-> (nombre_occurrence id entryliste)=1 or (nombre_occurrence id entryliste)=0
    |_-> true;;

(*

   convertie un document xml en une liste d'entry

*)
let entrylist_of_document = function
  |Document(a) -> a;;

(*

  convertie un document dtd en une liste de description

*)
let desclist_of_document = function 
  |DocumentDTD(a) -> a;;

(*
  
  convertie une liste d'entry en chaine de caractère

*)
let rec string_of_list = function 
  |[] -> ""
  |h::t -> h^" ; "^(string_of_list t);;
 
(*
  
  renvoie la liste des noms de balise qui se trouve a l interieur d'une balise 
  
*)
let rec list_xml = function 
  |[]-> []
  |h::t -> 
    match h with 
      |Id(Open(a),_,_) -> [a]@(list_xml t)
      |_-> list_xml t;;


(*
  
  fonction qui prend une list description et renvoie la liste des balise autoriser dans le dtd
 
*)
let rec liste_balise = function  
  |[]-> []
  |h::t -> match h with |Desc(a,b)-> [a]@(liste_balise t) 
    
(*
  
  fonction qui prend un open et une liste de chaine de caractere et teste s'il figure dans la liste
 
*)
let rec in_liste_balise = function op -> function 
  |[] -> false
  |h::t -> match op with |Open id when id = h -> true |_-> in_liste_balise op t ;;
(*
 
  fonction qui prend un entry et un liste de description et renvoi le paramètre correspondant

*)
let parametreDTD entri dtd =
  match entri with 
    |Id(Open a,b,c) ->
      let tete = a in 
      let rec retrouve_parametre = function str -> function
	|[] -> failwith "Fichier XML invalide \nLe fichier XML est non conforme au document DTD !"
	|h::t when (match h with Desc(x,y) -> x=str)=true -> 
	  (match h with 
	    |Desc(o1,o2)-> o2 )
	|h::t -> retrouve_parametre str t  in retrouve_parametre tete dtd
    |_ -> failwith "inatendue ";; 

(*
  
 renvoie la liste des noms de balise autoriser dans le model passer en paramètre

*)

let rec baliseparam = function model -> 
  match model with 
    |Donnee -> []
    |EMPTY -> []
    |Model(element) ->match element with
	|Occur(Atom(I a)) -> [a]
	|Occur(AtomPlus(I a)) -> [a]
	|Occur(AtomIntero(I a)) -> [a]
	|Occur(AtomEtoile(I a)) -> [a]
	|ListOccur([]) -> []
	|ListOccur(h::t) -> (baliseparam (Model(Occur h))) @ (baliseparam (Model(ListOccur t)))
	|DansOccur([]) -> []
	|DansOccur(h::t) -> (baliseparam (Model(Occur h))) @ (baliseparam (Model(ListOccur t)))
	|_-> [];;

(*

  teste si toutes les balises sont a l'interieur du paramètre dtd 
  elle sera utile dans Validator Xml
  
*)	
let rec in_param = function listeparam -> function idliste -> 
  let rec aux = function str -> function 
    |[] -> false
    |h::t -> if str = h then true else aux str t in 
  match idliste with |[] -> true |h::t -> if not (aux h listeparam) then failwith h else  (in_param listeparam t);;
(*
  
  teste occurrence d'une liste d'occurence
  
*)

let rec calcule_loc = function f1 -> function f2 -> function entrylist -> function 
  |[] -> (f1 true false)
  |h::[]-> (teste_occurrence h entrylist)
  |h::t -> 
    match h with 
      |Atom(Elements (ListOccur oc)) -> f1 (calcule_loc f1 f2 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomEtoile(Elements (ListOccur oc)) -> f1 (calcule_loc f1 f2 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomPlus(Elements (ListOccur oc)) -> f1 (calcule_loc f1 f2 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomIntero(Elements (ListOccur oc)) -> f1 (calcule_loc f1 f2 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |Atom(Elements (DansOccur oc)) -> f1 (calcule_loc f2 f1 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomEtoile(Elements (DansOccur oc)) -> f1 (calcule_loc f2 f1 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomPlus(Elements (DansOccur oc)) -> f1 (calcule_loc f2 f1 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |AtomIntero(Elements (DansOccur oc)) -> f1 (calcule_loc f2 f1 entrylist oc ) (calcule_loc f1 f2 entrylist t)
      |_-> f1 (teste_occurrence h entrylist) (calcule_loc f1 f2 entrylist t);;
let ou = (fun x y -> x or y);;
let et = (fun x y -> x && y);;

(*
 
  TESTE DE VALIDITE 

*)


exception ValidatorException of string;;

let rec validator = function xml -> function dtd -> 
  let entrylist=(entrylist_of_document xml) in 
  let desclist =(desclist_of_document dtd) in
  let liste_balise = liste_balise desclist in
  let rec aux = function entrylist ->
    match entrylist with 
      |[] -> true
      |Id(x,[Data(s)],y)::[] -> 
	if in_liste_balise x liste_balise then (match (parametreDTD (Id(x,[Data(s)],y)) desclist) with |Donnee -> true |_->false ) 
	else false
      |tetexml::restexml -> 
	let nom_de_la_balise =(match tetexml with |Id(Open(a),_,_ )-> a | _-> failwith "") in
	let interieur_de_la_tete = (match tetexml with |Id(a,b,c) -> b
	  | Data(a) -> failwith ("Fichier XML invalide !!\nLa balise <"^nom_de_la_balise^"> ne doit contenir que des balises.")) in
	let nouveau_xml = (Document interieur_de_la_tete) in
	let parametre_de_la_tete = parametreDTD tetexml desclist in 
      (match parametre_de_la_tete with
	|EMPTY -> (match interieur_de_la_tete with |[] -> (aux restexml) 
	    |_-> raise (ValidatorException ("Fichier XML invalide !!\nLa balise <"^nom_de_la_balise^"> doit être vide.")))
	|Donnee-> (match nouveau_xml with |Document(a) -> (match a with  |[Data(s)] -> (aux restexml) | _-> false ))
	|Model(element) -> let balise_v = try (in_param (baliseparam (Model element)) (list_xml  interieur_de_la_tete)) with |Failure c->
			  raise  (ValidatorException ("Fichier XML invalide !!\nLa balise <"^c^("> n'est pas definie dans les règles de la balise <")^(nom_de_la_balise)^"> dans le document DTD."))
	  |_-> false in
	  if not balise_v then 
	    raise ( ValidatorException ("Fichier XML invalide !!\nIl existe une balise intruit dans "^(nom_de_la_balise)^".")) else 
	  (match element with
	    | Occur(oc) ->(teste_occurrence oc interieur_de_la_tete) && (aux restexml) && (validator nouveau_xml dtd)
	    | ListOccur(listoc) -> 
	      if (calcule_loc  et ou interieur_de_la_tete listoc) then (aux restexml) && (validator nouveau_xml dtd) else false
	    | DansOccur(listoc) -> 
	      if (calcule_loc ou et interieur_de_la_tete listoc) then  (aux restexml) && (validator nouveau_xml dtd) else false
	  )
      )
  in aux entrylist;;

let main () = 
  print_string "\nVALIDATEUR DE DOCUMENT (XML,DTD)\n";
  print_newline();
  print_string "Veuillez saisir le chemin du fichier XML : ";
  let xml = read_line() in 
  print_string "Veuillez saisir le chemin du fichier DTD : ";
  let dtd = read_line() in
  try 
    let nouveauXML = (initialisateurXML (global_replace (regexp ";*") "" xml)) in
    let nouveauDTD = (initialisateurDTD (global_replace (regexp ";*") "" dtd)) in 
    if (List.length (entrylist_of_document nouveauXML)) = 1 then 
      if (validator nouveauXML nouveauDTD) then
	print_string "\nFichier XML Valide !!\n\n"
      else 
	print_string "\nFichier XML invalide !!!\n\n"
    else
      print_string "\nInvalide un fichier XML ne doit posseder qu'une seule balise racine\n\n"
  with
      ValidatorException c -> print_string("\n"^c^"\n\n")
    |	Failure c -> print_string("\n"^c^"\n\n")
    | Sys_error c -> print_string("\n"^c^"\n\n")
    |_-> print_string "\nerreur\n\n";;   
