open Core

(*TODO: consider deriving from yojson and that might be able to solve this for me*)
type company = Qdev | Quad [@@deriving show, sexp]

let company_of_string s = s |> String.sexp_of_t |> company_of_sexp

module HcmInfo = struct
  type t = { id : int; company : company } [@@deriving show, sexp]

  let t_of_json (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let id = json |> member "Id" |> to_int in
    let company = json |> member "Company" |> to_string |> company_of_string in

    { id; company }
end

type rights = RStandard | RAdmin [@@deriving show, sexp]

module User = struct
  type t = {
    id : string;
    rights : rights;
    has_permanent_parking : bool;
    hcm_info : HcmInfo.t list;
  }
  [@@deriving show, sexp]

  let t_of_json (json : Yojson.Basic.t) =
    let open Yojson.Basic.Util in
    let id = json |> member "Id" |> to_string in
    let rights =
      json |> member "Rights" |> to_int |> fun x ->
      match x with
      | 0 -> RStandard
      | 1 -> RAdmin
      | _ ->
          failwith (sprintf "Unsupported rights value: %d (allowed - [0, 1])" x)
    in
    let has_permanent_parking = json |> member "has_parking" |> to_bool in
    let hcm_info =
      json |> member "HcmInfo" |> to_list |> List.map ~f:HcmInfo.t_of_json
    in
    { id; rights; has_permanent_parking; hcm_info }
end

module Users = struct
  type t = {
    (*NOTE: these 2 are equivalent but the second one is used because otherwise*)
    (*we can't derive sexp for the first declaration*)
    (*users : (string, User.t, String.comparator_witness) Map.t;*)
    users : User.t Map.M(String).t;
    usersFilename : string;
  }
  [@@deriving sexp]

  let t_of_config filename =
    let json = Yojson.Basic.from_file filename in
    let open Yojson.Basic.Util in
    let entries = json |> to_assoc in
    let users = List.Assoc.map ~f:(fun v -> User.t_of_json v) entries in
    let users = Map.of_alist_exn (module String) users in
    { users; usersFilename = filename }
end

let%expect_test "parse users.json" =
  let users =
    Users.t_of_config
      "/home/angel/Documents/ocaml_examples/spaces_lot/users.json"
  in

  Users.sexp_of_t users |> Sexp.to_string |> printf "%s";

  (* TODO: use ATD for this instead of manually parsing out the json
      https://dev.realworldocaml.org/json.html#scrollNav-6
   *)
  [%expect
    {| ((users((aleksandras.sevcik((id U03NW7LJBCG)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1013)(company Quad))))))(alina.norvaise((id U03JJ153GCR)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 41)(company Qdev))))))(anastasia.kovalchuk((id U045VFB5E9J)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1031)(company Quad))))))(andrius.vaitkunas((id U03EXKV281M)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 50)(company Qdev))((id 1025)(company Quad))))))(angel.iliev((id U03FBL3R6KU)(rights RAdmin)(has_permanent_parking true)(hcm_info(((id 34)(company Qdev))))))(anton.tiunin((id U03MEJJ33QS)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1003)(company Quad))))))(anupras.laureckis((id U03LPJ6HB9P)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 182)(company Qdev))))))(augustas.sereika((id U03FL491Y4S)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 117)(company Qdev))))))(auguste.navickaite((id U041R29BGKS)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 187)(company Qdev))))))(aurimas.razmis((id U03FEANSULC)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 47)(company Qdev))))))(daniel.klioc((id U03F8U4B4LV)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 52)(company Qdev))))))(darius.taraila((id U087RQX7DUZ)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 254)(company Qdev))))))(dmitrij.vlasov((id U03F8NLPR1T)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 33)(company Qdev))))))(dmitrij.zylkov((id U03FEAQ3N3W)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 61)(company Qdev))))))(dovile.ramanauskaite((id U07160L90D7)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 190)(company Qdev))))))(egle.savickiene((id U07SR2RPYCS)(rights RAdmin)(has_permanent_parking false)(hcm_info(((id 1160)(company Quad))))))(einaras.daujotas((id U03FPUQNF0R)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 13)(company Quad))))))(ernestas.luza((id U04LFCA0V70)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1049)(company Quad))))))(evelina.bernataviciute((id U03ENHGR7U4)(rights RAdmin)(has_permanent_parking true)(hcm_info(((id 123)(company Quad))))))(gabriel.vergari((id U08EG8FV9NJ)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1173)(company Quad))))))(gediminas.kasputis((id U03FHMWFJQP)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 148)(company Quad))))))(giedre.kinciute((id U088BQMREJW)(rights RAdmin)(has_permanent_parking false)(hcm_info(((id 1165)(company Quad))))))(giedre.slaiciunaite((id U07BE3J9T6E)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1116)(company Quad))))))(giedrius.junda((id U078D6V5XKM)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 231)(company Qdev))))))(gintaras.vaira((id U03FBUSCHK8)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 4)(company Quad))))))(gytis.gaubys((id U05SE88RXT2)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1078)(company Quad))))))(henreta.mikalaviciene((id U06SBPV73UJ)(rights RAdmin)(has_permanent_parking false)(hcm_info(((id 1100)(company Quad))))))(ieva.raude((id U07S251CJE7)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1159)(company Quad))))))(ieva.sukyte((id U03PNFFGMGE)(rights RAdmin)(has_permanent_parking true)(hcm_info(((id 1014)(company Quad))))))(ignas.ragaisis((id U03FUQEADB2)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 9)(company Quad))))))(ilya.kustov((id U042GHQA51V)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 189)(company Qdev))))))(ingrida.braknyte((id U05U3HLH3H7)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1080)(company Quad))))))(ingrida.navickiene((id U03F5HR66H4)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 188)(company Quad))))))(jevgenijus.popovas((id U03F19CF4BH)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 22)(company Quad))))))(jolanta.voitenkiene((id U06Q274JUKW)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 216)(company Qdev))))))(jonas.zilionis((id U04MS1M7CQG)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1043)(company Quad))))))(julius.tumas((id U049RC3PY1Y)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1036)(company Quad))))))(jurga.katulyte((id U08EF8G474Y)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1168)(company Quad))))))(jurgita.novosiolova((id U06MF50UFFD)(rights RAdmin)(has_permanent_parking false)(hcm_info(((id 1098)(company Quad))))))(justina.adamkaviciene((id U03FB4N49NE)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 134)(company Quad))))))(justinas.puzas((id U062ACLLRJP)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 215)(company Qdev))))))(kamile.melynyte((id U07MC43AG86)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1148)(company Quad))))))(karen.kazarian((id U0585D2RQV8)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 199)(company Qdev))))))(kiryl.shmarlouski((id U03KP2U6J4V)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 140)(company Qdev))))))(laima.strigo((id U03F55117EJ)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 142)(company Qdev))))))(lina.vasiliauskiene((id U03F5998DDL)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 32)(company Qdev))))))(linas.ramanauskas((id U03F8EGKV0D)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 5)(company Quad))))))(lukas.elenbergas((id U044FFNS10U)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1028)(company Quad))))))(mantas.kuncevicius((id U03F87PP1QD)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 99)(company Quad))))))(mantas.milieska((id U03FC4KLLUA)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 160)(company Qdev))))))(mantas.paulinas((id U087D5F2KGS)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 250)(company Qdev))))))(mantvydas.ganciauskas((id U04QQT135MF)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1054)(company Quad))))))(mantvydas.kalibatas((id U0759MQJD8C)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1109)(company Quad))))))(marius.sukarevicius((id U06DP48MLUW)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 209)(company Qdev))))))(marius.sutkus((id U03F5C5RWQ6)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 181)(company Qdev))))))(milda.jankauskaite((id U08GAGW8MHA)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1177)(company Quad))))))(mirga.abramavice((id U03EDHQ4RQW)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 18)(company Quad))))))(mykolas.simutis((id U07MJ6G6CKD)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 232)(company Qdev))))))(oleg.krukovskij((id U03FC0V579Q)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 14)(company Qdev))))))(paulius.akulavicius((id U07864USBDW)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 228)(company Qdev))))))(paulius.astrauskas((id U03ESN3TEF5)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 145)(company Quad))))))(paulius.dauparas((id U03RS7HH1H8)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 184)(company Qdev))))))(ramunas.simonis((id U0609JND4CB)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1083)(company Quad))))))(rasa.danileviciute((id U06TLAL2Y6M)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1101)(company Quad))))))(rokas.naujalis((id U03FBHQPWV9)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 115)(company Qdev))))))(sandra.aleksieje((id U03E6NZQVTR)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1012)(company Quad))))))(sarunas.milasevicius((id U0661HNPM0X)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 204)(company Qdev))))))(sarunas.vilkas((id U06H67Y75TL)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1096)(company Quad))))))(saulius.lukosius((id U05TNQE8T0R)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1081)(company Quad))))))(sergei.berezko((id U03FCDYFN03)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 17)(company Qdev))((id 17)(company Quad))))))(tadas.vaitkevicius((id U0734G6V17D)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1113)(company Quad))))))(titas.kvederys((id U03GMTURPRD)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 151)(company Quad))))))(ugnius.bertasius((id U03FQJZ8XC1)(rights RAdmin)(has_permanent_parking true)(hcm_info(((id 44)(company Qdev))))))(vakaris.vaitulevicius((id U04NL5VG7TL)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 193)(company Qdev))))))(vidas.mikulskis((id U045QKVTY2K)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 20)(company Qdev))))))(viktorija.burlinskaite((id U07BSP4E725)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 1122)(company Quad))))))(viktorija.solonynka((id U05UVFBF80N)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 201)(company Qdev))))))(vygintas.simonavicius((id U03EJD1FLR3)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 93)(company Quad))))))(vytautas.aukstikalnis((id U03FBB7ATAP)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 46)(company Quad))))))(vytautas.butkevicius((id U03FDFJHENQ)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 25)(company Quad))))))(vytautas.gliaudelis((id U046V3F78DN)(rights RStandard)(has_permanent_parking true)(hcm_info(((id 24)(company Qdev))))))(xavier.errard((id U06P235T90A)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 214)(company Qdev))))))(zanas.kovaliovas((id U047RT091PD)(rights RStandard)(has_permanent_parking false)(hcm_info(((id 1030)(company Quad))))))))(usersFilename /home/angel/Documents/ocaml_examples/spaces_lot/users.json)) |}]
