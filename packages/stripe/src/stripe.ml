(** Stripe API Client for OCaml *)

module Core = Stripe_core

(** Re-export core types *)
type config = Core.config
type stripe_error = Core.stripe_error
type error_type = Core.error_type
type 'a api_result = 'a Core.api_result

let default_config = Core.default_config

(** Stripe Object - base type for all Stripe resources *)
module Stripe_object = struct
  type t = {
    id : string;
    object_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      raw = json;
    }

  let to_json t = t.raw

  let get_string t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_string_option

  let get_int t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_int_option

  let get_bool t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_bool_option
end

(** Address type used in many Stripe resources *)
module Address = struct
  type t = {
    city : string option;
    country : string option;
    line1 : string option;
    line2 : string option;
    postal_code : string option;
    state : string option;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      city = json |> member "city" |> to_string_option;
      country = json |> member "country" |> to_string_option;
      line1 = json |> member "line1" |> to_string_option;
      line2 = json |> member "line2" |> to_string_option;
      postal_code = json |> member "postal_code" |> to_string_option;
      state = json |> member "state" |> to_string_option;
    }

  let to_params ?prefix t =
    let p = match prefix with Some p -> p ^ "[" | None -> "" in
    let s = match prefix with Some _ -> "]" | None -> "" in
    List.filter_map Fun.id [
      Option.map (fun v -> (p ^ "city" ^ s, v)) t.city;
      Option.map (fun v -> (p ^ "country" ^ s, v)) t.country;
      Option.map (fun v -> (p ^ "line1" ^ s, v)) t.line1;
      Option.map (fun v -> (p ^ "line2" ^ s, v)) t.line2;
      Option.map (fun v -> (p ^ "postal_code" ^ s, v)) t.postal_code;
      Option.map (fun v -> (p ^ "state" ^ s, v)) t.state;
    ]
end

(** Customer resource *)
module Customer = struct
  type t = {
    id : string;
    object_ : string;
    address : Address.t option;
    balance : int;
    created : int;
    currency : string option;
    default_source : string option;
    delinquent : bool option;
    description : string option;
    email : string option;
    invoice_prefix : string option;
    livemode : bool;
    name : string option;
    phone : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      address = (
        let addr = json |> member "address" in
        if addr = `Null then None else Some (Address.of_json addr)
      );
      balance = json |> member "balance" |> to_int_option |> Option.value ~default:0;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      default_source = json |> member "default_source" |> to_string_option;
      delinquent = json |> member "delinquent" |> to_bool_option;
      description = json |> member "description" |> to_string_option;
      email = json |> member "email" |> to_string_option;
      invoice_prefix = json |> member "invoice_prefix" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      name = json |> member "name" |> to_string_option;
      phone = json |> member "phone" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Tax ID resource - represents a customer's tax ID for invoicing *)
module Tax_id = struct
  (** Owner type for tax IDs - can be owned by customer, account, application, or self *)
  type owner_type = Owner_customer | Owner_account | Owner_application | Owner_self

  let owner_type_of_string = function
    | "customer" -> Owner_customer
    | "account" -> Owner_account
    | "application" -> Owner_application
    | "self" -> Owner_self
    | _ -> Owner_customer

  type owner = {
    owner_type : owner_type;
    customer : string option;  (** Customer ID when type is "customer" *)
    account : string option;   (** Account ID when type is "account" *)
  }

  (** Tax ID types supported by Stripe.
      See https://docs.stripe.com/billing/customer/tax-ids *)
  type id_type =
    (* Europe *)
    | Ad_nrt         (** Andorran NRT number *)
    | Al_tin         (** Albanian TIN *)
    | Am_tin         (** Armenian TIN *)
    | At_vat         (** Austrian VAT number *)
    | Ba_tin         (** Bosnian TIN *)
    | Be_vat         (** Belgian VAT number *)
    | Bg_uic         (** Bulgarian UIC number *)
    | Bg_vat         (** Bulgarian VAT number *)
    | By_tin         (** Belarusian TIN *)
    | Ch_uid         (** Swiss UID number *)
    | Ch_vat         (** Swiss VAT number *)
    | Cy_vat         (** Cypriot VAT number *)
    | Cz_vat         (** Czech VAT number *)
    | De_stn         (** German Tax Number (Steuernummer) *)
    | De_vat         (** German VAT number *)
    | Dk_vat         (** Danish VAT number *)
    | Ee_vat         (** Estonian VAT number *)
    | Es_cif         (** Spanish CIF number *)
    | Es_vat         (** Spanish VAT number *)
    | Eu_oss_vat     (** European One-Stop Shop VAT number *)
    | Eu_vat         (** European VAT number (generic) *)
    | Fi_vat         (** Finnish VAT number *)
    | Fr_vat         (** French VAT number *)
    | Gb_vat         (** UK VAT number *)
    | Ge_vat         (** Georgian VAT number *)
    | Gr_vat         (** Greek VAT number *)
    | Hr_oib         (** Croatian OIB number *)
    | Hr_vat         (** Croatian VAT number *)
    | Hu_tin         (** Hungarian TIN number *)
    | Hu_vat         (** Hungarian VAT number *)
    | Ie_vat         (** Irish VAT number *)
    | Is_vat         (** Icelandic VAT number *)
    | It_vat         (** Italian VAT number *)
    | Li_uid         (** Liechtenstein UID number *)
    | Li_vat         (** Liechtenstein VAT number *)
    | Lt_vat         (** Lithuanian VAT number *)
    | Lu_vat         (** Luxembourg VAT number *)
    | Lv_vat         (** Latvian VAT number *)
    | Md_vat         (** Moldovan VAT number *)
    | Me_pib         (** Montenegrin PIB number *)
    | Mk_vat         (** North Macedonian VAT number *)
    | Mt_vat         (** Maltese VAT number *)
    | Nl_vat         (** Dutch VAT number *)
    | No_vat         (** Norwegian VAT number *)
    | No_voec        (** Norwegian VOEC number *)
    | Pl_vat         (** Polish VAT number *)
    | Pt_vat         (** Portuguese VAT number *)
    | Ro_tin         (** Romanian TIN number *)
    | Ro_vat         (** Romanian VAT number *)
    | Rs_pib         (** Serbian PIB number *)
    | Ru_inn         (** Russian INN number *)
    | Ru_kpp         (** Russian KPP number *)
    | Se_vat         (** Swedish VAT number *)
    | Si_tin         (** Slovenian TIN number *)
    | Si_vat         (** Slovenian VAT number *)
    | Sk_vat         (** Slovak VAT number *)
    | Sm_coe         (** San Marino COE number *)
    | Tr_tin         (** Turkish TIN number *)
    | Ua_vat         (** Ukrainian VAT number *)
    | Xi_vat         (** UK VAT for Northern Ireland *)
    (* Americas *)
    | Ar_cuit        (** Argentinian CUIT *)
    | Bo_tin         (** Bolivian TIN *)
    | Br_cnpj        (** Brazilian CNPJ number *)
    | Br_cpf         (** Brazilian CPF number *)
    | Ca_bn          (** Canadian Business Number *)
    | Ca_gst_hst     (** Canadian GST/HST Number *)
    | Ca_pst_bc      (** Canadian PST (British Columbia) *)
    | Ca_pst_mb      (** Canadian PST (Manitoba) *)
    | Ca_pst_sk      (** Canadian PST (Saskatchewan) *)
    | Ca_qst         (** Canadian QST (Quebec) *)
    | Cl_tin         (** Chilean TIN *)
    | Co_nit         (** Colombian NIT *)
    | Cr_tin         (** Costa Rican TIN *)
    | Do_rcn         (** Dominican Republic RCN *)
    | Ec_ruc         (** Ecuadorian RUC *)
    | Gt_nit         (** Guatemalan NIT *)
    | Hn_tin         (** Honduran TIN *)
    | Mx_rfc         (** Mexican RFC *)
    | Ni_ruc         (** Nicaraguan RUC *)
    | Pa_ruc         (** Panamanian RUC *)
    | Pe_ruc         (** Peruvian RUC *)
    | Py_ruc         (** Paraguayan RUC *)
    | Sv_nit         (** Salvadoran NIT *)
    | Us_ein         (** US Employer Identification Number *)
    | Uy_ruc         (** Uruguayan RUC *)
    | Ve_rif         (** Venezuelan RIF *)
    (* Asia Pacific *)
    | Au_abn         (** Australian Business Number *)
    | Au_arn         (** Australian Registered Body Number *)
    | Bd_bin         (** Bangladeshi BIN *)
    | Cn_tin         (** Chinese TIN *)
    | Hk_br          (** Hong Kong BR number *)
    | Id_npwp        (** Indonesian NPWP *)
    | In_gst         (** Indian GST number *)
    | Jp_cn          (** Japanese Corporate Number *)
    | Jp_rn          (** Japanese Registered Number *)
    | Jp_trn         (** Japanese Tax Registration Number *)
    | Kh_tin         (** Cambodian TIN *)
    | Kr_brn         (** Korean BRN *)
    | Kz_bin         (** Kazakhstani BIN *)
    | La_tin         (** Laotian TIN *)
    | Lk_svat        (** Sri Lankan SVAT *)
    | Mm_tin         (** Myanmar TIN *)
    | Mn_tin         (** Mongolian TIN *)
    | My_frp         (** Malaysian FRP *)
    | My_itn         (** Malaysian ITN *)
    | My_sst         (** Malaysian SST *)
    | Np_pan         (** Nepalese PAN *)
    | Nz_gst         (** New Zealand GST number *)
    | Ph_tin         (** Philippines TIN *)
    | Pk_ntn         (** Pakistani NTN *)
    | Sg_gst         (** Singapore GST *)
    | Sg_uen         (** Singapore UEN *)
    | Th_vat         (** Thai VAT *)
    | Tw_vat         (** Taiwanese VAT *)
    | Uz_tin         (** Uzbekistani TIN *)
    | Uz_vat         (** Uzbekistani VAT *)
    | Vn_tin         (** Vietnamese TIN *)
    (* Middle East & Africa *)
    | Ae_trn         (** UAE TRN *)
    | Ao_tin         (** Angolan TIN *)
    | Bh_vat         (** Bahraini VAT *)
    | Dz_vat         (** Algerian VAT *)
    | Eg_tin         (** Egyptian TIN *)
    | Et_tin         (** Ethiopian TIN *)
    | Gh_tin         (** Ghanaian TIN *)
    | Il_vat         (** Israeli VAT *)
    | Jo_tin         (** Jordanian TIN *)
    | Ke_pin         (** Kenyan PIN *)
    | Kw_vat         (** Kuwaiti VAT *)
    | Ma_vat         (** Moroccan VAT *)
    | Ng_tin         (** Nigerian TIN *)
    | Om_vat         (** Omani VAT *)
    | Qa_vat         (** Qatari VAT *)
    | Sa_vat         (** Saudi Arabia VAT *)
    | Sn_ninea       (** Senegalese NINEA *)
    | Tn_vat         (** Tunisian VAT *)
    | Tz_vat         (** Tanzanian VAT *)
    | Ug_tin         (** Ugandan TIN *)
    | Za_vat         (** South African VAT *)
    | Zm_tin         (** Zambian TIN *)
    | Zw_tin         (** Zimbabwean TIN *)
    (* Caribbean & Other *)
    | Aw_tin         (** Aruban TIN *)
    | Az_tin         (** Azerbaijani TIN *)
    | Bb_tin         (** Barbadian TIN *)
    | Bf_ifu         (** Burkinabe IFU *)
    | Bj_ifu         (** Beninese IFU *)
    | Bs_tin         (** Bahamian TIN *)
    | Cd_nif         (** Congolese NIF *)
    | Cm_niu         (** Cameroonian NIU *)
    | Cv_nif         (** Cape Verdean NIF *)
    | Gn_nif         (** Guinean NIF *)
    | Kg_tin         (** Kyrgyzstani TIN *)
    | Mr_nif         (** Mauritanian NIF *)
    | Sr_fin         (** Surinamese FIN *)
    | Tj_tin         (** Tajikistani TIN *)
    (* Other *)
    | Other of string (** Fallback for unknown types *)

  let id_type_of_string = function
    (* Europe *)
    | "ad_nrt" -> Ad_nrt | "al_tin" -> Al_tin | "am_tin" -> Am_tin
    | "at_vat" -> At_vat | "ba_tin" -> Ba_tin | "be_vat" -> Be_vat
    | "bg_uic" -> Bg_uic | "bg_vat" -> Bg_vat | "by_tin" -> By_tin
    | "ch_uid" -> Ch_uid | "ch_vat" -> Ch_vat | "cy_vat" -> Cy_vat
    | "cz_vat" -> Cz_vat | "de_stn" -> De_stn | "de_vat" -> De_vat
    | "dk_vat" -> Dk_vat | "ee_vat" -> Ee_vat | "es_cif" -> Es_cif
    | "es_vat" -> Es_vat | "eu_oss_vat" -> Eu_oss_vat | "eu_vat" -> Eu_vat
    | "fi_vat" -> Fi_vat | "fr_vat" -> Fr_vat | "gb_vat" -> Gb_vat
    | "ge_vat" -> Ge_vat | "gr_vat" -> Gr_vat | "hr_oib" -> Hr_oib
    | "hr_vat" -> Hr_vat | "hu_tin" -> Hu_tin | "hu_vat" -> Hu_vat
    | "ie_vat" -> Ie_vat | "is_vat" -> Is_vat | "it_vat" -> It_vat
    | "li_uid" -> Li_uid | "li_vat" -> Li_vat | "lt_vat" -> Lt_vat
    | "lu_vat" -> Lu_vat | "lv_vat" -> Lv_vat | "md_vat" -> Md_vat
    | "me_pib" -> Me_pib | "mk_vat" -> Mk_vat | "mt_vat" -> Mt_vat
    | "nl_vat" -> Nl_vat | "no_vat" -> No_vat | "no_voec" -> No_voec
    | "pl_vat" -> Pl_vat | "pt_vat" -> Pt_vat | "ro_tin" -> Ro_tin
    | "ro_vat" -> Ro_vat | "rs_pib" -> Rs_pib | "ru_inn" -> Ru_inn
    | "ru_kpp" -> Ru_kpp | "se_vat" -> Se_vat | "si_tin" -> Si_tin
    | "si_vat" -> Si_vat | "sk_vat" -> Sk_vat | "sm_coe" -> Sm_coe
    | "tr_tin" -> Tr_tin | "ua_vat" -> Ua_vat | "xi_vat" -> Xi_vat
    (* Americas *)
    | "ar_cuit" -> Ar_cuit | "bo_tin" -> Bo_tin | "br_cnpj" -> Br_cnpj
    | "br_cpf" -> Br_cpf | "ca_bn" -> Ca_bn | "ca_gst_hst" -> Ca_gst_hst
    | "ca_pst_bc" -> Ca_pst_bc | "ca_pst_mb" -> Ca_pst_mb
    | "ca_pst_sk" -> Ca_pst_sk | "ca_qst" -> Ca_qst | "cl_tin" -> Cl_tin
    | "co_nit" -> Co_nit | "cr_tin" -> Cr_tin | "do_rcn" -> Do_rcn
    | "ec_ruc" -> Ec_ruc | "gt_nit" -> Gt_nit | "hn_tin" -> Hn_tin
    | "mx_rfc" -> Mx_rfc | "ni_ruc" -> Ni_ruc | "pa_ruc" -> Pa_ruc
    | "pe_ruc" -> Pe_ruc | "py_ruc" -> Py_ruc | "sv_nit" -> Sv_nit
    | "us_ein" -> Us_ein | "uy_ruc" -> Uy_ruc | "ve_rif" -> Ve_rif
    (* Asia Pacific *)
    | "au_abn" -> Au_abn | "au_arn" -> Au_arn | "bd_bin" -> Bd_bin
    | "cn_tin" -> Cn_tin | "hk_br" -> Hk_br | "id_npwp" -> Id_npwp
    | "in_gst" -> In_gst | "jp_cn" -> Jp_cn | "jp_rn" -> Jp_rn
    | "jp_trn" -> Jp_trn | "kh_tin" -> Kh_tin | "kr_brn" -> Kr_brn
    | "kz_bin" -> Kz_bin | "la_tin" -> La_tin | "lk_svat" -> Lk_svat
    | "mm_tin" -> Mm_tin | "mn_tin" -> Mn_tin | "my_frp" -> My_frp
    | "my_itn" -> My_itn | "my_sst" -> My_sst | "np_pan" -> Np_pan
    | "nz_gst" -> Nz_gst | "ph_tin" -> Ph_tin | "pk_ntn" -> Pk_ntn
    | "sg_gst" -> Sg_gst | "sg_uen" -> Sg_uen | "th_vat" -> Th_vat
    | "tw_vat" -> Tw_vat | "uz_tin" -> Uz_tin | "uz_vat" -> Uz_vat
    | "vn_tin" -> Vn_tin
    (* Middle East & Africa *)
    | "ae_trn" -> Ae_trn | "ao_tin" -> Ao_tin | "bh_vat" -> Bh_vat
    | "dz_vat" -> Dz_vat | "eg_tin" -> Eg_tin | "et_tin" -> Et_tin
    | "gh_tin" -> Gh_tin | "il_vat" -> Il_vat | "jo_tin" -> Jo_tin
    | "ke_pin" -> Ke_pin | "kw_vat" -> Kw_vat | "ma_vat" -> Ma_vat
    | "ng_tin" -> Ng_tin | "om_vat" -> Om_vat | "qa_vat" -> Qa_vat
    | "sa_vat" -> Sa_vat | "sn_ninea" -> Sn_ninea | "tn_vat" -> Tn_vat
    | "tz_vat" -> Tz_vat | "ug_tin" -> Ug_tin | "za_vat" -> Za_vat
    | "zm_tin" -> Zm_tin | "zw_tin" -> Zw_tin
    (* Caribbean & Other *)
    | "aw_tin" -> Aw_tin | "az_tin" -> Az_tin | "bb_tin" -> Bb_tin
    | "bf_ifu" -> Bf_ifu | "bj_ifu" -> Bj_ifu | "bs_tin" -> Bs_tin
    | "cd_nif" -> Cd_nif | "cm_niu" -> Cm_niu | "cv_nif" -> Cv_nif
    | "gn_nif" -> Gn_nif | "kg_tin" -> Kg_tin | "mr_nif" -> Mr_nif
    | "sr_fin" -> Sr_fin | "tj_tin" -> Tj_tin
    | s -> Other s

  let id_type_to_string = function
    (* Europe *)
    | Ad_nrt -> "ad_nrt" | Al_tin -> "al_tin" | Am_tin -> "am_tin"
    | At_vat -> "at_vat" | Ba_tin -> "ba_tin" | Be_vat -> "be_vat"
    | Bg_uic -> "bg_uic" | Bg_vat -> "bg_vat" | By_tin -> "by_tin"
    | Ch_uid -> "ch_uid" | Ch_vat -> "ch_vat" | Cy_vat -> "cy_vat"
    | Cz_vat -> "cz_vat" | De_stn -> "de_stn" | De_vat -> "de_vat"
    | Dk_vat -> "dk_vat" | Ee_vat -> "ee_vat" | Es_cif -> "es_cif"
    | Es_vat -> "es_vat" | Eu_oss_vat -> "eu_oss_vat" | Eu_vat -> "eu_vat"
    | Fi_vat -> "fi_vat" | Fr_vat -> "fr_vat" | Gb_vat -> "gb_vat"
    | Ge_vat -> "ge_vat" | Gr_vat -> "gr_vat" | Hr_oib -> "hr_oib"
    | Hr_vat -> "hr_vat" | Hu_tin -> "hu_tin" | Hu_vat -> "hu_vat"
    | Ie_vat -> "ie_vat" | Is_vat -> "is_vat" | It_vat -> "it_vat"
    | Li_uid -> "li_uid" | Li_vat -> "li_vat" | Lt_vat -> "lt_vat"
    | Lu_vat -> "lu_vat" | Lv_vat -> "lv_vat" | Md_vat -> "md_vat"
    | Me_pib -> "me_pib" | Mk_vat -> "mk_vat" | Mt_vat -> "mt_vat"
    | Nl_vat -> "nl_vat" | No_vat -> "no_vat" | No_voec -> "no_voec"
    | Pl_vat -> "pl_vat" | Pt_vat -> "pt_vat" | Ro_tin -> "ro_tin"
    | Ro_vat -> "ro_vat" | Rs_pib -> "rs_pib" | Ru_inn -> "ru_inn"
    | Ru_kpp -> "ru_kpp" | Se_vat -> "se_vat" | Si_tin -> "si_tin"
    | Si_vat -> "si_vat" | Sk_vat -> "sk_vat" | Sm_coe -> "sm_coe"
    | Tr_tin -> "tr_tin" | Ua_vat -> "ua_vat" | Xi_vat -> "xi_vat"
    (* Americas *)
    | Ar_cuit -> "ar_cuit" | Bo_tin -> "bo_tin" | Br_cnpj -> "br_cnpj"
    | Br_cpf -> "br_cpf" | Ca_bn -> "ca_bn" | Ca_gst_hst -> "ca_gst_hst"
    | Ca_pst_bc -> "ca_pst_bc" | Ca_pst_mb -> "ca_pst_mb"
    | Ca_pst_sk -> "ca_pst_sk" | Ca_qst -> "ca_qst" | Cl_tin -> "cl_tin"
    | Co_nit -> "co_nit" | Cr_tin -> "cr_tin" | Do_rcn -> "do_rcn"
    | Ec_ruc -> "ec_ruc" | Gt_nit -> "gt_nit" | Hn_tin -> "hn_tin"
    | Mx_rfc -> "mx_rfc" | Ni_ruc -> "ni_ruc" | Pa_ruc -> "pa_ruc"
    | Pe_ruc -> "pe_ruc" | Py_ruc -> "py_ruc" | Sv_nit -> "sv_nit"
    | Us_ein -> "us_ein" | Uy_ruc -> "uy_ruc" | Ve_rif -> "ve_rif"
    (* Asia Pacific *)
    | Au_abn -> "au_abn" | Au_arn -> "au_arn" | Bd_bin -> "bd_bin"
    | Cn_tin -> "cn_tin" | Hk_br -> "hk_br" | Id_npwp -> "id_npwp"
    | In_gst -> "in_gst" | Jp_cn -> "jp_cn" | Jp_rn -> "jp_rn"
    | Jp_trn -> "jp_trn" | Kh_tin -> "kh_tin" | Kr_brn -> "kr_brn"
    | Kz_bin -> "kz_bin" | La_tin -> "la_tin" | Lk_svat -> "lk_svat"
    | Mm_tin -> "mm_tin" | Mn_tin -> "mn_tin" | My_frp -> "my_frp"
    | My_itn -> "my_itn" | My_sst -> "my_sst" | Np_pan -> "np_pan"
    | Nz_gst -> "nz_gst" | Ph_tin -> "ph_tin" | Pk_ntn -> "pk_ntn"
    | Sg_gst -> "sg_gst" | Sg_uen -> "sg_uen" | Th_vat -> "th_vat"
    | Tw_vat -> "tw_vat" | Uz_tin -> "uz_tin" | Uz_vat -> "uz_vat"
    | Vn_tin -> "vn_tin"
    (* Middle East & Africa *)
    | Ae_trn -> "ae_trn" | Ao_tin -> "ao_tin" | Bh_vat -> "bh_vat"
    | Dz_vat -> "dz_vat" | Eg_tin -> "eg_tin" | Et_tin -> "et_tin"
    | Gh_tin -> "gh_tin" | Il_vat -> "il_vat" | Jo_tin -> "jo_tin"
    | Ke_pin -> "ke_pin" | Kw_vat -> "kw_vat" | Ma_vat -> "ma_vat"
    | Ng_tin -> "ng_tin" | Om_vat -> "om_vat" | Qa_vat -> "qa_vat"
    | Sa_vat -> "sa_vat" | Sn_ninea -> "sn_ninea" | Tn_vat -> "tn_vat"
    | Tz_vat -> "tz_vat" | Ug_tin -> "ug_tin" | Za_vat -> "za_vat"
    | Zm_tin -> "zm_tin" | Zw_tin -> "zw_tin"
    (* Caribbean & Other *)
    | Aw_tin -> "aw_tin" | Az_tin -> "az_tin" | Bb_tin -> "bb_tin"
    | Bf_ifu -> "bf_ifu" | Bj_ifu -> "bj_ifu" | Bs_tin -> "bs_tin"
    | Cd_nif -> "cd_nif" | Cm_niu -> "cm_niu" | Cv_nif -> "cv_nif"
    | Gn_nif -> "gn_nif" | Kg_tin -> "kg_tin" | Mr_nif -> "mr_nif"
    | Sr_fin -> "sr_fin" | Tj_tin -> "tj_tin"
    | Other s -> s

  (** Verification status for a tax ID *)
  type verification_status =
    | Pending
    | Verified
    | Unverified
    | Unavailable

  let verification_status_of_string = function
    | "pending" -> Pending
    | "verified" -> Verified
    | "unverified" -> Unverified
    | "unavailable" -> Unavailable
    | _ -> Unavailable

  type verification = {
    status : verification_status;
    verified_name : string option;
    verified_address : string option;
  }

  type t = {
    id : string;
    object_ : string;
    country : string option;
    created : int;
    customer : string option;
    livemode : bool;
    owner : owner option;  (** Owner of the tax ID (Connect only) *)
    type_ : id_type;
    value : string;
    verification : verification option;
    raw : Yojson.Safe.t;
  }

  let owner_of_json json =
    let open Yojson.Safe.Util in
    {
      owner_type = json |> member "type" |> to_string |> owner_type_of_string;
      customer = json |> member "customer" |> to_string_option;
      account = json |> member "account" |> to_string_option;
    }

  let verification_of_json json =
    let open Yojson.Safe.Util in
    {
      status = json |> member "status" |> to_string |> verification_status_of_string;
      verified_name = json |> member "verified_name" |> to_string_option;
      verified_address = json |> member "verified_address" |> to_string_option;
    }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      country = json |> member "country" |> to_string_option;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      owner = (
        let o = json |> member "owner" in
        if o = `Null then None else Some (owner_of_json o)
      );
      type_ = json |> member "type" |> to_string |> id_type_of_string;
      value = json |> member "value" |> to_string;
      verification = (
        let v = json |> member "verification" in
        if v = `Null then None else Some (verification_of_json v)
      );
      raw = json;
    }

  let to_json t = t.raw

  (** Check if the tax ID is verified *)
  let is_verified t =
    match t.verification with
    | Some v -> v.status = Verified
    | None -> false
end

(** Charge resource *)
module Charge = struct
  type status = Succeeded | Pending | Failed

  let status_of_string = function
    | "succeeded" -> Succeeded
    | "pending" -> Pending
    | "failed" -> Failed
    | _ -> Pending

  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_captured : int;
    amount_refunded : int;
    captured : bool;
    created : int;
    currency : string;
    customer : string option;
    description : string option;
    failure_code : string option;
    failure_message : string option;
    livemode : bool;
    paid : bool;
    payment_intent : string option;
    refunded : bool;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_captured = json |> member "amount_captured" |> to_int_option |> Option.value ~default:0;
      amount_refunded = json |> member "amount_refunded" |> to_int_option |> Option.value ~default:0;
      captured = json |> member "captured" |> to_bool;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      failure_code = json |> member "failure_code" |> to_string_option;
      failure_message = json |> member "failure_message" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      paid = json |> member "paid" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      refunded = json |> member "refunded" |> to_bool;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentIntent resource *)
module Payment_intent = struct
  type status =
    | Requires_payment_method
    | Requires_confirmation
    | Requires_action
    | Processing
    | Requires_capture
    | Canceled
    | Succeeded

  let status_of_string = function
    | "requires_payment_method" -> Requires_payment_method
    | "requires_confirmation" -> Requires_confirmation
    | "requires_action" -> Requires_action
    | "processing" -> Processing
    | "requires_capture" -> Requires_capture
    | "canceled" -> Canceled
    | "succeeded" -> Succeeded
    | _ -> Requires_payment_method

  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_received : int;
    capture_method : string;
    client_secret : string option;
    confirmation_method : string;
    created : int;
    currency : string;
    customer : string option;
    description : string option;
    livemode : bool;
    payment_method : string option;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_received = json |> member "amount_received" |> to_int_option |> Option.value ~default:0;
      capture_method = json |> member "capture_method" |> to_string_option |> Option.value ~default:"automatic";
      client_secret = json |> member "client_secret" |> to_string_option;
      confirmation_method = json |> member "confirmation_method" |> to_string_option |> Option.value ~default:"automatic";
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Refund resource *)
module Refund = struct
  type status = Pending | Succeeded | Failed | Canceled

  let status_of_string = function
    | "pending" -> Pending
    | "succeeded" -> Succeeded
    | "failed" -> Failed
    | "canceled" -> Canceled
    | _ -> Pending

  type t = {
    id : string;
    object_ : string;
    amount : int;
    charge : string option;
    created : int;
    currency : string;
    payment_intent : string option;
    reason : string option;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      charge = json |> member "charge" |> to_string_option;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Balance resource *)
module Balance = struct
  type balance_amount = {
    amount : int;
    currency : string;
  }

  let balance_amount_of_json json =
    let open Yojson.Safe.Util in
    {
      amount = json |> member "amount" |> to_int;
      currency = json |> member "currency" |> to_string;
    }

  type t = {
    object_ : string;
    available : balance_amount list;
    pending : balance_amount list;
    livemode : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      object_ = json |> member "object" |> to_string;
      available = json |> member "available" |> to_list |> List.map balance_amount_of_json;
      pending = json |> member "pending" |> to_list |> List.map balance_amount_of_json;
      livemode = json |> member "livemode" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Event resource for webhooks *)
module Event = struct
  type t = {
    id : string;
    object_ : string;
    api_version : string option;
    created : int;
    data : Yojson.Safe.t;
    livemode : bool;
    pending_webhooks : int;
    request : Yojson.Safe.t option;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      api_version = json |> member "api_version" |> to_string_option;
      created = json |> member "created" |> to_int;
      data = json |> member "data";
      livemode = json |> member "livemode" |> to_bool;
      pending_webhooks = json |> member "pending_webhooks" |> to_int;
      request = (
        let req = json |> member "request" in
        if req = `Null then None else Some req
      );
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Subscription resource *)
module Subscription = struct
  type status =
    | Active
    | Past_due
    | Unpaid
    | Canceled
    | Incomplete
    | Incomplete_expired
    | Trialing
    | Paused

  let status_of_string = function
    | "active" -> Active
    | "past_due" -> Past_due
    | "unpaid" -> Unpaid
    | "canceled" -> Canceled
    | "incomplete" -> Incomplete
    | "incomplete_expired" -> Incomplete_expired
    | "trialing" -> Trialing
    | "paused" -> Paused
    | _ -> Active

  (** Collection method for subscription *)
  type collection_method = Charge_automatically | Send_invoice

  let collection_method_of_string = function
    | "charge_automatically" -> Charge_automatically
    | "send_invoice" -> Send_invoice
    | _ -> Charge_automatically

  type t = {
    id : string;
    object_ : string;
    (* Billing *)
    billing_cycle_anchor : int option;
    cancel_at : int option;
    cancel_at_period_end : bool;
    canceled_at : int option;
    collection_method : collection_method option;
    created : int;
    currency : string option;
    current_period_end : int;
    current_period_start : int;
    customer : string;
    days_until_due : int option;
    (* Payment *)
    default_payment_method : string option;
    default_source : string option;
    (* Description *)
    description : string option;
    (* Dates *)
    ended_at : int option;
    (* Invoice *)
    latest_invoice : string option;
    (* Mode *)
    livemode : bool;
    (* Metadata *)
    metadata : (string * string) list;
    (* Status *)
    status : status;
    start_date : int option;
    (* Trial *)
    trial_end : int option;
    trial_start : int option;
    (* Raw JSON for additional fields *)
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    let parse_metadata json =
      match json |> member "metadata" with
      | `Assoc pairs -> List.map (fun (k, v) -> (k, to_string v)) pairs
      | _ -> []
    in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      billing_cycle_anchor = json |> member "billing_cycle_anchor" |> to_int_option;
      cancel_at = json |> member "cancel_at" |> to_int_option;
      cancel_at_period_end = json |> member "cancel_at_period_end" |> to_bool;
      canceled_at = json |> member "canceled_at" |> to_int_option;
      collection_method = json |> member "collection_method" |> to_string_option 
                          |> Option.map collection_method_of_string;
      created = json |> member "created" |> to_int_option |> Option.value ~default:0;
      currency = json |> member "currency" |> to_string_option;
      current_period_end = json |> member "current_period_end" |> to_int;
      current_period_start = json |> member "current_period_start" |> to_int;
      customer = json |> member "customer" |> to_string;
      days_until_due = json |> member "days_until_due" |> to_int_option;
      default_payment_method = json |> member "default_payment_method" |> to_string_option;
      default_source = json |> member "default_source" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      ended_at = json |> member "ended_at" |> to_int_option;
      latest_invoice = json |> member "latest_invoice" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      metadata = parse_metadata json;
      status = json |> member "status" |> to_string |> status_of_string;
      start_date = json |> member "start_date" |> to_int_option;
      trial_end = json |> member "trial_end" |> to_int_option;
      trial_start = json |> member "trial_start" |> to_int_option;
      raw = json;
    }

  let to_json t = t.raw

  (** Check if subscription is in trial *)
  let is_trialing t = t.status = Trialing

  (** Check if subscription is active or trialing *)
  let is_active t = t.status = Active || t.status = Trialing

  (** Check if subscription has been canceled *)
  let is_canceled t = t.status = Canceled

  (** Check if subscription will cancel at period end *)
  let will_cancel t = t.cancel_at_period_end || Option.is_some t.cancel_at
end

(** Product resource *)
module Product = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    created : int;
    description : string option;
    livemode : bool;
    name : string;
    updated : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      created = json |> member "created" |> to_int;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      name = json |> member "name" |> to_string;
      updated = json |> member "updated" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** Price resource *)
module Price = struct
  type recurring = {
    interval : string;
    interval_count : int;
  }

  let recurring_of_json json =
    let open Yojson.Safe.Util in
    {
      interval = json |> member "interval" |> to_string;
      interval_count = json |> member "interval_count" |> to_int;
    }

  type t = {
    id : string;
    object_ : string;
    active : bool;
    billing_scheme : string;
    created : int;
    currency : string;
    livemode : bool;
    product : string;
    recurring : recurring option;
    unit_amount : int option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      billing_scheme = json |> member "billing_scheme" |> to_string;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      product = json |> member "product" |> to_string;
      recurring = (
        let rec_json = json |> member "recurring" in
        if rec_json = `Null then None else Some (recurring_of_json rec_json)
      );
      unit_amount = json |> member "unit_amount" |> to_int_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Invoice resource *)
module Invoice = struct
  type status =
    | Draft
    | Open
    | Paid
    | Uncollectible
    | Void

  let status_of_string = function
    | "draft" -> Draft
    | "open" -> Open
    | "paid" -> Paid
    | "uncollectible" -> Uncollectible
    | "void" -> Void
    | _ -> Draft

  type t = {
    id : string;
    object_ : string;
    amount_due : int;
    amount_paid : int;
    amount_remaining : int;
    created : int;
    currency : string;
    customer : string;
    livemode : bool;
    status : status option;
    subscription : string option;
    total : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_due = json |> member "amount_due" |> to_int;
      amount_paid = json |> member "amount_paid" |> to_int;
      amount_remaining = json |> member "amount_remaining" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string_option |> Option.map status_of_string;
      subscription = json |> member "subscription" |> to_string_option;
      total = json |> member "total" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentMethod resource *)
module Payment_method = struct
  type card = {
    brand : string;
    exp_month : int;
    exp_year : int;
    last4 : string;
  }

  let card_of_json json =
    let open Yojson.Safe.Util in
    {
      brand = json |> member "brand" |> to_string;
      exp_month = json |> member "exp_month" |> to_int;
      exp_year = json |> member "exp_year" |> to_int;
      last4 = json |> member "last4" |> to_string;
    }

  type t = {
    id : string;
    object_ : string;
    billing_details : Yojson.Safe.t;
    card : card option;
    created : int;
    customer : string option;
    livemode : bool;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      billing_details = json |> member "billing_details";
      card = (
        let c = json |> member "card" in
        if c = `Null then None else Some (card_of_json c)
      );
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** SetupIntent resource *)
module Setup_intent = struct
  type status =
    | Requires_payment_method
    | Requires_confirmation
    | Requires_action
    | Processing
    | Canceled
    | Succeeded

  let status_of_string = function
    | "requires_payment_method" -> Requires_payment_method
    | "requires_confirmation" -> Requires_confirmation
    | "requires_action" -> Requires_action
    | "processing" -> Processing
    | "canceled" -> Canceled
    | "succeeded" -> Succeeded
    | _ -> Requires_payment_method

  type t = {
    id : string;
    object_ : string;
    client_secret : string option;
    created : int;
    customer : string option;
    description : string option;
    livemode : bool;
    payment_method : string option;
    status : status;
    usage : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      client_secret = json |> member "client_secret" |> to_string_option;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      usage = json |> member "usage" |> to_string_option |> Option.value ~default:"off_session";
      raw = json;
    }

  let to_json t = t.raw
end

(** Coupon resource *)
module Coupon = struct
  type duration = Once | Repeating | Forever

  let duration_of_string = function
    | "once" -> Once
    | "repeating" -> Repeating
    | "forever" -> Forever
    | _ -> Once

  type t = {
    id : string;
    object_ : string;
    amount_off : int option;
    created : int;
    currency : string option;
    duration : duration;
    duration_in_months : int option;
    livemode : bool;
    max_redemptions : int option;
    name : string option;
    percent_off : float option;
    times_redeemed : int;
    valid : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_off = json |> member "amount_off" |> to_int_option;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      duration = json |> member "duration" |> to_string |> duration_of_string;
      duration_in_months = json |> member "duration_in_months" |> to_int_option;
      livemode = json |> member "livemode" |> to_bool;
      max_redemptions = json |> member "max_redemptions" |> to_int_option;
      name = json |> member "name" |> to_string_option;
      percent_off = json |> member "percent_off" |> to_float_option;
      times_redeemed = json |> member "times_redeemed" |> to_int;
      valid = json |> member "valid" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Discount resource *)
module Discount = struct
  type t = {
    id : string;
    object_ : string;
    coupon : Coupon.t;
    customer : string option;
    start : int;
    end_ : int option;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      coupon = json |> member "coupon" |> Coupon.of_json;
      customer = json |> member "customer" |> to_string_option;
      start = json |> member "start" |> to_int;
      end_ = json |> member "end" |> to_int_option;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** BalanceTransaction resource *)
module Balance_transaction = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    available_on : int;
    created : int;
    currency : string;
    description : string option;
    fee : int;
    net : int;
    source : string option;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      available_on = json |> member "available_on" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      fee = json |> member "fee" |> to_int;
      net = json |> member "net" |> to_int;
      source = json |> member "source" |> to_string_option;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Payout resource *)
module Payout = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    arrival_date : int;
    created : int;
    currency : string;
    description : string option;
    destination : string option;
    livemode : bool;
    method_ : string;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      arrival_date = json |> member "arrival_date" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      destination = json |> member "destination" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      method_ = json |> member "method" |> to_string;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Checkout Session resource *)
module Checkout_session = struct
  type line_item = {
    id : string;
    object_ : string;
    amount_total : int;
    currency : string;
    description : string option;
    quantity : int option;
  }

  type t = {
    id : string;
    object_ : string;
    cancel_url : string option;
    client_reference_id : string option;
    currency : string option;
    customer : string option;
    customer_email : string option;
    livemode : bool;
    mode : string;
    payment_intent : string option;
    payment_status : string;
    status : string option;
    success_url : string option;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      cancel_url = json |> member "cancel_url" |> to_string_option;
      client_reference_id = json |> member "client_reference_id" |> to_string_option;
      currency = json |> member "currency" |> to_string_option;
      customer = json |> member "customer" |> to_string_option;
      customer_email = json |> member "customer_email" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      mode = json |> member "mode" |> to_string;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      payment_status = json |> member "payment_status" |> to_string;
      status = json |> member "status" |> to_string_option;
      success_url = json |> member "success_url" |> to_string_option;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** TaxRate resource *)
module Tax_rate = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    country : string option;
    description : string option;
    display_name : string;
    inclusive : bool;
    jurisdiction : string option;
    percentage : float;
    state : string option;
    tax_type : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      country = json |> member "country" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      display_name = json |> member "display_name" |> to_string;
      inclusive = json |> member "inclusive" |> to_bool;
      jurisdiction = json |> member "jurisdiction" |> to_string_option;
      percentage = json |> member "percentage" |> to_float;
      state = json |> member "state" |> to_string_option;
      tax_type = json |> member "tax_type" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentLink resource *)
module Payment_link = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    currency : string option;
    livemode : bool;
    url : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      currency = json |> member "currency" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      url = json |> member "url" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Dispute resource *)
module Dispute = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    charge : string;
    currency : string;
    created : int;
    is_charge_refundable : bool;
    livemode : bool;
    payment_intent : string option;
    reason : string;
    status : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      charge = json |> member "charge" |> to_string;
      currency = json |> member "currency" |> to_string;
      created = json |> member "created" |> to_int;
      is_charge_refundable = json |> member "is_charge_refundable" |> to_bool;
      livemode = json |> member "livemode" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string;
      status = json |> member "status" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Account resource (Connect) *)
module Account = struct
  type business_profile = {
    mcc : string option;
    name : string option;
    url : string option;
  }

  type t = {
    id : string;
    object_ : string;
    business_type : string option;
    charges_enabled : bool;
    country : string;
    created : int;
    default_currency : string option;
    details_submitted : bool;
    email : string option;
    payouts_enabled : bool;
    type_ : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      business_type = json |> member "business_type" |> to_string_option;
      charges_enabled = json |> member "charges_enabled" |> to_bool;
      country = json |> member "country" |> to_string;
      created = json |> member "created" |> to_int;
      default_currency = json |> member "default_currency" |> to_string_option;
      details_submitted = json |> member "details_submitted" |> to_bool;
      email = json |> member "email" |> to_string_option;
      payouts_enabled = json |> member "payouts_enabled" |> to_bool;
      type_ = json |> member "type" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Transfer resource (Connect) *)
module Transfer = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_reversed : int;
    created : int;
    currency : string;
    description : string option;
    destination : string;
    livemode : bool;
    reversed : bool;
    source_transaction : string option;
    source_type : string;
    transfer_group : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_reversed = json |> member "amount_reversed" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      destination = json |> member "destination" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      reversed = json |> member "reversed" |> to_bool;
      source_transaction = json |> member "source_transaction" |> to_string_option;
      source_type = json |> member "source_type" |> to_string;
      transfer_group = json |> member "transfer_group" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** File resource *)
module File = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    expires_at : int option;
    filename : string option;
    purpose : string;
    size : int;
    type_ : string option;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      expires_at = json |> member "expires_at" |> to_int_option;
      filename = json |> member "filename" |> to_string_option;
      purpose = json |> member "purpose" |> to_string;
      size = json |> member "size" |> to_int;
      type_ = json |> member "type" |> to_string_option;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** FileLink resource *)
module File_link = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    expired : bool;
    expires_at : int option;
    file : string;
    livemode : bool;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      expired = json |> member "expired" |> to_bool;
      expires_at = json |> member "expires_at" |> to_int_option;
      file = json |> member "file" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Mandate resource *)
module Mandate = struct
  type customer_acceptance = {
    accepted_at : int option;
    type_ : string;
  }

  type t = {
    id : string;
    object_ : string;
    livemode : bool;
    payment_method : string;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Review resource (Radar) *)
module Review = struct
  type t = {
    id : string;
    object_ : string;
    charge : string option;
    created : int;
    livemode : bool;
    open_ : bool;
    payment_intent : string option;
    reason : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      charge = json |> member "charge" |> to_string_option;
      created = json |> member "created" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      open_ = json |> member "open" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** PromotionCode resource *)
module Promotion_code = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    code : string;
    coupon : Coupon.t;
    created : int;
    customer : string option;
    expires_at : int option;
    livemode : bool;
    max_redemptions : int option;
    times_redeemed : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      code = json |> member "code" |> to_string;
      coupon = json |> member "coupon" |> Coupon.of_json;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      expires_at = json |> member "expires_at" |> to_int_option;
      livemode = json |> member "livemode" |> to_bool;
      max_redemptions = json |> member "max_redemptions" |> to_int_option;
      times_redeemed = json |> member "times_redeemed" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** InvoiceItem resource *)
module Invoice_item = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    currency : string;
    customer : string;
    date : int;
    description : string option;
    discountable : bool;
    invoice : string option;
    livemode : bool;
    period_start : int;
    period_end : int;
    price : Price.t option;
    proration : bool;
    quantity : int;
    subscription : string option;
    unit_amount : int option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    let period = json |> member "period" in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      date = json |> member "date" |> to_int;
      description = json |> member "description" |> to_string_option;
      discountable = json |> member "discountable" |> to_bool;
      invoice = json |> member "invoice" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      period_start = period |> member "start" |> to_int;
      period_end = period |> member "end" |> to_int;
      price = (match json |> member "price" with
        | `Null -> None
        | p -> Some (Price.of_json p));
      proration = json |> member "proration" |> to_bool;
      quantity = json |> member "quantity" |> to_int;
      subscription = json |> member "subscription" |> to_string_option;
      unit_amount = json |> member "unit_amount" |> to_int_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Quote resource *)
module Quote = struct
  type t = {
    id : string;
    object_ : string;
    amount_subtotal : int;
    amount_total : int;
    created : int;
    currency : string option;
    customer : string option;
    description : string option;
    expires_at : int;
    livemode : bool;
    status : string;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_subtotal = json |> member "amount_subtotal" |> to_int;
      amount_total = json |> member "amount_total" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      expires_at = json |> member "expires_at" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** CreditNote resource *)
module Credit_note = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    customer : string;
    invoice : string;
    livemode : bool;
    memo : string option;
    number : string;
    out_of_band_amount : int option;
    reason : string option;
    refund : string option;
    status : string;
    subtotal : int;
    total : int;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      invoice = json |> member "invoice" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      memo = json |> member "memo" |> to_string_option;
      number = json |> member "number" |> to_string;
      out_of_band_amount = json |> member "out_of_band_amount" |> to_int_option;
      reason = json |> member "reason" |> to_string_option;
      refund = json |> member "refund" |> to_string_option;
      status = json |> member "status" |> to_string;
      subtotal = json |> member "subtotal" |> to_int;
      total = json |> member "total" |> to_int;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** ApplicationFee resource *)
module Application_fee = struct
  type t = {
    id : string;
    object_ : string;
    account : string;
    amount : int;
    amount_refunded : int;
    application : string;
    charge : string;
    created : int;
    currency : string;
    livemode : bool;
    refunded : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      account = json |> member "account" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_refunded = json |> member "amount_refunded" |> to_int;
      application = json |> member "application" |> to_string;
      charge = json |> member "charge" |> to_string;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      refunded = json |> member "refunded" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Topup resource *)
module Topup = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    description : string option;
    livemode : bool;
    source : string option;
    status : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      source = json |> member "source" |> to_string_option;
      status = json |> member "status" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** UsageRecord resource (for metered billing) *)
module Usage_record = struct
  type t = {
    id : string;
    object_ : string;
    livemode : bool;
    quantity : int;
    subscription_item : string;
    timestamp : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      quantity = json |> member "quantity" |> to_int;
      subscription_item = json |> member "subscription_item" |> to_string;
      timestamp = json |> member "timestamp" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** SubscriptionItem resource *)
module Subscription_item = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    price : Price.t;
    quantity : int option;
    subscription : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      price = json |> member "price" |> Price.of_json;
      quantity = json |> member "quantity" |> to_int_option;
      subscription = json |> member "subscription" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** SubscriptionSchedule resource *)
module Subscription_schedule = struct
  type t = {
    id : string;
    object_ : string;
    canceled_at : int option;
    completed_at : int option;
    created : int;
    customer : string;
    end_behavior : string;
    livemode : bool;
    status : string;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      canceled_at = json |> member "canceled_at" |> to_int_option;
      completed_at = json |> member "completed_at" |> to_int_option;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string;
      end_behavior = json |> member "end_behavior" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** List response wrapper *)
module List_response = struct
  type 'a t = {
    data : 'a list;
    has_more : bool;
    url : string;
  }

  let of_json parse_item json =
    let open Yojson.Safe.Util in
    {
      data = json |> member "data" |> to_list |> List.map parse_item;
      has_more = json |> member "has_more" |> to_bool;
      url = json |> member "url" |> to_string;
    }
end

(** Deleted response wrapper *)
module Deleted = struct
  type t = {
    id : string;
    object_ : string;
    deleted : bool;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      deleted = json |> member "deleted" |> to_bool;
    }
end

(** Billing Portal Session resource *)
module Billing_portal_session = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    customer : string;
    livemode : bool;
    return_url : string;
    url : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      return_url = json |> member "return_url" |> to_string;
      url = json |> member "url" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Customer Balance Transaction resource *)
module Customer_balance_transaction = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    customer : string;
    description : string option;
    ending_balance : int;
    livemode : bool;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      description = json |> member "description" |> to_string_option;
      ending_balance = json |> member "ending_balance" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end
