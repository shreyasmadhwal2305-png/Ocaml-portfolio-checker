(* 1. TYPES *)
type sector =
  | Tech | Defence | Energy | Finance | RealEstate | PSU

type company = {
  cname : string;
  sector : sector;
  price : int;
}

type horizon = ShortTerm | LongTerm

type risk = Low | Medium | High

type user = {
  name : string;
  pin : int;
}

(* 2. MARKET MOCK DATA *)
let companies = [
  { cname = "TCS"; sector = Tech; price = 3500 };
  { cname = "HAL"; sector = Defence; price = 3000 };
  { cname = "ONGC"; sector = Energy; price = 250 };
  { cname = "SBI"; sector = Finance; price = 600 };
  { cname = "DLF"; sector = RealEstate; price = 750 };
  { cname = "NTPC"; sector = PSU; price = 300 };
]

(* 3. INPUT VALIDATION *)
let minimum_investment = 500

let validate_amount amount =
  if amount < minimum_investment then None
  else Some amount

(* 4. SECTOR PREFERENCES *)
let preferred_sectors horizon =
  match horizon with
  | ShortTerm -> [Tech; Finance]
  | LongTerm -> [PSU; Energy; Defence]

(* 5. CORE FILTERING LOGIC *)
let companies_by_sector sectors =
  List.filter (fun c -> List.mem c.sector sectors) companies

let affordable_companies amount comps =
  List.filter (fun c -> c.price <= amount) comps

let suggest_companies amount horizon =
  preferred_sectors horizon
  |> companies_by_sector
  |> affordable_companies amount

(* 6. ENSURE MINIMUM DIVERSIFICATION *)
let ensure_minimum_stocks amount horizon =
  let primary = suggest_companies amount horizon in
  if List.length primary >= 10 then
    primary
  else
    affordable_companies amount companies

(* 7. MONEY ALLOCATION *)
let allocate_evenly amount stocks =
  match stocks with
  | [] -> []
  | _ ->
      let per_stock = amount / List.length stocks in
      List.map (fun c -> (c.cname, per_stock)) stocks

(* 8. RISK SCORING *)
let sector_risk = function
  | PSU -> Low
  | Energy -> Medium
  | Defence -> Medium
  | Finance -> Medium
  | Tech -> High
  | RealEstate -> High

let company_risk c =
  sector_risk c.sector

let portfolio_risk stocks =
  let risks = List.map company_risk stocks in
  if List.mem High risks then High
  else if List.mem Medium risks then Medium
  else Low

(* 9. SECTOR BREAKDOWN *)
let sector_breakdown stocks =
  let update acc sector =
    let prev =
      match List.assoc_opt sector acc with
      | None -> 0
      | Some v -> v
    in
    (sector, prev + 1) :: List.remove_assoc sector acc
  in
  List.fold_left (fun acc c -> update acc c.sector) [] stocks

(* 10. FINAL PORTFOLIO BUILDER *)
let build_portfolio amount horizon =
  match validate_amount amount with
  | None ->
      Error "Insufficient amount to build a diversified portfolio"
  | Some valid_amount ->
      let stocks = ensure_minimum_stocks valid_amount horizon in
      let allocation = allocate_evenly valid_amount stocks in
      let risk = portfolio_risk stocks in
      let sectors = sector_breakdown stocks in
      Ok (allocation, risk, sectors)

(* 11. USER INPUT HELPERS (UX LAYER) *)
let rec read_pin () =
  let pin = read_int () in
  if pin >= 1000 && pin <= 9999 then pin
  else (
    print_endline "Pin must be exactly 4 digits. Try again:";
    read_pin ()
  )

let get_user () =
  print_string "Your name: ";
  let name = read_line () in

  print_string "Your 4-digit code: ";
  let pin = read_pin () in

  { name; pin }

let rec get_amount () =
  print_string "Amount to invest: ";
  let amt = read_int () in
  if amt >= minimum_investment then amt
  else (
    print_endline "Insufficient amount. Minimum is 500.";
    get_amount ()
  )

let rec get_horizon () =
  print_string "Investment horizon (short/long): ";
  match String.lowercase_ascii (read_line ()) with
  | "short" -> ShortTerm
  | "long" -> LongTerm
  | _ ->
      print_endline "Please type 'short' or 'long'";
      get_horizon ()

(* 12. OUTPUT HELPERS *)
let string_of_risk = function
  | Low -> "Low"
  | Medium -> "Medium"
  | High -> "High"

let string_of_sector = function
  | Tech -> "Tech"
  | Defence -> "Defence"
  | Energy -> "Energy"
  | Finance -> "Finance"
  | RealEstate -> "Real Estate"
  | PSU -> "PSU"

(* 13. MAIN PROGRAM (USER FRIENDLY ENTRY POINT) *)
let run_portfolio_checker () =
  let user = get_user () in
  let amount = get_amount () in
  let horizon = get_horizon () in

  match build_portfolio amount horizon with
  | Error msg ->
      print_endline msg
  | Ok (allocation, risk, sectors) ->
      Printf.printf "\nHello %s 👋\n" user.name;
      Printf.printf "Portfolio Risk: %s\n\n" (string_of_risk risk);

      print_endline "Stock Allocation:";
      List.iter
        (fun (name, amt) ->
          Printf.printf " - %s : ₹%d\n" name amt)
        allocation;

      print_endline "\nSector Breakdown:";
      List.iter
        (fun (s, count) ->
          Printf.printf " - %s : %d stocks\n"
            (string_of_sector s) count)
        sectors