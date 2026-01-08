type model = int

type msg =
  | Question
  | Oui
  | Non
  | LocationChanged of Tea_navigation.Location.t
[@@deriving accessors]

let msg_to_string (msg : msg) =
  match msg with
  | Question ->
      "Question"
  | Oui ->
      "Oui"
  | Non ->
      "Non"
  | LocationChanged _location ->
      "Location changed"


let update model = function
  | Question ->
      (0 , Tea.Cmd.none)
  | Oui ->
      (1, Tea.Cmd.none)
  | Non ->
      (2, Tea.Cmd.none)
  | LocationChanged _location ->
      (model, Tea.Cmd.none)

let init () _location = (0, Tea.Cmd.none)
let view_button button_text msg =
  let open Tea.Html in
  button [Events.onClick msg]  [text button_text]

let view model =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  let response =
  match model with
  |0 -> ""
  |1 -> "Miaou"
  |2 -> "Dommage, bon courage !"
  | _ -> "Prends ton temps..."
in
  div [class' "h-screen bg-background-lavender m-auto flex flex-col justify-center"] 
    [ span [class' "flex justify-center m-auto text-center text-primary-plum"] [text "Es-tu un chat ?"]
    ; br []
    ; span [class' "flex justify-center m-auto text-center bg-primary-plum text-background-lavender p-5 border-1 rounded-xl"] [view_button "Oui" Oui]
    ; span [class' "flex justify-center m-auto text-center bg-primary-plum text-background-lavender p-5 border-1 rounded-xl"] [view_button "Non" Non]
    ; br []
    ; span [class' "flex justify-center m-auto text-center text-primary-plum"] [text response]
]
let subscriptions _model = Tea.Sub.none

let shutdown _model = Tea.Cmd.none

let start_app container =
  Tea.Navigation.navigationProgram locationChanged
    {init; update; view; subscriptions; shutdown}
    container ()

let start_debug_app ?(init = init) ?(shutdown = shutdown) container =
  Tea.Debug.navigationProgram locationChanged
    {init; update; view; subscriptions; shutdown}
    msg_to_string container ()

let start_hot_debug_app container cachedModel =
  (* Replace the existing shutdown function with one that returns the current
   * state of the app, for hot module replacement purposes *)
  (* inspired by https://github.com/walfie/ac-tune-maker *)
  let modelRef = ref None in
  let shutdown model =
    modelRef := Some model ;
    Tea.Cmd.none
  in
  let init =
    match cachedModel with
    | None ->
        init
    | Some model ->
        fun flags location ->
          let _model, cmd = init flags location in
          (model, cmd)
  in
  let app = start_debug_app ~init ~shutdown container in
  let oldShutdown = app##shutdown in
  let newShutdown () = oldShutdown () ; !modelRef in
  let _ = Js.Obj.assign app [%obj {shutdown= newShutdown}] in
  newShutdown
