type model = int

type msg =
  | FirstQuestion
  | SecondQuestion
  | Oui 
  | Non
  | LocationChanged of Tea_navigation.Location.t
[@@deriving accessors]

let msg_to_string (msg : msg) =
  match msg with
  | FirstQuestion ->
      "Question"
  | SecondQuestion ->
      "Second Question"
  | Oui ->
      "Oui"
  | Non ->
      "Non"
  | LocationChanged _location ->
      "Location changed"


let update model = function
  | FirstQuestion ->
      (0 , Tea.Cmd.none)
  | Oui ->
      (1, Tea.Cmd.none)
  | Non ->
      (2, Tea.Cmd.none)
  | SecondQuestion ->
      (0, Tea.Cmd.none)
  | LocationChanged _location ->
      (model, Tea.Cmd.none)

let init () _location = (0, Tea.Cmd.none)
let view_button model button_text msg =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  let isSelected =
    match model with
    | 1 -> "w-full text-xl text-center font-body text-primary-plum bg-background-lavender border-1 rounded-xl"
    | 2 -> "w-full text-xl text-center font-body text-primary-plum bg-background-lavender border-1 rounded-xl"
    | _ -> ""
  in
  button [Events.onClick msg; class' isSelected]  [text button_text]

let view model =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  let response =
  match model with
  |0 -> ""
  |1 -> "Miaou"
  |2 -> "Dommage, bon courage !"
  | _-> ""
  (* |3 -> "à table ! la patée est servie." *)
in
  div [class' "h-full bg-background-lavender m-auto"] 
    [ 
      div [class' "flex flex-col justify-center"] [ 
        span [class' "p-20 text-4xl text-center text-primary-plum font-display font-bold"] [text "Es-tu un chat ?"]
        ; img [src "/logo.png"; alt "cat"; class' "w-1/3 m-auto"] []
        ; p  [] []
        ];
      div [class' "flex flex-row justify-center gap-10 "] [
        span [class' " w-1/4 text-xl text-center font-body bg-primary-plum text-background-lavender border-1 rounded-xl"] [view_button model "Oui" Oui]
      ; span [class' " w-1/4 text-xl text-center font-body bg-primary-plum text-background-lavender border-1 rounded-xl"] [view_button model "Non" Non]
      ; p [] []
      ];
      div [class' "flex flex-col justify-center"] [
     span [class' "p-10 text-4xl text-center text-primary-plum font-display"] [text response]
      ]
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
