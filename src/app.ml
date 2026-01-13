type model = {is_a_cat: bool option; is_manchas: bool option}

type msg =
  | AnswerCat
  | AnswerNotCat
  | AnswerManchas
  | AnswerNotManchas
  | LocationChanged of Tea_navigation.Location.t
[@@deriving accessors]

let msg_to_string (_msg : msg) = "foo"

let update model = function
  | AnswerCat ->
      ({is_a_cat= Some true; is_manchas= model.is_manchas}, Tea.Cmd.none)
  | AnswerNotCat ->
      ({is_a_cat= Some false; is_manchas= model.is_manchas}, Tea.Cmd.none)
  | AnswerManchas ->
      ({is_a_cat= model.is_a_cat; is_manchas= Some true}, Tea.Cmd.none)
  | AnswerNotManchas ->
      ({is_a_cat= model.is_a_cat; is_manchas= Some false}, Tea.Cmd.none)
  | LocationChanged _location ->
      (model, Tea.Cmd.none)

let init () _location = ({is_a_cat= None; is_manchas= None}, Tea.Cmd.none)

let view_button ~selected button_text msg =
  let open Tea.Html in
  let colors =
    if selected then "text-primary-plum bg-background-lavender"
    else "bg-primary-plum text-background-lavender"
  in
  let open Tea.Html.Attributes in
  button
    [ Events.onClick msg
    ; class'
        ("w-1/4 text-xl text-center font-body border-1 rounded-xl " ^ colors) ]
    [text button_text]

let answer_view model =
  let open Tea.Html in
  (* let open Tea.Html.Attributes in *)
  match model.is_a_cat with
  | None ->
      div [] [text ""]
  | Some true ->
      div [] [text "Miaou"]
  | Some false ->
      div [] [text "Dommage, bon courage !"]

let answer_view_manchas model =
  let open Tea.Html in
  match model.is_a_cat with
  | Some true ->
      (match model.is_manchas with
      | None -> div [] []
      | Some true -> div [] [text "Le repas est servi !"]
      | Some false -> div [] [text "Dommage, bon courage !"]
      )
  | _ -> div [] []

let manchas_view_question model =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  match model.is_manchas with
  | None ->
      div
        [class' "h-full bg-background-lavender m-auto"]
        [ div
            [class' "flex flex-col justify-center"]
            [p [class' "h-30"] []
            ; span
                [ class'
                    "p-20 text-4xl text-center text-primary-plum font-display \
                     font-bold" ]
                [text "Es-tu Manchas ?"]
            ; img [src "/logo.png"; alt "cat"; class' "w-1/3 m-auto"] []
            ; p [] [] ]
        ; div
            [class' "flex flex-row justify-center gap-10 "]
            [ view_button
                ~selected:(model.is_manchas = Some true)
                "Oui" AnswerManchas
            ; view_button
                ~selected:(model.is_manchas = Some false)
                "Non" AnswerNotManchas ] ]
  | _ ->
      div [] []

let cat_view_question model =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  match model.is_a_cat with
  | None ->
      div
        [class' "h-full bg-background-lavender m-auto"]
        [ div
            [class' "flex flex-col justify-center"]
            [ p [class' "h-30"] []
            ; span
                [ class'
                    "p-20 text-4xl text-center text-primary-plum font-display \
                     font-bold" ]
                [text "Es-tu un chat ?"]
            ; p [] [] ]
        ; div
            [class' "flex flex-row justify-center gap-10 "]
            [ view_button ~selected:(model.is_a_cat = Some true) "Oui" AnswerCat
            ; view_button
                ~selected:(model.is_a_cat = Some false)
                "Non" AnswerNotCat ] ]
  | _ ->
      div [] [] (* Déjà répondu, ne montre plus la question *)

let view model =
  let open Tea.Html in
  let open Tea.Html.Attributes in
  div [class' "h-screen bg-background-lavender"]
    [ ( match model.is_a_cat with
      | None ->
          cat_view_question model
      | Some true -> (
        (* Si c’est un chat, on pose la question "Es-tu Manchas ?" *)
        match model.is_manchas with
        | None ->
            manchas_view_question model
        | Some _ ->
            div []
              [ div
                  [ class'
                      "flex flex-col justify-center text-center p-20 text-4xl \
                       text-primary-plum font-bold" ]
                  [answer_view model]
              ; div
                  [ class'
                      "flex flex-col justify-center text-center p-20 text-4xl \
                       text-primary-plum font-bold" ]
                  [answer_view_manchas model] ] )
      | Some false ->
          (* Si ce n’est pas un chat, on ne pose pas la question Manchas *)
          div [class' "flex justify-center "]
            [ 
            p [class' "h-100"] []
               ; div
                [ class'
                    "flex flex-col justify-center self-center text-center p-20 text-4xl \
                     text-primary-plum font-bold" ]
                [answer_view model] ] ) ]

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
