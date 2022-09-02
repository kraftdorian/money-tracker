module Main exposing (..)

import Browser
import Html exposing (Html, button, main_, h1, h2, div, ul, li, p, input, select, option, text)
import Html.Attributes exposing (type_, step, value, selected, disabled)
import Html.Events exposing (onInput, onClick)

main =
    Browser.sandbox
      { init = init
      , update = update
      , view = view
      }


-- MODEL

profit : String
profit = "Profit"

expense : String
expense = "Expense"

type AccountBalanceChangeKind
 = Profit
 | Expense

type alias AccountBalanceChangeHistoryEntry =
  { id: Int
  , amount: Int
  , kind: AccountBalanceChangeKind
  }

balanceKindToString : AccountBalanceChangeKind -> String
balanceKindToString kind =
  case kind of
    Profit -> profit
    Expense -> expense

balanceKindFromString : String -> Maybe AccountBalanceChangeKind
balanceKindFromString str =
  if str == profit then
    Just Profit
  else if str == expense then
    Just Expense
  else
    Nothing

calcBalance : List AccountBalanceChangeHistoryEntry -> Int
calcBalance history =
  List.foldl (\balance amount -> balance + amount) 0 (List.map calcAmount history)

calcAmount : AccountBalanceChangeHistoryEntry -> Int
calcAmount entry =
  case entry.kind of
    Profit -> entry.amount
    Expense -> negate entry.amount

moneyToInt : Float -> Int
moneyToInt val = truncate (val * 100)

moneyFromInt : Int -> Float
moneyFromInt val = (toFloat val) / 100

type alias Model =
  { amount: Float
  , balanceChangeKind: AccountBalanceChangeKind
  , history: List AccountBalanceChangeHistoryEntry
  }

init : Model
init =
  { amount = 0
  , balanceChangeKind = Profit
  , history = []
  }


-- UPDATE

type Msg
  = AmountInput String
  | BalanceKindInput String
  | BalanceChangeSubmit
  | BalanceChangeEntryDelete Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    AmountInput val ->
      case String.toFloat val of
        Just newAmount -> { model | amount = newAmount }
        Nothing -> model
    BalanceKindInput val ->
      case balanceKindFromString val of
        Just newKind -> { model | balanceChangeKind = newKind }
        Nothing -> model
    BalanceChangeSubmit ->
      { init
      | history
        = List.append
          model.history
          [ AccountBalanceChangeHistoryEntry
            ( case List.maximum (List.map (\entry -> entry.id) model.history) of
              Just lastId -> lastId + 1
              Nothing -> 1
            )
            (moneyToInt model.amount)
            model.balanceChangeKind
            ]
      }
    BalanceChangeEntryDelete entryId ->
      { model
      | history
        = List.filter (\entry -> entry.id /= entryId) model.history
      }


-- VIEW

entryIdText : AccountBalanceChangeHistoryEntry -> String
entryIdText entry =
  "Entry #" ++ (String.fromInt entry.id)

historyEntryListItem : AccountBalanceChangeHistoryEntry -> Html Msg
historyEntryListItem entry =
  li []
    [ text ((entryIdText entry) ++ " | ")
    , text (balanceKindToString entry.kind)
    , text ": "
    , text (String.fromFloat (moneyFromInt entry.amount))
    , button [ onClick (BalanceChangeEntryDelete entry.id) ]
      [ text ("Delete " ++ (entryIdText entry))
      ]
    ]

view : Model -> Html Msg
view model =
  main_ []
    [ h1 [] [ text "Money Tracker" ]
    , input
      [ type_ "number"
      , step (String.fromFloat 0.01)
      , value (String.fromFloat model.amount)
      , onInput AmountInput
      ] []
    , select [ onInput BalanceKindInput ]
      [ option
        [ value (balanceKindToString Profit)
        , selected (model.balanceChangeKind == Profit)
        ] [ text (balanceKindToString Profit) ]
      , option
        [ value (balanceKindToString Expense)
        , selected (model.balanceChangeKind == Expense)
        ] [ text (balanceKindToString Expense) ]
      ]
    , button [ onClick BalanceChangeSubmit, disabled (model.amount <= 0)  ]
      [ text ("Add " ++ (balanceKindToString model.balanceChangeKind))
      ]
    , div []
      [ h2 [] [ text "Your account balance" ]
      , p [] [ text (String.fromFloat (moneyFromInt (calcBalance model.history))) ]
      ]
    , div []
      [ h2 [] [ text "History" ]
      , ul []
        ( List.map historyEntryListItem model.history)
      ]
    ]