text <- list(
  start_game = list(English = "Start game", Polish = "Rozpocznij grę"),
  end_game = list(English = "End game", Polish = "Zakończ grę"),
  n_ais = list(English = "Number of AIs", Polish = "Liczba komputerowych graczy"),
  player_names = list(
    English = c("You", "AI One", "AI Two", "AI Three", "AI Four"), 
    Polish = c("Ty", "SI Jeden", "SI Dwa", "SI Trzy", "SI Cztery")
  ),
  instructions = list(
    title = list(English = "Instructions", Polish = "Instrukcje"),
    text = list(
      English = "You are playing Blef - a Polish card game inspired by poker. 
      \nAt the beginning of each round, each player gets a certain number of cards. The player who begins makes a bet (for example: \"pair of aces\"). By doing that, he / she states that if we pool all players' cards, we will find a given combination of cards (here: at least two aces). The next player can either make a bigger bet (state a more senior combination of cards) or check. 
      \nWhen player Y checks player X, everybody reveals their cards. If X's bet was right (X's stated combination exists across everybody's pooled cards), Y loses the round. If X's bet isn't right, X loses the round.
      \nThe player who loses gets one more card in the next round. When a player reaches a certain number of cards, they lose the game completely.
      \n\"Small\" means 9 thru K 
      \"Big\" means 10 thru A
      \"Great\" means 9 thru A",
      Polish = "Blef - gra karciana inspirowana pokerem. 
      \nNa początku każdej rundy, każdy gracz dostaje określoną ilość kart. Gracz, który rozpoczyna rundę, zgłasza układ (np. \"para asów\"). Robiąc to, deklaruje, że, jeśli zgromadzimy karty wszystkich gracy, znajdziemy w nich pewną kombinację kart (w tym przypadku: co najmniej dwa asy). Następny gracz może zgłosić wyższy układ lub sprawdzić.
      \nKiedy gracz Y sprawdza gracza X, wszyscy ujawniają swoje karty. Jeśli zgłoszenie X było poprawne (układ zgłoszony przez X występuje wśród zgromadzonych kart wszystkich graczy), Y przegrywa rundę. Jeśli zgłoszenie X nie było poprawne, X przegrywa rundę.
      \nGracz, który przegrywa rundę, dostaje jedną kartę więcej w następnej rundzie. Kiedy gracz osiąga określoną liczbę kart, ostatecznie przegrywa grę.
      \n\"Mały\" oznacza od 9 do K
      \"Duży\" oznacza od 10 do A
      \"Wielki\" oznacza od 9 do A"
    )
  ),
  cards_in_play = list(English = "Cards in play", Polish = "Karty w grze"),
  your_cards = list(English = "Your cards are the following:<br/>", Polish = "Masz następujące karty:<br/>"),
  players_cards = list(English = "<br/>Players have the following number of cards:<br/>", Polish = "<br/>Gracze mają następującą liczbę kart:<br/>"),
  your_move = list(English = "Your move", Polish = "Twój ruch"),
  hand_type = list(English = "Hand type", Polish = "Typ układu"),
  hand_types = list(
    English =  c(
      "High card", "Pair", "Two pairs", "Small straight", "Big straight", "Great straight", "Three of a kind",
      "Full house", "Colour", "Four of a kind", "Small flush", "Big flush", "Great flush"
    ),
    Polish =  c(
      "Wysoka karta", "Para", "Dwie pary", "Mały strit", "Duży strit", "Wielki strit", "Trójka",
      "Full house", "Kolor", "Kareta", "Mały poker", "Duży poker", "Wielki poker"
    )
  ),
  card_value = list(English = "Card value", Polish = "Wartość karty"),
  higher_pair_value = list(English = "Higher pair value", Polish = "Wartość wyższej pary"),
  lower_pair_value = list(English = "Lower pair value", Polish = "Wartość niższej pary"),
  three_cards_value = list(English = "Value of 3 cards", Polish = "Wartość 3 kart"),
  two_cards_value = list(English = "Value of 2 cards", Polish = "Wartość 2 kart"),
  colour = list(English = "Colour", Polish = "Kolor"),
  confirm_hand = list(English = "Submit", Polish = "Zgłoś"),
  check = list(English = "Check", Polish = "Sprawdź"),
  impossible_move = list(
    title = list(English = "Not possible", Polish = "Niedozwolony ruch"),
    text = list(
      English = "This bet is not higher than the last one. \n\nRemember that if you bet two pairs, the first one needs to be higher than the second one.",
      Polish = "Ten układ nie jest wyższy niż ostatni zgłoszony układ. \n\nPamiętaj, że jeśli zgłaszasz dwie pary, pierwsza z nich musi być większa."
    )
  ),
  history = list(English = "History", Polish = "Historia"),
  hand_exists_true = list(English = "Hand exists -", Polish = "Układ istnieje - "),
  hand_exists_false = list(English = "Hand does not exist -", Polish = "Układ nie istnieje - "),
  human_gets_card = list(English = "you get a card", Polish = "dostajesz kartę"),
  ai_gets_card = list(English = "gets a card", Polish = "dostaje kartę"),
  drops = list(English = "drops", Polish = "odpada"),
  human_win_true = list(English = "you won the game!", Polish = "wygrałeś/aś!"),
  human_win_false = list(English = "you lost the game...", Polish = "przegrałeś/aś..."),
  player_checked = list(English = "checked", Polish = "sprawdziłeś/aś"),
  ai_checked = list(English = "checked", Polish = "sprawdziła"),
  player_bet = list(English = "submitted: ", Polish = "zgłosiłeś/aś: "),
  ai_bet = list(English = "submitted: ", Polish = "zgłosiła: "),
  the_cards_were = list(English = "The cards were the following:", Polish = "Karty były następujące:")
)

