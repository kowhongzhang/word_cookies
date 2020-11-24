module type Generator = sig 
  val create_level: Game.t -> Game.t
end 

module type EasterEggs = sig 
  val word: unit -> unit
  val cookies: unit -> unit
  val word_cookies: unit -> unit
end


module Level_Generator : Generator = struct
  (** [print_greeting ()] prints the greeting that the player sees upon 
      entering the level generator. *)
  let print_greeting () = 
    print_string("Welcome to the Level Generator.
      If you give us the letters, words, and bonus word of a level.
      Then we can make a game level from it that you can play.\n")
  
  (** [is_upper_alpha char] is true if [char] is an uppercase letter. *)
  let is_upper_alpha char =
    let uppers = ['A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';
    'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'] in 
    List.mem char uppers


  (** [alpha ltrs] is true if every character in [ltrs] is alphabetical. *)
  let alpha ltrs = 
    let ltrs_list = String.to_seq ltrs in 
    ltrs_list |> (Seq.map is_upper_alpha) |> Seq.fold_left (&&) true
  
  (** [ask_letters ()] takes in the users input and checks that it is a valid
      string to use as letters for a level*)
  let rec ask_letters () = 
    print_string("Enter the letters for the level: ");
    let ltrs = String.uppercase_ascii (read_line ()) in 
    if (alpha ltrs) then ltrs 
    else (print_endline("Those are not letters, please try again."); ask_letters ())
  
  (** [ask_words ()] takes in the users input and checks that it is a valid
      string of words to use for a level*)
  let rec ask_words () = 
  print_string("Enter the words for the level, separated with a space.
  Do not enter the bonus word here.");
  let wrds = (read_line ()) |> String.uppercase_ascii |> String.split_on_char ' ' in 
   if wrds |> List.map alpha |> List.fold_left (&&) true then wrds 
   else (print_endline("Something went wrong, please try again."); ask_words ())
  
  (** [ask_bonus ()] takes in the users input and checks that it is a valid
      string to use as the bonus word for a level*)
  let rec ask_bonus () = 
  print_string("Enter the bonus word for the level: ");
  let bonus = String.uppercase_ascii (read_line ()) in 
    if (alpha bonus) then bonus 
    else (print_endline("Those are not letters, please try again."); ask_bonus ())
  (** [compare_lengths] is 1 if length of [s1] is greater than [s2], 0 if equal,
    -1 if otherwise. *)
  let compare_lengths s1 s2 = 
    Int.compare (String.length s1) (String.length s2)

  let sort_string s = 
    s |> String.to_seq |> List.of_seq |> List.sort compare |>
    List.to_seq |> String.of_seq

  (** [check_all ltrs words bonus] is true if the combination of the [ltrs],
      [words], and [bonus] can be used for a level. I.e No word uses letters
      not found in [ltrs], and no letter in [ltrs] is not used*)
  let check_all ltrs words bonus = 
    let all_words = bonus::words in 
    let longest = all_words |> List.sort compare_lengths |> List.rev |> List.hd in
    sort_string longest = sort_string ltrs

  let rec create_level game =
    print_greeting ();
    let ltrs = ask_letters () in 
    let words = ask_words () in 
    let bonus = ask_bonus () in 
    if check_all ltrs words bonus then Game.add_level (ltrs) words bonus game
    else (print_string("It looks like your letters don't match your words.
    Please Try Again.\n"); create_level game)
end 


module Easter_Eggs : EasterEggs = struct

  (** The chocolate-chip cookie recipe found on tasty.com *)
  let recipe = 
    "Ingredients(for 30 cookies):
      2 cups butter, melted
      2 cups brown sugar, packed
      2 cups granulated sugar
      4 large eggs
      1 tablespoon vanilla extract
      5 cups all-purpose flour
      2 teaspoons baking powder
      4 cups chocolate chips
    Preparation
      Preheat oven to 375°F (190°C).
      In a large bowl, whisk together the brown sugar, granulated sugar,
      and melted butter, until evenly combined and light in color.
      Add in the eggs and vanilla, mixing until smooth.
      Add the flour and baking powder, folding the mixture until it forms a smooth dough.
      Fold in the chocolate chips until evenly combined.
      Using an ice cream scoop, scoop 6 balls of dough onto a baking tray lined with parchment paper.
      Bake for 12 minutes, then serve!
    Enjoy!\n"
  
  (** The List of word that the random word generator chooses from. *)
  let words = [
    "Adult";"Aeroplane";"Air";"Aircraft Carrier";"Airforce";"Airport";"Album";
    "Alphabet";"Apple";"Arm";"Army";"Baby";"Backpack";"Balloon";"Banana";"Bank";
    "Barbecue";"Bathroom";"Bathtub";"Bed";"Bee";"Borneo";"Bird";"Bomb";"Book";
    "Boss";"Bottle";"Bowl";"Box";"Boy";"Brain";"Bridge";"Butterfly";"Button";
    "Cappuccino";"Car";"Carpet";"Carrot";"Cave";"Chair";"Chess";"Chief";"Child";
    "Chisel";"Chocolate";"Churro";"Circle";"Circus";"Clock";"Clown";"Coffee";
    "Comet";"Compact Disc";"Compass";"Computer";"Crystal";"Cup";"Cycle";
    "Database";"Desk";"Diamond";"Dress";"Drill";"Drink";"Drum";"Dung";"Ears";
    "Earth";"Egg";"Electricity";"Elephant";"Eraser";"Explosive";"Eyes";"Family";
    "Fan";"Feather";"Festival";"Film";"Finger";"Fire";"Floodlight";"Flower";
    "Foot";"Fork";"Freeway";"Fruit";"Fungus";"Game";"Garden";"Gas";"Gate";
    "Gemstone";"Girl";"Gloves";"Grapes";"Guitar";"Hammer";"Hat";"Hieroglyph";
    "Highway";"Horoscope";"Horse";"Hose";"Ice";"Ice-cream";"Insect";"Jet";"Junk";
    "Kaleidoscope";"Kitchen";"Knife";"Leather";"Leg";"Library";"Liquid";"Magnet";
    "Man";"Map";"Maze";"Meat";"Meteor";"Microscope";"Milk";"Milkshake";"Mist";
    "Money";"Monster";"Mosquito";"Mouth";"Nail";"Navy";"Necklace";"Needle";
    "Onion";"Paintbrush";"Pants";"Parachute";"Passport";"Pebble";"Pendulum";
    "Pepper";"Perfume";"Pillow";"Plane";"Planet";"Pocket";"Post-office";"Potato";
    "Printer";"Prison";"Pyramid";"Quarantine";"Radar";"Rainbow";"Record";
    "Restaurant";"Rifle";"Ring";"Robot";"Rock";"Rocket";"Roof";"Room";"Rope";
    "Saddle";"Salt";"Sandpaper";"Sandwich";"Satellite";"School";"Ship";"Shoes";
    "Shop";"Shower";"Signature";"Skeleton";"Snail";"Software";"Solid";"Space";
    "Spectrum";"Sphere";"Spice";"Spiral";"Spoon";"Sports";"Spot";"Square";
    "Staircase";"Star";"Stomach";"Sun";"Sunglasses";"Surveyor";"Swimming";
    "Sword";"Table";"Tapestry";"Teeth";"Telescope";"Television";"Tennis";
    "Thermometer";"Tiger";"Toilet";"Tongue";"Torch";"Torpedo";"Train";
    "Treadmill";"Triangle";"Tunnel";"Typewriter";"Umbrella";"Vacuum";"Vampire";
    "Videotape";"Vulture";"Water";"Weapon";"Web";"Wheelchair";"Window";"Woman";
    "Worm";"X-ray"
  ]

  let cookies () = print_string(recipe)
  
  let word () =
    let n = Random.int (List.length words) in
    print_endline(List.nth words n)
  
  let word_cookies () = 
    let recipe_list = recipe |> String.split_on_char ' ' in 
    let n = Random.int (List.length recipe_list) in
      print_endline(List.nth recipe_list n)
end
