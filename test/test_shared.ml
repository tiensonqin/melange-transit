module type Json = sig
  type mode =
    | Normal
    | Verbose

  type value =
    | Null
    | Bool of bool
    | String of string
    | Int of int
    | Int64 of int64
    | Float of float
    | Binary of string
    | Keyword of string
    | Symbol of string
    | Big_decimal of string
    | Big_int of string
    | Date of int64
    | Uuid of string
    | Uri of string
    | Array of value list
    | Map of (value * value) list
    | Set of value list
    | List of value list
    | Tagged of string * value

  exception Decode_error of string

  val to_string : ?mode:mode -> value -> string
  val of_string : string -> value
end

module Make (Json : Json) = struct
  open Json

  let random_constructors =
    Random_cases.
      {
        null = Null;
        bool = (fun value -> Bool value);
        string = (fun value -> String value);
        int = (fun value -> Int value);
        int64 = (fun value -> Int64 value);
        float = (fun value -> Float value);
        binary = (fun value -> Binary value);
        keyword = (fun value -> Keyword value);
        symbol = (fun value -> Symbol value);
        big_decimal = (fun value -> Big_decimal value);
        big_int = (fun value -> Big_int value);
        date = (fun value -> Date value);
        uuid = (fun value -> Uuid value);
        uri = (fun value -> Uri value);
        array = (fun values -> Array values);
        map = (fun entries -> Map entries);
        set = (fun values -> Set values);
        list_ = (fun values -> List values);
        tagged = (fun (tag, value) -> Tagged (tag, value));
      }

  let fail message = failwith message

  let check_string name expected actual =
    if not (String.equal expected actual) then
      fail
        (Printf.sprintf "%s: expected %S, got %S" name expected actual)

  let check_value name expected actual =
    if not (expected = actual) then
      fail
        (Printf.sprintf "%s: decoded value did not match expected value" name)

  let check_float name expected = function
    | Float actual when Float.equal expected actual -> ()
    | _ -> fail (Printf.sprintf "%s: expected float %.17g" name expected)

  let check_int64 name expected = function
    | Int64 actual when Int64.equal expected actual -> ()
    | _ -> fail (Printf.sprintf "%s: expected int64 %Ld" name expected)

  let check_int name expected = function
    | Int actual when actual = expected -> ()
    | _ -> fail (Printf.sprintf "%s: expected int %d" name expected)

  let max_cache_entries = 44 * 44

  let cache_keyword index = Printf.sprintf "cache/%04d" index

  let transit_keyword name = Printf.sprintf "\"~:%s\"" name

  let json_array items = "[" ^ String.concat "," items ^ "]"

  let fixed_write_cases =
    [
      ("write-null", Null, "[\"~#'\",null]");
      ("write-bool", Bool true, "[\"~#'\",true]");
      ("write-int", Int 42, "[\"~#'\",42]");
      ("write-float", Float 1.25, "[\"~#'\",1.25]");
      ("write-string", String "hello", "[\"~#'\",\"hello\"]");
      ("write-escaped-string", Array [ String "~x"; String "^x"; String "`x" ],
       "[\"~~x\",\"~^x\",\"~`x\"]");
      ("write-keyword", Keyword "color", "[\"~#'\",\"~:color\"]");
      ("write-symbol", Symbol "thing", "[\"~#'\",\"~$thing\"]");
      ("write-cache",
       Array [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ],
       "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]");
      ("write-map-duplicate-key",
       Map [ (String "name", String "Ada"); (String "name", String "Grace") ],
       "[\"^ \",\"name\",\"Grace\"]");
      ("write-date", Array [ Date 123_456_789L ], "[\"~m123456789\"]");
      ("write-int64-safe", Array [ Int64 2_147_483_648L ], "[2147483648]");
      ("write-int64-unsafe", Array [ Int64 9_007_199_254_740_992L ],
       "[\"~i9007199254740992\"]");
      ("write-special-scalars",
       Array
         [
           Big_int "12345678901234567890";
           Big_decimal "123.456";
           Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
           Uri "https://example.com";
         ],
       "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]");
      ("write-binary", Array [ Binary "hi" ], "[\"~baGk=\"]");
      ("write-set", Set [ String "a"; String "b" ], "[\"~#set\",[\"a\",\"b\"]]");
      ("write-list", List [ String "a" ], "[\"~#list\",[\"a\"]]");
      ("write-tagged", Tagged ("point", Array [ Int 10; Int 20 ]),
       "[\"~#point\",[10,20]]");
      ("write-complex-map", Map [ (Array [ Int 1; Int 2 ], String "point") ],
       "[\"~#cmap\",[[1,2],\"point\"]]");
      ("write-complex-map-cache-order",
       Map
         [
           ( Array [ Keyword "key/first"; Keyword "shared/token" ],
             Array [ Keyword "value/first"; Keyword "shared/token" ] );
         ],
       "[\"~#cmap\",[[\"~:key/first\",\"~:shared/token\"],[\"~:value/first\",\"^2\"]]]");
    ]

  let fixed_verbose_cases =
    [
      ("verbose-map",
       Map [ (String "name", String "Ada"); (String "name", String "Grace") ],
       "{\"name\":\"Grace\"}");
      ("verbose-no-cache", Array [ Keyword "color"; Keyword "color" ],
       "[\"~:color\",\"~:color\"]");
      ("verbose-date", Array [ Date 123_456_789L ],
       "[\"~t1970-01-02T10:17:36.789Z\"]");
    ]

  let fixed_read_cases =
    [
      ("read-null", "[\"~#'\",null]", Null);
      ("read-bool", "[\"~#'\",true]", Bool true);
      ("read-int", "[\"~#'\",42]", Int 42);
      ("read-string", "[\"~#'\",\"hello\"]", String "hello");
      ("read-escaped-string", "\"~~x\"", String "~x");
      ("read-keyword", "[\"~#'\",\"~:color\"]", Keyword "color");
      ("read-symbol", "[\"~#'\",\"~$thing\"]", Symbol "thing");
      ("read-cache", "[\"~:color\",\"^0\",\"~$thing\",\"^1\"]",
       Array [ Keyword "color"; Keyword "color"; Symbol "thing"; Symbol "thing" ]);
      ("read-cached-keyword-in-two-element-array",
       "[[\"~:db/add\",1,\"~:block/title\",\"x\"],[\"^0\",2,\"~:block/name\",\"y\"],[\"^0\",3,\"~:block/uuid\",\"~u11111111-1111-4111-8111-111111111111\"],[\"~:db.fn/retractAttribute\",4,\"~:block/alias\"],[\"~:db/retractEntity\",5],[\"^6\",6]]",
       Array
         [
           Array
             [ Keyword "db/add"; Int 1; Keyword "block/title"; String "x" ];
           Array
             [ Keyword "db/add"; Int 2; Keyword "block/name"; String "y" ];
           Array
             [
               Keyword "db/add";
               Int 3;
               Keyword "block/uuid";
               Uuid "11111111-1111-4111-8111-111111111111";
             ];
           Array
             [ Keyword "db.fn/retractAttribute"; Int 4; Keyword "block/alias" ];
           Array [ Keyword "db/retractEntity"; Int 5 ];
           Array [ Keyword "db/retractEntity"; Int 6 ];
         ]);
      ("read-nested-cached-keywords-in-two-element-arrays",
       "[[\"~:db/retractEntity\",[\"~:block/uuid\",\"~u22222222-2222-4222-8222-222222222222\"]],[\"^0\",[\"^1\",\"~u33333333-3333-4333-8333-333333333333\"]]]",
       Array
         [
           Array
             [
               Keyword "db/retractEntity";
               Array
                 [
                   Keyword "block/uuid";
                   Uuid "22222222-2222-4222-8222-222222222222";
                 ];
             ];
           Array
             [
               Keyword "db/retractEntity";
               Array
                 [
                   Keyword "block/uuid";
                   Uuid "33333333-3333-4333-8333-333333333333";
                 ];
             ];
         ]);
      ("read-cached-symbol-in-two-element-array",
       "[[\"~$operation\"],[\"^0\",1]]",
       Array
         [ Array [ Symbol "operation" ]; Array [ Symbol "operation"; Int 1 ] ]);
      ("read-cached-tag-in-two-element-array",
       "[[\"~#point\",1],[\"^0\",2]]",
       Array [ Tagged ("point", Int 1); Tagged ("point", Int 2) ]);
      ("read-cached-string-in-two-element-array",
       "[[\"^ \",\"name\",\"Ada\"],[\"^0\",1]]",
       Array
         [
           Map [ (String "name", String "Ada") ];
           Array [ String "name"; Int 1 ];
         ]);
      ("read-cached-tag-looking-string-in-two-element-array",
       "[[\"^ \",\"~~#point\",0],[\"^0\",1]]",
       Array
         [
           Map [ (String "~#point", Int 0) ];
           Array [ String "~#point"; Int 1 ];
         ]);
      ("read-map-key-cached-once",
       "[[\"^ \",\"~:first\",1,\"~:second\",2],[\"^1\",3,4]]",
       Array
         [
           Map [ (Keyword "first", Int 1); (Keyword "second", Int 2) ];
           Array [ Keyword "second"; Int 3; Int 4 ];
         ]);
      ("read-map", "[\"^ \",\"name\",\"Grace\"]",
       Map [ (String "name", String "Grace") ]);
      ("read-date", "[\"~t1970-01-02T10:17:36.789Z\"]",
       Array [ Date 123_456_789L ]);
      ("read-int64-tag", "[\"~i9007199254740992\"]",
       Array [ Int64 9_007_199_254_740_992L ]);
      ("read-special-scalars",
       "[\"~n12345678901234567890\",\"~f123.456\",\"~u531a379e-31bb-4ce1-8690-158dceb64be6\",\"~rhttps://example.com\"]",
       Array
         [
           Big_int "12345678901234567890";
           Big_decimal "123.456";
           Uuid "531a379e-31bb-4ce1-8690-158dceb64be6";
           Uri "https://example.com";
         ]);
      ("read-binary", "[\"~baGk=\"]", Array [ Binary "hi" ]);
      ("read-set", "[\"~#set\",[\"a\",\"b\"]]",
       Set [ String "a"; String "b" ]);
      ("read-list", "[\"~#list\",[\"a\"]]", List [ String "a" ]);
      ("read-tagged", "[\"~#point\",[10,20]]",
       Tagged ("point", Array [ Int 10; Int 20 ]));
      ("read-complex-map", "[\"~#cmap\",[[1,2],\"point\"]]",
       Map [ (Array [ Int 1; Int 2 ], String "point") ]);
      ("read-quote-ground", "\"~cx\"", String "x");
    ]

  let run_fixed () =
    List.iter
      (fun (name, value, expected) ->
        check_string name expected (to_string value))
      fixed_write_cases;
    List.iter
      (fun (name, value, expected) ->
        check_string name expected (to_string ~mode:Verbose value))
      fixed_verbose_cases;
    List.iter
      (fun (name, text, expected) -> check_value name expected (of_string text))
      fixed_read_cases;
    let boundary_names = List.init 45 cache_keyword in
    let boundary_values = List.map (fun name -> Keyword name) boundary_names in
    let boundary_json = List.map transit_keyword boundary_names in
    check_string "write-cache-code-boundaries"
      (json_array (boundary_json @ [ "\"^[\""; "\"^10\"" ]))
      (to_string
         (Array
            (boundary_values
            @ [ List.nth boundary_values 43; List.nth boundary_values 44 ])));
    check_value "read-cache-code-boundaries"
      (Array
         (boundary_values
         @ [ List.nth boundary_values 43; List.nth boundary_values 44 ]))
      (of_string
         (json_array (boundary_json @ [ "\"^[\""; "\"^10\"" ])));
    let wrapping_names = List.init (max_cache_entries + 1) cache_keyword in
    let wrapping_values = List.map (fun name -> Keyword name) wrapping_names in
    let wrapping_json = List.map transit_keyword wrapping_names in
    let wrapped_value = List.nth wrapping_values max_cache_entries in
    check_string "write-cache-wraps-at-capacity"
      (json_array (wrapping_json @ [ "\"^0\"" ]))
      (to_string (Array (wrapping_values @ [ wrapped_value ])));
    (match of_string (json_array (wrapping_json @ [ "\"^0\"" ])) with
    | Array values ->
        check_int "read-cache-wraps-at-capacity-length"
          (max_cache_entries + 2)
          (Int (List.length values));
        check_value "read-cache-wraps-at-capacity-first" (List.hd wrapping_values)
          (List.hd values);
        check_value "read-cache-wraps-at-capacity-new-slot" wrapped_value
          (List.nth values max_cache_entries);
        check_value "read-cache-wraps-at-capacity-reference" wrapped_value
          (List.nth values (max_cache_entries + 1))
    | _ -> fail "read-cache-wraps-at-capacity: expected array");
    check_int "read-int-max" 2_147_483_647 (of_string "[\"~#'\",2147483647]");
    check_int "read-int-min" (-2_147_483_648)
      (of_string "[\"~#'\",-2147483648]");
    check_int64 "read-int64-above-int" 2_147_483_648L
      (of_string "[\"~#'\",2147483648]");
    check_int64 "read-int64-below-int" (-2_147_483_649L)
      (of_string "[\"~#'\",-2147483649]");
    check_float "read-float" 1.25 (of_string "[\"~#'\",1.25]")

  let escaped text = String.escaped text

  let run_random_output _platform_name =
    List.iteri
      (fun index value ->
        let normal = to_string value in
        let verbose = to_string ~mode:Verbose value in
        let decode_random mode text =
          try of_string text with
          | Decode_error message ->
              fail
                (Printf.sprintf "random-%s-decode-%d failed for %S: %s" mode
                   index text message)
        in
        let normal_roundtrip = decode_random "normal" normal in
        let verbose_roundtrip = decode_random "verbose" verbose in
        check_value
          (Printf.sprintf "random-normal-roundtrip-%d" index)
          value normal_roundtrip;
        check_value
          (Printf.sprintf "random-verbose-roundtrip-%d" index)
          value verbose_roundtrip;
        Printf.printf "%03d\t%s\t%s\n" index (escaped normal) (escaped verbose))
      (Random_cases.values random_constructors)
end
