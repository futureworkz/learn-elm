module Exercise.User exposing (User, create, updateEmail)

{-| This is a multi-part exercise centering around the User type
Please follow the trainer's instruction.
-}


type alias User =
    { name : String
    , email : String
    , age : Int
    , gender : Maybe Gender
    }


type Gender
    = Male
    | Female


{-| Write a function named create to create a user

    cindy : User
    cindy =
        create "Cindy" "cindy@example.com" 22

-}
create : String -> String -> Int -> Maybe Gender -> Result String User
create name email age gender =
    validate <| User name email age gender


{-| Write a function named updateEmail to update a user's email

    updatedCindy : User
    updatedCindy =
        updateEmail "newCindy@example.com" cindy

-}
updateEmail : String -> User -> User
updateEmail newEmail userRecord =
    { userRecord | email = newEmail }


{-| Trainer's demostration
Write a function to find a user by name from a list of User
There is no find function in Elm Core List
-}
findByName : String -> List User -> Maybe User
findByName uname allRecords =
    let
        x =
            List.filter (\i -> i.name == uname) allRecords
    in
    List.head x


{-| Write a function named findByEmail to find a user by email from a list of User

    cindy : User
    cindy =
      findByEmail "cindy@example.com" <|
        [
          { name: "Mary", email: "mary@example.com", age: 22 },
          { name: "Cindy", email: "cindy@example.com", age: 26 },
          { name: "Miya", email: "miya@example.com", age: 28 },
          ]

-}
findByEmail : String -> List User -> Maybe User
findByEmail emailRecord allRecords =
    let
        x =
            List.filter (\i -> i.email == emailRecord) allRecords
    in
    List.head x


{-| Add a Maybe gender field to User type
Write a function that returns all female users
Ignore users who does not have gender
-}
getFemaleUsers : List User -> List User
getFemaleUsers users =
    List.filter (\i -> i.gender == Just Female) users



-- User Validations
-- Write a function to validate user name is more than or equal to 3 characters


validateName : User -> Result String User
validateName userRecord =
    let
        userInputName =
            userRecord.name
    in
    if String.length userInputName < 3 then
        Err "Name must be more than or equal to 3 characters"

    else
        Ok userRecord



-- Write a function to validate user email contains '@'


validateEmail : User -> Result String User
validateEmail userRecord =
    let
        userInputEmail =
            userRecord.email
    in
    {- case String.contains "@" userInputEmail of
       True ->
       Ok userRecord

       False ->
       Err "Email must contain a @ symbol"
    -}
    if String.contains "@" userInputEmail == False then
        Err "Email must contain a @ symbol"

    else
        Ok userRecord



-- Write a function to validate user age is >= 18


validateAge : User -> Result String User
validateAge userRecord =
    let
        userInputAge =
            userRecord.age
    in
    if userInputAge <= 18 then
        Err "User must be 18 and above!"

    else
        Ok userRecord



--Write a function to validate a user


validate : User -> Result String User
validate userRecord =
    validateName userRecord |> Result.andThen validateEmail |> Result.andThen validateAge
